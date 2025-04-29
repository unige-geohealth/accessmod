#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(stevedore)
library(jsonlite)
library(semver)
library(memoise)
library(cachem)

cm <- cache_mem(max_age = 5 * 60)
if (docker_available()) {
  docker <- docker_client()
} else {
  docker <- list()
}
base_namespace <- "fredmoser"
base_repo <- "accessmod"
base_tag <- paste0(base_namespace, "/", base_repo)
version_current <- readLines("version.txt")
version_min <- "5.7.16-alpha"
url_hub_fetch <- "https://hub.docker.com/v2/repositories/%1$s/tags/?page_size=%2$s&page=%3$s"
hostname <- Sys.info()["nodename"]


am_parse_versions <- function(versions) {
  result <- lapply(versions, am_parse_version)
  structure(result, class = "svlist")
}

am_parse_version <- function(version) {
  tryCatch(
    {
      return(parse_version(version))
    },
    error = function(cond) {
      return(NA)
    }
  )
}

am_parse_repo_tags <- function(image_name) {
  # Default values
  result <- list(
    registry = NULL,
    namespace = NULL,
    repository = NULL,
    tag = "latest", # Default tag if none specified
    version_semver = NULL
  )

  # Split tag from the rest
  parts <- strsplit(image_name, ":")[[1]]
  image_path <- parts[1]

  # Extract tag if present
  if (length(parts) > 1) {
    result$tag <- parts[2]
  }

  # Split the image path by "/"
  path_parts <- strsplit(image_path, "/")[[1]]

  # Handle different formats
  if (length(path_parts) == 1) {
    # Simple format: repo
    result$repository <- path_parts[1]
  } else if (length(path_parts) == 2) {
    # Format: namespace/repo
    result$namespace <- path_parts[1]
    result$repository <- path_parts[2]
  } else {
    # Format: registry/namespace/repo or longer path
    # Check if the first part looks like a registry (contains ".")
    if (grepl("\\.", path_parts[1])) {
      result$registry <- path_parts[1]
      result$namespace <- paste(path_parts[2:(length(path_parts) - 1)], collapse = "/")
      result$repository <- path_parts[length(path_parts)]
    } else {
      # No registry, just a long namespace path
      result$namespace <- paste(path_parts[1:(length(path_parts) - 1)], collapse = "/")
      result$repository <- path_parts[length(path_parts)]
    }
  }


  return(result)
}

am_get_image_names_df <- function(image_names) {
  # Input validation
  if (!is.character(image_names)) {
    stop("Input must be a character vector of image names")
  }

  # Apply the parsing function to all image names at once using lapply
  parsed_list <- lapply(image_names, am_parse_repo_tags)

  # Convert list of lists to a data frame more elegantly
  result_df <- data.frame(
    image_name = image_names,
    registry = vapply(parsed_list, function(x) ifelse(is.null(x$registry), NA_character_, x$registry), character(1)),
    namespace = vapply(parsed_list, function(x) ifelse(is.null(x$namespace), NA_character_, x$namespace), character(1)),
    repository = vapply(parsed_list, function(x) ifelse(is.null(x$repository), NA_character_, x$repository), character(1)),
    tag = vapply(parsed_list, function(x) x$tag, character(1)),
    stringsAsFactors = FALSE
  )

  return(result_df)
}


am_filter_versions <- function(repo_tags) {
  repos_df <- am_get_image_names_df(repo_tags)
  version_min_semver <- am_parse_version(version_min)

  repos_df_select <- repos_df[
    repos_df$tag != "latest" &
      vapply(repos_df$tag, isNotEmpty, logical(1)) &
      repos_df$repository == base_repo &
      repos_df$namespace == base_namespace &
      sapply(repos_df$tag, function(x) {
        v <- am_parse_version(x)
        isNotEmpty(v) && v > version_min_semver
      }),
  ]
  return(repos_df_select)
}


am_docker_versions_local <- function() {
  tryCatch(
    {
      images <- docker$image$list()

      # Check if repo_tags exists and is not NULL
      if (isEmpty(images)) {
        return(version_current)
      }

      repo_tags <- unlist(images$repo_tags)
      # Check if repoTagList is valid
      if (isEmpty(repo_tags)) {
        return(version_current)
      }

      versions_select <- am_filter_versions(repo_tags)
      return(versions_select$tag)
    },
    error = function(cond) {
      warning(cond)
      return(version_current)
    }
  )
}

am_docker_versions_remote_net <- function(n_pages = 10, page = 1) {
  tryCatch(
    {
      # Fetch tags from Docker Hub
      url <- sprintf(
        url_hub_fetch,
        base_tag,
        n_pages,
        page
      )
      tags <- fromJSON(url)
      repo_tags <- paste0(base_tag, ":", tags$results$name)
      versions_select <- am_filter_versions(repo_tags)
      return(versions_select$tag)
    },
    error = function(cond) {
      warning(cond)
      return(version_current)
    }
  )
}

am_docker_versions_summary <- function() {
  versions_remote <- am_docker_versions_remote()
  versions_local <- am_docker_versions_local()

  versions_local_semver <- am_parse_versions(versions_local)
  versions_remote_semver <- am_parse_versions(versions_remote)

  # Ensure all versions in the summary are strings
  summary <- list(
    local = as.list(as.character(versions_local_semver)),
    remote = as.list(as.character(versions_remote_semver)),
    current = version_current,
    maxLocal = as.character(max(versions_local_semver)),
    maxRemote = as.character(max(versions_remote_semver)),
    minLocal = as.character(min(versions_local_semver)),
    minRemote = as.character(min(versions_remote_semver)),
    hasUpdate = max(versions_remote) > max(versions_local_semver),
    date = Sys.time()
  )
  return(summary)
}

am_docker_versions_remote <- memoise(am_docker_versions_remote_net, cache = cm)
