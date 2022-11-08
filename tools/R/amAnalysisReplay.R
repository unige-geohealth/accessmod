#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

# Save and replay analysis

#
# Save 'replay' file
#
amAnalysisReplaySave <- function(
  analysis,
  mapset,
  location,
  args,
  argsEditable = c(),
  name,
  output = c(),
  timestamp = Sys.time(),
  overwrite = TRUE
) {
  valid <- amGrassSessionIsValid()

  if (!valid) {
    stop("Require valid am grass session")
  }

  editable <- amAnalysisReplayBuildEditable(args)


  # NULL values are converted to {} :(
  # ex. toJSON(list(a=NULL,b=NA,c="",d=character(1)),auto_unbox=T)
  # result = {"a":{},"b":null,"c":"","d":""}
  args <- lapply(
    args,
    function(a) {
      if (isEmpty(a)) {
        return(NA)
      }
      return(a)
    }
  )

  #
  # JSON + dir / file creation
  #
  str <- toJSON(list(
    timestamp = timestamp,
    analysis = analysis,
    mapset = mapset,
    location = location,
    output = output,
    editable = editable,
    args = args
  ),
  auto_unbox = TRUE
  )
  aPath <- system2("echo", config$pathConfigs)
  fPath <- file.path(aPath, sprintf("%s.json", name))

  if (!dir.exists(aPath)) {
    dir.create(aPath)
  }
  if (file.exists(fPath)) {
    if (overwrite) {
      unlink(fPath)
    } else {
      stop("amAnalysisReplaySave: file already exists")
    }
  }

  write(str, fPath)
}

#' Import archive wrapper
#' @param archive Archive path *.am5p
#' @param name Name of the project
#' @param owerwrite If the project exists, replace
amAnalisisReplayImportProject <- function(archive, name, owerwrite) {
  if (!file.exists(archive)) {
    stop(
      sprintf(
        "Import project requested but file '%s' does not exist",
        archive
      )
    )
  }

  amProjectImport(archive,
    name = name,
    overwrite = overwrite
  )
}

#
# Read 'replay' file and restart the analysis
#
amAnalysisReplayExec <- function(
  replayConf,
  exportDirectory = NULL,
  exportProjectDirectory = NULL,
  importProjectArchive = NULL,
  importProjectName = NULL,
  importProjectOverwrite = FALSE
) {
  if (!isEmpty(importProjectArchive)) {
    amAnalisisReplayImportProject(
      importProjectArchive,
      importProjectName,
      importProjectOverwrite,
    )
  }

  conf <- amAnalysisReplayParseConf(replayConf)
 

  amGrassNS(
    location = conf$location,
    mapset = conf$mapset,
    {
      amReMemoizeCostlyFunctions()
      res <- do.call(conf$analysis, conf$args)
      if (!isEmpty(exportDirectory)) {
        amAnalysisReplayExport(conf, exportDirectory)
      }
      return(res)
    }
  )
}

#
# Export all output
#
amAnalysisReplayExport <- function(replayConf, exportDirectory) {
  conf <- amAnalysisReplayParseConf(replayConf)
  exportDirectoryTmp <- file.path(tempdir(), amRandomName(""))
  if (isEmpty(exportDirectory) || !dir.exists(exportDirectory)) {
    warning(
      sprintf(
        "Export directory '%s' doesn't exist, creating one at %s",
        exportDirectory,
        exportDirectoryTmp
      )
    )
    mkdirs(exportDirectoryTmp)
    exportDirectory <- exportDirectoryTmp
  }

  for (dataName in conf$output) {
    tryCatch(
      {
        amExportData(
          dataName = dataName,
          exportDir = exportDirectory
        )
      },
      error = function(cond) {
        warning(cond)
      }
    )
  }

  return(exportDirectory)
}


#
# Parse replay file or config list
#
amAnalysisReplayParseConf <- function(replayConf) {
  dict <- config$dictReplayValidation
  isList <- mode(replayConf) == "list"
  isChar <- mode(replayConf) == "character"

  if (isList) {
    return(replayConf)
  }
  if (!isChar) {
    stop("amAnalysisReplayParseFile : unexpected input")
  }

  #
  # Parse JSON
  # - By default, don't convert to data.frame.
  # - `.editable` should be a list
  #
  if (isChar && file.exists(replayConf)) {
    replayConf <- fromJSON(replayConf, simplifyDataFrame = FALSE)
  }

  #
  # Coerse to data.frame if the validations schema specifies it.
  #
  for (item in dict) {
    for (argName in names(replayConf$args)) {
      if (argName == item$key) {
        if (!isEmpty(item$class) && item$class == "data.frame") {
          replayConf$args[[argName]] <- ldply(
            replayConf$args[[argName]],
            data.frame
          )
        }
      }
    }
  }

  #
  # Validate
  #
  issues <- amAnalysisReplayValidateConf(replayConf)


  if (isNotEmpty(issues)) {
    stop(paste(issues, collapse = "\n"))
  }

  return(replayConf)
}


#' Validate editable property of a replay config file
#'
#' @param replayConf Replay configuration list
#' @param issues Issues list to complete ( recursive mode )
#' @return issue Issues list
amAnalysisReplayValidateConf <- function(replayConf, issues = c()) {
  issues <- `if`(isEmpty(issues), character(0), issues)
  missingEditable <- isEmpty(replayConf$editable)

  if (missingEditable) {
    warning("Missing editable validation list")
    return(issues)
  }
  conf <- replayConf

  for (rule in conf$editable) {
    # Example str(rule) :
    # List of 3
    #  $ key : chr "parallel"
    #  $ mode: chr "logical"
    #
    for (argName in names(conf$args)) {
      # Example
      # > str(val)
      # logi TRUE
      # > str(argName)
      # chr "parallel"
      val <- conf$args[[argName]]

      if (rule$key == argName) {
        switch(rule$mode,
          "logical" = {
            modeOk <- mode(val) == "logical"
            if (!modeOk) {
              issues <- c(
                issues,
                sprintf(
                  "arg %s is not logical",
                  rule$key
                )
              )
            }
          },
          "numeric" = {
            modeOk <- mode(val) == "numeric"
            if (!modeOk) {
              issues <- c(
                issues,
                sprintf(
                  "arg %s is not numeric",
                  rule$key
                )
              )
            }

            if (modeOk && isNotEmpty(rule$min)) {
              if (val < rule$min) {
                issues <- c(
                  issues,
                  sprintf(
                    "arg %s is lower than %s",
                    rule$key,
                    rule$min
                  )
                )
              }
            }
            if (modeOk && isNotEmpty(rule$max)) {
              if (val > rule$max) {
                issues <- c(
                  issues,
                  sprintf(
                    "arg %s is greater than %s",
                    rule$key,
                    rule$min
                  )
                )
              }
            }
          },
          "character" = {
            modeOk <- mode(val) == "character"
            if (!modeOk) {
              issues <- c(
                issues,
                sprintf(
                  "arg %s is not character",
                  rule$key
                )
              )
            }

            if (modeOk && isNotEmpty(rule$choice)) {
              if (!val %in% rule$choice) {
                issues <- c(
                  issues,
                  sprintf(
                    "arg %s is not in %s",
                    rule$key,
                    paste0(
                      rule$choice,
                      collapse = ", "
                    )
                  )
                )
              }
            }
          },
          "list" = {
            modeOk <- mode(val) == "list"
            if (!modeOk) {
              issues <- c(
                issues,
                sprintf(
                  "arg %s is not character",
                  rule$key
                )
              )
            } else {
              for (v in val) {
                nestedConf <- list()
                nestedConf$editable <- rule$editable
                nestedConf$args <- v
                issues <- amAnalysisReplayValidateConf(nestedConf, issues)
              }
            }
          }
        )
      }
    }
  }

  return(issues)
}



#' From a list of argument name, build validation list for key 'editable'
#'
#' @param args
#' @return editable validation list
amAnalysisReplayBuildEditable <- function(args) {
  dict <- config$dictReplayValidation

  editable <- list()

  #
  # Build editable validation list
  # "parallel -> {
  #       key:'parallel',
  #       mode:'logical [list, numeric, character]'
  #     }"
  #
  for (item in dict) {
    for (argName in names(args)) {
      if (item$key == argName) {
        editable <- c(editable, list(item))
      }
    }
  }

  return(editable)
}
