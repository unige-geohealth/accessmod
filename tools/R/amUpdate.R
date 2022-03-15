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

# functions to handle update

amUpdateApp <- function(force = FALSE) {
  defMsg <- ams("update_do_not_reload")

  progressBarControl(
    visible = TRUE,
    percent = 0,
    title = defMsg,
    text = ams("update_merge_code")
  )

  if (force) {
    system("git reset --hard HEAD && git pull")
  } else {
    system("git merge FETCH_HEAD")
  }

  if (file.exists(".fetched_version")) {
    file.remove(".fetched_version")
  }
  if (file.exists(".fetched_changes")) {
    file.remove(".fetched_changes")
  }

  progressBarControl(
    visible = TRUE,
    percent = 30,
    title = defMsg,
    text = ams("update_install_libraries"),
    timeOut = 2
  )

  system("Rscript global.R")

  progressBarControl(
    visible = TRUE,
    percent = 90,
    title = defMsg,
    text = ams("update_restart"),
    timeOut = 2
  )

  amRestart()
}

amGetAppRevisionCurrent <- function() {
  system("git rev-parse --verify HEAD | awk '{print substr($0,1,7)}'", intern = T)
}

amGetAppRevisionFetched <- function() {
  fetched <- system("git rev-parse --verify FETCH_HEAD | awk '{print substr($0,1,7)}'", intern = T)
  if (amNoDataCheck(fetched)) fetched <- amGetAppRevisionCurrent()
  fetched
}

amGetAppCurrentBranch <- function() {
  current <- system("git branch | grep '*' |awk '{ print $2}'", intern = T)
  if (amNoDataCheck(current)) current <- "devel"
  current
}

amGetAppVersionCurrent <- function() {
  readLines("version.txt")
}

amHasAppUpdate <- function() {
  isTRUE(file.exists(".fetched_version"))
}

amGetAppVersionFetched <- function() {
  if (amHasAppUpdate()) {
    readLines(".fetched_version")
  }
}
amGetAppChangesFetched <- function() {
  changes <- ""
  if (amHasAppUpdate()) {
    changes <- readLines(".fetched_changes") %>%
      paste(collapse = "\n")
  }
  changes
}
amGetAppChangesCurrent <- function() {
  readLines("changes.md") %>% paste(collapse = "\n")
}

amUiLinkRepoRelease <- function(version = amGetAppVersionCurrent(),
                                text = amGetAppVersionCurrent()) {
  tags$a(
    target = "_blank",
    href = paste0(config$repository, "/releases/tag/", version),
    text
  )
}

amGetAppVersionCurrentLink <- function() {
  amUiLinkRepoRelease(
    version = amGetAppVersionCurrent(),
    text = amGetAppVersionCurrent()
  )
}
amGetAppVersionFetchedLink <- function() {
  # if(amHasAppUpdate()){
  amUiLinkRepoRelease(
    version = amGetAppVersionFetched(),
    text = amGetAppVersionFetched()
  )
  # }
}
