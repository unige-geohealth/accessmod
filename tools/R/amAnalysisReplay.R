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

amAnalysisGetPath <- function() {
  cacheDir <- config$pathCacheDir
  if (!dir.exists(cacheDir)) {
    stop("Cache directory not found")
  }
  analysisPathDir <- paste0(cacheDir, "/amAnalysis")
  if (!dir.exists(analysisPathDir)) {
    dir.create(analysisPathDir)
  }
  return(analysisPathDir)
}


amAnalysisGetPathFile <- function(name = NULL) {
  pathDir <- amAnalysisGetPath()
  if (!is.null(name)) {
    analysisPathFile <- sprintf("%1$s/amAnalysis_%2$s.rdata", pathDir, name)
  } else {
    analysisPathFile <- ""
  }
  return(analysisPathFile)
}

amAnalysisSave <- function(name = "default") {
  e <- parent.frame()
  params <- as.list(e)
  pathFile <- amAnalysisGetPathFile(name)
  call <- as.list(eval(quote(match.call()), env = e))
  fun <- call[[1]]
  out <- list(
    fun = as.character(fun),
    params = params
  )
  saveRDS(out, pathFile)
}

amAnalysisGetList <- function() {
  path <- amAnalysisGetPath()
  files <- list.files(path)
  getBase <- function(x) {
    str_split(x, "_|\\.")[[1]][2]
  }
  vapply(files, getBase, character(1), USE.NAMES = F)
}

amAnalysisGet <- function(name = NULL) {
  if (amNoDataCheck(name)) {
    name <- amAnalysisGetList()[[1]]
  }

  pathFile <- amAnalysisGetPathFile(name)
  if (file.exists(pathFile)) {
    return(readRDS(pathFile))
  }
}


amAnalysisReplay <- function(name = "default") {
  a <- amAnalysisGet(name)
  do.call(a$fun, a$params)
}

# shortcut
aar <- amAnalysisReplay
