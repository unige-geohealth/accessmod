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

fileExtProject <- config$fileArchiveProjectDb
pathDB <- config$pathGrassDataBase
pathCache <- config$pathCacheDir

#
# Convert sqlite path to relative path
#
amUpdateGrassDblnSqliteDbPath <- function(idProject) {
  dbStrRel <- "$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db"
  dbStrAbs <- sprintf("$GISDBASE/%1$s/%1$s/sqlite.db", idProject)
  varStrAbs <- sprintf("$GISDBASE/%1$s/%1$s/VAR", idProject)
  dbPath <- system(sprintf("echo %1$s", dbStrAbs), intern = TRUE)
  varPath <- system(sprintf("echo %s", varStrAbs), intern = TRUE)

  #
  # update VAR file.
  #
  if (file.exists(varPath)) {
    varFile <- read.dcf(varPath)
    varFileDb <- varFile[, "DB_DATABASE"]

    if (isNotEmpty(varFileDb)) {
      varFile[, "DB_DATABASE"] <- dbStrRel
      write.dcf(varFile, varPath)
    }
  }

  #
  # Replace abs path by relative path (dbStrRel)
  #
  dbDirPath <- dirname(dbPath)
  hasDb <- file.exists(dbPath)
  if (hasDb) {
    dbLinks <- list.files(
      path = dbDirPath,
      pattern = "dbln",
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE
    )

    for (fdb in dbLinks) {
      tryCatch(
        {
          dbTbl <- read.table(fdb, stringsAsFactors = FALSE, sep = "|")
          dbTbl$V4 <- dbStrRel
          strDb <- paste(dbTbl, collapse = "|")
          write(strDb, file = fdb)
        },
        error = function(e) {
          warning(e)
        }
      )
    }
  }
}

#
# Get sqlite path
#
amDbSqlitePath <- function() {
  dbSqlitePath <- system2("echo", config$pathSqliteDB)
  return(dbSqlitePath)
}


amProjectExport <- function(idProject) {
  fileName <- sprintf("%1$s.%2$s", idProject, fileExtProject)
  #
  # e.g. "/srv/shiny-server/data/cache/test.am5p
  #
  pathExport <- file.path(pathCache, fileName)
  pathProject <- file.path(pathDB, idProject)
  curwd <- getwd()

  on_exit_add({
    setwd(curwd)
  })

  if (!dir.exists(pathProject)) {
    stop("Project to export not found")
  }
  if (file.exists(pathExport)) {
    unlink(pathExport)
  }

  #
  # Update db path with relative db path
  #
  amUpdateGrassDblnSqliteDbPath(idProject)

  #
  # If zip from app folder, paths are absolute.
  #
  setwd(pathDB)
  zip(pathExport, idProject)
  return(pathExport)
}

#' Import am5p project archive
#'
#' @param projectPath Project file path or data upload list (shiny)
#' @param name Project name
#' @param overwrite If project exists, overwrite
#' @return NULL
amProjectImport <- function(projectPath, name, overwrite = FALSE) {
  name <- amSubPunct(name, "_")
  projects <- amGetGrassListLoc()
  isNameValid <- isNotEmpty(name)
  isExisting <- isTRUE(name %in% projects)
  isFileValid <- isFile(projectPath)
  isFileUploaded <- !isFileValid &&
    mode(projectPath) == "list" &&
    isFile(projectPath$datapath)
  fileType <- NULL

  if (isFileUploaded) {
    fileType <- projectPath$type
    projectPath <- projectPath$datapath
  }

  isExtValid <- identical(
    file_ext(projectPath),
    fileExtProject
  )

  if (isEmpty(fileType)) {
    fileType <- system2(
      "file",
      c(
        "-b",
        "--mime-type",
        projectPath
      ),
      stdout = TRUE
    )
  }

  isTypeValid <- identical(fileType, "application/zip")

  if (isExisting && overwrite) {
    unlink(file.path(pathDB, name), recursive = T)
    isExisting <- FALSE
  }

  if (isExisting) {
    warning(
      sprintf(
        "Project %s already exists. Stoping here.
        Remove it or use overwrite=TRUE to replace it",
        name
      )
    )
    return(NULL)
  }

  if (isExisting || !isExtValid || !isTypeValid || !isNameValid) {
    stop("Invalid importation. Check name, extension and type")
  }

  tmpDir <- file.path(tempdir(), amRandomName("import"))
  amDebugMsg(sprintf("Create temporary directory %s", tmpDir))
  dir.create(tmpDir)
  on_exit_add({
    unlink(tmpDir)
  })
  amDebugMsg(sprintf("Unzip %s into %s", projectPath, tmpDir))
  unzip(projectPath, exdir = tmpDir)
  projOldName <- list.files(tmpDir)[[1]]
  #
  # Rename location/mapset to new name
  #
  if (name != projOldName) {
    #
    # /tmp/oldname -> /tmp/newname
    #
    file.rename(
      file.path(tmpDir, projOldName),
      file.path(tmpDir, name)
    )
    #
    # /tmp/newname/oldname -> /tmp/newname/newname
    #
    file.rename(
      file.path(tmpDir, name, projOldName),
      file.path(tmpDir, name, name)
    )
  }

  #
  # Move into local db
  # NOTE:
  # file.rename do not work due to "reason 'Cross-device link'"
  # file.copy is slow AF
  #
  fromPath <- file.path(tmpDir, name)
  toPath <- file.path(pathDB, name)
  amDebugMsg(sprintf("move %s to %s", fromPath, toPath))
  system2(
    "mv",
    c(
      "-v",
      fromPath,
      toPath
    )
  )
  #
  # Update db links with relative path
  #

  amDebugMsg(sprintf("Update SQLite db path for %s", name))
  amUpdateGrassDblnSqliteDbPath(name)
  return(NULL)
}


#' Create a new project from a dem raster
#'
#' @param newDem {List} Upload list
#' @param newProjectName {Character} New project name
#' @param onProgress {Function} Callback to update a progress bar. Takes 3 arguments : text, percent, timeout
#'
amProjectCreateFromDem <- function(newDem, newProjectName, onProgress = function(text, percent, timout) {}) {
  #
  # Order files by size,
  #
  newDem <- newDem[with(newDem, order(-size)), ]
  tmpDir <- dirname(newDem[1, "datapath"])
  newDem$newPath <- file.path(tmpDir, newDem$name)
  file.rename(newDem$datapath, newDem$newPath)


  #
  # Validate
  #
  amValidateFileExt(newDem$name, "rast")
  #
  # take the first raster (heavier) as the base map
  #
  tmpMapPath <- newDem[1, "newPath"]

  on_exit_add({
    unlink(tmpDir, recursive = T)
  })

  #
  # Test for projection issues
  #
  r <- raster(tmpMapPath)
  destProj <- proj4string(r)

  onProgress(
    text = "Test data projection",
    percent = 4
  )

  if (isEmpty(destProj)) {
    stop(msgNoProj)
  }

  if (!grepl("+to_meter|+units=m", destProj)) {
    stop(
      "No metric parameter found. Please make sure that your data is projected in metric format."
    )
  }

  onProgress(
    text = "Init new project session",
    percent = 10
  )

  execGRASS("g.proj",
    location = newProjectName,
    proj4 = destProj,
    flags = "c"
  )

  amGrassSessionUpdate(
    location = newProjectName,
    mapset = "PERMANENT",
    resetRegion = FALSE
  )

  onProgress(
    text = "Importation in database",
    percent = 15
  )

  execGRASS("r.in.gdal",
    input = tmpMapPath,
    band = 1,
    output = config$mapDem,
    flags = c("overwrite", "quiet"),
    title = paste(newProjectName, "DEM")
  )

  execGRASS(
    "r.colors",
    map = config$mapDem,
    color = "elevation"
  )

  amMapsetCreate(
    newProjectName,
    switch = TRUE
  )

  execGRASS("db.connect", driver = "sqlite", database = config$pathSqliteDB)

  onProgress(
    text = "Set colors and remove temp files",
    percent = 80
  )

  onProgress(
    text = "Done",
    percent = 100
  )
}
