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
  isUploadObject <- is.list(projectPath) && isNotEmpty(projectPath$datapath)

  if (isUploadObject) {
    projectPath <- projectPath$datapath
  }

  isFileValid <- file.exists(projectPath)

  isExtValid <- identical(
    file_ext(projectPath),
    fileExtProject
  )

  fileType <- system2(
    "file",
    c(
      "-b",
      "--mime-type",
      projectPath
    ),
    stdout = TRUE
  )

  isTypeValid <- identical(fileType, "application/zip")

  if (isExisting && !overwrite) {
    warning(
      sprintf(
        "Project %s already exists. Stoping here.
        Remove it or use overwrite=TRUE to replace it",
        name
      )
    )
    return(NULL)
  }

  if (!isFileValid || !isExtValid || !isTypeValid || !isNameValid) {
    stop("Invalid importation. Check name, extension and type")
  }

  tmpDir <- file.path(tempdir(), amRandomName("import"))
  amDebugMsg(sprintf("Create temporary directory %s", tmpDir))
  dir.create(tmpDir)
  on_exit_add({
    unlink(tmpDir, recursive = TRUE)
  })
  amDebugMsg(sprintf("Unzip %s into %s", projectPath, tmpDir))
  unzip(projectPath, exdir = tmpDir)
  archiveRoots <- list.files(tmpDir)
  if (length(archiveRoots) != 1 ||
      !dir.exists(file.path(tmpDir, archiveRoots[[1]]))) {
    stop("Invalid project archive structure")
  }
  projOldName <- archiveRoots[[1]]
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

  amGrassNS(
    gisdbase = tmpDir,
    location = name,
    mapset = name,
    resetRegion = FALSE,
    {
      invisible(TRUE)
    }
  )

  amDebugMsg(sprintf("move %s to %s", fromPath, toPath))
  backupPath <- NULL
  if (isExisting && overwrite) {
    backupPath <- file.path(
      pathDB,
      amRandomName(paste0(".", name, "_backup"))
    )
    if (!file.rename(toPath, backupPath)) {
      stop(sprintf("Unable to stage existing project '%s' for replacement", name))
    }
  }

  moveOutput <- system2(
    "mv",
    c(
      "-v",
      fromPath,
      toPath
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  moveStatus <- attr(moveOutput, "status")
  if (is.null(moveStatus)) moveStatus <- 0L

  if (moveStatus != 0L || !dir.exists(toPath)) {
    if (dir.exists(toPath)) {
      unlink(toPath, recursive = TRUE)
    }
    if (!is.null(backupPath) && dir.exists(backupPath)) {
      file.rename(backupPath, toPath)
    }
    stop(sprintf(
      "Unable to install project '%s': %s",
      name,
      paste(moveOutput, collapse = "\n")
    ))
  }

  #
  # Update db links with relative path
  #

  amDebugMsg(sprintf("Update SQLite db path for %s", name))
  tryCatch(
    {
      amUpdateGrassDblnSqliteDbPath(name)
    },
    error = function(e) {
      unlink(toPath, recursive = TRUE)
      if (!is.null(backupPath) && dir.exists(backupPath)) {
        file.rename(backupPath, toPath)
      }
      stop(e)
    }
  )

  if (!is.null(backupPath) && dir.exists(backupPath)) {
    unlink(backupPath, recursive = TRUE)
  }
  return(NULL)
}


#' Convert a CRS definition with projinfo
#'
#' AccessMod uses only structured PROJJSON and WKT2:2019 at this boundary.
#'
#' @param definition {Character} CRS definition accepted by PROJ
#' @param format {Character} Requested output format
#' @return {Character} Converted definition
amCrsProjInfoConvert <- function(definition, format) {
  if (length(definition) != 1 || is.na(definition) || !nzchar(definition)) {
    stop("CRS definition is empty")
  }

  errorFile <- tempfile("projinfo_error_")
  on.exit(unlink(errorFile), add = TRUE)

  output <- suppressWarnings(system2(
    "projinfo",
    c("-q", "-o", format, shQuote(definition)),
    stdout = TRUE,
    stderr = errorFile
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L

  if (status != 0L || length(output) == 0) {
    detail <- if (file.exists(errorFile)) {
      paste(readLines(errorFile, warn = FALSE), collapse = "\n")
    } else {
      "unknown projinfo error"
    }
    stop(sprintf("Unable to convert CRS: %s", detail))
  }

  paste(output, collapse = "\n")
}


#' Test whether a CRS string is WKT2
#'
#' @param wkt {Character} CRS definition
#' @return {Logical}
amCrsIsWkt2 <- function(wkt) {
  isTRUE(grepl(
    paste0(
      "^[[:space:]]*(BOUNDCRS|COMPOUNDCRS|DERIVEDPROJCRS|ENGCRS|",
      "GEODCRS|GEOGCRS|PARAMETRICCRS|PROJCRS|TIMECRS|VERTCRS)",
      "[[:space:]]*\\["
    ),
    wkt
  ))
}


#' Canonicalize a CRS as WKT2:2019
#'
#' Identity BoundCRS wrappers are removed using their structured PROJJSON
#' transformation parameters. Any real transformation is preserved.
#'
#' @param wkt {Character} Input WKT definition
#' @return {List} Canonical WKT and normalization metadata
amCrsCanonicalWkt2 <- function(wkt) {
  inputIsWkt2 <- amCrsIsWkt2(wkt)
  crsJson <- amCrsProjInfoConvert(wkt, "PROJJSON")
  crs <- jsonlite::fromJSON(crsJson, simplifyVector = FALSE)
  isBound <- identical(crs$type, "BoundCRS")
  unwrapIdentity <- FALSE

  if (isBound) {
    parameters <- crs$transformation$parameters
    expectedCodes <- 8605:8611
    parameterCodes <- suppressWarnings(as.integer(vapply(
      parameters,
      function(parameter) {
        if (is.null(parameter$id$code)) NA_real_ else parameter$id$code
      },
      numeric(1)
    )))
    parameterValues <- suppressWarnings(as.numeric(vapply(
      parameters,
      function(parameter) {
        if (is.null(parameter$value)) NA_real_ else parameter$value
      },
      numeric(1)
    )))

    unwrapIdentity <- length(parameterCodes) == length(expectedCodes) &&
      setequal(parameterCodes, expectedCodes) &&
      all(is.finite(parameterValues)) &&
      all(parameterValues == 0)

    if (unwrapIdentity) {
      crs <- crs$source_crs
      crsJson <- jsonlite::toJSON(
        crs,
        auto_unbox = TRUE,
        digits = NA,
        pretty = FALSE
      )
    }
  }

  wkt2 <- amCrsProjInfoConvert(crsJson, "WKT2:2019")
  parsed <- sf::st_crs(wkt2)
  if (is.na(parsed) || !amCrsIsWkt2(wkt2)) {
    stop("CRS conversion did not produce valid WKT2:2019")
  }

  list(
    wkt = wkt2,
    inputIsWkt2 = inputIsWkt2,
    wasBound = isBound,
    unwrappedIdentity = unwrapIdentity,
    needsRewrite = !inputIsWkt2 || unwrapIdentity
  )
}


#' Smoke-test OGR export using the active GRASS project CRS
#'
#' @return {Logical}
amProjectCrsExportSmokeTest <- function() {
  region <- amGrassRegionMeta()
  vectorName <- amRandomName("tmp__crs_wkt2_smoke")
  inputPath <- tempfile(fileext = ".txt")
  outputPath <- tempfile(fileext = ".gpkg")

  on.exit({
    rmVectIfExists(vectorName)
    unlink(c(inputPath, outputPath))
  }, add = TRUE)

  writeLines(sprintf(
    "%.15f|%.15f",
    mean(c(region$w, region$e)),
    mean(c(region$s, region$n))
  ), inputPath)

  execGRASS(
    "v.in.ascii",
    input = inputPath,
    output = vectorName,
    separator = "pipe",
    columns = "x double precision, y double precision",
    x = 1,
    y = 2,
    flags = c("overwrite", "quiet")
  )
  execGRASS(
    "v.out.ogr",
    input = vectorName,
    output = outputPath,
    format = "GPKG",
    flags = c("overwrite", "m", "s")
  )

  isTRUE(file.exists(outputPath) && file.info(outputPath)$size > 0)
}


#' Ensure that the active GRASS project has a canonical WKT2 CRS
#'
#' Legacy project metadata is converted at this compatibility boundary. The
#' update is transactional and restores the original metadata on any failure.
#'
#' @return {List} Migration status and canonical WKT
amProjectEnsureWkt2 <- function() {
  gisdbase <- amGrassSessionGetEnv("GISDBASE")
  project <- amGrassSessionGetProject()
  mapset <- amGrassSessionGetMapset()
  projectPath <- file.path(gisdbase, project)
  permanentPath <- file.path(projectPath, "PERMANENT")
  wktPath <- file.path(permanentPath, "PROJ_WKT")
  internalCrsFiles <- list.files(
    permanentPath,
    pattern = "^PROJ_",
    full.names = TRUE
  )

  if (!dir.exists(permanentPath) || length(internalCrsFiles) == 0) {
    return(invisible(list(migrated = FALSE, skipped = TRUE, wkt = NULL)))
  }

  hasStoredWkt <- isTRUE(file.exists(wktPath) && file.info(wktPath)$size > 0)
  storedWkt <- if (hasStoredWkt) {
    paste(readLines(wktPath, warn = FALSE), collapse = "\n")
  } else {
    amGrassProjectWkt()
  }

  isOrdinaryWkt2 <- hasStoredWkt &&
    amCrsIsWkt2(storedWkt) &&
    !grepl("^[[:space:]]*BOUNDCRS[[:space:]]*\\[", storedWkt) &&
    !is.na(sf::st_crs(storedWkt))
  if (isOrdinaryWkt2) {
    return(invisible(list(
      migrated = FALSE,
      skipped = FALSE,
      wkt = storedWkt
    )))
  }

  canonical <- amCrsCanonicalWkt2(storedWkt)
  needsMigration <- !hasStoredWkt || canonical$needsRewrite

  if (!needsMigration) {
    return(invisible(list(
      migrated = FALSE,
      skipped = FALSE,
      wkt = canonical$wkt
    )))
  }

  snapshotPath <- tempfile("crs_snapshot_")
  dir.create(snapshotPath)
  removeSnapshot <- TRUE
  on.exit({
    if (isTRUE(removeSnapshot)) {
      unlink(snapshotPath, recursive = TRUE)
    }
  }, add = TRUE)

  permanentFiles <- list.files(
    permanentPath,
    pattern = "^(PROJ_|WIND$|DEFAULT_WIND$)",
    full.names = TRUE
  )
  mapsetWind <- file.path(projectPath, mapset, "WIND")
  snapshotFiles <- unique(c(
    permanentFiles,
    if (file.exists(mapsetWind)) mapsetWind else character(0)
  ))
  snapshotRelative <- substring(snapshotFiles, nchar(projectPath) + 2)

  for (i in seq_along(snapshotFiles)) {
    destination <- file.path(snapshotPath, snapshotRelative[[i]])
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(file.copy(snapshotFiles[[i]], destination, overwrite = TRUE))) {
      stop(sprintf(
        "Unable to snapshot project CRS metadata file '%s'",
        snapshotRelative[[i]]
      ))
    }
  }

  restoreSnapshot <- function() {
    currentPermanent <- list.files(
      permanentPath,
      pattern = "^(PROJ_|WIND$|DEFAULT_WIND$)",
      full.names = TRUE
    )
    currentMapsetWind <- file.path(projectPath, mapset, "WIND")
    unlink(unique(c(
      currentPermanent,
      if (file.exists(currentMapsetWind)) currentMapsetWind else character(0)
    )))

    for (relativePath in snapshotRelative) {
      source <- file.path(snapshotPath, relativePath)
      destination <- file.path(projectPath, relativePath)
      dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
      if (!isTRUE(file.copy(source, destination, overwrite = TRUE))) {
        stop(sprintf("Unable to restore '%s'", relativePath))
      }
    }
  }

  regionBefore <- amGrassRegionMeta()
  migrated <- FALSE
  migrationError <- NULL

  tryCatch(
    {
      canonicalPath <- tempfile(fileext = ".wkt")
      on.exit(unlink(canonicalPath), add = TRUE)
      writeLines(canonical$wkt, canonicalPath)

      amGrassSessionUpdate(
        location = project,
        mapset = "PERMANENT",
        resetRegion = FALSE
      )
      execGRASS("g.proj", wkt = canonicalPath, flags = "c")
      amGrassSessionUpdate(
        location = project,
        mapset = mapset,
        resetRegion = FALSE
      )

      actualWkt <- amGrassProjectWkt()
      actualCanonical <- amCrsCanonicalWkt2(actualWkt)
      regionAfter <- amGrassRegionMeta()
      regionFields <- c("n", "s", "e", "w", "nsres", "ewres")
      regionUnchanged <- isTRUE(all.equal(
        unlist(regionBefore[regionFields]),
        unlist(regionAfter[regionFields]),
        tolerance = 1e-10,
        check.attributes = FALSE
      ))
      crsMatches <- isTRUE(
        sf::st_crs(canonical$wkt) == sf::st_crs(actualCanonical$wkt)
      )

      if (!regionUnchanged || !crsMatches ||
          !amProjectCrsExportSmokeTest()) {
        stop("WKT2 migration validation failed")
      }

      migrated <- TRUE
    },
    error = function(e) {
      migrationError <<- conditionMessage(e)
    }
  )

  if (!migrated) {
    restorationError <- tryCatch(
      {
        restoreSnapshot()
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    sessionError <- tryCatch(
      {
        amGrassSessionUpdate(
          location = project,
          mapset = mapset,
          resetRegion = FALSE
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )

    if (!is.null(restorationError)) {
      removeSnapshot <- FALSE
      stop(sprintf(
        paste0(
          "project_crs_migration_failed: project '%s' restoration failed: ",
          "%s; recovery snapshot retained at '%s'"
        ),
        project,
        restorationError,
        snapshotPath
      ))
    }

    if (!is.null(sessionError)) {
      stop(sprintf(
        "project_crs_migration_failed: project '%s' was restored but could not be reopened: %s",
        project,
        sessionError
      ))
    }

    stop(sprintf(
      "project_crs_migration_failed: project '%s' was restored: %s",
      project,
      migrationError
    ))
  }

  invisible(list(migrated = TRUE, skipped = FALSE, wkt = canonical$wkt))
}


#' Create a new project from a dem raster
#'
#' @param newDem {List} Upload list
#' @param newProjectName {Character} New project name
#' @param onProgress {Function} Callback to update a progress bar. Takes 3 arguments : text, percent
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
  # ->wkt2
  #
  r <- rast(tmpMapPath)
  sourceWkt <- crs(r)
  if (isEmpty(sourceWkt)) {
    stop(msgNoProj)
  }
  destWkt <- amCrsCanonicalWkt2(sourceWkt)$wkt
  tmpWkt <- tempfile(".wkt")
  writeLines(destWkt, tmpWkt)

  onProgress(
    text = "Test data projection",
    percent = 4
  )

  is_metric <- linearUnits(r) > 0L

  if (!is_metric) {
    stop(
      "No metric parameter found. Please make sure that your data is projected in metric format."
    )
  }

  onProgress(
    text = "Init new project session",
    percent = 10
  )


  # GRASS 8.5 renamed the g.proj creation option from "location" to
  # "project". Keep this code usable with both the current and legacy base
  # images while the new image is rolled out.
  gProjProjectOption <- if ("project" %in% parseGRASS("g.proj")$pnames) {
    "project"
  } else {
    "location"
  }
  gProjArgs <- list(
    cmd = "g.proj",
    wkt = tmpWkt,
    flags = "c"
  )
  gProjArgs[[gProjProjectOption]] <- newProjectName
  do.call(execGRASS, gProjArgs)

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

  #
  # Align the GRASS computational region to the DEM extent.
  # Without this, any spatial operation or import validation that reads the
  # region immediately after project creation will see the default (projection-
  # wide) extents instead of the actual DEM footprint.
  #
  amRegionReset()

  onProgress(
    text = "Set colors and remove temp files",
    percent = 80
  )

  onProgress(
    text = "Done",
    percent = 100
  )
}


#' Delete an AccessMod project (GRASS location)
#'
#' @param idProject {Character} Project name / location to delete
#' @return NULL invisibly
amProjectDelete <- function(idProject) {
  projects <- amGetGrassListLoc()

  if (!isTRUE(idProject %in% projects)) {
    stop(sprintf("Project '%s' not found", idProject))
  }

  projPath <- file.path(pathDB, idProject)

  if (!dir.exists(projPath)) {
    stop(sprintf("Project directory not found: %s", projPath))
  }

  unlink(projPath, recursive = TRUE, force = TRUE)
  return(invisible(NULL))
}
