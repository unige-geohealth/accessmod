print("Testing canonical WKT2 project migration and vector export")

testFilesDir <- "tests/project_crud/files"
sourceDem <- list.files(
  file.path(testFilesDir, "raster_dem"),
  pattern = "\\.img$",
  full.names = TRUE
)[[1]]
sourceWkt <- crs(rast(sourceDem))
sourceCanonical <- amCrsCanonicalWkt2(sourceWkt)

nonIdentityJson <- jsonlite::fromJSON(
  amCrsProjInfoConvert(sourceWkt, "PROJJSON"),
  simplifyVector = FALSE
)
nonIdentityJson$transformation$parameters[[1]]$value <- 1
nonIdentityWkt <- amCrsProjInfoConvert(
  jsonlite::toJSON(
    nonIdentityJson,
    auto_unbox = TRUE,
    digits = NA,
    pretty = FALSE
  ),
  "WKT2:2019"
)
nonIdentityCanonical <- amCrsCanonicalWkt2(nonIdentityWkt)
invalidRejected <- inherits(
  try(amCrsCanonicalWkt2("not a CRS"), silent = TRUE),
  "try-error"
)

amtest$check(
  "WKT2 CRS: identity BoundCRS unwraps but real transformations remain bound",
  isTRUE(sourceCanonical$wasBound) &&
    isTRUE(sourceCanonical$unwrappedIdentity) &&
    grepl("^PROJCRS\\[", sourceCanonical$wkt) &&
    isTRUE(nonIdentityCanonical$wasBound) &&
    !isTRUE(nonIdentityCanonical$unwrappedIdentity) &&
    grepl("^BOUNDCRS\\[", nonIdentityCanonical$wkt) &&
    isTRUE(invalidRejected)
)

crsExport <- amGrassNS(location = "demo", mapset = "demo", {
  project <- amRandomName(
    prefix = "test_crs_wkt2_",
    n = 8,
    cleanString = TRUE
  )
  projectPath <- file.path(pathDB, project)

  tryCatch(
    {
      demDirectory <- file.path(tempdir(), amRandomName("crs_wkt2_dem_"))
      dir.create(demDirectory)
      demPath <- file.path(demDirectory, basename(sourceDem))
      file.copy(sourceDem, demPath)

      dem <- data.frame(
        datapath = demPath,
        name = basename(demPath),
        size = file.info(demPath)$size,
        stringsAsFactors = FALSE
      )
      amProjectCreateFromDem(dem, project)

      # Recreate the legacy CRS state, then force the validation step to fail.
      legacyWktPath <- tempfile(fileext = ".wkt")
      writeLines(sourceWkt, legacyWktPath)
      amGrassSessionUpdate(
        location = project,
        mapset = "PERMANENT",
        resetRegion = FALSE
      )
      execGRASS("g.proj", wkt = legacyWktPath, flags = "c")
      amGrassSessionUpdate(
        location = project,
        mapset = project,
        resetRegion = FALSE
      )

      metadataFiles <- unique(c(
        list.files(
          file.path(projectPath, "PERMANENT"),
          pattern = "^(PROJ_|WIND$|DEFAULT_WIND$)",
          full.names = TRUE
        ),
        file.path(projectPath, project, "WIND")
      ))
      metadataFiles <- metadataFiles[file.exists(metadataFiles)]
      checksumBefore <- tools::md5sum(metadataFiles)

      smokeEnvironment <- environment(amProjectCrsExportSmokeTest)
      originalSmokeTest <- amProjectCrsExportSmokeTest
      rollbackError <- NULL
      tryCatch(
        {
          assign(
            "amProjectCrsExportSmokeTest",
            function() stop("injected CRS validation failure"),
            envir = smokeEnvironment
          )
          rollbackError <- tryCatch(
            {
              amProjectEnsureWkt2()
              NULL
            },
            error = function(e) conditionMessage(e)
          )
        },
        finally = {
          assign(
            "amProjectCrsExportSmokeTest",
            originalSmokeTest,
            envir = smokeEnvironment
          )
        }
      )
      checksumAfter <- tools::md5sum(metadataFiles)
      rollbackExact <- !is.null(rollbackError) &&
        grepl("project_crs_migration_failed", rollbackError, fixed = TRUE) &&
        identical(unname(checksumBefore), unname(checksumAfter))

      migration <- amProjectEnsureWkt2()
      migratedWkt <- amGrassProjectWkt()
      migratedCanonical <- amCrsCanonicalWkt2(migratedWkt)

      vectorSource <- list.files(
        file.path(testFilesDir, "vector_facility"),
        pattern = "\\.(shp|dbf|shx|prj)$",
        full.names = TRUE
      )
      vectorDirectory <- file.path(
        tempdir(),
        amRandomName("crs_wkt2_vector_")
      )
      dir.create(vectorDirectory)
      vectorPaths <- file.path(vectorDirectory, basename(vectorSource))
      file.copy(vectorSource, vectorPaths)
      vectorMain <- vectorPaths[grepl("\\.shp$", vectorPaths)]
      vectorName <- paste0("vFacility", config$sepClass, project)
      amUploadVector(vectorMain, vectorName, vectorPaths, "test")

      output <- tempfile(fileext = ".gpkg")
      execGRASS(
        "v.out.ogr",
        input = vectorName,
        output = output,
        format = "GPKG",
        flags = c("overwrite", "m", "s")
      )
      exported <- sf::st_read(output, quiet = TRUE)

      list(
        rollbackExact = rollbackExact,
        migrated = isTRUE(migration$migrated),
        canonical = amCrsIsWkt2(migratedWkt) &&
          !isTRUE(migratedCanonical$wasBound),
        exported = file.exists(output) && nrow(exported) > 0,
        error = NULL
      )
    },
    error = function(e) {
      list(
        rollbackExact = FALSE,
        migrated = FALSE,
        canonical = FALSE,
        exported = FALSE,
        error = conditionMessage(e)
      )
    },
    finally = {
      amGrassSessionUpdate(
        location = "demo",
        mapset = "demo",
        resetRegion = FALSE
      )
      if (dir.exists(projectPath)) {
        unlink(projectPath, recursive = TRUE)
      }
    }
  )
})

amtest$check(
  "WKT2 project migration: rollback is exact and OGR export succeeds",
  isTRUE(crsExport$rollbackExact) &&
    isTRUE(crsExport$migrated) &&
    isTRUE(crsExport$canonical) &&
    isTRUE(crsExport$exported),
  ifelse(is.null(crsExport$error), "", crsExport$error)
)

legacyFormatPattern <- paste0("(?i)proj", "[._ -]?", "4")
applicationFiles <- c(
  list.files("tools/R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("modules", pattern = "\\.R$", recursive = TRUE, full.names = TRUE),
  list.files("tests", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
)
legacyFormatHits <- applicationFiles[vapply(
  applicationFiles,
  function(path) {
    any(grepl(legacyFormatPattern, readLines(path, warn = FALSE), perl = TRUE))
  },
  logical(1)
)]

amtest$check(
  "WKT2 policy: application R code contains no legacy CRS format references",
  length(legacyFormatHits) == 0,
  paste(legacyFormatHits, collapse = ", ")
)
