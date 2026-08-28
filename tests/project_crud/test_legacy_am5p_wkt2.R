print("Testing legacy project archive WKT2 migration")

legacyArchiveImport <- local({
  sourceProject <- file.path(pathDB, "demo")
  stagingDirectory <- tempfile("legacy_archive_")
  dir.create(stagingDirectory)
  archiveProject <- amRandomName(
    prefix = "legacy_archive_project_",
    n = 8,
    cleanString = TRUE
  )
  importedProject <- amRandomName(
    prefix = "legacy_imported_project_",
    n = 8,
    cleanString = TRUE
  )
  importedPath <- file.path(pathDB, importedProject)

  on.exit({
    if (dir.exists(importedPath)) {
      unlink(importedPath, recursive = TRUE)
    }
    unlink(stagingDirectory, recursive = TRUE)
  })

  file.copy(sourceProject, stagingDirectory, recursive = TRUE)
  stagedProject <- file.path(stagingDirectory, "demo")
  renamedProject <- file.path(stagingDirectory, archiveProject)
  file.rename(stagedProject, renamedProject)
  file.rename(
    file.path(renamedProject, "demo"),
    file.path(renamedProject, archiveProject)
  )
  unlink(file.path(renamedProject, "PERMANENT", "PROJ_WKT"))

  archive <- file.path(
    tempdir(),
    paste0(amRandomName("legacy_wkt2_"), ".", fileExtProject)
  )
  on.exit(unlink(archive), add = TRUE)
  amZip(archive, renamedProject)

  result <- tryCatch(
    {
      amProjectImport(archive, importedProject)
      amGrassNS(
        location = importedProject,
        mapset = importedProject,
        {
          wkt <- amGrassProjectWkt()
          list(
            imported = dir.exists(importedPath),
            storedWkt = file.exists(file.path(
              importedPath,
              "PERMANENT",
              "PROJ_WKT"
            )),
            canonical = amCrsIsWkt2(wkt),
            exportable = amProjectCrsExportSmokeTest(),
            error = NULL
          )
        }
      )
    },
    error = function(e) {
      list(
        imported = FALSE,
        storedWkt = FALSE,
        canonical = FALSE,
        exportable = FALSE,
        error = conditionMessage(e)
      )
    }
  )

  result
})

amtest$check(
  "Legacy project archive: import migrates CRS to canonical WKT2",
  isTRUE(legacyArchiveImport$imported) &&
    isTRUE(legacyArchiveImport$storedWkt) &&
    isTRUE(legacyArchiveImport$canonical) &&
    isTRUE(legacyArchiveImport$exportable),
  ifelse(is.null(legacyArchiveImport$error), "", legacyArchiveImport$error)
)
