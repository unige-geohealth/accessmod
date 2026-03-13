print("Testing Project CRUD Operations")

testFilesDir <- "tests/project_crud/files"

# Generate a distinctive project name that won't conflict with pre-loaded projects
testProjectName <- amRandomName(prefix = "test", n = 8, cleanString = TRUE)

# Helper: copy only shapefile components to a temp dir
tmpCopyShp <- function(srcDir) {
  tmpDir <- file.path(tempdir(), amRandomName())
  dir.create(tmpDir)
  files <- list.files(srcDir, pattern = "\\.(shp|dbf|shx|prj)$", full.names = TRUE)
  newPaths <- file.path(tmpDir, basename(files))
  file.copy(files, newPaths)
  return(newPaths)
}

# Helper: copy a single raster file to a temp dir
tmpCopyRaster <- function(srcDir, pattern = "\\.img$") {
  tmpDir <- file.path(tempdir(), amRandomName())
  dir.create(tmpDir)
  file <- list.files(srcDir, pattern = pattern, full.names = TRUE)[[1]]
  newPath <- file.path(tmpDir, basename(file))
  file.copy(file, newPath)
  return(newPath)
}

# Initialize GRASS session – amProjectCreateFromDem will change it to testProjectName
amGrassNS(location = "demo", mapset = "demo", {

  # ============================================================
  # 1. CREATE: build new project from DEM
  # ============================================================

  demMain <- tmpCopyRaster(file.path(testFilesDir, "raster_dem"))
  newDem <- data.frame(
    datapath = demMain,
    name = basename(demMain),
    size = file.info(demMain)$size,
    stringsAsFactors = FALSE
  )

  tryCatch({
    amProjectCreateFromDem(newDem, testProjectName)
    amtest$check(
      "project_crud: create project from DEM",
      amIsValidLocation(testProjectName),
      sprintf("Expected location '%s' to exist after creation", testProjectName)
    )
  }, error = function(e) {
    amtest$check("project_crud: create project from DEM", FALSE, e$message)
  })

  # ============================================================
  # 2. IMPORT RASTER: land cover into test project
  # ============================================================

  rLandCoverName <- paste0("rLandCover", config$sepClass, testProjectName)
  lcMain <- tmpCopyRaster(file.path(testFilesDir, "raster_land_cover"))

  tryCatch({
    amUploadRaster(config, lcMain, rLandCoverName, lcMain, "rLandCover", "test")
    amtest$check(
      "project_crud: import land cover raster",
      amRastExists(rLandCoverName),
      sprintf("Expected raster '%s' to exist after import", rLandCoverName)
    )
  }, error = function(e) {
    amtest$check("project_crud: import land cover raster", FALSE, e$message)
  })

  # ============================================================
  # 3. IMPORT VECTOR: facilities into test project
  # ============================================================

  vFacilityName <- paste0("vFacility", config$sepClass, testProjectName)
  facFiles <- tmpCopyShp(file.path(testFilesDir, "vector_facility"))
  facMain <- facFiles[grepl("\\.shp$", facFiles)]

  tryCatch({
    amUploadVector(facMain, vFacilityName, facFiles, "test")
    amtest$check(
      "project_crud: import facility vector",
      amVectExists(vFacilityName),
      sprintf("Expected vector '%s' to exist after import", vFacilityName)
    )
  }, error = function(e) {
    amtest$check("project_crud: import facility vector", FALSE, e$message)
  })

  # ============================================================
  # 4. IMPORT TABLE: scenario into test project SQLite DB
  # ============================================================

  tScenarioName <- paste0("tScenario", config$sepClass, testProjectName)
  dbCon <- amMapsetGetDbCon()

  tryCatch({
    amUploadTable(
      config,
      tScenarioName,
      file.path(testFilesDir, "table_scenario", "table_scenario_demo.xlsx"),
      "tScenario",
      dbCon,
      "test"
    )
    scenTbl <- dbGetQuery(dbCon, sprintf("SELECT * FROM `%s`", tScenarioName))
    amtest$check(
      "project_crud: import scenario table",
      nrow(scenTbl) > 0,
      sprintf("Expected table '%s' to have rows after import", tScenarioName)
    )
  }, error = function(e) {
    amtest$check("project_crud: import scenario table", FALSE, e$message)
  })

  dbDisconnect(dbCon)

  # ============================================================
  # 5. DELETE: remove the test project directory
  # ============================================================

  projPath <- file.path(config$pathGrassDataBase, testProjectName)

  tryCatch({
    unlink(projPath, recursive = TRUE, force = TRUE)
    amtest$check(
      "project_crud: delete project",
      !dir.exists(projPath),
      sprintf("Expected project directory '%s' to be removed", projPath)
    )
  }, error = function(e) {
    amtest$check("project_crud: delete project", FALSE, e$message)
  })

  # Restore session to demo so subsequent tests start clean
  amGrassSessionUpdate(location = "demo", mapset = "demo", resetRegion = FALSE)
})
