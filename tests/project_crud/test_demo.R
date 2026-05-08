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

tmpOverlappingZoneShp <- function() {
  tmpDir <- file.path(tempdir(), amRandomName())
  dir.create(tmpDir)

  meta <- amMapMeta()
  bbox <- ext(meta$bbxSp$orig)
  width <- bbox[2] - bbox[1]
  height <- bbox[4] - bbox[3]
  size <- min(width, height) / 8
  overlap <- size / 1000000
  x0 <- bbox[1] + width / 4
  y0 <- bbox[3] + height / 4

  poly_1 <- st_polygon(list(matrix(c(
    x0, y0,
    x0 + size, y0,
    x0 + size, y0 + size,
    x0, y0 + size,
    x0, y0
  ), ncol = 2, byrow = TRUE)))

  poly_2 <- st_polygon(list(matrix(c(
    x0 + size - overlap, y0,
    x0 + 2 * size - overlap, y0,
    x0 + 2 * size - overlap, y0 + size,
    x0 + size - overlap, y0 + size,
    x0 + size - overlap, y0
  ), ncol = 2, byrow = TRUE)))

  zones <- st_sf(
    zone_id = c("z1", "z2"),
    geometry = st_sfc(poly_1, poly_2, crs = st_crs(meta$orig$proj))
  )

  shp <- file.path(tmpDir, "overlapping_zone.shp")
  st_write(zones, shp, quiet = TRUE)
  list.files(tmpDir, full.names = TRUE)
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
  # 4. IMPORT VECTOR: overlapping polygons into test project
  # ============================================================

  vZoneName <- paste0("vZone", config$sepClass, testProjectName)
  zoneFiles <- tmpOverlappingZoneShp()
  zoneMain <- zoneFiles[grepl("\\.shp$", zoneFiles)]

  tryCatch({
    amUploadVector(zoneMain, vZoneName, zoneFiles, "test")
    topo <- amGetTableFeaturesCount(vZoneName, types = c("areas", "lines"))
    nLines <- topo$count[topo$type == "lines"]
    if (length(nLines) == 0) nLines <- 0

    amtest$check(
      "project_crud: import overlapping polygon vector without stray lines",
      amVectExists(vZoneName) && isTRUE(nLines == 0),
      sprintf("Expected vector '%s' to exist with no line primitives after import", vZoneName)
    )
  }, error = function(e) {
    amtest$check("project_crud: import overlapping polygon vector without stray lines", FALSE, e$message)
  })

  roadFiles <- tmpOverlappingZoneShp()
  roadMain <- roadFiles[grepl("\\.shp$", roadFiles)]
  vRoadName <- paste0("vRoad", config$sepClass, testProjectName)

  invalidRoadImport <- tryCatch({
    amUploadVector(roadMain, vRoadName, roadFiles, "test")
    FALSE
  }, error = function(e) {
    grepl("Invalid vector geometry", e$message)
  })

  amtest$check(
    "project_crud: reject polygon upload for road vector class",
    invalidRoadImport,
    "Expected polygon vector import as vRoad to fail geometry validation"
  )

  # ============================================================
  # 5. IMPORT SHAPE: overlapping catchments into feature collection storage
  # ============================================================

  vCatchmentName <- paste0("vCatchment", config$sepClass, testProjectName)
  catchmentFiles <- tmpOverlappingZoneShp()
  catchmentMain <- catchmentFiles[grepl("\\.shp$", catchmentFiles)]

  tryCatch({
    amUploadShape(catchmentMain, vCatchmentName, catchmentFiles, "test")
    catchmentPaths <- amGetShapesList(
      pattern = sprintf("^%s\\.", amRegexEscape(vCatchmentName))
    )
    catchmentPath <- catchmentPaths[[vCatchmentName]]
    catchments <- sf::st_read(catchmentPath, quiet = TRUE)

    amtest$check(
      "project_crud: import catchment feature collection as geopackage shape",
      isTRUE(length(catchmentPaths) == 1) &&
        isTRUE(file_ext(catchmentPath) == "gpkg") &&
        isTRUE(nrow(catchments) == 2) &&
        !amVectExists(vCatchmentName),
      sprintf("Expected shape '%s' to be stored as GPKG outside GRASS", vCatchmentName)
    )
  }, error = function(e) {
    amtest$check("project_crud: import catchment feature collection as geopackage shape", FALSE, e$message)
  })

  invalidCatchmentFiles <- tmpCopyShp(file.path(testFilesDir, "vector_facility"))
  invalidCatchmentMain <- invalidCatchmentFiles[grepl("\\.shp$", invalidCatchmentFiles)]
  vCatchmentNewName <- paste0("vCatchmentNew", config$sepClass, testProjectName)

  invalidCatchmentImport <- tryCatch({
    amUploadShape(invalidCatchmentMain, vCatchmentNewName, invalidCatchmentFiles, "test")
    FALSE
  }, error = function(e) {
    grepl("Invalid vector geometry", e$message)
  })

  amtest$check(
    "project_crud: reject point upload for catchment shape class",
    invalidCatchmentImport,
    "Expected point shape import as vCatchmentNew to fail geometry validation"
  )

  # ============================================================
  # 6. IMPORT TABLE: scenario into test project SQLite DB
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
  # 7. DELETE: remove the test project directory
  # ============================================================

  projPath <- file.path(config$pathGrassDataBase, testProjectName)

  tryCatch({
    amProjectDelete(testProjectName)
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
