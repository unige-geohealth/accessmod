print("Testing Accessibility Analysis")

# --------------------------------------------------------------- #
# Series of tests for demo location, with a scenario
# using only motorized transport : no effect of DEM, so
# isotropic vs anisotropic should be equal.
# --------------------------------------------------------------- #

conf <- amAnalysisReplayParseConf(
  "tests/accessibility/config_demo_motorized.json"
)

travelTime <- conf$args$outputTravelTime
maxTravelTime <- conf$args$maxTravelTime
knightMove <- c("knight", "standard")
analysis <- c("isotropic", "anisotropic")

exportDir <- file.path(tempdir(), amRandomName())
means <- list()
nulls <- list()

amGrassNS(
  location = conf$location,
  mapset = conf$mapset,
  {
    for (k in knightMove) {
      for (a in analysis) {
        #
        # Update configuration
        #
        id <- sprintf("%s_%s", k, a)
        conf$args$knightMove <- k == "knight"
        conf$args$typeAnalysis <- a

        #
        # Create output directory
        #
        pathDir <- file.path(exportDir, k, a)
        dir.create(pathDir,
          recursive = TRUE,
          showWarnings = FALSE
        )

        #
        # Start analysis
        #
        exportedDirs <- amAnalysisReplayExec(conf,
          exportDirectory = pathDir
        )
        #
        # Test files
        #
        passExport <- FALSE
        switch(a,
          "anisotropic" = {
            fileSpeed <- list.files(exportedDirs$rSpeed__test)
            fileTravel <- list.files(exportedDirs$rTravelTime__test)
            passExport <- "raster_speed_test.img" %in% fileSpeed &&
              "raster_speed_test_report.txt" %in% fileSpeed &&
              "raster_travel_time_test.img" %in% fileTravel &&
              "raster_travel_time_test_report.txt" %in% fileTravel
          },
          "isotropic" = {
            fileFriction <- list.files(exportedDirs$rFriction__test)
            fileTravel <- list.files(exportedDirs$rTravelTime__test)
            browser()
            passExport <- "raster_friction_test.img" %in% fileFriction &&
              "raster_friction_test_report.txt" %in% fileFriction &&
              "raster_travel_time_test.img" %in% fileTravel &&
              "raster_travel_time_test_report.txt" %in% fileTravel
          }
        )
        amtest$check(sprintf("Exported files %s ", id), passExport)

        #
        # Fetch stats
        #
        means[id] <- amGetRasterStat(travelTime, metric = "mean")
        nulls[id] <- amGetRasterStat(travelTime, metric = "null")
        max <- amGetRasterStat(travelTime, metric = "max")
        desc <- sprintf("Max travel time ok: %s", id)
        amtest$check(desc, max <= maxTravelTime)
      }
    }
  }
)

amtest$check(
  "Motorized scenario, knight mode, iso == aniso",
  identical(means$knight_isotropic, means$knight_anisotropic)
)

amtest$check(
  "Motorized scenario, standard mode, iso == aniso",
  identical(means$standard_isotropic, means$standard_anisotropic)
)

amtest$check(
  "Expected mean travel time, k + iso",
  identical(round(means$knight_isotropic, 3), round(29.522, 3))
)

amtest$check(
  "Expected mean travel time, s + aniso",
  identical(round(means$standard_anisotropic, 3), round(29.419, 3))
)

amtest$check(
  "Expected nulls in travel time, k + iso",
  identical(nulls$knight_isotropic, 31829)
)

amtest$check(
  "Expected nulls in travel time, k + aniso",
  identical(nulls$knight_anisotropic, nulls$knight_isotropic)
)

amtest$check(
  "Expected nulls in travel time, s + aniso",
  identical(nulls$standard_anisotropic, 32283)
)

amtest$check(
  "Expected nulls in travel time, s + iso",
  identical(nulls$standard_isotropic, nulls$standard_anisotropic)
)
