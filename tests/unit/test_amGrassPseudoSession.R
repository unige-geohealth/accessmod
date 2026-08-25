#
# Unit test: GRASS pseudo-session identifiers
#
# No GRASS command is executed. The test only verifies the in-memory session
# object and generated GISRC with deliberately different project/mapset names.
#

sessionIdentifiers <- local({
  gisdbase <- tempfile("grassdb_")
  dir.create(gisdbase)
  on.exit(unlink(gisdbase, recursive = TRUE))

  amGrassNS(
    gisdbase = gisdbase,
    location = "project_a",
    mapset = "mapset_b",
    resetRegion = FALSE,
    {
      gisrc <- read.dcf(amGrassSessionGet()$gisrc)
      list(
        location = amGrassSessionGetLocation(),
        mapset = amGrassSessionGetMapset(),
        gisrcLocation = unname(gisrc[1, "LOCATION_NAME"]),
        gisrcMapset = unname(gisrc[1, "MAPSET"])
      )
    }
  )
})

amtest$check(
  "GRASS pseudo-session: location and mapset remain distinct",
  identical(
    sessionIdentifiers,
    list(
      location = "project_a",
      mapset = "mapset_b",
      gisrcLocation = "project_a",
      gisrcMapset = "mapset_b"
    )
  )
)

windRepair <- local({
  gisdbase <- tempfile("grassdb_wind_")
  projectPath <- file.path(gisdbase, "project_a")
  permanentPath <- file.path(projectPath, "PERMANENT")
  mapsetPath <- file.path(projectPath, "mapset_b")
  dir.create(permanentPath, recursive = TRUE)
  dir.create(mapsetPath, recursive = TRUE)
  on.exit(unlink(gisdbase, recursive = TRUE))

  permanentWind <- file.path(permanentPath, "WIND")
  defaultWind <- file.path(permanentPath, "DEFAULT_WIND")
  mapsetWind <- file.path(mapsetPath, "WIND")
  writeLines("source: permanent", permanentWind)
  writeLines("source: default", defaultWind)

  amGrassNS(
    gisdbase = gisdbase,
    location = "project_a",
    mapset = "mapset_b",
    resetRegion = FALSE,
    {
      writeLines("source: current", mapsetWind)
      validUnchanged <- !amMapsetRepairWindIfEmpty() &&
        identical(readLines(mapsetWind), "source: current")

      file.create(mapsetWind, showWarnings = FALSE)
      repairedFromPermanent <- amMapsetRepairWindIfEmpty() &&
        identical(readLines(mapsetWind), "source: permanent")

      unlink(mapsetWind)
      repairedMissing <- amMapsetRepairWindIfEmpty() &&
        identical(readLines(mapsetWind), "source: permanent")

      list(
        validUnchanged = validUnchanged,
        repairedFromPermanent = repairedFromPermanent,
        repairedMissing = repairedMissing
      )
    }
  )
})

amtest$check(
  "GRASS WIND repair: valid files are preserved and empty files are restored",
  all(unlist(windRepair, use.names = FALSE))
)

permanentWindRepair <- local({
  gisdbase <- tempfile("grassdb_permanent_wind_")
  permanentPath <- file.path(gisdbase, "project_a", "PERMANENT")
  dir.create(permanentPath, recursive = TRUE)
  on.exit(unlink(gisdbase, recursive = TRUE))

  permanentWind <- file.path(permanentPath, "WIND")
  defaultWind <- file.path(permanentPath, "DEFAULT_WIND")
  file.create(permanentWind, showWarnings = FALSE)
  writeLines("source: default", defaultWind)

  amGrassNS(
    gisdbase = gisdbase,
    location = "project_a",
    mapset = "PERMANENT",
    resetRegion = FALSE,
    {
      amMapsetRepairWindIfEmpty() &&
        identical(readLines(permanentWind), "source: default")
    }
  )
})

amtest$check(
  "GRASS WIND repair: PERMANENT falls back to DEFAULT_WIND",
  permanentWindRepair
)

windRepairUnavailable <- local({
  gisdbase <- tempfile("grassdb_no_wind_")
  permanentPath <- file.path(gisdbase, "project_a", "PERMANENT")
  mapsetPath <- file.path(gisdbase, "project_a", "mapset_b")
  dir.create(permanentPath, recursive = TRUE)
  dir.create(mapsetPath, recursive = TRUE)
  on.exit(unlink(gisdbase, recursive = TRUE))

  mapsetWind <- file.path(mapsetPath, "WIND")
  file.create(mapsetWind, showWarnings = FALSE)

  amGrassNS(
    gisdbase = gisdbase,
    location = "project_a",
    mapset = "mapset_b",
    resetRegion = FALSE,
    {
      !amMapsetRepairWindIfEmpty() && file.info(mapsetWind)$size == 0
    }
  )
})

amtest$check(
  "GRASS WIND repair: unavailable fallbacks leave the project untouched",
  windRepairUnavailable
)
