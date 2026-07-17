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
