#
# Unit tests: Leaflet/GRASS preview boundaries
#

projectBbox <- st_as_sf(st_as_sfc(st_bbox(c(
  xmin = 30,
  ymin = -20,
  xmax = 40,
  ymax = -10
), crs = st_crs("EPSG:4326"))))

outsideBbox <- st_as_sf(st_as_sfc(st_bbox(c(
  xmin = -10,
  ymin = 40,
  xmax = 0,
  ymax = 50
), crs = st_crs("EPSG:4326"))))

amtest$check(
  "Leaflet preview: a viewport outside the project returns no preview",
  is.null(amGrassLatLongPreview(
    raster = "unused",
    bbxSpLatLongLeaf = outsideBbox,
    bbxSpLatLongOrig = projectBbox,
    mapCacheDir = tempdir(),
    width = 800,
    projOrig = st_crs("EPSG:4326")$wkt,
    projDest = st_crs("EPSG:4326")$wkt
  ))
)
