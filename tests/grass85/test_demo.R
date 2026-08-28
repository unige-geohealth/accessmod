print("Testing GRASS 8.5 Structured Output")

grass85 <- amGrassNS(location = "demo", mapset = "demo", {
  raster <- "rPopulation__demo_patients"
  zones <- "rLandCover__demo"
  vector <- "vFacility__demo"
  outerMaskName <- "tmp__grass85_outer_mask_test"
  innerMaskName <- "tmp__grass85_inner_mask_test"

  maps <- amGrassList(type = "raster", pattern = raster)
  stats <- amGrassRasterStats(raster)
  zonal <- amGrassRasterStats(raster, zones = zones)
  categories <- amGetRasterCategory(zones)
  rasterMeta <- amRasterMeta(raster)
  vectorInfo <- amGetTableFeaturesCount(vector)
  sampled <- amGrassVectorRasterValues(vector, raster)
  region <- amGrassRegionMeta()
  wkt <- amGrassProjectWkt()
  mapMeta <- amMapMeta()
  outsideRasterQuery <- amRastQueryByLatLong(
    coord = c(x = 0, y = 0),
    rasterName = raster,
    projOrig = mapMeta$orig$proj,
    projDest = mapMeta$latlong$proj
  )

  nullRaster <- "tmp__grass85_null_test"
  nullRasterResult <- tryCatch({
    execGRASS(
      "r.mapcalc",
      expression = paste0(nullRaster, " = null()"),
      flags = "overwrite"
    )
    list(
      stats = amGrassRasterStats(nullRaster),
      sum = amGetRasterStat(nullRaster, "sum"),
      empty = amRastIsEmpty(nullRaster)
    )
  }, finally = {
    rmRastIfExists(nullRaster)
  })

  tmpMapset <- amRandomName("tmp_mapset_grass85")
  mapsetWarnings <- character(0)
  mapsetCreated <- tryCatch({
    withCallingHandlers(
      amMapsetCreate(tmpMapset, switch = FALSE),
      warning = function(w) {
        mapsetWarnings <<- c(mapsetWarnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    amMapsetExists(tmpMapset)
  }, finally = {
    if (amMapsetExists(tmpMapset)) {
      amMapsetRemove(tmpMapset)
    }
  })

  previousMask <- amGrassSessionGetMask()
  masked <- amGrassMaskNS({
    inner <- amGrassMaskNS({
      list(
        active = amGrassSessionGetMask(),
        exists = amRastExists(innerMaskName),
        count = amGetRasterStat(raster, "n")
      )
    }, raster = raster, maskName = innerMaskName)

    list(
      outerActive = amGrassSessionGetMask(),
      outerExists = amRastExists(outerMaskName),
      inner = inner,
      innerRemoved = !amRastExists(innerMaskName)
    )
  }, raster = zones, maskName = outerMaskName)

  list(
    maps = maps,
    stats = stats,
    zonal = zonal,
    categories = categories,
    rasterMeta = rasterMeta,
    vectorInfo = vectorInfo,
    sampled = sampled,
    region = region,
    wkt = wkt,
    outsideRasterQuery = outsideRasterQuery,
    nullRaster = nullRasterResult,
    nullRasterRemoved = !amRastExists(nullRaster),
    mapsetCreated = mapsetCreated,
    mapsetWarnings = mapsetWarnings,
    tmpMapsetRemoved = !amMapsetExists(tmpMapset),
    previousMask = previousMask,
    masked = masked,
    restoredMask = amGrassSessionGetMask(),
    outerMaskRemoved = !amRastExists(outerMaskName)
  )
})

amtest$check(
  "GRASS 8.5: g.list JSON has the normalized map contract",
  nrow(grass85$maps) == 1 &&
    identical(names(grass85$maps), c("name", "mapset", "type", "fullname"))
)

amtest$check(
  "GRASS 8.5: r.univar JSON supports scalar and zonal statistics",
  all(c("n", "non_null_cells", "null_cells", "mean", "sum") %in% names(grass85$stats)) &&
    all(c("zone", "label", "sum") %in% names(grass85$zonal)) &&
    nrow(grass85$zonal) > 0
)

amtest$check(
  "GRASS 8.5: r.univar JSON preserves empty-raster statistics",
  nrow(grass85$nullRaster$stats) == 1 &&
    identical(grass85$nullRaster$stats$n, 0L) &&
    identical(grass85$nullRaster$stats$non_null_cells, 0L) &&
    identical(grass85$nullRaster$stats$null_cells, grass85$nullRaster$stats$cells) &&
    all(is.na(grass85$nullRaster$stats[c("min", "max", "sum")])) &&
    isTRUE(grass85$nullRaster$sum == 0) &&
    isTRUE(grass85$nullRaster$empty) &&
    isTRUE(grass85$nullRasterRemoved)
)

amtest$check(
  "GRASS pseudo-session: temporary mapset creation emits no missing-lock warning",
  isTRUE(grass85$mapsetCreated) &&
    length(grass85$mapsetWarnings) == 0 &&
    isTRUE(grass85$tmpMapsetRemoved)
)

amtest$check(
  "GRASS 8.5: raster, vector, category, sample, region, and WKT readers are structured",
  all(c("class", "label") %in% names(grass85$categories)) &&
    grass85$rasterMeta$datatype %in% c("CELL", "FCELL", "DCELL") &&
    nrow(grass85$vectorInfo) > 0 &&
    all(c("cat", "val") %in% names(grass85$sampled)) &&
    identical(grass85$region$LOCATION_NAME, "demo") &&
    identical(grass85$region$MAPSET, "demo") &&
    grepl("PROJCRS|GEOGCRS", grass85$wkt)
)

amtest$check(
  "GRASS 8.5: operation-scoped mask is active, restored, and removed",
  identical(grass85$masked$outerActive, "tmp__grass85_outer_mask_test") &&
    isTRUE(grass85$masked$outerExists) &&
    identical(grass85$masked$inner$active, "tmp__grass85_inner_mask_test") &&
    isTRUE(grass85$masked$inner$exists) &&
    grass85$masked$inner$count > 0 &&
    isTRUE(grass85$masked$innerRemoved) &&
    identical(grass85$restoredMask, grass85$previousMask) &&
    isTRUE(grass85$outerMaskRemoved)
)

amtest$check(
  "GRASS 8.5: raster query outside its extent returns null placeholders",
  nrow(grass85$outsideRasterQuery) == 1 &&
    identical(grass85$outsideRasterQuery$value, "-") &&
    identical(grass85$outsideRasterQuery$label, "-")
)
