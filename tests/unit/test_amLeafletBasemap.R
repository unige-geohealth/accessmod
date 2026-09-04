#
# Unit tests: MapTiler basemap configuration
#

amtest$check(
  "MapTiler basemaps: all UI choices resolve to current styles",
  {
    basemaps <- unname(amMapTilerBasemapChoices)
    basemaps <- basemaps[basemaps != "empty"]
    specs <- lapply(basemaps, amMapTilerTileSpec, apiKey = "unit-test-key")
    identical(
      vapply(specs, `[[`, character(1), "style"),
      c("dataviz-v4-light", "dataviz-v4-dark", "topo-v4", "satellite-v4")
    )
  }
)

amtest$check(
  "MapTiler basemaps: tile URLs contain the retina suffix, encoded key, and correct formats",
  {
    simple <- amMapTilerTileSpec("simple", "key with/slash")
    satellite <- amMapTilerTileSpec("satellite", "unit-test-key")
    grepl("dataviz-v4-light/\\{z\\}/\\{x\\}/\\{y\\}\\{r\\}\\.png\\?key=key%20with%2Fslash$", simple$url) &&
      grepl("satellite-v4/\\{z\\}/\\{x\\}/\\{y\\}\\{r\\}\\.jpg\\?key=unit-test-key$", satellite$url)
  }
)

amtest$check(
  "MapTiler basemaps: attribution names MapTiler and OpenStreetMap",
  {
    attribution <- amMapTilerTileSpec("simple", "unit-test-key")$attribution
    grepl("MapTiler", attribution) && grepl("OpenStreetMap", attribution)
  }
)

amtest$check(
  "MapTiler basemaps: Leaflet call uses MapTiler tile geometry",
  {
    originalKey <- config$mapApiKey
    on.exit(config$mapApiKey <- originalKey, add = TRUE)
    config$mapApiKey <- "unit-test-key"
    map <- amAddMapTilerTiles(leaflet(), "simple", layerId = "baselayer")
    call <- map$x$calls[[1]]
    options <- call$args[[4]]
    identical(call$method, "addTiles") &&
      identical(call$args[[2]], "baselayer") &&
      identical(options$tileSize, 512) &&
      identical(options$zoomOffset, -1) &&
      identical(options$minZoom, 1) &&
      identical(options$maxZoom, 21) &&
      identical(options$detectRetina, FALSE) &&
      isTRUE(options$crossOrigin)
  }
)

amtest$check(
  "MapTiler basemaps: empty API key leaves provider-side failure handling",
  grepl("\\?key=$", amMapTilerTileSpec("simple", "")$url)
)

amtest$check(
  "MapTiler basemaps: unknown choices are rejected",
  inherits(try(amMapTilerTileSpec("unknown", "unit-test-key"), silent = TRUE), "try-error")
)

amtest$check(
  "Leaflet bounds: valid project metadata is normalized",
  {
    mapMeta <- list(latlong = list(bbx = list(ext = list(
      x = list(min = 30, max = 40),
      y = list(min = -20, max = -10)
    ))))
    identical(
      amLeafletProjectBounds(mapMeta),
      list(west = 30, south = -20, east = 40, north = -10)
    )
  }
)

amtest$check(
  "Leaflet bounds: startup and invalid values are rejected",
  is.null(amLeafletProjectBounds(NULL)) &&
    is.null(amLeafletNormalizeBounds(list(
      west = NA,
      south = -20,
      east = 40,
      north = -10
    ))) &&
    is.null(amLeafletNormalizeBounds(list(
      west = 40,
      south = -20,
      east = 30,
      north = -10
    )))
)
