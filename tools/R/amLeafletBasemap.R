#         ___                                  __  ___           __
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ___/ /
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/

amMapTilerBasemaps <- list(
  simple = list(style = "dataviz-v4-light", extension = "png"),
  dark = list(style = "dataviz-v4-dark", extension = "png"),
  terrain = list(style = "topo-v4", extension = "png"),
  satellite = list(style = "satellite-v4", extension = "jpg")
)

amMapTilerBasemapChoices <- c(
  "Simple I" = "simple",
  "Dark" = "dark",
  "Terrain" = "terrain",
  "Satellite I" = "satellite",
  "Empty" = "empty"
)

amMapTilerTileSpec <- function(basemap, apiKey = config$mapApiKey) {
  spec <- amMapTilerBasemaps[[basemap]]
  if (is.null(spec)) {
    stop(sprintf("Unknown MapTiler basemap: %s", basemap))
  }

  spec$url <- sprintf(
    "https://api.maptiler.com/maps/%s/{z}/{x}/{y}.%s?key=%s",
    spec$style,
    spec$extension,
    URLencode(apiKey, reserved = TRUE)
  )
  spec$attribution <- paste0(
    '<a href="https://www.maptiler.com/copyright/" target="_blank">',
    '&copy; MapTiler</a> ',
    '<a href="https://www.openstreetmap.org/copyright" target="_blank">',
    '&copy; OpenStreetMap contributors</a>'
  )
  spec
}

amAddMapTilerTiles <- function(map, basemap, layerId = NULL, group = NULL) {
  spec <- amMapTilerTileSpec(basemap)
  addTiles(
    map,
    urlTemplate = spec$url,
    attribution = spec$attribution,
    layerId = layerId,
    group = group,
    options = tileOptions(
      tileSize = 512,
      zoomOffset = -1,
      minZoom = 1,
      maxZoom = 21,
      crossOrigin = TRUE
    )
  )
}
