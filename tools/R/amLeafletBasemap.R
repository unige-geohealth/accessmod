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

# Normalize Leaflet bounds at the R/JavaScript boundary. During Shiny startup,
# both the viewport and project metadata can briefly be NULL or incomplete.
# Passing those values to fitBounds() or GRASS turns them into NA coordinates.
amLeafletNormalizeBounds <- function(bounds) {
  if (is.null(bounds)) {
    return(NULL)
  }

  values <- suppressWarnings(as.numeric(unlist(
    bounds[c("west", "south", "east", "north")],
    use.names = FALSE
  )))

  if (
    length(values) != 4 ||
      any(!is.finite(values)) ||
      values[1] >= values[3] ||
      values[2] >= values[4]
  ) {
    return(NULL)
  }

  as.list(setNames(values, c("west", "south", "east", "north")))
}

amLeafletProjectBounds <- function(mapMeta) {
  if (is.null(mapMeta)) {
    return(NULL)
  }

  ext <- mapMeta$latlong$bbx$ext
  amLeafletNormalizeBounds(list(
    west = ext$x$min,
    south = ext$y$min,
    east = ext$x$max,
    north = ext$y$max
  ))
}

amMapTilerTileSpec <- function(basemap, apiKey = config$mapApiKey) {
  spec <- amMapTilerBasemaps[[basemap]]
  if (is.null(spec)) {
    stop(sprintf("Unknown MapTiler basemap: %s", basemap))
  }

  spec$url <- sprintf(
    "https://api.maptiler.com/maps/%s/{z}/{x}/{y}{r}.%s?key=%s",
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
