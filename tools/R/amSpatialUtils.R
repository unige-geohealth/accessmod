
amExtentsMatch <- function(x, y, dist) {
  x_sf <- st_as_sf(x)
  y_sf <- st_as_sf(y)
  extent_match <- isTRUE(st_is_within_distance(
    x = x_sf,
    y = y_sf,
    0,
    sparse = FALSE
  )[[1]])
}
