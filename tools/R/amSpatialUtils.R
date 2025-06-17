#' Check if Extents Match
#'
#' Check if x contains at least one point in y
#'
#' @param x A spatial object convertible to an sf object.
#' @param y A spatial object convertible to an sf object.
#' @return Logical value indicating if y contains at leaset one point of x
amExtentsMatch <- function(x, y) {
  x_sf <- st_as_sf(x)
  y_sf <- st_as_sf(y)
  contains <- st_contains(x_sf, y_sf, sparse = FALSE)
  return(any(contains))
}


#' Shift a Point by Given Offsets
#'
#' This internal function shifts a spatial point by specified x and y offsets.
#'
#' @param point A spatial point object convertible to an sf object.
#' @param offset_x A numeric value specifying the offset in the x direction.
#' @param offset_y A numeric value specifying the offset in the y direction.
#' @return An sf object representing the shifted point.
amShiftPoint <- function(point, offset_x, offset_y) {
  sf_point <- st_as_sf(point)

  coords <- st_coordinates(sf_point)

  # Add the offset to the coordinates
  coords[1] <- coords[1] + as.numeric(offset_x)
  coords[2] <- coords[2] + as.numeric(offset_y)

  # Create a new sf object with the shifted coordinates
  point_shifted <- st_sfc(st_point(coords), crs = st_crs(sf_point))

  # Convert back to sf object
  sf_point_shifted <- st_as_sf(point_shifted)

  return(sf_point_shifted)
}
