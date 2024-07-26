#' Check if Extents Match Within a Given Distance
#'
#' This internal function checks if the extents of two spatial objects match 
#' within a specified distance.
#'
#' @param x A spatial object convertible to an sf object.
#' @param y A spatial object convertible to an sf object.
#' @param dist A numeric value specifying the distance .
#' @return Logical value indicating if bbox matches
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
