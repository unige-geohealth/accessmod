#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

leafletPatchDependencies <- function() {
  ## in a function : call to htmlDependency in run time, not build time
  list(
    htmltools::htmlDependency(
      name = "leaflet_patch",
      version = "0.0.1",
      src = c(href = "modules/leaflet"),
      script = "patch.js"
    )
  )
}

#' @export
addScale <- function(map) {
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "addScale"
  )
}

#' @export
addPng <- function(map, lat1, lng1, lat2, lng2, imgUrl, group = NULL, layerId = NULL, options = list()) {
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "addPng",
    layerId,
    group,
    imgUrl,
    lat1,
    lng1,
    lat2,
    lng2,
    options
  )
}
setPngOpacity <- function(map, layerId = NULL, opacity = 1) {
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "setPngOpacity",
    layerId,
    opacity
  )
}
#' @export
addMarkersRelocate <- function(map, data, label = "cat", group = NULL, layerId = NULL) {
  markers <- as.data.frame(data@coords)
  names(markers) <- c("lng", "lat")
  markers$value <- data$amRasterValue
  markers$id <- data$cat
  markers$label <- data[[label]]

  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "addMarkersRelocate",
    layerId,
    group,
    markers
  )
}

removeMarkersRelocate <- function(map, layerId = NULL) {
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "removeMarkersRelocate",
    layerId
  )
}

updateMarkerRelocate <- function(map, markerId, value = NULL, lat = NULL, lng = lng, group = NULL, layerId = NULL) {
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    "updateMarkerRelocate",
    layerId,
    group,
    markerId,
    value,
    lat,
    lng
  )
}
