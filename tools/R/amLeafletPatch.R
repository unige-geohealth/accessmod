#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/


leafletPatchDependencies <- function() {
  ## in a function : call to htmlDependency in run time, not build time
  list(
    htmltools::htmlDependency(
      name = "leaflet_patch",
      version = "0.0.1",
      src = c(href="patch"),
      script = "leaflet_patch.js"
      )
    )
}

#' @export
addScale = function(map) {
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    'addScale'
    )
}

#'@export
addPng = function(map, lat1, lng1, lat2, lng2, imgUrl, group=NULL,layerId=NULL,options = list()) {

  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    'addPng',
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
setPngOpacity = function(map, layerId=NULL,opacity=1) {
  
  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    'setPngOpacity',
    layerId,
    opacity
    )
}
#'@export
addMarkersRelocate = function(map, data, group=NULL, layerId=NULL) {

  markers <- as.data.frame(data@coords)
  names(markers) <- c('lng','lat')
  markers$value <- data$amRasterValue;
  markers$id <- data$cat;

  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    'addMarkersRelocate',
    layerId,
    group,
    markers
    )
}

removeMarkersRelocate = function(map, layerId=NULL) {

  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    'removeMarkersRelocate',
    layerId
    )
}

updateMarkerRelocate = function(map,markerId,value=NULL,lat=NULL,lng=lng,group=NULL,layerId=NULL){

  map$dependencies <- c(map$dependencies, leafletPatchDependencies())
  invokeMethod(
    map,
    getMapData(map),
    'updateMarkerRelocate',
    layerId,
    group,
    markerId,
    value,
    lat,
    lng
    )

}
