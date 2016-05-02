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

