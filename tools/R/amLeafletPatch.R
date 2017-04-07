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

