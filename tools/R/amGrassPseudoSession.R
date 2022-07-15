#
# Instead of creating a session that relies on env variables,
# set env variable before each system2/system command to
# create pseudo GRASS session.
#

#
# Default am_grass object
#
# am_grass <- list(
# gisdbase = Sys.getenv("GISDBASE"),
# gisrc = tempfile(),
# mapset = "demo",
# location_name = "demo",
# env = environment()
# )

#' Grass namespaced session
#'
#' @param expr {Expression} Callback to evaluate
#' @param gisdbase {Character} GRASS db path / location (default = from sys env)
#' @param location {Character} Existing location
#' @param mapset {Character} Existing mapset
#' @param gisrc {Character} Path to gisrc ( default = temporary )
amGrassNS <- function(
  expr,
  gisdbase = NULL,
  mapset = "demo",
  location = "demo",
  gisrc = NULL,
  resetRegion = TRUE
) {
  if (isEmpty(gisdbase)) {
    gisdbase <- Sys.getenv("GISDBASE")
  }

  ns_env <- parent.frame()

  ns_env$am_grass <- list(
    gisrc = gisrc,
    mapset = mapset,
    location_name = location,
    gisdbase = gisdbase,
    env = ns_env
  )

  amGrassSessionUpdate(
    mapset = mapset,
    location = location,
    resetRegion = resetRegion
  )

  eval(expr)
}

#' Get am_grass current object
#'
#' @return am_grass
amGrassSessionGet <- function() {
  amg <- dynGet("am_grass", list(), inherits = T)
  return(amg)
}

#' Get am_grass current item
#'
#' @param name {Character} Name of the env variable. E.g. GISRC
#' @return am_grass
amGrassSessionGetEnv <- function(name) {
  amg <- amGrassSessionGet()
  item <- amg[[tolower(name)]]
  return(item)
}

#' Get current location
#'
#' @return location
amGrassSessionGetLocation <- function() {
  amGrassSessionGetEnv("location_name")
}

#' Get current mapset
#'
#' @return mapset
amGrassSessionGetMapset <- function() {
  amGrassSessionGetEnv("mapset")
}

#' Validate mapset  / location
#'
#' @param location Location to check
#' @param mapset Mapset to check
amIsValidLocation <- function(location) {
  cmdPath <- sprintf("echo $GISDBASE/%s", location)
  path <- system(cmdPath, intern = T)
  valid <- dir.exists(path)
  return(valid)
}
amIsValidMapsetLocation <- function(mapset, location = NULL) {
  location <- ifelse(isEmpty(location), amGrassSessionGetLocation(), location)
  cmdPath <- sprintf("echo $GISDBASE/%s/%s", location, mapset)
  path <- system(cmdPath, intern = T)
  valid <- dir.exists(path)
  return(valid)
}

#' Update am_grass object and gisrc
#'
#' @param location {Character} Existing location
#' @param mapset {Character} Existing mapset
#' @param resetRegion {Boolean} Update region file using default
#' @param overwriteMode {Boolean} Set GRASS_OVERWRITE mode
#' @return am_grass
amGrassSessionUpdate <- function(
  mapset = NULL,
  location = NULL,
  resetRegion = TRUE,
  overwriteMode = FALSE
) {
  amg <- amGrassSessionGet()

  if (!isEmpty(amg$gisrc) && file.exists(amg$gisrc)) {
    unlink(amg$gisrc)
  }

  amg_new <- list(
    gisrc = tempfile(),
    gisdbase = amg$gisdbase,
    gis_lock = round(runif(1) * 10000),
    mapset = ifelse(isEmpty(mapset), amg$mapset, mapset),
    location_name = ifelse(isEmpty(location), amg$location_name, location),
    grass_overwrite = ifelse(
      isEmpty(overwriteMode),
      amg$grass_overwrite,
      ifelse(overwriteMode, 1, 0)
    ),
    env = amg$env
  )

  gisrcValue <- list(
    "GISDBASE" = amg_new$gisdbase,
    "LOCATION_NAME" = amg_new$location,
    "MAPSET" = amg_new$mapset,
    "GIS_LOCK" = amg_new$gis_lock,
    "GRASS_OVERWRITE" = amg_new$grass_overwrite
  )

  write.dcf(gisrcValue, file = amg_new$gisrc)

  assign(
    "am_grass",
    amg_new,
    envir = amg$env
  )

  if (resetRegion) {
    amRegionReset()
  }
}

#' Check if curent session is valid
#'
#' TODO: add more checks
#' @return Boolean
amGrassSessionIsValid <- function() {
  gisrc <- amGrassSessionGetEnv("gisrc")
  return(!isEmpty(gisrc) && file.exists(gisrc))
}

#' Stop if session is not valid
#'
amGrassSessionStopIfInvalid <- function() {
  isValid <- amGrassSessionIsValid()
  if (!isValid) {
    stop(ams("srv_session_required"))
  }
}
