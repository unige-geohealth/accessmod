#
# Instead of creating a session that relies on env variables,
# set env variable before each system2/system command to
# create pseudo GRASS session.
#

#
# Init/Test grass session 
# -> use that for creating a manual session
#
amGrassSessionTest <- function() {
  x <- amGrassNS({amGrassRegionMeta()});
  return(x$nsres == 1000) 
}


#' Grass namespaced session
#'
#' @param expr {Expression} Callback to evaluate
#' @param gisdbase {Character} GRASS db path / location (default = from sys env)
#' @param location {Character} Existing location
#' @param mapset {Character} Existing mapset
#' @param gisrc {Character} Path to gisrc ( default = temporary )
#' @param ensureWkt2 {Logical} Migrate legacy project CRS metadata to WKT2
amGrassNS <- function(
  expr,
  gisdbase = NULL,
  mapset = "demo",
  location = "demo",
  gisrc = NULL,
  resetRegion = TRUE,
  ensureWkt2 = TRUE
) {
  if (isEmpty(gisdbase)) {
    gisdbase <- Sys.getenv("GISDBASE")
  }

  ns_env <- parent.frame()

  ns_env$am_grass <- list(
    gisrc = gisrc,
    mapset = mapset,
    location_name = location,
    grass_mask = NULL,
    gisdbase = gisdbase,
    env = ns_env
  )

  amGrassSessionUpdate(
    mapset = mapset,
    location = location,
    resetRegion = resetRegion
  )

  if (isTRUE(ensureWkt2) &&
      exists("amProjectEnsureWkt2", mode = "function")) {
    amProjectEnsureWkt2()
  }

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

#' Get current project
#'
#' LOCATION_NAME remains the required GRASS environment key. This alias uses
#' the GRASS 8.5 user-facing terminology without breaking saved configurations.
amGrassSessionGetProject <- function() {
  amGrassSessionGetLocation()
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

  if (isNotEmpty(amg$gisrc) && file.exists(amg$gisrc)) {
    unlink(amg$gisrc)
  }

  amg_new <- list(
    gisrc = tempfile(),
    gisdbase = amg$gisdbase,
    gis_lock = round(runif(1) * 10000),
    mapset = ifelse(isEmpty(mapset), amg$mapset, mapset),
    location_name = ifelse(isEmpty(location), amg$location_name, location),
    grass_mask = amg$grass_mask,
    grass_overwrite = ifelse(
      isEmpty(overwriteMode),
      amg$grass_overwrite,
      ifelse(overwriteMode, 1, 0)
    ),
    env = amg$env
  )

  gisrcValue <- list(
    "GISDBASE" = amg_new$gisdbase,
    "LOCATION_NAME" = amg_new$location_name,
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

#' Get the active operation-scoped raster mask name
amGrassSessionGetMask <- function() {
  amGrassSessionGetEnv("grass_mask")
}

amGrassSessionSetMask <- function(mask = NULL) {
  amg <- amGrassSessionGet()
  amg$grass_mask <- mask
  assign("am_grass", amg, envir = amg$env)
  invisible(mask)
}

#' Evaluate an expression with an isolated GRASS raster mask
amGrassMaskNS <- function(
  expr,
  raster,
  maskcats = "*",
  inverse = FALSE,
  maskName = amRandomName("tmp__mask")
) {
  expr <- substitute(expr)
  previousMask <- amGrassSessionGetMask()
  amGrassSessionSetMask(maskName)

  on.exit({
    amGrassSessionSetMask(previousMask)
    rmRastIfExists(maskName)
  }, add = TRUE)

  rmRastIfExists(maskName)
  execGRASS(
    "r.mask",
    raster = raster,
    maskcats = maskcats,
    flags = c(if (inverse) "i", "overwrite")
  )
  eval(expr, envir = parent.frame())
}

#' Check if curent session is valid
#'
#' TODO: add more checks
#' @return Boolean
amGrassSessionIsValid <- function() {
  gisrc <- amGrassSessionGetEnv("gisrc")
  return(isNotEmpty(gisrc) && file.exists(gisrc))
}

#' Stop if session is not valid
#'
amGrassSessionStopIfInvalid <- function() {
  isValid <- amGrassSessionIsValid()
  if (!isValid) {
    stop(ams("srv_session_required"))
  }
}
