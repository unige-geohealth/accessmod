#
# Instead of creating a session that relies on env variables,
# set env variable before each system2/system command to
# create pseudo GRASS session.
#

#
# Default am_grass object
#
am_grass <- list(
  gisdbase = Sys.getenv("GISDBASE"),
  gisrc = tempfile(),
  mapset = "demo",
  location_name = "demo",
  env = environment()
)

#' Grass namespaced session
#' @param expr {Expression} Expression to evaluate
#' @param gisdbase {Character} GRASS db path / location (default = from sys env)
#' @param location {Character} Existing location
#' @param mapset {Character} Existing mapset
#' @param gisrc {Character} Path to gisrc ( default = temporary )
amGrassNS <- function(expr,
                      gisdbase = NULL,
                      mapset = NULL,
                      location = NULL,
                      gisrc = NULL) {
  if (isEmpty(gisrc)) {
    gisrc <- tempfile()
  }

  if (isEmpty(gisdbase)) {
    gisdbase <- Sys.getenv("GISDBASE")
  }

  envbase <- parent.frame()

  assign("am_grass",
    value = list(
      gisrc = gisrc,
      mapset = mapset,
      location_name = location,
      gisdbase = gisdbase,
      env = envbase
    ), envir = envbase
  )
  on.exit({
    unlink(gisrc)
  })

  eval(expr, envir = environment())
}

#' Get am_grass current object
#' @return am_grass
amGrassSessionGet <- function() {
  amg <- dynGet("am_grass", inherits = T)
  return(amg)
}

#' Get am_grass current item
#' @param name {Character} Name of the env variable. E.g. GISRC
#' @return am_grass
amGrassSessionGetEnv <- function(name) {
  amg <- dynGet("am_grass", inherits = T)
  item <- amg[[tolower(name)]]
  return(item)
}

#' Update am_grass object and gisrc
#' @param location {Character} Existing location
#' @param mapset {Character} Existing mapset
#' @return am_grass
amGrassSessionSet <- function(mapset, location) {
  amg <- amGrassSessionGet()

  amg_new <- list(
    gisrc = amg$gisrc,
    gisdbase = amg$gisdbase,
    mapset = ifelse(isEmpty(mapset), amg$mapset, mapset),
    location_name = ifelse(isEmpty(location), amg$location_name, location),
    env = amg$env
  )

  gisrcValue <- list(
    "GISDBASE" = amg_new$gisdbase,
    "LOCATION_NAME" = amg_new$location,
    "MAPSET" = amg_new$mapset
  )

  write.dcf(gisrcValue, file = amg_new$gisrc)

  assign(
    "am_grass",
    amg_new,
    envir = amg$env
  )
}

#' Reassign system2 to make use of amGrass sessions
#' -> workaround to sessions from rgrass
#'
amReasign("base", "system2", function(...) {
  strenv <- ""
  args <- list(...)
  amg <- amGrassSessionGet()

  for (n in c("mapset", "location_name", "gisrc")) {
    if (!isEmpty(n)) {
      strenv <- paste0(strenv, toupper(n), "=", amg[[n]], ";")
    }
  }

  if (isEmpty(args$stdout)) {
    args$stdout <- TRUE
  }

  args$env <- strenv
  amDebugMsg("system2 proxy", args[[1]])
  do.call(
    "system2_orig",
    args
  )
})

#' Reassign system to make use of amGrass sessions
#' -> workaround to sessions from rgrass
#'
amReasign("base", "system", function(...) {
  strenv <- ""
  args <- list(...)
  amg <- amGrassSessionGet()

  for (n in c("mapset", "location_name", "gisrc")) {
    if (!isEmpty(n)) {
      strenv <- paste0(strenv, toupper(n), "=", amg[[n]], ";")
    }
  }
  args[[1]] <- paste0(strenv, args[[1]])

  do.call(
    "system_orig",
    args
  )
})
