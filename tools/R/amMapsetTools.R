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


#' Reset AccessMod region
#' @param {Character} rasters Rasters to set the region
#' @param {Character} vectors vectors to set the region
amRegionSet <- function(rasters = character(0), vectors = character(0)) {
  hasRasters <- !isEmpty(rasters)
  hasVectors <- !isEmpty(vectors)

  if (!hasRasters && !hasVectors) {
    return
  }

  rasterAlign <- ifelse(hasRasters, rasters[1], character(0))

  execGRASS("g.region", flags = "d")

  execGRASS("g.region",
    raster = rasters,
    vector = vectors,
    align = config$mapDem
    # zoom = config$mapDem
  )
}

#' Reset AccessMod region
#'
amRegionReset <- function() {
  amRegionSet(config$mapDem)
}

#' Get all existing mapset
#'
#' @note : only allowed g.mapset. Other mapset operation should be
#'         made using am* functions
#'
#'
amMapsetGetAll <- function() {
  allMapset <- execGRASS("g.mapset", flags = "l", intern = T)
  strsplit(allMapset, " ")[[1]]
}

#' Create new mapset
#' create new mapset, and optionaly switch to it
#'
#' @param {Character} Name of another mapset
#'
amMapsetCreate <- function(mapset, switch = FALSE) {
  allMapsets <- amMapsetGetAll()
  currMapset <- amGrassSessionGetMapset()

  skip <- isEmpty(mapset) || mapset %in% allMapsets || mapset == currMapset

  if (skip) {
    return()
  }

  pathMapset <- system(
    sprintf("echo $GISDBASE/$LOCATION_NAME/%s", mapset),
    intern = T
  )

  #
  # GIS_LOCK is required by g.mapset, and not read from 
  # current grass pseudo session
  #
  execGRASS("g.mapset", flags = "c", mapset = mapset)
  amGrassSessionUpdate(mapset = mapset)

  # Double quote path : store as is. No hard coding / absolute path.
  dbPath <- "'$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'"
  execGRASS("db.connect", driver = "sqlite", database = dbPath)

  if (!switch) {
    amGrassSessionUpdate(mapset = currMapset)
  }

  return(mapset)
}

#' Remove a mapset by name
#'
#' @param {Character} mapset Mapset to remove
#' @param {Character} stringCheck Security Regex test before removing
#'
amMapsetRemove <- function(mapset, stringCheck = "^tmp_", location = NULL) {
  location <- ifelse(isEmpty(location), amGrassSessionGetLocation(), location)

  isValid <- amIsValidMapsetLocation(mapset, location) &&
    mapset != location &&
    grepl(stringCheck, mapset)

  if (!isValid) {
    strErr <- sprintf(
      "amMapsetRemove: mapset \"%s\" in location \"%s\" can't be removed",
      mapset,
      location
    )
    warning(strErr)
    return()
  }
  cmdPath <- sprintf("echo $GISDBASE/%s/%s", location, mapset)
  mapsetPath <- system(cmdPath, intern = T)
  unlink(mapsetPath, recursive = T)
}




#' Remove multiple mapset by name
#'
#' @param {Character} pattern
#'
amMapsetRemoveAll <- function(pattern = "^tmp_") {
  tmpMapset <- amMapsetGetAll()
  for (m in tmpMapset) {
    if (grepl(pattern, m)) {
      amMapsetRemove(m, pattern)
    }
  }
}

#' Create con object for SQLite
#' Based on LOCATION_NAME, GISDBASE env + mapset name or MAPSET env.
#'
#' @param {Character} mapset Optional mapset name. Default is $MAPSET
#' @return {RSQLite} dbCon object
#'
amMapsetGetDbCon <- function(mapset = NULL) {
  pathGrass <- amGrassSessionGetEnv("GISDBASE")
  location <- amGrassSessionGetEnv("LOCATION_NAME")
  if (isEmpty(mapset)) {
    mapset <- amGrassSessionGetEnv("MAPSET")
  }
  sqlitePath <- file.path(pathGrass, location, mapset, "sqlite.db")
  dbCon <- dbConnect(RSQLite::SQLite(), sqlitePath)
  return(dbCon)
}


#' get request from mapset map db
#'
#' @param {Character} mapset Mapset to remove
#' @param {Character} layer Table name
#' @param {Character} query
#'
amMapsetDbGetQuery <- function(mapset, layer, query = NULL) {
  out <- data.frame()
  if (is.null(query)) query <- paste0("SELECT * FROM ", layer)

  dbCon <- amMapsetGetDbCon(mapset)
  on_exit_add({
    dbDisconnect(dbCon)
  })

  out <- dbGetQuery(dbCon, query)

  return(out)
}
