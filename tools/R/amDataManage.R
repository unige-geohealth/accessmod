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

#' Search and organise data into dataList reactive function
#' @param dataList Reactive values object that will hold available data list
#' @return populate dataList
#' @export
amUpdateDataListObject <- function(dataList) {

  sessionValid <- amGrassSessionIsValid()

  if (!sessionValid) {
    return()
  }

  mapset <- amGrassSessionGetEnv('mapset')

  dbCon <- amMapsetGetDbCon()

  on_exit_add({
    dbDisconnect(dbCon)
  })

  rmVectIfExists("^tmp_*")
  rmRastIfExists("^tmp_*")
  archivesSelect <- amGetArchiveList(
    config$pathArchiveGrass,
    config$archiveBaseName
  )

  # extract data of type table only from sqlite
  sqlTables <- dbListTables(dbCon)

  # get all class of type table
  tblClasses <- config$dataClass[config$dataClass$type == "table", "class"]

  # filter only tables with valid table classes
  tables <- unlist(sapply(tblClasses, function(x) {
    v <- grep(paste0(x, config$sepClass), sqlTables, value = T)
  }))

  if (length(tables) > 0) {
    # create selectize input. E.g table_model__p003 >>
    # named list element :  $`table_model [p003]`
    # value [1] "table_model__p003@p_500_m"
    tablesSelect <- amCreateSelectList(
      dName = tables,
      sepTag = config$sepTagFile,
      sepClass = config$sepClass,
      mapset = mapset
    )
  } else {
    tablesSelect <- NULL
  }
  vectorsSelect <- amCreateSelectList(
    dName = execGRASS("g.list", type = "vector", intern = TRUE),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )
  rastersSelect <- amCreateSelectList(
    dName = execGRASS("g.list", type = "raster", intern = TRUE),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )
  shapesSelect <- amCreateSelectList(
    dName = names(amGetShapesList()),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )
  configSelect <- amCreateSelectList(
    dName = names(amGetConfigList()),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )

  #
  # Clean unnamed stuff : no class found, or invalid
  #

  # Remove unamed raster
  if (!is.null(rastersSelect)) {
    rastToRemove <- rastersSelect[is.na(names(rastersSelect))]
    if (isTRUE(length(rastToRemove) > 0)) {
      sapply(rastToRemove, function(x) {
        x <- unlist(strsplit(x, config$sepMapset))[1]
        message(paste("removing unnamed file", x))
        rmRastIfExists(x)
      })
    }
  }

  # Remove unamed vector
  if (!is.null(vectorsSelect)) {
    vectToRemove <- vectorsSelect[is.na(names(vectorsSelect))]
    if (isTRUE(length(vectToRemove)) > 0) {
      sapply(vectToRemove, function(x) {
        x <- unlist(strsplit(x, config$sepMapset))[1]
        message(paste("removing unnamed file", x))
        rmVectIfExists(x)
      })
    }
  }

  # Remove unamed tables
  if (!is.null(tablesSelect)) {
    tableToRemove <- tablesSelect[is.na(names(tablesSelect))]
    if (isTRUE(length(tableToRemove) > 0)) {
      sapply(tableToRemove, function(x) {
        x <- unlist(strsplit(x, config$sepMapset))[1]
        message(paste("removing unnamed file", x))
        sql <- paste("DROP TABLE IF EXISTS", x)
        dbGetQuery(dbCon, sql)
      })
    }
  }

  # Remove unamed shapefile
  if (!is.null(shapesSelect)) {
    shapesToRemove <- unique(c(
      shapesSelect[is.na(names(shapesSelect))],
      shapesSelect[grep(paste0("^NA", config$sepClass), shapesSelect)]
    ))
    if (isTRUE(length(shapesToRemove) > 0)) {
      sToRm <- amNoMapset(shapesToRemove)
      p <- system(paste("echo", config$pathShapes), intern = T)
      allShapes <- list.files(p)
      toRm <- sapply(sToRm, function(x) {
        file.path(p, allShapes[grep(paste0("^", x), allShapes)])
      }, simplify = T)
      toRm <- toRm[file.exists(toRm)]
      if (length(toRm) > 0) {
        message(paste("removing file", paste(toRm, sep = ",")))
        file.remove(toRm)
      }
    }
  }

  # Remove unnamed configs
  if (!is.null(configSelect)) {
    configToRemove <- unique(c(
      configSelect[is.na(names(configSelect))],
      configSelect[grep(paste0("^NA", config$sepClass), configSelect)]
    ))
    if (isTRUE(length(configToRemove) > 0)) {
      rToRm <- amNoMapset(configToRemove)
      p <- system(paste("echo", config$pathConfigs), intern = T)
      allConfigs <- list.files(p)
      toRm <- sapply(rToRm, function(x) {
        file.path(p, allConfigs[grep(paste0("^", x), allConfigs)])
      }, simplify = T)
      toRm <- toRm[file.exists(toRm)]
      if (length(toRm) > 0) {
        message(paste("removing file", paste(toRm, sep = ",")))
        file.remove(toRm)
      }
    }
  }

  dataList$raster <- rastersSelect
  dataList$vector <- vectorsSelect
  dataList$shape <- shapesSelect
  dataList$table <- tablesSelect
  dataList$archive <- archivesSelect
  dataList$config <- configSelect
  
  dataList$df <- rbind(
    amDataListToDf(tablesSelect, config$sepClass, "table"),
    amDataListToDf(vectorsSelect, config$sepClass, "vector"),
    amDataListToDf(rastersSelect, config$sepClass, "raster"),
    amDataListToDf(shapesSelect, config$sepClass, "shape"),
    amDataListToDf(configSelect, config$sepClass, "config")
  )

  dataList$tags <- amGetUniqueTags(dataList$df$tag)

}
