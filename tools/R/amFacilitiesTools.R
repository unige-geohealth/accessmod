#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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


amGetFacilitiesTable <- function(mapHf, mapMerged, mapPop, mapDem, tblSpeed, dbCon, format = FALSE) {
  # mapHf : vector map of facilities
  # map merged : raster landcover merged map
  # mapPop : raster map of population
  # Return value :
  # Facilitie attribute table with additional columns :
  # amOnBarrier : check if facilities is located on barrier (no landcover value)
  # amOnZero : check if facilities is located on landcover cell with speed of zero
  # amCatLandCover : get value of merged land cover for each facilities.
  # amPopCell : count population in cells where facilities are located.


  has_merged <- amRastExists(mapMerged)
  has_facilities <- amVectExists(mapHf)
  has_pop <- amRastExists(mapPop)
  has_speed <- isNotEmpty(tblSpeed)
  has_dem <- amRastExists(mapDem)


  if (!has_merged || !has_facilities) {
    return(NULL)
  }

  tblAttribute <- dbGetQuery(dbCon, paste("select * from", mapHf))

  if (nrow(tblAttribute) == 0) {
    return(NULL)
  }
  #
  # check if HF are located on barrier by querying merged land cover values.
  #
  tbl <- amGetFacilitiesTableWhatRast(mapHf, mapMerged)
  names(tbl) <- c("cat", "amCatLandCover")
  tbl$amOnBarrier <- is.na(tbl$amCatLandCover)

  if (has_speed) {
    classWithZero <- tblSpeed[tblSpeed$speed == 0, ]$class
    tbl$amOnZero <- tbl$amCatLandCover %in% classWithZero
  } else {
    tbl$amOnZero <- "unset"
  }
  #
  # count population on facilities sites
  #
  if (has_pop) {
    tblPop <- amGetFacilitiesTableWhatRast(mapHf, mapPop)
    names(tblPop) <- c("cat", "amPopCell")
    tblPop[is.na(tblPop$amPopCell), "amPopCell"] <- 0
    #
    # merge results
    #
    tbl <- merge(tbl, tblPop, by = "cat")
  }

  #
  # Check DEM values
  #
  if (has_dem) {
    tblDem <- amGetFacilitiesTableWhatRast(mapHf, mapDem)
    names(tblDem) <- c("cat", "amDemValue")
    tblDem$amOutsideDem <- is.na(tblDem$amDemValue)
    #
    # merge results
    #
    tbl <- merge(tbl, tblDem, by = "cat")
  }

  #
  # merge accessmod table with attribute table
  #
  tbl <- merge(tbl, tblAttribute, by = "cat")

  #
  # Format
  #
  if (isTRUE(format)) {

    # Choose which columns display first.
    colOrder <- unique(c(
      config$vectorKey,
      "amOnBarrier",
      if (has_speed) "amOnZero",
      if (has_dem) "amOutsideDem",
      names(tbl)
    ))

    tbl <- tbl[order(tbl$amOnBarrier, decreasing = TRUE), colOrder]
    tbl$amOnBarrier <- ifelse(sapply(tbl$amOnBarrier, isTRUE), "yes", "no")
    if (has_dem) {
      tbl <- tbl[order(tbl$amOutsideDem, decreasing = TRUE), colOrder]
      tbl$amOutsideDem <- ifelse(sapply(tbl$amOutsideDem, isTRUE), "yes", "no")
    }
    if (has_speed) {
      tbl <- tbl[order(tbl$amOnZero, decreasing = TRUE), colOrder]
      tbl$amOnZero <- ifelse(sapply(tbl$amOnZero, isTRUE), "yes", "no")
    }
  }

  return(tbl)
}


amGetRasterValueAtPoint <- function(inputPoint, inputRaster) {
  data <- execGRASS("v.what.rast",
    map = inputPoint,
    raster = inputRaster,
    flags = "p",
    intern = T
  )

  if (isEmpty(data)) {
    tbl <- data.frame(V1 = character(0), v2 = character(0))
  } else {
    tbl <- read.table(
      text = data, ,
      sep = "|",
      stringsAsFactors = FALSE,
      na.strings = "*",
      colClasses = c("integer", "numeric")
    )
  }

  names(tbl) <- c("cat", "val")
  return(tbl)
}


amGetFacilitiesTableWhatRast <- function(mapHf, mapRaster) {
  on_exit_add({
    amRegionReset()
  })

  amRegionSet(mapRaster, mapHf)

  tbl <- amGetRasterValueAtPoint(mapHf, mapRaster)

  return(tbl)
}
