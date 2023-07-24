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


#' Convert files using ogr2ogr ( replace gdalUtils::ogr2ogr )
#'
#'
amOgrConvert <- function(
  fileIn,
  fileOut,
  layerName = NULL,
  format = "GPKG",
  toSrs = NULL,
  update = FALSE,
  append = FALSE,
  overwrite = FALSE) {
  args <- c()
  if (!is.null(format)) {
    strFormat <- sprintf("-f '%s'", format)
    args <- c(args, strFormat)
  }
  if (!is.null(layerName)) {
    strName <- sprintf("-nln '%s'", layerName)
    args <- c(args, strName)
  }
  if (!is.null(toSrs)) {
    strProj <- sprintf("-t_srs '%s'", toSrs)
    args <- c(args, strProj)
  }
  if (update) {
    args <- c(args, "-update")
  }
  if (append) {
    args <- c(args, "-append")
  }
  if (overwrite) {
    args <- c(args, "-overwrite")
  }
  # Ensure file paths are properly escaped
  fileIn <- shQuote(fileIn)
  fileOut <- shQuote(fileOut)
  args <- c(args, fileOut)
  args <- c(args, fileIn)

  system2("ogr2ogr", args, wait = TRUE)
}
