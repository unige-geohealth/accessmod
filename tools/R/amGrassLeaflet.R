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

#' amGrassLatLongPreview: Generates a preview of a raster map with given bounding boxes
#'
#' @param raster Character. The name of the raster map to preview (e.g., "land_cover").
#' @param bbxSpLatLongLeaf Spatial object. Bounding box of the current region in latitude/longitude (result of bbxLeafToSp(input$<map>_bounds)).
#' @param bbxSpLatLongOrig Spatial object. Bounding box of the current region in the original projected format.
#' @param mapCacheDir Character. Relative path to the cache directory (e.g., "../data/cache"). This directory must exist.
#' @param width Numeric. Maximum resolution of the final output file.
#' @param projOrig Character. Original projection string (e.g., "EPSG:4326").
#' @param projDest Character. Destination projection string.
#'
#' @return List. A list containing paths to the PNG map and legend, and the bounding box matrix.
#'
#' @examples
#' \dontrun{
#' result <- amGrassLatLongPreview(
#'   raster = "land_cover",
#'   bbxSpLatLongLeaf = bbxLeafToSp(input$map_bounds),
#'   bbxSpLatLongOrig = some_proj_bbox,
#'   mapCacheDir = "../data/cache",
#'   width = 800,
#'   projOrig = "EPSG:4326",
#'   projDest = "EPSG:3857"
#' )
#' }
#'
#' @export
amGrassLatLongPreview <- function(
  raster = NULL,
  bbxSpLatLongLeaf,
  bbxSpLatLongOrig,
  mapCacheDir,
  width,
  projOrig,
  projDest
) {
  #
  # simple time diff
  #

  if (is.null(bbxSpLatLongLeaf) || is.null(bbxSpLatLongOrig)) {
    return(NULL)
  }

  #
  # get intersection between leaflet extent and project extent
  #
  bbxSpLatLongInter <- st_intersection(
    bbxSpLatLongOrig,
    bbxSpLatLongLeaf
  )

  if (is.null(bbxSpLatLongInter)) {
    return(NULL)
  }

  bbxMatLatLongInter <- st_bbox(bbxSpLatLongInter)
  bbxMatLatLongInterRound <- round(bbxMatLatLongInter, 3)

  #
  # Cache file names
  #
  cacheMap <- file.path(
    mapCacheDir,
    paste0(
      raster,
      "__",
      paste0(
        bbxMatLatLongInterRound,
        collapse = "_"
      ), ".png"
    )
  )
  cacheLegend <- file.path(
    mapCacheDir, paste0("legend_", raster, ".png")
  )
  #
  # If the cache file does not exists, create it.
  #
  if (!file.exists(cacheMap)) {
    tryCatch(
      finally = {
        amRegionReset()
        rmRastIfExists("MASK*")
        rmRastIfExists("tmp_*")
        rmVectIfExists("tmp_*")
      }, {
        #
        # Get bbox in current projection
        #
        bbxSpProjInter <- st_transform(
          bbxSpLatLongInter,
          projOrig
        )
        bbxMatProjInter <- st_bbox(bbxSpProjInter)

        #
        # Set resolution and extent
        #
        res <- (bbxMatProjInter$xmax - bbxMatProjInter$xmin) / width

        execGRASS("g.region",
          e = paste(bbxMatProjInter$xmax),
          w = paste(bbxMatProjInter$xmin),
          s = paste(bbxMatProjInter$ymin),
          n = paste(bbxMatProjInter$ymax),
          res = paste(ceiling(res))
        )

        #
        # r.out.png : faster than r.out.gdal and more reliable
        #
        execGRASS("r.out.png",
          input = raster,
          output = cacheMap,
          compression = 0,
          flags = c("overwrite", "t")
        )
      }
    )
  }

  return(list(
    pngMap = cacheMap,
    pngLegend = cacheLegend,
    bbx = bbxMatLatLongInter
  ))
}


amRastQueryByLatLong <- function(coord, rasterName, projOrig, projDest, nullValue = "-") {
  # Convert coordinates to sf POINT
  coord_sf <- st_as_sf(
    data.frame(x = coord["x"], y = coord["y"]),
    coords = c("x", "y"),
    crs = projDest
  )

  # Reproject to the original raster CRS
  coord_transformed <- st_transform(coord_sf, crs = projOrig)

  # Extract coordinates (assuming you want the coordinates for r.what input)
  coord_max <- st_coordinates(coord_transformed)[1, ]

  suppressWarnings({
    val <- execGRASS("r.what",
      map = rasterName,
      coordinates = c(coord_max[1], coord_max[2]),
      flags = c("c", "quiet", "f"),
      null_value = nullValue,
      intern = TRUE
    )
    val <- amCleanTableFromGrass(val, header = FALSE)
  })

  names(val) <- c("long", "lat", "lab", "value", "label")

  return(val)
}

# conversion of leaflet bounding box to sp object:
#  Leaflet has no bounding limit and sp does, crop leaflet box.
# to use this as standard bouding box, set CRS.
amBbxLeafToSf <- function(bbxLeaflet) {
  if (is.null(bbxLeaflet)) {
    return(null)
  }
  crsDest <- "EPSG:4326"

  east <- pmax(pmin(bbxLeaflet$east, 180), -180)
  west <- pmax(pmin(bbxLeaflet$west, 180), -180)
  south <- pmax(pmin(bbxLeaflet$south, 90), -90)
  north <- pmax(pmin(bbxLeaflet$north, 90), -90)

  r <- rast()
  ext(r) <- c(west, east, south, north)
  vExt <- vect(ext(r))
  crs(vExt) <- crsDest
  return(st_as_sf(vExt))
}
