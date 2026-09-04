#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical
#    accessibility to health care
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



config <- list()

#
# set sqlite location with
#
# find . -name "dbln" -type f -print0 | xargs -0 sed -i 's/\/srv\/shiny-server/\/home\/administrator\/Documents\/accessmod/g'
#
# Log mode %in% c("debug", "perf")
#
# config$logMode = c("debug","perf");
# config$logMode <- c("debug")
# config$logMode = c("perf");
config$logMode <- c()
# git remote
config$repository <- "https://github.com/unige-geohealth/accessmod"

config$useParallel <- TRUE

# Public client key used by MapTiler basemaps. The value is supplied by the
# runtime environment and must not be committed to the application source.
config$mapApiKey <- Sys.getenv("MAPTILER_API_KEY", unset = "")

# grass binaries and libs
config$os <- Sys.info()[["sysname"]]
config$hostname <- Sys.info()[["nodename"]]

# shiny options
config$maxUploadSize <- 2000

# default raster DEM name
config$mapDem <- "rDem__dem@PERMANENT"
# progress bar default id
config$pBarId <- "pbar"
# default time out
config$pBarTimeOut <- 0
# default vector key
config$vectorKey <- "cat"
# scaling up range of suitability
config$scalingUpRescaleRange <- c(0L, 10000L)
# character separator
config$sepTagFile <- "_"
config$sepClass <- "__"
config$sepTagRepl <- " "
config$sepMapset <- "@"

# max row table preview
config$maxRowPreview <- 50
# allowed mode of transportation. As required by r.walk.accessmod.
# KEYWORD=list(raster value=<key value to distinguish mode from speed>)
config$listTranspMod <- list(
  WALKING = list(rastVal = 1000),
  BICYCLING = list(rastVal = 2000),
  MOTORIZED = list(rastVal = 3000)
)

config$defaultTranspMode <- "WALKING"
#
# Default paths
#

# GRASS GIS paths, depends on the system configuration. Default are :
switch(config$os,
  "Linux" = {
    config$pathGrassBase70 <- "/usr/local/grass80"
  }
)


# name from the web serverver
config$archiveBaseName <- "accessmodArchive"


#
# Web prefix
#
config$prefixCache <- "cache"
config$prefixDict <- "dict"

#
# base directory.
#
config$pathModule <- normalizePath("modules/")
config$pathModuleManager <- file.path(
  config$pathModule,
  "amManageModules",
  "amServer.R"
)
config$pathGrassHome <- normalizePath("/data/logs/")
config$pathGrassDataBase <- normalizePath("/data/dbgrass/")
config$pathCacheDir <- normalizePath("/data/cache/")
config$pathGrassDemo <- normalizePath("config/data/demo")

#
# dictionary and language parameters
#
config$pathDictDir <- normalizePath("www/dictionary")
config$pathDictMain <- file.path(config$pathDictDir, "main.json")
config$pathClasses <- file.path(config$pathDictDir, "classes.json")
config$pathLanguageFile <- normalizePath(".language", mustWork = F)

#
# create directories if necessary.
#
dir.create(showWarnings = F, recursive = T, config$pathGrassDataBase)
dir.create(showWarnings = F, config$pathGrassHome)
dir.create(showWarnings = F, config$pathCacheDir)

#
# Add ressource path
#
shiny::addResourcePath(config$prefixCache, config$pathCacheDir)
shiny::addResourcePath(config$prefixDict, config$pathDictDir)

#
# default language
# NOTE: see in tools/R/amTranslate.R : a function already exists for doing this.
#
config$language <- "en"
if (file.exists(config$pathLanguageFile)) {
  language <- readLines(config$pathLanguageFile)
  if (length(language) == 1 && nchar(language) == 2) {
    config$language <- language
  }
}
config$languageDefault <- "en"
config$dictLanguages <- list(
  "English" = "en",
  "Français" = "fr",
  "Español" = "es",
  "Português" = "pt"
)
#
# NOTE: to update the dictionnary after adding language use :
#' @example amTranslateDictUpdateLanguages()
#
config$dict <- fromJSON(config$pathDictMain)
config$dataClass <- fromJSON(config$pathClasses)
config$dictReplayValidation <- fromJSON(
  "./tools/R/amAnalysisReplayValidationDict.json",
  simplifyDataFrame = F
)


#
# path to set after grass session started ( need grass env. variables )
# to retrieve correct path, use system(paste("echo",sqliteDB),intern=TRUE)
#

# sqlite database
config$pathSqliteDB <- "$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db"
# path to archives
config$pathArchiveGrass <- "$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodArchives"
# path to shapefile
config$pathShapes <- "$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodShapes"
# path to lists
config$pathConfigs <- "$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodConfigs"


# rc file
grassRcFile <- file.path(config$pathGrassHome, ".grassrc6")
# store archive in mapset. Path generated inside a GRASS environment only.
# get archive path  ex. system(paste("echo",archives),intern=TRUE)

# Log file. An empty log is valid and is handled by amReadLogs().
config$pathLog <- file.path(config$pathGrassHome, "logs.txt")
if (!file.exists(config$pathLog)) file.create(config$pathLog)

config$nLogMax <- 5000
config$nLogDefault <- 300
config$nLogRetain <- 100000
config$nLogTrimTrigger <- 120000

config$pathPerf <- file.path(config$pathCacheDir, "perf.csv")
if (!file.exists(config$pathPerf)) write("", config$pathPerf)


# global variables
config$amLocation <- ""
config$amTitle <- "Accessmod 5.0"

source("config/config-errors.R")


# verbose mode.

# file extension allowed See also validateFilExt in fun/helper.R
config$fileArchiveProjectDb <- c("am5p")
config$fileArchiveAnalysisConfig <- c("am5c")
config$fileAdf <- c("dblbnd.adf", "hdr.adf", "prj.adf", "vat.adf", "w001001.adf", "w001001x.adf")
config$fileAdfMin <- c("prj.adf", "w001001.adf", "hdr.adf")
config$fileShpExt <- c(".shp", ".dbf", ".prj", ".sbn", ".sbx", ".xml", ".shx", ".cpg")
config$fileShpExtMin <- c(".shp", ".prj", ".dbf", ".shx")
config$fileImgMin <- c(".img")
config$filesAccept <- list(
  "project" = c(sprintf(".%s", config$fileArchiveProjectDb)),
  "config" = c(sprintf(".%s", config$fileArchiveAnalysisConfig)),
  "shape" = c(".gpkg", ".sqlite", ".spatialite", config$fileShpExt),
  "vector" = c(".gpkg", ".sqlite", ".spatialite", config$fileShpExt),
  "raster" = c(".adf", ".geotiff", ".GeoTIFF", ".tiff", ".tif", ".img", ".ige"),
  "table" = c(".xls", ".csv", ".xlsx", ".ods", ".tsv", ".dta", ".psv", ".dbf", ".rds", ".RData", ".json", ".xml")
)
config$fileAcceptMultiple <- list(
  "shape" = TRUE,
  "vector" = TRUE,
  "raster" = TRUE,
  "table" = FALSE
)

#
# Proposed export forma
# -not yet in use
#
config$filesExport <- list(
  config = c(".json"),
  project = c(".am5p"),
  vector = c(".shp", ".gpkg"),
  table = c(".xlsx", ".csv"),
  raster = c(".img",'.tif')
)



#
# Set gdal type for raster
# https://grass.osgeo.org/grass77/manuals/r.out.gdal.html
#
config$rasterDataTypes <- c(
  "Byte",
  "UInt16",
  "Int16",
  "UInt32",
  "Int32",
  "Float32",
  "Float64"
)

names(config$rasterDataTypes) <- c(
  "Byte (0-255)",
  "UInt16 (integer 0 to 65'535)",
  "Int16 (integer -32'768 to 32'767 )",
  "UInt32 (integer 0 to 4'294'967'295)",
  "Int32 (integer 0 to 4'294'967'295)",
  "Float32 (float -3.4E38 to 3.4E38)",
  "Float64 (float -1.79E308 to 1.79E308)"
)

config$rasterDataTypesDefault <- "Float32"
# control table col names and type for tables
config$tableColNames <- list(
  "tScenario" = c("class", "label", "speed", "mode"),
  "tLandCover" = c("class", "label"),
  "tStackRoad" = c("class", "label"),
  "tStack" = c("class", "label"),
  "tCapacity" = c("min", "max", "label", "capacity"),
  "tExclusion" = c("layer", "buffer", "method"),
  "tSuitability" = c("factor", "layer", "weight", "options")
)

config$tableColType <- list(
  "tScenario" = c("integer", "character", "integer", "character"),
  "tLandCover" = c("integer", "character"),
  "tStackRoad" = c("integer", "character"),
  "tCapacity" = c("numeric", "numeric", "character", "numeric"),
  "tExclusion" = c("character", "numeric", "character"),
  "tSuitability" = c("character", "character", "numeric", "character")
)





# get a version grouped by class with class id as key
config$dataClassList <- dlply(config$dataClass, .(class), c)


config$defaultNoData <- "no_data"
config$defaultWithoutData <- "no_data"
config$defaultNoDataCheck <- c(config$defaultNoData, config$defaultWithoutData)
config$dynamicFacilities <- "vOutputFacility"
config$dynamicPopulation <- "rOutputPopulation"
config$dynamicLayers <- c(config$dynamicFacilities, config$dynamicPopulation)

#
# icons
#
config$iconSmall <- img(src = "logo/icons/logo24x24.png")
config$iconMedium <- img(src = "logo/icons/logo32x32.png")
config$iconLarge <- img(src = "logo/icons/logo128x128.png")
config$iconHuge <- img(src = "logo/icons/logo648x648.png")
config$iconWhoSvg <- img(src = "logo/who.svg", style = "width:100%; max-height:40px;")
config$iconWho <- img(src = "logo/icons/WHO-EN-C-H.png")
config$iconWhoSmall <- img(src = "logo/icons/WHO-EN-C-H_small.png", width = "95%")
config$helpTitle <- tags$span(icon("info-circle"), "AccessMod 5")

# order config list
config <- config[sort(names(config))]
