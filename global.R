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


#
# Dependencies
#
library(parallel)
library(tools)
library(shiny)
library(jsonlite)

# am alternate http server to manage async req (amServer.R)
library(httpuv)

# used in GIS preview
library(leaflet)

# used in
# - amReadLogs to read last subset lines
library(R.utils)

# R interface to GRASS GIS
library(rgrass7)
use_sp()

# provide fast tabular data manipulation
# NOTE: Used only in referral analysis ! use dplyr ?
library(data.table)

# raster manipulationimportget info without loading file.
library(raster)

# ldply in handson table (amHandsonlogical.return=T,character.only=T)
library(plyr)

# used for anti_join in amUpdateDataListName.
library(dplyr)

# complete access to system GDAL.
# used : gdalinfo, gdalsrsinfo, ogr2ogr ...
library(gdalUtils)

# interaction r -> gdal (writeOgr,..)
library(rgdal)

# R interface to DBI library for SQLITE. Used to check grass db without grass.
library(RSQLite)

# Used to cache values. E.g. Stack conflict validation in merge LDC
library(memoise)

# admin LTE/bootstrap template
library(shinydashboard)

# Swiss-army knife for data I/O
library(rio)

# used in GIS preview for gIntersection
library(rgeos)

# String manipulation
library(stringr)

# Stevedor : docker interface
library(stevedore)


#
# load configuration file
#
source("config/config-app.R")


#
# AccessMod functions
#
source("tools/R/amFunctions.R")
source("tools/R/amSelectizeHelpers.R")
source("tools/R/amSpeedBufferRegion.R")
source("tools/R/amUpdate.R")
source("tools/R/amGrassLeaflet.R")
source("tools/R/amTranslate.R")
source("tools/R/amExport.R")
source("tools/R/amMapsetTools.R")
source("tools/R/amFacilitiesTools.R")
source("tools/R/amProgress.R")
source("tools/R/amDebounce.R")
source("tools/R/amShinyBindings.R")
source("tools/R/amDataManage.R")
source("tools/R/amAnalysisTravelTime.R")
source("tools/R/amAnalysisZonal.R")
source("tools/R/amAnalysisCatchment.R")
source("tools/R/amAnalysisCapacity.R")
source("tools/R/amAnalysisReplay.R")
source("tools/R/amProjectImportExport.R")
source("tools/R/amAnalysisReferralParallel.R")
source("tools/R/amAnalysisTimeDist.R")
source("tools/R/amAnalysisScalingUp.R")
source("tools/R/amHandson.R")
source("tools/R/amUi.R")
source("tools/R/amUi_doubleSortableInput.R")
source("tools/R/amGdalUtil.R")
source("tools/R/amReassign.R")
source("tools/R/amGrassPseudoSession.R")
source("tools/R/amFrictionSpeed.R")
source("tools/R/amCreateDataNames.R")
source("tools/R/amServer.R")
#
# Memoize manager
#
source("tools/R/amMemoised.R")

#
# Init with demo data if no project
#
source("tools/R/amInitData.R")


#
# R options
#
options(
  shiny.maxRequestSize = config$maxUploadSize * 1024^2
)
options(tz = Sys.getenv("TZ")) # why options do not match env ?



#
# Set GRASS verbose level
#
if ("perf" %in% config$logMode) {
  Sys.setenv(GRASS_VERBOSE = -1)
} else {
  Sys.setenv(GRASS_VERBOSE = 0)
}
