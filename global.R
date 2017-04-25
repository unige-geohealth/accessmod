#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/

#
# Init config list, checkpoint date and package installation
#

library(checkpoint)

#
# shortcut to test if package exists
#

library <- function(pkgName=NULL){
  base::library(pkgName,logical.return=T,character.only=T)
}


# Option list for packge provisionning
#
opt <- list(
  # Path to the checkpoint installation
  pathFull = normalizePath("~/.am5/.checkpoint",mustWork=F),
  pathBase =  normalizePath("~/.am5/",mustWork=F),
  # Date of the CRAN checkpoint
  date = "2016-11-30",
  # Version of R used. 
  version = paste(R.version$major,R.version$minor,sep="."),
  platform = R.version$platform,
  packageOk = FALSE,
  libraryOk = FALSE
  )

opt$libPaths = c(
  file.path(
    opt$pathFull,opt$date,
    "lib",
    opt$platform,
    opt$version
    ),
  file.path(
    opt$pathFull,
    paste0("R-",opt$version)
    )
  )

opt$libraryOk = all(
  sapply(
    opt$libPaths,
    dir.exists
    )
  )

if( opt$libraryOk ){
  .libPaths( opt$libPaths )

  # dependencies
  opt$packagesOk <- all(
    c(
      library("tools")
      , library("shiny")
      # used in GIS preview
      , library("leaflet")
      # used in amReadLogs to read last subset lines
      , library("R.utils")
      # R interface to GRASS GIS
      , library("rgrass7")
      # provide fast tabular data manipulation #NOTE: Used only in referral analysis ! use dplyr ?
      , library("data.table")
      # raster manipulation, import, get info without loading file.
      , library("raster")
      # complete access to system GDAL. 
      , library("gdalUtils")
      # map display. Used in project mondue
      , library("maps")
      # R interface to DBI library for SQLITE. Used to check grass db without grass.
      , library("RSQLite")
      # ldply in handson table (amHandson, logical.return=T,character.only=T)
      , library("plyr")
      # admin LTE/bootstrap template
      , library("shinydashboard")
      # geojson process. Used in gis preview
      , library("geojsonio")
      #Swiss-army knife for data I/O
      , library("rio")
      # used in GIS preview for gintersection
      , library("rgeos")
      , library("stringr")
      , library("digest")
      )
    )
}

if( !isTRUE(opt$packagesOk) || !isTRUE(opt$libraryOk) ){

  warning("Packges list or library path is not set, this could take a while")

  dir.create(
    path=opt$pathFull,
    recursive=TRUE,
    showWarnings=FALSE
    )

  checkpoint(
    snapshotDate = opt$date,
    checkpointLocation = opt$pathBase,
    scanForPackages = TRUE
    )
}




#
# load configuration file
#

source("config/config-app.R")

#
# WARNING devtools and load_all mess with data.table object ! 
#
source('tools/R/amFunctions.R') 
source('tools/R/amProgress.R')
source('tools/R/amDataManage.R')
source('tools/R/amAnalysisZonal.R')
source('tools/R/amAnalysisCatchment.R')
source('tools/R/amAnalysisCapacity.R')
source('tools/R/amAnalysisReferral.R')
source('tools/R/amAnalysisScalingUp.R')
source('tools/R/amHandson.R')
source('tools/R/amUi.R')





