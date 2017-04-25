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
  library(pkgName,logical.return=T,character.only=T)
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
      library("tools", logical.return=T,character.only=T)
      , library("shiny", logical.return=T,character.only=T)
      # used in GIS preview
      , library("leaflet", logical.return=T,character.only=T) 
      # used in amReadLogs to read last subset lines
      , library("R.utils", logical.return=T,character.only=T)       
      # R interface to GRASS GIS
      , library("rgrass7", logical.return=T,character.only=T) 
      # provide fast tabular data manipulation #NOTE: Used only in referral analysis ! use dplyr ?
      , library("data.table", logical.return=T,character.only=T)       
      # raster manipulation, import, get info without loading file.
      , library("raster", logical.return=T,character.only=T)   
      # complete access to system GDAL. 
      , library("gdalUtils", logical.return=T,character.only=T) 
      # map display. Used in project mondue
      , library("maps", logical.return=T,character.only=T)
      # R interface to DBI library for SQLITE. Used to check grass db without grass.
      , library("RSQLite", logical.return=T,character.only=T)
      # ldply in handson table (amHandson, logical.return=T,character.only=T)
      , library("plyr", logical.return=T,character.only=T)
      # admin LTE/bootstrap template
      , library("shinydashboard", logical.return=T,character.only=T)
      # geojson process. Used in gis preview
      , library("geojsonio", logical.return=T,character.only=T) 
      #Swiss-army knife for data I/O
      , library("rio", logical.return=T,character.only=T) 
      # used in GIS preview for gintersection
      , library("rgeos", logical.return=T,character.only=T) 
      , library("stringr", logical.return=T,character.only=T)
      , library("digest", logical.return=T,character.only=T)
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





