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

pok <- function(pkgName=NULL){
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
      pok("tools")
      , pok("shiny")
      , pok("leaflet") # used in GIS preview
      , pok("R.utils") # used in amReadLogs to read last subset lines
      , pok("rgrass7") # R interface to GRASS GIS
      , pok("data.table") # provide fast tabular data manipulation #NOTE: Used only in referral analysis ! use dplyr ?
      , pok("raster") # raster manipulation, import, get info without loading file.
      , pok("gdalUtils") # complete access to system GDAL. 
      , pok("maps") # map display. Used in project mondue
      , pok("RSQLite") # R interface to DBI pok for SQLITE. Used to check grass db without grass.
      , pok("plyr") # ldply in handson table (amHandson)
      , pok("shinydashboard") # admin LTE/bootstrap template
      , pok("geojsonio") # geojson process. Used in gis preview
      , pok("rio") #Swiss-army knife for data I/O
      , pok("rgeos") # used in GIS preview for gintersection
      , pok("stringr") # NOTE: is this used ? 
      , pok("digest")
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





