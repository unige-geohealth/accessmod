# load all packages at once, at the begining of the server function.
# TODO: load packages inside functions def, and put 

# install from git repository:


# installation instruction :

# R >3.2
# grass 7.0.0
# V8 (for geojsonio, see https://github.com/jeroenooms/v8)

#
# for reproducibility and updates, use checkpoints. 
# Update are handled in amFunctions.R -> amUpdateApp
#
library(checkpoint)

if(! dir.exists(config$checkPointPath) ){
  print("checkoint not yet initialised, this will take a while")
  dir.create(config$checkPointPath)
  checkpoint(config$checkPointDate)
}else{
  checkpoints = list.files(config$checkPointPath,recursive=F,pattern="\\d{4}-\\d{2}-\\d{2}")
 .libPaths(c(checkpoint:::checkpointPath(max(checkpoints),"~"),.libPaths()))
}

#
# WARNING devtools and load_all mess with data.table object ! 
#

library(tools)
library(shiny)
library(leaflet) # used in GIS preview
library(R.utils) # used in amReadLogs to read last subset lines
library(rgrass7) # R interface to GRASS GIS
library(data.table) # provide fast tabular data manipulation #NOTE: Used only in referral analysis ! use dplyr ?
library(raster) # raster manipulation, import, get info without loading file.
library(gdalUtils) # complete access to system GDAL. 
library(maps) # map display. Used in project mondue
library(RSQLite) # R interface to DBI library for SQLITE. Used to check grass db without grass.
library(plyr) # ldply in handson table (amHandson)
library(shinydashboard) # admin LTE/bootstrap template
library(geojsonio) # geojson process. Used in gis preview
library(rio) #Swiss-army knife for data I/O
library(rgeos) # used in GIS preview for gintersection
library(stringr) # NOTE: is this used ? 
library(digest)


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



