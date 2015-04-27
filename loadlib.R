# load all packages at once, at the begining of the server function.
# this is could be an expensive task ! 
# TODO: load packages inside functions def, and put 
library(devtools)
library(R.utils) # used in amReadLogs to read last subset lines
library(devtools)
library(rgrass7) # R interface to GRASS GIS
library(htmltools) # html tools. NOTE: check for unused package
library(data.table) # provide fast tabular data manipulation #NOTE:Used only in referral analysis. Check if dplyr could do the job. 
library(raster) # raster manipulation, import, get info without loading file.
library(rgdal) # striped R version of GDAL. NOTE: redundant with gdalutils ?
library(gdalUtils) # complete access to system GDAL. 
library(rgeos) # R interface to geometry engine geos. NOTE: check for unused package
library(maps) # map display. Used in project mondue
library(RSQLite) # R interface to DBI library for SQLITE. Used to check grass db without grass.
library(plyr) # ldply in handson table (amHandson)
library(pingr) # ping utility to check of repository is available in update process.
library(leaflet) # ! fork of shiny leaflet: fxi/AccessMod_leaflet-shiny
library(shinydashboard) # admin LTE/bootstrap template
library(geojsonio) # geojson process. Used in gis preview
library(rio) #Swiss-army knife for data I/O
library(tools)
library(shinyBS)
