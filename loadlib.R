# load all packages at once, at the begining of the server function.
# TODO: load packages inside functions def, and put 

# install from git repository:


# installation instruction :

# R >3.2
# grass 7.0.0
# V8 (for geojsonio, see https://github.com/jeroenooms/v8)

#installation method
library(tools)


#library(stringr) #string manipulation. #NOTE: where this is used ?

# packrat::install_github("rstudio/shiny")
library(shiny)
# install.packages("devtools")
library(devtools)
# packrat::install_github("fxi/AccessMod_leaflet-shiny")
library(leaflet) # used in GIS preview
# install.packages("R.utils")
library(R.utils) # used in amReadLogs to read last subset lines
#install.packages("rgrass7")
library(rgrass7) # R interface to GRASS GIS
#install.packages("data.table")
library(data.table) # provide fast tabular data manipulation #NOTE: Used only in referral analysis ! use dplyr ?
#install.packages("raster")
library(raster) # raster manipulation, import, get info without loading file.
#install.packages("gdalUtils")
library(gdalUtils) # complete access to system GDAL. 
#install.packages("maps")
library(maps) # map display. Used in project mondue
#install.packages("RSQLite")
library(RSQLite) # R interface to DBI library for SQLITE. Used to check grass db without grass.
#install.packages("plyr")
library(plyr) # ldply in handson table (amHandson)
#install.packages("pingr")
library(pingr) # ping utility to check of repository is available in update process.
# packrat::install_github("rstudio/shinydashboard")
library(shinydashboard) # admin LTE/bootstrap template
# packrat::install_github("ropensci/geojsonio")
library(geojsonio) # geojson process. Used in gis preview
# packrat::install_github("leeper/rio")
library(rio) #Swiss-army knife for data I/O
# install.packages("rgeos")
library(rgeos) # used in GIS preview for gintersection


# other library to load, depends on..
#"evaluate"  "httr"      "roxygen2"  "devtools"  "geojsonio"
library("stringr")


