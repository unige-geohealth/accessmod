#! /bin/bash

# instead of using submodule with source code, which we don't want to modify in this context, get archive version of R packages. AccessMod will install them in local lib if missing at launch.

# custom version of leaflet for shiny
wget -O leaflet.tar.gz https://github.com/fxi/AccessMod_leaflet-shiny/archive/master.tar.gz

# Rccp library, used for rio package.
wget -O Rcpp.tar.gz https://github.com/RcppCore/Rcpp/archive/master.tar.gz

# read/write geojson/topojson
wget -O geojsonio.tar.gz https://github.com/ropensci/geojsonio/archive/master.tar.gz

#read xl files. Used by rio
wget -O readxl.tar.gz https://github.com/hadley/readxl/archive/master.tar.gz

#read any table data.
wget -O rio.tar.gz https://github.com/leeper/rio/archive/master.tar.gz

# bring Admin LTE bootstrap 3 template to shiny
wget -O shinydashboard.tar.gz https://github.com/rstudio/shinydashboard/archive/master.tar.gz
