#!/bin/sh

echo "\
  library(shiny);\
  appPath = Sys.getenv('SHINY_PATH_APP');\
  appPort = Sys.getenv('SHINY_PORT');\
  hostname = Sys.getenv('HOSTNAME');\
  host = '0.0.0.0';\
  setwd(appPath);\
  runApp('.',host=host,launch.browser=FALSE,port=as.numeric(appPort))" > run.R

