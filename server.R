#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# server.R :
# loading package and modules, handle actions, config file, serving dynamic UI
# 
# Depends on config/config.R for customisation in a specific environement.

# load libraries. Some are pre-downloaded from a git repo!
library(tools)
library(raster)
library(rgdal)
library(maps)
library(R.utils)
library(spgrass6)
library(gdalUtils)
library(htmltools)
library(shinysky)# devtools::install_github("AnalytixWare/ShinySky")

# source files path
modPath=normalizePath('modules/')
funPath=normalizePath('fun/')
constPath=normalizePath('config/')



# main functimpNew 
shinyServer(function(input, output, session) { 

  #reactive list to hold changes in GIS config.
  locData<-reactiveValues()
  locData$gisLock<-NULL
  locData$deleteMap<-NULL
  locData$uploadMap<-NULL

  for(f in list.files(constPath)){
    source(file.path(constPath,f),local=T)
  } 

  for(f in list.files(funPath)){
    source(file.path(funPath,f),local=T)
  } 
  for(f in list.files(modPath)){
    source(file.path(modPath,f),local=T)
  } 

  

  # reactive map list with multiple dependencies on action buttons  
  mapList<-reactive({
    # update list when any of those ractive value change
    input$navList
    input$btnAddStackRoad
    input$btnAddStackBarrier
    input$btnAddStackLcv
    input$btnMerge
    input$btnCreateTimeCostMap
    input$mapNew
    input$navList
    locData$deleteMap
    locData$uploadMap
    #update only if gisLock is set
    if(!is.null(locData$gisLock)){
      mapList<-list(
        vect=execGRASS('g.mlist',type='vect',intern=TRUE),
        rast=execGRASS('g.mlist',type='rast',intern=TRUE),
        road=execGRASS('g.mlist',type='vect',pattern=paste0('road',charTagGrass,'*'),intern=TRUE),
        barrier=execGRASS('g.mlist',type='vect',pattern=paste0('barrier',charTagGrass,'*'),intern=TRUE),
        hf=execGRASS('g.mlist',type='vect',pattern=paste0('health_facilities',charTagGrass,'*'),intern=TRUE),
        lcv=execGRASS('g.mlist',type='rast',pattern=paste0('land_cover',charTagGrass,'*'),intern=TRUE),
        pop=execGRASS('g.mlist',type='rast',pattern=paste0('population',charTagGrass,'*'),intern=TRUE),
        stack=execGRASS('g.mlist',type='rast',pattern=paste0('^stack_*'),intern=TRUE),
        merged=execGRASS('g.mlist',type='rast',pattern=paste0('^merged',charTagGrass,'*'),intern=TRUE)
        ) 
    }else{
      mapList=list(
        vect=""
        )
    }
  })






})
