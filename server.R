#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# server.R : coordinate server task :
# loading package and modules, config file, serving dynamic UI
# 
# Depends on config/config.R for customisation in a specific environement.

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



# main function 
shinyServer(function(input, output, session) { 
  
  
  for(f in list.files(funPath)){
    source(file.path(funPath,f),local=T)
  } 
  for(f in list.files(modPath)){
    source(file.path(modPath,f),local=T)
  } 
  for(f in list.files(constPath)){
    source(file.path(constPath,f),local=T)
  } 
  
 # reactive map list with multiple dependencies on action buttons  
  mapList<-reactive({
    iL<-input$location
    iN<-input$mapNew
    # take dependencies on other action. 
    t<-input$btnAddStackRoad
    t<-input$btnAddStackBarrier
    t<-input$btnAddStackLcv
    t<-input$btnMerge
    t<-input$delVect
    t<-input$delRast
    t<-input$btnCreateTimeCostMap
    gisLock<-get.GIS_LOCK()
    if(!is.null(gisLock) && !gisLock=="" ){
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
