# file limitation.
options(shiny.maxRequestSize = 200*1024^2)
library(tools)
library(raster)
library(rgdal)
library(maps)
library(R.utils)
library(spgrass6)
library(gdalUtils)
library(htmltools)
#library(shinyTable) # install_github("shinyTable", "trestletech")
library(shinysky)# devtools::install_github("AnalytixWare/ShinySky")


# source files path
modPath=normalizePath('modules/')
funPath=normalizePath('fun/')
constPath=normalizePath('const/')



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
  
  
  mapList<-reactive({
    iL<-input$location
    iN<-input$mapNew
    # take dependencies on other action. 
    input$btnAddStackRoad
    input$btnAddStackBarrier
    input$btnAddStackLcv
    input$btnMerge
    input$delVect
    input$delRast
    input$btnCreateTimeCostMap

    #if(!is.null(iL) && !iL=='select' && !iL==''){
    if(file.exists(grassRcFile)){
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
    mapList=NULL
    }
    
    
  })
  
  
  
  
  
})
