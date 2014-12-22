# file limitation.
options(shiny.maxRequestSize = 200*1024^2)
library(tools)
library(raster)
#library(digest)
library(maps)
library(R.utils)
library(spgrass6)
library(gdalUtils)
library(htmltools)
#library(shinyTable) # install_github("shinyTable", "trestletech")
library(shinysky)# devtools::install_github("AnalytixWare/ShinySky")
location = ''
modPath='modules/'
funPath='fun/'



shinyServer(function(input, output, session) {  
  
 mapList<-reactive({
    iL<-input$location
    iN<-input$mapNew
    mapList<-list(
      vect=execGRASS('g.mlist',type='vect',intern=TRUE),
      rast=execGRASS('g.mlist',type='rast',intern=TRUE),
      stack=execGRASS('g.mlist',type='rast',pattern='s_*',intern=TRUE,),
      road=execGRASS('g.mlist',type='vect',pattern=paste0('road',charTagGrass,'*'),intern=TRUE),
      barrier=execGRASS('g.mlist',type='vect',pattern=paste0('barrier',charTagGrass,'*'),intern=TRUE),
      hf=execGRASS('g.mlist',type='vect',pattern=paste0('health_facilities',charTagGrass,'*'),intern=TRUE),
      lcv=execGRASS('g.mlist',type='rast',pattern=paste0('land_cover',charTagGrass,'*'),intern=TRUE),
      pop=execGRASS('g.mlist',type='rast',pattern=paste0('population',charTagGrass,'*'),intern=TRUE),
      pop=execGRASS('g.mlist',type='rast',pattern=paste0('^s_*'),intern=TRUE)
    ) 
  })
  
  
  
  for(f in list.files(funPath)){
    source(file.path(funPath,f),local=T)
  } 
  for(f in list.files(modPath)){
    source(file.path(modPath,f),local=T)
  } 

  
  

  
  

  
  
  
})
