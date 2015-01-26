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

# load base packages.
library(shiny)
library(devtools)
library(R.utils)
# List of packages to load (or install from github)
packagesCran= c(
  "tools", # base tools and file utilities 
  "htmltools", # html tools, compagnion of shiny.
  "devtools", # development tools
  "raster", #class and function for raster map
  "rgdal", #intern gdal command
  "maps", # download and display generic maps
#  "R.utils", # additional R commands for package development
  "spgrass6", # interface between R and grass.
  "gdalUtils", # launch system gdal command from R
  "RSQLite",
  "gdata"
  )

# List of packages to load (or install from github)
packagesGithub<-c(
  #'openxlsx'='awalker89/openxlsx',
  'shinysky'='AnalytixWare/ShinySky', # additional shiny features : handsontable.js, ...
  'shinyBS'='ebailey78/shinyBS' # additional shiny style : buttons, loading, etc..
  )


# source files path
modPath=normalizePath('modules/')
funPath=normalizePath('fun/')
configPath=normalizePath('config/')


# server function.
shinyServer(function(input, output, session){ 
  # load function path
  for(f in list.files(funPath)){
    source(file.path(funPath,f),local=T)
  } 


  # load config files
  for(f in list.files(configPath)){
    source(file.path(configPath,f),local=T)
  } 

 # package manager load or install
  packageManager(pkgCran=packagesCran,pkgGit=packagesGithub,libPath=rLibLoc)




  # create reactive listener : hold UI reactive values not dependant from input or output.
  listen<-reactiveValues()

  # create reactive object to hold variables in module "manage data" 
  mapMetaList<-reactiveValues()

  # set liste$gislock to NULL
  listen$gisLock<-NULL
  # set an empty tagList in listen reactive values.
  listen$reactiveStyle<-tagList()
  #listen$reactiveClass<-tagList()
  # if reactive tagList is updated, render as UI.
  output$updateStyle<-renderUI({
    listen$reactiveStyle
  })
# output$toggleClassList<-renderUI({
#    listen$toggleClassList
#  })


  # reset mapMetaList if gisLock change.
  observe({  
    listen$gisLock 
    mapMetaList<-reactiveValues()
  })


  # reactive project list 
  # updated every time gislock change.
  projectList<-reactive({
    gLock<-listen$gisLock
    pL<-grassListLoc(grassDataBase) 
  }) 

  # invalidate if a set of input element are updated.
  # updated only if dataList is requested and if gislock is active
  dataList<-reactive({
    if(!is.null(listen$gisLock)){
      #input$navList
      input$btnAddStackRoad
      input$btnAddStackBarrier
      input$btnAddStackLcv
      input$btnMerge
      input$btnCreateTimeCostMap
      #input$mapNew
      #input$navList
      listen$deleteData
      listen$uploadData

      # get list of table in db
      sqlexpr<-"select name from sqlite_master where type='table' AND name like 'table_%' "
      tables<-dbGetQuery(listen$dbCon,sqlexpr)$name
      # filter

      dataList<-list(
        table=tables,
        vect=execGRASS('g.mlist',type='vect',intern=TRUE),
        rast=execGRASS('g.mlist',type='rast',intern=TRUE),
        road=execGRASS('g.mlist',type='vect',pattern=paste0('road',sepTagPrefix,'*'),intern=TRUE),
        barrier=execGRASS('g.mlist',type='vect',pattern=paste0('barrier',sepTagPrefix,'*'),intern=TRUE),
        hf=execGRASS('g.mlist',type='vect',pattern=paste0('health_facilities',sepTagPrefix,'*'),intern=TRUE),
        lcv=execGRASS('g.mlist',type='rast',pattern=paste0('land_cover',sepTagPrefix,'*'),intern=TRUE),
        pop=execGRASS('g.mlist',type='rast',pattern=paste0('population',sepTagPrefix,'*'),intern=TRUE),
        stack=execGRASS('g.mlist',type='rast',pattern=paste0('^stack_*'),intern=TRUE),
        merged=execGRASS('g.mlist',type='rast',pattern=paste0('^merged',sepTagPrefix,'*'),intern=TRUE)
        ) 
    }else{
      dataList=list(
        vect=""
        )
    }
  })



  # source modules files.
  for(f in list.files(modPath)){
    source(file.path(modPath,f),local=T)
  } 
  })

