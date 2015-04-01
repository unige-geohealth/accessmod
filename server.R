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
library(shinydashboard)
library(devtools)
#library(R.utils)
# List of packages to load (or install from github)
packagesCran= c(
  "tools", # base tools and file utilities
  "htmltools", # html tools, companion of shiny.
  "data.table", # faster than data.frame for large data processing
  "devtools", # development tools
  "raster", #class and function for raster map
  "rgdal", #intern gdal command
  "rgeos",# map manipulation
  "maps", # download and display generic maps
  "rjson", # read json formated file (e.g geojson)
  "rgrass7",
  "gdalUtils", # launch system gdal command from R
  "RSQLite", # interface to SQLITE database
  "gdata", # enable compatibility with read.xls (and xlsx files)
  "plyr" # data manipulation
  )

#require(compiler)
#enableJIT(3)

# List of packages to load (or install from github)
packagesGithub<-c(
  'leaflet'="fxi/AccessMod_leaflet-shiny",
  'shinydashboard'="rstudio/shinydashboard",# UI
  'geojsonio'="ropensci/geojsonio"
  )

# source files path
modPath=normalizePath('modules/server/')
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
  packageManager(pkgCran=packagesCran,pkgGit=packagesGithub,libPath=libPath)
  # reactive value to hold event and logic 
  listen<-reactiveValues()
  # reactive object to hold variables in module "manage data"
  dataMetaList<-reactiveValues()
  # reactive values to store list of data set
  dataList<-reactiveValues()
  # reactive values to store list of project
  projectList<-reactiveValues()
  # set liste$gislock to NULL
  listen$gisLock<-NULL
  # if a gisLock exists, extract archive path from archiveGrass (contains grass env. variable)
  observe({
    if(!is.null(listen$gisLock)){
      # archiveGrass need grass environment variables, as defined in config.R
      archivePath<-system(paste('echo',archiveGrass),intern=TRUE) 
      # if archive directory is not existant, create it.
      dir.create(archivePath,showWarnings = FALSE)
      archivePath<-normalizePath(archivePath) 
      #add ressource for shiny 
      addResourcePath(archiveBaseName,archivePath)
      listen$archivePath=archivePath
    }else{
      listen$archivePath=NULL
    }
  },priority=110)

  # set data list
  observe({
    amErrorAction(title='data list observer',{
      # gisLock change when grass is initialised : startup and locatio change
      gLock<-listen$gisLock 
      # dataListUpdate change on demand, when new map are created: function dataListUpdate().
      listen$dataListUpdate
      # if gisLock is set, allow querying database.
      if(!is.null(gLock)){
        amDebugMsg('Update dataList: search in grass and sqlite. GisLock=',gLock)
        rmVectIfExists('^tmp_*')
        rmRastIfExists('^tmp_*')
        sqlexpr<-"select name from sqlite_master where type='table' AND name like 'table_%' "
        archive<-list.files(listen$archivePath)
        archive<-archive[order(archive,decreasing=T)]
        mapset<-isolate(listen$mapset)
        tables<-dbGetQuery(isolate(listen$dbCon),sqlexpr)$name
        if(length(tables)>0){
          tables<-amCreateSelectList(
            dName=tables,
            sepTag=sepTagFile,
            sepClass=sepClass,
            mapset=mapset)
        }else{
          tables=NULL
        }
        vectors<-amCreateSelectList(
          dName=execGRASS('g.list',type='vector',intern=TRUE),
          sepTag=sepTagFile,
          sepClass=sepClass,
          mapset=mapset
          )

        rasters<-amCreateSelectList(
          dName=execGRASS('g.list',type='raster',intern=TRUE),
          sepTag=sepTagFile,
          sepClass=sepClass,
          mapset=mapset
          )

        dataList$raster<-rasters
        dataList$vector<-vectors
        dataList$table<-tables
        dataList$archive<-archive

        dataList$df<-rbind(
          amDataListToDf(rasters,sepClass,'raster'),
          amDataListToDf(vectors,sepClass,'vector'),
          amDataListToDf(tables,sepClass,'table')
          )

      }else{
        amDebugMsg('DataList: no gisLock. ')
      }

})

  },priority=100)

  #init base project list
  projectList$loc<-grassListLoc(grassDataBase)
  # if a new project is set, update.
  observe({ 
    listen$projectListUpdate
    projectList$loc<-grassListLoc(grassDataBase)
  })


  # directory for map cache
  addResourcePath('mapCache','../data/cache')
  # create leaflet map
  #amMap <- createLeafletMap(session, "amMap")
  amPreviewMap <- createLeafletMap(session, "amPreviewMap")

  # source modules files.
  # for(f in list.files(modPath)){
  #   source(file.path(modPath,f),local=T)
  # }

  # source server files.
  source(file.path(modPath,'module_project.R'),local=T)
  source(file.path(modPath,'module_data.R'),local=T)
  source(file.path(modPath,'module_preview.R'),local=T)
  source(file.path(modPath,'module_logs.R'),local=T)
  source(file.path(modPath,'module_info.R'),local=T)
  source(file.path(modPath,'module_1.R'),local=T)
  source(file.path(modPath,'module_3.R'),local=T)

  })

