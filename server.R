#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# server.R :
# loading package and modules, handle actions, config file, serving dynamic UI
#
library(shiny)
#shiny::runApp('.',port=3838,launch.browser=F)
# server function.
shinyServer(function(input, output, session){
  # file sourced here because:
  # 1. Some functions need access to reactive object created inside this shinyServer function. TODO:Find out why and avoid that ! 
  # 2. We have to show user that shiny is busy with updating process or packrat process.
  source('tools/R/amFunctions.R',local=T) 
  source('tools/R/amHandson.R',local=T)
  source("config.R")
  amErrorAction(title="Shiny server",{
    source('loadlib.R')
    # Session reactive values :
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

    # Extract dynamic paths:
    # if a gisLock exists, extract archive path from archiveGrass
    observe({
      if(!is.null(listen$gisLock)){
        # archiveGrass need grass environment variables, as defined in config.R
        archivePath<-system(paste('echo',config$pathArchiveGrass),intern=TRUE) 
        # if archive directory doesn't exist, create it.
        dir.create(archivePath,showWarnings = FALSE)
        archivePath<-normalizePath(archivePath) 
        # add ressource for shiny 
        addResourcePath(
          prefix=config$pathArchiveBaseName,
          directoryPath = archivePath
          )
        listen$archivePath=archivePath #
      }else{
        listen$archivePath=NULL
      }
    },priority=110)

    # set data list
    observe({
      amErrorAction(title='Data list observer',{
        amDataManager(listen,dataList,config)
          })
    },priority=100)

    #init base project list
    projectList$loc<-grassListLoc(config$pathGrassDataBase)
    # if a new project is set, update.
    observe({ 
      listen$projectListUpdate
      projectList$loc<-grassListLoc(config$pathGrassDataBase)
    })
    # TODO: transfer this to preview module ?
    # directory for map cache
    addResourcePath('mapCache',config$pathCacheDir)
    # create leaflet map
    #amMap <- createLeafletMap(session, "amMap")
    amPreviewMap <- createLeafletMap(session, "amPreviewMap")
    #modules checker. 
    # we want to prevent all reactives values to be triggered at the same time,
    # so, we have put an observer in GIS and analysis module that will launch
    # as soon as input$whichTab give their ID.
    # BUT. this will also invalidate all reactive value contained. We don't want that.
    observe({
      tab<-input$whichTab
      tab<-paste0('tabControl_',tab)
      listen[[tab]]<-TRUE
    })
    #source modules (amServer files in given module path)
    modList<-dir(config$pathModule,full.names = T)
    for(m in modList){
      amServPath<-file.path(m,'amServer.R')
      if(file.exists(amServPath)){
        source(amServPath,local=TRUE)
      }
    }
})
})


