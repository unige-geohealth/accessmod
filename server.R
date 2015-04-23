#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# server.R :
# loading package and modules, handle actions, config file, serving dynamic UI
#

# server function.
shinyServer(function(input, output, session){
  source("config.R")
  source('tools/R/amFunctions.R',local=T)
  source('tools/R/amHandson.R',local=T)

  # set busy mode.
  amBusyManage(session,TRUE)

  # package manager load or install.
  amErrorAction(title='Package manager',{
    amPackageManager(
      pkgCran      = config$packagesCran,
      pkgLocal     = config$packagesLocal,
      libPath      = config$pathLib,
      pathLocalPkg = config$pathLocalPkg
      )
})

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
      # if archive directory is not existant, create it.
      dir.create(archivePath,showWarnings = FALSE)
      archivePath<-normalizePath(archivePath) 
      #add ressource for shiny 
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
  # TODO: group reactive value, convert this script in function.
  observe({
    amErrorAction(title='Data list observer',{
      # gisLock change when grass is initialised : startup and locatio change
      gLock<-listen$gisLock 
      # dataListUpdate change on demand, when new map are created: function dataListUpdate().
      listen$dataListUpdate
      # if gisLock is set, allow querying database.
      if(!is.null(gLock)){
        amDebugMsg('Update dataList: search in grass and sqlite. GisLock=',gLock)

        # TODO: clean this and make a function from this mess.
        rmVectIfExists('^tmp_*')
        rmRastIfExists('^tmp_*')
        archives<-list.files(listen$archivePath)
        archivesSelect<-archives[order(archives,decreasing=T)]
        mapset<-isolate(listen$mapset)
        sqlTables<-"select name from sqlite_master where type='table' AND name like 'table_%' "
        tables<-dbGetQuery(isolate(listen$dbCon),sqlTables)$name
        if(length(tables)>0){
          # create selectize input. E.g table_model__p003 >>
          # named list element :  $`table_model [p003]`
          # value [1] "table_model__p003@p_500_m"
          tablesSelect<-amCreateSelectList(
            dName=tables,
            sepTag=config$sepTagFile,
            sepClass=config$sepClass,
            mapset=mapset)
        }else{
          tablesSelect=NULL
        }
        vectorsSelect<-amCreateSelectList(
          dName=execGRASS('g.list',type='vector',intern=TRUE),
          sepTag=config$sepTagFile,
          sepClass=config$sepClass,
          mapset=mapset
          )

        rastersSelect<-amCreateSelectList(
          dName=execGRASS('g.list',type='raster',intern=TRUE),
          sepTag=config$sepTagFile,
          sepClass=config$sepClass,
          mapset=mapset
          )

        # if amCreateSelectList found NA in name (wrong data name)
        # remove from GRASS db
        if(T){
          if(!is.null(rastersSelect)){
            rastToRemove<-rastersSelect[is.na(names(rastersSelect))]
            if(isTRUE(length(rastToRemove)>0)){
              sapply(rastToRemove,function(x){
                x<-unlist(strsplit(x,config$sepMapset))[1]
                message(paste("removing unnamed file", x))
                rmRastIfExists(x)}
                )
            }
          }
          if(!is.null(vectorsSelect)){
            vectToRemove<-vectorsSelect[is.na(names(vectorsSelect))]

            if(isTRUE(length(vectToRemove))>0){
              sapply(vectToRemove,function(x){
                x<-unlist(strsplit(x,config$sepMapset))[1]
                message(paste("removing unnamed file", x))
                rmVectIfExists(x)}
                )
            }
          }
          if(!is.null(tablesSelect)){
            tableToRemove<-tablesSelect[is.na(names(tablesSelect))]
            if(isTRUE(length(tableToRemove)>0)){
              sapply(tableToRemove,function(x){
                x<-unlist(strsplit(x,config$sepMapset))[1]
                message(paste("removing unnamed file", x))
                sql<-paste("DROP TABLE IF EXISTS",x)
                dbGetQuery(isolate(listen$dbCon),sql)}
                )
            }
          }
        }



        dataList$raster<-rastersSelect
        dataList$vector<-vectorsSelect
        dataList$table<-tablesSelect
        dataList$archive<-archivesSelect

        dataList$df<-rbind(
          amDataListToDf(rastersSelect,config$sepClass,'raster'),
          amDataListToDf(vectorsSelect,config$sepClass,'vector'),
          amDataListToDf(tablesSelect,config$sepClass,'table')
          )

      }else{
        amDebugMsg('DataList: no gisLock. ')
      }

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
  # set busy mode.
  amBusyManage(session,FALSE)

})


