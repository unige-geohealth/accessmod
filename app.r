#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# main app file. Load ui and server.

source('tools/R/amFunctions.R') 
source('tools/R/amHandson.R')
source('tools/R/amUi.R')
source("config.R")
source('loadlib.R')

#options(warn=2, error=browser, shiny.error=browser)

# User interface
ui=dashboardPage(

  title='accessmod 5.0',
  skin="black",
  header=dashboardHeader(
    title = h3('AccessMod 5')
    ),
  sidebar=dashboardSidebar(
    tagList(
      h5(id="proj-name",''),
      sidebarMenu(id='whichTab',
        menuItem('Projects',tabName='module_project',icon=icon('map-marker')),
        menuItem('Data',tabName='module_data',icon=icon('folder-open')),
        menuItem('Preview',tabName='module_preview',icon=icon('globe')),
        menuItem('Analysis',tabName='module_selector',icon=icon('sitemap')),  
        menuItem('Logs',tabName='module_logs',icon=icon('archive')),
        menuItem('Settings',tabName='module_settings',icon=icon('cogs'))
        )
      )
    ),
  body=tags$section(class = "content",
      tourPanel(title="shinyTour"),
      div(class="tour_overlay",style="display: none;"),
      tags$head(
        tags$script(src='accessmod.js'),
        tags$link(rel="stylesheet",type="text/css",href='handsontable/handsontable.full.min.css'),
        tags$script(src='handsontable/handsontable.full.min.js'),
        tags$script(src='handsontable/shinyskyHandsonTable.js'),
        tags$link(rel="stylesheet",type="text/css",href='sweetalert/sweetalert2.css'),
        tags$script(src='sweetalert/sweetalert2.min.js'),
        tags$link(rel="stylesheet",type="text/css",href='accessmod.css')
        ), 
      tabItems(
        tabItem('module_project', 
            loadUi('modules/amManageProject/amUi.R')
          ),
        tabItem("module_data",
          loadUi('modules/amManageData/amUi.R')
          ), 
        tabItem("module_preview",
          loadUi('modules/amGisPreview/amUi.R')
          ), 
        tabItem("module_selector",
          loadUi('modules/amManageModules/amUi.R')
          ),
        tabItem("module_logs",
          loadUi('modules/amManageLogs/amUi.R')
          ),
        tabItem("module_settings",
          loadUi('modules/amManageSettings/amUi.R')
          )
        )
      )
  )

server<-function(input, output, session){
  amErrorAction(title="Shiny server",{

    tConf<-tourConfig$new("~/Desktop/tour.sqlite")
    tourMembersManager(input,session,tConf)
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
        amDebugMsg('data list observer change')
        # dependencie on dataListUpdate
        listen$dataListUpdate
        # check existing value and update reactive value inside dataList
        amDataManager(
          config=config,
          dataList=dataList,
          gisLock=listen$gisLock,
          dbCon=listen$dbCon,
          archivePath=listen$archivePath,
          mapset=listen$mapset
          )
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
}


shinyApp(ui=ui,server=server)
