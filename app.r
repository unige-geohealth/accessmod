#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# main app file. Load ui and server.

source('loadlib.R')
source('tools/R/amFunctions.R') 
source('tools/R/amHandson.R')
source('tools/R/amUi.R')
source("config.R")

#options(warn=2, error=browser, shiny.error=browser)

# User interface
ui=dashboardPage(

  title='AccessMod 5.0',
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


# set global grassSession reactive values
grassSession<-reactiveValues()
# check if there is already an active grass session and update value accordingly.
if(isTRUE(nchar(get.GIS_LOCK())>0)){
  grassSession$mapset<-execGRASS('g.mapset',flags='p',intern=T)
}



server<-function(input, output, session){
  amErrorAction(title="Shiny server",{
    tConf<-tourConfig$new("~/tour.sqlite")
    tourMembersManager(input,session,tConf)

    # Session reactive values :
    # reactive value to hold event and logic 
    listen<-reactiveValues()
    # reactive object to hold variables in module "manage data"
    dataMetaList<-reactiveValues()
    # reactive values to store list of data set
    dataList<-reactiveValues()
    # initiat gisLock with NULL
    grassSession$gisLock<-NULL
    # get available grass locations (does not need grass env)
    grassSession$locations<-amGetGrassListLoc(config$pathGrassDataBase)
    # update data list if requested
    observeEvent(listen$dataListUpdate,{
      amErrorAction(title='Data list observer',{
        amDataManager(config,dataList,grassSession)
      })
    },priority=100)

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
