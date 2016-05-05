
# User interface
dashboardPage( 
  title="AccessMod 5.0",
  skin="black",  
  header=dashboardHeader(
    title=div(class="amCenterTitle",tags$a(href="http://www.who.int",config$iconWhoSvg))
    ),
  sidebar=dashboardSidebar(
    div(
      hr(),
      div(class="amCenterTitle",
        h4("AccessMod 5 (beta)"),
        tags$span("Current project:"),
        tags$h4(id="projName",""),
        div(class="shinyCookies",id="amCookies")
        ),
      hr(),
      sidebarMenu(id="whichTab",
        menuItem(
          "Projects",
          tabName="module_project",icon=icon("map-marker")),
        menuItem("Data",tabName="module_data",icon=icon("folder-open")),
        menuItem("Toolbox",tabName="module_selector",icon=icon("cubes")),  
        menuItem("Raster preview",tabName="module_preview",icon=icon("globe")),
        menuItem("Logs",tabName="module_logs",icon=icon("archive")),
        menuItem("Settings",tabName="module_settings",icon=icon("cogs")),
        menuItem("About",tabName="module_about",icon=icon("info-circle"))
        )
      , p(style="display:none",paste("ui update","acj")) 
      )
    ),

  body=tags$div(class = "content-wrapper",
    tags$body(
      # full body progress bar
      progressBarUi(
        id=config$pBarId,
        zIndex=2000,
        listActionButton=FALSE,
        defaultButtonText="close",
        addCancelButton=FALSE,
        classButtons=""),
      # default modal panel
      uiOutput("amModal"),
      # help modal panel
      uiOutput("amHelpPanel") 
      ),
    tags$head(
      tags$script(src="accessmod.js"),
      tags$link(rel="stylesheet",type="text/css",href="handsontable/handsontable.full.min.css"),
      tags$script(src="handsontable/handsontable.full.min.js"),
      tags$script(src="handsontable/shinyskyHandsonTable.js"),
      tags$link(rel="stylesheet",type="text/css",href="css/accessmod.css"),
      tags$link(rel="stylesheet",type="text/css",href="css/sortableDouble.css"),
      tags$link(rel="stylesheet",type="text/css",href="css/geom.css")
      ), 
    tabItems(
      tabItem("module_project", 
        loadUi("modules/amManageProject/amUi.R")
        ),
      tabItem("module_data",
        loadUi("modules/amManageData/amUi.R")
        ), 
      tabItem("module_preview",
        loadUi("modules/amGisPreview/amUi.R")
        ), 
      tabItem("module_selector",
        loadUi("modules/amManageModules/amUi.R")
        ),
      tabItem("module_logs",
        loadUi("modules/amManageLogs/amUi.R")
        ),
      tabItem("module_settings",
        loadUi("modules/amManageSettings/amUi.R")
        ),
      tabItem("module_about",
        loadUi("modules/amAbout/amUi.R")
        )
      )
    )
  )
