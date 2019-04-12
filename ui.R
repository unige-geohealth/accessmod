#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/

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
        h4("AccessMod 5"),
        tags$span(amt('main_ui_current_project')),
        tags$h4(id="projName",""),
        div(class="shinyCookies",id="amCookies")
        ),
      hr(),
      sidebarMenu(id="whichTab",
        menuItem(
          text = amt('main_ui_tab_project'),
          tabName="module_project",
          icon=icon("map-marker")
          ),
        menuItem(
          text= amt('main_ui_tab_data'),
          tabName="module_data",
          icon=icon("folder-open")
          ),
        menuItem(
          text=amt('main_ui_tab_analysis'),
          tabName="module_analysis",
          icon=icon("cubes")
          ),
        menuItem(
          text=amt('main_ui_tab_toolbox'),
          tabName="module_toolbox",
          icon=icon("cogs")
          ),
        menuItem(
          text=amt('main_ui_tab_logs'),
          tabName="module_logs",
          icon=icon("archive")
          ),
        menuItem(
          text=amt('main_ui_tab_settings'),
          tabName="module_settings",
          icon=icon("sliders")
          ),
        menuItem(
          text=amt('main_ui_tab_about'),
          tabName="module_about",
          icon=icon("info-circle")
          )
        )
      , p(style="display:none",paste("ui update","acj")) 
      )
    ),

  body=tags$div(class = "content-wrapper",
    tags$body(
     # default modal panel
      uiOutput("amModal"),
      # help modal panel
      uiOutput("amHelpPanel") 
      ),
    tags$head(
      tags$script(src="js/accessmod.js"),
      tags$script(src="js/accessmod_translate.js"),
      tags$link(rel="stylesheet",type="text/css",href="handsontable/handsontable.full.min.css"),
      tags$script(src="handsontable/handsontable.full.min.js"),
      tags$script(src="handsontable/shinyskyHandsonTable.js"),
      tags$script(src="progress/progress.js"),
      tags$link(rel="stylesheet",type="text/css",href="css/accessmod.css"),
      tags$link(rel="stylesheet",type="text/css",href="css/sortableDouble.css"),
      tags$link(rel="stylesheet",type="text/css",href="css/geom.css"),
      tags$link(rel="shortcut icon", href="img/favicon.ico?")
      ), 
    tabItems(
      tabItem("module_project", 
        loadUi("modules/amManageProject/amUi.R")
        ),
      tabItem("module_data",
        loadUi("modules/amManageData/amUi.R")
        ),
      tabItem("module_analysis",
        loadUi("modules/amManageAnalysis/amUi.R")
        ),
      tabItem("module_toolbox",
        loadUi("modules/amManageToolbox/amUi.R")
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
