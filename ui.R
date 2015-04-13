#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# ui.R  : main static HTML page
# All modules are grafted through this script.

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)
source('tools/R/amUi.R')
source('tools/R/amFunctions.R')
source('tools/R/amHandson.R')

# register help "item" in SQLite from aH (accessMod help)
checkForHelpEntry=T

if(checkForHelpEntry){
library(RSQLite)
dbCon=dbConnect(SQLite(),'help/db/help.sqlite')
}else{
dbCon=NULL
}

# NOTE: why configHelp is not accessible from localy sourced modules??
configHelp<<-list(
  module='AccessMod',
  dbCon=dbCon
  )




ui <- dashboardPage(
  title='accessmod 5.0',
  skin="black",
  dashboardHeader(
    title = h3('AccessMod 5')
    ),
  dashboardSidebar(
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
  tags$section(class = "content",
    tags$head(
      tags$script(src='accessmod.js'),
      tags$link(rel="stylesheet",type="text/css",href='handsontable/handsontable.full.min.css'),
      tags$script(src='handsontable/handsontable.full.min.js'),
      tags$script(src='handsontable/shinyskyHandsonTable.js'),
      tags$link(rel="stylesheet",type="text/css",href='sweetalert/sweetalert2.css'),
      tags$script(src='sweetalert/sweetalert2.min.js'),
      tags$script(src='intro/intro.min.js'),
      tags$link( rel="stylesheet", href="intro/introjs.min.css"),

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
