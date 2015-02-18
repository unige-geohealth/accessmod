#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# ui.R  : main static HTML page
# All modules are greffed through this script.
library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  title='accessmod 5.0',
  skin="black",
  dashboardHeader(
    title = h3('AccessMod 5',img(src="logo/icons/logo32x32.png")) 
    ),
  dashboardSidebar(
    tagList(
      h5(id="proj-name",''),
      sidebarMenu(
        menuItem('Projects',tabName='projects',icon=icon('cog')),
          menuItem('Data',tabName='data',icon=icon('folder-open')),
          menuItem('Preview',tabName='preview',icon=icon('globe')),
          menuItem('Modules',icon=icon('sitemap'),  
            menuSubItem('Module 1',tabName='module1'),
            menuSubItem('Module 2', tabName='module2'),
            menuSubItem('Module 2', tabName='module3')
            ),
          menuItem('Logs',tabName='logs',icon=icon('archive')),
          menuItem('Info',tabName='info',icon=icon('info-circle'))
          )
        )
    ),
  #dashboardBody(
  tags$section(class = "content",
    tags$head(
      tags$link(rel="stylesheet",type="text/css",href='accessmod.css'),
      tags$script(src='accessmod.js'),
      #handsontable and binding from shinysky
      tags$link(rel="stylesheet",type="text/css",href='handsontable/handsontable.full.min.css'),
      tags$script(src='handsontable/handsontable.full.min.js'),
      tags$script(src='handsontable/shinyskyHandsonTable.js'),
      #tags$script(src='https://code.jquery.com/jquery-1.11.1.min.js'),
      tags$link(rel="stylesheet",type="text/css",href='sweetalert/lib/sweet-alert.css'),
      tags$script(src='sweetalert/lib/sweet-alert.js')

      ), 

    tabItems(
      tabItem('projects',
        uiOutput('moduleProject')
        ),
      tabItem("data",
        uiOutput('moduleData')
        ),  
tabItem("preview",
        uiOutput('modulePreview')
        ),  
      tabItem("module1",
        uiOutput('module1')
        ),
      tabItem("module2",
        uiOutput('module2')
        ),
 tabItem("module3",
        uiOutput('module3')
        ),

      tabItem("logs",
        uiOutput('moduleLogs')
        ),
      tabItem("info",
        uiOutput('moduleInfo')
        )
      )
    )
  )
