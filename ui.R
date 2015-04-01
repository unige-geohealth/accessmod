
#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# ui.R  : main static HTML page
# All modules are grafted through this script.
source('fun/amUi.R')
ui <- dashboardPage(
  title='accessmod 5.0',
  skin="black",
  dashboardHeader(
    title = h3('AccessMod 5')#,img(src="logo/icons/logo32x32.png")) 
    ),
  dashboardSidebar(
    tagList(
      h5(id="proj-name",''),
      sidebarMenu(id='whichTab',
        menuItem('Projects',tabName='module_project',icon=icon('cog')),
        menuItem('Data',tabName='module_data',icon=icon('folder-open')),
        #menuItem('Tools',tabName='module_tools',icon=icon('wrench')),
        menuItem('Preview',tabName='module_preview',icon=icon('globe')),
        menuItem('Modules',tabName='module_selector',icon=icon('sitemap')),  
        #menuItem('Report',tabName='module_report',icon=icon('book')),
         # menuSubItem('Module 1',tabName='module_1'),
         # menuSubItem('Module 2', tabName='module_2'),
         # menuSubItem('Module 3', tabName='module_3')
        menuItem('Logs',tabName='module_logs',icon=icon('archive')),
        menuItem('Info',tabName='module_info',icon=icon('info-circle'))
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
      tabItem('module_project',
        loadUi('modules/ui/module_project.R')
        ),
      tabItem("module_data",
        loadUi('modules/ui/module_data.R')
        ), 
      tabItem("module_preview",
        loadUi('modules/ui/module_preview.R')
        ), 
      tabItem("module_selector",
        loadUi('modules/ui/module_selector.R')
        ),
     # tabItem("module_1",
     #   loadUi('modules/ui/module_1.R')
     #   ),
   #   tabItem("module_2",
   #     loadUi('modules/ui/module_3.R')
   #     ),
    #  tabItem("module_3",
    #    loadUi('modules/ui/module_3.R')
    #    ),
      tabItem("module_logs",
        loadUi('modules/ui/module_logs.R')
        ),
      tabItem("module_info",
        loadUi('modules/ui/module_info.R')
        )
      #      tabItem("module3",
      #        uiOutput('module3')
      #        ),
      #
      #      tabItem("logs",
      #        uiOutput('moduleLogs')
      #        ),
      #      tabItem("info",
      #        uiOutput('moduleInfo')
      #        )
      )
    )
  )
