#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# ui.R  : main static HTML page
# All modules are greffed through this script.
# test

shinyUI(  
  fluidPage(
           tags$head(
             tags$style(type="text/css", "label.control-label, .selectize-control.multi .item { display: block; }")
           ),
           title='Accessmod',
           #titlePanel(tags$h3(uiOutput('title'))),
           fluidRow(
             column(2, h4(uiOutput('title'))), 
             column(3, p(h6(uiOutput('location')))),
             column(5, p(h6(textOutput('messageAccessMod'))))
           ),
           
           tabsetPanel(
             tabPanel('Info',uiOutput('modInfo')), # Info screen
             tabPanel('Project',uiOutput('modProject')), # manage location and mapset
             tabPanel('Manage map',uiOutput('modManageMap')), # Import and manage map
             tabPanel('Modules', uiOutput('modAccesmod')),
             tabPanel('Logs',uiOutput('modLogs'))
           )
  ))


