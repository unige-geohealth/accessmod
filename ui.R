#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# ui.R  : main static HTML page
# All modules are greffed through this script.

shinyUI(  
  fluidPage( 
    uiOutput("reload"),
    #fluidRow(
      #column(2,
        uiOutput('title'),
       # ),
      #column(3,
        #p(uiOutput('locTitle')),
        #),
     # ),
    fluidRow(
      column(12,p(uiOutput('messageAccessMod')))
      ),
    navlistPanel( 
      tabPanel(p(icon("cog"),'Project'),uiOutput('modProject')), # manage location and mapset
      tabPanel(p(icon("folder-open"),'Manage map'),uiOutput('modManageMap')), # Import and manage map
      tabPanel(p(icon("sitemap"),"Modules"), uiOutput('modAccesmod')),
      tabPanel(p(icon("archive"),'Logs'),uiOutput('modLogs')),
      tabPanel(p(icon("info-circle"),"Info"),uiOutput('modInfo')), # Info screen
      widths=c(2,10),
      id='navList'
      ),
    tags$head(
      tags$style(type="text/css", "label.control-label, .selectize-control.multi .item { display: block; }"),
      tags$link(rel="shortcut icon", href="favicon.ico")
      # , tags$link(rel="stylesheet",type="text/css",href="bootstrap.css")
      )
    )
  )




