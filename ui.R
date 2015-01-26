#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# ui.R  : main static HTML page
# All modules are greffed through this script.
library(shiny)
library(shinyBS) #for alert function


shinyUI( 
  fluidPage(
    tags$head(
      tags$link(rel="stylesheet",type="text/css",href='accessmod.css'),
      tags$link(rel="shortcut icon", href="favicon.ico")
      ),
    uiOutput("js"),
    uiOutput('title'),
    uiOutput('updateStyle'),
    # uiOutput('toggleClassList'),
    fluidRow(
      shinyBS::bsAlert(inputId = "alert_anchor"),
      column(11,p(uiOutput('messageAccessMod')))
      ),
    navlistPanel( 
      tabPanel(p(icon("cog"),'Project'),uiOutput('modProject')), # manage location and mapset
      tabPanel(p(icon("folder-open"),'Data'),uiOutput('modManageData')), # Import and manage map
      tabPanel(p(icon("sitemap"),"Modules"), uiOutput('modAccesmod')),
      tabPanel(p(icon("archive"),'Logs'),uiOutput('modLogs')),
      tabPanel(p(icon("info-circle"),"Info"),uiOutput('modInfo')), # Info screen
      widths=c(2,10),
      id='navList'
      )
    )
  )





