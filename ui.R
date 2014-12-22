library(shinyTable)


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
      column(5, p(h6(uiOutput('messageAccesMod'))))
      ),
    #mainPanel(
      tabsetPanel(
        tabPanel('Info',uiOutput('modInfo')), # Info screen
        tabPanel('Project',uiOutput('modManage')), # manage map and location
        tabPanel('Add map',uiOutput('modAddMap')), # Import map
        tabPanel('Modules',
          uiOutput('modAccesmod')
          #     )
          ),
        tabPanel('Logs',tableOutput('logs'))
        )  
      #)
))


