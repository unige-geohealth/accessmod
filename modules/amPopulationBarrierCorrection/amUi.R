#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Correct population on barrier - ui


uiPopCorrectConfig <- tagList(
    sidebarPanel( width = 4,
      amCenterTitle(title="Configuration",sub="Select population, merged land cover and administrative zones",h=3),
      selectInput("selectPopCorPopulation",
        label = "Select population (raster)",
        choices = ""
        ),
      selectInput("selectPopCorLandCoverMerged",
        label = "Select land cover merged (raster)",
        choices = ""
        ),
      selectInput("selectPopCorZones",
        label = "Select zones (vector)",
        choices = ""
        ),
      textInput("txtPopCorTags",
        label = "Add short tag",
        value = ""
        ),
      uiOutput("uiPopCorValidation"),
      actionButton("btnPopCorCompute","Compute")
      ),
    tags$div(class="col-xs-12 col-md-8 col-lg-6",
      h4("")
    ) 
  )

fluidRow(
  uiOutput('helpPanelPopCor'), 
  amCenterTitle(title="Correct population on barriers",sub="Redistribute population on barriers accross administrative zone"),
  column(width=12,
    uiPopCorrectConfig
    )
  )
