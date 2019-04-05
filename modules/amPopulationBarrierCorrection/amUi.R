#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Correct population on barrier - ui


uiPopCorrectConfig <- tagList(
  sidebarPanel(width = 4,
    amCenterTitle(title = amt(
      id = "toolbox_popcor_config_title"
      ),
      sub = amt(
        id = "toolbox_popcor_config_sub"
        ),
      h = 3
      ),
    selectInput("selectPopCorPopulation",
      label = amt(
        id = "toolbox_popcor_select_pop_raster"
        ),
      choices = ""
      ),
    selectInput("selectPopCorLandCoverMerged",
      label = amt(
        id = "toolbox_popcor_select_merged_lc_raster"
        ),
      choices = ""
      ),
    selectInput("selectPopCorZones",
      label = amt(
        id = "toolbox_popcor_select_zones_vector"
        ),
      choices = ""
      ),
    textInput("txtPopCorTags",
      label = amt(
        id = "toolbox_popcor_add_tag"
        ),
      value = ""
      ),
    uiOutput("uiPopCorValidation"),
    actionButton("btnPopCorCompute", amt(
      id = "toolbox_popcor_compute_btn"
      )),
    tags$div(class = "col-xs-12 col-md-8 col-lg-6",
      h4("")
      ) 
    )
  )

fluidRow(
  uiOutput('helpPanelPopCor'), 
  amCenterTitle(title = amt(
    id = "toolbox_popcor_main_title"
    ),
    sub = amt(
      id = "toolbox_popcor_sub"
      )
    ),
  column(width = 12,
    uiPopCorrectConfig
    )
  )
