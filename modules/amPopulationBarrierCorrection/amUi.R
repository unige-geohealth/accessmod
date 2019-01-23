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
      id = "toolbox_popcor_config",
      str = "Configuration"
      ),
      sub = amt(
          id = "toolbox_popcor_config_sub",
        str = "Select population, merged land cover and administrative zones"
        ),
      h = 3
	  ),
    selectInput("selectPopCorPopulation",
      label = amt(
        id = "toolbox_popcor_sel_pop",
        str = "Select population (raster)"
        ),
      choices = ""
      ),
    selectInput("selectPopCorLandCoverMerged",
      label = amt(
        id = "toolbox_popcor_sel_lcm",
        str = "Select land cover merged (raster)"
        ),
      choices = ""
      ),
    selectInput("selectPopCorZones",
      label = amt(
        id = "toolbox_popcor_sel_zones",
        str = "Select zones (vector)"
        ),
      choices = ""
      ),
    textInput("txtPopCorTags",
      label = amt(
        id = "toolbox_popcor_add_tag",
        str = "Add short tag"
        ),
      value = ""
      ),
    uiOutput("uiPopCorValidation"),
    actionButton("btnPopCorCompute", amt(
      id = "toolbox_popcor_compute",
      str = "Compute"
      )),
    tags$div(class = "col-xs-12 col-md-8 col-lg-6",
      h4("")
      ) 
    )
  )

fluidRow(
  uiOutput('helpPanelPopCor'), 
  amCenterTitle(title = amt(
    id = "toolbox_popcor_help",
    str = "Correct for population on barriers"
    ),
    sub = amt(
      id = "toolbox_popcor_help_sub",
      str = "Redistribute population on barriers within administrative zones"
      )
	),
  column(width = 12,
    uiPopCorrectConfig
    )
  )
