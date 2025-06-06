#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Correct population on barrier - ui


uiPopCorrectConfig <- tagList(
  sidebarPanel(
    width = 4,
    amCenterTitle(
      title = amt(
        id = "toolbox_popcor_config_title"
      ),
      sub = amt("toolbox_popcor_config_sub"),
      h = 3
    ),
    selectInput("selectPopCorPopulation",
      label = amt("toolbox_popcor_select_pop_raster"),
      choices = ""
    ),
    selectInput("selectPopCorLandCoverMerged",
      label = amt("toolbox_popcor_select_merged_lc_raster"),
      choices = ""
    ),
    hr(),
    amRadioButtons("toolbox_popcor_mode",
      label = tags$div(
        amt("toolbox_popcor_mode")
      ),
      choiceNames = list(
        amt("toolbox_popcor_mode_pop_known"),
        amt("toolbox_popcor_mode_pop_not_known")
      ),
      choiceValues = list("known", "unknown"),
      selected = "unknown"
    ),
    selectInput("selectPopCorZones",
      label = amt("toolbox_popcor_select_zones_vector"),
      choices = ""
    ),
    conditionalPanel(
      condition = 'input.toolbox_popcor_mode==="known"',
      selectInput("selectPopCorZonesPopCol",
        label = amt("toolbox_popcor_select_zones_pop"),
        choices = ""
      )
    ),
    hr(),
    textInput("txtPopCorTags",
      label = amt("toolbox_popcor_add_tag"),
      value = ""
    ),
    uiOutput("uiPopCorValidation"),
    actionButton("btnPopCorCompute", amt(
      id = "toolbox_popcor_compute_btn"
    )),
    tags$div(
      class = "col-xs-12 col-md-8 col-lg-6",
      h4("")
    )
  )
)

fluidRow(
  uiOutput("helpPanelPopCor"),
  amCenterTitle(
    title = amt(
      id = "toolbox_popcor_main_title"
    ),
    sub = amt(
      id = "toolbox_popcor_sub"
    )
  ),
  column(
    width = 12,
    uiPopCorrectConfig
  )
)
