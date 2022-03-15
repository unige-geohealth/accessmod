#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

wellPanel(
  amCenterTitle(div(
    icon("sign-in"),
    amt("analysis_data_input_title")
  ),
  h = 3,
  m = 0,
  sub = amt("analysis_data_input_sub")
  ),
  #
  # Select population layer
  #
  conditionalPanel(
    condition = "(
    input.moduleSelector=='module_3' |
      input.moduleSelector=='module_5' |
      input.moduleSelector=='module_6'
    )",
    selectInput("popSelect", amt("analysis_select_pop_raster"),
      choices = ""
    )
  ),
  conditionalPanel(
    condition = "(input.moduleSelector=='module_6')",
    selectInput("popResidualSelect", amt("analysis_select_residual_pop_raster"),
      choices = ""
    )
  ),
  #
  # select merged landcover and model table
  #
  conditionalPanel(
    condition = "
  input.moduleSelector != 'module_5'
  ",
    selectInput("mergedSelect", amt("analysis_select_merged_lc_raster"),
      choices = ""
    ),
    selectInput("modelSelect", amt("analysis_select_scenario_table"),
      choices = ""
    ),
    conditionalPanel(
      condition = "input.moduleSelector== 'module_4'",
      tags$b(icon("play"), amt("analysis_select_from"))
    ),
    #
    # select facility tmap and columns
    #
    conditionalPanel(
      condition = "!(input.moduleSelector=='module_6' & input.useExistingHf == 'FALSE')",
      selectInput("hfSelect", amt("analysis_select_health_facility_vector"),
        choices = ""
      ),
      conditionalPanel(
        condition = "
      input.moduleSelector=='module_3' |
        input.moduleSelector=='module_4' |
        input.moduleSelector=='module_6'
      ",
        conditionalPanel(
          "isNotEmpty(input.hfSelect) ",
          div(
            style = "margin-left:10%;",
            selectInput("hfIdxField", amt("analysis_select_health_facility_id_field"),
              choices = ""
            ),
            selectInput("hfNameField", amt("analysis_select_health_facility_name_field"),
              choices = ""
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.moduleSelector=='module_4'",
        tags$b(
          icon("stop"),
          amt("analysis_select_to")
        ),
        selectInput("hfSelectTo", amt("analysis_select_existing_health_facility_vector"),
          choices = ""
        ),
        conditionalPanel(
          "isNotEmpty(input.hfSelectTo) ",
          div(
            style = "margin-left:10%;",
            selectInput("hfIdxFieldTo", amt("analysis_select_health_facility_id_field_to"),
              choices = ""
            ),
            selectInput("hfNameFieldTo", amt("analysis_select_health_facility_name_field_to"),
              choices = ""
            )
          )
        )
      ),
      #
      # Select health facilities capacity field
      #
      conditionalPanel(
        condition = "(
      input.moduleSelector=='module_6' |
        input.moduleSelector=='module_3'
      ) && isNotEmpty(input.hfSelect)
      && input.mod3param.indexOf('ignoreCapacity') === -1",
        div(
          style = "margin-left:10%;",
          selectInput("hfCapacityField", amt("analysis_select_health_facility_capacity"),
            choices = ""
          )
        )
      )
    )
  ),
  #
  # Select cumulative cost map
  #
  conditionalPanel(
    condition = "(
  input.moduleSelector=='module_5'
  )",
    selectInput("travelTimeSelect", amt("analysis_select_travel_time_raster"),
      choices = ""
    )
  ),
  #
  # Module 3 and 5 . Choose zonal map
  #
  conditionalPanel(
    condition = "
  (input.moduleSelector=='module_3' &
    input.mod3param.indexOf('zonalPop') != -1
  ) |
input.moduleSelector=='module_5'
",
    selectInput("zoneSelect", amt("analysis_select_zone_vector"),
      choices = ""
    ),
    conditionalPanel(
      "isNotEmpty(input.zoneSelect)",
      div(
        style = "margin-left:10%;",
        selectInput("zoneId", amt("analysis_select_zone_id_integer"),
          choices = ""
        ),
        selectInput("zoneLabel", amt("analysis_select_zone_name_text"),
          choices = ""
        )
      )
    )
  ),
  conditionalPanel(
    condition = "(
  input.moduleSelector=='module_5' &&
    isNotEmpty(input.popSelect) &&
    isNotEmpty(input.travelTimeSelect) &&
    isNotEmpty(input.zoneSelect)
  )",
    textInput("textTimeCumCosts",
      label = tags$div(
        amt("analysis_select_max_travel_time"),
        tags$br(),
        tags$small(
          class = "text-muted",
          amt("analysis_select_max_travel_time_desc")
        ),
        tags$small(
          class = "text-muted",
          id = "txtZonalMinMax"
        )
      ),
      value = "0"
    )
  ),
  conditionalPanel(
    condition = "(
  input.moduleSelector=='module_6'
  )",
    #
    # select external capacity table
    #
    selectInput("capTableSelect", amt("analysis_select_capacity_table"),
      choices = ""
    ),
    selectInput("suitabilityTableSelect", amt("analysis_select_suitability_table"),
      choices = ""
    ),
    selectInput("exclusionTableSelect", amt("analysis_select_exclusion_table"),
      choices = ""
    )
  )
)
