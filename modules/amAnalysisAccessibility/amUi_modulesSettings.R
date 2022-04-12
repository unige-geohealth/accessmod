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
  amCenterTitle(div(icon("wrench"), amt(
    id = "analysis_settings_title"
  )),
  h = 3,
  m = 0,
  sub = amt(
    id = "analysis_settings_sub"
  )
  ),
  #
  # Settings anisotropic
  #
  conditionalPanel(
    condition = "
    input.moduleSelector=='module_2' |
      input.moduleSelector=='module_3' |
      input.moduleSelector=='module_4' |
      input.moduleSelector=='module_6'
    ",
    conditionalPanel(
      condition = "input.moduleSelector=='module_6'",
      #
      #  Choice of start layer
      #
      amRadioButtons("useExistingHf",
        label = tags$div(
          amt("analysis_settings_existing_health_facility"),
          amt(config$dynamicFacilities)
        ),
        choiceNames = list(
          amt("analysis_settings_use_facility_empty"),
          amt("analysis_settings_use_facility_existing")
        ),
        choiceValues = list(FALSE, TRUE),
        selected = TRUE
      ),
      #
      # Additional text
      #
      amCenterTitle(
        title = amt(
          id = "analysis_settings_param_new_health_facility"
        ),
        h = 4
      )
    ),
    #
    # General accessibility analysis setting
    #
    amRadioButtons("typeAnalysis", amt(
      id = "analysis_settings_type"
    ),
    choiceNames = list(
      amt("analysis_settings_isotropic"),
      amt("analysis_settings_anisotropic")
    ),
    choiceValues = list("isotropic", "anisotropic"),
    selected = "anisotropic",
    inline = FALSE
    ),
    conditionalPanel(
      condition = "
      input.typeAnalysis=='anisotropic' & (
        input.moduleSelector=='module_2' |
          input.moduleSelector=='module_3' |
          input.moduleSelector=='module_6'
        ) ",
      amRadioButtons("dirAnalysis", amt(
        id = "analysis_settings_travel_direction"
      ),
      choiceNames = list(
        amt("analysis_settings_travel_from_hf"),
        amt("analysis_settings_travel_to_hf")
      ),
      choiceValues = list("fromHf", "toHf"),
      selected = "toHf",
      inline = FALSE
      )
    )
  ),
  #
  # Module 3: sorting parameters
  #
  conditionalPanel(
    condition = "input.moduleSelector=='module_3'",
    amRadioButtons("hfOrder", amt(
      id = "analysis_settings_health_facility_order"
    ),
    choiceNames = list(
      amt("analysis_settings_health_facility_order_table"),
      amt("analysis_settings_health_facility_order_travel_time"),
      amt("analysis_settings_health_facility_order_circular_buffer")
    ),
    choiceValues = list(
      "tableOrder",
      "travelTime",
      "circBuffer"
    )
    ),
    #  conditionalPanel(condition = "input.hfOrder!="tableOrder"",
    conditionalPanel(
      condition = "input.hfOrder=='tableOrder' && isNotEmpty(input.hfSelect)",
      selectInput("hfOrderColumn", amt(
        id = "analysis_settings_health_facility_select"
      ),
      choices = ""
      )
    ),
    conditionalPanel(
      condition = "input.hfOrder=='circBuffer'",
      numericInput("popBufferRadius", amt(
        id = "analysis_settings_buffer_radius"
      ),
      value = 5000
      )
    ),
    conditionalPanel(
      condition = "input.hfOrder=='travelTime'",
      numericInput("maxTravelTimeProcOrder",
        label = amt(
          id = "analysis_settings_given_travel_time"
        ),
        value = 120,
        min = 0,
        max = 1080, # note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
        step = 1
      )
    ),
    amRadioButtons("hfOrderSorting", amt(
      id = "analysis_settings_sorting_health_facility"
    ),
    choiceNames = list(
      amt("analysis_settings_sorting_health_facility_asc"),
      amt("analysis_settings_sorting_health_facility_desc")
    ),
    choiceValues = list("hfOrderAsc", "hfOrderDesc"),
    selected = "hfOrderDesc",
    inline = FALSE
    )
  ),
  #
  # Referral options
  #
  conditionalPanel(
    condition = "input.moduleSelector == 'module_4'",
    checkboxInput(
      inputId = "checkReferralPermute",
      label = tags$div(
        amt("analysis_settings_referral_permute_groups"),
        tags$small(class = "text-muted", amt("analysis_settings_referral_permute_groups_desc"))
      ),
      value = FALSE
    ),
    checkboxInput(
      inputId = "checkReferralKeepNetwork",
      label = amt("analysis_settings_referral_keep_network"),
      value = FALSE
    ),
    checkboxInput(
      inputId = "checkReferralParallel",
      label = tags$div(
        amt("analysis_settings_referral_use_parallel"),
        tags$small(class = "text-muted", amt("analysis_settings_referral_use_parallel_desc"))
      ),
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.checkReferralPermute !== true",
      checkboxInput(
        inputId = "checkReferralLimitClosest",
        label = amt(
          id = "analysis_settings_referral_limit_closest"
        ),
        value = TRUE
      )
    ),
    checkboxInput(
      inputId = "checkReferralSnapToGrid",
      label = tags$div(
        amt("analysis_settings_referral_snap_grid"),
        tags$br(),
        tags$small(
          class = "text-muted",
          amt("analysis_settings_referral_snap_grid_desc")
        )
      ),
      # https://github.com/fxi/AccessMod_shiny/issues/363#issuecomment-932108899
      value = TRUE
    )
  ),
  #
  # Set maximum walk time
  #
  conditionalPanel(
    condition = "(
    input.moduleSelector=='module_2' |
      input.moduleSelector=='module_3' |
      input.moduleSelector=='module_4' |
      input.moduleSelector=='module_6'
    )",
    numericInput("maxTravelTime",
      label = amt(
        id = "analysis_settings_max_travel_time"
      ),
      value = 120,
      min = 0,
      max = 2^32 / 2 - 1,
      step = 1
    ),
    #
    #  Scaling up
    #
    conditionalPanel(
      condition = "input.moduleSelector=='module_6'",
      tagList(
        amCenterTitle(
          title = div(
            amt(
              id = "analysis_settings_compute_limits"
            ),
            actionLink(
              inputId = "helpLinkComputeLimit",
              icon = icon("question-circle"),
              label = ""
            )
          ),
          h = 4
        ),
        div(
          numericInput("maxScUpPopGoal",
            label = amt(
              id = "analysis_settings_pop_cover_percent"
            ),
            value = 80,
            min = 0,
            max = 100
          ),
          numericInput("maxScUpNewHf",
            label = amt(
              id = "analysis_settings_new_health_facility_number"
            ),
            value = 0,
            min = 0,
            max = 500,
            step = 1
          ),
          numericInput("maxScUpTime",
            label = amt(
              id = "analysis_settings_max_process_time"
            ),
            value = 0,
            min = 0,
            max = 400
          )
        )
      )
    )
  ),
  #
  # Module 3 and 6 more options
  #
  conditionalPanel(
    condition = "(
  input.moduleSelector=='module_3'|
    input.moduleSelector=='modue_6'
  )",
    amCheckboxGroupInput("mod3param", amt(
      id = "analysis_settings_options"
    ),
    choiceNames = list(
      amt("analysis_settings_options_compute_catchment"),
      amt("analysis_settings_options_remove_covered_population"),
      amt("analysis_settings_options_compute_population_on_barrier"),
      amt("analysis_settings_options_compute_zonal_statistic"),
      amt("analysis_settings_options_ignore_capacity"),
      amt("analysis_settings_options_add_column_pop_orig_travel_time"),
      amt("analysis_settings_options_add_columns_pop_coverage_extented")
    ),
    choiceValues = list(
      "vectCatch",
      "rmPop",
      "popBarrier",
      "zonalPop",
      "ignoreCapacity",
      "addColumnPopOrigTravelTime",
      "addColumnsPopCoverageExtended"
    ),
    selected = c(
      "rmPop",
      "vectCatch",
      "popBarrier"
    )
    )
  ),
  conditionalPanel(
    condition = "(
  input.moduleSelector!='module_5'
  )",
    conditionalPanel(
      condition = " (
  input.showAdvancedTools === true
  )",
      tags$div(
        tags$label(amt("analysis_settings_options_advanced")),
        checkboxInput("checkWithSpeedMask",
          label = amt("analysis_settings_with_speed_mask"),
          value = FALSE
        ),
        conditionalPanel(
          condition = " (
    input.moduleSelector=='module_2'
    )",
          checkboxInput("checkWithNearest",
            label = tags$div(
              amt("analysis_settings_with_nearest"),
              tags$br(),
              tags$small(
                class = "text-muted",
                amt("analysis_settings_with_nearest_desc")
              )
            ),
            value = FALSE
          )
        )
      )
    ),
    # tags$small(amt("analysis_settings_with_speed_mask_help")),
    textInput("costTag", amt(
      id = "analysis_settings_add_tag"
    ),
    value = ""
    )
  ),
  conditionalPanel(
    condition = "(
  input.moduleSelector == 'module_5'
  )",
    tagList(
      checkboxInput("checkZoneTableWide",
        label = tags$div(
          amt("analysis_zonal_stat_check_table_wide"),
          tags$br(),
          tags$small(
            class = "text-muted",
            amt("analysis_zonal_stat_check_table_wide_desc")
          )
        ),
        value = TRUE
      )
    )
  )
)
