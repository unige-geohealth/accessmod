wellPanel(
  amCenterTitle(div(icon("wrench"), amt(
    id = "analysis_settings_title"
    )),
    h = 3,
    m = 0,
    sub = amt(
      id = "analysis_settings_sub"
      )),
  #
  # Settings anisotropic
  #
  conditionalPanel(condition = "
    input.moduleSelector=='module_2' | 
      input.moduleSelector=='module_3' |
      input.moduleSelector=='module_4' |
      input.moduleSelector=='module_6'
    ",
    conditionalPanel(condition = "input.moduleSelector=='module_6'",
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
          choiceValues =  list(FALSE,TRUE),
          selected = TRUE
        ),
      #
      # Additional text
      #
      amCenterTitle(title = amt(
        id = "analysis_settings_param_new_health_facility"
        ),
        h = 4)
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
      choiceValues = list('isotropic','anisotropic'), 
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
      amRadioButtons('dirAnalysis', amt(
        id = "analysis_settings_travel_direction"
        ),
        choiceNames = list(
           amt("analysis_settings_travel_from_hf"),
           amt("analysis_settings_travel_to_hf")
          ),
        choiceValues = list("fromHf","toHf"),
        selected = "toHf",
        inline = FALSE
        )
      )
    ),
  #
  # Module 3: sorting parameters
  #
  conditionalPanel(condition = "input.moduleSelector=='module_3'",
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
    conditionalPanel(condition = "input.hfOrder=='tableOrder' && isNotEmpty(input.hfSelect)",
      selectInput("hfOrderColumn", amt(
        id = "analysis_settings_health_facility_select"
        ),
        choices = "")
      ),
    conditionalPanel(condition = "input.hfOrder=='circBuffer'",
      numericInput("popBufferRadius", amt(
        id = "analysis_settings_buffer_radius"
        ),
        value = 5000)
      ),
    conditionalPanel(condition = "input.hfOrder=='travelTime'",
      numericInput("maxTravelTimeProcOrder",
        label =  amt(
          id = "analysis_settings_given_travel_time"
          ),
        value = 120,
        min = 0,
        max = 1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
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
      choiceValues =  list("hfOrderAsc","hfOrderDesc"),
      selected = "hfOrderDesc",
      inline = FALSE
      )
    ),
  #
  # Referral options
  #
  conditionalPanel(condition = "input.moduleSelector=='module_4'",
    checkboxInput(
      inputId = "checkReferralLimitClosest",
      label = amt(
        id = "analysis_settings_referral_limit_closest"
        ),
      value = TRUE
      )
    ),
  #
  # Set maximum walk time
  #
  conditionalPanel(condition = "(
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
    max = 2^32/2-1,
    step = 1
    ),
  #
  #  Scaling up  
  #
  conditionalPanel(condition = "input.moduleSelector=='module_6'",
    tagList(
      amCenterTitle(
        title = div(amt(
          id = "analysis_settings_compute_limits"
          ),
          actionLink(
            inputId = 'helpLinkComputeLimit',
            icon = icon('question-circle'),
            label = ''
            )
          ),
        h = 4),
      div(
        numericInput('maxScUpPopGoal',
          label = amt(
            id = "analysis_settings_pop_cover_percent"
            ),
          value = 80,
          min = 0,
          max = 100
          ),
        numericInput('maxScUpNewHf',
          label =  amt(
            id = "analysis_settings_new_health_facility_number"
            ),
          value = 0,
          min = 0,
          max = 500,
          step = 1
          ),
        numericInput('maxScUpTime',
          label =  amt(
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
conditionalPanel(condition = "(
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
    amt("analysis_settings_options_compute_zonal_statistic")
    ),
  choiceValues = list(
    "vectCatch",
    "rmPop",
    "popBarrier",
    "zonalPop"
    ),
  selected = c(
  "rmPop",
  "vectCatch",
  "popBarrier"
  ))
),
conditionalPanel(condition = "(
  input.moduleSelector!='module_5'
  )",
  tags$label(amt("analysis_settings_options_advanced"),
    checkboxInput('checkWithSpeedMask',
      label = amt("analysis_settings_with_speed_mask"),
      value = FALSE
      )),
  #tags$small(amt("analysis_settings_with_speed_mask_help")),
  textInput('costTag', amt(
    id = "analysis_settings_add_tag"
    ),
    value = ''
    )
  )
)
