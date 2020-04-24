wellPanel(
  amCenterTitle(div(icon('sign-in'), 
      amt(
        id = "analysis_data_input_title"
        )
      ),
    h = 3,
    m = 0,
    sub = amt(
      id = "analysis_data_input_sub"
      )
    ),
  #
  # Select population layer
  #
  conditionalPanel(condition = "(
    input.moduleSelector=='module_3' |
      input.moduleSelector=='module_5' |
      input.moduleSelector=='module_6'
    )",
  selectInput("popSelect", amt(
      id = "analysis_select_pop_raster"
      ),
    choices = ""
    )
  ),
conditionalPanel(condition = "(input.moduleSelector=='module_6')",
  selectInput("popResidualSelect", amt(
      id = "analysis_select_residual_pop_raster"
      ),
    choices = ""
    )
  ),
#
# select merged landcover and model table
#
conditionalPanel(condition = "
  input.moduleSelector != 'module_5'
  ",
  selectInput("mergedSelect", amt(
      id = "analysis_select_merged_lc_raster"
      ),
    choices = ""),
  selectInput("modelSelect", amt(
      id = "analysis_select_scenario_table"
      ),
    choices = ""),
  conditionalPanel(condition = "input.moduleSelector== 'module_4'",
    tags$b(icon('play'), amt(
        id = "analysis_select_from"
        )
      )
    ),
  #
  # select facility tmap and columns
  #
  conditionalPanel(condition = "!(input.moduleSelector=='module_6' & input.useExistingHf == 'FALSE')",
    selectInput("hfSelect", amt(
        id = "analysis_select_health_facility_vector"
        ),
      choices = ""),
    conditionalPanel(condition = "
      input.moduleSelector=='module_3' |
        input.moduleSelector=='module_4' |
        input.moduleSelector=='module_6'
      ",
      conditionalPanel("isNotEmpty(input.hfSelect) ",
        div(style = "margin-left:10%;",
          selectInput("hfIdxField", amt(
              id = "analysis_select_health_facility_id_field"
              ),
            choices = ""),
          selectInput("hfNameField", amt(
              id = "analysis_select_health_facility_name_field"
              ),
            choices = "") 
          )
        )
      ),
    conditionalPanel(condition = "input.moduleSelector=='module_4'",
      tags$b(icon("stop"),
        amt(
          id = "analysis_select_to"
          )
        ),
      selectInput("hfSelectTo", amt(
          id = "analysis_select_existing_health_facility_vector"
          ),
        choices = ""), 
      conditionalPanel("isNotEmpty(input.hfSelectTo) ",
        div(style = "margin-left:10%;",
          selectInput("hfIdxFieldTo", amt(
              id = "analysis_select_health_facility_id_field_to"
              ),
            choices = ""),
          selectInput("hfNameFieldTo", amt(
              id = "analysis_select_health_facility_name_field_to"
              ),
            choices = "") 
          )
        )
      ),
    #
    # Select health facilities capacity field  
    #
    conditionalPanel(condition = "(
      input.moduleSelector=='module_6' |
        input.moduleSelector=='module_3'
      ) && isNotEmpty(input.hfSelect)
      && input.mod3param.indexOf('ignoreCapacity') === -1",
    div(style = "margin-left:10%;",
      selectInput("hfCapacityField", amt(
          id = "analysis_select_health_facility_capacity"
          ),
        choices = ""
        )
      )
    )
  )
),
#
# Select cumulative cost map
#
conditionalPanel(condition = "(
  input.moduleSelector=='module_5'
  )",
selectInput("travelTimeSelect", amt(
    id = "analysis_select_travel_time_raster"
    ),
  choices = ""
  )
),
#
# Module 3 and 5 . Choose zonal map
#
conditionalPanel(condition = "
  (input.moduleSelector=='module_3' & 
    //input.zonalPopOption.indexOf('zonalCoverage') != -1 &
    input.mod3param.indexOf('zonalPop') != -1
  ) |
input.moduleSelector=='module_5' 
",
selectInput("zoneSelect", amt(
    id = "analysis_select_zone_vector"
    ),
  choices = ""
  ),
conditionalPanel("isNotEmpty(input.zoneSelect)",
  div(style = "margin-left:10%;",
    selectInput("zoneId", amt(
        id = "analysis_select_zone_id_integer"
        ),
      choices = ""
      ),
    selectInput("zoneLabel", amt(
        id = "analysis_select_zone_name_text"
        ),
      choices = ""
      )
    )
  )
),

conditionalPanel(condition = "(
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
      class='text-muted',
      amt('analysis_select_max_travel_time_desc')
    )
    ),
  value = '0'
)
),
conditionalPanel(condition = "(
  input.moduleSelector=='module_6'
  )",
#
# select external capacity table 
#
selectInput("capTableSelect", amt(
    id = "analysis_select_capacity_table"
    ),
  choices = ""
  ),
selectInput("suitabilityTableSelect", amt(
    id = "analysis_select_suitability_table"
    ),
  choices = ""
  ),
selectInput("exclusionTableSelect", amt(
    id = "analysis_select_exclusion_table"
    ),
  choices = ""
  )
)
)

