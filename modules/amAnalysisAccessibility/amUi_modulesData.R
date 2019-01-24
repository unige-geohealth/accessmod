wellPanel(
  amCenterTitle(div(icon('sign-in'), amt(
    id = "analysis_data_input",
    str = 'Data input'
    )),
    h = 3,
    m = 0,
    sub = amt(
      id = "analysis_data_input_sub",
      str = "Select the data to be used in this analysis"
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
      id = "analysis_select_pop",
      str = "Select population layer (raster)"
      ),
      choices = ""
      )
    ),
  conditionalPanel(condition = "(input.moduleSelector=='module_6')",
    selectInput("popResidualSelect", amt(
      id = "analysis_select_residual_pop",
      str = "Select residual population layer (raster)"
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
      id = "analysis_select_mlc",
      str = "Select merged land cover layer (raster)"
      ),
      choices = ""),
    selectInput("modelSelect", amt(
      id = "analysis_select_scenario",
      str = "Select scenario table (table)"
      ),
      choices = ""),
    conditionalPanel(condition = "input.moduleSelector== 'module_4'",
      tags$b(icon('play'), amt(
        id = "analysis_select_from",
        str = "From:"
        ))
      ),
  #
  # select facility tmap and columns
  #
  conditionalPanel(condition = "!(input.moduleSelector=='module_6' & input.useExistingHf == 'FALSE')",
    selectInput("hfSelect", amt(
      id = "analysis_select_hf",
      str = "Select existing health facilities layer (vector)"
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
            id = "analysis_select_hf_id_field",
            str = "Select facility ID field (unique)"
            ),
            choices = ""),
          selectInput("hfNameField", amt(
            id = "analysis_select_hf_name_field",
            str = "Select facility name field (text)"
            ),
            choices = "") 
          )
        )
      ),
    conditionalPanel(condition = "input.moduleSelector=='module_4'",
      tags$b(icon("stop"), amt(
        id = "analysis_select_to",
        str = "To:"
        )),
      selectInput("hfSelectTo", amt(
        id = "analysis_select_exist_hf",
        str = "Select existing health facilities layer (vector)"
        ),
        choices = ""), 
      conditionalPanel("isNotEmpty(input.hfSelectTo) ",
        div(style = "margin-left:10%;",
          selectInput("hfIdxFieldTo", amt(
            id = "analysis_select_hf_id_field_to",
            str = "Select facility ID field (unique)"
            ),
            choices = ""),
          selectInput("hfNameFieldTo", amt(
            id = "analysis_select_hf_name_field_to",
            str = "Select facility name field (text)"
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
      ) && isNotEmpty(input.hfSelect
      )",
      div(style = "margin-left:10%;",
        selectInput("hfCapacityField", amt(
          id = "analysis_select_hf_cap",
          str = "Select facilities capacity field (numeric):"
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
      id = "analysis_select_tt",
      str = "Select travel time layer (raster)"
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
      id = "analysis_select_zone",
      str = "Select zones layer (vector)"
      ),
      choices = ""
      ),
    conditionalPanel("isNotEmpty(input.zoneSelect)",
      div(style = "margin-left:10%;",
        selectInput("zoneId", amt(
          id = "analysis_select_zone_id",
          str = "Select zone unique ID (integer)"
          ),
          choices = ""
          ),
        selectInput("zoneLabel", amt(
          id = "analysis_select_zone_name",
          str = "Select zone name (text)"
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
  #sliderInput("sliderTimeAnalysis","Select maximum travel time [minutes]",value = 0,min = 0, max = 0,step = 1),
    numericInput("sliderTimeAnalysis", amt(
      id = "analysis_select_max_tt",
      str = "Select maximum travel time [minutes]"
      ),
      value = 0,
      min = 0,
      max = 0,
      step = 1
      )
    ),
  conditionalPanel(condition = "(
    input.moduleSelector=='module_6'
    )",
  #
  # select external capacity table 
  #
  selectInput("capTableSelect", amt(
    id = "analysis_select_table_cap",
    str = "Select existing capacity table"
    ),
    choices = ""
    ),
  selectInput("suitabilityTableSelect", amt(
    id = "analysis_select_table_suit",
    str = "Select existing suitability table"
    ),
    choices = ""
    ),
  selectInput("exclusionTableSelect", amt(
    id = "analysis_select_table_exclu",
    str = "Select existing exclusion table"
    ),
    choices = ""
    )
  )
)

