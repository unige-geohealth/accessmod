
wellPanel(
  amCenterTitle(div(icon("table"), amt(
    id = "analysis_scaleup_table"
    )),
    h = 3,
    m = 0,
    sub = amt(
      id = "analysis_scaleup_table_sub"
      )),
  amAccordionGroup(id = 'scalingUpSettings',
    style = 'margin-left:-5%;margin-right:-5%',
    show = NULL,
    itemList = list(
      'Suitability index' = list(
        title = div(amt(
          id = "analysis_scaleup_suitability_index_title"
          )),
        content = tagList(
          #
          # Choice of factor for the suitability index
          #
          selectInput('selFactor', amt(
          id = "analysis_scaleup_select_factor"
          ),
          choices = NULL 
          ),
          conditionalPanel(condition = "input.selFactor == 'popsum'",
            numericInput('factorPopSumRadius',
              label = amt(
                id = "analysis_scaleup_set_radius"
                ),
              value = 1,
              min = 0,
              max = 10,
              step = 1/1000
              ),
            p(span(id = "popSumNumCells"))
            ),
          conditionalPanel(condition = "input.selFactor == 'traveltime'",
            p(amt(
              id = "analysis_scaleup_capacity_parameters"
              )),
            amRadioButtons('factorTypeAnalysis', amt(
              id = "analysis_scaleup_type"
              ),
              choiceNames = list(
                amt("analysis_settings_isotropic"),
                amt("analysis_settings_anisotropic")
                ),
              choiceValues = list("iso","aniso"),
              selected = 'iso',
              inline = FALSE
              ),
            conditionalPanel(condition = "input.factorTypeAnalysis=='aniso'",
              amRadioButtons('factorTravelDirection',
                label = amt(
                  id = "analysis_scaleup_travel_direction"
                  ),
                choiceNames = list(
                  amt('analysis_settings_travel_from_hf'),
                  amt('analysis_settings_travel_to_hf')
                  ),
                choiceValues = list("from","to"),
                selected = 'to',
                inline = FALSE
                )
              )
            ),
          amRadioButtons('factorDirection',
            label = amt(
              id = "analysis_scaleup_priority"
              ),
            choiceNames = list(
              amt("analysis_scaleup_priority_hvms"),
              amt("analysis_scaleup_priority_hvls")
              ),
            choiceValues = list(
              "hvms",
              "hvls"
              ),
            selected = 'hvms'
            ),
          numericInput('factorWeight',
            label = amt(
              id = "analysis_scaleup_select_weight"
              ),
            min = 0,
            max = 10,
            value = 1
            ),
          selectInput('selFactorLayer', amt(
             id = "analysis_scaleup_select_layer"
            ),
            choices = ""),
          actionButton('btnAddFactor', 
            icon = icon('plus-circle'), amt(
              id = "analysis_scaleup_add_btn"
              )
            )
          )
        ),
      'exclusionAreas' = list(
        title = div(amt(
          id = "analysis_scaleup_exclusion_main"
          )),
        content = tagList(
          #
          #  Choice of exclusion area 
          #
          selectInput('selExclusion', amt(
            id = "analysis_scaleup_select_exclusion"
            ),
            choices = ""
            ),
          numericInput('exclusionBuffer',
            label = amt(
              id = "analysis_scaleup_optional_buffer"
              ),
            value = 5,
            min = 0,
            max = 99
            ),
          amRadioButtons('exclusionMethod',
            label = amt(
              id = "analysis_scaleup_exclusion_method"
              ),
            choiceNames = list(
               amt("analysis_scaleup_exclusion_method_keep_inside"),
               amt("analysis_scaleup_exclusion_method_keep_outside")
              ),
            choiceValues = list(
              "keep_inside",
              "keep_outside"
              )
            ),
          actionButton('btnAddExclusion',
            icon = icon('plus-circle'), amt(
              id = "analysis_scaleup_exclusion_add_btn"
              )
            )
          )
        )
      )
    )
  )
