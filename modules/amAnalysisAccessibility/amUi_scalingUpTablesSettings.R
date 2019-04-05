
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
          choices = c(
              'Sum of population within a radius' = 'popsum',
              'Euclidean distance from features' = 'dist',
              'Travel time from/to feature' = 'traveltime',
              'Generic priority map' = 'priority'
              )
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
            p(span(id = "popSumNumCells","0"), amt(
              id = "analysis_scaleup_cells_resolution_warning"
              ))
            ),
          conditionalPanel(condition = "input.selFactor == 'traveltime'",
            p(amt(
              id = "analysis_scaleup_capacity_parameters"
              )),
            radioButtons('factorTypeAnalysis', amt(
              id = "analysis_scaleup_type"
              ),
              c('Isotropic (ignore DEM)' = 'iso',
                'Anisotropic (use DEM)' = 'aniso'
                ),
              selected = 'iso',
              inline = FALSE
              ),
            conditionalPanel(condition = "input.factorTypeAnalysis=='aniso'",
              radioButtons('factorTravelDirection',
                label = amt(
                  id = "analysis_scaleup_travel_direction"
                  ),
                  choices = c(
                    "From feature" = "from",
                    "Towards feature" = "to"),
                selected = 'to',
                inline = FALSE
                )
              )
            ),
          radioButtons('factorDirection',
            label = amt(
              id = "analysis_scaleup_priority"
              ),
            choices = c(
              'Higher values are more suitable' = 'hvms',
              'Higher values are less suitable' = 'hvls'
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
          radioButtons('exclusionMethod',
            label = amt(
              id = "analysis_scaleup_exclusion_method"
              ),
            c(
              'Keep candidates outside the areas + buffer' = 'keepOutside',
              'Keep candidates inside the areas + buffer' = 'keepInside'
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
