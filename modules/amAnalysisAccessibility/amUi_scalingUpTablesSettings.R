
wellPanel(
  amCenterTitle(div(icon("table"), amt(
    id = "analysis_scaleup_table",
	str = 'Scaling up tables'
	)),
	h = 3,
	m = 0,
	sub = amt(
      id = "analysis_scaleup_table_sub",
	  str = "Configure the tables for the scaling-up analysis."
	  )),
  amAccordionGroup(id = 'scalingUpSettings',
    style = 'margin-left:-5%;margin-right:-5%',
	show = NULL,
	itemList = list(
      'Suitability index' = list( 
        title = div(amt(
		  id = "analysis_scaleup_suit_index",
		  str = 'Suitability index'
		  )),
        content = tagList(
          #
          # Choice of factor for the suitability index
          #
          selectInput('selFactor', amt(
		  id = "analysis_scaleup_sel_factor",
		  str = "Select a factor for the suitability index"
		  ),
		  choices = c(
              'Sum of population within a radius' = 'popsum',
              'Euclidean distance from features' = 'dist',
              'Travel time from/to feature' = 'traveltime',
              'Generic priority map' = 'priority')
            ),
          conditionalPanel(condition = "input.selFactor == 'popsum'",
            numericInput('factorPopSumRadius',
              label = amt(
			    id = "analysis_scaleup_radius",
				str = 'Set a radius (km)'
				),
              value = 1,
              min = 0,
              max = 10,
              step = 1/1000
              ),
            p(span(id = "popSumNumCells","0"), amt(
			  id = "analysis_scaleup_cells_res",
			  str = " cells will be processed at each iteration, at current resolution."
			  ))
            ),
          conditionalPanel(condition = "input.selFactor == 'traveltime'",
            p(amt(
			  id = "analysis_scaleup_cap_param",
			  str = "Parameters to be applied on the capacity table"
			  )),
            radioButtons('factorTypeAnalysis', amt(
			  id = "analysis_scaleup_type",
			  str = 'Type of analysis'
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
				  id = "analysis_scaleup_travel_dir",
				  str = 'Direction of travel'
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
            label =  amt(
			  id = "analysis_scaleup_priority",
			  str = 'Direction of prioritization'
			  ),
            choices = c(
              'Higher values are more suitable' = 'hvms',
              'Higher values are less suitable' = 'hvls'
              ),
            selected = 'hvms'
            ),
          numericInput('factorWeight',
            label = amt(
			  id = "analysis_scaleup_sel_weight",
			  str = "Select factor weight"
			  ),
            min = 0,
            max = 10,
            value = 1
            ),
          selectInput('selFactorLayer', amt(
 		    id = "analysis_scaleup_sel_layer",
			str ='Select available layer'
			),
			choices = ""),
          actionButton('btnAddFactor', 
		    icon = icon('plus-circle'), amt(
			  id = "analysis_scaleup_add",
			  str ="Add"
			  ))
		)
      ),
      'exclusionAreas' = list(
        title = div(amt(
		  id = "analysis_scaleup_exclusion_main",
		  str ='Exclusion areas'
		  )),
        content = tagList(
          #
          #  Choice of exclusion area 
          #
          selectInput('selExclusion',amt(
		    id = "analysis_scaleup_exclusion",
			str ='Select exclusion areas (vector or raster)'
			),
			choices = ""),
          numericInput('exclusionBuffer',
            label = amt(
			  id = "analysis_scaleup_opt_buffer",
			  str ='Set an optional buffer (km)'
			  ),
            value = 5,
            min = 0,
            max = 99
            ),
          radioButtons('exclusionMethod',
            label = amt(
			  id = "analysis_scaleup_exclusion_mth",
			  str ='Choose exclusion method'
			  ),
            c(
              'Keep candidates outside the areas + buffer' = 'keepOutside',
              'Keep candidates inside the areas + buffer' = 'keepInside'
              )
            ),
          actionButton('btnAddExclusion',
		    icon = icon('plus-circle'), amt(
			  id = "analysis_scaleup_exclusion_add",
			  str ='Add'))
          )
      )
    )
  )
)
