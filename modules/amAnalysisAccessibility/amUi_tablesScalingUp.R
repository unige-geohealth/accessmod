fluidRow(
  amCenterTitle(amt(
    id = "analysis_tb_scaleup",
	str = 'Scaling up'
	),
    sub = amt(
      id = "analysis_tb_scaleup_sub",
	  str = "Configuration tables for the scaling up algorithm."
	  )),
  fluidRow(class = "amRowTable",
    h4(amt(
      id = "analysis_tb_scaleup_new",
	  str = 'Capacity table for new facilities creation'
	  )),
    conditionalPanel(
	  "!isNotEmpty(input.mergedSelect) ||
 	  !isNotEmpty(input.hfSelect) ||
	  !isNotEmpty(input.popSelect) ",
      tags$p(amt(
	    id = "analysis_tb_scaleup_add_items",
		str = "Please add merged land cover, facilities and population"
		))
      ),
    conditionalPanel(
	  "isNotEmpty(input.mergedSelect) ||
	  isNotEmpty(input.hfSelect) ||
	  isNotEmpty(input.popSelect) ",
      div(class = "amTableMargin",
        actionLink('btnAddRowCapacity',
		  icon = icon("plus-circle"),
		  amt(
		    id = "analysis_tb_scaleup_add_row",
			str = 'Add row'
			)),
        actionLink('btnRmRowCapacity',
		  icon = icon("minus-circle"),
		  amt(
		    id = "analysis_tb_scaleup_rm_row",
			str = 'Remove row'
			)),
        hotable("capacityTable")
        ),
      h4(amt(
	    id = "analysis_tb_scaleup_suit",
		str = 'Suitability factors'
		)),
      div(class = "amTableMargin",
        actionLink("btnResetSuitTable",
		  icon = icon('undo'),
		  amt(
		    id = "analysis_tb_scaleup_reset",
			str = "Reset"
			)),
        actionLink("btnRmSuitTableUnselected",
		  icon = icon('minus-circle'),
		  amt(
		    id = "analysis_tb_scaleup_rm_unsel",
			str = "Remove unselected row"
			)),
        hotable("suitabilityTable")
        ),
      h4(amt(
	    id = "analysis_tb_scaleup_exclusion",
		str = 'Exclusion areas'
		)),
      div(class = "amTableMargin",
        actionLink("btnResetExcluTable",
		icon = icon('undo'),
		amt(
		  id = "analysis_tb_scaleup_reset",
		  str = "Reset"
		  )),
        actionLink("btnRmExcluUnselected",
		icon = icon('minus-circle'),
		amt(
		  id = "analysis_tb_scaleup_rm_unsel",
		  str = "Remove unselected row"
		  )),
        hotable("exclusionTable")
        )
      )
    )
  )
