fluidRow(
  amCenterTitle(amt(
    id = "analysis_table_scaleup_title"
    ),
    sub = amt(
      id = "analysis_table_scaleup_sub"
      )),
  fluidRow(class = "amRowTable",
    h4(amt(
      id = "analysis_table_scaleup_new_facility"
      )),
    conditionalPanel(
      "!isNotEmpty(input.mergedSelect) ||
      !isNotEmpty(input.hfSelect) ||
      !isNotEmpty(input.popSelect) ",
      tags$p(
        class = "callout callout-info",
        amt(
        id = "analysis_table_scaleup_add_items"
        ))
      ),
    conditionalPanel(
      "isNotEmpty(input.mergedSelect) ||
      isNotEmpty(input.hfSelect) ||
      isNotEmpty(input.popSelect) ",
      div(class = "amTableMargin",
        actionLink('btnAddRowCapacity',
          icon = icon("plus-circle"
          ),
          amt(
            id = "analysis_table_scaleup_add_row_btn"
            )),
        actionLink('btnRmRowCapacity',
          icon = icon("minus-circle"
          ),
          amt(
            id = "analysis_table_scaleup_rm_row_btn"
            )),
        hotable("capacityTable",
          height = "20vh"
        )),
      h4(amt(
        id = "analysis_table_scaleup_suitability"
        )),
      div(class = "amTableMargin",
        actionLink("btnResetSuitTable",
          icon = icon('undo'
          ),
          amt(
            id = "analysis_table_scaleup_reset_suitability_btn"
            )),
        actionLink("btnRmSuitTableUnselected",
          icon = icon('minus-circle'
          ),
          amt(
            id = "analysis_table_scaleup_rm_unselected_suitability_btn"
            )),
        hotable("suitabilityTable",
          height = "20vh"
        )),
      h4(amt(
        id = "analysis_table_scaleup_exclusion"
        )),
      div(class = "amTableMargin",
        actionLink("btnResetExcluTable",
        icon = icon('undo'
        ),
        amt(
          id = "analysis_table_scaleup_reset_exclusion_btn"
          )),
        actionLink("btnRmExcluUnselected",
        icon = icon('minus-circle'
        ),
        amt(
          id = "analysis_table_scaleup_rm_exclusion_btn"
          )),
        hotable("exclusionTable",
          height = "20vh"
        ))
      )
    )
  )
