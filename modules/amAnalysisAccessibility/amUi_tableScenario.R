
fluidRow(
  amCenterTitle(amt(
      id = "analysis_travel_scenario_title"
      ),
    sub = amt(
      id = "analysis_travel_scenario_sub"
      )),
  fluidRow(class = "amRowTable",
    h4(amt(
        id = "analysis_travel_scenario_processed"
        )),
    conditionalPanel("!isNotEmpty(input.mergedSelect)",
      tags$p(
        class = "callout callout-info",
        amt(
          id = "analysis_travel_scenario_add_lc"
          ))
      ),
    conditionalPanel("isNotEmpty(input.mergedSelect)",
      actionLink('speedTableUndo',
        icon = icon('undo'
          ),
        amt(
          id = "analysis_travel_scenario_reset"
          )),
      '|',
      actionLink('speedTableMerge',
        icon = icon('magic'
          ),
        amt(
          id = "analysis_travel_scenario_import_selected_table"
          )),
      '|',
      actionLink('helpLinkSpeedTable',
        icon = icon('question-circle'
          ),
        ''),
      hotable("speedRasterTable",
        height = "30vh"
        ),
      h4(amt(
          id = "analysis_travel_scenario_selected_table"
          )),
      conditionalPanel("!isNotEmpty(input.modelSelect)",
        tags$p(
          class = "callout callout-info",
          amt(
            id = "analysis_travel_scenario_add_table"
            )
          )
        ),
      conditionalPanel("isNotEmpty(input.modelSelect)",
        uiOutput('speedTableMergeValidation'),
        hotable("speedSqliteTable",
          height = "30vh"
          )
        )
      )
    )
  )
