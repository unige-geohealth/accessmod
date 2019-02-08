
fluidRow(
  amCenterTitle(amt(
    id = "analysis_travel_scenario_title",
    str = 'Travel scenario'
    ),
    sub = amt(
    id = "analysis_travel_scenario_sub",
    str = 'Define the speed of travel for each land cover class.'
    )),
  fluidRow(class = "amRowTable",
    h4(amt(
      id = "analysis_travel_scenario_processed",
      str = 'Travel scenario to be processed'
      )),
    conditionalPanel("!isNotEmpty(input.mergedSelect)",
      tags$p(amt(
        id = "analysis_travel_scenario_add_lc",
        str = "Please add a merged land cover"
        ))
      ),
    conditionalPanel("isNotEmpty(input.mergedSelect)",
      actionLink('speedTableUndo',
        icon = icon('undo'
        ),
        amt(
          id = "analysis_travel_scenario_reset",
          str = 'Reset to original content'
          )),
        '|',
      actionLink('speedTableMerge',
        icon = icon('magic'
        ),
        amt(
          id = "analysis_travel_scenario_import_selected_table",
          str = "Import content from the selected scenario table"
          )),
        '|',
      actionLink('helpLinkSpeedTable',
        icon = icon('question-circle'
        ),
        ''),
      hotable("speedRasterTable"
      ),
      h4(amt(
        id = "analysis_travel_scenario_selected_table",
        str = 'Selected scenario table'
        )),
      conditionalPanel("!isNotEmpty(input.modelSelect)",
        tags$p(amt(
          id = "analysis_travel_scenario_add_table",
          str = "Please add a scenario table"
          ))
        ),
      conditionalPanel("isNotEmpty(input.modelSelect)",
        uiOutput('speedTableMergeValidation'
        ),
        hotable("speedSqliteTable"
        ))
      )
    )
  )
