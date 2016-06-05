
fluidRow(
  amCenterTitle('Travel scenario',sub='Define the speed of travel for each land cover class.'),
  fluidRow(class="amRowTable",
    h4('Travel scenario to be processed'),
    conditionalPanel("!isNotEmpty(input.mergedSelect)",
      tags$p("Please add a merged land cover")
      ),
    conditionalPanel("isNotEmpty(input.mergedSelect)",
      actionLink('speedTableUndo',icon=icon('undo'),'Reset to original content'),'|',
      actionLink('speedTableMerge',icon=icon('magic'),"Import content from the selected scenario table"),'|',
      actionLink('helpLinkSpeedTable',icon=icon('question-circle'),''),
      hotable("speedRasterTable"),
      h4('Selected scenario table'),
      conditionalPanel("!isNotEmpty(input.modelSelect)",
        tags$p("Please add a scenario table")
        ),
      conditionalPanel("isNotEmpty(input.modelSelect)",
        uiOutput('speedTableMergeValidation'),
        hotable("speedSqliteTable")
        )
      )
    )

  )
