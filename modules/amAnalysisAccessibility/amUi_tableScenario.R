
fluidRow(
  amCenterTitle('Travel scenario',sub='Define the speed of travel for each land cover class.'),
  fluidRow(class="amRowTable",
    h4('Travel scenario to be processed'),
    div(class="amTableMargin",
      actionLink('speedTableUndo',icon=icon('undo'),'Reset to original content'),'|',
      actionLink('speedTableMerge',icon=icon('magic'),"Import content from the selected scenario table"),'|',
      actionLink('helpLinkSpeedTable',icon=icon('question-circle'),''),
      hotable("speedRasterTable")
      )
    ),
  fluidRow(class="amRowTable",
    h4('Selected scenario table'),
    div(class="amTableMargin",
      uiOutput('speedTableMergeValidation'),
      hotable("speedSqliteTable")
      )
    )
  )

