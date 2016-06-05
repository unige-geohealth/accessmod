fluidRow(
  amCenterTitle('Scaling up',sub="Configuration tables for the scaling up algorithm."),
  fluidRow(class="amRowTable",
    h4('Capacity table for new facilities creation'),
    conditionalPanel("!isNotEmpty(input.mergedSelect) || !isNotEmpty(input.hfSelect) || !isNotEmpty(input.popSelect) ",
      tags$p("Please add merged land cover, facilities and population")
      ),
    conditionalPanel("isNotEmpty(input.mergedSelect) || isNotEmpty(input.hfSelect) || isNotEmpty(input.popSelect) ",
      div(class="amTableMargin",
        actionLink('btnAddRowCapacity',icon=icon("plus-circle"),'Add row'),
        actionLink('btnRmRowCapacity',icon=icon("minus-circle"),'Remove row'),
        hotable("capacityTable")
        ),
      h4('Suitability factors'),
      div(class="amTableMargin",
        actionLink("btnResetSuitTable",icon=icon('undo'),"Reset"),
        actionLink("btnRmSuitTableUnselected",icon=icon('minus-circle'),"Remove unselected row"),
        hotable("suitabilityTable")
        ),
      h4('Exclusion areas'),
      div(class="amTableMargin",
        actionLink("btnResetExcluTable",icon=icon('undo'),"Reset"),
        actionLink("btnRmExcluUnselected",icon=icon('minus-circle'),"Remove unselected row"),
        hotable("exclusionTable")
        )
      )
    )
  )
