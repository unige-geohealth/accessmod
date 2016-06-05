
fluidRow(class='amTableMargin',
  amCenterTitle('Zonal statistics',sub="Population coverage by zone(s) for the selected maximum travel time"),
  conditionalPanel("!isNotEmpty(input.popSelect) || !isNotEmpty(input.travelTimeSelect) || !isNotEmpty(input.zoneSelect) ",
      tags$p("Please add population, travel time and zones layers.")
      ),
  conditionalPanel("isNotEmpty(input.popSelect) && isNotEmpty(input.travelTimeSelect) && isNotEmpty(input.zoneSelect) ",
  actionLink('helpLinkZoneCoverageTable',icon=icon('question-circle'),''),
  hotable('zoneCoverageTable')
  )
)

