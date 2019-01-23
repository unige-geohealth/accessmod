
fluidRow(class = 'amTableMargin',
  amCenterTitle(amt(
    id = "analysis_zonal_stat",
    str = 'Zonal statistics'
    ),
    sub = amt(
     id = "analysis_zonal_stat_sub",
     str = "Population coverage by zone(s) for the selected maximum travel time"
     )),
  conditionalPanel(
    "!isNotEmpty(input.popSelect) ||
    !isNotEmpty(input.travelTimeSelect) ||
    !isNotEmpty(input.zoneSelect) ",
    tags$p(amt(
      id = "analysis_zonal_stat_add",
      str = "Please add population, travel time and zones layers."
      ))
      ),
  conditionalPanel(
    "isNotEmpty(input.popSelect) &&
    isNotEmpty(input.travelTimeSelect) &&
    isNotEmpty(input.zoneSelect) ",
    actionLink('helpLinkZoneCoverageTable',
    icon = icon('question-circle'),
    ''),
  hotable('zoneCoverageTable')
  )
)

