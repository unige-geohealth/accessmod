
fluidRow(class = 'amTableMargin',
  amCenterTitle(amt(
      id = "analysis_zonal_stat_title"
      ),
    sub = amt(
      id = "analysis_zonal_stat_sub"
      )),
  conditionalPanel(
    "!isNotEmpty(input.popSelect) ||
    !isNotEmpty(input.travelTimeSelect) ||
    !isNotEmpty(input.zoneSelect) ",
  tags$p(
    class = "callout callout-info",
    amt(
      id = "analysis_zonal_stat_add"
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

