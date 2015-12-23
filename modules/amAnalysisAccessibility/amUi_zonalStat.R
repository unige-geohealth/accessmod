
fluidRow(class='amTableMargin',
  amCenterTitle('Zonal statistic',sub="Population coverage by zone(s) for the accessibility analysis"),
  hotable('zoneCoverageTable'),
  amCenterTitle('Travel time preview',sub="Map of the selected travel time"),
  plotOutput('previewTravelTime')
)

