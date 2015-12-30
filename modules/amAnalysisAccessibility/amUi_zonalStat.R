
fluidRow(class='amTableMargin',
  amCenterTitle('Zonal statistic',sub="Population coverage by zone(s), at selected travel time"),
  hotable('zoneCoverageTable'),
  amCenterTitle('Travel time',sub="Map of the selected travel time"),
  plotOutput('previewTravelTime')
)

