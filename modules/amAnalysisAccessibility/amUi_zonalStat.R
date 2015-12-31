
fluidRow(class='amTableMargin',
  amCenterTitle('Zonal statistics',sub="Population coverage by zones, at selected travel time"),
  hotable('zoneCoverageTable'),
  amCenterTitle('Travel time',sub="Map of the selected travel time"),
  plotOutput('previewTravelTime')
)

