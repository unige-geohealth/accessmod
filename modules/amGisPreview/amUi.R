


tileProviders = list(
  "Simple I" = "CartoDB.PositronNoLabels",
  "Dark" = "CartoDB.DarkMatterNoLabels",
  "Outdoors" = "Thunderforest.Outdoors",
  "Terrain" ="Esri.WorldTerrain",
  "Satellite I" = "Esri.WorldImagery",
  "Empty" = "empty"
  )




fluidRow(
  sidebarPanel(width=12,
    fluidRow(width=12,
      column(width=8,
        h4('Map information'),

        selectizeInput('mapToPreview','Select data to preview (raster)',choices="",
          options = list(
            placeholder = 'Search ...',
            onInitialize = I(
              'function() {
              this.setValue("");
}'
)
            )
          ),
        div(p(tags$b('Data values'),'(click on the map).')),
        hotable('previewValueTable')
        ),
      column(width=4,
        h4('Map display'),
        sliderInput('previewOpacity','Set data opacity',min=0,max=1,value=0.8,step=0.1),
        selectizeInput("selBaseMap","Select a basemap",tileProviders)
#        checkboxInput("checkShowLegend","Show legend",value=FALSE)
        )
      )
    ),
  mainPanel(width=12,
   tagList(
    leafletOutput("mapPreview","100%","500")
    )
   )
  )

#arcGisAttrib<-"i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
    #  #'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
    #  #,HTML(arcGisAttrib)
