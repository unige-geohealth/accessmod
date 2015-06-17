




# other map tiles providers :
#initialTileLayer = "//{s}.tiles.mapbox.com/v3/fxi.801dac55/{z}/{x}/{y}.png",
#initialTileLayer = "//{s}.tiles.mapbox.com/v3/fxi.l2o6dd72/{z}/{x}/{y}.png",
#initialTileLayer="http://a{s}.acetate.geoiq.com/tiles/terrain/{z}/{x}/{y}.png",
#initialTileLayer="http://otile1.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg",
#initialTileLayer="http://{s}.tile.osm.org/{z}/{x}/{y}.png",
#initialTileLayer="http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
#initialTileLayerAttribution = HTML('tiles:acetate.geoiq.com,data:OSM'),
#arcgisonline attribution
arcGisAttrib<-"i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"

tagList(
 mainPanel(width=12,
    leafletMap(
      "amPreviewMap", "100%", 500,
         initialTileLayer = 
      'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      initialTileLayerAttribution = HTML(arcGisAttrib),
      options=list(
        center = c(0,0),
        zoom=2,
        maxZoom = 18,
        zoomControl=FALSE
        ))
    ),
  sidebarPanel(width=12,
    fluidRow(width=12,
      column(width=8,
        h4('Map info'),
        selectizeInput('mapToPreview','Select data to preview (raster)',choices=""),
        div(p(tags$b('Data values'),'(click on the map).')),
        hotable('previewValueTable')
        ),
      column(width=4,
        h4('Map display'),
     #   radioButtons('showExtent','Extent spotlight',choices=list(
     #       'Bounding box'='extBbx',
     #       'Administrative'='extZone'
     #       ),
     #     ),
        sliderInput('previewOpacity','Set data opacity',min=0,max=1,value=0.8,step=0.1)
        )
      )
    )
   )
