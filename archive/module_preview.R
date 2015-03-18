



output$modulePreview<-renderUI({
  tagList( 
    previewMapPanel,
    selectPanel
    )
})


selectPanel<-renderUI({
  sidebarPanel(width=12,
    fluidRow(width=12,
      column(width=8,
        h4('Map info'),
        selectizeInput('mapToPreview','Select map to preview',choices=dataList$raster),
        tags$b(p('Left-click on the selected map to get value')),
        hotable('previewValueTable')
        ),
      column(width=4,
        h4('Map diplay'),
        sliderInput('previewOpacity','Set the map opacity',min=0,max=1,value=0.8,step=0.1)
        )
      )
    )
})

previewMapPanel<-renderUI({
  mainPanel(width=12,
    leafletMap(
      "amPreviewMap", "100%", 500,
      #initialTileLayer = "//{s}.tiles.mapbox.com/v3/fxi.801dac55/{z}/{x}/{y}.png",
      #initialTileLayer = "//{s}.tiles.mapbox.com/v3/fxi.l2o6dd72/{z}/{x}/{y}.png",
      initialTileLayer = 
      'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',

      #initialTileLayer="http://a{s}.acetate.geoiq.com/tiles/terrain/{z}/{x}/{y}.png",
      #initialTileLayer="http://otile1.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg",
      #initialTileLayer="http://{s}.tile.osm.org/{z}/{x}/{y}.png",
      #initialTileLayer="http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
      #initialTileLayerAttribution = HTML('tiles:acetate.geoiq.com,data:OSM'),
      initialTileLayerAttribution = HTML("i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"),
      options=list(
        center = c(0,0),
        zoom=2,
        maxZoom = 18,
        zoomControl=FALSE
        ))
    )
})

observe({ 
  listen$previewMapReady<-ifelse(
    length(input$amPreviewMap_bounds)==4,
    TRUE,
    FALSE
    )
})

# if the location change and if the map is ready, change extent geojson.
changePreviewExtent<-reactive({
  mapReady<-listen$previewMapReady
  m <- listen$mapMeta
  if(!is.null(m) && !is.null(mapReady) && mapReady){
    amPreviewMap$addGeoJSON(amBboxGeoJson(m,proj='latlong'),'extent')
    bbx<-as.numeric(unlist(m$latlong$bbx$ext))
    amPreviewMap$fitBounds(bbx[3],bbx[2],bbx[4],bbx[1]) 
  }
})

# if mapToPreview change, evaluate reactive expression.
observe({
  mapToPreview<-input$mapToPreview
  if(!is.null(mapToPreview) && !mapToPreview==""){
    changePreviewExtent()
    amDebugMsg('change preview extent action')
  }
})


amRastQueryByLatLong<-function(coord,rasterName){
  coord<-SpatialPoints(data.frame(coord['x'],coord['y']))
  proj4string(coord)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
  coord<-spTransform(coord,CRS(getLocationProj()))@bbox[,'max']
  val<-execGRASS('r.what',map=rasterName,coordinates=c(coord[1],coord[2]),flags='f',intern=T) 
  val<-read.table(text=val,sep="|",stringsAsFactors=F)
  val[is.na(val)]<-'-'
  names(val)<-c('long','lat','lab','value','cat label')
  val$value<-val$value
  val$lab<-NULL
  return(val)
}



observe({
  clickCoord<-input$amPreviewMap_click
  mapToPreview<-isolate(input$mapToPreview)
  oldValues<-isolate(listen$previewValueTable)
  if(!is.null(mapToPreview) && !is.null(clickCoord)){
    clickCoord<-c(x=clickCoord$lng, y=clickCoord$lat)
    res<-amRastQueryByLatLong(clickCoord,mapToPreview)
  #  res<-data.frame(longitude=clickCoord['x'],latitude=clickCoord['y'],value=res)


    if(!is.null(oldValues)){ 
      allValues<-rbind(res,oldValues)
    }else{
      allValues=res
    }


    listen$previewValueTable<-allValues
    output$previewValueTable<-renderHotable(allValues,readOnly=T,fixed=2,stretch='last')   
  }
})




# set a region to extract from grass
observe({
  # preview parameter list
  pL              = list(
    # reactive
    leafletBounds = input$amPreviewMap_bounds,# leaflet bounds change
    mapToPreview  = input$mapToPreview, # map from dataList$raster
    opacity       = input$previewOpacity, # opacity change
    # isolate
    mapReady      = isolate(listen$previewMapReady),
    meta      = isolate(listen$mapMeta)
    )

  pL$mapToPreviewExists<-pL$mapToPreview %in% isolate(dataList$raster)

  ready<-!any(FALSE %in% pL || TRUE %in% sapply(pL,is.null))

  if(ready){
    amErrorAction({
      # render map : png path and boundingbox
      mapPreview<-amGrassLatLongPreview(
        mapToPreview=pL$mapToPreview,
        bbxSpLatLongLeaf=amBbxLeafToSp(pL$leafletBounds),
        bbxSpLatLongOrig=amBboxSp(pL$meta,proj='latlong'),
        mapCacheDir=cacheDir,
        resGrassEW=pL$meta$grid$`East-west`,
        resMax=400)
      # retrieve resulting intersecting bounding box
      bbx<-mapPreview$bbx
      # from local path to mapCache path,  registered as external ressource for shiny. (addRessourcePath)
      previewPath<-file.path('mapCache',basename(mapPreview$pngFile))

      # send data to map 
      amPreviewMap$addOverlay(
        bbx['y','min'],bbx['x','min'],
        bbx['y','max'],bbx['x','max'],
        previewPath, 'preview-test',
        options=list(
          opacity=pL$opacity
          )
        )
    },title='Map preview generator')
  }
})

# conversion of leaflet bounding box to sp object:
#  Leaflet has no bounding limit and sp does, crop leaflet box.
# to use this as standard bouding box, set CRS.
amBbxLeafToSp<-function(bbxLeaflet){
  if(!is.null(bbxLeaflet)){
    proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
    east<-pmax(pmin(bbxLeaflet$east,180),-180)
    west<-pmax(pmin(bbxLeaflet$west,180),-180)
    south<-pmax(pmin(bbxLeaflet$south,90),-90)
    north<-pmax(pmin(bbxLeaflet$north,90),-90)
    ext<-extent(c(east,west,south,north))
    ext<-as(ext,'SpatialPolygons')
    proj4string(ext)<-CRS(proj4dest)
    return(ext)
  }else{
    return(null)
  }
}




amGrassLatLongPreview<-function(
  mapToPreview=NULL, # map name to preview. ex. land_cover
  bbxSpLatLongLeaf, # bbx sp object with current region in lat/long (bbxLeafToSp(input$<map>_bounds))
  bbxSpLatLongOrig, # bbx sp object with current region in projected format
  mapCacheDir, # relative path to cache directory eg. ../data/cache. Must exists
  resGrassEW, # grass resolution for east-west. NOTE: could be extracted from "g.region -m | grep EW"
  resMax # maximum resolution of final file.
  ){
  toc<-function(...){
    if(exists('toc')){
      start=tic
      time=Sys.time()
      diff<-time-start
      message(paste(as.character(...),diff))
      diff
    }
  }
  tic<-Sys.time()
  # var naming convention for bounding boxes. NOTE: put everything in a list instead?
  # bbx<class><projection><label>
  # class : sp, vector, matrix
  # projection : Latitude Longitude, projected
  # label : leaflet, intersection, original
  if(!is.null(bbxSpLatLongLeaf) && !is.null(bbxSpLatLongOrig)){

    message('retrieve map from grass to create png file in lat long ')
    # define bounding box intersection.
    #get intersection betweed leaflet extent and project extent
    bbxSpLatLongInter<-gIntersection(bbxSpLatLongOrig,bbxSpLatLongLeaf)
    bbxMatLatLongInter<-bbxSpLatLongInter@bbox
    # to avoid to much cache files, round bbx values.
    # NOTE: if rendering time is short, skip this process ?
    bbxMatLatLongInterRound<-round(bbxMatLatLongInter,10)
    # file names
    cacheMap<-file.path(mapCacheDir,paste0(mapToPreview,"__",paste0(bbxMatLatLongInterRound,collapse="_"),'.png'))
    # don't evaluate if map is already in cache.
    if(!file.exists(cacheMap)){
      rmRastIfExists('MASK*')
      rmRastIfExists('tmp_*')
      rmVectIfExists('tmp_*')

      #create sp object with computed intersection extent and transform to grass orig projection
      bbxSpProjInter<-spTransform(bbxSpLatLongInter,CRS(getLocationProj()))
      #get resulting bbx
      bbxMatProjInter<-bbxSpProjInter@bbox
      #settting resolution. 
      resOverlay<-diff(bbxMatProjInter['x',])/resMax # max x resolution. Leaflet map is 800px, so..
      #resGrassNS<-metaOrig$summary$North
      res=ifelse(resOverlay>resGrassEW,resOverlay,resGrassEW)
      toc('start g.region')
      execGRASS('g.region',
        e=paste(bbxMatProjInter['x','max']),
        w=paste(bbxMatProjInter['x','min']),
        n=paste(bbxMatProjInter['y','max']),
        s=paste(bbxMatProjInter['y','min']),
        res=paste(resOverlay) 
        )
      toc('end g.region, start create mask from region')
      execGRASS('v.in.region',output='tmp_mask')
      execGRASS('r.mask',vector='tmp_mask')
      # compute preview map
      toc('start resampling at the new resolution')
      toc('end mapcalc, start r.out.png')
      # export in png with transparency and remove mask
      execGRASS('r.out.png',input=mapToPreview, output=cacheMap,flags=c('overwrite','w','t')) # with world file
      # NOTE: uncomment those lines if reprojection is needed. For a map preview, this should be ok...
      #  gdalwarp(tempMapPng,
      #    dstfile=tempMapTiff,
      #    #s_srs=metaOrig$projOrig,
      #    t_srs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
      #    output_Raster=FALSE,
      #    overwrite=TRUE)
      #  # as gdal can't warp directly in png (why?), translate it.
      #  gdal_translate(tempMapTiff,
      #    dst_dataset=cacheMap,
      #    ot='byte',
      #    of='PNG'
      #    )
      # set back the grass resgion to dem values.
      toc('end r.out.png, start g.region')
      execGRASS('g.region', raster='dem')
      toc('stop g.region, cleaning temp map')
      rmRastIfExists('MASK*')
      rmRastIfExists('tmp_*')
      rmVectIfExists('tmp_*')
    }
    message('retrieving done. in ',format(toc(),units='s'))
    return(list(
        pngFile=cacheMap,
        bbx=bbxMatLatLongInter
        ))   
  }
}

