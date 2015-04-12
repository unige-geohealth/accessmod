#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## map preview 


observe({
  maps<-dataList$raster
  updateSelectInput(session,"mapToPreview",choices=maps,selected=maps[1])
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

  emptyJson<- list(type="Point",coordinates=c(0,0))
  m <- listen$mapMeta
  if(!is.null(m) && isTRUE(mapReady)){
    extentType<-input$showExtent
    listen$zoneMap<-amNameCheck(grep('^zone_admin__*',dataList$vector,value=T)[1],'vector')
    if(isTRUE(extentType=='extZone') && isTRUE(!is.null(listen$zoneMap))){
      amPreviewMap$addGeoJSON(emptyJson,'extent')
      amPreviewMap$addGeoJSON(addSpotLight(),'spotLight')
    }else{ 
      amPreviewMap$addGeoJSON(emptyJson,'spotLight')
      amPreviewMap$addGeoJSON(amBboxGeoJson(m,proj='latlong'),'extent')
    }
    bbx<-as.numeric(unlist(m$latlong$bbx$ext))
    amPreviewMap$fitBounds(bbx[3],bbx[2],bbx[4],bbx[1]) 
  }
})



# if mapToPreview change, evaluate reactive expression.
observe({
  mapToPreview<-input$mapToPreview
  if(!is.null(mapToPreview) && !mapToPreview==""){
    changePreviewExtent()
  }
})





# test geojson spotlight 
addSpotLight<-reactive({
  zoneMap<-listen$zoneMap
  amErrorAction(title='amGeojsonSpotLight',{
    if(!is.null(zoneMap)){
      proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
      bbxStyle<-list(
        fillColor = "black",
        fillOpacity = 0.6,
        opacity=0.1,
        weight = 1,
        color = "#000000"
        )
      worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
      spotLightTime<-system.time({ 
          # convert to raster at low resolution to speed up the process. 
          execGRASS('g.region',
            res=paste(5000))
          execGRASS('v.to.rast',
            input=zoneMap,
            output='tmp_zone',
            type='area',
            use='val',
            value=1,
            flags='overwrite')
          execGRASS('r.buffer',
            input='tmp_zone',
            output='tmp_zone_buffer',
            distances=10000,
            flags='overwrite'
            )
          execGRASS('r.mapcalc',
            expression='tmp_a = tmp_zone_buffer/tmp_zone_buffer',
            flags='overwrite'
            )
          execGRASS('r.to.vect',
            input='tmp_a',
            output='tmp_b',
            type='area',
            flags='overwrite'
            )
          execGRASS('g.region',raster=config$mapDem)
          area<-read.table(
            text=execGRASS('v.to.db',map='tmp_b',flags=c('c','p'),option='area',intern=T),
            sep='|',
            header=T)
          #areaTot<-area[area$cat=='total area','area']
          area<-area[!area$cat=='total area',]
          areaMax<-max(area$area)
          areaMaxCat<-as.character(area[area$area==areaMax,]$cat)
          execGRASS('v.extract',
            input='tmp_b',
            output='tmp_c',
            cats=areaMaxCat,
            flags='overwrite'
            )
          if(is.na(areaMax) || !is.finite(areaMax)) areaMax=1e5
          execGRASS('v.generalize',
            input='tmp_c',
            output='tmp_d',
            method='snakes',
            threshold=1e5,
            flags='overwrite'
            )
          execGRASS('v.clean',
            input='tmp_d',
            output='tmp_e',
            tool='rmarea',
            threshold=areaMax-1000,
            flags='overwrite'
            )
        spotLight<-readVECT('tmp_e')
        spotLight<-spTransform(spotLight,CRS(proj4dest))
        spotLight<-geojson_list(spotLight)[[1]]
        spotLightCoord<-spotLight$features[[1]]$geometry$coordinates[[1]]
        spotLight$features[[1]]$geometry$coordinates<-list(worldCoord,spotLightCoord)
        spotLight$style<-bbxStyle
        return(spotLight)
      })
      amDebugMsg('Preview spotlight in',spotLightTime)
    }
    })
})


amRastQueryByLatLong<-function(coord,rasterName,projOrig,projDest){
  coord<-SpatialPoints(data.frame(coord['x'],coord['y']))
  #proj4string(coord)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
  proj4string(coord)<-projDest
  #coord<-spTransform(coord,CRS(getLocationProj()))@bbox[,'max']
  coord<-spTransform(coord,CRS(projOrig))@bbox[,'max']
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
  isolate({
    mapToPreview<-amNameCheck(input$mapToPreview,'raster')
    oldValues<-listen$previewValueTable
    if(!is.null(mapToPreview) && !is.null(clickCoord)){
      clickCoord<-c(x=clickCoord$lng, y=clickCoord$lat)
      res<-amRastQueryByLatLong(
        clickCoord,
        mapToPreview,
        projOrig=listen$mapMeta$orig$proj,
        projDest=listen$mapMeta$latlong$proj)
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
})




# set a region to extract from grass
observe({
  # preview parameter list
  pL              = list(
    # reactive
    leafletBounds = input$amPreviewMap_bounds,# leaflet bounds change
    mapToPreview  = amNameCheck(input$mapToPreview,'raster'), # map from dataList$raster
    opacity       = input$previewOpacity, # opacity change
    # isolate
    mapReady      = isolate(listen$previewMapReady),
    meta      = isolate(listen$mapMeta)
    )
  isolate({

    ready<-!any(FALSE %in% pL || TRUE %in% sapply(pL,is.null))

    if(ready){
      amErrorAction({
        # render map : png path and boundingbox
        mapPreview<-amGrassLatLongPreview(
          mapToPreview=pL$mapToPreview,
          bbxSpLatLongLeaf=amBbxLeafToSp(pL$leafletBounds),
          bbxSpLatLongOrig=amBboxSp(pL$meta,proj='latlong'),
          mapCacheDir=config$pathCacheDir,
          resGrassEW=pL$meta$grid$`East-west`,
          resMax=400,
          projOrig=listen$mapMeta$orig$proj,
          projDest=listen$mapMeta$latlong$proj 
          )
        if(is.null(mapPreview))return(NULL)
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
  resMax, # maximum resolution of final file.
  projOrig,
  projDest
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
    if(is.null(bbxSpLatLongInter))return(NULL)
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
      bbxSpProjInter<-spTransform(bbxSpLatLongInter,CRS(projOrig))
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
      execGRASS('g.region', raster=config$mapDem)
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

## test geojson spotlight based on raster
# NOTE: did not work well with sparse raster : could create a lot of island.
#addSpotLight<-reactive({
#  mapToPreview<-amNameCheck(input$mapToPreview,'raster')
#  mapReady<-listen$previewMapReady
#  amErrorAction(title='amGeojsonSpotLight',{
#    if(!is.null(mapToPreview) && !mapToPreview=="" && isTRUE(mapReady)){
#      proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
#      bbxStyle<-list(
#        fillColor = "black",
#        fillOpacity = 0.6,
#        opacity=0.1,
#        weight = 1,
#        color = "#000000"
#        )
#      worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
#      spotLightTime<-system.time({
#        #execGRASS('r.mask',raster=mapToPreview)
#        #execGRASS('r.mapcalc',expression=paste("tmp__a =",mapToPreview,'/',mapToPreview),flags='overwrite')
#        #execGRASS('r.mask',flags="r")
#        execGRASS('g.region',res=paste(5000))
#        system.time(
#        execGRASS('r.mapcalc',expression=paste("tmp__a =",mapToPreview,'/',mapToPreview),flags='overwrite')
#        execGRASS('r.mapcalc',expression=paste(mapToPreview," = tmp__a/tmp__a"),flags='overwrite')
#        execGRASS('r.buffer',input=mapToPreview,output='tmp__a',distances=10000,flags='overwrite')
#        )
#        execGRASS('r.mapcalc',expression="tmp__b = tmp__a/tmp__a",flags='overwrite')
#        execGRASS('r.to.vect',input='tmp__b',output='tmp__b',type='area',flags='overwrite')
#        execGRASS('v.generalize',input='tmp__b',output='tmp_c',method='snakes', threshold=1,flags='overwrite')
#        area<-read.table(
#          text=execGRASS('v.to.db',map='tmp_c',flags=c('c','p'),option='area',intern=T),
#          sep='|',
#          header=T)
#        areaTot<-area[area$cat=='total area','area']
#        if(is.na(areaTot) || !is.finite(areaTot)) areaTot=1e5
#        execGRASS('v.clean',input='tmp_c',output='tmp_d',tool=c('rmarea','rmline'),threshold=areaTot/10,flags='overwrite')
#        browser()
#        spotLight<-readVECT('tmp_d')
#        spotLight<-spTransform(spotLight,CRS(proj4dest))
#        spotLight<-fromJSON(geojson_json(spotLight)[[1]])
#        spotLightCoord<-spotLight$features[[1]]$geometry$coordinates[[1]]
#        spotLight$features[[1]]$geometry$coordinates<-list(worldCoord,spotLightCoord)
#        spotLight$style<-bbxStyle
#        amPreviewMap$addGeoJSON(spotLight,'spotLight')
#      })
#      amDebugMsg('Preview spotlight in',spotLightTime)
#    }
#    })
#})
#
