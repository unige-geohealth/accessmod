#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## map preview 


observe({
  amModEnabled<-listen$tabControl_module_preview  
  if(isTRUE(!is.null(amModEnabled) && amModEnabled)){
    # directory for map cache
    addResourcePath('mapCache',config$pathCacheDir)
    # create leaflet map
    #amMap <- createLeafletMap(session, "amMap")
    amPreviewMap <- createLeafletMap(session, "amPreviewMap")




    observe({
      maps<-dataList$raster
      updateSelectInput(session,"mapToPreview",choices=maps)
    })

    observe({ 
      ready=length(input$amPreviewMap_bounds)==4
      listen$previewMapReady<-ifelse(ready,TRUE,FALSE)
    })

    # if the location change and if the map is ready, change extent geojson.
    changePreviewExtent<-reactive({
      mapReady<-listen$previewMapReady
      m <- listen$mapMeta
      if(!is.null(m) && isTRUE(mapReady)){
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
      }
    })

    observe({
      clickCoord<-input$amPreviewMap_click
      isolate({
        mapToPreview<-amNameCheck(dataList,input$mapToPreview,'raster')
        if(!is.null(mapToPreview) && !is.null(clickCoord)){
          clickCoord<-c(x=clickCoord$lng, y=clickCoord$lat)
          tbl<-amRastQueryByLatLong(
            clickCoord,
            mapToPreview,
            projOrig=listen$mapMeta$orig$proj,
            projDest=listen$mapMeta$latlong$proj)
          #  res<-data.frame(longitude=clickCoord['x'],latitude=clickCoord['y'],value=res)
        }else{
          tbl=data.frame(long='-',lat='-',value='-',label='-')
        }
        output$previewValueTable<-renderHotable(tbl,readOnly=T,fixed=2,stretch='last')   
      })
    })

    # set a region to extract from grass
    observe({
      # preview parameter list
      pL              = list(
        # reactive
        leafletBounds = input$amPreviewMap_bounds,# leaflet bounds change
        mapToPreview  = amNameCheck(dataList,input$mapToPreview,'raster'), # map from dataList$raster
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
              resGrassEW=pL$meta$grid$ewres,
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
  }
})

