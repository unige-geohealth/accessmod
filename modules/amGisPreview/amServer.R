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

#
#   observe({
#      clickCoord<-input$amPreviewMap_click
#      isolate({
#        mapToPreview<-amNameCheck(dataList,input$mapToPreview,'raster')
#        oldValues<-listen$previewValueTable
#        if(!is.null(mapToPreview) && !is.null(clickCoord)){
#          clickCoord<-c(x=clickCoord$lng, y=clickCoord$lat)
#          res<-amRastQueryByLatLong(
#            clickCoord,
#            mapToPreview,
#            projOrig=listen$mapMeta$orig$proj,
#            projDest=listen$mapMeta$latlong$proj)
#          #  res<-data.frame(longitude=clickCoord['x'],latitude=clickCoord['y'],value=res)
#          if(!is.null(oldValues)){ 
#            allValues<-rbind(res,oldValues)
#          }else{
#            allValues=res
#          }
#          listen$previewValueTable<-allValues
#          output$previewValueTable<-renderHotable(allValues,readOnly=T,fixed=2,stretch='last')   
#        }
#      })
#    })
#

#
#    # test geojson spotlight 
#    addSpotLight<-reactive({
#      zoneMap<-listen$zoneMap
#      amErrorAction(title='amGeojsonSpotLight',{
#        if(!is.null(zoneMap)){
#          proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
#          bbxStyle<-list(
#            fillColor = "black",
#            fillOpacity = 0.6,
#            opacity=0.1,
#            weight = 1,
#            color = "#000000"
#            )
#          worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
#          spotLightTime<-system.time({ 
#            # convert to raster at low resolution to speed up the process. 
#            execGRASS('g.region',
#              res=paste(5000))
#            execGRASS('v.to.rast',
#              input=zoneMap,
#              output='tmp_zone',
#              type='area',
#              use='val',
#              value=1,
#              flags='overwrite')
#            execGRASS('r.buffer',
#              input='tmp_zone',
#              output='tmp_zone_buffer',
#              distances=10000,
#              flags='overwrite'
#              )
#            execGRASS('r.mapcalc',
#              expression='tmp_a = tmp_zone_buffer/tmp_zone_buffer',
#              flags='overwrite'
#              )
#            execGRASS('r.to.vect',
#              input='tmp_a',
#              output='tmp_b',
#              type='area',
#              flags='overwrite'
#              )
#            execGRASS('g.region',raster=config$mapDem)
#            area<-read.table(
#              text=execGRASS('v.to.db',map='tmp_b',flags=c('c','p'),option='area',intern=T),
#              sep='|',
#              header=T)
#            #areaTot<-area[area$cat=='total area','area']
#            area<-area[!area$cat=='total area',]
#            areaMax<-max(area$area)
#            areaMaxCat<-as.character(area[area$area==areaMax,]$cat)
#            execGRASS('v.extract',
#              input='tmp_b',
#              output='tmp_c',
#              cats=areaMaxCat,
#              flags='overwrite'
#              )
#            if(is.na(areaMax) || !is.finite(areaMax)) areaMax=1e5
#            execGRASS('v.generalize',
#              input='tmp_c',
#              output='tmp_d',
#              method='snakes',
#              threshold=1e5,
#              flags='overwrite'
#              )
#            execGRASS('v.clean',
#              input='tmp_d',
#              output='tmp_e',
#              tool='rmarea',
#              threshold=areaMax-1000,
#              flags='overwrite'
#              )
#            spotLight<-readVECT('tmp_e')
#            spotLight<-spTransform(spotLight,CRS(proj4dest))
#            spotLight<-geojson_list(spotLight)[[1]]
#            spotLightCoord<-spotLight$features[[1]]$geometry$coordinates[[1]]
#            spotLight$features[[1]]$geometry$coordinates<-list(worldCoord,spotLightCoord)
#            spotLight$style<-bbxStyle
#            return(spotLight)
#          })
#          amDebugMsg('Preview spotlight in',spotLightTime)
#        }
#        })
#    })
#


#  # if the location change and if the map is ready, change extent geojson.
#    changePreviewExtent<-reactive({
#      mapReady<-listen$previewMapReady
#
#      emptyJson<- list(type="Point",coordinates=c(0,0))
#      m <- listen$mapMeta
#      if(!is.null(m) && isTRUE(mapReady)){
#        extentType<-input$showExtent
#        listen$zoneMap<-amNameCheck(dataList,grep('^zone_admin__*',dataList$vector,value=T)[1],'vector')
#        if(isTRUE(extentType=='extZone') && isTRUE(!is.null(listen$zoneMap))){
#          amPreviewMap$addGeoJSON(emptyJson,'extent')
#          amPreviewMap$addGeoJSON(addSpotLight(),'spotLight')
#        }else{ 
#          amPreviewMap$addGeoJSON(emptyJson,'spotLight')
#          amPreviewMap$addGeoJSON(amBboxGeoJson(m,proj='latlong'),'extent')
#        }
#        bbx<-as.numeric(unlist(m$latlong$bbx$ext))
#        amPreviewMap$fitBounds(bbx[3],bbx[2],bbx[4],bbx[1]) 
#      }
#    })
#
#


