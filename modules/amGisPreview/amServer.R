#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

source("tools/R/amLeafletPatch.R")

#
# Add cache path
#


idModule <- "module_toolbox"

#
# create leaflet map
#
observe({
  output$mapPreview <- renderLeaflet({
    leaflet() %>%
      mapOptions(zoomToLimits = "first") %>%
      addScale()
  })
},suspended = TRUE) %>% amStoreObs(idModule,"render_leaflet")

#
# Set provider layer
#
observe({
  if(input$selBaseMap != "empty"){
    leafletProxy("mapPreview") %>%
      addProviderTiles(
        input$selBaseMap,
        layerId = "baselayer"
      )
  }else{
    leafletProxy("mapPreview") %>%
      removeTiles("baselayer") 
  }
},suspended = TRUE) %>% amStoreObs(idModule,"base_map")


#
# Update raster layer to map
#
observe({
  showInternal <- isTRUE(input$checkInternalRasterDisplay)

  update <- listen$dataListUpdated

  isolate({
  clRaster <- config$dataClass$type == "raster"
  clInternal <- config$dataClass$internal
  clChoices <- TRUE

  if(showInternal){
    clChoices <- clRaster
  }else{
    clChoices <- clRaster & !clInternal
  }

  dc <- config$dataClass[clChoices,'class']


    amUpdateSelectChoice(
      idData = dc,
      idSelect = "selectRasterToMap",
      dataList = dataList
    )
  })

},suspended = TRUE) %>% amStoreObs(idModule,"raster_list")

#
# Set default extent based on project
#
observe({
  m <- listen$mapMeta
  bbx <- as.numeric(unlist(m$latlong$bbx$ext))


  leafletProxy("mapPreview") %>%
    fitBounds(bbx[1],bbx[3],bbx[2],bbx[4]) 
},suspended = TRUE) %>% amStoreObs(idModule,"map_meta")

#
# Get attribute data for raster click
#
observe({
  clickCoord <- input$mapPreview_click
  selectRasterToMap <- amNameCheck(dataList,input$selectRasterToMap,'raster')
  isolate({
    if(!is.null(selectRasterToMap) && !is.null(clickCoord)){

      clickCoord <- c(x = clickCoord$lng, y = clickCoord$lat)
      tbl <- amRastQueryByLatLong(
        clickCoord,
        selectRasterToMap,
        projOrig = listen$mapMeta$orig$proj,
        projDest = listen$mapMeta$latlong$proj
      )
    }else{
      tbl = data.frame(long = '-',lat = '-',value = '-',label = '-')
    }

    output$uiMapClickRasterValue <- renderUI({
      tags$ul(class="list",
        tags$li(
          tags$label('Easting'),': ',
          tbl$long
          ),
        tags$li(
          tags$label('Northing'),': ',
          tbl$lat
          ),
        tags$li(
          tags$label('Value'),': ',
          tbl$value
          ),
        tags$li(
          tags$label('Label'),': ',
          tbl$label
        )
      )
    })

    #output$previewValueTable <- renderHotable(tbl,readOnly = T,fixed = 2,stretch = 'last')   
  })
},suspended = TRUE) %>% amStoreObs(idModule,"render_click")


#
# Debounce reactive raster preview
#
reactPreview <- reactive({


  pL = list(
    leafletBounds = input$mapPreview_bounds,# leaflet bounds change
    selectRasterToMap = amNameCheck(dataList,input$selectRasterToMap,'raster'), # map from dataList$raster
    meta = isolate(listen$mapMeta)
  )

}) %>% amReactiveDebounce(2000)


#
# Update opacity
#
observe({
  opacity <- input$previewOpacity 
  selectRaster <- input$selectRasterToMap

  isolate({
    # validate
    selectRaster <- amNameCheck(dataList,selectRaster,'raster')
    hasRaster <- !amNoDataCheck(selectRaster)


    bounds <- input$mapPreview_bounds 
    hasMap <- !amNoDataCheck(bounds)

    if( hasMap && hasRaster ){


      leafletProxy("mapPreview") %>%
        setPngOpacity('rasterPreview',opacity)
    }
  })
})

#
# Get raster extract from grass 
#
observe({ 
  amErrorAction(
    title = 'Map preview generator',
    {
      # preview parameter list
      pL <- reactPreview()

      isolate({
        ready <- !any(TRUE %in% sapply(pL,is.null))
        noRaster <- amNoDataCheck(pL$selectRasterToMap)
        opacity <- input$previewOpacity

        if(noRaster){


          leafletProxy("mapPreview") %>%
            removeImage('rasterPreview')
        }

        if(ready){

          # render map : png path and boundingbox

          rasterPreview <- amGrassLatLongPreview(
            raster = pL$selectRasterToMap,
            bbxSpLatLongLeaf = amBbxLeafToSp(pL$leafletBounds),
            bbxSpLatLongOrig = amBboxSp(pL$meta,proj = 'latlong'),
            mapCacheDir = config$pathCacheDir,
            width = 800, #note: find correct map width
            projOrig = listen$mapMeta$orig$proj,
            projDest = listen$mapMeta$latlong$proj 
          )

          # retrieve resulting intersecting bounding box
          bbx <- rasterPreview$bbx
          pngMap <- rasterPreview$pngMap
          pngExists <- !amNoDataCheck(pngMap) && file.exists(pngMap)

          if(!pngExists){
            return()
          }

          leafletProxy("mapPreview") %>%
            removeImage('rasterPreview') %>%
            addPng(
              layerId = "rasterPreview",
              lat1 = bbx['y','min'],
              lng1 = bbx['x','min'],
              lat2 = bbx['y','max'],
              lng2 = bbx['x','max'],
              imgUrl = file.path('cache',basename(pngMap)),
              options = list(
                opacity = opacity
              )
            )
        }  
      })
    })
},suspended = TRUE) %>% amStoreObs(idModule,"map_move")

#
# Facilities choice
#
observe({
  #
  # New layer have been added by this module
  #
  updateFacilities <- listen$updateSelectFacilitiesToMap

  #
  # Generic data list updated 
  #
  update <- listen$dataListUpdated

  #
  # Defaults
  #
  hasUpdateFacilities <- !amNoDataCheck(updateFacilities)
  selected <- NULL
  if(hasUpdateFacilities){
    selected <-  updateFacilities$selected
  }

  isolate({
    amUpdateSelectChoice(
      idData = c("vFacilityNew", "vFacility"),
      idSelect = c("selectFacilitiesToMap"),
      dataList = dataList,
      selected = selected
    )
  })
},suspended = TRUE) %>% amStoreObs(idModule, "map_update_select_hf")

#
# Update columns names select
#
observeEvent(input$selectFacilitiesToMap,{
  dbCon <- grassSession$dbCon
  hf <- input$selectFacilitiesToMap
  if(amNoDataCheck(hf) ){
    return()
  }
  hf <- amNameCheck(dataList,
    name = hf,
    class = 'vector'
  )
  cols <- dbListFields(dbCon,amNoMapset(hf))
  updateSelectInput(session,'selectFacilitiesLabel',choices=cols)
},suspended = TRUE) %>% amStoreObs(idModule, "map_update_select_hf_label")

#
# Save selected facilites in react object
#
reactFacilities <- reactive({

  dbCon <- grassSession$dbCon
  update <- listen$updateSelectFacilitiesToMap
  hf <- input$selectFacilitiesToMap
  toProj <- listen$mapMeta$latlong$proj

  cols <- dbListFields(dbCon,amNoMapset(hf))
  hf <- amNameCheck(dataList,
    name = hf,
    class = 'vector'
  )
  hfSpDf <- readVECT(hf)
  hfSpDfReproj <- sp::spTransform(hfSpDf,toProj)


  #
  # Return reprojected vector as spatial dataframe
  #
  return(hfSpDfReproj)
})



#
# Save raster value of selected facilities
#
reactFacilitiesRasterValue <- reactive({

  update <- listen$updateSelectFacilitiesToMap
  hf <- input$selectFacilitiesToMap
  toProj <- listen$mapMeta$latlong$proj
  rast <-  input$selectRasterToMap

  rast <- amNameCheck(dataList,
    name = rast,
    class = 'raster'
  )

  hf <- amNameCheck(dataList,
    name = hf,
    class = 'vector'
  )

  hfSpDf <- reactFacilities()

  if(!amNoDataCheck(rast)){

    tbl <- amGetFacilitiesTableWhatRast(hf,rast)
    names(tbl) <- c('cat','amRasterValue')
    hfSpDf <- merge(hfSpDf,tbl,by=c("cat"))
  }else{
    hfSpDf$amRasterValue <- NA
  }

  return(hfSpDf)
})


#
# Add hf to relocate
#
observe({
  amErrorAction(
    title = "Add hf to relocate",
    { 

      #
      # Force add HF, in case of update - same name, shiny does not invalidate.
      #
      update <- listen$updateSelectFacilitiesToMap
      label <- input$selectFacilitiesLabel
      if(amNoDataCheck(label)){
        return();
      }
      #
      # Check if HF is present in dataList
      #
      hf <- amNameCheck(dataList,
        name = input$selectFacilitiesToMap,
        class = 'vector'
      )

      rast <- amNameCheck(dataList,
        name = input$selectRasterToMap,
        class = 'raster'
      )

      if(amNoDataCheck(hf)){
        leafletProxy("mapPreview") %>%
          removeMarkersRelocate(
            layerId = 'hf'
          )

        return()
      }

      #
      # Add HF
      #
      hfSpDf <- reactFacilitiesRasterValue()

      leafletProxy("mapPreview") %>%
        removeMarkersRelocate(
          layerId = 'hf'
          ) %>%
      addMarkersRelocate(
        layerId = 'hf',
        data = hfSpDf,
        label = label
      )

    })

},suspended = TRUE) %>% amStoreObs(idModule, "map_select_hf")

#
# Listen to drag end, update values
#
observeEvent(input$mapPreview_marker_dragend,{
  amErrorAction(
    title = "Save relocation update value",
    {  


      marker <- input$mapPreview_marker_dragend
      marker$time <- Sys.time()

      hf <- amNameCheck(dataList,
        name = input$selectFacilitiesToMap,
        class = 'vector'
      )

      rast <- amNameCheck(dataList,
        name = input$selectRasterToMap,
        class = 'raster'
      )
    
      if(!amNoDataCheck(marker$id)){
        value <- NULL
        lat <- marker$lat
        lng <- marker$lng
        markerCoord <- c(x = lng, y = lat)

        if(amRastExists(rast)){

          tbl <- amRastQueryByLatLong(
            markerCoord,
            rast,
            projOrig = listen$mapMeta$orig$proj,
            projDest = listen$mapMeta$latlong$proj,
            nullValue = "*"
          )
          value <- tbl$value
          #
          # rGrass does not support NA as null/na string, 
          # convert this here
          #
          value <- ifelse(value == "*",NA,value)
        }
        
        leafletProxy("mapPreview") %>%
          updateMarkerRelocate(
            layerId = 'hf',
            markerId = marker$id,
            value = value,
            lat = lat,
            lng = lng
          )
      }

    })

},suspended = TRUE) %>% amStoreObs(idModule, "map_listen_dragend")

#
# Validate
#
observe({
  amErrorAction(
    title = "Save relocation validation",
    {  

      err <- NULL
      info <- NULL
      msgList <- NULL
      outMap <- character(0)
      relocateTag <- input$relocateTag
      state <- input$mapPreview_state 
      outName <- list()
      modeSaveNew <- isTRUE(input$relocateSaveMode == "new")
      hasChange <- isTRUE(state$isEnabled) && isTRUE(state$hasHistory) 
      nChanges <- ifelse(hasChange,length(state$changes),0)

      #
      # Check input
      #
      facilitiesSelected <- amNameCheck(dataList,
        name = input$selectFacilitiesToMap,
        class = 'vector'
      )
      hasLayer <- !amNoDataCheck(facilitiesSelected)

      #
      # Check if has tags
      #
      hasTag <- isTRUE(
        !any(
          tags == '',
          is.null(relocateTag),
          nchar(relocateTag) < 1 
        )
      )

      #
      # Add error of not valid layer selected
      #
      if(!hasLayer){
        err <- tagList(err,
          amt(
            id = "tool_map_relocate_missing_layer"
          )
        )
      }

      #
      # Output dataset name
      #
      if(hasLayer){
        if(modeSaveNew){
          outName <- amCreateNames('vFacility',relocateTag,dataList,FALSE)
        }else{
          outName <- list(
            html = list(tags$b(class="text-warning",amTagsFileToDisplay(facilitiesSelected), '( overwrite )')),
            file = facilitiesSelected
          )
        }
      }

      #
      # Add info message
      #
      if( hasLayer ){
        info <- tagList(info,
          amt(
            id = "tool_map_relocate_changes_count",
            children = tags$span(
              nChanges
            )
          )
        )
      }

      #
      # Add error of no tags and user set save new mode
      #
      if(hasLayer && !hasTag && modeSaveNew){
        err <- tagList(err,
          amt(
            id = "tool_map_relocate_missing_tags"
          )
        )
      }

      #
      # Build ui message
      #
      hasError <- length(err) > 0
      hasInfo <- length(info) > 0

      #
      # Build info message
      #
      if(hasInfo){
        info <- lapply(info,function(e){
          if(!amNoDataCheck(e)){
            div(
              icon("info-circle"),
              tags$b(e)
              )}
        })
        msgList <- tagList(msgList,
          tags$div(
            tags$b(
              amt(
                id = "tool_map_validation_info_notice"
              )
              ),
            tags$div(
              class="text-info",
              info
            )
          )
        )
      }

      #
      # Build errors message
      #
      if(hasError){
        err <- lapply(err,function(e){
          if(!amNoDataCheck(e)){
            div(
              icon("exclamation-triangle"),
              e
              )}
        })
        msgList <- tagList(
          msgList,
          tags$b(
            amt(
              id = "tool_map_validation_issues_notice"
            )
            ),
          tags$div(
            class ="text-danger",
            err
          )
        )
      }else{

        #
        # Output data name message
        #
        out <-  lapply(outName$html,function(o){
          if(!amNoDataCheck(o)){
            div(
              icon("sign-out"),
              o
            )
          }
        })
        msgList <- tagList( 
          msgList,
          tags$b(
            amt(
              id = "tool_map_relocate_out_name"
            )
            ), 
          tags$div(
            class = "text-info",
            out
          ) 
        )
      }


      #
      # Output validation message
      #
      output$uiValidationRelocate <- renderUI({
        msgList
      })

      #
      # Update buttons
      #
      amActionButtonToggle(
        session = session,
        'btnRelocateSave',
        disable = !hasLayer || !hasChange || hasError
      )

      amSelectizeToggle('selectFacilitiesToMap',
        disable = hasChange
      )
      amSelectizeToggle('selectRasterToMap',
        disable = hasChange
      )

      listen$relocateData <- list(
        valid = !hasError && hasLayer && hasChange,
        changes = state$changes,
        outName = outName$file 
      )

    })

},suspended = TRUE) %>% amStoreObs(idModule, "map_listen_relocation_changes")



observeEvent(input$btnRelocateSave,{
  amErrorAction(
    title = "Save relocation data",
    {
      state <- listen$relocateData
      toProj <- listen$mapMeta$orig$proj
      fromProj <- listen$mapMeta$latlong$proj

      if(amNoDataCheck(state) && !isTRUE(state$valid) ) return()

      #
      # Block next save action
      #
      amActionButtonToggle(
        session = session,
        'btnRelocateSave',
        disable = TRUE
      )

      #
      # Get reactive hf, extract coordinates
      #
      hf <- reactFacilities()
      dfHf <- as.data.frame(hf)
      coords <- dfHf[,c('coords.x1','coords.x2')]
      coordsOrig <- coords
      coords$cat <- hf$cat
      changes <- state$changes
      dfHf[,c('coords.x1','coords.x2')] <- NULL

      #
      # For all changes, updates coordinates
      #
      for(change in changes){
        coords[coords$cat == change$id,c('coords.x1','coords.x2')] <- c(change$lng,change$lat)
      }

      #
      # Set coordinates, crs
      #
      coordinates(dfHf) <- coords[,c('coords.x1','coords.x2')]
      crs(dfHf) <- fromProj
      spDf <- sp::spTransform(dfHf,toProj)
      spDf$cat <- NULL

      spDf$am_p_lng <- coordsOrig$coords.x1
      spDf$am_p_lng <- coordsOrig$coords.x2

      #
      # Write data 
      #
      rmVectIfExists(state$outName)
      writeVECT(
        SDF = spDf,
        vname = state$outName,
        v.in.ogr_flags = c("o", "overwrite"),
        driver = "GPKG"
      )
      
      #
      # Show a message
      #
      outputDatasets <- tags$ul(
        HTML(paste("<li>", amTagsFileToDisplay(state$outName), "</li>"))
      )

      msg <- tagList(
        p(
          tagList(
            sprintf(
              ams("tool_map_relocate_out_saved"),
              length(changes)
            )
          )
          ),
        outputDatasets
      )

      amMsg(session,
        type = 'message',
        title = amt(
          id = "tool_map_relocate_process_finished"
          ),
        text = msg
      )

      #
      #
      #
      amUpdateDataList(listen)
      amReMemoizeCostlyFunctions()
      listen$outFiles <- state$outName

      #
      # Remove saved facilities
      #
      leafletProxy("mapPreview") %>%
        removeMarkersRelocate(
          layerId = 'hf'
        )
      #
      # Force add to map in case outName == oldName
      #
      listen$updateSelectFacilitiesToMap <- list(
        trigger = runif(1),
        selected = amAddMapset(listen$outFiles)
      )

      #
      # Invalidate selected facilities table
      #
      listen$updateFacilitiesTables <- runif(1)

      #
      # Remove old tags
      #
      updateTextInput(session,
        inputId = 'relocateTag',
        value = ""
      )
    })
})



