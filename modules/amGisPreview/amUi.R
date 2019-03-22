
tileProviders = list(
  "Simple I" = "CartoDB.PositronNoLabels",
  "Dark" = "CartoDB.DarkMatterNoLabels",
  "Outdoors" = "Thunderforest.Outdoors",
  "Terrain" ="Esri.WorldTerrain",
  "Satellite I" = "Esri.WorldImagery",
  "Empty" = "empty"
  )


fluidRow(
  div(class = "col-xs-12 col-md-4",
    amAccordionGroup(id = 'mapTools',
      itemList = list(
        'mapOptions' = list(
          title = div(icon('map-o'), amt(
              id = "tool_map_options",
              str = "Options"
              )),
          content = tagList(
            selectizeInput("selBaseMap", amt(
                id = "toolbox_map_basemap_select",
                str = "Select a basemap"
                ),
              tileProviders
              )
            )
          ),
        'rasterSettings' = list(
          title = div(span(class='icon-grid_area_before'), amt(
              id = "tool_map_raster",
              str = "Raster"
              )),
          content = tagList(
            selectizeInput('selectRasterToMap', amt(
                id = "toolbox_map_raster_select",
                str = 'Select layer to use (raster)'
                ),
              choices = "",
              options = list(
                placeholder = 'Search',
                onInitialize = I(
                  'function() {
                  this.setValue("");}'
                  )
                )
              ),
            sliderInput('previewOpacity', amt(
                id = "toolbox_map_opacity",
                str = 'Set layer opacity'
                ),
              min = 0,
              max = 1,
              value = 0.8,
              step = 0.1
              ),
            tags$label(
              amt(
                id = "toolbox_map_click_map",
                str = 'Click the map to query the selected raster.'
                )
              ),
            uiOutput('uiMapClickRasterValue')
            )
          ),
        'facilitiesSettings' = list(
          title = div(icon('hospital-o'), amt(
              id = "tool_map_facilities",
              str = "Facilities"
              )
            ),
          content =  tagList(
            selectizeInput('selectFacilitiesToMap', amt(
                id = "toolbox_map_facilities_select",
                str = 'Select facilities to use ( vector )'
                ),
              choices = "",
              options = list(
                placeholder = 'Search',
                onInitialize = I(
                  'function() {
                  this.setValue("");}'
                  )
                )
              ),
            radioButtons(
              inputId = "relocateSaveMode",
              label = amt(
                id = "toolbox_map_relocate_save_mode",
                str = "Relocate save mode"
                ),
              # NOTE: Newer version of shiny offer choicesNames/choicesValue,
              # needed by translation with amt
              choices = list(
                "Update existing layer" = "update" ,
                "Create new facilities layer" = "new"
                ),
              selected = "update"
              ),
            hr(),
            conditionalPanel(
              condition = "input.relocateSaveMode == 'new'",
              textInput(
                inputId = "relocateTag", 
                label = amt(
                  id = "toolbox_map_relocate_tag",
                  str = "Add short tags"
                  ),
                value = "")
              ),
            actionButton("btnRelocateSave", amt(
                id = "toolbox_map_relocate_save",
                str = "Save changes"
                )
              ),
            hr(),
            uiOutput("uiValidationRelocate")
            )
          )
        )
      )
    ),
  div(class = "col-xs-12 col-md-8",
    leafletOutput("mapPreview","100%","500")
    )
  )

#arcGisAttrib<-"i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community"
#  #'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
#  #,HTML(arcGisAttrib)
