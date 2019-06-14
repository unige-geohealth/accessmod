
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
              id = "tool_map_options"
              )),
          content = tagList(
            selectizeInput("selBaseMap", amt(
                id = "toolbox_map_basemap_select"
                ),
              tileProviders
              )
            )
          ),
        'rasterSettings' = list(
          title = div(span(class='icon-grid_area_before'), amt(
              id = "tool_map_raster"
              )),
          content = tagList(
            selectizeInput('selectRasterToMap', amt(
                id = "toolbox_map_raster_select"
                ),
              choices = "",
              options = list(
                placeholder = ams('placeholder_enter_value'),
                onInitialize = I(
                  'function() {
                  this.setValue("");}'
                  )
                )
              ),
            conditionalPanel(
              condition = "input.showAdvancedTools === true",
              checkboxInput("checkInternalRasterDisplay", amt(
                  id = "data_internal_choice"
                  ),
                value = FALSE)
              ),
            sliderInput('previewOpacity', amt(
                id = "toolbox_map_opacity"
                ),
              min = 0,
              max = 1,
              value = 0.8,
              step = 0.1
              ),
            tags$label(
              amt(
                id = "toolbox_map_click_map"
                )
              ),
            uiOutput('uiMapClickRasterValue')
            )
          ),
        'facilitiesSettings' = list(
          title = div(icon('hospital-o'), amt(
              id = "tool_map_facilities"
              )
            ),
          content =  tagList(
            selectizeInput('selectFacilitiesToMap', amt(
                id = "toolbox_map_facilities_select"
                ),
              choices = "",
              options = list(
                placeholder = ams('placeholder_enter_value'),
                onInitialize = I(
                  'function() {
                  this.setValue("");}'
                  )
                )
              ),
            amRadioButtons(
              inputId = "relocateSaveMode",
              label = amt(
                id = "toolbox_map_relocate_save_mode"
                ),
              choiceNames = list(
                 amt('toolbox_map_relocate_save_mode_update'),
                 amt('toolbox_map_relocate_save_mode_new')
                ),
              choiceValues = list(
                 "update",
                 "new"
                ),
              selected = "update"
              ),
            hr(),
            conditionalPanel(
              condition = "input.relocateSaveMode == 'new'",
              textInput(
                inputId = "relocateTag", 
                label = amt(
                  id = "toolbox_map_relocate_tag"
                  ),
                value = "")
              ),
            actionButton("btnRelocateSave", amt(
                id = "toolbox_map_relocate_save"
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
