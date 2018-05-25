#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module selector / top menu in analysis

toolInlineBtn<-div(id='toolSelector',
  class="form-group shiny-input-radiogroup shiny-input-container-inline shiny-flow-layout",
  'data-toggle'='buttons',style='line-height:37px;',
  tags$label('for'='toolSelector'),
  tags$label(class="btn btn-default btn-inline active",
    tags$input(type="radio",id="tool2",name="toolSelector",value='tool_map_preview',checked='checked'),
    icon('map-o'),'Raster preview'
    ),
  tags$label(class="btn btn-default btn-inline",
    tags$input(type="radio",id="tool1",name="toolSelector",value='tool_merge_landcover'),
    icon('list'),'Merge land cover'
    )
  )

fluidRow(
  column( width=12,HTML(gsub('\n','',toolInlineBtn))),
  hr(),
  column(width=12,
    conditionalPanel(condition="
      input.toolSelector=='tool_merge_landcover'
      ",loadUi('modules/amAnalysisMergeLandCover/amUi.R')
      ),
    conditionalPanel(condition="
      input.toolSelector=='tool_map_preview'
      ",loadUi('modules/amGisPreview/amUi.R')
      )
     )
  )



