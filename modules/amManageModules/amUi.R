
#"<div id='moduleSelector' class='btn-group form-group shiny-input-radiogroup shiny-input-container' data-toggle='buttons'>

#TODO: create new function "amCheckBoxButtonGroup" to generate UI from a list of input.
moduleInlineBtn<-div(id='moduleSelector',
  class="btn-group form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline",
  'data-toggle'='buttons',
  tags$label('for'='moduleSelector'),
  tags$label(class="btn btn-default btn-inline active",
    tags$input(type="radio",id="ms1",name="moduleSelector",checked='checked',value='module_1'),
    icon('list'),'Merge landcover'
    ),
  tags$label(class="btn btn-default btn-inline",
    tags$input(type="radio",id="ms2",name="moduleSelector",value='module_2'),
    icon('clock-o'),'Accessibility analysis'
    ),
  tags$label(class="btn btn-default btn-inline",
    tags$input(type="radio",id="ms3",name="moduleSelector",value='module_3'),
    icon('hospital-o'),'Geographic coverage analysis'
    ),
  tags$label(class="btn btn-default btn-inline",
    tags$input(type="radio",id="ms4",name="moduleSelector",value='module_4'),
    icon('table'),'Referral accessibility analysis'
    ),
  tags$label(class="btn btn-default btn-inline",
    tags$input(type="radio",id="ms5",name="moduleSelector",value='module_5'),
    icon('bar-chart'),'Zonal potential coverage'
    )
 # ,tags$label(class="btn btn-default btn-inline",
 #   tags$input(type="radio",id="ms6",name="moduleSelector",value='module_6'),
 #   icon('plus'),'Scalling up'
 #   )
  )

fluidRow(
  mainPanel( width=12,HTML(gsub('\n','',moduleInlineBtn))),
  hr(),
  mainPanel(width=12,
    conditionalPanel(condition="
      input.moduleSelector=='module_1'
      ",loadUi('modules/amAnalysisMergeLandCover/amUi.R')
      ),
    conditionalPanel(condition="
      input.moduleSelector=='module_2' || 
      input.moduleSelector=='module_3' || 
      input.moduleSelector=='module_4' ||
      input.moduleSelector=='module_5' 
      // input.moduleSelector=='module_6'  
      ",loadUi('modules/amAnalysisAccessibility/amUi.R')
      )

    )
  )

