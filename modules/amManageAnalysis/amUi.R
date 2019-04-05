#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module selector / top menu in analysis

moduleInlineBtn <- div(id = 'moduleSelector',
  class = "form-group shiny-input-radiogroup shiny-input-container-inline shiny-flow-layout",
    'data-toggle' = 'buttons',
    style = 'line-height:37px;',
    tags$label('for' = 'moduleSelector'
    ),
    tags$label(class = "btn btn-default active btn-inline w6-100",
    tags$input(type = "radio",
      id = "ms2",
      name = "moduleSelector",
      value = 'module_2',
      checked = 'checked'
      ),
    icon('clock-o'), amt(
      id = "analysis_accessibility_main_tab"
      )
    ),
  tags$label(class = "btn btn-default btn-inline w6-100",
    tags$input(type = "radio",
      id = "ms3",
      name = "moduleSelector",
      value = 'module_3'
      ),
    icon('hospital-o'), amt(
      id = "analysis_coverage_main_tab"
      )
    ),
  tags$label(class = "btn btn-default btn-inline w6-100 ",
    tags$input(type = "radio",
      id = "ms4",
      name = "moduleSelector",
      value = 'module_4'
      ),
    icon('table'), amt(
      id = "analysis_referral_main_tab"
      )
    ),
  tags$label(class = "btn btn-default btn-inline w6-100",
    tags$input(type = "radio",
      id = "ms5", 
      name = "moduleSelector",
      value = 'module_5'
      ),
    icon('bar-chart'), amt(
      id = "analysis_zonal_stats_main_tab"
      )
    ),
  tags$label(class = "btn btn-default btn-inline w6-100 ",
    tags$input(type = "radio",
      id = "ms6",
      name = "moduleSelector",
      value = 'module_6'
      ),
    icon('plus'), amt(
      id = "analysis_scalingup_main_tab"
      )
    )
  )


fluidRow(
  column(width = 12,
    HTML(gsub('\n', '', moduleInlineBtn
    ))),
  hr(),
  column(width = 12,
 #   conditionalPanel(condition = "
      #input.moduleSelector=='module_1'
      #",loadUi('modules/amAnalysisMergeLandCover/amUi.R')
      #),
    conditionalPanel(condition = "
      input.moduleSelector=='module_2' || 
      input.moduleSelector=='module_3' || 
      input.moduleSelector=='module_4' ||
      input.moduleSelector=='module_5' ||
       input.moduleSelector=='module_6'  
      ", loadUi('modules/amAnalysisAccessibility/amUi.R'
      ))
    )
  )




