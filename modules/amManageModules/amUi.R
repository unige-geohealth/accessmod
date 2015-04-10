

#TODO: create new function "amCheckBoxButtonGroup" to generate UI from a list of input.
moduleInlineBtn<-
#"<div id='moduleSelector' class='btn-group form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline' data-toggle='buttons'>
"<div id='moduleSelector' class='btn-group' data-toggle='buttons'>
<label class='btn btn-default btn-inline active'>
<input type='radio' id='ms1' name='moduleSelector' checked='checked' value='module_1' />
<i class='fa fa-list'></i> Merge landcover
</label>
<label class='btn btn-default btn-inline'>
<input type='radio' id='ms2' name='moduleSelector' value='module_2' />
<i class='fa fa-clock-o'></i> Accessibility analysis
</label>
<label class='btn btn-default btn-inline'>
<input type='radio' id='ms3' name='moduleSelector' value='module_3' />
<i class='fa fa-hospital-o'></i> Geographic coverage analysis
</label>
<label class='btn btn-default btn-inline'>
<input type='radio' id='ms4' name='moduleSelector' value='module_4' />
<i class='fa fa-table'></i> Referral accessibility analysis
</label>
<label class='btn btn-default btn-inline'>
<input type='radio' id='ms5' name='moduleSelector' value='module_5' />
<i class='fa fa-bar-chart'></i> Zonal potential coverage
</label>

</div>"



fluidRow(
 mainPanel( width=12,HTML(gsub('\n','',moduleInlineBtn))),
 hr(),
  mainPanel(width=12,
    conditionalPanel(condition="input.moduleSelector=='module_1'",
      loadUi('modules/amAnalysisMergeLandCover/amUi.R')
      ),
    conditionalPanel(condition="
      input.moduleSelector=='module_2' || 
      input.moduleSelector=='module_3' || 
      input.moduleSelector=='module_4' ||
      input.moduleSelector=='module_5'
      ",loadUi('modules/amAnalysisAccessibility/amUi.R')
      )

    )
  )

