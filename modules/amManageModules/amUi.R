
#"<div id='moduleSelector' class='btn-group form-group shiny-input-radiogroup shiny-input-container' data-toggle='buttons'>





#testList<-list('bar-chart'='a','table'='b','clock-o'='c')  
#names(testList)<-c(HTML(icon('open')),HTML(icon('close')),HTML(icon('play')))  




jsModuleSelectFun = "function(item,escape){
var lab = \"\" ;
var fa = \"\";
switch(item.value){
  case 'module_1':
  lab = \"Merge land cover \";
  fa = \"list\";
  break;
  case 'module_2':
  lab = \"Accessibility analysis\";
  fa = \"clock-o\";
  break;
  case 'module_3':
  lab = \"Geographic coverage analysis\";
  fa = \"hospital-o\";
  break;
  case 'module_4':
  lab= \"Referral accessibility analysis\";
  fa=\"table\";
  break;
  case 'module_5':
  lab= \"Zonal statistics\";
  fa = \"bar-chart\";
  break;
};
return '<div><i class=\"fa fa-'+fa+'\"></i>  ' + lab+ '</div>' ;
}"

testList=c(
  "module_1",
  "module_2",
  "module_3",
  "module_4",
  "module_5"
  )




testOption= list(render=I(
    sprintf("{
      item:%s , 
      option:%s 
}",jsModuleSelectFun,jsModuleSelectFun)
))




  #
  #
  #  #TODO: create new function "amCheckBoxButtonGroup" to generate UI from a list of input.
  #  moduleInlineBtn<-div(id='moduleSelector',
  #    class="btn-group form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline",
  #    'data-toggle'='buttons',
  #    tags$label('for'='moduleSelector'),
  #    tags$label(class="btn btn-default btn-inline active",
  #      tags$input(type="radio",id="ms1",name="moduleSelector",checked='checked',value='module_1'),
  #      icon('list'),'Merge landcover'
  #      ),
  #    tags$label(class="btn btn-default btn-inline",
  #      tags$input(type="radio",id="ms2",name="moduleSelector",value='module_2'),
  #      icon('clock-o'),'Accessibility analysis'
  #      ),
  #    tags$label(class="btn btn-default btn-inline",
  #      tags$input(type="radio",id="ms3",name="moduleSelector",value='module_3'),
  #      icon('hospital-o'),'Geographic coverage analysis'
  #      ),
  #    tags$label(class="btn btn-default btn-inline",
  #      tags$input(type="radio",id="ms4",name="moduleSelector",value='module_4'),
  #      icon('table'),'Referral accessibility analysis'
  #      ),
  #    tags$label(class="btn btn-default btn-inline",
  #      tags$input(type="radio",id="ms5",name="moduleSelector",value='module_5'),
  #      icon('bar-chart'),'Zonal statistics'
  #      )
  #    # ,tags$label(class="btn btn-default btn-inline",
  #    #   tags$input(type="radio",id="ms6",name="moduleSelector",value='module_6'),
  #    #   icon('plus'),'Scalling up analysis'
  #    #   )
  #    )
  # mainPanel( width=12,
  # HTML(gsub('\n','',moduleInlineBtn))) 
  #   ),


  fluidRow(
    column(width=12,
      amAccordionGroup(id='toolsManage',show=c(1,2),itemList=list(
          'toolSelection'=list(
            title=div(icon('cubes'),'Select tool'),
            content=tagList(
              fluidRow(
                column(width=3,
                  selectizeInput('moduleSelector',"",choices=testList,options=testOption)
                  ),
                column(width=9,
                  h3('Tool description'),
                  p('Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer augue ex, suscipit non mi eu, iaculis efficitur mi. Nullam a dui velit. Sed eget pretium est. Cras magna ipsum, mollis eu egestas ut.')
                  )
                )
              )
            )
          )
        ),
      #amAccordionGroup(id='toolsUsage',show=c(1),itemList=list(
      #    'toolSelected'=list(
      #      title=div(icon('cube'),'Tool selected'),
      #      content=tagList(
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
  #   )
  # )
  # )
  #)
