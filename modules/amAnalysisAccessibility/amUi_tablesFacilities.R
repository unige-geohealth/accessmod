fluidRow(
  amCenterTitle('Facilities selection',sub="Filter and select the facilities on which the analysis will be applied."),
  h4('Selected facilities'),
  checkboxInput('hfDisplayRules','Display the panel for creating selection rules',value=FALSE),
  #
  # Rule Panel
  #
  fluidRow(
    column(width=12,
      # NOTE: hfDisplayRules is used in amServer to trigger hfFilter* fields.
      conditionalPanel(condition="input.hfDisplayRules == true",
        tagList(
          sidebarPanel(width=5,
            tagList(
              h4('Add new rules'),
              div("data-display-if"="input.moduleSelector=='module_4'",
                style="display:inline",
                div(id="selHfFromTo",
                  class="form-group shiny-input-radiogroup shiny-input-container",
                  style="display:inline",
                  div(class="shiny-options-group",style="display:inline",
                    tags$input(
                      type="radio",
                      style="margin:4px",
                      name="selHfFromTo",
                      value="From",
                      checked="checked","From"
                      ),
                    tags$input(
                      type="radio",
                      style="margin:4px",
                      name="selHfFromTo",
                      value="To",
                      checked="checked","To"
                      )
                    ) 
                  )
                ),
              selectInput('hfFilterField','Select field',choices="",selected=""),
              selectInput('hfFilterOperator','Operator',choices="",selected=""),
              selectInput('hfFilterVal','Select values',choices="",selected="",multiple=T),
                actionButton('btnAddHfRule','Add rule',icon=icon('plus')),
                actionButton('btnSelectHfFromRule','Apply rules',icon=icon('check'))
              )
            ),
          column(width=7,
            h4('Rules to apply'),
            hotable('hfTableRules')
            )
          )
        )
      )
    ),
  #
  #  Actions
  #
  div(class="amTableMargin",  
    #
    # Table of facilities (module 4 : origine facilities / FROM )
    #
    conditionalPanel(condition="input.moduleSelector=='module_4'",
        tags$h3('From')
      ),
    actionLink('btnSelectAllHfFrom','All',icon=icon('check-square-o'), 
      onclick="hotableSetColValues('hfTable','amSelect',true)"
      ),'|',
    actionLink('btnSelectNoHfFrom','None',icon=icon('square-o'), 
      onclick="hotableSetColValues('hfTable','amSelect',false)"
      ),
    hotable('hfTable',height="300px"),
    #
    # Table of facilities (module 4 :  destination facilities / TO)
    #
    conditionalPanel(condition="input.moduleSelector=='module_4'",
      tags$h3('To'),
      actionLink('btnSelectAllHfTo','All',icon=icon('check-square-o'), 
        onclick="hotableSetColValues('hfTableTo','amSelect',true)"
        ),'|',
      actionLink('btnSelectNoHfTo','None',icon=icon('square-o'), 
        onclick="hotableSetColValues('hfTableTo','amSelect',false)"
        ),
      hotable('hfTableTo',height="300px")
      )
    ) 
  )

