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
              selectInput('hfFilterField','Select field',choices="",selected=""),
              selectInput('hfFilterOperator','Operator',choices="",selected=""),
              selectInput('hfFilterVal','Select values',choices="",selected="",multiple=T),
              actionButton('btnAddHfRule','',icon=icon('plus'))
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
    div("data-display-if"="input.moduleSelector=='module_4'",style="display:inline",
      div(id="selHfFromTo",class="form-group shiny-input-radiogroup shiny-input-container",style="display:inline",
        " Apply selection to table ",
        div(class="shiny-options-group",style="display:inline",
          tags$input(type="radio",style="margin:4px",name="selHfFromTo",value="From",checked="checked","From"),
          tags$input(type="radio",style="margin:4px",name="selHfFromTo",value="To",checked="checked","To")
          ) 
        )," | "
      ),

    actionLink('btnSelectAllHf','All',icon=icon('check-square-o')),'|',
    actionLink('btnSelecteNoHf','None',icon=icon('square-o')),'|',
    actionLink('btnSelectHfFromRule','Apply rules',icon=icon('list-ol')),
    div("data-display-if"="input.showDevelTools==true",style="display:inline",
      '|',actionLink('btnSelectRandomHf','random (10%)')
      ),
    #
    # Facilities table
    #
    conditionalPanel(condition="input.moduleSelector=='module_4'",
      tagList(
        tags$b('From')
        )
      ),
    hotable('hfTable',height="300px"),
    conditionalPanel(condition="input.moduleSelector=='module_4'",
      tags$b('To'),
      hotable('hfTableTo',height="300px")
      )
    ) 
  )

