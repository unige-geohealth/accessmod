fluidRow(
  amCenterTitle('Facilities selection',
    sub="Filter and select the facilities on which the analysis will be applied."),
  fluidRow(class="amRowTable",
    h4('Selected facilities'),
    conditionalPanel("!isNotEmpty(input.hfSelect)",
      tags$p("Please add facilities data")
      ),
    conditionalPanel("isNotEmpty(input.hfSelect)",
      checkboxInput('hfDisplayRules',
        'Display the panel for creating selection rules',
        value=FALSE
        ),
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
                  selectInput('hfFilterField',
                    label='Select field',
                    choices="",
                    selected=""
                    ),
                  selectInput('hfFilterOperator',
                    label='Operator',
                    choices="",
                    selected=""
                    ),
                  selectInput('hfFilterVal',
                    label='Select values',
                    choices="",
                    selected="",
                    multiple=T
                    ),
                  actionButton('btnAddHfRule',
                    'Add rule',
                    icon=icon('plus')
                    ),
                  actionButton('btnSelectHfFromRule',
                    'Apply rules',
                    icon=icon('check')
                    )
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
        conditionalPanel(
          condition="input.moduleSelector=='module_4'",
          tags$h3('From')
          ),
        tags$a(
          id='btnSelectAllHfFrom',
          onclick="hotableSetColValues('hfTable','amSelect',true)",
          icon('check-square-o'),
          'All'
          ),'|',
        tags$a(
          id='btnSelectNoHfFrom',
          onclick="hotableSetColValues('hfTable','amSelect',false)",
          icon('square-o'), 
          'None'
          ),'|',
        tags$a(
          id='btnSelectNoOnBarrierFrom',
          onclick="hotableSetColValuesByCond('hfTable','amSelect',false,'amOnBarrier','yes')",
          icon('flag'), 
          'Not on barrier'
          ),
        hotable('hfTable',height="500px"),
        #
        # Table of facilities (module 4 :  destination facilities / TO)
        #
        conditionalPanel(
          condition="input.moduleSelector=='module_4'",
          tags$h3('To'),
          tags$a(
            id='btnSelectAllHfTo',
            onclick="hotableSetColValues('hfTableTo','amSelect',true)",
            icon('check-square-o'), 
            'All'
            ),'|',
          tags$a(
            id='btnSelectNoHfTo',
            onclick="hotableSetColValues('hfTableTo','amSelect',false)",
            icon('flag'), 
            'None'
            ),'|',
        tags$a(
          id='btnSelectNoOnBarrierTo',
          onclick="hotableSetColValuesByCond('hfTableTo','amSelect',false,'amOnBarrier','yes')",
          icon('minus-circle'), 
          'Not on barrier'
          ),

          hotable('hfTableTo',height="500px")
          )
        ) 
      )
    )
  )
