fluidRow(
  amCenterTitle('Facilities selection',
    sub="Filter and select the facilities on which the analysis will be applied."),
  fluidRow(class="amRowTable",
    h4('Selected facilities'),
    conditionalPanel("!isNotEmpty(input.hfSelect)",
      tags$p("Please add facilities data")
      ),
    conditionalPanel("isNotEmpty(input.hfSelect)",
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
        tags$div(
          class="amTableControls",
          tags$a(
            id='btnSelectAllHfFrom',
            onclick="hotableSetColValues('hfTable',{col:'amSelect',set:true})",
            ' [ All ]'
            ),' ',
          tags$a(
            id='btnSelectNoHfFrom',
            onclick="hotableSetColValues('hfTable',{col:'amSelect',set:false})",
            ' [ None ]'
            ),' ',
          HTML("<div data-opt={\"col\":\"amSelect\",\"valueSet\":true,\"valueUnset\":false,\"labelSet\":\"Add\",\"labelUnset\":\"Remove\"} id=\"hfTableSelectTools\"></div>")
          ),
        hotable('hfTable',height="500px"),
        #
        # Table of facilities (module 4 :  destination facilities / TO)
        #
        conditionalPanel(
          condition="input.moduleSelector=='module_4'",
          tags$h3('To'),
          tags$div(
            class="amTableControls",
            tags$a(
              id='btnSelectAllHfTo',
              onclick="hotableSetColValues('hfTableTo',{col:'amSelect',set:true})",
              ' [ All ]'
              ),' ',
            tags$a(
              id='btnSelectNoHfTo',
              onclick="hotableSetColValues('hfTableTo',{col:'amSelect',set:false})",
              ' [ None]'
              ),' ',
            HTML("<div data-opt={\"col\":\"amSelect\",\"valueSet\":true,\"valueUnset\":false,\"labelSet\":\"Add\",\"labelUnset\":\"Remove\"} id=\"hfTableToSelectTools\"></div>")
            ),
          hotable('hfTableTo',height="500px")
          )
        ) 
      )
    )
  )
