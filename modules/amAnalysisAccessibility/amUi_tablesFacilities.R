fluidRow(
  amCenterTitle(amt(
    id = "analysis_fac_sel",
    str = 'Facilities selection'
    ),
    sub = amt(
      id = "analysis_fac_sel_sub",
      str = "Filter and select the facilities on which the analysis will be applied."
      )),
  fluidRow(class = "amRowTable",
    h4(amt(
      id = "analysis_fac_selected",
      str = 'Selected facilities'
      )),
    conditionalPanel("!isNotEmpty(input.hfSelect)",
      tags$p(amt(
        id = "analysis_fac_sel_add",
        str = "Please add facilities data"
        ))
      ),
    conditionalPanel("isNotEmpty(input.hfSelect)",
      #
      #  Actions
      #
      div(class = "amTableMargin",  
        #
        # Table of facilities (module 4 : origine facilities / FROM )
        #
        conditionalPanel(
          condition = "input.moduleSelector=='module_4'",
          tags$h3(amt(
            id = "analysis_fac_sel_from",
            str = 'From'
            ))
          ),
        tags$div(
          class = "amTableControls",
          tags$a(
            id = 'btnSelectAllHfFrom',
            onclick = "hotableSetColValues('hfTable',{col:'amSelect',set:true})",
            ' [ All ]'
            ),' ',
          tags$a(
            id = 'btnSelectNoHfFrom',
            onclick = "hotableSetColValues('hfTable',{col:'amSelect',set:false})",
            ' [ None ]'
            ),' ',
          HTML("<div data-opt={\"col\":\"amSelect\",\"valueSet\":true,\"valueUnset\":false,\"labelSet\":\"Select\",\"labelUnset\":\"Unselect\"} id=\"hfTableSelectTools\"></div>")
          ),
        hotable('hfTable',
          height = "500px"
          ),
        #
        # Table of facilities (module 4 :  destination facilities / TO)
        #
        conditionalPanel(
          condition = "input.moduleSelector=='module_4'",
          tags$h3(amt(
            id = "analysis_fac_sel_to",
            str = 'To'
            )),
          tags$div(
            class = "amTableControls",
            tags$a(
              id = 'btnSelectAllHfTo',
              onclick = "hotableSetColValues('hfTableTo',{col:'amSelect',set:true})",
              ' [ All ]'
              ),' ',
            tags$a(
              id = 'btnSelectNoHfTo',
              onclick = "hotableSetColValues('hfTableTo',{col:'amSelect',set:false})",
              ' [ None]'
              ),' ',
            HTML("<div data-opt={\"col\":\"amSelect\",\"valueSet\":true,\"valueUnset\":false,\"labelSet\":\"Select\",\"labelUnset\":\"Unselect\"} id=\"hfTableToSelectTools\"></div>")
            ),
          hotable('hfTableTo',
            height = "500px"
            )
          )
        ) 
      )
    )
  )
