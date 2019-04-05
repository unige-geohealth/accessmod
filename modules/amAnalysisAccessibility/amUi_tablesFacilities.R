fluidRow(
  amCenterTitle(amt(
      id = "analysis_facility_selection_title"
      ),
    sub = amt(
      id = "analysis_facility_selection_sub"
      )),
  fluidRow(class = "amRowTable",
    h4(amt(
        id = "analysis_facility_selected_facilities"
        )),
    conditionalPanel("!isNotEmpty(input.hfSelect)",
      tags$p(
        class = "callout callout-info",
        amt(
          id = "analysis_facility_selection_add_data"
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
              id = "analysis_facility_selection_from"
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
          conditionalPanel("!isNotEmpty(input.hfSelectTo)",
            tags$p(
              class = "callout callout-info",
              amt(
                id = "analysis_facility_selection_add_data"
                ))
            ),
          conditionalPanel("isNotEmpty(input.hfSelect)",
            tags$h3(amt(
                id = "analysis_facility_selection_to"
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
  )
