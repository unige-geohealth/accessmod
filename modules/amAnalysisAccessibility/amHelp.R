



# help panels. NOTE: should be written in external file / db ?...

observeEvent(input$helpLinkSpeedTable,{
  content <- tagList(
    tags$p(ams(
      id = "help_accessibility_modify_travel_scenario_1"
      )),
    tags$ol(
      tags$li(
        ams(
          id = "help_accessibility_modify_travel_scenario_2"
          )
        ),
      tags$li(
        ams(
          id = "help_accessibility_modify_travel_scenario_3"
          )
        ),
      tags$li(
        ams(
          id = "help_accessibility_modify_travel_scenario_4"
          )
        )
      ),
    tags$p(
      ams(
        id = "help_accessibility_resetting_table_content"
        )
      )
    )


  amUpdateModal(
    panelId = 'amHelpPanel',
    title = config$helpTitle,
    html = content
    )
  })



observeEvent(input$helpLinkComputeLimit,{
  content <- tagList(
    tags$ul(
      tags$li(
        ams(
          id = "help_accessibility_stopping_values_notice"
          )
        ),
      tags$li(
        ams(
          id = "help_accessibility_zero_as_no_limit"
          )
        )
      )
    )

  amUpdateModal(
    panelId = 'amHelpPanel',
    title = config$helpTitle,
    html = content
    )
  })

observeEvent(input$helpLinkZoneCoverageTable,{
  content <-tagList(
    tags$ul(
      tags$li(
        ams(
          id = "help_accessibility_table_column_explanations"
          )
        )
      )
    )
  amUpdateModal(
    panelId = 'amHelpPanel',
    title = config$helpTitle,
    html = content
    ) 
  })
