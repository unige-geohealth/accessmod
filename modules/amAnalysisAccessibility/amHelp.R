



# help panels. NOTE: should be written in external file / db ?...

observeEvent(input$helpLinkSpeedTable,{
  content <- tagList(
    tags$p(ams(
      id = "help_accessibility_modify_travel_scenario_1",
      str = "You can edit the content of the \"travel scenario to be processed\" table (\"label\", \"speed\" and \"mode\" columns) by:",
      lang = language
      )),
    tags$ol(
      tags$li(
        ams(
          id = "help_accessibility_modify_travel_scenario_2",
          str = "importing the content from the selected scenario table;",
          lang = language
          )
        ),
      tags$li(
        ams(
          id = "help_accessibility_modify_travel_scenario_3",
          str = "Directlty editing the text in the table;",
          lang = language
          )
        ),
      tags$li(
        ams(
          id = "help_accessibility_modify_travel_scenario_4",
          str = "Copying and pasting text from an external spreadsheet.",
          lang = language
          )
        )
      ),
    tags$p(
      ams(
        id = "help_accessibility_resetting_table_content",
        str = "Clicking on \"reset\" will bring back the original content of the table.",
        lang = language
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
          id = "help_accessibility_stopping_values_notice",
          str = "The computation will stop as soon as one of those values are reached.",
          lang = language
          )
        ),
      tags$li(
        ams(
          id = "help_accessibility_zero_as_no_limit",
          str = "A value of zero will be considered as 'no limit'.",
          lang = language
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
          id = "help_accessibility_table_column_explanations",
          str = "The 'popTravelTime' column contains the population and the 'popCoveredPercent' column the percentage of the total population being covered in each zone for the selected maximum travel time.",
          lang = language
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
