



# help panels. NOTE: should be written in external file / db ?...

observeEvent(input$helpLinkSpeedTable,{
  content <- tagList(
    tags$p("You can edit the content of the \"travel scenario to be processed\" table (\"label\", \"speed\" and \"mode\" columns) by:"),
    tags$ol(
      tags$li("importing the content from the selected scenario table;"),
      tags$li("Directlty editing the text in the table;"),
      tags$li("Copying and pasting text from an external spreadsheet.")
      ),
    tags$p("Clicking on \"reset\" will bring back the original content of the table.")
    )


  amUpdateModal(panelId='amHelpPanel',title=config$helpTitle,html=content)

})



observeEvent(input$helpLinkComputeLimit,{
  content <- tagList(
    tags$ul(
      tags$li("The computation will stop as soon as one of those values are reached."),
      tags$li("A value of zero will be considered as 'no limit'.")
    ))

  amUpdateModal(panelId='amHelpPanel',title=config$helpTitle,html=content)

})

observeEvent(input$helpLinkZoneCoverageTable,{
  content <-tagList(
    tags$ul(
      tags$li("The 'popTravelTime' column contains the population and the 'popCoveredPercent' column  the percentage of  the total population being covered in each zone for the selected maximum travel time. ")
      )
    )
  amUpdateModal(panelId='amHelpPanel',title=config$helpTitle,html=content) 
})
