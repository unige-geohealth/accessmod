# help panels. NOTE: should be written in external file / db ?... js based help ?

observeEvent(input$helpLinkLcvTable,{
  content <- tagList(
    tags$p("You can edit the labels below by:"),
    tags$ol(
      tags$li("Importing the content from the selected land cover table",icon("magic"),";"),
      tags$li("Directly editing the text;"),
      tags$li("Copying and pasting text from an external spreadsheet.")
      ),
    tags$p("Clicking on \"reset\" will bring the table back to the original labels in the landcover layer.")
    )


  amUpdateModal(panelId='amHelpPanel',title=config$helpTitle,html=content)

})


observeEvent(input$helpLinkRoadTable,{
  content <- tagList(
    tags$p("Preview for the selected column (max. 50 rows displayed)."),
    tags$p("Classe \"0\" and/or empty labels are not allowed."),
    tags$p("The classes in the road network layer have to be different than those used in the land cover layer. Any conflicts have to be corrected before running the module")
    )
  amUpdateModal(panelId='amHelpPanel',title=config$helpTitle,html=content)
})




