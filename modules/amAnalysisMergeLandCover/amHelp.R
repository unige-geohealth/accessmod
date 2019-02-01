# help panels. NOTE: should be written in external file / db ?... js based help ?

observeEvent(input$helpLinkLcvTable,{
  content <- tagList(
    tags$p(
      ams(
        id = "help_merge_lc_labels_edition_1",
        str = "You can edit the labels below by:",
        lang = language
        )
      ),
    tags$ol(
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_2",
          str = "Importing the content from the selected land cover table",
          lang = language
          ),
        icon("magic"),
        ";"
        ),
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_3",
          str = "Directly editing the text;",
          lang = language
          )
        ),
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_4",
          str = "Copying and pasting text from an external spreadsheet.",
          lang = language
          )
        )
      ),
    tags$p(
      ams(
        id = "help_merge_lc_resetting_table_content",
        str = "Clicking on \"reset\" will bring the table back to the original labels in the landcover layer.",
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


observeEvent(input$helpLinkRoadTable,{
  content <- tagList(
    tags$p(
      ams(
          id = "help_merge_lc_column_preview",
          str = "Preview for the selected column (max. 50 rows displayed).",
          lang = language
          )
        ),
    tags$p(
      ams(
        id = "help_merge_lc_empty_labels_ban",
        str = "Empty labels are not allowed.",
        lang = language
        )
      ),
    tags$p(
      ams(
        id = "help_merge_lc_different_classes_requirement",
        str = "The classes in the road network layer have to be different than those used in the land cover layer. Any road classes presenting a value below 1000 will be automatically modified by AccessMod to help solve this issue. Any remaining conflicts after this modification will have to be fixed before running the module.",
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




