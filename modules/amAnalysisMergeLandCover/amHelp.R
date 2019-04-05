# help panels. NOTE: should be written in external file / db ?... js based help ?

observeEvent(input$helpLinkLcvTable,{
  content <- tagList(
    tags$p(
      ams(
        id = "help_merge_lc_labels_edition_1"
        )
      ),
    tags$ol(
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_2"
          ),
        icon("magic"),
        ";"
        ),
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_3"
          )
        ),
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_4"
          )
        )
      ),
    tags$p(
      ams(
        id = "help_merge_lc_resetting_table_content"
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
          id = "help_merge_lc_column_preview"
          )
        ),
    tags$p(
      ams(
        id = "help_merge_lc_empty_labels_ban"
        )
      ),
    tags$p(
      ams(
        id = "help_merge_lc_different_classes_requirement"
        )
      )
    )
  amUpdateModal(
    panelId = 'amHelpPanel',
    title = config$helpTitle,
    html = content
    )
  })




