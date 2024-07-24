raster_summary_to_ui <- function(out) {
  hasResolutionIssue <- isTRUE(
    round(out$projectBefore$resolution$x) !=
      round(out$data$resolution$x) ||
      round(out$projectBefore$resolution$y) !=
        round(out$data$resolution$y)
  )
  ui <- tags$div(
    class = "panel panel-default",
    tags$div(
      class = "panel-heading",
      tags$h3(class = "panel-title", ams("srv_data_raster_summary"))
    ),
    tags$div(
      class = "panel-body",
      # Resolution information
      tags$h4(ams("srv_data_resolution")),
      tags$ul(
        class = "list-group",
        tags$li(
          class = "list-group-item",
          tags$span(
            class = "badge",
            paste(
              round(out$projectBefore$resolution$x, 4), "x",
              round(out$projectBefore$resolution$y, 4)
            )
          ),
          ams("srv_data_resol_before_importation")
        ),
        tags$li(
          class = "list-group-item",
          tags$span(
            class = "badge",
            paste(
              round(out$projectAfter$resolution$x, 4), "x",
              round(out$projectAfter$resolution$y, 4)
            )
          ),
          ams("srv_data_resol_after_importation")
        ),
        tags$li(
          class = paste("list-group-item", ifelse(hasResolutionIssue, "list-group-item-danger", "")),
          tags$span(
            class = "badge",
            paste(
              round(out$data$resolution$x, 4), "x",
              round(out$data$resolution$y, 4)
            )
          ),
          ams("srv_data_resol_imported_dataset")
        )
      ),

      # Projection information using details/summary
      tags$h4(ams("srv_data_projection")),
      tags$details(
        tags$summary(ams("srv_data_project_before_importation")),
        tags$pre(
          class = "small", style = "white-space: pre-wrap; word-break: break-word;",
          out$projectBefore$projection
        )
      ),
      tags$details(
        tags$summary(ams("srv_data_project_after_importation")),
        tags$pre(
          class = "small", style = "white-space: pre-wrap; word-break: break-word;",
          out$projectAfter$projection
        )
      ),
      tags$details(
        tags$summary(ams("srv_data_project_imported_dataset")),
        tags$pre(
          class = "small", style = "white-space: pre-wrap; word-break: break-word;",
          out$data$projection
        )
      ),

      # Warning for resolution issues
      if (hasResolutionIssue) {
        tags$div(
          class = "alert alert-warning",
          icon("warning"),
          tags$strong(ams("srv_data_has_resolution_issue_warning"))
        )
      },

      # Additional information
      tags$hr(),
      tags$p(
        tags$strong(ams("srv_data_null_values")), ": ",
        ams("srv_data_number_null_cells_found"),
        tags$span(class = "badge", out$data$numberOfNulls)
      ),
      tags$p(
        class = "text-muted",
        tags$strong(ams("srv_data_note_notice")), " ",
        ams("srv_data_table_control_after_importation_notice")
      )
    )
  )
}
