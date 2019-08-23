fluidRow(
  div(class = "col-xs-12 col-md-4",
    amAccordionGroup("manageProject",
      show = c(1),
      itemList = list(
        "selectProject" = list(
          title = div(
            icon("play-circle"),
            amt("project_open_title")
            ),
          content = selectInput("selectProject",
            label = amt("project_open_select"),
            choices = NULL
            )
          ),
        "addProject" = list(
          title = div(
            icon("plus-circle"),
            amt("project_new_import_title")
            ),
          content = tagList(
            textInput("txtNewProjectName",
              label = amt("project_new_name_text"),
              value = ""
              ),
            tags$p(
              tags$b(
                id = "txtHintNewProject", 
                icon("info-circle"),
                amt("project_new_name_unlock_dem")
                )
              ),
            amFileInput("fileNewDem",
              amt("project_new_choose_dem_btn")
             ),
           amFileInput('btnProjectImport',
              label = amt("project_import_btn"),
              fileAccept = c(config$fileArchiveProjectDb),
              multiple = FALSE
              )
            )
          ),
        "rmProject" = list(
          title = div(
            icon("trash-o"),
            amt("project_delete_title")
            ),
          content = tagList(
            selectInput("selectProjectToDelete",
              amt("project_delete_select_project"),
              choice = NULL
              ),
            actionButton("btnDelProject",
              amt("project_delete_btn")
              )
            )
          ),
        "exportProject" = list(
          title = div(
            icon("external-link"),
            amt("project_export_title")
            ),
          content = tagList(
            actionButton("btnProjectExport",
              amt("project_export_btn")
              )
            )
          ),
        "selectLanguage" = list(
          title = div(
            icon("language "),
            amt("project_language_title")
            ),
          content = selectInput("selectLanguage",
            label = amt("project_language_select"),
            selected = config$language,
            choices = config$dictLanguages
            )
          )
        )
      )
    ),
  div(class = "col-xs-12 col-md-8",
    amCenterTitle(
      title = amt("project_summary_title"),
      sub = amt("project_summary_sub")
      ),
    amAccordionGroup("projectSummary",
      show = c(1),
      itemList = list(
        "locationMap" = list(
          title = amt("project_location_map_title"),
          content = plotOutput("locationMap")
          ),
        "projectionSystem" = list(
          title = amt("project_projection_system_title"),
          content = uiOutput("infoProj4String")
          ),
        "gridParameter" = list(
          title = amt("project_grid_parameter_title"),
          content = uiOutput("infoGrid")
          ),
        "gridExtent" = list(
          title = amt("project_extent_metric_title"),
          content = uiOutput("infoExtentProj")
          ),
        "gridExtLatLong" = list(
          title = amt("project_extent_degrees_title"),
          content = uiOutput("infoExtentLatLong")
          )
        )
      )
    )
  )

