fluidRow(
  div(class = "col-xs-12 col-md-4",
    amAccordionGroup(
      id = "manageProject",
      show = c(1),
      itemList = list(
        "selectProject" = list(
          title = div(
            icon("play-circle"
              ),
            amt(
              id = "project_open_title"
              )
            ),
          content = selectInput("selectProject",
            label = amt(
              id = "project_open_select"
              ),
            choices = NULL
            )
          ),
        "addProject" = list(
          title = div(
            icon("plus-circle"
              ),
            amt(
              id = "project_new_title"
              )
            ),
          content = tagList(
            textInput("txtNewProjectName",
              label = amt(
                id = "project_new_name_text"
                ),
              value = ""
              ),
            tags$p(
              tags$b(
                id = "hint-new-dem", 
                icon("info-circle"
                  ),
                amt(
                  id = "project_new_name_unlock_dem"
                  )
                )
              ),
            amFileInput("fileNewDem",
              amt(
                id = "project_new_choose_dem_btn"
                )
              )
            )
          ),
        "rmProject" = list(
          title = div(
            icon("trash-o"),
            amt(
              id = "project_delete_title"
              )
            ),
          content = tagList(
            selectInput("selectProjectToDelete",
              amt(
                id = "project_delete_select_project"
                ),
              choice = NULL
              ),
            actionButton("btnDelProject",
              amt(
                id = "project_delete_btn"
                )
              )
            )
          ),
        "selectLanguage" = list(
          title = div(
            icon("language "),
            amt(
              id = "project_language_title"
              )
            ),
          content = selectInput("selectLanguage",
            label = amt(
              id = "project_language_select"
              ),
            selected = config$langUser,
            choices = config$dictLanguages
            )
          )
        )
      )
    ),
  div(class = "col-xs-12 col-md-8",
    amCenterTitle(
      title = amt(
        id = "project_summary_title"
        ),
      sub = amt(
        id = "project_summary_sub"
        )
      ),
    amAccordionGroup(
      id = "projectSummary",
      show = c(1),
      itemList = list(
        "locationMap" = list(
          title = amt(
            id = "project_location_map_title"
            ),
          content = plotOutput("locationMap"
          )),
        "projectionSystem" = list(
          title = amt(
            id = "project_projection_system_title"
            ),
          content = uiOutput("infoProj4String"
          )),
        "gridParameter" = list(
          title = amt(
            id = "project_grid_parameter_title"
            ),
          content = uiOutput("infoGrid"
          )),
        "gridExtent" = list(
          title = amt(
            id = "project_extent_metric_title"
            ),
          content = uiOutput("infoExtentProj"
          )),
        "gridExtLatLong" = list(
          title = amt(
            id = "project_extent_degrees_title"
            ),
          content = uiOutput("infoExtentLatLong"
          ))
        )
      )
    )
  )

