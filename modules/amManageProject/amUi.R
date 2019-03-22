fluidRow(
  div(class = "col-xs-12 col-md-4",
    amAccordionGroup(
      id = 'manageProject',
      show = c(1),
      itemList = list(
        'selectProject' = list(
          title = div(
            icon('play-circle'
              ),
            amt(
              id = 'project_open_title',
              str = 'Open'
              )
            ),
          content = selectInput("selectProject",
            label = amt(
              id = "project_open_select",
              str = "Select the project"
              ),
            choices = NULL
            )
          ),
        'addProject' = list(
          title = div(
            icon('plus-circle'
              ),
            amt(
              id = "project_new_title",
              str = "New"
              )
            ),
          content = tagList(
            textInput('txtNewProjectName',
              label = amt(
                id = 'project_new_name_text',
                str = 'Enter a new available project name (min 4 characters)'
                ),
              value = ''
              ),
            tags$p(
              tags$b(
                id = 'hint-new-dem', 
                icon('info-circle'
                  ),
                amt(
                  id = 'project_new_name_unlock_dem',
                  str = 'Enter a new project name to unlock the upload of the DEM.'
                  )
                )
              ),
            amFileInput('fileNewDem',
              amt(
                id = "project_new_choose_dem_btn",
                str = 'Choose DEM'
                )
              )
            )
          ),
        'rmProject' = list(
          title = div(
            icon('trash-o'),
            amt(
              id = "project_delete_title",
              str = 'Delete'
              )
            ),
          content = tagList(
            selectInput('selectProjectToDelete',
              amt(
                id = "project_delete_select_project",
                str = "Select the project to delete",
                ),
              choice = NULL
              ),
            actionButton('btnDelProject',
              amt(
                id = "project_delete_btn",
                str = "Delete"
                )
              )
            )
          ),
        'selectLanguage' = list(
          title = div(
            icon('language '),
            amt(
              id = 'project_language_title',
              str = 'Language'
              )
            ),
          content = selectInput("selectLanguage",
            label = amt(
              id = "project_language_select",
              str = "Select the language"
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
        id = "project_summary_title",
        str = 'Project summary'
        ),
      sub = amt(
        id = "project_summary_sub",
        str = "Spatial summary of the current project."
        )
      ),
    amAccordionGroup(
      id = "projectSummary",
      show = c(1),
      itemList = list(
        'locationMap' = list(
          title = amt(
            id = 'project_location_map_title',
            str = 'Location map'
            ),
          content = plotOutput('locationMap'
          )),
        'projectionSystem' = list(
          title = amt(
            id = "project_projection_system_title",
            str = "Projection system"
            ),
          content = uiOutput('infoProj4String'
          )),
        'gridParameter' = list(
          title = amt(
            id = "project_grid_parameter_title",
            str = "Grid parameters"
            ),
          content = uiOutput('infoGrid'
          )),
        'gridExtent' = list(
          title = amt(
            id = "project_extent_metric_title",
            str = 'Extent (metric)'
            ),
          content = uiOutput('infoExtentProj'
          )),
        'gridExtLatLong' = list(
          title = amt(
            id = "project_extent_degrees_title",
            str = 'Extent (decimal degrees)'
            ),
          content = uiOutput('infoExtentLatLong'
          ))
        )
      )
    )
  )

