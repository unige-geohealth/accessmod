fluidRow(
  div(class = "col-xs-12 col-md-4",
    amAccordionGroup(
      id = 'manageProject',
      show = c(1),
      itemList = list(
        'selectLanguage' = list(
          title = div(
            icon('language '),
            amt(
              id = 'project_language',
              str = 'Language'
              )
            ),
          content = selectInput("selectLanguage",
            label = amt(
              id = "project_select_language",
              str = "Select the language"
              ),
            selected = config$langUser,
            choices = config$dictLanguages
            )
          ),
        'selectProject' = list(
          title = div(
            icon('play-circle'
              ),
            amt(
              id = 'project_form_open',
              str = 'Open'
              )
            ),
          content = selectInput("selectProject",
            label = amt(
              id = "project_select",
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
              id = "project_form_new",
              str = "New"
              )
            ),
          content = tagList(
            textInput('txtNewProjectName',
              label = amt(
                id = 'project_text_name_new',
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
                  id = 'project_text_name',
                  str = 'Enter a new project name to unlock the upload of the DEM.'
                  )
                )
              ),
            amFileInput('fileNewDem',
              amt(
                id = "project_choose_dem",
                str = 'Choose DEM'
                )
              )
            )
          ),
        'rmProject' = list(
          title = div(
            icon('trash-o'),
            amt(
              id = "project_delete",
              str = 'Delete'
              )
            ),
          content = tagList(
            selectInput('selectProjectToDelete',
              amt(
                id = "project_select_delete",
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
            id = 'project_location_map',
            str = 'Location map'
            ),
          content = plotOutput('locationMap'
          )),
        'projectionSystem' = list(
          title = amt(
            id = "project_projection_system",
            str = "Projection system"
            ),
          content = uiOutput('infoProj4String'
          )),
        'gridParameter' = list(
          title = amt(
            id = "project_grid_parameter",
            str = "Grid parameters"
            ),
          content = uiOutput('infoGrid'
          )),
        'gridExtent' = list(
          title = amt(
            id = "project_extent_metric",
            str = 'Extent (metric)'
            ),
          content = uiOutput('infoExtentProj'
          )),
        'gridExtLatLong' = list(
          title = amt(
            id = "project_extent_degrees",
            str = 'Extent (decimal degrees)'
            ),
          content = uiOutput('infoExtentLatLong'
          ))
        )
      )
    )
  )

