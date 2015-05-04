
fluidRow(
  column(width=3,
    tourGroup(id="tour_manage_project",title="Manage project",
      amAccordionGroup(id='manageProject',show=c(1),itemList=list(
          'selectProject'=list(
            title=div(icon('play-circle'),'Open project'),
            content=selectInput("selectProject",label="Select a project",choices="")
            ),
          'addProject'=list(
            title=div(icon('plus-circle'),'New project'),
            content=tagList(
              textInput('txtNewProjectName','Enter a new available project name (min 4 characters)',value=''),
              tags$p(tags$b(id='hint-new-dem',icon('info-circle'),'Enter new name')),
              amFileInput('fileNewDem','Choose DEM')
              )
            )
          )
        )
      )
#    tourBtnTogglePanel(icon('info-circle'))
    ),
  amPanel(width=8,
    tagList(
      h3('Project summary'), 
      tags$h4('Location map'),
      plotOutput('locationMap'),
      tags$h5('Projection used (proj4string)'),
      uiOutput('infoProj4String'),
      fluidRow(width=12,
        column(width=4,
          h5('Grid parameters'),
          uiOutput('infoGrid')
          ),
        column(width=4,
          h5('Extent'),
          uiOutput('infoExtentProj')
          ),
        column(width=4,
          h5('Extent (lat/long)'),
          uiOutput('infoExtentLatLong')
          )
        )
      )
    )
  )

