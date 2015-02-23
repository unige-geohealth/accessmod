

fluidRow(
  sidebarPanel(width=3,
    tagList(
      tags$h4(icon('play-circle'),'Project'), 
      selectInput("selectProject","Select a project",choices=""),
      tags$h4(icon('plus-circle'),'New Project'),
      textInput('txtNewProjectName','Enter a new available project name (min 4 characters)',value=''),
      tags$p(tags$b(id='hint-new-dem',icon('info-circle'),'Enter new name')),
      amFileInput('fileNewDem','Upload DEM')
      )
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

