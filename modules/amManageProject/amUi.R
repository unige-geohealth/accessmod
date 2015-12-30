
fluidRow(
  div(class="col-xs-12 col-md-4",
    #tourGroup(id="tour_manage_project",title="Manage project",
      amAccordionGroup(id='manageProject',show=c(1),itemList=list(
          'selectProject'=list(
            title=div(icon('play-circle'),'Open'),
            content=selectInput("selectProject",label="Select the project",choices="")
            ),
          'addProject'=list(
            title=div(icon('plus-circle'),'New'),
            content=tagList(
              textInput('txtNewProjectName','Enter a new available project name (min 4 characters)',value=''),
              tags$p(tags$b(id='hint-new-dem',icon('info-circle'),'Enter a new project name to unlock the upload of the DEM.')),
              amFileInput('fileNewDem','Choose DEM')
              )
            ),
          'rmProject'=list(
            title=div(icon('trash-o'),'Delete'),
            content=tagList(
              selectInput('selectProjectToDelete',"Select the project to delete",choice=""),
              actionButton('btnDelProject',"Delete")
              )
            )
          )
        )
     # )
    ),
  div(class="col-xs-12 col-md-8",
    amCenterTitle('Project summary',sub="Spatial summary of the current project."),
    amAccordionGroup(id="projectSummary",show=c(1),itemList=list(
        'locationMap'=list(
          title=div('Location map'),
          content=plotOutput('locationMap')
          ),
        'projectionSystem'=list(
          title='Projection system',
          content=uiOutput('infoProj4String')
          ),
        'gridParameter'=list(
          title='Grid parameters',
          content=uiOutput('infoGrid')
          ),
        'gridExtent'=list(
          title='Extent (metric)',
          content=uiOutput('infoExtentProj')
          ),
        'gridExtLatLong'=list(
          title='Extent (decimal degrees)',
          content=uiOutput('infoExtentLatLong')
          )
        )
      )
    )
  )

