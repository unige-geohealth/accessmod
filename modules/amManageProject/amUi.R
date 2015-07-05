
fluidRow(
  column(width=3,
    tourGroup(id="tour_manage_project",title="Manage project",
      amAccordionGroup(id='manageProject',show=c(1),itemList=list(
          'selectProject'=list(
            title=div(icon('play-circle'),'Open'),
            content=selectInput("selectProject",label="Select the project",choices="")
            ),
          'addProject'=list(
            title=div(icon('plus-circle'),'New'),
            content=tagList(
              textInput('txtNewProjectName','Enter a new available project name (min 4 characters)',value=''),
              tags$p(tags$b(id='hint-new-dem',icon('info-circle'),'Enter new name to unlock DEM upload.')),
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
      )
#    tourBtnTogglePanel(icon('info-circle'))
    ),
  amPanel(width=8,
    tagList(
      h3('Project summary'),

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
          ))


#      tags$h4('Location map'),
#      plotOutput('locationMap'),
#      tags$h4('Projection system'),
#      uiOutput('infoProj4String'),
#      fluidRow(width=12,
#        column(width=4,
#          h4('Grid parameters'),
#          uiOutput('infoGrid')
#          ),
#        column(width=4,
#          h4('Extent'),
#          uiOutput('infoExtentProj')
#          ),
#        column(width=4,
#          h4('Extent (lat/long)'),
#          uiOutput('infoExtentLatLong')
#          )
#        )
      )
    )
  )

