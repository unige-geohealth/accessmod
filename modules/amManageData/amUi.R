#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module manage_data 
#
# USER INTERFACE

fluidRow(
  column(width=3,
    amAccordionGroup(id='manageData',itemList=list(
        'addData'=list(
          title=div(icon('plus-circle'),'Import'),
          content=tagList(
            selectInput('dataClass','Select data class:',choices=""),
            textInput('dataTag','Add short tags',value=''),
            uiOutput('msgModuleData'),
            amFileInput('btnDataNew',label='Choose dataset'),
            amProgressBar("progNewData")
            )
          ),
        'filtData'=list(
          title=div(icon('filter'),'Filter'),
          content=tagList(
            radioButtons('typeDataChoice','Data type',
              c("Vector" = "vector",
                "Raster" = "raster",
                "Table"  = "table",
                "All"    = "all"),
              selected   = "all",
              inline=TRUE
              ),
            textInput(inputId = 'filtData','Text (any field, case sensitive)',''), 
            selectInput(inputId = 'filtDataTags','Tags filter',choices='',selected='',multiple=T), 
            checkboxInput("internalDataChoice",'Show internal data',value=FALSE)
            )      
          ),
        'renameData'=list(
          title=div(icon('refresh'),'Rename'),
          content=tagList(
            #h4('Rename changed tags'),
            p('Manually modify the tag(s) in the adjacent table and click on the button to implement the change (does not apply on the DEM)'),
            actionButton('btnUpdateName','Update modified tag(s)')
            )
          ),
        'archiveData'=list(
          title=div(icon('compress'),'Archive'),
          content= tagList(
            p('Click to archive (.zip) the data appearing as selected in the right table'),
            actionButton('createArchive','Create archive'),
            p(' '),
            amProgressBar('progArchive')
            )
          ),
        'exportData'=list(
          title=div(icon('download'),'Export'),
          content=tagList(
            #h4('Export selected archive'),
            selectInput('selArchive','Select archive',choices=""),
            actionButton('getArchive','Export archive')
            )
          ),
        'remData'=list(
          title=div(icon('trash-o'),'Delete'),
          content=tagList(
            p('This action will delete the selected data'),
            #p('Click to delete the data appearing as selected in the right table'),
            checkboxInput('showDelOption',"I understand that this action can't be undone."),
            conditionalPanel(
              condition = "input.showDelOption == true",
              list(
                tags$b("WARNING: please check selected dataset prior to deleting."),
                actionButton('delDataSelect','Delete permanently')
                )
              )
            )
          )
        )) 
    ),
  amPanel(
    tagList(
      h3('Available data'),
      hotable('dataListTable')
      )
    )
  )


