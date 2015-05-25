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
            p('Projected map or table'),
            selectInput('dataClass','Select data class:',choices=""),
            textInput('dataTag','Add short tags',value=''),
            uiOutput('msgModuleData'),
            #tags$p(tags$b(id='hint-new-data',icon('info-circle'),'Enter new name')),
            amFileInput('btnDataNew',label='Choose dataset'),
            amProgressBar("progNewData")
            )
          ),
        'filtData'=list(
          title=div(icon('filter'),'Filter'),
          content=tagList(
            radioButtons('typeDataChoice','Type of data',
              c("Vector" = "vector",
                "Raster" = "raster",
                "Table"  = "table",
                "All"    = "all"),
              selected   = "all",
              inline=TRUE
              ),
            textInput(inputId = 'filtData','Text filter',''), 
            selectInput(inputId = 'filtDataTags','Tags filter',choices='',selected='',multiple=T), 
            checkboxInput("internalDataChoice",'Show internal data',value=FALSE)
            )      
          ),
        'archiveData'=list(
          title=div(icon('compress'),'Archive'),
          content= tagList(
            h4('Archive selected data'),
            actionButton('createArchive','Create archive'),
            p(' '),
            amProgressBar('progArchive')
            )
          ),
        'exportData'=list(
          title=div(icon('download'),'Export'),
          content=tagList(
            h4('Export selected archive'),
            selectInput('selArchive','Select archive',choices=""),
            actionButton('getArchive','Export archive')
            )
          ),
        'remData'=list(
          title=div(icon('trash-o'),'Remove'),
          content=tagList(
            h4('Remove dataset'),
            checkboxInput('showDelOption',"I understand that these actions can't be undone.'."),
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
      h3('Available datasets'),
      hotable('dataListTable')
      )
    )
  )


