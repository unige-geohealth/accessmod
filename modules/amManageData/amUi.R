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
          title=div(icon('plus-circle'),'Add new dataset'),
          content=tagList(
            p('Projected map or table'),
            selectInput('dataClass','Select data class:',choices=""),
            textInput('dataTag','Add short tags',value=''),
            uiOutput('msgModuleData'),
            #tags$p(tags$b(id='hint-new-data',icon('info-circle'),'Enter new name')),
            amFileInput('btnDataNew',label='Import dataset'),
            amProgressBar("progNewData")
            )
          ),
        'filtData'=list(
          title=div(icon('filter'),'Filter data'),
          content=tagList(
            radioButtons('typeDataChoice','Type of data',
              c("Vector" = "vector",
                "Raster"="raster",
                "Table"="table",
                "All"="all"),
              selected="all",
              inline=TRUE
              ),
            textInput(inputId = 'filtData','Text filter',''), 
            checkboxInput("internalDataChoice",'Show internal data',value=FALSE)
            )      
          ),
        'archiveData'=list(
          title=div(icon('compress'),'Archive and export'),
          content= tagList(
            p('Archive selected data'),
            actionButton('createArchive','Create archive'),
            h4('Retrieve archive'),
            selectInput('selArchive','Select archive',choices=""),
            actionButton('getArchive','Export archive'),
            p(' '),
            amProgressBar('progArchive')
            )
          ),
        'remData'=list(
          title=div(icon('trash-o'),'Delete selection'),
          content=tagList(
            checkboxInput('showDelOption','Show removing option for selected dataset.'),
            conditionalPanel(
              condition = "input.showDelOption == true",
              list(
                tags$b("WARNING: this can't be undone."),
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

#
#
## manage data panel
#formDataManage<-renderUI({
#  tagList(
#    formDataManageFilter,
#    hr(),
#    formDataManageArchive,
#    hr(),
#    formDataManageRemove
#    )
#
#})
#
#
#formDataManageFilter<-renderUI({
#  tagList(
#    h4('Filter dataset'),
#    radioButtons('typeChoice','Type of data',
#      c("Vector" = "vector",
#        "Raster"="raster",
#        "Table"="table",
#        "All"="all"),
#      selected="all",
#      inline=T
#      ),
#    textInput(inputId = 'filtData','Text filter',''),  
#    addUIDep(
#      selectizeInput("filtDataTag", 
#        "Tags and class filter",
#        choices="",
#        multiple=TRUE, 
#        options = list(plugins = list("drag_drop", "remove_button"))
#        )
#      )
#    )
#})
#
#formDataManageArchive<-renderUI({
#  tagList(
#    h4('Archive'),
#    p('Archive selected data'),
#    amProgressBar('progArchive'),
#    actionButton('createArchive','Create archive'),
#    h4('Retrieve archive'),
#    selectInput('selArchive','Select archive',choices=dataList$archive),
#    actionButton('getArchive','Export archive')
#    )
#})
#
#formDataManageRemove<-renderUI({
#tagList(
#    h4('Removing selection'),
#    checkboxInput('showDelOption','Show removing option for selected dataset.'),
#    conditionalPanel(
#      condition = "input.showDelOption == true",
#      list(
#        hr(),
#        actionButton('delDataSelect','Delete permanently'),
#        hr()
#        )
#      )
#  )
#})


