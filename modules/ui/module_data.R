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
  sidebarPanel(width=3,
    tagList(
      h4(icon('plus-circle'),'Add new dataset'),
      amProgressBar("progNewData"),
      p('Projected map or table'),
      selectInput('dataClass','Select data class:',choices=""),
      textInput('dataTag','Add short tags',value=''),
      tags$p(tags$b(id='hint-new-data',icon('info-circle'),'Enter new name')),
      amFileInput('btnDataNew',label='Import dataset'),
      hr(),
      h4('Filter dataset'),
      radioButtons('typeChoice','Type of data',
        c("Vector" = "vector",
          "Raster"="raster",
          "Table"="table",
          "All"="all"),
        selected="all",
        inline=T
        ),
      textInput(inputId = 'filtData','Text filter',''),  
    #  addUIDep(
    #    selectizeInput("filtDataTag", 
    #      "Tags and class filter",
    #      choices="",
    #      multiple=TRUE, 
    #      options = list(plugins = list("drag_drop", "remove_button"))
    #      )
    #    ),
      hr(),
      h4('Archive'),
      amProgressBar('progArchive'),
      p('Archive selected data'),
      actionButton('createArchive','Create archive'),
      h4('Retrieve archive'),
      selectInput('selArchive','Select archive',choices=""),
      actionButton('getArchive','Export archive'),
      hr(),
      h4('Removing selection'),
      checkboxInput('showDelOption','Show removing option for selected dataset.'),
      conditionalPanel(
        condition = "input.showDelOption == true",
        list(
          hr(),
          actionButton('delDataSelect','Delete permanently'),
          hr()
          )
        )
      )
    ),
  amPanel(
    tagList(
      h3('Available datasets'),
#      dataTableOutput("dataTableSubset")
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


