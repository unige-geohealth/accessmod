#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module manage_data :
# -upload new data
# -browse exisiting dat
# -preview data (not yet) 
# -delete data

################################################################################
#
#  UI
#  Main renderer for UI, group child renderUI
#
################################################################################
output$modManageData<-renderUI({
  if(!is.null(listen$gisLock)){
    sidebarLayout(
      sidebarPanel(
        busyIndicator("Calculation In progress",wait = 1000),
        formDataSet, # set new data and tag
        formDataUpload, # generate name and upload logic
        hr(),
        formDataManage, # manage existing dataset
        width=3),
      mainPanel(
        tableDataset
        )
      )
  }else{
    panel('warning','No project selected',p(msgNoLocation))
  }
})

################################################################################
#
# New data form
# - new dataset class selector
# - new dataset  hint
# - new dataset upload
#
# Notes : New data upload use a reactive list to hold meta data: 
#         dataMetaList, with objects named :
#         - type : rast,vect,table
#         - name : prefix + sepTagPrefix + tags
#         - tags : string of tags sep by sepTagFile
#         - class : class of data (land_cover, table_model, etc..)
#         - ready : dataset is ready to be created.
#
################################################################################




# form select the class of new data, based on dataClass.
formDataSet<-renderUI({
  dataClassChoices<-dataClass[dataClass$allowNew==TRUE,'class']
  tagList(
    h4('Add new dataset'),
    p('Projected map or table'),
    selectInput('dataClass','Select data class:',
      choices=dataClassChoices,
      selected=dataClassChoices[1],
      selectize=F
      ),
    textInput('dataTag','Add short tags',value=''),
    uiOutput('hintNewData')
    )
})

# init empty metadata
dataMetaList<-reactiveValues(
  type=NULL,
  name=NULL,
  class=NULL,
  tags=NULL,
  ready=NULL,
  accepts=NULL
  )

# observer to perform a quick validation of user tag input
observe({  
  dataTag<-input$dataTag
  if(!is.null(dataTag)&&!dataTag==""){ 
    updateTextInput(session,'dataTag',value=autoSubPunct(dataTag,sepTagUi))
  }
})

# form to validate choice based on class and tags select and
# populate dataMetaList
#formDataValidate<-renderText({
observe({
  tagMinChar<-1
  msgList<-list()
  dTag<-input$dataTag
  dClass<-input$dataClass
  #dataMetaList<-reactivesValues()
  if(!is.null(dClass) && !dClass=="" && !is.null(dTag) && !dTag==""){
    # get unique and ordered tags
    dTag<-getUniqueTagString(dTag,sepIn=sepTagUi,sepOut=sepTagFile)
    # get registered type for this class
    dType<-dataClass[dataClass$class==dClass,'type']
    # proposed data name
    dName<-paste0(c(dClass,dTag),collapse=sepTagPrefix)
    # rule 1 : check if dataTag contains enough characters
    if(nchar(dTag)<tagMinChar){
      msgList$tooShort<-paste("Add minimum",tagMinChar," character tag")
    }else{
      # rule 2 : check if dataset name is already taken.
      # this rule just print a message, but an option to 
      # avoid overwritting could be implemented here.
      if(dName %in% dataList()[[dType]]){ 
        msgList$exists<-paste(" '",dName,"' already exists and will be overwritten.")
      }else{
        msgList$ok<-paste("'",dName,"' available.")
      }
      # populate reactive values.
      dataMetaList$name<-dName
      dataMetaList$type<-dType
      dataMetaList$class<-dClass
      dataMetaList$tags<-dTag
      dataMetaList$ready<-TRUE
      dataMetaList$accepts<-acceptFiles[[dType]]
      dataMetaList$multiple<-acceptMultiple[[dType]]
    }
  }else{
    dataMetaList$ready=FALSE
    msgList$default=paste('Please enter required informations.')
  }
  output$hintNewData<-renderUI(p(
      icon('info-circle'),
        paste(msgList$exists,msgList$tooShort,msgList$default,msgList$ok)
      ))
})

# btn upload style
formDataUpload<-renderUI({
  ready<-dataMetaList$ready
  if(length(ready)>0 && ready){ 
    amFileInput('btnDataNew',
      label='Add dataset',
      style='success',
      fileAccept=dataMetaList$accepts,
      multiple=dataMetaList$multiple
      )
  }else{
     amFileInput('btnDataNew',
      label='Add dataset',
      style='danger',
      disable=TRUE)

  }
})

# manage data panel
formDataManage<-renderUI({
  tagList(
    h4('Filter dataset'),
    radioButtons('typeChoice','Type of data',
      c("Vector" = "vect",
        "Raster"="rast",
        "Table"="table",
        "All"="all"),
      selected="all",
      inline=T
      ),
    textInput(inputId = 'filtData','filter datas names',''),  
    addUIDep(
      selectizeInput("filtDataTag", 
        "filter by tags",
        choices="",
        multiple=TRUE, 
        options = list(plugins = list("drag_drop", "remove_button"))
        )
      ),
    hr(),
    h4('Archive selection'),
    bsProgressBar('progArchive',visible=FALSE),
    bsActionButton('createArchive','Create archive',style='danger'),
    hr(),
    h4('Retrieve archive'),
    selectArchive,
    bsActionButton('getArchive','Get archive',style='danger',disable=TRUE),
    h4('Removing selection'),
    checkboxInput('showDelOption','Show removing option for selected dataset.'),
    conditionalPanel(
      condition = "input.showDelOption == true",
      list(
        hr(),
        bsActionButton('delDataSelect','Delete permanently',style='warning'),
        hr()
        )
      )

    )

})


selectArchive<-renderUI({
  if(!is.null(listen$gisLock)){
    aL<-archiveList()
    if(!is.null(aL)&&length(aL)>0){ 
      selectInput('selArchive','Select archive',choices=aL,selected=aL[length(aL)],selectize=F)
    }else{
      p('No archives found.')
    }
  }
})

#
#observe({
#  dataName<-dataMetaList$name
#  toggleClass(
#    id='dataTag',
#    class=ifelse(!is.null(dataName)&&!dataName=="",'.inputOk','.inputError')
#    )
#})


# upload a data 
observe({
  dataNew<-input$btnDataNew # take reactivity on btnDataNew only.
  dataType<-isolate(dataMetaList$type)
  dataName<-isolate(dataMetaList$name)
  dataClass<-isolate(dataMetaList$class)
  tryReproj<-TRUE # auto reprojection  ?
  # If this observer is trigged, therw should be no null in static list. 
  # to be sure:
  if(!is.null(dataNew) && !is.null(dataName)){
    # get the temp dir
    dataDir<-dirname(dataNew$datapath[1])
    # rename file. Instead of fileinput default, set original name :
    # e.g. road.shp instead of "3"
    dataNew$newPath<-file.path(dataDir,dataNew$name)
    file.rename(dataNew$datapath,dataNew$newPath)
    # if multiple data (shp, adf...), set a directory as data source.
    if(nrow(dataNew)==1){
      dataInput<-dataNew$newPath
      dataFiles<-dataInput
    }else{
      dataInput<-dataDir
      dataFiles<-list.files(dataInput,full.names=T)
    }

    # upload handler for each type.
    # TODO: i
    # 1. Remove tryCatch functions in every upload function and manage error from here only
    # 2. Remove msg in function, replace by message, warning and error.
    # 3. For each uploader, avoid different parameters. DataInput and DataFiles could be grouped in list?
    tryCatch({
      switch(dataType,
        "rast" = amUploadRaster(dataInput,dataName,dataFiles),
        "vect" = amUploadVector(dataInput,dataName,dataFiles),
        "table" = amUploadTable(dataName,dataFiles,dataClass) 
        )
  listen$uploadData<-sample(100,1)
  dataMetaList$ready<-FALSE
  msg(paste('Module manage:',dataName,'imported'))
},
   error = function(cond){
     # filter common error and interpret them
     hint<-c(
       e="file does not exist",
       c="Error : file not recognized, make sure you have uploaded a supported raster files, with all its dependencies.")
     cndMsg <- conditionMessage(cond)
     cond<-ifelse(length(grep(hint[1],cndMsg))>0,hint[2],cond)
     msg(paste('Importation failed:',cond))
   }
   # warning = function(c) msg(paste(dataName,'importation warning',c))
   # message = function(c) msg(paste('Dem importation msg',c))
   )
    # remove tag
    updateTextInput(session,'dataTag',value='')
  }


})



# delete button raster
observe({
  delDataSelect<-input$delDataSelect
  if(!is.null(delDataSelect) && delDataSelect >0){
    tbl<-isolate(dataTableSubset())
    rastName<-as.character(tbl[tbl$type=='rast','name'])
    rastName<-rastName[!rastName %in% 'dem']
    vectName<-as.character(tbl[tbl$type=='vect','name'])
    if(!is.null(rastName) && length(rastName)>0){
      msg(paste('Module manage : removing raster datas. Selected=',paste(rastName, collapse='; ')))
      execGRASS('g.remove',rast=rastName)
    }
    if(!is.null(vectName) && length(vectName)>0){
      msg(paste('Module manage : removing vectors datas. Selected=',paste(vectName, collapse='; ')))
      execGRASS('g.remove',vect=vectName)
    }
    updateTextInput(session,'filtData',value = '')
    updateSelectizeInput(session,'filtDataTag',selected = '')
    listen$deleteData<-sample(100,1)
  }  
})

# Dynamic filter by existing tag for raster
dataTableSubset<-reactive({
  # invalidation dependencies
  input$delDataSelect
  filtDataTag<-input$filtDataTag
  filtData<-input$filtData
  typeChoice<-input$typeChoice

  if(!is.null(filtDataTag) || !is.null(filtData)){
    tryCatch({
      # get names of available datas from dataList. Get main type only.
      dataNames<-as.character(unlist(dataList()[c('rast','vect','table')]))
      # if no names are present, stop and return an empty table
      if(length(dataNames)<1)return(data.table())
      # filter tags based name, create list of length 2:
      # 1.table of decomposed tags and name
      # 2.unique tags.
      filteredList<-amFilterDataTag(
        namesToFilter=dataNames,
        filterTag=input$filtDataTag,
        filterText=input$filtData
        )
      # query dataClassList for matching type with prefix
      #tbl<-filteredList$tagsTable  
      tbl<-filteredList
      names(tbl)<-c('class','tags','name','nameFilter')
      if(nrow(tbl)>0){
        #tbl$type<-as.character(unlist(dataClassList[tbl$prefix]))
        tbl$type<-dataClass[match(tbl$class, dataClass$class),'type']
      }else{
        return(data.frame())
      }
      # filter data type
      mType<-switch(typeChoice,
        vect=c('vect'),
        rast=c('rast'),
        table=c('table'),
        all=c('vect','rast','table') 
        ) 
      tbl<-tbl[tbl$type %in% mType,]
      # rename table
      # unique tags to populate selectize input.
      tagsUnique<-c(
        unique(tbl$class), # e.g c(road, landcover, barrier)
        unique(unlist(strsplit(tbl$tags,sepTagRepl))) # e.g. c(secondary, cumulative)
        )
      # using filtered value, update choices in filtDataTag selectize input.
      updateSelectizeInput(session,'filtDataTag',choices=tagsUnique,selected=filtDataTag)
      return(tbl)
    },error=function(c)message(c)
    )
  }
})


# render dataTable
output$dataTableSubset<-renderDataTable({
  tbl<-dataTableSubset()
  if(length(tbl)>0){ 
    dataTableSubset()[,c('class','tags','type')]
  }else{
    data.frame()
  }
},options=list(
  searching = FALSE,
  pageLength = 100,
  searchable=FALSE, 
  paging=FALSE
  ))


# if no data are selected, avoid creation of archives.
observe({
  tDataL<-length(dataTableSubset())
  if(tDataL>0){
    updateButton(session,'createArchive',style='success',disable=FALSE)
  }else{
    updateButton(session,'createArchive',style='danger',disable=TRUE)
  }
})


# check for archive path when gisLock change
# set liste$archivePath to actual archivePath
observe({
  if(!is.null(listen$gisLock)){
    archivePath<-system(paste('echo',archiveGrass),intern=TRUE) 
    # if archive directory is not existant, create it.
    R.utils::mkdirs(archivePath)
    archivePath<-normalizePath(archivePath) 
    addResourcePath(archiveBaseName,archivePath)
    listen$archivePath<-archivePath
  }
})


# archive list 
archiveList<-reactive({
  if(!is.null(listen$gisLock)){
    listen$addArchive
    archivePath<-isolate(listen$archivePath)
    list.files(archivePath)
  }
})


#observe({
#  selArchive<-input$selArchive
#  if(!is.null(selArchive)&&!selArchive==""){
#    updateButton(session,'getArchive',style='success',disable=FALSE)
#  }else{
#    updateButton(session,'getArchive',style='danger',disable=TRUE)
#  }
#})

observe({
  selArchive<-input$selArchive
  if(!is.null(selArchive)&&!selArchive==""){
    #updateStylid,te
    updateButton(session,'getArchive',style='success',disable=FALSE)
  }else{
    updateButton(session,'getArchive',style='danger',disable=TRUE)
  }
})


# link selected archive to a new window location. The browser should as to download.
#TODO: as it's rendered in the same window, it could break shiny application, or reset it. Make sure that's not a problem with standard browser. Works with webkit browser.
output$js<-renderUI({
  getArchive<-input$getArchive 
  if(!is.null(getArchive) && getArchive>0){
    msg(paste('Manage data: archive',isolate(input$selArchive),"requested for download."))
    archivePath<-isolate(file.path(archiveBaseName,input$selArchive))
    message(archivePath)
    tags$script(paste0("window.location.assign(\"",archivePath,"\");")) 
  }
})

#if create archive is requested, get data names, export them and create archive.
# for each data a dataDir will be created, listed in listDirs.
observe({
  createArchive<-input$createArchive
  archivePath<-isolate(listen$archivePath)
  if(!is.null(createArchive) && createArchive>0){
    tryCatch({ 
      updateButton(session,'createArchive',style='danger',disable=TRUE)
      updateProgressBar(session,'progArchive',value=1,visible=TRUE)
      tData<-isolate(dataTableSubset()[c('name','type')])
      tData[]<-lapply(tData, as.character)
      tmpDataDir <- tempdir()
      listDataDirs<-c() #empty dataDir container      
      wdOrig<-getwd()
      tDataL<-nrow(tData)
      inc=1/(tDataL+1)*100 # increment for progressbar. +1 for zip
      for(i in 1:nrow(tData)){
        dataName<-tData[i,'name']
        dataDir<-file.path(tmpDataDir,dataName)
        dir.create(dataDir)
        switch(tData[1,'type'],
          'vect'={
            filePath<-exportGrass(dataName,dataDir,type='vect')
            listDataDirs<-c(listDataDirs,dataDir)
          },
          'rast'={
            filePath<-exportGrass(dataName,dataDir,type='rast')   
            listDataDirs<-c(listDataDirs,dataDir)
          },
          'table'=msg('no code yet for table.')
          )
        updateProgressBar(session,'progArchive',value=i*inc)
      }
      archiveName<-file.path(archivePath,paste0(getSysTime(),'.zip'))
      setwd(tmpDataDir)
      zip(archiveName,files = basename(listDataDirs))
      unlink(listDataDirs,recursive=T)
      setwd(wdOrig)    
      listen$addArchive<-runif(1)
      updateProgressBar(session,'progArchive',value=100)
      msg(paste('Module manage: archive created:',basename(archiveName)))
      Sys.sleep(1)
      updateProgressBar(session,'progArchive',value=0,visible=FALSE)
    },
    error=function(c)msg(paste('Error:',c))
    )
  }
})


tableDataset<-renderUI({
  stylePanel<-ifelse(!is.null(dataTableSubset())&&length(dataTableSubset())>0,'info','warning')
  div(class='table-fixed',
    tagList(
      panel(stylePanel,'Table of available dataset',
        dataTableOutput("dataTableSubset"))
      ))
})
