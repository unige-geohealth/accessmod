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

# observer

observe({
  dataClassChoices<-config$dataClass[config$dataClass$allowNew==TRUE,'class']
  updateSelectInput(session,'dataClass',choices=dataClassChoices)
})

# observer to perform a quick validation of user tag input
#observe({  
#  dataTag<-input$dataTag
#  if(!is.null(dataTag)&&!dataTag==""){
#    updateTextInput(session,'dataTag',value=amSubPunct(dataTag,config$sepTagUi))
#  }
#})


observe({
  archiveList<-dataList$archive
  if(is.null(archiveList))archiveList=""
  updateSelectInput(session,"selArchive",choices=archiveList,selected=archiveList[1])
})


# validate choice based on class and tags select and  populate dataMetaList
observe({
  # init
  tagMinChar<-1
  msgList<-list()#empty list. return null if no msg.
  dInfo=NULL
  err = character(0)
  info = character(0)

# input import
  dTag<-input$dataTag# reevaluate if tags changes
  dClass<-input$dataClass # reevaluate if class changes
  sepTagUi=config$sepTagUi
  sepTagFile=config$sepTagFile

  #-------------------#
  # validation process
  #-------------------#
  if(!is.null(dClass) && !dClass=="" && !is.null(dTag) && !dTag==""){
    # get unique and ordered tags
    #dTag<-amGetUniqueTag(dTag,sepIn=sepTagUi,sepOut=sepTagFile)
    dTag<-amSubPunct(dTag,sepTagFile,rmTrailingSep=T,rmLeadingSep=T)
    # get registered type for this class
    dType<-config$dataClass[config$dataClass$class==dClass,'type']
    # formated data name
    dName<-amNewName(dClass,dTag,config$sepClass,config$sepTagFile)

    tagsTooShort<-nchar(dTag)<tagMinChar
    dataExists<-paste0(dName,config$sepMapset,listen$mapset) %in% isolate(dataList)[[dType]]

    if(tagsTooShort) err <-c(err,'Tags too short or missing. Please complete.')
    if(dataExists) err <- c(err,paste(dName," already exists. Please delete it first or change tags."))
    if(!dataExists) info <- c(info,paste(dName," available."))

    if(! tagsTooShort && !dataExists){
      # populate meta data list
      dInfo<-list(
        name=dName,
        type=dType,
        class=dClass,
        tags=dTag,
        accepts=config$filesAccept[[dType]],
        multiple=config$fileAcceptMultiple[[dType]]
        )
    }
  }else{
    err <- c(err,'Please enter required information.')
  }


  # create HTML for validation message list.
  if(length(err)>0){
    err<-tags$ul(
      HTML(paste("<li>",icon('exclamation-triangle'),err,"</li>",collapse=""))
      )
    disBtn=TRUE
  }else{
    disBtn=FALSE
  }
  if(length(info)>0) info<- tags$ul(HTML(paste("<li>",icon('info-circle'),info,"</li>",collapse="")))

  # send result to ui
  if(length(err)>0 || length(info)>0){
    msgList <- tagList(tags$b('Validation'),err,info)
  }else{
    msgList <- tagList(tags$b(paste('This message is not supposed to be empty.')))
  }

  output$msgModuleData <-renderUI({msgList})
  amActionButtonToggle(session=session,'btnDataNew',disable=disBtn)
  # update the file input part
  if(!disBtn){
    amFileInputUpdate('btnDataNew',session, accepts=dInfo$accepts,multiple=dInfo$multiple)
  }
  # save in reactive object for upload function
  listen$newDataMeta<-dInfo
})




# upload a dataset 
observe({
  dNew<-input$btnDataNew # take reactivity on btnDataNew only.
  dMeta<-isolate(listen$newDataMeta)
  tryReproj<-TRUE # auto reprojection  ?
  if(!is.null(dNew) && !is.null(dMeta)){
    amBusyManage(session,TRUE)
    amUpdateProgressBar(session,'progNewData',20)
    updateTextInput(session,'dataTag',value='')
    amErrorAction(title='Module data : importation',{
      # extract arg from list
      dType<-dMeta$type
      dName<-dMeta$name
      dClass<-dMeta$class

      if(paste0(dName,config$sepMapset,isolate({listen$mapset})) %in% isolate(dataList)[[dType]]){
      
      }
      # get the temp dir
      dDir<-dirname(dNew$datapath[1])
      # rename file. Instead of fileinput default, set original name :
      # e.g. road.shp instead of "3"
      dNew$newPath<-file.path(dDir,dNew$name)
      file.rename(dNew$datapath,dNew$newPath)
      # if multiple data (shp, adf...), set a directory as data source.
      if(nrow(dNew)==1){
        dInput<-dNew$newPath
        dFiles<-dInput
      }else{
        dInput<-dDir
        dFiles<-list.files(dInput,full.names=T)
      }
      # retrieve default color table by class
      dColors<-config$dataClass[config$dataClass$class==dClass,'colors']
      # TODO: 
      # 1.use basename and dirname in function instead of two similar input path.
      # 2. update dataList via listen from here instead from upload function.
      # upload handler for each type. 
      #    dInput = complete path to dir if multiple OR single file . length=1
      #    dFiles = complete path to file(s) . length=1+
      #    dClass = for table, distinction between class (model, lcv..)
      #    listen = used to signal data update in dataList and, 
      #             for table, get dataBase connection
      switch(dType,
        "raster" = amUploadRaster(dInput,dName,dFiles,dColors,listen),
        "vector" = amUploadVector(dInput,dName,dFiles,listen),
        "table" = amUploadTable(dName,dFiles,dClass,listen) 
        )
amUpdateProgressBar(session,'progNewData',100)
      # if no error intercepted by tryCatch:invalidate metadata, log message and remove tags.
      listen$newDataMeta<-NULL
      amMsg(session,type="log",text=paste('Module manage:',dName,'imported'))

    }) 
    amBusyManage(session,FALSE)
  }
})

# Delete selected dataset
observe({
  delDataSelect<-input$delDataSelect
  if(!is.null(delDataSelect) && delDataSelect >0){
    tbl<-isolate(dataListTableSelected())
    rastName<-as.character(tbl[tbl$type=='raster','origName'])
    rastName<-rastName[!rastName %in% 'dem'] # do not allow removing DEM
    vectName<-as.character(tbl[tbl$type=='vector','origName'])
    tableName<-as.character(tbl[tbl$type=='table','origName'])
    if(length(rastName)>0){
      amMsg(session,type="log",text=paste('Module manage : removing raster datas. Selected=',paste(rastName, collapse='; ')))
      rmRastIfExists(rastName)
    }
    if(length(vectName)>0){
      amMsg(session,type="log",text=paste('Module manage : removing vectors datas. Selected=',paste(vectName, collapse='; ')))
      rmVectIfExists(vectName)
    }
    if(length(tableName)>0){
      dbCon<-isolate(listen$dbCon)
      #sqlexpr<-paste("DROP TABLE IF EXISTS",tableName,";",collapse="")
      #dbGetQuery(dbCon,sqlexpr) NOTE:doesn't work, and doesn't return a message...
      for(t in tableName){
      dbGetQuery(dbCon,paste("DROP TABLE IF EXISTS",t))
    }
    }
    updateTextInput(session,'filtData',value = '')
    updateSelectizeInput(session,'filtDataTag',selected = '')
    amUpdateDataList(listen)
  }  
})


# create reactive data list table with subset by text filter.
dataListTable<-reactive({

  tbl<-dataList$df[]
  if(length(tbl)<1)return()
  f<-input$filtData
  t<-input$typeDataChoice
  i<-input$internalDataChoice
  d<-config$dataClass
  c<-d[d$internal == FALSE | d$internal == i,]$class
  tbl<-tbl[tbl$class %in% c,]
  t<-switch(t,
          vector=c('vector'),
          raster=c('raster'),
          table=c('table'),
          all=c('vector','raster','table') 
          ) 
  tbl<-amDataSubset(pattern=f,type=t,tbl) 
  if(nrow(tbl)>0){ 
    tbl$select=TRUE
    return(tbl)
  }else{
    data.frame(NULL)
  }
})

# display data set table in handson table
output$dataListTable<-renderHotable({
  tbl<-dataListTable()
  if(length(tbl)>0){  
    tbl<-tbl[c('select','type','class','tags','origName')]
  }else{
    tbl<-data.frame(select='',class="-",tags="-",type="-",origName='-')
  }
  tbl
},stretch='last',readOnly=c(2,3,5))


# rename layers
observe({
  amErrorAction(title='Data list auto rename',{
    dataListUpdate<-hot.to.df(input$dataListTable)[,c('type','class','tags','origName')]
    isolate({
      # If any empty string in select column or dash in original name: do nothing.
      # Why ? This is the hint of a empty table (not yet populated)
      if(any('' %in% dataListUpdate$select || "-" %in% dataListUpdate$origName)){
        amUpdateDataList(listen)
        return()
      }
      #create original dataList table
      dataListOrig<-dataListTable()[,c('type','class','tags','origName')]
      # launch functio to update tables and maps by tags.
      amUpdateDataListName(dataListOrig,dataListUpdate,listen$dbCon)

      amUpdateDataList(listen)
    })
          })
})




# table of data set selected, merged with dataListTable.
# NOTE: take dependencies on both : handson table OR dataListTable().
dataListTableSelected<-reactive({
  tblHot <- hot.to.df(input$dataListTable)
  tblOrig <- dataListTable()
  if(length(tblOrig)<1 || length(tblHot)<1) return()
  tbl<-merge(tblOrig,tblHot,by=c('tags','type','class'))
  if(length(tbl)>0)return(tbl[tbl$select.y==TRUE,]) # return only selected rows.
  data.frame(select='',class="",tags="",type="-")
})


# if no data is selected, disable "createArchive" button.
observe({
  tbl=dataListTableSelected()
  if(is.null(tbl) || nrow(tbl)<1 ||  TRUE %in% tbl$select ){
    disBtn=TRUE
  }else{
    disBtn=FALSE
  }
  amActionButtonToggle('createArchive',session, disable=disBtn)
})

# if no archive is selected, disable "getArchive" button.
observe({
  selArchive<-input$selArchive
  amActionButtonToggle('getArchive',session,disable=is.null(selArchive)||selArchive=="")
})


# if get archive btn is pressed, lauch amGetData fucntion
observe({
  getArchive<-input$getArchive
  selArchive<-isolate(input$selArchive)
  if(!is.null(getArchive) && getArchive>0 && !is.null(selArchive) && !selArchive==""){
    amBusyManage(session,TRUE)
    amMsg(session,type="log",text=paste('Manage data: archive',selArchive,"requested for download."))
    # archiveBaseName= base url accessible from client side.
    #archivePath<-file.path(isolate({listen$archivePath}),selArchive)
    archivePath<-file.path(config$pathArchiveBaseName,selArchive)
    amGetData(session, archivePath)
    amBusyManage(session,FALSE)
  }
})



#if create archive is requested, get data names, export them and create archive.
# for each data a dataDir will be created, listed in listDirs.
observe({
  createArchive<-input$createArchive
  archivePath<-isolate(listen$archivePath)
  dbC<-isolate(listen$dbCon)
  if(!is.null(createArchive) && createArchive>0){

    amBusyManage(session,TRUE)
    amErrorAction(title='Module data: create archive',{
      amActionButtonToggle('createArchive',session,disable=TRUE)
      amUpdateProgressBar(session,'progArchive',1)
      tData<-isolate(dataListTableSelected())
      tData<-tData[c('origName','type')]
      tData[]<-lapply(tData, as.character)
      tmpDataDir <- tempdir()
      listDataDirs<-c() #empty dataDir container      
      wdOrig<-getwd()
      tDataL<-nrow(tData)
      inc=1/(tDataL+1)*100 # increment for progressbar. +1 for zip
      for(i in 1:tDataL){
        dataName<-tData[i,'origName']
        dataDir<-file.path(tmpDataDir,dataName)
        dir.create(dataDir,showWarnings=F)
        type<-tData[i,'type']
        amMsg(session,type='log',text=paste("export",type,dataName),title="Export")
        switch(type,
          'vector'={
            amExportData(dataName,dataDir,type='vector')
          },
          'raster'={
            amExportData(dataName,dataDir,type='raster')   
          },
          'table'={
            amExportData(dataName,dataDir,type='table',dbCon=dbC)
          }
          )
        listDataDirs<-c(listDataDirs,dataDir)
        amUpdateProgressBar(session,'progArchive',i*inc)
        print(paste(i,'on',tDataL,'exported.'))
      }
      archiveName<-file.path(archivePath,paste0(amSysTime(),'.zip'))
      setwd(tmpDataDir)
      zip(archiveName,files = basename(listDataDirs))#files = all directories.
      unlink(listDataDirs,recursive=T)
      setwd(wdOrig)    
      amUpdateDataList(listen)
      amMsg(session,type="log",text=paste('Module manage: archive created:',basename(archiveName)))
      amUpdateProgressBar(session,'progArchive',100)
      amSleep(1000) #
  })

    amBusyManage(session,FALSE)
  }
})


