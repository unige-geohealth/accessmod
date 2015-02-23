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
  dataClassChoices<-dataClass[dataClass$allowNew==TRUE,'class']
  updateSelectInput(session,'dataClass',choices=dataClassChoices)

})

# observer to perform a quick validation of user tag input
observe({  
  dataTag<-input$dataTag
  if(!is.null(dataTag)&&!dataTag==""){ 
    updateTextInput(session,'dataTag',value=amSubPunct(dataTag,sepTagUi))
  }
})


observe({
  archiveList<-dataList$archive
  if(is.null(archiveList))archiveList=""
  updateSelectInput(session,"selArchive",choices=archiveList,selected=archiveList[1])
})


# validate choice based on class and tags select and  populate dataMetaList
observe({
  tagMinChar<-1
  msgList<-list()#empty list. return null if no msg.
  dInfo=NULL
  dTag<-input$dataTag# reevaluate if tags changes
  dClass<-input$dataClass # reevaluate if class changes
  #-------------------#
  # validation process
  #-------------------#
  if(!is.null(dClass) && !dClass=="" && !is.null(dTag) && !dTag==""){
    # get unique and ordered tags
    dTag<-amGetUniqueTag(dTag,sepIn=sepTagUi,sepOut=sepTagFile)
    # get registered type for this class
    dType<-dataClass[dataClass$class==dClass,'type']
    # formated data name
    dName<-amNewName(dClass,dTag,sepClass,sepTagFile)
    if(nchar(dTag)<tagMinChar){
      #-----------------------------------------------------#
      # rule 1 : check if dataTag contains enough characters
      #-----------------------------------------------------#
      msgList$tooShort<-paste("Add minimum",tagMinChar," character tag")
    }else{
      if(paste0(dName,'@',listen$mapset) %in% isolate(dataList)[[dType]]){
      #-------------------------------------------------#
      # rule 2 : check if dataset name is already taken.
      # this rule just print a message, but an option to 
      # avoid overwritting could be implemented here.
      #-------------------------------------------------#
      msgList$exists<-paste(" '",dName,"' already exists and will be overwritten.")
    }else{
      #-------------------------------------------#
      # if everything is ok, set the final message
      #-------------------------------------------#
      msgList$ok<-paste("'",dName,"' available.")
      }
      # populate meta data list
      dInfo<-list(
        name=dName,
        type=dType,
        class=dClass,
        tags=dTag,
        accepts=acceptFiles[[dType]],
        multiple=acceptMultiple[[dType]]
        )
    }
  }else{
    #----------------#
    # default message
    #----------------#
    msgList$default=paste('Please enter required informations.')
  }
  #-------------#
  # action on UI
  #-------------#
  # update hint div
  amUpdateText(session,"hint-new-data",
    paste(icon('info-circle'),paste(msgList,collapse='. '))
    )
  # update button status
  dis<-is.null(dInfo)
  # update the btn part
  amActionButtonToggle('btnDataNew',session,disable=dis)
  # update the file input part
  if(!dis){
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
    amUpdateProgressBar(session,'progNewData',20)
    updateTextInput(session,'dataTag',value='')
    amErrorAction(title='Module data : importation',{
      # extract arg from list
      dType<-dMeta$type
      dName<-dMeta$name
      dClass<-dMeta$class
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
      dColors<-dataClass[dataClass$class==dClass,'colors']
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
      sqlexpr<-paste("DROP TABLE",tableName,";",collapse="")
      dbGetQuery(dbCon,sqlexpr)
    }
    updateTextInput(session,'filtData',value = '')
    updateSelectizeInput(session,'filtDataTag',selected = '')
    amUpdateDataList(listen)
  }  
})



dataListTable<-reactive({
  tbl<-dataList$df[]
  if(length(tbl)<1)return()
  f<-input$filtData
  t<-input$typeChoice
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

output$dataListTable<-renderHotable({
  tbl<-dataListTable()

  if(length(tbl)>0){  
    tbl<-tbl[c('select','type','class','tags')]
  }else{
    tbl<-data.frame(select='',class="",tags="",type="-")
  }
  tbl
},stretch='last')


dataListTableSelected<-reactive({
  tblHot <- hot.to.df(input$dataListTable)
  tblOrig <- dataListTable()
  if(length(tblOrig)<1 || length(tblHot)<1) return()
  tbl<-merge(tblOrig,tblHot,by=c('tags','type','class'))
  if(length(tbl)>0)return(tbl[tbl$select.y==T,])
  data.frame(select='',class="",tags="",type="-")
})



#
## render dataTable
#output$dataTableSubset<-renderDataTable({
#  tbl<-dataTableSubset()
#  if(length(tbl)>0){ 
#    dataTableSubset()[,c('class','tags','type')]
#  }else{
#    data.frame()
#  }
#},options=list(
#  searching = FALSE,
#  pageLength = 100,
#  searchable=FALSE, 
#  paging=FALSE
#  ))
#
#
#

## Dynamic filter by existing tag for raster
#dataTableSubset<-reactive({
#  # invalidation dependencies
#  listen$gisLock
#  input$delDataSelect
#  filtDataTag<-input$filtDataTag
#  filtData<-input$filtData
#  typeChoice<-input$typeChoice
#  if(!is.null(filtDataTag) || !is.null(filtData)){
#    amErrorAction(title='Module data: data subset',{
#        # get names of available data from dataList. Get main type only.
#        dataNames<-as.character(unlist(reactiveValuesToList(isolate(dataList))[c('vector','raster','table')]))
#        # if no names are present, stop and return an empty table
#        if(length(dataNames)<1)return(data.frame())
#        # filter tags based name, create list of length 2:
#        # 1.table of decomposed tags and name
#        # 2.unique tags.
#        tbl<-amFilterDataTag(
#          namesToFilter=dataNames,
#          filterTag=input$filtDataTag,
#          filterText=input$filtData
#          )
#        # query dataClassList for matching type with prefix
#        names(tbl)<-c('class','tags','name','nameFilter')
#        if(nrow(tbl)>0){
#          #tbl$type<-as.character(unlist(dataClassList[tbl$prefix]))
#          tbl$type<-dataClass[match(tbl$class, dataClass$class),'type']
#        }else{
#          return(data.frame())
#        }
#        # filter data type
#        mType<-switch(typeChoice,
#          vector=c('vector'),
#          raster=c('raster'),
#          table=c('table'),
#          all=c('vector','raster','table') 
#          ) 
#        tbl<-tbl[tbl$type %in% mType,]
#        # rename table
#        # unique tags to populate selectize input.
#        return(tbl)
#    }) 
#  }
#})
#

#observe({
#  tbl<-dataListTable()
#  filtDataTag<-input$filtDataTag
#  if(length(tbl)>0){
#    tagsUnique<-c(
#      unique(tbl$class), # e.g c(road, landcover, barrier)
#      unique(unlist(strsplit(tbl$tags,sepTagRepl))) # e.g. c(secondary, cumulative)
#      )
#    # using filtered value, update choices in filtDataTag selectize input.
#    updateSelectizeInput(session,'filtDataTag',choices=tagsUnique,selected=filtDataTag)
#  }
#})

## render dataTable
#output$dataTableSubset<-renderDataTable({
#  tbl<-dataTableSubset()
#  if(length(tbl)>0){ 
#    dataTableSubset()[,c('class','tags','type')]
#  }else{
#    data.frame()
#  }
#},options=list(
#  searching = FALSE,
#  pageLength = 100,
#  searchable=FALSE, 
#  paging=FALSE
#  ))
#

# if no data are selected, avoid creation of archives.
observe({
  tbl=dataListTableSelected()
  if(is.null(tbl) || nrow(tbl)<1 ||  TRUE %in% tbl$select ){
    disBtn=TRUE
  }else{
    disBtn=FALSE
  }
  amActionButtonToggle('createArchive',session, disable=disBtn)
})


observe({
  selArchive<-input$selArchive
  amActionButtonToggle('getArchive',session,disable=is.null(selArchive)||selArchive=="")
})


observe({
  getArchive<-input$getArchive
  selArchive<-isolate(input$selArchive)
  if(!is.null(getArchive) && getArchive>0 && !is.null(selArchive) && !selArchive==""){
    amMsg(session,type="log",text=paste('Manage data: archive',selArchive,"requested for download."))
    # archiveBaseName= base url accessible from client side.
    archivePath<-file.path(archiveBaseName,selArchive)
    amGetData(session, archivePath)
  }
})



#if create archive is requested, get data names, export them and create archive.
# for each data a dataDir will be created, listed in listDirs.
observe({
  createArchive<-input$createArchive
  archivePath<-isolate(listen$archivePath)
  dbC<-isolate(listen$dbCon)
  if(!is.null(createArchive) && createArchive>0){
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
      Sys.sleep(1)
  })
  }
})


