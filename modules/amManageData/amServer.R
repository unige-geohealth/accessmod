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

# Update selection of available data class to upload
observe({

  dAll <- list() 

  dc<-config$dataClass[
    config$dataClass$allowNew,
    c(config$language,'class','type')
    ]

  for(i in c("raster","table","vector")){
    ds <- dc[dc$type==i,]
    val <- ds$class
    names(val)<- paste0("(",substr(i,0,1),") ",ds[,config$language])
    dAll[[i]]<-val
  }

  updateSelectInput(
    session=session,
    inputId='dataClass',
    choices=dAll
    )

})

# Update available archives
observe({
  archiveList<-dataList$archive
  if(is.null(archiveList))archiveList=""
  updateSelectInput(
    session = session,
    inputId = "selArchive",
    choices = archiveList,
    selected = archiveList[1]
    )
})

# Update tags for the data filter
observe({
 tagsList <- dataList$tags
 lastComputedTags <- listen$lastComputedTags 
 
 if(is.null(tagsList))tagsList=""
 if(is.null(lastComputedTags))lastComputedTags=""

 updateSelectInput(
   session = session,
   inputId = 'filtDataTags',
   choices = tagsList,
   selected = lastComputedTags
   )
})


# validate choice based on class and tags select and  populate dataMetaList
observe({
  amErrorAction( title = "Data panel validation",{
  # init
  tagMinChar<-1
  msgList<-list()#empty list. return null if no msg.
  dInfo=NULL
  err = character(0)
  info = character(0)

# input import
  dTag<-input$dataTag# reevaluate if tags changes
  dClass<-input$dataClass # reevaluate if class changes
  sepTagFile=config$sepTagFile 
  isDem <- isTRUE(dClass == amGetClass(config$mapDem))




  # name validation process
  if(isDem){ 
    info <- c(info,"The importation of a new DEM will overwrite your current DEM and reset the base grid parameters: number of cells, resolution and extent. Proceed with caution.")
    info <- c(info,"Data is automatically named, no tags are required")
     # populate meta data list
    dName <- strsplit(config$mapDem,"@")[[1]][[1]]
        dInfo<-list(
          name=dName,
          type="raster",
          class=amGetClass(dName),
          tags="dem",
          accepts=config$filesAccept[['raster']],
          multiple=config$fileAcceptMultiple[['raster']]
          )
  }else{
    if(!is.null(dClass) && !dClass=="" && (!is.null(dTag) && !dTag=="")){
      # get unique and ordered tags
      dTag<-amSubPunct(dTag,sepTagFile,rmTrailingSep=T,rmLeadingSep=T)
      dTagDisplay <- amSubPunct(dTag)
      # get registered type for this class
      dType<-config$dataClass[config$dataClass$class==dClass,'type']
      # formated data name
      dName<-amNewName(dClass,dTag,config$sepClass,config$sepTagFile)
      dNameDisplay <- paste0(amClassListInfo(dClass)," [",paste0(dTagDisplay,collapse=','),"]")

      tagsTooShort<-nchar(dTag)<tagMinChar
      dataExists<-paste0(dName,config$sepMapset,grassSession$mapset) %in% isolate(dataList)[[dType]]


      if(tagsTooShort) err <-c(err,'Tags too short or missing. Please complete.')
      if(dataExists) err <- c(err,paste("The data '",dNameDisplay,"' already exists. Please change the tag(s)"))
      # removed as required by Steeve.
      if(!dataExists) info <- c(info,paste(dNameDisplay," available."))

      if((! tagsTooShort && !dataExists)){
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
      err <- c(err,'Please enter the required information.')
    }
  }


 

  # create HTML for validation message list.
  if(length(err)>0){
    err<-tags$ul(
      HTML(
        paste(
          "<li>",
          icon('exclamation-triangle'),
          err,
          "</li>",
          collapse=""
          )
        )
      )
    disBtn=TRUE
  }else{
    disBtn=FALSE
  }
  if(length(info)>0) info <- tags$ul(
    HTML(
      paste(
        "<li>",
        icon('info-circle'),
        info,
        "</li>",
        collapse=""
        )
      )
    )

  # send result to ui
  if(length(err)>0 || length(info)>0){
    msgList <- tagList(tags$b('Validation information'),err,info)
  }else{
    msgList <- "" # tagList(tags$b(paste('This message is not supposed to be empty.')))
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
})




# upload a dataset 
observeEvent(input$btnDataNew,{

  amErrorAction(
  title='Module data : importation',
  {
    dNew<-input$btnDataNew # take reactivity on btnDataNew only.
    dMeta<-listen$newDataMeta
    tryReproj<-TRUE # auto reprojection  ?
    if(!is.null(dNew) && !is.null(dMeta)){
    
    # init progressBar
    pBarTitle <- "Data importation"
    progressBarControl(
      visible=TRUE,
      percent=0,
      title=pBarTitle,
      text="Data analysis..."
      )

   
    updateTextInput(session,'dataTag',value='')
      # extract arg from list
      dType<-dMeta$type
      dName<-dMeta$name
      dClass<-dMeta$class
      if (dClass == "dem") {
        dName = amNoMapset(config$mapDem)
      }

      # get the temp dir
      dDir<-dirname(dNew$datapath[1])
      # rename file. Instead of fileinput default, set original name :
      # e.g. road.shp instead of "3"
      dNew$newPath<-file.path(dDir,dNew$name)
      file.rename(dNew$datapath,dNew$newPath)
      stopifnot(file.exists(dNew$newPath)) 
      # if multiple data (shp, adf...), set a directory as data source.
      if(nrow(dNew)==1){
        dInput<-dNew$newPath
        dFiles<-dInput
      }else{
        dInput<-dDir
        dFiles<-list.files(dInput,full.names=T)
      }
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
        "raster" = amUploadRaster(
          config,
          dataInput=dInput,
          dataName=dName,
          dataFiles=dFiles,
          dataClass=dClass,
          pBarTitle = "Upload raster"
          ),
        "vector" = amUploadVector(
          dataInput=dInput,
          dataName=dName,
          dataFiles=dFiles,
          pBarTitle = "Upload vector"
          ),
        "table" = amUploadTable(
          config,
          dataName=dName,
          dataFile=dFiles,
          dataClass=dClass,
          dbCon=grassSession$dbCon,
          pBarTitle = pBarTitle
          )
        )

      amUpdateDataList(listen)

      progressBarControl(
        visible=TRUE,
        percent=100,
        title=pBarTitle,
        text="Process end")
      

    #  amUpdateProgressBar(session,'progNewData',100)
      # if no error intercepted by tryCatch:invalidate metadata, log message and remove tags.
      listen$newDataMeta<-NULL
      amMsg(session,type="log",text=paste('Module manage:',dName,'imported'))

   progressBarControl(
        visible=FALSE,
        percent=0,
        title="",
        text="")
      
  }
    }) 
})



#
#observeEvent(input$btnRmSelected,{
#  tbl<-dataListTableSelected()
#
#})
#
observeEvent(input$delDataSelect,{
  amErrorAction(title="Module data: data deletion confirmation",{
    tbl <- dataListTableSelected()
    nItems <- nrow(tbl)

    if(nItems >1 ){
      txtHead<-tags$span(sprintf("Those %s items will be deleted",nItems))
    }else{ 
      txtHead<-tags$span("This item will be deleted")
    }

    listData <- tags$div(style="max-height:300px;overflow-y:scroll;",
      tags$ul(
        HTML(paste("<li><b>",toupper(tbl$type),": </b>",tbl$displayClass,"[",tbl$tags,"] </li>"))
        )
      )
    content  <- tagList(
      txtHead,
      listData
      )
    aBtns = list(
      actionButton('delDataSelectConfirm',"Delete")
      )
    amUpdateModal(panelId="amModal",title="Confirmation",html=content,listActionButton=aBtns,addCancelButton=TRUE)
    })
})



# Delete selected dataset
observeEvent(input$delDataSelectConfirm,{
    amUpdateModal("amModal",close=TRUE) 
    tbl<-isolate(dataListTableSelected())
    rastName<-as.character(tbl[tbl$type=='raster','origName'])
    rastName<-rastName[!rastName %in% 'dem'] # do not allow removing DEM
    vectName<-as.character(tbl[tbl$type=='vector','origName'])
    tableName<-as.character(tbl[tbl$type=='table','origName'])
    shapeName<-as.character(tbl[tbl$type=='shape','origName'])
    listName<-as.character(tbl[tbl$type=='list','origName'])
    if(length(rastName)>0){
      amMsg(session,type="log",text=paste('Module manage : removing raster datas. Selected=',paste(rastName, collapse='; ')))
      rmRastIfExists(rastName)
    }
    if(length(vectName)>0){
      amMsg(session,type="log",text=paste('Module manage : removing vectors datas. Selected=',paste(vectName, collapse='; ')))
      rmVectIfExists(vectName)
    }
    if(length(tableName)>0){
      dbCon<-isolate(grassSession$dbCon)
      #sqlexpr<-paste("DROP TABLE IF EXISTS",tableName,";",collapse="")
      #dbGetQuery(dbCon,sqlexpr) NOTE:doesn't work, and doesn't return a message...
      for(t in tableName){
      dbGetQuery(dbCon,paste("DROP TABLE IF EXISTS",t))
    }
    }
    if(length(shapeName)>0){
    for(i in shapeName){
      allShpFiles <- amGetShapesList(pattern=sprintf('^%s\\.',i))
        for( shpP in allShpFiles){
          file.remove(shpP) 
        }
    }
    }
    if(length(listName)>0){
    for(i in listName){
        allListFiles<- amGetListsList(pattern=sprintf('^%s\\.',i))
        for( lFile in allListFiles ){
          file.remove(lFile) 
        }
    }
    }
    updateTextInput(session,'filtData',value = '')
    updateSelectInput(session,'filtDataTags',selected = '')
    amUpdateDataList(listen)
})


# create reactive data list table with subset by text filter.
dataListTable<-reactive({
  tbl<-dataList$df[]
  if(length(tbl)<1)return()
  f<-input$filtData
  a<-input$filtDataTags
  t<-input$typeDataChoice
  i<-input$internalDataChoice
  d<-config$dataClass

  isolate({
    # outfiles are requested after a computation. invalide other filter
    o <- listen$outFiles
  })
  if(!is.null(o)){
    tbl <- tbl[tbl$origName %in% o,]
    listen$outFiles <- NULL 
  }else{
    c<-d[d$internal == FALSE | d$internal == i,]$class
    tbl<-tbl[tbl$class %in% c,]
    t<-switch(t,
      vector=c('vector','shape'),
      raster=c('raster'),
      table=c('table'),
      list=c('list'),
      all=c('vector','raster','table','shape','list') 
      ) 
    tbl<-amDataSubset(pattern=f,type=t,tbl)  
    for(i in a){
      tbl=tbl[grep(i,tbl$tags),]
    }
  }


  if(nrow(tbl)>0){ 
    tbl$select=FALSE
    return(tbl)
  }else{
    return(data.frame(NULL))
  }
})




# display data set table in handson table
output$dataListTable<-renderHotable({
  tbl<-dataListTable()
  if(length(tbl)>0){
    tbl<-tbl[c('class','origName','select','type','displayClass','tags')]
  }else{
    tbl<-data.frame(FALSE,"-","-","-","-","-")
  }
  tbl
}
  , stretch='last'
  , readOnly=c(1,2,4,5)
  , hide=c(1,2)
  , columnHeaders=c('class','origName','Select','Type','Class','Tags')
  )


# rename layers based on selected rows in input table of datasets
observeEvent(input$btnUpdateName,{
  amErrorAction(title='Data list rename',{
    cols <- c('class','origName','select','type','displayClass','tags')


    # table updated
    tblU<-hotToDf(
      input$dataListTable,
      colNames=cols
      ) 
    # table original
    tblO<-dataListTable()[,cols]
      # update tags for each row, change filename. Return if something has changeed.
      hasChanged <- amUpdateDataListName(
        dataListOrig=tblO,
        dataListUpdate=tblU,
        dbCon=grassSession$dbCon,
        config=config
        )
      if(hasChanged){
        amUpdateDataList(listen)
      }
          })
})





# table of data set selected, merged with dataListTable.
# NOTE: take dependencies on both : handson table OR dataListTable().
dataListTableSelected<-reactive({
  tbl = data.frame()
  amErrorAction(title='Dataset table subset',{ 
    tblHot <- hotToDf(
      input$dataListTable,
      colNames=c('class','origName','select','type','displayClass','tags')
      )  
    if('select' %in% names(tblHot)){
      tbl=tblHot[tblHot$select,]
    }
    })
    return(tbl)
})




# if no data is selected, disable "createArchive" and "delDataSelect" button.
observe({
  tbl=dataListTableSelected()

  if(isTRUE(is.null(tbl)) | isTRUE(nrow(tbl)<1) | !isTRUE(any(tbl$select)) ){
    disBtn=TRUE
  }else{
    disBtn=FALSE
  }
  amActionButtonToggle('createArchive',session, disable=disBtn)
})


observe({
  tbl <- dataListTableSelected()
  msgDel <- ""
  disBtn <- TRUE
  hasDem <- TRUE

  if(isTRUE(is.null(tbl)) | isTRUE(nrow(tbl)<1) | !isTRUE(any(tbl$select))){
    disBtn <- TRUE
  }else{
    disBtn <- FALSE
    hasDem <- isTRUE(grep(tbl$class,pattern=config$mapDem)>0)
    if(hasDem){
      msgDel <- "DEM (Digital elevation model) can not be deleted, please unselect it."
      disBtn <- TRUE
    }
  }

  amActionButtonToggle('delDataSelect',session, disable=disBtn)
  amUpdateText(id="txtDelMessage",text=msgDel)
})





# if no archive is selected, disable "getArchive" button.
observe({
  selArchive<-input$selArchive
  amActionButtonToggle('getArchive',session,disable=is.null(selArchive)||selArchive=="")
})


# if get archive btn is pressed, lauch amGetData fucntion
observeEvent(input$getArchive,{
  selArchive<-input$selArchive
  amErrorAction(title='Module data: get archive', {
    if(isTRUE(!is.null(selArchive)) && isTRUE(!selArchive=="")){
      amMsg(session,type="log",text=paste('Manage data: archive',selArchive,"requested for download."))
      # archiveBaseName= base url accessible from client side.
      archivePath<-file.path(config$archiveBaseName,selArchive)
      amGetData(session, archivePath)
    }else{
      amMsg(session,type='log',text='Nothing to download')
    }
          })
})



#if create archive is requested, get data names, export them and create archive.
# for each data a dataDir will be created, listed in listDirs.
# TODO: make a function with this
observeEvent(input$createArchive,{
  archivePath<-system(paste("echo",config$pathArchiveGrass),intern=T)
  dbCon <- grassSession$dbCon
  pathShapes<-grassSession$pathShapes
  amErrorAction(title='Module data: create archive',{
      if(isTRUE(file.exists(archivePath) && "SQLiteConnection" %in% class(dbCon))){
        amActionButtonToggle('createArchive',session,disable=TRUE)
       
        pBarTitle <- "Archive creation"

    
        progressBarControl(
          visible=TRUE,
          percent=1,
          title=pBarTitle,
          text="archive start ..")
        
        
        tData<-dataListTableSelected()
        tData<-tData[c('origName','type')]
        tData[]<-lapply(tData, as.character)
        tmpDataDir <- tempdir()
        tmpDataDir <- file.path(tmpDataDir,amRandomName())
        mkdirs(tmpDataDir)
        listDataDirs<-c() #empty dataDir container      
        wdOrig<-getwd()
        tDataL<-nrow(tData)
        inc=1/(tDataL+1)*100 # increment for progressbar. +1 for zip
        for(i in 1:tDataL){

          # dataName conversion for file output

      
          dataName<-tData[i,'origName']
    dataNameOut <- amGetNameConvertExport(
            name = dataName,
            language="en"
            )



          type<-tData[i,'type']
          dataDir<-file.path(tmpDataDir,dataNameOut)
          if(dir.exists(dataDir)){
            removeDirectory(dataDir,recursive=T)
          }
          dir.create(dataDir,showWarnings=F)
          amMsg(session,type='log',text=paste("export",type,dataNameOut),title="Export")
          switch(type,
            'vector'={
              amExportData(
                dataName,
                dataNameOut,
                dataDir,
                type=type
                )
            },
            'raster'={
              amExportData(
                dataName,
                dataNameOut,
                dataDir,
                type=type
                )   
            },
            'table'={
              amExportData(
                dataName,
                dataNameOut,
                dataDir,
                type=type,
                dbCon=dbCon
                )
            },
            'shape'={
              amExportData(
                dataName,
                dataNameOut,
                dataDir,
                type=type
                )
            },
            'list'={
              amExportData(
                dataName,
                dataNameOut,
                dataDir,
                type=type
                )
            }
            )
          

          # progress bar handling
          m <- ""
          if(i==tDataL){
            m <- "Finished. Create archive..."
          }else{
            m <- paste("(",i,"on",tDataL,"exported)")
          }
          
          expStatus <- paste( dataNameOut, "Exported. ", m)
          
          pbc(
            visible=TRUE,
            percent=i*inc,
            title=pBarTitle,
            text=expStatus
            )
          listDataDirs <- c( listDataDirs, dataDir ) 
        }
       # file path setting 
        dateStamp <- getClientDateStamp() 
        archiveName<-file.path(archivePath,paste0('am5_',dateStamp,'.zip'))
        setwd(tmpDataDir)
        zip(archiveName,files = basename(listDataDirs))#files = all directories.
        unlink(listDataDirs,recursive=T)
        setwd(wdOrig) 
        amUpdateDataList(listen)
        amMsg(session,type="log",text=paste('Module manage: archive created:',basename(archiveName)))

        # progress bar finish
        pbc(
          visible=TRUE,
          percent=(i+1)*inc,
          title=pBarTitle,
          text="Finished"
          )
        # hide progress bar
        pbc(
            visible=FALSE,
            percent=0,
            title="",
            text=""
            )
      }
    })
})


