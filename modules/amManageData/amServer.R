#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# Module manage_data :
# -upload new data
# -browse exisiting dat
# -preview data (not yet) 
# -delete data
idModule =  "module_data"


#
# When an analisis end, make a selection with those files and hide filters
#
observe({
  #
  # Listen to analysis output
  #
  hasFiles <- !amNoDataCheck(listen$outFiles)

  #
  # Clean 
  #
  amCleanGrassTemp()

  #
  # Change ui
  #

  updateCheckboxInput(session,"checkShowLastOutputButton",value = hasFiles)
  if(!hasFiles){
    updateCheckboxInput(session,"checkFilterLastOutput",value = FALSE)
    listen$updateDataListTable <- runif(1)
  }

  amDebugMsg(idModule,'listen$outFiles')

},suspended = TRUE) %>% amStoreObs(idModule,"data_list_hide_filters")

#
# If the "display all" btn is pressed, remove the conditional ui 
#
observeEvent(input$checkFilterLastOutput,{
  amErrorAction(title = "Filter data: last output",{

    outFiles <- listen$outFiles
    tbl <- dataList$df
    hasOutFiles <- !amNoDataCheck(outFiles)
    isEnabled <- isTRUE(input$checkFilterLastOutput) 
    if( hasOutFiles && isEnabled ){

      tbl <- tbl[tbl$origName %in% outFiles,]
      tbl$select <- TRUE
      listen$dataListTable <- tbl

    }else{
      listen$updateDataListTable <- runif(1)
    }

    amDebugMsg(idModule,'checkFilterLastOutput')
})
},suspended = TRUE) %>% amStoreObs(idModule, "data_list_filter_last_output")



observe({
  tbl <- dataList$df
  if(is.null(tbl) || row(tbl)==0) return(NULL)

  amDebugMsg(idModule,'Build data list table')

  #
  # Trigger update
  #
  update <- listen$updateDataListTable

  #
  # Input
  #
  filtData <- input$filtData
  filtDataTags <- input$filtDataTags
  typeChoice <- input$typeDataChoice
  internal <- input$internalDataChoice

  #
  # Settings
  # 

  classes <- config$dataClass
  classes <- classes[classes$internal == FALSE | classes$internal == internal,]$class


  isolate({
    # Don't take dependencies on the out file directly in a reactive table, as we set it to NULL later
    oldTable <- dataListTableSelected()
  })

  tbl <- tbl[tbl$class %in% classes,]

  typeChoice <- switch(typeChoice,
    vector = c('vector','shape'),
    raster = c('raster'),
    table = c('table'),
    list = c('list'),
    all = c('vector','raster','table','shape','list') 
    ) 



  tbl <- amDataSubset(
    pattern = filtData,
    type = typeChoice,
    tbl
    )

  for( tag in filtDataTags ){
    tbl <- tbl[grep(tag,tbl$tags),]
  }

  if(nrow(tbl)>0){
    if(!amNoDataCheck(oldTable)){
      tbl$select <- tbl$origName %in% oldTable$origName
    }else{
      tbl$select <- FALSE
    }

    listen$dataListTable <- tbl
  }else{
    listen$dataListTable <- data.frame(NULL)
  }

},suspended = TRUE) %>% amStoreObs(idModule, "data_list_filter_normal")


#
# create reactive data list table with subset by text filter.
#
dataListTable <- reactive({
  listen$dataListTable
})


# table of data set selected, merged with dataListTable.
# NOTE: take dependencies on both : handson table OR dataListTable().
dataListTableSelected <- reactive({
  tbl = data.frame()
  amErrorAction(title = 'Dataset table subset',{ 
    tblHot <- hotToDf(
      input$dataListTable,
      colNames = c('class','origName','select','type','displayClass','tags')
      )  
    if('select' %in% names(tblHot)){
      #
      # filter values selected
      #
      tbl = tblHot[tblHot$select,]
    }
    })
  return(tbl)
})




# display data set table in handson table
output$dataListTable <- renderHotable({

  amDebugMsg(idModule,'Render data list table')

  tbl <- dataListTable()
  if(length(tbl)>0){
    tbl <- tbl[c('class','origName','select','type','displayClass','tags')]
  }else{
    tbl <- data.frame("-","-","-","-","-","-")
  }
  tbl
}
  , stretch = 'last'
  , readOnly = c(1,2,4,5)
  , hide = c(1,2)
  , columnHeaders = c('class','origName','Select','Type','Class','Tags')
  , toolsConditionalColumn = list(
    column = "Select",
    valueSet = TRUE,
    valueUnset = FALSE,
    columnSelectInput = c('type','displayClass','tags')
    )
  )


# Update selection of available data class to upload
observe({

  language <- listen$language 
  dAll <- list()

  dc <- config$dataClass[
    config$dataClass$importable,
    c(language,'class','type')
    ]

  for(i in c("raster","table","vector")){
    ds <- dc[dc$type==i,]
    val <- ds$class
    names(val) <-  paste0("(",substr(i,0,1),") ",ds[,language])
    dAll[[i]] <- val
  }

  updateSelectInput(
    session = session,
    inputId = 'dataClass',
    choices = dAll
    )

},suspended = TRUE) %>% amStoreObs(idModule,"update_data_class")

# Update available archives
observe({
  archiveList <- dataList$archive
  if(is.null(archiveList))archiveList = ""
  updateSelectInput(
    session = session,
    inputId = "selArchive",
    choices = archiveList,
    selected = archiveList[1]
    )
},suspended = TRUE) %>% amStoreObs(idModule,"update_archive_list")


observe({
  tagsList <- dataList$tags
  if(is.null(tagsList))tagsList = ""
  selectTags <- c()

  isolate({
    selectTags <- listen$lastComputedTags
  })

  updateSelectInput(
    session = session,
    inputId = 'filtDataTags',
    choices = tagsList,
    selected = selectTags
    )

},suspended = TRUE) %>% amStoreObs(idModule,"update_tag_filter")


# validate choice based on class and tags select and  populate dataMetaList
observe({
  amErrorAction( title = "Data panel validation",{
    # init
    tagMinChar <- 1
    msgList <- list()#empty list. return null if no msg.
    dInfo = NULL
    err = character(0)
    info = character(0)

    # input import
    dTag <- input$dataTag# reevaluate if tags changes
    dClass <- input$dataClass # reevaluate if class changes
    sepTagFile = config$sepTagFile 
    isDem  <-  isTRUE(dClass == amGetClass(config$mapDem))




    # name validation process
    if(isDem){ 
      info <- c(info,
        ams(
          id = "srv_data_import_new_dem_warning"
          )
        )
      info <- c(info,
        ams(
          id = "srv_data_automatic_name"
          )
        )
      # populate meta data list
      dName <- strsplit(config$mapDem,"@")[[1]][[1]]
      dInfo <- list(
        name = dName,
        type = "raster",
        class = amGetClass(dName),
        tags = "dem",
        accepts = config$filesAccept[['raster']],
        multiple = config$fileAcceptMultiple[['raster']]
        )
    }else{
      if(!is.null(dClass) && !dClass=="" && (!is.null(dTag) && !dTag=="")){
        # get unique and ordered tags
        dTag <- amSubPunct(dTag,sepTagFile,rmTrailingSep = T,rmLeadingSep = T)
        dTagDisplay <- amSubPunct(dTag)
        # get registered type for this class
        dType <- config$dataClass[config$dataClass$class==dClass,'type']
        # formated data name
        dName <- amNewName(dClass,dTag,config$sepClass,config$sepTagFile)
        dNameDisplay <- paste0(amClassListInfo(dClass)," [",paste0(dTagDisplay,collapse = ','),"]")

        tagsTooShort <- nchar(dTag)<tagMinChar
        dataExists <- paste0(dName,config$sepMapset,grassSession$mapset) %in% isolate(dataList)[[dType]]

        if(dType == "raster") info <- c(info,
          ams(
            id = "srv_data_verify_projection_resolution_warning"
            )
          )

        if(tagsTooShort) err <- c(err,
          ams(
            id = "srv_data_short_missing_tag_notice"
            )
          )
        if(dataExists) err <- c(err,
          sprintf(
            ams(
              id = "srv_data_name_exists_change_tag"
              ),
            dNameDisplay
            )
          )
        # removed as required by Steeve.
        if(!dataExists) info <- c(info,paste(dNameDisplay," available."))

        if((! tagsTooShort && !dataExists)){
          # populate meta data list
          dInfo <- list(
            name = dName,
            type = dType,
            class = dClass,
            tags = dTag,
            accepts = config$filesAccept[[dType]],
            multiple = config$fileAcceptMultiple[[dType]]
            )
        }
      }else{
        err <- c(err,
          ams(
            id = "srv_data_enter_missing_info_notice"
            )
          )
      }
    }




    # create HTML for validation message list.
    if(length(err)>0){
      err <- tags$ul(
        HTML(
          paste(
            "<li>",
            icon('exclamation-triangle'),
            err,
            "</li>",
            collapse = ""
            )
          )
        )
      disBtn = TRUE
    }else{
      disBtn = FALSE
    }
    if(length(info)>0) info <- tags$ul(
      HTML(
        paste(
          "<li>",
          icon('info-circle'),
          info,
          "</li>",
          collapse = ""
          )
        )
      )

    # send result to ui
    if(length(err)>0 || length(info)>0){
      msgList <- tagList(tags$b(
          ams(
            id = "srv_data_validation_information_notice"
            )),
        err,
        info
        )
    }else{
      msgList <- "" # tagList(tags$b(paste('This message is not supposed to be empty.')))
    }

    output$msgModuleData <-renderUI({msgList})
    amActionButtonToggle(session = session,'btnDataNew',disable = disBtn)
    # update the file input part
    if(!disBtn){
      amFileInputUpdate('btnDataNew',session, accepts = dInfo$accepts,multiple = dInfo$multiple)
    }
    # save in reactive object for upload function
    listen$newDataMeta <- dInfo
    })
},suspended = TRUE) %>% amStoreObs(idModule,"validation_new_data")




# upload a dataset 
observeEvent(input$btnDataNew,{

  amErrorAction(
    title = 'Module data : importation',
    {
      dNew <- input$btnDataNew # take reactivity on btnDataNew only.
      dMeta <- listen$newDataMeta
      tryReproj <- TRUE # auto reprojection  ?
      if(!is.null(dNew) && !is.null(dMeta)){

        # init progressBar
        pBarTitle <- ams(
          id = "srv_data_importation_notice"
          )
        progressBarControl(
          visible = TRUE,
          percent = 0,
          title = pBarTitle,
          text = ams(
            id = "srv_data_analysis_progress_notice"
            )
          )


        updateTextInput(
          session,
          'dataTag',
          value = ''
          )
        # extract arg from list
        dType <- dMeta$type
        dName <- dMeta$name
        dClass <- dMeta$class
        if (dClass == "dem") {
          dName = amNoMapset(config$mapDem)
        }

        # get the temp dir
        dDir <- dirname(dNew$datapath[1])
        # rename file. Instead of fileinput default, set original name :
        # e.g. road.shp instead of "3"
        dNew$newPath <- file.path(dDir,dNew$name)
        file.rename(dNew$datapath,dNew$newPath)
        stopifnot(file.exists(dNew$newPath)) 
        # if multiple data (shp, adf...), set a directory as data source.
        if(nrow(dNew)==1){
          dInput <- dNew$newPath
          dFiles <- dInput
        }else{
          dInput <- dDir
          dFiles <- list.files(dInput,full.names = T)
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
        out <- switch(dType,
          "raster" = amUploadRaster(
            config,
            dataInput = dInput,
            dataName = dName,
            dataFiles = dFiles,
            dataClass = dClass,
            pBarTitle = ams(
              id = "srv_data_upload_raster_notice"
              )
            ),
          "vector" = amUploadVector(
            dataInput = dInput,
            dataName = dName,
            dataFiles = dFiles,
            pBarTitle = ams(
              id = "srv_data_upload_vector_notice"
              )
            ),
          "table" = amUploadTable(
            config,
            dataName = dName,
            dataFile = dFiles,
            dataClass = dClass,
            dbCon = grassSession$dbCon,
            pBarTitle = pBarTitle
            )
          )

        amUpdateDataList(listen)

        progressBarControl(
          visible = TRUE,
          percent = 100,
          title = pBarTitle,
          text = ams(
            id = "srv_data_process_end_notice"
            )
          )

        # if no error intercepted by tryCatch:invalidate metadata, log message and remove tags.
        listen$newDataMeta <- NULL
        amMsg(session,
          type = "log",
          text = sprintf(
            ams("srv_data_imported_project_notice"),
            dName
            )
          )


        if(isTRUE(dType == "raster")){

          hasResolutionIssue <- isTRUE(
            round(out$projectBefore$resolution$x) != 
            round( out$data$resolution$x ) || 
            round(out$projectBefore$resolution$y) != 
            round(out$data$resolution$y)
          )

          ui = tags$div(class = "panel panel-default",
            tags$div(class = "panel-heading",""),
            tags$table(
              class = "table table-condensed",
              tags$thead(
                tags$tr(
                  tags$th(""),
                  tags$th(HTML("Resolution&nbsp;(&nbsp;m&nbsp;)")),
                  tags$th(HTML("Projection&nbsp;(&nbsp;proj 4&nbsp;)"))
                  )
                ),
              tags$tbody(
                tags$tr(
                  tags$td(
                    ams( "srv_data_project_before_importation")
                    ),
                  tags$td(paste(
                      round(out$projectBefore$resolution$x,4)
                      ,"x"
                      , round(out$projectBefore$resolution$y,4)
                      )),
                  tags$td(out$projectBefore$projection)
                  ),
                tags$tr(
                  tags$td(
                    ams("srv_data_project_after_importation")
                    ),
                  tags$td(paste(
                      round(out$projectAfter$resolution$x,4)
                      ,"x"
                      , round(out$projectAfter$resolution$y,4)
                      )),
                  tags$td(out$projectAfter$projection)
                  ),
                tags$tr(
                  class = ifelse(hasResolutionIssue,'danger',''),
                  tags$td(
                    ams("srv_data_imported_dataset")
                    ),
                  tags$td(paste(
                      round(out$data$resolution$x,4)
                      ,"x"
                      , round(out$data$resolution$y,4)
                      )),
                  tags$td(out$data$projection)
                  )
                )
              ),
            tags$div(
              class = "panel-body",
              tags$p(
                if(hasResolutionIssue){
                  tags$span(
                    icon('warning'),
                    tags$b(
                      ams("srv_data_has_resolution_issue_warning")
                      )
                    )
                }
                ),
              tags$p(
                tags$b(
                  ams("srv_data_null_values")
                  ),
                tags$span(
                  ams("srv_data_number_null_cells_found")
                  ), 
                tags$b(out$data$numberOfNulls)
                ),
              tags$p(
                tags$b(
                  ams("srv_data_note_notice")
                  ),
                tags$span(
                  ams("srv_data_table_control_after_importation_notice")
                  )
                )
              )
            )

          amMsg(
            type = "ui",
            title = sprintf(
              ams(
                id = "srv_data_projection_resolution_notice"
                )
              ),
            subtitle = ams(
              id = "srv_data_summary_sub"
              ),
            text = ui
            )

        }

        progressBarControl(
          visible = FALSE,
          percent = 0,
          title = "",
          text = "")

      }
    }) 
},suspended = TRUE) %>% amStoreObs(idModule, "btn_add_data")





#
#observeEvent(input$btnRmSelected,{
#  tbl <- dataListTableSelected()
#
#})
#
observeEvent(input$delDataSelect,{
  amErrorAction(title = "Module data: data deletion confirmation",{
    tbl <- dataListTableSelected()
    nItems <- nrow(tbl)

    if(nItems >1 ){
      txtHead<-tags$span(
        sprintf(
          ams(
            id = "srv_data_items_to_delete_warning"
            ),
          nItems
          )
        )
    }else{ 
      txtHead<-tags$span(
        ams(
          id = "srv_data_one_item_to_delete_warning"
          )
        )
    }

    listData <- tags$div(style = "max-height:300px;overflow-y:scroll;",
      tags$ul(
        HTML(paste(
            "<li><b>",
            toupper(tbl$type),
            ": </b>",
            tbl$displayClass,
            "[",
            tbl$tags,
            "] </li>"
            ))
        )
      )
    content <- tagList(
      txtHead,
      listData
      )
    aBtns = list(
      actionButton('delDataSelectConfirm', ams('data_delete_data'))
      )
    amUpdateModal(
      panelId = "amModal",
      title = ams(
        id = "srv_data_modal_confirmation_1"
        ),
      html = content,
      listActionButton = aBtns,
      addCancelButton = TRUE,
      cancelButtonText = ams('data_delete_cancel')
      )
    })
},suspended = TRUE) %>% amStoreObs(idModule,"del_delete_data")



# Delete selected dataset
observeEvent(input$delDataSelectConfirm,{
  amUpdateModal("amModal", close = TRUE) 
  tbl <- isolate(dataListTableSelected())
  rastName <- as.character(tbl[tbl$type=='raster','origName'])
  rastName <- rastName[!rastName %in% 'dem'] # do not allow removing DEM
  vectName <- as.character(tbl[tbl$type=='vector','origName'])
  tableName <- as.character(tbl[tbl$type=='table','origName'])
  shapeName <- as.character(tbl[tbl$type=='shape','origName'])
  listName <- as.character(tbl[tbl$type=='list','origName'])
  if(length(rastName)>0){
    amMsg(session,
      type = "log",
      text = sprintf(
        ams(
          id = "srv_data_manage_remove_raster_selected"
          ),
        paste(rastName, collapse = '; ')
        )
      )
    rmRastIfExists(rastName)
  }
  if(length(vectName)>0){
    amMsg(session,
      type = "log",
      text = sprintf(
        ams(
          id = "srv_data_manage_remove_vector_selected"
          ),
        paste(vectName, collapse = '; ')
        )
      )
    rmVectIfExists(vectName)
  }
  if(length(tableName)>0){
    dbCon <- isolate(grassSession$dbCon)
    #sqlexpr <- paste("DROP TABLE IF EXISTS",tableName,";",collapse = "")
    #dbGetQuery(dbCon,sqlexpr) NOTE:doesn't work, and doesn't return a message...
    for(t in tableName){
      dbGetQuery(dbCon, paste("DROP TABLE IF EXISTS", t))
    }
  }
  if(length(shapeName)>0){
    for(i in shapeName){
      allShpFiles <- amGetShapesList(pattern = sprintf('^%s\\.',i))
      for( shpP in allShpFiles){
        file.remove(shpP) 
      }
    }
  }
  if(length(listName)>0){
    for(i in listName){
      allListFiles<- amGetListsList(pattern = sprintf('^%s\\.',i))
      for( lFile in allListFiles ){
        file.remove(lFile) 
      }
    }
  }
  updateTextInput(session,
    'filtData',
    value = ''
    )
  updateSelectInput(session,
    'filtDataTags',
    selected = ''
    )
  amUpdateDataList(listen)

},suspended = TRUE) %>% amStoreObs(idModule, "del_delete_data_confirm")


# rename layers based on selected rows in input table of datasets
observeEvent(input$btnUpdateName,{
  amErrorAction(title = 'Data list rename',{
    cols <- c('class','origName','select','type','displayClass','tags')


    # table updated
    tblU<-hotToDf(
      input$dataListTable,
      colNames = cols
      ) 
    # table original
    tblO<-dataListTable()[,cols]
    # update tags for each row, change filename. Return if something has changeed.
    hasChanged <- amUpdateDataListName(
      dataListOrig = tblO,
      dataListUpdate = tblU,
      dbCon = grassSession$dbCon,
      config = config
      )
    if(hasChanged){
      amUpdateDataList(listen)
    }
    })

},suspended = TRUE) %>% amStoreObs(idModule, "btn_update_name")



# if no data is selected, disable "createArchive" and "delDataSelect" button.
observe({
  tbl = dataListTableSelected()

  hasTrue <- isTRUE(any(sapply(tbl$select,isTRUE)))
  isNotNull <- isTRUE(!is.null(tbl))
  hasRows <- isTRUE(nrow(tbl) > 0)

  if( hasTrue && isNotNull && hasRows ){
    disBtn = FALSE
  }else{
    disBtn = TRUE
  }

  amActionButtonToggle(
    'createArchive',
    session,
    disable = disBtn
    )

}, suspended = TRUE) %>% amStoreObs(idModule, "toggle_btn_create_archive")


observe({
  tbl <- dataListTableSelected()
  msgDel <- ""
  disBtn <- TRUE
  hasDem <- TRUE

  if(isTRUE(is.null(tbl)) | isTRUE(nrow(tbl)<1) | !isTRUE(any(tbl$select))){
    disBtn <- TRUE
  }else{
    disBtn <- FALSE
    hasDem <- isTRUE(grep(tbl$class,pattern = config$mapDem)>0)
    if(hasDem){
      msgDel <- ams(
        id = "srv_data_dem_not_deletable_notice"
        )
      disBtn <- TRUE
    }
  }

  amActionButtonToggle(
    'delDataSelect',
    session,
    disable = disBtn
    )
  amUpdateText(id = "txtDelMessage", text = msgDel)
}, suspended = TRUE) %>% amStoreObs(idModule, "toggle_btn_delete")


# if no archive is selected, disable "getArchive" button.
observe({
  selArchive <- input$selArchive
  amActionButtonToggle(
    'getArchive',
    session,
    disable = is.null(selArchive)||selArchive==""
    )
  amActionButtonToggle(
    'btnDeleteArchive',
    session,
    disable = is.null(selArchive)||selArchive==""
    )
}, suspended = TRUE) %>% amStoreObs(idModule,"toggle_btn_sel_archive")


# if get archive btn is pressed, lauch amGetData fucntion
observeEvent(input$getArchive,{
  selArchive <- input$selArchive
  amErrorAction(title = 'Module data: get archive', {
    if(isTRUE(!is.null(selArchive)) && isTRUE(!selArchive=="")){
      amMsg(session,
        type = "log",
        text = sprintf(
          ams(
            id = "srv_data_archive_requested_download"
            ),
          selArchive
          )
        )
      # archiveBaseName =  base url accessible from client side.
      archivePath <- file.path(config$archiveBaseName,selArchive)
      amGetData(session, archivePath)
    }else{
      amMsg(session,
        type = 'log',
        text = ams(
          id = "srv_data_nothing_to_download_notice"
          )
        )
    }
    })
}, suspended = TRUE) %>% amStoreObs(idModule, "btn_get_data")


observeEvent(input$btnDeleteArchive,{
  amErrorAction(title = "Module data: archive deletion confirmation",{

    selArchive <- input$selArchive
    txtConfirm <- tags$span(
      sprintf(
        ams(
          id = "srv_data_deletion_confirmation_warning"
          ),
        selArchive
        )
      )

    aBtns = list(
      actionButton('btnDeleteArchiveConfirm',"Delete")
      )

    amUpdateModal(
      panelId = "amModal",
      title = ams(
        id = "srv_data_modal_confirmation_2"
        ),
      html = txtConfirm,
      listActionButton = aBtns,
      addCancelButton = TRUE
      )
    })

}, suspended = TRUE) %>% amStoreObs(idModule, "btn_delete_archive")

#
# Delete select archive 
#
observeEvent(input$btnDeleteArchiveConfirm,{

  amUpdateModal("amModal", close = TRUE) 

  archivePath <- system(paste("echo", config$pathArchiveGrass), intern = T)
  selArchive <- input$selArchive
  archiveFilePath <- file.path(archivePath, selArchive)

  if(file.exists(archiveFilePath)){
    file.remove(archiveFilePath)
    amUpdateDataList(listen)
  }

},suspended = TRUE) %>% amStoreObs(idModule, "btn_delete_archive_confirm")


#if create archive is requested, get data names, export them and create archive.
# for each data a dataDir will be created, listed in listDirs.
# TODO: make a function with this
observeEvent(input$createArchive,{
  archivePath <- system(paste("echo",
      config$pathArchiveGrass
      ),
    intern = T)
  dbCon <- grassSession$dbCon
  pathShapes <- grassSession$pathShapes
  amErrorAction(title = 'Module data: create archive',{
    if(isTRUE(file.exists(archivePath) && "SQLiteConnection" %in% class(dbCon))){
      amActionButtonToggle('createArchive', session, disable = TRUE)

      pBarTitle <- ams("srv_data_archive_creation_notice")

      customArchiveName <- input$txtArchiveName

      progressBarControl(
        visible = TRUE,
        percent = 1,
        title = pBarTitle,
        text = ams("srv_data_initialization_progress_notice")
      )


      tData <- dataListTableSelected()
      tData <- tData[c('origName','type')]
      tData[] <- lapply(tData, as.character)
      tmpDataDir <- tempdir()
      tmpDataDir <- file.path(tmpDataDir,amRandomName())
      mkdirs(tmpDataDir)
      listDataDirs <- c() #empty dataDir container      
      wdOrig <- getwd()
      tDataL <- nrow(tData)
      inc = 1/(tDataL+1)*100 # increment for progressbar. +1 for zip
      #rasterDataType <- input$selRasterDataType
      for(i in 1:tDataL){

        # dataName conversion for file output
        dataName <- tData[i,'origName']
        dataNameOut <- amGetNameConvertExport(
          name = dataName,
          language = listen$language
          )

        type <- tData[i,'type']
        dataDir <- file.path(tmpDataDir,dataNameOut)
        if(dir.exists(dataDir)){
          removeDirectory(dataDir,recursive = T)
        }
        dir.create(dataDir,showWarnings = F)
        
        amMsg(
          session,
          type = 'log',
          text = sprintf(
            ams(
              id = "srv_data_export_naming"
              ),
            type,
            dataNameOut),
          title = "Export"
        )

        #
        # Generic export data to folder
        #
        amExportData(
          dataName = dataName,
          dataNameOut = dataNameOut,
          exportDir = dataDir,
          type = type,
          dbCon = dbCon
        )

        # progress bar handling
        m <- ""
        if( i == tDataL ){
          m <- ams("srv_data_process_finished_create_archive")
        }else{
          m <- sprintf(
            ams( "srv_data_exported_file_notice"),
            i,
            tDataL
          )
        }

        expStatus <- sprintf(
          ams( "srv_data_"),
          dataNameOut,
          m
          )

        pbc(
          visible = TRUE,
          percent = i*inc,
          title = pBarTitle,
          text = expStatus
          )

        listDataDirs <- c( listDataDirs, dataDir ) 
      }
      
      #
      # File path setting 
      #
      dateStamp <- getClientDateStamp() %>%
        format("%Y_%m_%d@%H:%M")
    
      customArchiveName <- amSubPunct(customArchiveName)

      if(amNoDataCheck(customArchiveName)){
        customArchiveName <- 'am5_export'
      }
      archiveName <- sprintf('%1$s_%2$s.zip',customArchiveName,dateStamp)
      archiveFilePath <- file.path(archivePath,archiveName)

      amDebugMsg(list(
          archiveName = archiveName,
          archiveFilePath = archiveFilePath,
          tmpDataDir = tmpDataDir
          ))

      setwd(tmpDataDir)
      on.exit(setwd(wdOrig))
      zip(
        archiveFilePath,
        files = basename(listDataDirs)
      ) #files = all directories.
      unlink(listDataDirs, recursive = T)
      setwd(wdOrig) 
      amUpdateDataList(listen)
      amMsg(session,
        type = "log",
        text = sprintf(
          ams(
            id = "srv_data_archive_creation_path"
            ),
          basename(archiveFilePath
          )
        )
      )

      # progress bar finish
      pbc(
        visible = TRUE,
        percent = (i+1)*inc,
        title = pBarTitle,
        text = ams(
          id = "srv_data_progress_process_finished"
          )
        )
      # hide progress bar
      pbc(
        visible = FALSE,
        percent = 0,
        title = "",
        text = ""
        )
    }
    })
}, suspended = TRUE) %>% amStoreObs(idModule, "btn_create_archive")


