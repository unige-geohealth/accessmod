#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

idModule <- "module_data"

observe(
  {
    hasFiles <- isNotEmpty(listen$outFiles)
    amCleanGrassTemp()
    updateCheckboxInput(session, "checkShowLastOutputButton", value = hasFiles)
    if (!hasFiles) {
      updateCheckboxInput(session, "checkFilterLastOutput", value = FALSE)
      listen$updateDataListTable <- runif(1)
    }
    amDebugMsg(idModule, "listen$outFiles")
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "data_list_hide_filters")

observeEvent(input$checkFilterLastOutput,
  {
    amErrorAction(title = "Filter data: last output", {
      outFiles <- listen$outFiles
      tbl <- dataList$df
      hasOutFiles <- isNotEmpty(outFiles)
      isEnabled <- isTRUE(input$checkFilterLastOutput)
      if (hasOutFiles && isEnabled) {
        tbl <- tbl[tbl$origName %in% outFiles, ]
        tbl$am_select <- TRUE
        listen$dataListTable <- tbl
      } else {
        listen$updateDataListTable <- runif(1)
      }

      amDebugMsg(idModule, "checkFilterLastOutput")
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "data_list_filter_last_output")

observe(
  {
    tbl <- dataList$df

    if (isEmpty(tbl)) {
      return(NULL)
    }

    amDebugMsg(idModule, "Build data list table")

    update <- listen$updateDataListTable
    filtData <- input$filtData
    filtDataTags <- input$filtDataTags
    typeChoice <- input$typeDataChoice
    internal <- input$internalDataChoice

    classes <- config$dataClass
    classes <- classes[classes$internal == FALSE | classes$internal == internal, ]$class

    isolate({
      oldTable <- dataListTableSelected()
    })

    tbl <- tbl[tbl$class %in% classes, ]

    typeChoice <- switch(typeChoice,
      vector = c("vector", "shape"),
      raster = c("raster"),
      table = c("table"),
      config = c("config"),
      all = c("vector", "raster", "table", "shape", "config")
    )

    tbl <- amDataSubset(
      pattern = filtData,
      type = typeChoice,
      tbl
    )

    for (tag in filtDataTags) {
      tbl <- tbl[grep(tag, tbl$tags), ]
    }

    if (nrow(tbl) > 0) {
      if (isNotEmpty(oldTable)) {
        tbl$am_select <- tbl$origName %in% oldTable$origName
      } else {
        tbl$am_select <- FALSE
      }

      listen$dataListTable <- tbl
    } else {
      listen$dataListTable <- data.frame(NULL)
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "data_list_filter_normal")

dataListTable <- reactive({
  tbl <- listen$dataListTable

  if (isEmpty(tbl) || is.null(tbl)) {
    tbl <- data.frame("-", "-", "-", "-", "-", "-", "-", "-")
    names(tbl) <- c(
      "class",
      "tags",
      "type",
      "searchCol",
      "origName",
      "displayName",
      "displayClass",
      "am_select"
    )
  }
  tbl
})

dataListTableSelected <- reactive({
  tbl <- data.frame()
  amErrorAction(title = "Dataset table subset", {
    tbl_select <- tabulator_to_df(input$dataListTable_data)
    if ("am_select" %in% names(tbl_select)) {
      tbl <- tbl_select[tbl_select$am_select, ]
    }
  })

  return(tbl)
})

output$dataListTable <- render_tabulator({
  tbl <- dataListTable()

  tabulator(
    data = tbl,
    add_selector_bar = TRUE,
    add_select_column = TRUE,
    return_select_column_name = "am_select",
    return_select_column = TRUE,
    stretched = "last",
    columnHeaders = c("_", "Tags", "Type", "_", "_", "_", "Class", "_"),
    readOnly = c("type", "displayClass"),
    hide = c("class", "searchCol", "origName", "displayName", "am_select"),
    # columnOrder = c("type", "class", "tags"),
    options = list(
      index = "cat",
      columnDefaults = list(
        resizable = FALSE
      )
    )
  )
})

observe(
  {
    language <- listen$language
    dAll <- list()

    dc <- config$dataClass[
      config$dataClass$importable,
      c(language, "class", "type")
    ]

    for (i in c("raster", "table", "vector")) {
      ds <- dc[dc$type == i, ]
      val <- ds$class
      names(val) <- paste0("(", substr(i, 0, 1), ") ", ds[, language])
      dAll[[i]] <- val
    }

    updateSelectInput(
      session = session,
      inputId = "dataClass",
      choices = dAll
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_class")

observe(
  {
    archiveList <- dataList$archive
    if (is.null(archiveList)) archiveList <- ""
    updateSelectInput(
      session = session,
      inputId = "selArchive",
      choices = archiveList,
      selected = archiveList[1]
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_archive_list")

observe(
  {
    tagsList <- dataList$tags
    if (is.null(tagsList)) tagsList <- ""
    selectTags <- c()

    isolate({
      selectTags <- listen$lastComputedTags
    })

    updateSelectInput(
      session = session,
      inputId = "filtDataTags",
      choices = tagsList,
      selected = selectTags
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_tag_filter")

observe(
  {
    amErrorAction(title = "Data panel validation", {
      tagMinChar <- 1
      msgList <- list()
      dInfo <- NULL
      err <- character(0)
      info <- character(0)

      dTag <- input$dataTag
      dClass <- input$dataClass
      sepTagFile <- config$sepTagFile
      isDem <- isTRUE(dClass == amGetClass(config$mapDem))

      if (isDem) {
        info <- c(
          info,
          ams(
            id = "srv_data_import_new_dem_warning"
          )
        )
        info <- c(
          info,
          ams(
            id = "srv_data_automatic_name"
          )
        )
        dName <- strsplit(config$mapDem, "@")[[1]][[1]]
        dInfo <- list(
          name = dName,
          type = "raster",
          class = amGetClass(dName),
          tags = "dem",
          accepts = config$filesAccept[["raster"]],
          multiple = config$fileAcceptMultiple[["raster"]]
        )
      } else {
        if (!is.null(dClass) && !dClass == "" && (!is.null(dTag) && !dTag == "")) {
          dTag <- amSubPunct(dTag, sepTagFile, rmTrailingSep = T, rmLeadingSep = T)
          dTagDisplay <- amSubPunct(dTag)
          dType <- config$dataClass[config$dataClass$class == dClass, "type"]
          dName <- amNewName(dClass, dTag, config$sepClass, config$sepTagFile)
          dNameDisplay <- paste0(amClassListInfo(dClass), " [", paste0(dTagDisplay, collapse = ","), "]")

          tagsTooShort <- nchar(dTag) < tagMinChar
          dataExists <- paste0(dName, config$sepMapset, grassSession$mapset) %in% isolate(dataList)[[dType]]

          if (dType == "raster") {
            info <- c(
              info,
              ams(
                id = "srv_data_verify_projection_resolution_warning"
              )
            )
          }

          if (tagsTooShort) {
            err <- c(
              err,
              ams(
                id = "srv_data_short_missing_tag_notice"
              )
            )
          }
          if (dataExists) {
            err <- c(
              err,
              sprintf(
                ams(
                  id = "srv_data_name_exists_change_tag"
                ),
                dNameDisplay
              )
            )
          }
          if (!dataExists) info <- c(info, paste(dNameDisplay, " available."))

          if ((!tagsTooShort && !dataExists)) {
            dInfo <- list(
              name = dName,
              type = dType,
              class = dClass,
              tags = dTag,
              accepts = config$filesAccept[[dType]],
              multiple = config$fileAcceptMultiple[[dType]]
            )
          }
        } else {
          err <- c(
            err,
            ams(
              id = "srv_data_enter_missing_info_notice"
            )
          )
        }
      }

      if (length(err) > 0) {
        err <- tags$ul(
          HTML(
            paste(
              "<li>",
              icon("exclamation-triangle"),
              err,
              "</li>",
              collapse = ""
            )
          )
        )
        disBtn <- TRUE
      } else {
        disBtn <- FALSE
      }
      if (length(info) > 0) {
        info <- tags$ul(
          HTML(
            paste(
              "<li>",
              icon("info-circle"),
              info,
              "</li>",
              collapse = ""
            )
          )
        )
      }

      if (length(err) > 0 || length(info) > 0) {
        msgList <- tagList(
          tags$b(
            ams(
              id = "srv_data_validation_information_notice"
            )
          ),
          err,
          info
        )
      } else {
        msgList <- ""
      }

      output$msgModuleData <- renderUI({
        msgList
      })
      amActionButtonToggle(session = session, "btnDataNew", disable = disBtn)
      if (!disBtn) {
        amFileInputUpdate("btnDataNew", session, accepts = dInfo$accepts, multiple = dInfo$multiple)
      }
      listen$newDataMeta <- dInfo
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "validation_new_data")

observeEvent(input$btnDataNew,
  {
    amErrorAction(
      title = "Module data : importation",
      {
        dNew <- input$btnDataNew
        dMeta <- listen$newDataMeta
        tryReproj <- TRUE
        if (!is.null(dNew) && !is.null(dMeta)) {
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
            "dataTag",
            value = ""
          )
          dType <- dMeta$type
          dName <- dMeta$name
          dClass <- dMeta$class
          if (dClass == "dem") {
            dName <- amNoMapset(config$mapDem)
          }

          dDir <- dirname(dNew$datapath[1])
          dNew$newPath <- file.path(dDir, dNew$name)
          file.rename(dNew$datapath, dNew$newPath)
          stopifnot(file.exists(dNew$newPath))
          if (nrow(dNew) == 1) {
            dInput <- dNew$newPath
            dFiles <- dInput
          } else {
            dInput <- dDir
            dFiles <- list.files(dInput, full.names = T)
          }
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

          listen$newDataMeta <- NULL
          amMsg(session,
            type = "log",
            text = sprintf(
              ams("srv_data_imported_project_notice"),
              dName
            )
          )

          if (isTRUE(dType == "raster")) {
            ui <- raster_summary_to_ui(out)

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
            text = ""
          )
        }
      }
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_add_data")

observeEvent(input$delDataSelect,
  {
    amErrorAction(title = "Module data: data deletion confirmation", {
      tbl <- dataListTableSelected()

      nItems <- nrow(tbl)

      if (nItems > 1) {
        txtHead <- tags$span(
          sprintf(
            ams(
              id = "srv_data_items_to_delete_warning"
            ),
            nItems
          )
        )
      } else {
        txtHead <- tags$span(
          ams(
            id = "srv_data_one_item_to_delete_warning"
          )
        )
      }

      listData <- tags$div(
        style = "max-height:300px;overflow-y:scroll;",
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
      aBtns <- list(
        actionButton("delDataSelectConfirm", ams("data_delete_data"))
      )
      amUpdateModal(
        panelId = "amModal",
        title = ams(
          id = "srv_data_modal_confirmation_1"
        ),
        html = content,
        listActionButton = aBtns,
        addCancelButton = TRUE,
        cancelButtonText = ams("data_delete_cancel")
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "del_delete_data")

observeEvent(input$delDataSelectConfirm,
  {
    amUpdateModal("amModal", close = TRUE)
    tbl <- isolate(dataListTableSelected())
    rastName <- as.character(tbl[tbl$type == "raster", "origName"])
    rastName <- rastName[!rastName %in% "dem"]
    vectName <- as.character(tbl[tbl$type == "vector", "origName"])
    tableName <- as.character(tbl[tbl$type == "table", "origName"])
    shapeName <- as.character(tbl[tbl$type == "shape", "origName"])
    configName <- as.character(tbl[tbl$type == "config", "origName"])

    if (length(rastName) > 0) {
      amMsg(session,
        type = "log",
        text = sprintf(
          ams(
            id = "srv_data_manage_remove_raster_selected"
          ),
          paste(rastName, collapse = "; ")
        )
      )
      rmRastIfExists(rastName)
    }
    if (length(vectName) > 0) {
      amMsg(session,
        type = "log",
        text = sprintf(
          ams(
            id = "srv_data_manage_remove_vector_selected"
          ),
          paste(vectName, collapse = "; ")
        )
      )
      rmVectIfExists(vectName)
    }
    if (length(tableName) > 0) {
      dbCon <- isolate(grassSession$dbCon)
      for (t in tableName) {
        dbGetQuery(dbCon, paste("DROP TABLE IF EXISTS", t))
      }
    }
    if (length(shapeName) > 0) {
      for (i in shapeName) {
        allShpFiles <- amGetShapesList(pattern = sprintf("^%s\\.", i))
        for (shpP in allShpFiles) {
          file.remove(shpP)
        }
      }
    }

    if (length(configName) > 0) {
      for (name in configName) {
        allConfigFiles <- amGetConfigList(pattern = sprintf("^%s\\.", name))
        for (lFile in allConfigFiles) {
          file.remove(lFile)
        }
      }
    }
    updateTextInput(session,
      "filtData",
      value = ""
    )
    updateSelectInput(session,
      "filtDataTags",
      selected = ""
    )
    amUpdateDataList(listen)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "del_delete_data_confirm")

observeEvent(input$btnUpdateName,
  {
    amErrorAction(title = "Data list rename", {
      tbl_select <- tabulator_to_df(input$dataListTable_data)
      tblO <- dataListTable()[, c("class", "origName", "am_select", "type", "displayClass", "tags")]

      hasChanged <- amUpdateDataListName(
        dataListOrig = tblO,
        dataListUpdate = tbl_select,
        dbCon = grassSession$dbCon,
        config = config
      )
      if (hasChanged) {
        amUpdateDataList(listen)
      }
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_update_name")

observe(
  {
    tbl <- dataListTableSelected()

    hasTrue <- isTRUE(any(sapply(tbl$am_select, isTRUE)))
    isNotNull <- isTRUE(!is.null(tbl))
    hasRows <- isTRUE(nrow(tbl) > 0)

    if (hasTrue && isNotNull && hasRows) {
      disBtn <- FALSE
    } else {
      disBtn <- TRUE
    }

    amActionButtonToggle(
      "createArchive",
      session,
      disable = disBtn
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_btn_create_archive")

observe(
  {
    tbl <- dataListTableSelected()
    msgDel <- ""
    disBtn <- TRUE
    hasDem <- TRUE

    if (isTRUE(is.null(tbl)) | isTRUE(nrow(tbl) < 1) | !isTRUE(any(tbl$am_select))) {
      disBtn <- TRUE
    } else {
      disBtn <- FALSE
      hasDem <- isTRUE(grep(tbl$class, pattern = config$mapDem) > 0)
      if (hasDem) {
        msgDel <- ams(
          id = "srv_data_dem_not_deletable_notice"
        )
        disBtn <- TRUE
      }
    }

    amActionButtonToggle(
      "delDataSelect",
      session,
      disable = disBtn
    )
    amUpdateText(id = "txtDelMessage", text = msgDel)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_btn_delete")

observe(
  {
    selArchive <- input$selArchive
    amActionButtonToggle(
      "getArchive",
      session,
      disable = is.null(selArchive) || selArchive == ""
    )
    amActionButtonToggle(
      "btnDeleteArchive",
      session,
      disable = is.null(selArchive) || selArchive == ""
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_btn_sel_archive")

observeEvent(input$getArchive,
  {
    selArchive <- input$selArchive
    amErrorAction(title = "Module data: get archive", {
      if (isTRUE(!is.null(selArchive)) && isTRUE(!selArchive == "")) {
        amMsg(session,
          type = "log",
          text = sprintf(
            ams(
              id = "srv_data_archive_requested_download"
            ),
            selArchive
          )
        )
        archivePath <- file.path(config$archiveBaseName, selArchive)
        amGetData(session, archivePath)
      } else {
        amMsg(session,
          type = "log",
          text = ams(
            id = "srv_data_nothing_to_download_notice"
          )
        )
      }
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_get_data")

observeEvent(input$btnDeleteArchive,
  {
    amErrorAction(title = "Module data: archive deletion confirmation", {
      selArchive <- input$selArchive
      txtConfirm <- tags$span(
        sprintf(
          ams(
            id = "srv_data_deletion_confirmation_warning"
          ),
          selArchive
        )
      )

      aBtns <- list(
        actionButton("btnDeleteArchiveConfirm", "Delete")
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
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_delete_archive")

observeEvent(input$btnDeleteArchiveConfirm,
  {
    amUpdateModal("amModal", close = TRUE)

    archivePath <- system(paste("echo", config$pathArchiveGrass), intern = T)
    selArchive <- input$selArchive
    archiveFilePath <- file.path(archivePath, selArchive)

    if (file.exists(archiveFilePath)) {
      file.remove(archiveFilePath)
      amUpdateDataList(listen)
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_delete_archive_confirm")

observeEvent(input$createArchive,
  {
    archivePath <- system(
      paste(
        "echo",
        config$pathArchiveGrass
      ),
      intern = T
    )
    dbCon <- grassSession$dbCon
    pathShapes <- grassSession$pathShapes
    valid <- isTRUE(file.exists(archivePath) && "SQLiteConnection" %in% class(dbCon))
    if (!valid) {
      return()
    }
    amErrorAction(title = "Module data: create archive", {
      amActionButtonToggle("createArchive", session, disable = TRUE)
      pBarTitle <- ams("srv_data_archive_creation_notice")
      customArchiveName <- input$txtArchiveName

      progressBarControl(
        visible = TRUE,
        percent = 1,
        title = pBarTitle,
        text = ams("srv_data_initialization_progress_notice")
      )

      tData <- dataListTableSelected()
      tData <- tData[c("origName", "type")]
      tData[] <- lapply(tData, as.character)
      tmpDataDir <- file.path(tempdir(), amRandomName())
      mkdirs(tmpDataDir)
      listDataDirs <- c()
      wdOrig <- getwd()
      tDataL <- nrow(tData)
      inc <- 1 / (tDataL + 1) * 100

      for (i in 1:tDataL) {
        dataName <- tData[i, "origName"]
        type <- tData[i, "type"]

        dataDir <- amExportData(
          language = listen$language,
          dataName = dataName,
          exportDir = tmpDataDir,
          type = type,
          dbCon = dbCon
        )

        listDataDirs <- c(listDataDirs, dataDir)

        msgStatus <- sprintf(
          ams("srv_data_exported_file_notice"),
          i,
          tDataL
        )

        if (i == tDataL) {
          msgStatus <- ams("srv_data_process_finished_create_archive")
        }

        pbc(
          visible = TRUE,
          percent = i * inc,
          title = pBarTitle,
          text = msgStatus
        )
      }

      dateStamp <- getClientDateStamp() %>%
        format("%Y_%m_%d@%H_%M")

      customArchiveName <- amSubPunct(customArchiveName)

      if (isEmpty(customArchiveName)) {
        customArchiveName <- "am5_export"
      }
      archiveName <- sprintf("%1$s_%2$s.zip", customArchiveName, dateStamp)
      archiveFilePath <- file.path(archivePath, archiveName)

      amDebugMsg(list(
        archiveName = archiveName,
        archiveFilePath = archiveFilePath,
        tmpDataDir = tmpDataDir
      ))
      pbc(
        visible = TRUE,
        percent = i * inc,
        title = pBarTitle,
        text = ams("srv_data_export_zip")
      )
      amZip(
        archivePath = archiveFilePath,
        files = listDataDirs
      )
      unlink(listDataDirs, recursive = T)
      amUpdateDataList(listen)
      amMsg(session,
        type = "log",
        text = sprintf(
          ams("srv_data_archive_creation_path"),
          basename(archiveFilePath)
        )
      )

      pbc(
        visible = TRUE,
        percent = (i + 1) * inc,
        title = pBarTitle,
        text = ams(
          id = "srv_data_progress_process_finished"
        )
      )
      pbc(
        visible = FALSE,
        percent = 0,
        title = "",
        text = ""
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_create_archive")
