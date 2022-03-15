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



idModule <- "module_toolbox"
#------------------------------------------------------------------------------#

# Update selectize when data list change

#------------------------------------------------------------------------------#

# Land cover
observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("rLandCover"),
      idSelect = c("landCoverSelect"),
      dataList = dataList
    )
    amUpdateSelectChoice(
      idData = c("tLandCover"),
      idSelect = c("landCoverSelectTable"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_ldc_table")

# Road
observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("vRoad"),
      idSelect = c("roadSelect"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_road_select")

# Barrier
observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("vBarrier"),
      idSelect = c("barrierSelect"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_barrier_select")



#------------------------------------------------------------------------------#

# Handle list of stack items

#------------------------------------------------------------------------------#

#
# Update stack contents
#

# observe change in rasters, update stack listener
observeEvent(dataList$raster,
  {
    listen$stackAll <- dataList$raster[grep("^rStack*", dataList$raster)]
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "filter_stack")

# observe stack listenner, update ui
observeEvent(listen$stackAll,
  {
    #
    # get values
    #
    oldIn <- input$stackMapList_1
    oldOut <- input$stackMapList_2
    stackAll <- listen$stackAll
    #
    # new stack item
    #
    newStack <- stackAll[!stackAll %in% c(oldIn, oldOut)]
    newOut <- na.exclude(c(newStack, stackAll[match(oldOut, stackAll)]))
    newIn <- na.exclude(stackAll[match(stackAll, oldIn)])

    newIn <- newIn[!sapply(newIn, is.null)]
    newOut <- newOut[!sapply(newOut, is.null)]

    #
    # update sortable input
    #
    amUpdateDoubleSortableInput("stackMapList", newIn, newOut)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_stack_sortable_input")


#
# handle action buttons
#

observeEvent(input$btnStackAllProcess,
  {
    #
    # Move all item in the list to process
    #
    amUpdateDoubleSortableInput("stackMapList", list1 = listen$stackAll)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_stack_sortable_input_all")

observeEvent(input$btnStackAllSkip,
  {
    #
    # Move all item to skip list
    #
    amUpdateDoubleSortableInput("stackMapList", list2 = listen$stackAll)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_stack_sortable_input_all_skip")


#------------------------------------------------------------------------------#

# MERGING NEW LANDCOVER

#------------------------------------------------------------------------------#



# populate selectize input

observeEvent(input$btnDeleteStack,
  {
    amErrorAction(title = "Module merge landcover : stack deletion confirmation", {
      dList <- dataList$raster[grep("^rStack", dataList$raster)]
      dSel <- input$stackMapList_2
      dList <- dList[which(dList %in% dSel)]

      if (length(dList > 0)) {
        if (length(dList) > 1) {
          txtHead <- tags$span(ams("srv_merge_landcover_several_items_deleted"))
        } else {
          txtHead <- tags$span(ams("srv_merge_landcover_one_item_deleted"))
        }
        content <- tagList(
          txtHead,
          tags$ul(
            HTML(
              paste("<li>", names(dList), "</li>")
            )
          )
        )
        aBtns <- list(
          actionButton(
            "btnDeleteStackConfirm",
            ams("srv_merge_landcover_delete_btn")
          )
        )
        addCancel <- TRUE
      } else {
        content <- tags$span(
          ams("srv_merge_landcover_nothing_selected")
        )
        aBtns <- NULL
        addCancel <- FALSE
      }

      amUpdateModal(
        panelId = "amModal",
        title = ams("srv_merge_landcover_modal_confirmation"),
        html = content,
        listActionButton = aBtns,
        addCancelButton = addCancel
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_delete_stack")



observeEvent(input$btnDeleteStackConfirm,
  {
    amUpdateModal("amModal", close = TRUE)
    sel <- amNameCheck(dataList, input$stackMapList_2, "raster")
    if (length(sel) > 0) {
      for (m in sel) {
        rmRastIfExists(m)
      }
      amUpdateDataList(listen)
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_delete_stack_confirm")






stackConflictTable <- reactive({
  tbl <- data.frame(
    map = character(0),
    class = character(0),
    label = character(0)
  )

  sel <- input$stackMapList_1
  update <- listen$updatedConflictTable

  amErrorAction(title = "stack conflict table", {
    if (isTRUE(length(sel) > 0)) {
      for (m in sel) {
        t <- amGetRasterCategory_cached(m)
        if (nrow(t) > 0) {
          t$map <- m
          tbl <- rbind(t, tbl)
        }
      }

      dupClass <- tbl$class[duplicated(tbl$class)]
      tbl <- tbl[tbl$class %in% dupClass, ]
      tbl <- tbl[order(tbl$class), ]
      tbl <- tbl[!grepl("rStackBarrier", tbl$map), ]
    }
  })
  return(tbl)
})

# validation
observe(
  {
    tbl <- stackConflictTable()
    # stackList <- amNameCheck(dataList,input$mapStack,'raster')
    stackList <- amNameCheck(dataList, input$stackMapList_1, "raster")
    stackTags <- input$stackTag
    rmArtefact <- input$cleanArtefact
    advancedTool <- input$showAdvancedTools
    language <- listen$language
    uiBtn <- div()
    amErrorAction(title = "Stack merge validation", {
      # conflict table update
      if (!is.null(tbl)) {
        isolate({
          nRowConflict <- nrow(tbl)
          # test if nrow >0 and send a message to UI in case of conflict
          if (isTRUE(nRowConflict > 1)) {
            if (isTRUE(advancedTool)) {
              #
              # Template for new classes = old classes
              #
              tbl$newClass <- tbl$class
              #
              # Ui
              #
              uiBtn <- tags$div(
                class = "row",
                column(
                  width = 6,
                  tags$p(
                    ams("srv_merge_landcover_manual_quick_correction")
                  )
                ),
                column(
                  width = 6,
                  actionButton(
                    "btnCorrectStack",
                    ams("srv_merge_landcover_quick_correction_btn")
                  )
                )
              )
            }
          } else {
            tbl <- data.frame(
              map = as.character(NA),
              class = as.integer(NA),
              label = as.character(NA)
            )
          }
          # render hotable with a possibly empty table
          output$stackConflict <- renderHotable(
            {
              tbl
            },
            stretched = "last",
            readOnly = c(1, 2, 3)
          )
          output$uiBtnCorrectStack <- renderUI({
            uiBtn
          })
        })
      }

      # validation process
      if (TRUE) {
        err <- character(0)
        stackItemMissing <- isTRUE(any(sapply(stackList, is.null)))
        hasConflict <- isTRUE(!is.null(tbl) && nrow(tbl) > 1)
        hasTag <- isTRUE(!any(stackTags == "", is.null(stackTags), nchar(stackTags) < 1))
        stackLcvName <- "rStackLandCover"
        stackNotOneLcv <- !isTRUE(length(grep(stackLcvName, stackList)) == 1)
        if (stackItemMissing) {
          err <- c(
            err,
            ams("srv_merge_landcover_stack_not_found_relaunch")
          )
        } else {
          if (hasConflict) {
            nConf <- duplicated(tbl$class)
            nConf <- length(nConf[nConf])
            # confPlur <- ifelse(nConf>1,"conflicts","conflict")
            err <- c(
              err,
              sprintf(
                ams("srv_merge_landcover_class_conflicts_found"),
                nConf
              )
            )
          } else {
            if (stackNotOneLcv) {
              err <- c(
                err,
                ams("srv_merge_landcover_land_cover_item_required")
              )
            } else {
              if (!hasTag) {
                err <- c(
                  err,
                  ams("srv_merge_landcover_tag_required")
                )
              }
            }
          }
        }
        if (length(err) > 0) {
          err <- HTML(paste("<div>",
            icon("exclamation-triangle"),
            err, "</div>",
            collapse = ""
          ))
          msgList <- tagList(
            tags$b(
              ams("srv_merge_landcover_validation_issues_notice")
            ),
            err
          )
          disBtn <- TRUE
        } else {
          msgList <- tagList(p(""))
          disBtn <- FALSE
        }
      }


      # set outputname if no validation problem
      if (!is.null(rmArtefact) && hasTag && !disBtn) {
        # existing dataset (use local scoping)

        if (!rmArtefact) {
          classes <- c("rLandCoverMerged")
        } else {
          classes <- c("rLandCoverMerged", "rLandCoverBridge")
        }

        vNames <- amCreateNames(classes, stackTags, dataList)

        outMap <- tagList(
          tags$b(
            ams("srv_merge_landcover_output_dataset_notice")
          ),
          HTML(paste("<div>",
            icon("sign-out"),
            vNames$html,
            "<div/>",
            collapse = ""
          ))
        )
      } else {
        outMap <- ""
      }


      output$stackNameInfo <- renderUI(outMap)
      output$stackWarning <- renderUI({
        msgList
      })
      amActionButtonToggle(id = "btnMerge", session, disable = disBtn)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "stack_validation")



#
# btn stack correction logic : auto value
#

observe(
  {
    amErrorAction(title = "Stack correct conflict btn validation", {
      cTable <- hotToDf(input$stackConflict)
      if (!isTRUE("newClass" %in% names(cTable))) {
        return()
      }
      cTable$newClass <- suppressWarnings(as.integer(cTable$newClass))
      cTable$newClass[is.na(cTable$newClass)] <- 0L
      output$stackConflict <- renderHotable(
        {
          cTable
        },
        readOnly = c(1, 2, 3),
        stretched = "last"
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "stack_confict_table")

# if btn correct stack is pressed
# reclassify raster.
# NOTE: we can't do a simple update using r.mapcalc or r.category : we need to keep cat label.
observeEvent(input$btnCorrectStack,
  {
    amErrorAction(title = "Stack correction", {
      pBarTitle <- ams("srv_merge_landcover_stack_value_correction")
      # get input table with modified column
      cTable <- hotToDf(input$stackConflict)
      nCtbl <- nrow(cTable)
      i <- 1
      # if number of row is greater than one
      if (nCtbl > 1) {
        # for each map in table
        for (m in cTable$map) {
          pbc(
            timeout = 1,
            id = "correct_stack",
            visible = TRUE,
            percent = 0,
            title = pBarTitle,
            text = sprintf(
              ams("srv_merge_landcover_stack_item_order_1"),
              i,
              nCtbl
            )
          )
          i <- i + 1

          # get tables orig and new classes
          oClass <- as.integer(cTable[cTable$map == m, "class"])
          nClass <- as.integer(cTable[cTable$map == m, "newClass"])
          # if texts in classes are different
          if (!isTRUE(all(oClass %in% nClass))) {
            # read table from raster category

            tbl <- amGetRasterCategory_cached(m)

            if (noDataCheck(tbl)) {
              stop(
                sprintf(
                  ams("srv_merge_landcover_empty_category_table_notice"),
                  m
                )
              )
            }
            # add no label if <NA> found
            tbl[is.na(tbl$V2), "V2"] <- "no label"
            # empty file to hold rules
            rulesFile <- tempfile()
            # extract all old classes
            clOrig <- as.numeric(tbl$V1)
            clNew <- as.numeric(tbl$V1)

            clNew[clNew == oClass] <- nClass
            # compose a new rules file and
            rules <- paste(clOrig, "=", clNew, " ", tbl$V2, collapse = "\n")
            write(x = rules, file = rulesFile)
            # reclass value using rules file
            execGRASS("r.reclass",
              input = m,
              output = "tmp_reclass",
              rules = rulesFile,
              flags = "overwrite"
            )
            # remove dependencies of tmp_reclass, overwrite m
            execGRASS("r.mapcalc",
              expression = sprintf("%1$s = %2$s", amNoMapset(m), "tmp_reclass"),
              flags = "overwrite"
            )
            # update conflict table
            listen$updatedConflictTable <- runif(1)
          }
        }
        pbc(
          id = "correct_stack",
          visible = TRUE,
          percent = 100,
          title = pBarTitle,
          text = ams("srv_merge_landcover_all_done_notice")
        )
      }
    })
    pbc(
      id = "correct_stack",
      visible = FALSE,
      percent = 0,
      title = pBarTitle,
      text = ""
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "stack_correction")



#  merge action
observeEvent(input$btnMerge,
  {
    timeCheck <- system.time({
      amActionButtonToggle(session = session, id = "btnMerge", disable = TRUE)
      stackTag <- input$stackTag
      # sel <- amNameCheck(dataList,input$mapStack,'raster')
      sel <- amNameCheck(dataList, input$stackMapList_1, "raster")
      if (!is.null(sel) && isTRUE(nchar(stackTag) > 0)) {
        amErrorAction(title = "Module 1: merge process", {
          updateTextInput(session, "stackTag", value = "")
          cleanBridge <- input$cleanArtefact

          pBarTitle <- ams("srv_merge_landcover_stack_merge_notice")
          selL <- length(sel)
          inc <- 100 / (selL + 1)
          incN <- 0

          stackTag <-
            amSubPunct(
              stackTag,
              config$sepTagFile,
              rmTrailingSep = T,
              rmLeadingSep = T,
              rmDuplicateSep = T
            )

          # set names
          merged <- amCreateNames("rLandCoverMerged", stackTag, dataList)$file
          bridges <- amCreateNames("rLandCoverBridge", stackTag, dataList)$file

          mapPosition <- 1
          tempBase <- "tmp__"
          isFirstMap <- TRUE
          rmRastIfExists("tmp_*")
          if (amRastExists("MASK")) execGRASS("r.mask", flags = "r")

          # Use barrier as mask for each stack element
          # keep order in tempMap name. eg. tmp__12_stack_road_test
          for (i in 1:selL) {
            pbc(
              timeout = 1,
              id = "stack_merge",
              visible = TRUE,
              percent = incN * inc,
              title = pBarTitle,
              text = sprintf(
                ams("srv_merge_landcover_stack_item_order_temp_map"),
                i,
                selL
              )
            )
            incN <- incN + 1

            # exctract stack item
            map <- sel[i]

            # If it's a barrier
            if (length(grep("rStackBarrier", map)) > 0) {
              if (amRastExists("MASK")) {
                # If a mask already exist, update it
                execGRASS("r.mapcalc",
                  expression = paste("MASK=isnull(", map, ")?MASK:null()"),
                  flags = "overwrite"
                )
              } else {
                # If not mask exist, use it as inverse mask
                execGRASS("r.mask", raster = map, flags = c("i"))
              }
            } else {
              # it's not a barrier : create temporary version of it using MASK context.
              # convert number to character eg. 12 "00012"
              classPos <- paste0(
                paste0(rep(0, 5 - nchar(mapPosition)),
                  collapse = ""
                ),
                mapPosition
              )
              tempMap <- paste0(tempBase, classPos, "_", map)
              execGRASS("r.mapcalc",
                expression = paste(tempMap, "=", map),
                flags = "overwrite"
              )
            }
            mapPosition <- mapPosition + 1


            pbc(
              id = "stack_merge",
              visible = TRUE,
              percent = incN * inc,
              title = pBarTitle,
              text = sprintf(
                ams("srv_merge_landcover_stack_item_order_2"),
                i,
                selL
              )
            )
          }
          # removing temp mask and active mask
          rmRastIfExists("tmp_mask__*")
          if (amRastExists("MASK")) execGRASS("r.mask", flags = "r")
          # get list of tmp__stack... maps.

          tempMapList <- execGRASS("g.list",
            type = "raster",
            pattern = paste0(tempBase, "*"),
            intern = TRUE
          )

          if (length(tempMapList) > 1) {
            execGRASS("r.patch",
              input = paste(tempMapList, collapse = ","),
              output = merged,
              flags = c("overwrite")
            )
          } else {
            execGRASS("g.copy",
              raster = paste0(tempMapList, ",", merged),
              flags = "overwrite"
            )
          }

          # In accessmod accessibility analysis, a null cell is a barrier, e.g. a river, mountain, militarized zone.
          # When we patch road maps to land cover maps, small overlaps can appear on top of predefined barrier.
          # Those overlaps act as briges when used in travel time analyis, thus, create shortcuts and wrong calculation.
          # If we used densified lines during rasterization process of roads, we can safely set the "one cell diagonal
          # bridge" as barrier without breaking road continuity.
          if (cleanBridge) {
            pbc(
              id      = "stack_merge",
              visible = TRUE,
              percent = 100,
              title   = pBarTitle,
              text    = ams("srv_merge_landcover_cleaning_artefacts_notice")
            )
            fromRoad <- sel[grep("rStackRoad", sel)]
            amBridgeFinder(fromRoad, merged, bridges)
            amBridgeRemover(bridges, removeFromMap = merged)
          }

          execGRASS("r.colors",
            map = merged,
            color = "random"
          )
          rmRastIfExists(paste0(tempBase, "*"))

          pbc(
            id = "stack_merge",
            visible = TRUE,
            percent = 100,
            title = pBarTitle,
            text =
              ams("srv_merge_landcover_process_finished_1"),
            timeOut = 2
          )
          pbc(
            id      = "stack_merge",
            visible = FALSE
          )

          amUpdateDataList(listen)
        })
      }

      amActionButtonToggle(session = session, id = "btnMerge", disable = FALSE)
    })
    print(timeCheck)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_merge")


#------------------------------------------------------------------------------#

# ADD TO STACK LANDCOVER

#------------------------------------------------------------------------------#




#
#    observe({
#      lcv <- dataList$raster[grep('^land_cover__',dataList$raster)]
#      lcvTable <- dataList$table[grep('^table_land_cover__',dataList$table)]
#      if(length(lcv)<1)lcv = ""
#      if(length(lcvTable)<1)lcvTable = ""
#      updateSelectInput(session,'landCoverSelect',choices = lcv)
#      updateSelectInput(session,'landCoverSelectTable',choices = lcvTable)
#    })
#


# toggle buttons to merge lcv table and add to stack
observe(
  {
    amErrorAction(title = "Land cover table validation ui handling", {
      lS <- amNameCheck(dataList, input$landCoverSelect, "raster")
      tbl <- hotToDf(input$landCoverRasterTable)
      language <- listen$language
      if (TRUE) {
        err <- character(0)
        uTable <- tolower(gsub("\\s", "", unlist(tbl)))
        # TODO: check why 0 is not wanted here.
        hasEmptyCells <- isTRUE(
          "-" %in% uTable ||
            "" %in% uTable ||
            NA %in% uTable
          # 0 %in% uTable
        )
        hasDuplicate <- isTRUE(any(duplicated(uTable)))
        lcvNotFound <- isTRUE(is.null(lS))
        if (lcvNotFound) {
          err <- c(
            err,
            ams("srv_merge_landcover_land_cover_missing")
          )
        } else {
          if (hasEmptyCells) {
            err <- c(
              err,
              ams("srv_merge_landcover_table_missing_or_empty")
            )
          } else {
            if (hasDuplicate) {
              err <- c(
                err,
                ams("srv_merge_landcover_table_duplicated_values")
              )
            }
          }
        }
        if (length(err) > 0) {
          err <- HTML(paste(
            "<div>",
            icon("exclamation-triangle"),
            err,
            "</div>",
            collapse = ""
          ))
          disBtn <- TRUE
        } else {
          disBtn <- FALSE
        }
      }


      # send result to ui
      if (length(err) > 0) {
        msgList <- tagList(
          tags$b(
            ams("srv_merge_landcover_validation_issues_notice_to_ui")
          ),
          err
        )
      } else {
        msgList <- tagList(
          p(
            ams("srv_merge_landcover_save_labels_add_lc")
          )
        )
      }
      output$stackLandcoverValidation <- renderUI(msgList)

      amActionButtonToggle(id = "btnAddStackLcv", session, disable = disBtn)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "landcover_table_validation")



observe(
  {
    tblUpdated <- hotToDf(input$landCoverRasterTable)
    isolate({
      amErrorAction(title = "Land cover table validation correction", {
        tblOriginal <- isolate(landCoverRasterTable())
        testNrow <- nrow(tblUpdated) == nrow(tblOriginal)
        # rule 1 : if nrow doesnt match, return original
        if (!is.null(tblUpdated) && !is.null(tblOriginal) && testNrow) {
          # rule 2: do not allow changing class
          tblValidated <- data.frame(class = tblOriginal$class, label = amSubPunct(tblUpdated$label, "_"))
        } else {
          tblValidated <- tblOriginal
        }
        output$landCoverRasterTable <- renderHotable(
          {
            tblValidated
          },
          readOnly = c(1),
          fixed = 1,
          stretched = "last"
        )
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "land_cover_raster_validation")


# Get reactive land cover cat table from raster.
landCoverRasterTable <- reactive({
  amErrorAction(title = "Land cover table raster", {
    sel <- amNameCheck(dataList, input$landCoverSelect, "raster")
    tbl <- data.frame(as.integer(NA), as.character(NA))

    if (!is.null(sel)) {
      tbl <- amGetRasterCategory_cached(sel)
      tbl[, 2] <- as.character(amSubPunct(tbl[, 2], "_"))
    }

    names(tbl) <- config$tableColNames[["tLandCover"]]
    return(tbl)
  })
})

landCoverSqliteTable <- reactive({
  amErrorAction(title = "Land cover table sqlite", {
    sel <- amNameCheck(dataList,
      input$landCoverSelectTable,
      "table",
      dbCon = isolate(grassSession$dbCon)
    )
    if (!is.null(sel)) {
      tbl <- dbGetQuery(isolate(grassSession$dbCon), paste("select * from", sel))
      tbl[, 1] <- as.integer(tbl[, 1])
      tbl[, 2] <- amSubPunct(tbl[, 2], "_")
    } else {
      tbl <- data.frame(as.integer(NA), as.character(NA))
      names(tbl) <- config$tableColNames[["tLandCover"]]
    }
    tbl
  })
})


# Save change in the lcv map.
landCoverRasterSave <- function(selLcv, tblLcv) {
  if (!is.null(selLcv) && !is.null(tblLcv)) {
    # save table in temp file
    tblOut <- tempfile()
    cla <- "rStackLandCover"
    stackName <- amNewName(cla, c(amGetTag(selLcv, type = "file"), "grid"))
    write.table(tblLcv,
      file = tblOut,
      row.names = F,
      col.names = F,
      sep = "\t",
      quote = F
    )
    execGRASS("g.copy", raster = paste0(selLcv, ",", stackName), flags = "overwrite")
    execGRASS("r.category", map = stackName, rules = tblOut)
    colorSetting <- amClassListInfo(cla, "colors")
    execGRASS("r.colors", map = stackName, color = colorSetting[1])
    amMsg(session,
      type = "log",
      text = sprintf(
        ams("srv_merge_landcover_"),
        selLcv,
        stackName
      )
    )
  }
}

# if select lcv map change or undo btn is pressed, update hotable with value from raster.
observe(
  {
    input$mergeLcvUndo # re evaluate if undo is pressed
    tblSqlite <- landCoverSqliteTable()
    tblRaster <- landCoverRasterTable()
    output$landCoverRasterTable <- renderHotable(tblRaster,
      readOnly = c(1),
      fixedCols = 1,
      stretched = "last"
    )
    output$landCoverSqliteTable <- renderHotable(tblSqlite,
      readOnly = T,
      fixedCols = 1,
      stretched = "last"
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "render_table_landcover")

# if merge button is pressed, merge external and raster table
observeEvent(input$mergeLcv,
  {
    amErrorAction(title = "Merge external lcv table", {
      tbl <- hotToDf(isolate(input$landCoverRasterTable))
      tbl[tbl == ""] <- NA
      tblExt <- hotToDf(isolate(input$landCoverSqliteTable))

      if (amNoDataCheck(tblExt)) {
        stop(
          ams("srv_merge_landcover_empty_external_table_warning")
        )
      }

      tblExt[tblExt == ""] <- NA
      # merging. we have to preserve Y classes and manual edits !
      # so, replacing only NA with corresponding value from ext table.
      # It doesn't seems to be doable with merge(y,x,all.x=T,by='class')
      # so, do it manually.
      naClass <- tbl[is.na(tbl$label), ]$class # class of missing label
      tblExtSub <- na.omit(tblExt[tblExt$class %in% naClass, ]) # find corresponding value in ext
      tbl[tbl$class %in% tblExtSub$class, ]$label <- tblExtSub$label # replace value with non-na remplacement
      tbl[, 1] <- as.integer(tbl[, 1])
      tbl[, 2] <- amSubPunct(tbl[, 2], "_")
      output$landCoverRasterTable <- renderHotable(
        {
          tbl
        },
        readOnly = c(1),
        fixedCols = 1,
        stretched = "last"
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "landcover_merge_table")


# if stack btn is pressed, save in GRASS.
observe(
  {
    btn <- input$btnAddStackLcv
    amErrorAction(title = "Add to stack: lcv", {
      isolate({
        pBarTitle <- ams("srv_merge_landcover_add_land_cover")
        sel <- amNameCheck(dataList, input$landCoverSelect, "raster")
        tbl <- hotToDf(input$landCoverRasterTable)
        if (!is.null(btn) && btn > 0 && !is.null(sel)) {
          pbc(
            id      = "stack_add_lcv",
            visible = TRUE,
            percent = 0,
            title   = pBarTitle,
            text    = ""
          )

          landCoverRasterSave(sel, tbl)
          amUpdateDataList(listen)

          pbc(
            id = "stack_add_lcv",
            visible = TRUE,
            percent = 100,
            title = pBarTitle,
            text =
              ams("srv_merge_landcover_process_finished_2"),
            timeOut = 2
          )

          listen$updatedConflictTable <- runif(1)
          pbc(
            id      = "stack_add_lcv",
            visible = FALSE
          )
        }
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_landcover_add_stack")

#------------------------------------------------------------------------------#

# ADD TO STACK ROAD

#------------------------------------------------------------------------------#


# populate selectize input




#
#
#
#  observe({
#    roadList <- amListData('vRoad',dataList)
#      if(length(roadList)==0)hfList=character(1)
#      amDebugMsg('Road 1. update input. roadList=',roadList)
#      updateSelectInput(session,'roadSelect',choices=roadList,selected=roadList[1])
#    })



# get road table columns
observe(
  {
    sel <- amNameCheck(dataList, input$roadSelect, "vector")
    amErrorAction(title = "get road table columns", {
      if (!is.null(sel)) {
        cla <- grassDbColType(sel, "INTEGER")
        cla <- cla[!cla %in% c("cat")]
        lab <- grassDbColType(sel, "CHARACTER")
      } else {
        cla <- ""
        lab <- ""
      }

      updateSelectInput(session,
        "roadSelectClass",
        choices = cla,
        selected = cla[1]
      )
      updateSelectInput(session,
        "roadSelectLabel",
        choices = lab,
        selected = lab[1]
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "road_list_update")

# create raod preview table
observe(
  {
    cla <- input$roadSelectClass
    lab <- input$roadSelectLabel
    noClass1000 <- input$checkDontAdd1000
    language <- listen$language
    amErrorAction(title = "create road preview table", {
      isolate({
        sel <- amNameCheck(dataList, input$roadSelect, "vector")
        if (!is.null(sel) && !is.null(cla) && !cla == "" && !is.null(lab) && !lab == "") {
          q <- paste("SELECT DISTINCT", cla, ",", lab, " FROM", sel, "LIMIT", config$maxRowPreview)
          tbl <- dbGetQuery(grassSession$dbCon, q)
          names(tbl) <- config$tableColNames[["tStackRoad"]]
          tbl[, 2] <- amSubPunct(tbl[, 2], "_")
        } else {
          tbl <- as.data.frame(list(cla = "-", lab = "-"), stringsAsFactors = FALSE)
          names(tbl) <- config$tableColNames[["tStackRoad"]]
          tbl
        }

        output$roadPreviewTable <- renderHotable(
          {
            tbl
          },
          readOnly = T,
          stretched = "all",
          fixedCols = 2
        )


        if (TRUE) {
          msgList <- character(0)
          err <- character(0)
          info <- character(0)
          uTable <- tolower(gsub("\\s", "", unlist(tbl)))
          hasEmptyCells <- isTRUE("-" %in% uTable || "" %in% uTable || NA %in% uTable)
          hasDuplicate <- any(duplicated(tbl$class)) || any(duplicated(tbl$label))
          roadLayerNotFound <- isTRUE(is.null(sel))
          autoAdd1000 <- isTRUE(any(tbl$class < 1000) && !noClass1000)

          if (roadLayerNotFound) {
            err <- c(
              err,
              ams("srv_merge_landcover_road_not_found")
            )
          } else {
            if (hasEmptyCells) {
              err <- c(
                err,
                ams("srv_merge_landcover_table_empty_values")
              )
            } else {
              if (hasDuplicate) {
                err <- c(
                  err,
                  ams("srv_merge_landcover_table_duplicated_values_warning")
                )
              }
            }
          }


          if (autoAdd1000) {
            info <- c(
              info,
              ams("srv_merge_landcover_classes_under_1000")
            )
          }

          if (length(err) > 0) {
            err <- HTML(paste("<div>",
              icon("exclamation-triangle"),
              err,
              "</div>",
              collapse = ""
            ))
            disBtn <- TRUE
          } else {
            disBtn <- FALSE
          }
        }


        # send result to ui
        if (length(err) > 0) {
          msgList <- tagList(tags$b(ams("srv_merge_landcover_validation_issues_notice")), err)
        } else {
          if (length(info) > 0) {
            info <- HTML(paste("<div>",
              icon("info"),
              info,
              "<div>",
              collapse = ""
            ))
            msgList <- tagList(
              msgList,
              tags$b(
                ams("srv_merge_landcover_information_notice")
              ),
              info
            )
          }
        }
        output$stackRoadValidation <- renderUI(msgList)

        amActionButtonToggle(
          session = session,
          id = "btnAddStackRoad",
          disable = disBtn
        )
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_btn_add_stack")

# Add vector road to raster road stack
observeEvent(input$btnAddStackRoad,
  {
    amErrorAction(title = "Module 1: add stack road", {
      amActionButtonToggle(
        session = session,
        id = "btnAddStackRoad",
        disable = TRUE
      )
      stackClass <- "rStackRoad"
      pBarTitle <- ams("srv_merge_landcover_add_roads")

      tbl <- hotToDf(input$roadPreviewTable)
      sel <- amNameCheck(dataList, input$roadSelect, "vector")
      cla <- input$roadSelectClass
      lab <- input$roadSelectLabel


      if (!is.null(sel) && !is.null(cla) && !is.null(lab)) {
        tblN <- nrow(tbl)
        inc <- 100 / tblN
        incN <- 0


        # increment
        for (i in 1:tblN) {
          pbc(
            timeout = 1,
            id = "stack_add_road",
            visible = TRUE,
            percent = incN * inc,
            title = pBarTitle,
            text = sprintf(
              ams("srv_merge_landcover_stack_item_order_3"),
              i,
              tblN
            )
          )
          incN <- incN + 1


          #
          # Init
          #
          class <- tbl[i, "class"]
          label <- tbl[i, "label"]
          outNameTmp <- paste0("tmp__", sel)
          outNameStack <- amNewName(
            stackClass,
            c(label, amGetTag(sel, type = "file"), "line")
          )
          colorSetting <- amClassListInfo(stackClass, "colors")

          #
          # Extract road by class
          #
          execGRASS("v.extract",
            input  = sel,
            output = outNameTmp,
            where  = paste0(cla, "=", class),
            flags  = "overwrite"
          )

          #
          # Class and label cleaning
          #

          if (isTRUE(class < 1000 && !input$checkDontAdd1000)) {
            class <- 1000 + class
          }

          label <- tolower(amSubPunct(label, sep = "_"))
          labelRule <- amSubPunct(label, sep = " ")
          tmpFile <- tempfile()
          tmpRules <- paste0(class, "\t", labelRule)


          #
          # Write temp rules
          #

          write(tmpRules, file = tmpFile)

          #
          # Raster creation
          #


          execGRASS("v.to.rast",
            use = "val",
            type = "line",
            input = outNameTmp,
            output = outNameStack,
            value = class,
            flags = c("d", "overwrite")
          )

          execGRASS("r.colors", map = outNameStack, color = colorSetting[1])

          execGRASS("r.category",
            map = outNameStack,
            rules = tmpFile
          )

          rmVectIfExists(outNameTmp)

          pbc(
            id = "stack_add_road",
            visible = TRUE,
            percent = incN * inc,
            title = pBarTitle,
            text = sprintf(
              ams("srv_merge_landcover_stack_item_order_4"),
              i,
              tblN
            )
          )
        }
        amUpdateDataList(listen)
        pbc(
          id      = "stack_add_road",
          visible = TRUE,
          percent = 100,
          title   = pBarTitle,
          text    = ams("srv_merge_landcover_process_finished_3"),
          timeOut = 2
        )

        listen$updatedConflictTable <- runif(1)
        pbc(
          id      = "stack_add_road",
          visible = FALSE
        )
      }
      amActionButtonToggle(
        session = session,
        id = "btnAddStackRoad",
        disable = FALSE
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_road_add_stack")



#------------------------------------------------------------------------------#

#  ADD TO STACK BARRIER

#------------------------------------------------------------------------------#




# toggle add to stack barrier btn
observe(
  {
    bT <- input$barrierType
    bS <- amNameCheck(dataList, input$barrierSelect, "vector")
    disableBtn <- any(is.null(bT), bT == "", is.null(bS), bS == "")
    amActionButtonToggle(
      id = "btnAddStackBarrier",
      session,
      disable = disableBtn
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_barrier_add_stack")

#
# Set default skeleton resolution and buffer
#
observeEvent(listen$mapMeta,
  {
    res <- listen$mapMeta$grid$nsres

    updateNumericInput(
      session = session,
      "numBarrierSkeletonRes",
      max = res,
      min = res / 5,
      value = round(res / 3)
    )
    updateNumericInput(
      session = session,
      "numBarrierSkeletonBuffer",
      max = res,
      min = 1,
      value = round(res / 2)
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "ldc_stack_skeleton_config_res")




#
# Validation
#
observe(
  {
    amErrorAction(title = "Add vector barrier validation", {
      #
      # Get input
      #
      skeletonRes <- input$numBarrierSkeletonRes
      skeletonBuffer <- input$numBarrierSkeletonBuffer
      mapMeta <- listen$mapMeta
      res <- mapMeta$grid$nsres
      ratio <- res / skeletonRes
      cols <- mapMeta$grid$cols * ratio
      rows <- mapMeta$grid$rows * ratio
      cells <- mapMeta$grid$cells
      cellsAfter <- cols * rows
      cellsHighNumber <- cellsAfter > (3 * cells)
      isPoly <- input$barrierType == "area"
      polyAsSkeleton <- isPoly && input$checkBarrierPolyAsSkeleton
      validRes <- TRUE
      validBuffer <- TRUE
      hasLayer <- !amNoDataCheck(input$barrierSelect)

      #
      # init messages
      #
      msgList <- tagList()
      err <- character(0)
      warn <- character(0)

      isolate({
        #
        # Check layer
        #
        if (!hasLayer) {
          err <- c(err, ams("srv_merge_landcover_err_missing_barrier"))
        } else {
          #
          # Check skeleton resolution and buffer parameters
          #
          if (polyAsSkeleton) {
            validRes <- !amNoDataCheck(skeletonRes) &&
              is.numeric(skeletonRes) &&
              skeletonRes > 0 &&
              skeletonRes <= res
            validBuffer <- !amNoDataCheck(skeletonBuffer) &&
              is.numeric(skeletonBuffer) &&
              skeletonBuffer >= 0 &&
              skeletonBuffer <= res
          }

          #
          # Add validation messages
          # and update descriptions
          #
          if (polyAsSkeleton && !validRes) {
            err <- c(err, ams("toolbox_land_cover_poly_skeleton_err_invalid_res"))
          } else {
            output$txtMessageSkeletonRes <- renderText({
              sprintf(
                ams("toolbox_land_cover_poly_skeleton_res_desc"),
                skeletonRes,
                format(cellsAfter, sci = TRUE),
                round(res / 3)
              )
            })
            if (polyAsSkeleton && cellsHighNumber) {
              warn <- c(
                warn,
                sprintf(
                  ams("toolbox_land_cover_poly_skeleton_warn_ncell"),
                  format(cellsAfter, sci = TRUE)
                )
              )
            }
          }

          if (polyAsSkeleton && !validBuffer) {
            err <- c(err, ams("toolbox_land_cover_poly_skeleton_err_invalid_buffer"))
          } else {
            output$txtMessageSkeletonBuffer <- renderText({
              sprintf(
                ams("toolbox_land_cover_poly_skeleton_buffer_desc"),
                round(res / 2),
                skeletonBuffer
              )
            })
          }
        }

        #
        # Display error messages and block button if needed
        #
        hasError <- length(err) > 0
        hasWarn <- length(warn) > 0

        amActionButtonToggle(
          session = session,
          id = "btnAddStackBarrier",
          disable = hasError
        )

        if (hasError) {
          msgList <- msgList <- tagList(
            msgList,
            HTML(
              paste("<div>",
                icon("exclamation-triangle"),
                err,
                "</div>",
                collapse = ""
              )
            )
          )
        }

        if (hasWarn) {
          msgList <- msgList <- tagList(
            msgList,
            HTML(
              paste("<div>",
                icon("info-circle"),
                warn,
                "</div>",
                collapse = ""
              )
            )
          )
        }
      })

      #
      # Add message list
      #
      output$msgAddStackBarrier <- renderUI({
        msgList
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "ldc_stack_skeleton_config_msg")






# preview table of barrier features
barrierPreview <- reactive({
  sel <- amNameCheck(dataList, input$barrierSelect, "vector")
  amErrorAction(title = "Module 1: barrier preview", {
    tbl <- amGetTableFeaturesCount(sel, types = c("lines", "areas", "points"))
    return(tbl)
  })
})

# render table
observe(
  {
    tbl <- barrierPreview()

    if (!amNoDataCheck(tbl$count)) {
      tbl[tbl$type == "areas", "type"] <- ams("toolbox_land_cover_barrier_type_polygons")
      tbl[tbl$type == "lines", "type"] <- ams("toolbox_land_cover_barrier_type_lines")
      tbl[tbl$type == "points", "type"] <- ams("toolbox_land_cover_barrier_type_points")
    }
    output$barrierPreviewTable <- renderHotable(
      {
        tbl
      },
      readOnly = T,
      fixedCols = 2,
      stretched = "all"
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "barrier_render_table")

#  pre select feature based on max count by type
observe(
  {
    tbl <- na.omit(barrierPreview())
    if (!amNoDataCheck(tbl)) {
      sel <- tbl[which.max(tbl$count), "type"]
      updateRadioButtons(session, "barrierType", selected = gsub("s$", "", sel))
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "barrier_auto_select")


# add to stack process
observeEvent(input$btnAddStackBarrier,
  {
    amErrorAction(title = "Add to stack : barrier", {
      stackClass <- "rStackBarrier"
      pBarTitle <- ams("srv_merge_landcover_add_barriers")
      sel <- amNameCheck(dataList, input$barrierSelect, "vector")
      polyAsSkeleton <- input$checkBarrierPolyAsSkeleton
      skeletonRes <- input$numBarrierSkeletonRes
      skeletonBuffer <- input$numBarrierSkeletonBuffer

      on.exit({
        amRegionReset()
        rmVectIfExists("tmp__poly_barrier*")
        rmRastIfExists("tmp__poly_barrier*")
        pbc(
          id = "stack_add_barrier",
          visible = FALSE
        )
        amActionButtonToggle(
          session = session,
          id = "btnAddStackBarrier",
          disable = FALSE
        )
      })
    })

    amActionButtonToggle(
      session = session,
      id = "btnAddStackBarrier",
      disable = TRUE
    )

    prog <- function(percent = 1, text = "progress", visible = TRUE) {
      pbc(
        timeout = 1,
        id      = "stack_add_barrier",
        visible = visible,
        percent = percent,
        title   = pBarTitle,
        text    = text
      )
    }



    type <- input$barrierType
    if (!is.null(sel) && !sel == "") {
      cl <- 1
      la <- "barrier"
      tmpFile <- tempfile()
      write(paste0(cl, "\t", la), tmpFile)
      nSel <- length(sel)
      inc <- 100 / nSel
      incN <- 0

      for (i in 1:nSel) {
        progPercent <- incN * inc

        prog(
          percent = progPercent + 1,
          text = sprintf(
            ams("srv_merge_landcover_stack_item_order_5"),
            i,
            nSel
          )
        )

        s <- sel[i]

        outNameStack <- amNewName(
          class = stackClass,
          tags = c(amGetTag(s, type = "file"), type)
        )

        outNameStackCenter <- amNewName(
          class = stackClass,
          tags = c(amGetTag(s, type = "file"), "line")
        )
        #
        # Use polygon skeleton  as barrier.
        #
        if (isTRUE(type == "area") && isTRUE(polyAsSkeleton)) {
          resInit <- amMapMeta()$grid$nsres
          resAnalysis <- skeletonRes
          distBuffer <- skeletonBuffer

          execGRASS("g.region",
            res = as.character(resAnalysis)
          )

          prog(
            percent = progPercent + 2,
            text = sprintf(
              ams("srv_merge_landcover_stack_skeleton"),
              sprintf("rasterize at %s m ", resAnalysis)
            )
          )

          execGRASS("v.to.rast",
            input  = s,
            output = "tmp__poly_barrier_rast",
            use    = "val",
            value  = 1,
            flags  = c("overwrite")
          )

          if (distBuffer > 0) {
            prog(
              percent = progPercent + 3,
              text = sprintf(
                ams("srv_merge_landcover_stack_skeleton"),
                sprintf("create %s m buffer", distBuffer)
              )
            )

            execGRASS("r.buffer",
              input     = "tmp__poly_barrier_rast",
              output    = "tmp__poly_barrier_buffer",
              distances = distBuffer,
              flags     = c("overwrite")
            )
          }
          #
          # Rest region to default
          #
          amRegionReset()

          prog(
            percent = progPercent + 4,
            text = sprintf(
              ams("srv_merge_landcover_stack_skeleton"),
              sprintf("find skeleton at original resolution ", distBuffer)
            )
          )

          execGRASS("r.thin",
            input      = ifelse(distBuffer > 0, "tmp__poly_barrier_buffer", "tmp__poly_barrier_rast"),
            output     = "tmp__poly_barrier_thin",
            flags      = c("overwrite"),
            iterations = 150
          )

          prog(
            percent = progPercent + 5,
            text = sprintf(
              ams("srv_merge_landcover_stack_skeleton"),
              sprintf("convert skeleton to lines ", distBuffer)
            )
          )

          execGRASS("r.to.vect",
            input  = "tmp__poly_barrier_thin",
            output = "tmp__poly_barrier_skeleton",
            flags  = c("overwrite", "t"),
            type   = c("line")
          )

          prog(
            percent = progPercent + 6,
            text = sprintf(
              ams("srv_merge_landcover_stack_skeleton"),
              sprintf("extract raster densified line ", distBuffer)
            )
          )

          execGRASS("v.to.rast",
            input  = "tmp__poly_barrier_skeleton",
            output = outNameStackCenter,
            type   = "line",
            use    = "val",
            value  = cl,
            flags  = c("overwrite", "d")
          )
          execGRASS("r.category", map = outNameStackCenter, rules = tmpFile)
        }

        prog(
          percent = progPercent + 7,
          text    = ams("srv_merge_landcover_stack_convert_raster")
        )

        execGRASS("v.to.rast",
          use = "val",
          input = s,
          output = outNameStack,
          type = type,
          value = cl,
          flags = c("overwrite", if (type == "line") {
            "d"
          })
        )
        execGRASS("r.category", map = outNameStack, rules = tmpFile)
        prog(
          percent = progPercent + 8,
          text = sprintf(
            ams("srv_merge_landcover_stack_item_order_6"),
            i,
            nSel
          )
        )

        incN <- incN + 1
      }


      amUpdateDataList(listen)
      pbc(
        id      = "stack_add_barrier",
        visible = TRUE,
        percent = 99,
        title   = pBarTitle,
        text    = ams("srv_merge_landcover_process_finished_4"),
        timeOut = 2
      )

      listen$updatedConflictTable <- runif(1)
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_add_stack_barrier")
