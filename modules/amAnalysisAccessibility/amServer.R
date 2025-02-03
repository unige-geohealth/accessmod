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

source("modules/amAnalysisAccessibility/amServer_validation.R", local = TRUE)
idModule <- "module_analysis"
#
# Populate or update selectInput
#
observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("rLandCoverMerged"),
      idSelect = "mergedSelect",
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_merge_ldc")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("vFacilityNew", "vFacility"),
      idSelect = c("hfSelect", "hfSelectTo"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_facility_new")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("tScenario", "tScenarioOut"),
      idSelect = "modelSelect",
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_scenario")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("rPopulationResidual", "rPopulation"),
      idSelect = c("popSelect"),
      dataList = dataList
    )
    amUpdateSelectChoice(
      idData = c("rPopulation", "rPopulationResidual"),
      idSelect = c("popResidualSelect"),
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_population")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("rTravelTime", "rTravelTimeImported"),
      idSelect = "travelTimeSelect",
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_travel_time")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("vZone"),
      idSelect = "zoneSelect",
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_zones")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("tCapacity", "tCapacityOut"),
      idSelect = "capTableSelect",
      dataList = dataList
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_capacity")

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("tSuitability", "tSuitabilityOut"),
      idSelect = "suitabilityTableSelect",
      dataList = dataList,
      addChoices = amsNamed(config$defaultWithoutData),
      debug = TRUE
    )
    amUpdateSelectChoice(
      idData = c("tExclusion", "tExclusionOut"),
      idSelect = "exclusionTableSelect",
      dataList = dataList,
      addChoices = amsNamed(config$defaultWithoutData),
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_suitability")

observeEvent(listen$language,
  {
    opt <- list("popsum", "dist", "traveltime", "priority")

    names(opt) <- list(
      ams("analysis_scaleup_select_factor_popsum"),
      ams("analysis_scaleup_select_factor_dist"),
      ams("analysis_scaleup_select_factor_traveltime"),
      ams("analysis_scaleup_select_factor_priority")
    )

    updateSelectizeInput(session,
      inputId = "selFactor",
      choices = opt
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_factor_choices")

#
#  Scaling up suitability factor layer
#
observeEvent(
  {
    listen$dataListUpdated
    input$selFactor
  },
  {
    #
    # Update selFactorLayer input according to user choice
    #
    switch(input$selFactor,
      "popsum" = {
        amUpdateSelectChoice(
          idData = c("rPopulation", "rPopulationResidual"),
          idSelect = "selFactorLayer",
          addChoices = amsNamed(config$dynamicPopulation),
          dataList = dataList,
          selected = config$dynamicPopulation
        )
      },
      "dist" = {
        amUpdateSelectChoice(
          idData = c("vRoad", "vBarrier", "vFacility"),
          idSelect = "selFactorLayer",
          addChoices = amsNamed(config$dynamicFacilities),
          dataList = dataList,
          selected = config$dynamicFacilities
        )
      },
      "traveltime" = {
        amUpdateSelectChoice(
          idData = c("vRoad", "vBarrier", "vFacility"),
          idSelect = "selFactorLayer",
          addChoices = amsNamed(config$dynamicFacilities),
          dataList = dataList,
          selected = config$dynamicFacilities
        )
      },
      "priority" = {
        amUpdateSelectChoice(
          idData = c("rPriority"),
          idSelect = "selFactorLayer",
          dataList = dataList
        )
      }
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_suit_factors")

#
#  Set layers available for exclusion
#

observeEvent(listen$dataListUpdated,
  {
    amUpdateSelectChoice(
      idData = c("rExclusion", "vExclusion"),
      addChoices = amsNamed(config$dynamicFacilities),
      idSelect = "selExclusion",
      dataList = dataList,
      emptySelected = FALSE
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_exclusion")

#
#  Capacity table
#
# Extract capacity table and render in tabulator
observe(
  {
    amErrorAction(title = "Set new capacity table", {
      listen$dataListUpdated
      input$capTableSelect
      listen$selProject

      isolate({
        capNewTable <- amNameCheck(dataList,
          input$capTableSelect,
          "table",
          dbCon = grassSession$dbCon
        )

        if (is.null(capNewTable) || nchar(capNewTable) == 0) {
          tbl <- data.frame(
            min = as.numeric(0),
            max = as.numeric(0),
            label = as.character("label"),
            capacity = as.numeric(0)
          )
        } else {
          tbl <- dbGetQuery(grassSession$dbCon, paste("SELECT * FROM", capNewTable))
          tbl <- as.data.frame(lapply(tbl, function(x) {
            if (is.integer(x)) {
              x <- as.numeric(x)
            }
            x
          }))
        }

        output$capacityTable <- render_tabulator({
          tabulator(
            data = tbl,
            readOnly = FALSE,
            stretched = "last"
          )
        })
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_capacity_init")

#
# Enable / disable limit closest input
#
observeEvent(input$checkReferralPermute, {
  amInputToggle(
    "checkReferralLimitClosest",
    disable = input$checkReferralPermute
  )

  if (isTRUE(input$checkReferralPermute)) {
    #
    # Requested in #431
    #
    updateCheckboxInput(
      session,
      inputId = "checkReferralLimitClosest",
      value = FALSE
    )
  }
})


# Add a row to capacity table
observeEvent(input$btnAddRowCapacity,
  {
    tbl <- tabulator_to_df(input$capacityTable_data)
    proxy <- tabulator_proxy("capacityTable")

    row <- data.frame(
      min = as.numeric(0),
      max = as.numeric(0),
      label = as.character("label"),
      capacity = as.numeric(0)
    )

    tabulator_add_rows(proxy, row, position = "bottom")
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_capacity_add_row")

# Remove a row from capacity table
observeEvent(input$btnRmRowCapacity,
  {
    proxy <- tabulator_proxy("capacityTable")
    tabulator_remove_last_row(proxy)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_capacity_rm_row")

#
# Zonal stat: fields from zonal vector map
#

# Get fields summary reactive list
zoneFields <- reactive({
  zoneSel <- amNameCheck(dataList, input$zoneSelect, "vector")
  # Get field summary
  isolate({
    if (length(zoneSel) > 0) {
      zoneFieldsSummary <- amGetFieldsSummary(
        dbCon = grassSession$dbCon,
        table = zoneSel
      )
    } else {
      zoneFieldsSummary <- list()
    }
    return(zoneFieldsSummary)
  })
})

# Get zone attribute table fields summary (num, char, idx candidate, val unique)
observe(
  {
    amErrorAction(title = "Set zone fields", {
      zoneFieldIdx <- zoneFields()$int
      # zoneFieldIdx <- zoneFieldIdx[zoneFieldIdx %in% zoneFields()$int]
      # NOTE: We have to convert vector of zone to raster to use r.univar. In this case, only integer column are allowed.
      # NOTE: v.rast.stat could be a better choice, but it does not return a table: new prefixed column are created in original vector.
      zoneFieldLabel <- zoneFields()$char
      if (length(zoneFieldIdx) > 0 && length(zoneFieldLabel) > 0) {
        # Search for common id and label/name field position using grep
        idPos <- grep("[iI][dD]", zoneFieldIdx)
        # Set id selection
        if (length(idPos) > 0) {
          zoneIdSel <- zoneFieldIdx[idPos][1]
        } else {
          zoneIdSel <- zoneFieldIdx[1]
        }
      } else {
        zoneFieldIdx <- ""
        zoneIdSel <- ""
      }

      zoneFieldLabel <- zoneFields()$char
      if (length(zoneFieldLabel) > 0) {
        labelPos <- grep("[nN][aA][mM][eE]", zoneFieldLabel)
        # Set label selection
        if (length(labelPos) > 0) {
          zoneLabelSel <- zoneFieldLabel[labelPos][1]
        } else {
          zoneLabelSel <- zoneFieldLabel[1]
        }
      } else {
        zoneFieldLabel <- ""
        zoneLabelSel <- ""
      }

      updateSelectInput(session,
        "zoneId",
        choices = zoneFieldIdx,
        selected = zoneIdSel
      )
      updateSelectInput(session,
        "zoneLabel",
        choices = zoneFieldLabel,
        selected = zoneLabelSel
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_field_zone_id_label")

#
# HF fields summary (FROM/TO)
#
# Get hf (from) attribute table fields summary (num, char, idx candidate, val unique)
hfFields <- reactive({
  selHfFrom <- amNameCheck(dataList, input$hfSelect, "vector")
  # Get field summary
  isolate({
    if (length(selHfFrom) > 0) {
      hfFrom <- amGetFieldsSummary(dbCon = grassSession$dbCon, selHfFrom)
    } else {
      hfFrom <- list()
    }
    return(hfFrom)
  })
})

# Get hf (to) attribute table fields summary (num, char, idx candidate, val unique)
hfFieldsTo <- reactive({
  isModReferral <- isTRUE(input$moduleSelector == "module_4")
  selHfTo <- amNameCheck(dataList, input$hfSelectTo, "vector")
  selHfFrom <- amNameCheck(dataList, input$hfSelect, "vector")
  if (!is.null(selHfTo)) {
    if (selHfFrom == selHfTo) {
      return(hfFields())
    }
    # Get field summary
    isolate({
      if (length(selHfTo) && isModReferral) {
        return(
          amGetFieldsSummary(dbCon = grassSession$dbCon, selHfTo)
        )
      }
    })
  }
  list()
})

# Update select order field
observe(
  {
    amErrorAction(title = "Update hf order field", {
      hfFieldsNum <- hfFields()$num
      if (length(hfFieldsNum) > 0) {
        hfFieldsNum <- hfFieldsNum[!hfFieldsNum == config$vectorKey]
        capField <- grep("[oO]rder|[cC]apac", hfFieldsNum, value = TRUE)
        if (length(capField) > 0) {
          sel <- capField[1]
        } else {
          sel <- hfFieldsNum[1]
        }
      } else {
        hfFieldsNum <- ""
        sel <- ""
      }
      updateSelectInput(session, "hfOrderColumn", choices = hfFieldsNum, selected = sel)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_hf_order_column")

# Update idx fields FROM
observe(
  {
    amErrorAction(title = "Update hf field", {
      hfFieldsIdx <- hfFields()$idx

      if (length(hfFieldsIdx) > 0) {
        sel <- config$vectorKey
      } else {
        hfFieldsIdx <- ""
        sel <- ""
      }

      hfFieldsIntIdx <- hfFields()$intIdx

      if (length(hfFieldsIntIdx) > 0) {
        selInt <- config$vectorKey
      } else {
        hfFieldsIntIdx <- ""
        selInt <- ""
      }

      updateSelectInput(
        session,
        "hfIdxField",
        choices = hfFieldsIdx,
        selected = sel
      )
      updateSelectInput(
        session,
        "hfIdxIntField",
        choices = hfFieldsIntIdx,
        selected = selInt
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_hf_idx_column")

# Update idx fields TO
observe(
  {
    amErrorAction(title = "Update hf field", {
      hfFieldsIdx <- hfFieldsTo()$idx
      if (length(hfFieldsIdx) > 0) {
        sel <- config$vectorKey
      } else {
        sel <- ""
        hfFieldsIdx <- ""
      }
      updateSelectInput(session, "hfIdxFieldTo", choices = hfFieldsIdx, selected = config$vectorKey)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_hf_to_column")

# Update select HF capacity fields
observe(
  {
    amErrorAction(title = "Update hf field", {
      hfFieldsNum <- hfFields()$num
      hfIdx <- input$hfIdxField
      if (isTRUE(nchar(hfIdx) > 0) && length(hfFieldsNum) > 0) {
        hfFieldsNum <- hfFieldsNum[!hfFieldsNum == config$vectorKey]
        hfFieldsNum <- hfFieldsNum[!hfFieldsNum == hfIdx]
        capField <- grep("[cC]apac", hfFieldsNum, value = TRUE)
        if (length(capField) > 0) {
          sel <- capField[1]
        } else {
          sel <- hfFieldsNum[1]
        }
      } else {
        hfFieldsNum <- ""
        sel <- ""
      }
      updateSelectInput(session, "hfCapacityField", choices = hfFieldsNum, selected = sel)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_hf_capacity_field")

# Update name fields
observe(
  {
    amErrorAction(title = "Update hf field", {
      hfIdx <- input$hfIdxField
      hfCapacity <- input$hfCapacityField
      hfFields <- c(hfFields()$char, hfFields()$num)
      if (isTRUE(nchar(hfIdx) > 0 && length(hfFields) > 0)) {
        hfFields <- hfFields[!hfFields %in% hfIdx]
        hfFields <- hfFields[!hfFields %in% hfCapacity]
        nameField <- grep("[nN]ame", hfFields, value = TRUE)
      } else {
        hfFields <- ""
        nameField <- ""
      }
      if (length(nameField) > 0) {
        sel <- nameField[1]
      } else {
        sel <- hfFields[1]
      }
      updateSelectInput(session, "hfNameField", choices = hfFields, selected = sel)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_hf_name_field")

# Update label fields TO
observe(
  {
    amErrorAction(title = "Update hf field", {
      hfIdx <- input$hfIdxFieldTo
      hfFields <- c(hfFieldsTo()$char, hfFieldsTo()$num)
      if (isTRUE(nchar(hfIdx) > 0) && length(hfFields) > 0) {
        hfFields <- hfFields[!hfFields %in% hfIdx]
        nameField <- grep("[nN]ame", hfFields, value = TRUE)
      } else {
        hfFields <- ""
        nameField <- ""
      }
      if (length(nameField) > 0) {
        sel <- nameField[1]
      } else {
        sel <- hfFields[1]
      }
      updateSelectInput(session, "hfNameFieldTo", choices = hfFields, selected = sel)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_hf_name_field_to")

#
# Population on barrier validation
#

# Pop on barrier stat
popOnBarrierStat <- reactive({
  # if(input$moduleSelector=='module_3'){
  pop <- amNameCheck(dataList, input$popSelect, "raster")
  merged <- amNameCheck(dataList, input$mergedSelect, "raster")
  if (!is.null(pop) & !is.null(merged)) {
    tmpMapPop <- "tmp__test_pop_on_barrier"
    execGRASS("r.mask", flags = "i", raster = merged)
    execGRASS("r.mapcalc",
      flags = "overwrite",
      expression = paste(tmpMapPop, " = ", pop, "")
    )
    execGRASS("r.mask", flags = "r")

    sumPop <- execGRASS("r.univar",
      map = tmpMapPop,
      flags = c("g", "t"),
      intern = T
    ) %>%
      amCleanTableFromGrass(
        cols = c("non_null_cells", "sum")
      )

    origPop <- execGRASS("r.univar",
      map = pop,
      flags = c("g", "t"),
      intern = T
    ) %>%
      amCleanTableFromGrass(
        cols = c("sum")
      )

    return(
      list(
        sum = round(sumPop$sum, 2),
        cells = sumPop$non_null_cells,
        percent = round(100 * (sumPop$sum / origPop$sum), 2)
      )
    )
  }
  #   }
  return(list())
})

#
# Scaling up validation options
#

observeEvent(input$selFactorLayer,
  {
    selFactor <- input$selFactorLayer
    if (isTRUE(!is.null(selFactor) && nchar(selFactor) > 0)) {
      disBtn <- FALSE
    } else {
      disBtn <- TRUE
    }
    amActionButtonToggle(session = session, "btnAddFactor", disable = disBtn)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_btn_add_factor")

#
# Indication of the number of cells processed for popsum distance
#

observeEvent(input$factorPopSumRadius,
  {
    radius <- input$factorPopSumRadius * 1000
    ncellsTxt <- 0
    valid <- is.numeric(radius) && !is.na(radius) && length(radius) > 0
    if (valid) {
      grid <- listen$mapMeta$grid
      area <- pi * radius^2
      frac <- area / grid$nsres^2

      if (isTRUE(frac < 1)) {
        ncells <- grid$cells
      } else {
        ncells <- frac * grid$cells
      }
      ncellsTxt <- format(ncells, digits = "4", scientific = TRUE)
      if (isTRUE(ncells > 1e6)) {
        ncellsTxt <- sprintf(ams("analysis_scaleup_cells_resolution_warning"), ncellsTxt)
      }
    }

    amActionButtonToggle(session = session, "btnAddFactor", disable = !valid)
    amUpdateText(id = "popSumNumCells", text = ncellsTxt)
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_button_add_factor_and_pop_sum")

# Initial exclusion table
observe(
  {
    amErrorAction(title = "Initial exclusion table", {
      selProject <- listen$selProject
      excluTable <- amNameCheck(
        dataList,
        input$exclusionTableSelect,
        "table",
        dbCon = grassSession$dbCon
      )
      btnReset <- input$btnResetExcluTable
      hasTable <- isNotEmpty(excluTable)

      tbl <- data.frame(
        id = 1,
        layer = "vOutputFacility",
        buffer = 5,
        method = "keep_inside"
      )

      isolate({
        if (hasTable) {
          tblDb <- dbGetQuery(grassSession$dbCon, paste("SELECT * FROM", excluTable))
          if (nrow(tblDb) > 0) {
            tbl <- tblDb
            tbl <- tbl[, c("layer", "buffer", "method")]
            tbl$id <- seq_len(nrow(tbl))
          }
        }

        output$exclusionTable <- render_tabulator({
          tabulator(
            data = tbl,
            readOnly = TRUE,
            add_select_column = TRUE,
            return_select_column = TRUE,
            return_select_column_name = "select",
            option = list(
              layout = "fitColumns",
              index = "id"
            ),
            columns = list(
              list(title = "Id", field = "id", visible = FALSE),
              list(title = "Layer", field = "layer"),
              list(title = "Buffer", field = "buffer"),
              list(title = "Method", field = "method")
            )
          )
        })
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_exclusion_init")

# Add exclusion
observeEvent(input$btnAddExclusion,
  {
    amErrorAction(title = "Button add exclusion", {
      proxy <- tabulator_proxy("exclusionTable")
      tbl <- tabulator_to_df(input$exclusionTable_data)
      layer <- input$selExclusion
      buffer <- input$exclusionBuffer
      method <- input$exclusionMethod

      id <- if (isEmpty(tbl$id)) {
        1
      } else {
        max(tbl$id) + 1
      }

      row <- data.frame(
        id = id,
        layer = layer,
        buffer = buffer,
        method = method
      )

      tabulator_add_rows(proxy, row, position = "bottom")
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_exclusion_add")

# Remove unselected exclusions
observeEvent(input$btnRmExcluSelected,
  {
    amErrorAction(title = "Button remove unselected exclusion row", {
      tbl <- tabulator_to_df(input$exclusionTable_data)
      proxy <- tabulator_proxy("exclusionTable")
      ids <- tbl[which(tbl$select), "id"]


      if (isEmpty(ids)) {
        return()
      }
      tabulator_remove_rows(proxy, ids)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_exclusion_rm")

# Initial suitability table
observe(
  {
    amErrorAction(title = "Initial suitability table", {
      selProject <- listen$selProject
      suitTable <- amNameCheck(dataList,
        input$suitabilityTableSelect,
        "table",
        dbCon = grassSession$dbCon
      )
      btnReset <- input$btnResetSuitTable
      hasTable <- isNotEmpty(suitTable)

      tbl <- data.frame(
        id = 1,
        factor = "popsum",
        layer = "rOutputPopulation",
        weight = 1,
        options = "r=1;p=hvms"
      )

      isolate({
        if (hasTable) {
          tblDb <- dbGetQuery(grassSession$dbCon, paste("SELECT * FROM", suitTable))
          if (nrow(tblDb) > 0) {
            tbl <- tblDb
            tbl <- tbl[, c("factor", "layer", "weight", "options")]
            tbl$id <- seq_len(nrow(tbl))
          }
        }

        output$suitabilityTable <- render_tabulator({
          tabulator(
            data = tbl,
            readOnly = TRUE,
            fixedCols = 1,
            stretched = "last",
            add_select_column = TRUE,
            return_select_column = TRUE,
            return_select_column_name = "select",
            options = list(
              index = "id"
            ),
            columns = list(
              list(title = "Id", field = "id", visible = FALSE),
              list(title = "Factor", field = "factor"),
              list(title = "Layer", field = "layer"),
              list(title = "Weight", field = "weight"),
              list(title = "Options", field = "options")
            )
          )
        })
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_suitability_init")

# Add factor to suitability table
observeEvent(input$btnAddFactor,
  {
    amErrorAction(title = "Button add factor", {
      # Import input
      tbl <- na.omit(tabulator_to_df(input$suitabilityTable_data))
      proxy <- tabulator_proxy("suitabilityTable")

      # Init variables
      sep <- ";"
      opt <- character(0)
      layer <- input$selFactorLayer
      fact <- input$selFactor
      weight <- input$factorWeight

      # Set options for population sum and traveltime
      switch(fact,
        "popsum" = {
          opt <- paste0("r=", input$factorPopSumRadius)
        },
        "traveltime" = {
          type <- input$factorTypeAnalysis
          if (type == "aniso") {
            opt <- paste0("d=", input$factorTravelDirection)
          }
          opt <- paste(
            c(
              opt,
              paste0("t=", input$factorTypeAnalysis),
              paste0("k=", input$factorKnight)
            ),
            collapse = sep
          )
        }
      )

      # Set options
      opt <- paste(c(opt, paste0("p=", input$factorDirection)), collapse = sep)

      id <- if (isEmpty(tbl$id)) {
        1
      } else {
        max(tbl$id) + 1
      }

      # Add factor to existing table
      row <- data.frame(
        id = id,
        factor = fact,
        layer = layer,
        weight = weight,
        options = opt
      )

      tabulator_add_rows(proxy, row, position = "bottom")
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_suitability_add")

# Remove unselected factors from suitability table
observeEvent(input$btnRmSuitTableSelected,
  {
    amErrorAction(title = "Button remove selected suitability table row", {
      tbl <- tabulator_to_df(input$suitabilityTable_data)
      proxy <- tabulator_proxy("suitabilityTable")
      ids <- tbl[which(tbl$select), "id"]

      if (isEmpty(ids)) {
        return()
      }
      tabulator_remove_rows(proxy, ids)
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_suitability_rm")

# Extract category from merged landcover raster and add new columns
dataSpeedRasterTable <- reactive({
  idMerged <- input$mergedSelect

  tbl <- data.frame(
    class = as.integer(NA),
    label = as.character(NA),
    speed = as.integer(NA),
    mode = as.character(NA)
  )

  isolate({
    idMerged <- amNameCheck(dataList, idMerged, "raster")
    if (isEmpty(idMerged)) {
      return(tbl)
    }
    lcvMergedCat <- execGRASS("r.category", map = idMerged, intern = T)
    if (isEmpty(lcvMergedCat)) {
      amMsg(session,
        type = "warning",
        title = "speedRasterTableReactive",
        text = sprintf(
          ams("srv_analysis_accessibility_no_category_warning"),
          idMerged
        )
      )
      return(tbl)
    }

    tbl <- read.csv(
      text = lcvMergedCat,
      sep = "\t",
      header = F,
      stringsAsFactors = F
    )
    names(tbl) <- c("class", "label")
    noLabel <- is.na(tbl$label) | is.null(tbl$label)
    tbl[noLabel, "label"] <- paste0("no_label_", as.character(tbl[noLabel, "class"]))
    tbl[, "speed"] <- 0
    tbl[, "mode"] <- as.character(config$defaultTranspMode)
    return(tbl)
  })
})

# Display tabulator table of speed table from raster
observe(
  {
    amErrorAction(title = "Observe speed raster table", {
      tbl <- dataSpeedRasterTable()
      undo <- input$speedTableUndo
      if (isTRUE(nrow(tbl) > 0) || (isTRUE(!is.null(undo)) && isTRUE(undo) > 0)) {
        # Create raster table with original value
        output$speedRasterTable <- render_tabulator({
          tabulator(
            data = tbl,
            readOnly = FALSE,
            fixedCols = 2,
            stretched = "last",
            dropDown = list(
              mode = names(config$listTranspMod)
            )
          )
        })
        # Update selector lcv class to exclude
        updateSelectInput(session,
          "excludeLandCoverClass",
          choices = tbl$class,
          selected = ""
        )
      }
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_data_ldc_classes")

# Render tabulator table from sqlite lcv table
observe(
  {
    # Reactive table for speed/mode values. Empty if none.
    sel <- amNameCheck(dataList,
      input$modelSelect,
      "table",
      dbCon = isolate(grassSession$dbCon)
    )
    isolate({
      if (!is.null(sel)) {
        tbl <- dbGetQuery(grassSession$dbCon, paste("select * from", sel))
        tbl$class <- as.integer(tbl$class)
      } else {
        tbl <- data.frame(
          class = as.integer(NA),
          label = as.character(NA),
          speed = as.integer(NA),
          mode = as.character(NA)
        )
      }
      output$speedSqliteTable <- render_tabulator({
        tabulator(
          data = tbl,
          readOnly = TRUE,
          fixedCols = 2,
          stretched = "last"
        )
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_speed_sqlite_init")

tblSpeedRaster <- reactive({
  tabulator_to_df(input$speedRasterTable_data)
})
tblSpeedSqlite <- reactive({
  tabulator_to_df(input$speedSqliteTable_data)
})

# Create facilities table with additional AccessMod columns
tblHfOrig <- reactive({
  selHf <- amNameCheck(dataList, input$hfSelect, "vector")
  selMerged <- amNameCheck(dataList, input$mergedSelect, "raster")
  selPop <- amNameCheck(dataList, input$popSelect, "raster")
  tblOrig <- tblSpeedRaster()
  updateTable <- listen$updateFacilitiesTables
  isolate({
    return(amGetFacilitiesTable_cached(
      mapHf = selHf,
      mapMerged = selMerged,
      mapPop = selPop,
      mapDem = config$mapDem,
      dbCon = grassSession$dbCon,
      tblSpeed = tblOrig
    ))
  })
})

# Create facilities table for second table (To)
tblHfOrigTo <- reactive({
  selHf <- amNameCheck(dataList, input$hfSelect, "vector")
  selHfTo <- amNameCheck(dataList, input$hfSelectTo, "vector")
  selMerged <- amNameCheck(dataList, input$mergedSelect, "raster")
  selPop <- amNameCheck(dataList, input$popSelect, "raster")
  tblOrig <- tblSpeedRaster()
  updateTable <- listen$updateFacilitiesTables
  isolate({
    # if(input$moduleSelector=='module_4'){
    if (isTRUE(selHf == selHfTo) && isTRUE(nrow(tblHfOrig()) > 0)) {
      return(tblHfOrig())
    } else {
      return(amGetFacilitiesTable_cached(
        mapHf = selHfTo,
        mapMerged = selMerged,
        mapPop = selPop,
        mapDem = config$mapDem,
        dbCon = grassSession$dbCon,
        tblSpeed = tblOrig
      ))
    }
  })
})

# Render facilities table (From)
observe(
  {
    tbl <- tblHfOrig()
    if (!is.null(tbl) && nrow(tbl) > 0) {
      # Choose which columns display first.
      colOrder <- unique(c(
        config$vectorKey,
        "amOnBarrier",
        "amOnZero",
        "amOutsideDem",
        names(tbl)
      ))
      tbl <- tbl[order(tbl$amOnBarrier, decreasing = T), colOrder]
      tbl <- tbl[order(tbl$amOnZero, decreasing = T), colOrder]
      tbl <- tbl[order(tbl$amOutsideDem, decreasing = T), colOrder]
      # render_tabulator converts logical to checkboxes which are always writable.
      # To avoid writing on this logical vector, use plain text:
      tbl$amOnBarrier <- ifelse(sapply(tbl$amOnBarrier, isTRUE), "yes", "no")
      tbl$amOnZero <- ifelse(sapply(tbl$amOnZero, isTRUE), "yes", "no")
      tbl$amOutsideDem <- ifelse(sapply(tbl$amOutsideDem, isTRUE), "yes", "no")
    } else {
      # Display at least a data frame with named columns.
      tbl <- data.frame(
        cat = as.integer(NA),
        amOnBarrier = as.integer(NA),
        amOnZero = as.integer(NA),
        amOutsideDem = as.integer(NA)
      )
    }

    output$hfTable <- render_tabulator({
      tabulator(
        data = tbl,
        readOnly = TRUE,
        fixedCols = 3,
        stretched = "all",
        add_selector_bar = TRUE,
        add_select_column = TRUE,
        return_select_column = TRUE,
        return_select_column_name = "amSelect",
        options = list(
          index = "cat"
        )
      )
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_hf_init")

# Render facilities table (To)
observe(
  {
    amErrorAction(title = "tblHfOrigTo to tabulator", {
      tbl <- tblHfOrigTo()
      if (!is.null(tbl) && nrow(tbl) > 0) {
        # render_tabulator converts logical to checkboxes which are always writable.
        # To avoid writing on this logical vector, use plain text:
        tbl$amOnBarrier <- ifelse(tbl$amOnBarrier == TRUE, "yes", "no")
        tbl$amOnZero <- ifelse(tbl$amOnZero == TRUE, "yes", "no")
        tbl$amOutsideDem <- ifelse(sapply(tbl$amOutsideDem, isTRUE), "yes", "no")
        # Choose which columns display first.
        colOrder <- unique(c(
          config$vectorKey,
          "amOnBarrier",
          "amOnZero",
          "amOutsideDem",
          names(tbl)
        ))
        tbl <- tbl[order(tbl$amOnBarrier, decreasing = T), colOrder]
        tbl <- tbl[order(tbl$amOnZero, decreasing = T), colOrder]
        tbl <- tbl[order(tbl$amOutsideDem, decreasing = T), colOrder]
      } else {
        # Display at least a data frame with named columns.
        tbl <- data.frame(
          cat = as.integer(NA),
          amOnBarrier = as.integer(NA),
          amOnZero = as.integer(NA),
          amOutsideDem = as.integer(NA)
        )
      }
      output$hfTableTo <- render_tabulator({
        tabulator(
          data = tbl,
          readOnly = TRUE,
          fixedCols = 3,
          stretched = "all",
          add_selector_bar = TRUE,
          add_select_column = TRUE,
          return_select_column = TRUE,
          return_select_column_name = "amSelect",
          options = list(
            index = "cat"
          )
        )
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "table_hf_to_init")

# HF table out (From)
tblHfOut <- reactive({
  tbl <- tabulator_to_df(input$hfTable_data)
  if (isNotEmpty(tbl)) {
    tbl[[config$vectorKey]] <- as.integer(tbl[[config$vectorKey]])
  } else {
    tbl <- data.frame()
  }
  return(tbl)
})

# HF subset (From) used in other functions
tblHfSubset <- reactive({
  tbl <- tblHfOut()
  if (isNotEmpty(tbl)) {
    tbl <- tbl[sapply(tbl$amSelect, isTRUE), ]
  } else {
    tbl <- data.frame()
  }
  return(tbl)
})

# HF table (To)
tblHfOutTo <- reactive({
  tbl <- tabulator_to_df(input$hfTableTo_data)
  if (isNotEmpty(tbl)) {
    tbl[[config$vectorKey]] <- as.integer(tbl[[config$vectorKey]])
  } else {
    tbl <- data.frame()
  }
  return(tbl)
})

# HF table subset (To) used in other functions
tblHfSubsetTo <- reactive({
  tbl <- tblHfOutTo()
  if (isNotEmpty(tbl)) {
    tbl <- tbl[sapply(tbl$amSelect, isTRUE), ]
  } else {
    tbl <- data.frame()
  }
  return(tbl)
})

# Speed table merge button enabling

observe(
  {
    amErrorAction(title = "Autocomplete scenario table validation", {
      selP <- listen$selProject
      tblOrig <- tblSpeedRaster()
      tblExt <- tblSpeedSqlite()

      if (TRUE) {
        noDataCheck <- any(sapply(unlist(tblExt), isEmpty))

        validMode <- isTRUE(all(
          tolower(tblExt$mode) %in% tolower(names(config$listTranspMod))
        ))
        labelMatch <- isTRUE(all(tblExt$label %in% tblOrig$label))
        classMatch <- isTRUE(all(as.integer(tblExt$class) %in% as.integer(tblOrig$class)))

        # Validation message

        err <- character(0)
        info <- character(0)
        disableBtn <- TRUE
        warningButton <- TRUE

        if (noDataCheck) err <- c(err, "Empty field found")
        if (!noDataCheck) {
          if (!validMode) {
            info <- c(
              info,
              sprintf(
                ams(
                  id = "srv_analysis_access_unmatched_transport_warning"
                ),
                paste(names(config$listTranspMod), collapse = ","),
                config$defaultTranspMode
              )
            )
          }
          if (!labelMatch || !classMatch) {
            info <- c(
              info,
              ams(
                id = "srv_analysis_access_unmatched_scenario_labels_warning"
              )
            )
          }
        }
        if (length(info) > 0) {
          info <- HTML(paste("<div>",
            icon("info-circle"),
            info,
            "</div>",
            collapse = ""
          ))
        }

        if (length(err) > 0) {
          disBtn <- TRUE
        } else {
          disBtn <- FALSE
        }

        # Send result to UI
        if (length(err) > 0 || length(info) > 0) {
          msgList <- tagList(tags$b("Information:"), err, info)
        } else {
          msgList <- "" # tagList(tags$b('Ready to compute.'))
        }
        amActionLinkToggle(session = session, "speedTableMerge", disable = disBtn)
        output$speedTableMergeValidation <- renderUI(msgList)
      }
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_merge_speed_table")

# Table merge process
observeEvent(input$speedTableMerge,
  {
    amErrorAction(title = "Autocomplete scenario table", {
      tblOrig <- tblSpeedRaster()
      tblExt <- tblSpeedSqlite()
      if (length(tblOrig) == 0 || length(tblExt) == 0) {
        return()
      }
      tblExt <- tblExt[!duplicated(tblExt$class), ]
      classOrig <- as.integer(tblOrig[, "class"])
      tblExt$class <- as.integer(tblExt$class)
      tblMergeOk <- tblExt[tblExt$class %in% classOrig, ]
      tblMergeNo <- tblOrig[!classOrig %in% tblExt$class, ]
      tblMerge <- rbind(tblMergeOk, tblMergeNo)
      tblMerge <- tblMerge[order(tblMerge$class, decreasing = F), ]
      output$speedRasterTable <- render_tabulator({
        tabulator(
          data = tblMerge,
          readOnly = c("class", "label"),
          fixedCols = 2,
          stretched = "all",
          dropDown = list(
            mode = names(config$listTranspMod)
          )
        )
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_merge_table")


# Disable button 'btnComputeAccessibility' each time it's activated
observe(
  {
    btn <- input$btnComputeAccessibility
    if (!is.null(btn) && btn > 0) {
      amActionButtonToggle(
        session = session,
        "btnComputeAccessibility",
        disable = TRUE
      )
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "toggle_compute_if_enabled")

# main function
observeEvent(input$btnComputeAccessibility,
  {
    # progress init message
    msgInit <- ams(
      id = "srv_analysis_accessibility_initialization"
    )

    pbc(
      visible = TRUE,
      percent = 1,
      title = ams(
        id = "srv_analysis_accessibility_progress"
      ),
      text = msgInit
    )

    amErrorAction(
      title = "Accessibility analysis (m2,m3,m4,m6)",
      pBarFinalRm = TRUE,
      {
        # check time
        start <- Sys.time()

        # change this value to show or not the final message
        finished <- FALSE

        # dbcon for tables write
        dbCon <- grassSession$dbCon

        # get current mapset and loc
        currentMapset <- amGrassSessionGetLocation()
        currentLocation <- amGrassSessionGetMapset()

        # msg from analysis
        listWarningAnalysis <- c()

        # invalidate data list
        amUpdateDataList(listen)

        # input table
        tbl <- tblSpeedRaster()
        tblHf <- tblHfOut()

        if (input$moduleSelector == "module_4") {
          tblHfTo <- tblHfOutTo()
        }

        # input maps
        mapMerged <- amNameCheck(dataList, input$mergedSelect, "raster")
        mapHf <- amNameCheck(dataList, input$hfSelect, "vector")
        mapHfTo <- amNameCheck(dataList, input$hfSelectTo, "vector")
        mapPop <- amNameCheck(dataList, input$popSelect, "raster")
        mapPopResidual <- amNameCheck(dataList, input$popResidualSelect, "raster")
        mapZoneAdmin <- amNameCheck(dataList, input$zoneSelect, "vector")

        # field selection
        hfIdx <- input$hfIdxField
        hfIdxInt <- input$hfIdxIntField
        hfLab <- input$hfNameField
        hfIdxTo <- input$hfIdxFieldTo
        hfLabTo <- input$hfNameFieldTo
        zoneFieldLabel <- input$zoneLabel
        zoneFieldId <- input$zoneId
        capField <- input$hfCapacityField
        orderField <- input$hfOrderColumn

        # parameters
        maxTravelTime <- input$maxTravelTime
        maxTravelTimeOrder <- input$maxTravelTimeProcOrder
        dirAnalysis <- input$dirAnalysis
        typeAnalysis <- input$typeAnalysis
        knightMove <- input$checkKnightMove
        addNearest <- input$showAdvancedTools && input$checkWithNearest
        limitClosest <- input$checkReferralLimitClosest
        useParallel <- input$checkReferralParallel
        snapToGrid <- input$checkReferralSnapToGrid
        permuteGroups <- input$checkReferralPermute
        keepNetDist <- input$checkReferralKeepNetwork
        useMaxSpeedMask <- input$checkWithSpeedMask
        selectedAnalysis <- input$moduleSelector
        hfOrder <- input$hfOrder
        hfOrderSorting <- input$hfOrderSorting
        popBuffer <- input$popBufferRadius
        modParam <- input$mod3param
        keepFullHfTable <- FALSE
        configSettingsOnly <- isTRUE(input$checkComputeConfigAnalysisOnly)
        roundingMethod <- ifelse(input$checkUseLegacyRounding, "floor", "ceil")

        # Region optimisation

        # scaling up only additional tables
        if (input$moduleSelector == "module_6") {
          tblCapacity <- na.omit(tabulator_to_df(input$capacityTable))
          tblExclusion <- na.omit(tabulator_to_df(input$exclusionTable))
          tblSuitability <- na.omit(tabulator_to_df(input$suitabilityTable))
          # radio button character value..
          useExistingHf <- input$useExistingHf == "TRUE"
          maxScUpNewHf <- input$maxScUpNewHf
          maxScUpPopGoal <- input$maxScUpPopGoal
          maxScUpTime <- input$maxScUpTime

          maxProcessingTime <- input$maxProcessingTime
          rmPotentialPop <- TRUE
        }

        # logic
        # return path = towards facilities.
        towardsFacilities <- ifelse(dirAnalysis == "toHf", TRUE, FALSE)


        # set output names.
        tag <- amGetUniqueTags(input$costTag)
        mapSpeed <- amAddTag("rSpeed", tag, T, F)
        mapFriction <- amAddTag("rFriction", tag, T, F)
        mapCumulative <- amAddTag("rTravelTime", tag, T, F)
        mapNearest <- amAddTag("rNearest", tag, T, F)
        mapPopResidualOut <- amAddTag("rPopulationResidual", tag, T, F)
        hfCatchment <- amAddTag("vCatchment", tag, T, F)
        hfCatchmentNew <- amAddTag("vCatchmentNew", tag, T, F)
        mapPopOnBarrier <- amAddTag("rPopulationOnBarrier", tag, T, F)
        mapNetDist <- amAddTag("vReferralNetwork", tag, T, F)
        tableModel <- amAddTag("tScenarioOut", tag, T, F)
        tableCapacityOut <- amAddTag("tCapacityOut", tag, T, F)
        tableCapacityStat <- amAddTag("tCapacityStat", tag, T, F)
        tableCapacityStatNew <- amAddTag("tCapacityStatNew", tag, T, F)
        tableZonalStat <- amAddTag("tZonalStat", tag, T, F)
        tableReferral <- amAddTag("tReferral", tag, T, F)
        tableReferralNearestDist <- amAddTag("tReferralDist", tag, T, F)
        tableReferralNearestTime <- amAddTag("tReferralTime", tag, T, F)
        mapNewHf <- amAddTag("vFacilityNew", tag, T, F)
        tableExclOut <- amAddTag("tExclusionOut", tag, T, F)
        tableSuitOut <- amAddTag("tSuitabilityOut", tag, T, F)
        nameAnalysisParam <- amAddTag("lAnalysisParameters", tag, T, F)

        #
        # Start processing data
        #
        amDebugMsg(sprintf(
          ams("srv_analysis_access_processing_data"),
          typeAnalysis,
          input$moduleSelector
        ))

        #
        # Save tables
        #
        dbWriteTable(
          dbCon,
          tableModel,
          tbl,
          overwrite = TRUE
        )

        if (selectedAnalysis == "module_6") {
          dbWriteTable(
            dbCon,
            tableCapacityOut,
            tblCapacity,
            overwrite = TRUE
          )

          dbWriteTable(
            dbCon,
            tableSuitOut,
            tblSuitability,
            overwrite = TRUE
          )

          dbWriteTable(
            dbCon,
            tableExclOut,
            tblExclusion,
            overwrite = TRUE
          )
        }

        #
        # Start analysis
        #
        switch(selectedAnalysis,
          "module_2" = {
            timeoutValueInteger <- -1L

            pBarTitle <- ams(
              id = "srv_analysis_accessibility_start_analysis"
            )

            msg <- ams(
              id = "srv_analysis_accessibility_processing_facilities_warning"
            )

            pbc(
              visible = TRUE,
              percent = 1,
              title = pBarTitle,
              text = msg
            )
            # keep only
            if (!keepFullHfTable) {
              tblHf <- amTableSubsetCols(tblHf, c(
                config$vectorKey,
                "amSelect"
              ))
            }

            args <- list(
              inputHf = mapHf,
              inputMerged = mapMerged,
              outputSpeed = mapSpeed,
              outputFriction = mapFriction,
              outputTravelTime = mapCumulative,
              outputNearest = mapNearest,
              typeAnalysis = typeAnalysis,
              knightMove = knightMove,
              addNearest = addNearest,
              joinField = hfIdxInt,
              towardsFacilities = towardsFacilities,
              maxTravelTime = maxTravelTime,
              useMaxSpeedMask = useMaxSpeedMask,
              timeoutValue = timeoutValueInteger,
              tableScenario = tbl,
              tableFacilities = tblHf,
              roundingMethod = roundingMethod
            )

            amAnalysisReplaySave(
              name = nameAnalysisParam,
              mapset = currentMapset,
              location = currentLocation,
              timestamp = Sys.time(),
              analysis = "amTravelTimeAnalysis",
              args = args,
              overwrite = TRUE,
              output = c(
                mapSpeed,
                mapFriction,
                mapCumulative,
                mapNearest,
                nameAnalysisParam
              )
            )

            if (!configSettingsOnly) {
              do.call("amTravelTimeAnalysis", args)

              #
              # Check for timeout  -1
              #
              statCumulative <- amGetRasterStat_cached(mapCumulative, "min")
              hasTimeout <- timeoutValueInteger %in% statCumulative
              if (hasTimeout) {
                msg <- ""
                maxVal <- 0
                if (maxTravelTime == 0) {
                  maxVal <- 2^16 / 2 - 1
                } else {
                  maxVal <- 2^32 / 2 - 1
                }
                msg <- sprintf(
                  ams(
                    id = "srv_analysis_accessibility_longer_travel_time_warning"
                  ),
                  maxVal
                )
                listWarningAnalysis <- c(listWarningAnalysis, msg)
              }
            }

            pbc(
              visible = TRUE,
              percent = 100,
              title = pBarTitle,
              text = ams(
                id = "srv_analysis_accessibility_process_finished_timeout"
              )
            )

            pbc(
              visible = FALSE,
            )
            #
            # Finished without error
            #
            finished <- TRUE
          },
          "module_3" = {
            amErrorAction(
              title = "Geographic coverage analysis",
              pBarFinalRm = TRUE,
              {
                if (!keepFullHfTable) {
                  tblHf <- amTableSubsetCols(tblHf, c(
                    config$vectorKey,
                    "amSelect",
                    orderField,
                    capField,
                    hfIdx,
                    hfLab
                  ))
                }

                args <- list(
                  inputMerged = mapMerged,
                  inputPop = mapPop,
                  inputHf = mapHf,
                  inputZoneAdmin = mapZoneAdmin,
                  outputPopResidual = mapPopResidualOut,
                  outputHfCatchment = hfCatchment,
                  outputPopBarrier = mapPopOnBarrier,
                  outputTableCapacity = tableCapacityStat,
                  outputTableZonal = tableZonalStat,
                  outputSpeed = mapSpeed,
                  outputFriction = mapFriction,
                  typeAnalysis = typeAnalysis,
                  knightMove = knightMove,
                  removeCapted = "rmPop" %in% modParam,
                  vectCatch = "vectCatch" %in% modParam,
                  popOnBarrier = "popBarrier" %in% modParam,
                  towardsFacilities = towardsFacilities,
                  radius = popBuffer,
                  maxTravelTime = maxTravelTime,
                  maxTravelTimeOrder = maxTravelTimeOrder,
                  useMaxSpeedMask = useMaxSpeedMask,
                  hfIdx = hfIdx,
                  nameField = hfLab,
                  capField = capField,
                  orderField = orderField,
                  ignoreCapacity = "ignoreCapacity" %in% modParam,
                  addColumnPopOrigTravelTime =
                    "addColumnPopOrigTravelTime" %in% modParam,
                  addColumnsPopCoverageExtended =
                    "addColumnsPopCoverageExtended" %in% modParam,
                  zonalCoverage = "zonalPop" %in% modParam,
                  zoneFieldId = zoneFieldId,
                  zoneFieldLabel = zoneFieldLabel,
                  hfOrder = hfOrder,
                  hfOrderSorting = hfOrderSorting,
                  tableScenario = tbl,
                  tableFacilities = tblHf,
                  roundingMethod = roundingMethod
                )

                amAnalysisReplaySave(
                  name = nameAnalysisParam,
                  mapset = currentMapset,
                  location = currentLocation,
                  timestamp = Sys.time(),
                  analysis = "amCapacityAnalysis",
                  args = args,
                  overwrite = TRUE,
                  output = c(
                    mapPopResidualOut,
                    hfCatchment,
                    mapPopOnBarrier,
                    tableCapacityStat,
                    tableZonalStat,
                    mapSpeed,
                    mapFriction,
                    nameAnalysisParam
                  )
                )

                if (!configSettingsOnly) {
                  do.call("amCapacityAnalysis", args)
                }
                finished <- TRUE
              }
            )
          },
          "module_4" = {
            if (!keepFullHfTable) {
              tblHf <- amTableSubsetCols(tblHf, c(
                "amSelect",
                config$vectorKey,
                hfIdx,
                hfLab
              ))
              tblHfTo <- amTableSubsetCols(tblHfTo, c(
                "amSelect",
                config$vectorKey,
                hfIdxTo,
                hfLabTo
              ))
            }

            args <- list(
              inputHfFrom = mapHf,
              inputHfTo = mapHfTo,
              inputMerged = mapMerged,
              outputSpeed = mapSpeed,
              outputFriction = mapFriction,
              outputReferral = tableReferral,
              outputNearestDist = tableReferralNearestDist,
              outputNearestTime = tableReferralNearestTime,
              outputNetDist = mapNetDist,
              maxTravelTime = maxTravelTime,
              useMaxSpeedMask = useMaxSpeedMask,
              idField = hfIdx,
              labelField = hfLab,
              idFieldTo = hfIdxTo,
              labelFieldTo = hfLabTo,
              typeAnalysis = typeAnalysis,
              knightMove = knightMove,
              limitClosest = limitClosest,
              parallel = useParallel,
              permuteGroups = permuteGroups,
              keepNetDist = keepNetDist,
              snapToGrid = snapToGrid,
              resol = listen$mapMeta$grid$nsres,
              unitCost = "m",
              unitDist = "km",
              tableScenario = tbl,
              tableFacilities = tblHf,
              tableFacilitiesTo = tblHfTo,
              roundingMethod = roundingMethod
            )

            amAnalysisReplaySave(
              name = nameAnalysisParam,
              mapset = currentMapset,
              location = currentLocation,
              timestamp = Sys.time(),
              analysis = "amAnalysisReferral",
              args = args,
              overwrite = TRUE,
              output = c(
                mapSpeed,
                mapFriction,
                tableReferral,
                tableReferralNearestTime,
                tableReferralNearestDist,
                mapNetDist,
                nameAnalysisParam
              )
            )

            if (!configSettingsOnly) {
              do.call("amAnalysisReferral", args)
            }

            #
            # Fnished without error
            #
            finished <- TRUE
          },
          "module_6" = {
            amErrorAction(
              title =
                "Accessibility analysis (m6)",
              pBarFinalRm = TRUE,
              {
                titleAnalysis <- sprintf(
                  ams("srv_analysis_access_scaleup_analysis_title"),
                  input$moduleSelector
                )

                if (!keepFullHfTable) {
                  tblHf <- amTableSubsetCols(tblHf, c(
                    "amSelect",
                    config$vectorKey,
                    hfIdx,
                    hfLab
                  ))
                }

                args <- list(
                  inputMerged = mapMerged,
                  inputPop = mapPop,
                  inputPopResidual = mapPopResidual,
                  inputFacility = mapHf,
                  outputFriction = mapFriction,
                  outputSpeed = mapSpeed,
                  outputFacility = mapNewHf,
                  outputPopResidual = mapPopResidualOut,
                  outputCatchment = hfCatchmentNew,
                  outputCapacityAnalysis = tableCapacityStatNew,
                  pBarTitle = titleAnalysis,
                  maxTravelTime = maxTravelTime,
                  useMaxSpeedMask = useMaxSpeedMask,
                  useExistingFacilities = useExistingHf,
                  typeAnalysis = typeAnalysis,
                  knightMove = knightMove,
                  limitFacilitiesNumber = maxScUpNewHf,
                  limitProcessingTime = maxScUpTime,
                  limitPopCoveragePercent = maxScUpPopGoal,
                  tableFacilities = tblHf,
                  tableScenario = tbl,
                  tableCapacity = tblCapacity,
                  tableExclusion = tblExclusion,
                  tableSuitability = tblSuitability,
                  roundingMethod = roundingMethod
                )

                amAnalysisReplaySave(
                  name = nameAnalysisParam,
                  mapset = currentMapset,
                  location = currentLocation,
                  timestamp = Sys.time(),
                  analysis = "amAnalysisScalingUp",
                  args = args,
                  overwrite = TRUE,
                  output = c(
                    mapFriction,
                    mapSpeed,
                    mapPopResidualOut,
                    hfCatchmentNew,
                    tableCapacityStatNew,
                    nameAnalysisParam
                  )
                )

                if (!configSettingsOnly) {
                  do.call("amAnalysisScalingUp", args)
                }
                finished <- TRUE
              }
            )
          }
        )

        pbc(
          visible = FALSE,
        )


        if (finished) {
          if (configSettingsOnly) {
            amMsg(session,
              type = "message",
              title = ams(
                "srv_analysis_accessibility_process_finished_message"
              ),
              text = ams(
                "srv_analysis_accessibility_process_finished_message_config_only"
              )
            )
          } else {
            #
            # Subset out file according th the internal data option in settings
            #
            internal <- input$internalDataChoice
            classes <- config$dataClass
            classes <- classes[
              classes$internal == FALSE |
                classes$internal == internal,
            ]$class
            allFiles <- listen$outputNames$file
            areSelected <- amGetClass(allFiles) %in% classes
            allFiles <- listen$outputNames$file[areSelected]
            allFilesUI <- listen$outputNames$ui[areSelected]

            #
            # Trigger outFiles listener
            #
            listen$outFiles <- allFiles

            #
            # Remove old tags
            #
            updateTextInput(session, "costTag", value = "")

            #
            # Create ui output message.
            #
            outputDatasets <- tags$ul(
              HTML(paste("<li>", allFilesUI, "</li>"))
            )
            if (length(listWarningAnalysis) > 0) {
              outputWarnings <- tags$ul(
                HTML(paste("<li>", listWarningAnalysis, "</li>"))
              )
            } else {
              outputWarnings <- ""
            }

            timing <- round(
              difftime(
                Sys.time(),
                start,
                units = "m"
              ),
              3
            )
            msg <- sprintf(
              ams(
                id = "srv_analysis_accessibility_process_finished_timing"
              ),
              timing
            )
            msg <- tagList(
              p(msg),
              outputDatasets,
              outputWarnings
            )
            amMsg(session,
              type = "message",
              title = ams(
                id = "srv_analysis_accessibility_process_finished_message"
              ),
              text = msg
            )
          }
        }
      }
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_compute")


## module 5

# update slider input
observe(
  {
    travelTimeSelect <- amNameCheck(
      dataList,
      input$travelTimeSelect,
      "raster"
    )
    isolate({
      if (!is.null(travelTimeSelect)) {
        # updateSliderInput(session,'sliderTimeAnalysis',
        updateNumericInput(session, "zonalStatMaxTT",
          max = ceiling(amGetRasterStat_cached(travelTimeSelect, "max")),
          min = floor(amGetRasterStat_cached(travelTimeSelect, "min")),
          step = 1
        )
      }
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_time_numeric_input")



# prepare zonal map for later use : select zone where we have at least one HF.



observeEvent(input$btnZonalStat,
  {
    amErrorAction(title = "Zonal stat", {
      # result list
      res <- list()

      mapZone <- amNameCheck(
        dataList,
        input$zoneSelect,
        "vector"
      )
      mapPop <- amNameCheck(
        dataList,
        input$popSelect,
        "raster"
      )
      mapTravelTime <- amNameCheck(
        dataList,
        input$travelTimeSelect,
        "raster"
      )
      fieldZoneLabel <- input$zoneLabel
      fieldZoneId <- input$zoneId


      if (
        !is.null(mapZone) &&
          !is.null(mapTravelTime) &&
          isTRUE(nchar(fieldZoneId) > 0) &&
          isTRUE(nchar(fieldZoneLabel) > 0) &&
          isTRUE(input$moduleSelector == "module_5")
      ) {
        minTravelTime <- amGetRasterStat_cached(mapTravelTime, c("min"))
        maxTravelTime <- amGetRasterStat_cached(mapTravelTime, c("max"))
        timeCumCosts <- amSplitToNum(
          input$textTimeCumCosts,
          min = minTravelTime,
          max = maxTravelTime
        )

        #
        # Generate table
        #
        res <- amZonalAnalysis_cached(
          inputTravelTime = mapTravelTime,
          inputPop        = mapPop,
          inputZone       = mapZone,
          timeCumCosts    = timeCumCosts,
          zoneIdField     = fieldZoneId,
          zoneLabelField  = fieldZoneLabel
        )

        if (input$checkZoneTableWide && !isTRUE(res$empty)) {
          dt <- as.data.table(res$table)
          formText <- paste(
            paste(
              fieldZoneId,
              fieldZoneLabel,
              "popTotal",
              sep = "+"
            ),
            "time_m",
            sep = "~"
          )
          form <- as.formula(formText)
          tblCast <- dcast(dt, form, value.var = list("popTravelTime", "popCoveredPercent"))
          res$table <- as.data.frame(tblCast)
        }

        #
        # Zonal stat table
        #
        output$zoneCoverageTable <- renderHotable(
          {
            res$table
          },
          readOnly = TRUE,
          fixed = 1
        )
      }
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "btn_zonal_stat")
