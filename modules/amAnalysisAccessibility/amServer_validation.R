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

# Preventive field validation
# NOTE: This validation step was written for one module.
# With almost all modules depending on it, this should be rewritten.
idModule <- "module_analysis"

observe(
  {
    amErrorAction(title = "Module 2,3,4,6: validation", {
      #
      # Init messages
      #
      err <- character(0)
      info <- character(0)
      dubious <- character(0)
      out <- character(0)
      msgList <- character(0)

      #
      # Store current module
      #

      module2 <- isTRUE(input$moduleSelector == "module_2") # Accessibility
      module3 <- isTRUE(input$moduleSelector == "module_3") # Coverage
      module4 <- isTRUE(input$moduleSelector == "module_4") # Referral
      module5 <- isTRUE(input$moduleSelector == "module_5") # Zonal
      module6 <- isTRUE(input$moduleSelector == "module_6") # Scaling up

      isAnisotropic <- isTRUE(input$typeAnalysis == "anisotropic")
      isIsotropic <- isTRUE(input$typeAnalysis == "isotropic")

      addNearest <- module2 && input$showAdvancedTools && input$checkWithNearest
      missingNearestIdx <- addNearest && isEmpty(input$hfIdxIntField)

      if (module5) {
        ttInRange <- TRUE
        ttZero <- FALSE
        maxTT <- 0

        # Check if data exist
        layerOkTT <- isTRUE(!is.null(amNameCheck(dataList, input$travelTimeSelect, "raster")))
        layerOkZones <- isTRUE(!is.null(amNameCheck(dataList, input$zoneSelect, "vector")))
        layerOkPop <- isTRUE(!is.null(amNameCheck(dataList, input$popSelect, "raster")))

        if (layerOkTT) {
          maxTT <- ceiling(amGetRasterStat_cached(input$travelTimeSelect, c("max")))
          minTT <- floor(amGetRasterStat_cached(input$travelTimeSelect, c("min")))
          strTT <- input$textTimeCumCosts
          selectTT <- amSplitToNum(strTT, default = NULL)
          ttInRange <- isNotEmpty(selectTT) && all(selectTT <= maxTT & selectTT > minTT)
          ttZero <- maxTT == 0

          #
          # Update min / max value in desc div
          #
          amUpdateText("txtZonalMinMax",
            text = sprintf(
              "min=%1$s, max=%2$s",
              minTT + 1,
              maxTT
            )
          )
        }
      } else {
        #
        # Clean tags
        #

        tagsClean <- amGetUniqueTags(input$costTag)

        #
        # Control maps and values
        #

        # Check if data exist
        merged <- isTRUE(!is.null(amNameCheck(dataList, input$mergedSelect, "raster")))
        hf <- isTRUE(!is.null(amNameCheck(dataList, input$hfSelect, "vector")))
        pop <- isTRUE(!is.null(amNameCheck(dataList, input$popSelect, "raster")))
        popRes <- isTRUE(!is.null(amNameCheck(dataList, input$popResSelect, "raster")))

        # Table validation
        hfOnBarrier <- any(tblHfSubset()$amOnBarrier == "yes")
        if(!is.logical(hfOnBarrier)){
          browser()
        }
        hfOnZero <- any(tblHfSubset()$amOnZero == "yes")
        hfOutsideDem <- any(tblHfSubset()$amOutsideDem == "yes")
        tblSpeedHasZero <- isTRUE(0 %in% tblSpeedRaster()$speed)

        if (module4) {
          hfOnBarrier <- hfOnBarrier || any(tblHfSubsetTo()$amOnBarrier == "yes")
          hfOnZero <- hfOnZero || any(tblHfSubsetTo()$amOnZero == "yes")
          hfOutsideDem <- hfOutsideDem || any(tblHfSubsetTo()$amOutsideDem == "yes")
          refLimitClosest <- isTRUE(input$checkReferralLimitClosest)
          refKeepNetDistLayer <- isTRUE(input$checkReferralKeepNetwork)
        }

        # Check if there is at least one facility selected.
        hfNoSelected <- isTRUE(!any(tblHfSubset()$amSelect))
        hfNoSelectedTo <- isTRUE(!any(tblHfSubsetTo()$amSelect))
        # Parameter validation
        unlimitedTT <- isTRUE(input$maxTravelTime == 0)
        wrongTT <- isTRUE(
          !is.numeric(input$maxTravelTime) ||
            isEmpty(input$maxTravelTime) ||
            input$maxTravelTime < 0 ||
            input$maxTravelTime > 2147483647
        )
        #
        # Parameters control.
        #
        if (module3) {
          hfIdx <- isTRUE(nchar(input$hfIdxField) > 0)
          capField <- isTRUE(nchar(input$hfCapacityField) > 0)
          hfBuffer <- isTRUE(input$hfOrder == "circBuffer")
          popBuffer <- isTRUE(input$popBufferRadius > listen$mapMeta$grid$nsres)
          zonalPop <- isTRUE("zonalPop" %in% input$mod3param)
          ignoreCapacity <- isTRUE("ignoreCapacity" %in% input$mod3param)
          popBarrier <- isTRUE("popBarrier" %in% input$mod3param)

          if (zonalPop) {
            zonalSelect <- isTRUE(!is.null(amNameCheck(dataList, input$zoneSelect, "vector")))
            zoneId <- isTRUE(length(input$zoneId) > 0)
            zoneLabel <- isTRUE(length(input$zoneLabel) > 0)
          }

          if (capField) {
            capacities <- tblHfSubset()[[input$hfCapacityField]]
            capacitiesNotValid <- any(is.na(capacities)) || any(is.null(capacities))
          }

          hfOrderInconsistency <- isTRUE(input$hfOrder != "tableOrder" && !"rmPop" %in% input$mod3param)
          zonalCoverageInconsistency <- isTRUE(zonalPop && !"rmPop" %in% input$mod3param)
        }

        if (module6) {
          tblCapTypeOk <- TRUE
          tblCapMissingOk <- TRUE
          tblCapOverlapOK <- TRUE
          tblCapInRangeOk <- TRUE
          tblCapGreaterThanPrevOk <- TRUE
          tblCapWithoutButHfSelect <- FALSE
          tblSuitOk <- FALSE
          tblSuitOnlyDynFac <- FALSE
          tblSuitLayerMissing <- character(0)
          tblSuitLayerOk <- TRUE
          tblExclOk <- FALSE
          tblExclLayerMissing <- character(0)
          tblExclLayerOk <- TRUE
          tblCapBeginWithZero <- TRUE
          tblCapMinMaxOk <- TRUE
          tblCapLabelOk <- TRUE
          popSelect <- TRUE
          maxScUpPopGoalNoLimit <- FALSE
          maxScUpTimeNoLimit <- FALSE
          maxScUpHfNoLimit <- FALSE
          allScUpNoLimit <- FALSE

          # Replace hotToDf with tabulator_to_df
          tblCapacityNew <- tabulator_to_df(input$capacityTable_data)
          tblSuit <- tabulator_to_df(input$suitabilityTable_data)
          tblExcl <- tabulator_to_df(input$exclusionTable_data)

          withoutFacility <- isTRUE(input$useExistingHf == "FALSE")
          popResidualIsResidual <- isTRUE(amGetClass(input$popResidualSelect) == "rPopulationResidual")

          popNotResidualButHfSelect <- FALSE
          popResidualButNoHfSelect <- FALSE
          # Options
          maxScUpHf <- input$maxScUpNewHf
          maxScUpTime <- input$maxScUpTime
          maxScUpPopGoal <- input$maxScUpPopGoal

          # Auto correction
          if (isTRUE(maxScUpPopGoal > 100)) {
            updateNumericInput(session, "maxScUpPopGoal", value = 100)
          }

          maxScUpHfNoLimit <- isTRUE(maxScUpHf < 1)
          maxScUpTimeNoLimit <- isTRUE(maxScUpTime < 1)
          maxScUpPopGoalNoLimit <- isTRUE(maxScUpPopGoal < 1)

          allScUpNoLimit <- all(
            c(
              maxScUpPopGoalNoLimit,
              maxScUpHfNoLimit,
              maxScUpTimeNoLimit
            )
          )

          if (withoutFacility) {
            if (!hfNoSelected && hf) {
              tblCapWithoutButHfSelect <- TRUE
            }
            # Manually validate hf layer and hf on barrier.
            hfNoSelected <- FALSE
            hfOnBarrier <- FALSE
            hfOnZero <- FALSE
            hfOutsideDem <- FALSE
            hf <- TRUE
          } else {
            # If there is hf select without a population residual
            if (!hfNoSelected && !popResidualIsResidual) {
              popNotResidualButHfSelect <- TRUE
            }
            if (hfNoSelected && popResidualIsResidual) {
              popResidualButNoHfSelect <- TRUE
            }
          }

          # Validate suitability table
          if (!is.null(tblSuit)) {
            tblSuitOk <- nrow(na.omit(tblSuit)) > 0
          }
          if (tblSuitOk) {
            # If without facility and all layer in suitability are dynamic facility
            tblSuitOnlyDynFac <- withoutFacility && all(tblSuit$layer == config$dynamicFacilities) && !hfNoSelected && hf

            # Validate layer names
            suitLayers <- tblSuit$layer[!tblSuit$layer %in% config$dynamicLayers]
            tblSuitLayerMissing <- suitLayers[!sapply(suitLayers, amMapExists)]
            if (length(tblSuitLayerMissing) > 0) {
              tblSuitLayerOk <- isTRUE(length(tblSuitLayerMissing) == 0)
            }
          }

          if (!is.null(tblExcl)) {
            tblExclOk <- TRUE
          }

          if (tblExclOk) {
            exclLayers <- tblExcl$layer[!tblExcl$layer %in% config$dynamicLayers]
            if (length(tblExclLayerMissing) > 0) {
              tblExclLayerMissing <- exclLayers[!sapply(exclLayers, amMapExists)]
              tblExclLayerOk <- isTRUE(length(tblSuitLayerMissing) == 0)
            }
          }

          # Validate null
          if (!is.null(tblCapacityNew)) {
            # Validate missing value
            tblCapMissingOk <- isTRUE(all(
              sapply(tblCapacityNew, function(x) {
                all(stringr::str_length(x) > 0)
              })
            ))

            if (tblCapMissingOk) {
              # Validate type
              tblCapTypeOk <- all(
                is.numeric(tblCapacityNew$min),
                is.numeric(tblCapacityNew$max),
                is.numeric(tblCapacityNew$capacity),
                is.character(tblCapacityNew$label)
              )
            }
            # Validate overlap min max and capacity in range.
            if (tblCapMissingOk) {
              # Max greater than min
              tblCapMinMaxOk <- all(tblCapacityNew$min < tblCapacityNew$max)
              tblCapBeginWithZero <- isTRUE(tblCapacityNew$min[1] == 0)
              # Checking previous row values
              nR <- nrow(tblCapacityNew)
              if (nR > 1) {
                for (i in 2:nR) {
                  # Capacity is greater than previous capacity
                  tblCapGreaterThanPrevOk <- all(
                    tblCapGreaterThanPrevOk,
                    isTRUE(tblCapacityNew[i, "capacity"] > tblCapacityNew[i - 1, "capacity"])
                  )
                  # Min max+1 overlap
                  tblCapOverlapOK <- all(
                    tblCapOverlapOK,
                    isTRUE(tblCapacityNew[i, "min"] > tblCapacityNew[i - 1, "max"])
                  )
                }
              }
              # Capacity in min max range
              tblCapInRangeOk <- isTRUE(
                all(tblCapacityNew$capacity <= tblCapacityNew$max &
                  tblCapacityNew$capacity >= tblCapacityNew$min)
              )
              # Unique labels
              tblCapLabelOk <- isTRUE(length(unique(tblCapacityNew$label)) ==
                length(tblCapacityNew$label))
            }
          }
        }
      }

      #
      # Collect messages in err and info
      #


      if (module5) {
        #
        # zonal analysis
        #
        if (!ttZero && !ttInRange) {
          err <- c(
            err,
            sprintf(
              ams("srv_analysis_accessibility_travel_time_input"),
              minTT + 1,
              maxTT
            )
          )
        }
        if (ttZero) {
          err <- c(
            err,
            sprintf(
              ams("srv_analysis_accessibility_travel_time_input_zero")
            )
          )
        }
        if (!layerOkZones) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_missing_zone")
          )
        }
        if (!layerOkPop) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_missing_population")
          )
        }
        if (!layerOkTT) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_missing_travel_time")
          )
        }
      } else {
        #
        # Other modules
        #
        if (missingNearestIdx) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_missing_nearest_idx")
          )
        }
        if (wrongTT) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_max_travel_time_input")
          )
        }
        if (!hf) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_missing_facility_layer")
          )
        }
        if (hfOnBarrier) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_facilities_on_barrier")
          )
        }
        if (hfOnZero) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_facilities_on_0kmh")
          )
        }
        if (hfOutsideDem) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_facilities_outside_dem")
          )
        }
        if (!merged) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_missing_merged_lc_warning")
          )
        }
        if (unlimitedTT && !module4) {
          info <- c(
            info,
            ams("srv_analysis_accessibility_max_travel_time_set_0min")
          )
        }
        if (unlimitedTT && module2) {
          info <- c(
            info,
            ams("srv_analysis_accessibility_max_travel_time_warning")
          )
        }
        if (unlimitedTT && !module2 && !module4) {
          info <- c(
            info,
            ams("srv_analysis_accessibility_travel_time_>32727_ignored")
          )
        }
        if (module2 | module6) {
          if (hfNoSelected) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_select_facilities")
            )
          }
        }
        if (module3 | module6) {
          if (!pop) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_select_population")
            )
          }
        }

        if (module3) {
          if (tblSpeedHasZero) {
            info <- c(
              info,
              ams("srv_analysis_accessibility_tbl_speed_has_zero")
            )
          }

          if (!hfIdx) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_no_group_warning")
            )
          }
          if (hfNoSelected) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_select_one_facility_warning")
            )
          }
          if (!capField && !ignoreCapacity) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_set_capacity_warning")
            )
          }
          if (!ignoreCapacity && capField && capacitiesNotValid) {
            err <- c(err, ams("srv_analysis_accessibility_capacities_not_valid"))
          }

          if (hfBuffer) {
            if (!popBuffer) {
              err <- c(
                err,
                ams("srv_analysis_accessibility_circular_buffer_warning")
              )
            }
          }
          if (!popBarrier) info <- c(info, ams("srv_analysis_accessibility_no_pop_barrier_warning"))
          if (hfOrderInconsistency) {
            info <- c(
              info,
              ams("srv_analysis_accessibility_facilities_processing_order")
            )
          }
          if (zonalPop) {
            if (!zonalSelect) {
              err <- c(
                err,
                ams("srv_analysis_accessibility_select_zone_warning")
              )
            }
            if (!zoneId) {
              err <- c(
                err,
                ams("srv_analysis_accessibility_zonal_id_missing")
              )
            }
            if (!zoneLabel) {
              err <- c(
                err,
                ams("srv_analysis_accessibility_zonal_label_missing")
              )
            }
          }
          if (zonalCoverageInconsistency) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_remove_covered_pop_warning")
            )
          }

          #
          # if check population
          #
          # population on barrier

          if (isTRUE(length(err) < 1) &&
            isTRUE(popOnBarrierStat()$sum > 0)) {
            info <- c(
              info,
              sprintf(
                ams("srv_analysis_accessibility_pop_on_barrier_removed"),
                popOnBarrierStat()$cells,
                popOnBarrierStat()$sum,
                popOnBarrierStat()$percent
              )
            )
          }
        }
        if (module4) {
          if (hfNoSelected) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_select_one_facility_from")
            )
          }
          if (hfNoSelectedTo) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_select_one_facility_to")
            )
          }
        }
        if (module6) {
          if (allScUpNoLimit) {
            info <- c(
              info,
              ams("srv_analysis_accessibility_scaling_up_unlimited")
            )
          } else {
            if (maxScUpPopGoalNoLimit) {
              info <- c(
                info,
                ams("srv_analysis_accessibility_coverage_100percent")
              )
            }
            if (maxScUpTimeNoLimit) {
              info <- c(
                info,
                ams("srv_analysis_accessibility_processing_unlimited")
              )
            }
            if (maxScUpHfNoLimit) {
              info <- c(
                info,
                ams("srv_analysis_accessibility_new_facilities_unlimited")
              )
            }
          }

          if (popNotResidualButHfSelect) {
            dubious <- c(
              dubious,
              ams("srv_analysis_accessibility_facilities_residual_pop_warning")
            )
          }
          if (popResidualButNoHfSelect) {
            dubious <- c(
              dubious,
              ams("srv_analysis_accessibility_residual_pop_no_facilities")
            )
          }
          if (!withoutFacility) {
            info <- c(
              info,
              ams("srv_analysis_accessibility_selected_facilities_verification")
            )
          }
          # if(hfNoSelected && !pop) err = c(err,'Scaling up : if no facility is selected, you must choose a population map.')
          # if(!hfNoSelected && popRes) err = c(err,'Scaling up : if .')
          if (!tblSuitLayerOk) {
            err <- c(
              err,
              sprintf(
                ams("srv_analysis_accessibility_suitability_table_missing_layer"),
                tblSuitLayerMissing
              )
            )
          }
          if (!tblExclLayerOk) {
            err <- c(
              err,
              sprintf(
                ams("srv_analysis_accessibility_exclusion_table_missing_layer"),
                tblExclLayerMissing
              )
            )
          }
          if (!tblSuitOk) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_suitability_table_missing_value")
            )
          }
          if (!tblCapMissingOk) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_table_missing_value")
            )
          }
          if (!tblCapTypeOk) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_table_type_error")
            )
          }
          if (!tblCapMinMaxOk) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_table_min_max_equality")
            )
          }
          if (!tblCapBeginWithZero) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_table_first_min_value_0")
            )
          }
          if (!tblCapGreaterThanPrevOk) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_capacity_not_incremental")
            )
          }
          if (!tblCapInRangeOk) {
            info <- c(
              info,
              ams("srv_analysis_accessibility_scaleup_values_not_in_range")
            )
          }
          if (!tblCapOverlapOK) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_min_value_greater_previous_max")
            )
          }
          if (tblCapWithoutButHfSelect) {
            info <- c(
              info,
              ams("srv_analysis_accessibility_start_with_empty_layer_warning")
            )
          }
          if (tblSuitOnlyDynFac) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_suitability_table_add_non_dynamic_layer")
            )
          }
          if (!tblCapLabelOk) {
            err <- c(
              err,
              ams("srv_analysis_accessibility_scaleup_capacity_duplicate_labels")
            )
          }
          # if(hfNoSelected) err = c(err, "Select at least one facility.")
        }

        # output name text.
        if (!isTRUE(length(tagsClean) > 0)) {
          err <- c(
            err,
            ams("srv_analysis_accessibility_add_tag_instruction")
          )
        }
      }

      #
      # create HTML for validation message list.
      #

      if (length(err) > 0) {
        plur <- if (length(err) > 1) "s"
        err <- HTML(paste("<div>",
          icon("exclamation-triangle"),
          err,
          "</div>",
          collapse = ""
        ))
        msgList <- tagList(tags$b(sprintf(ams("srv_analysis_accessibility_validation_issues"), plur)), err)
        disBtn <- TRUE
      } else {
        disBtn <- FALSE

        #
        # Ressource validation
        #
        if (!module5) {
          rEst <- amGetRessourceEstimate(input$hfSelect)

          rRequired <- rEst$required
          rAvailable <- rEst$available
          info <- c(
            info,
            sprintf(
              ams("srv_analysis_accessibility_estimate_required_memory"),
              rRequired$memory,
              rAvailable$memory
            )
          )
          info <- c(
            info,
            sprintf(
              ams("srv_analysis_accessibility_estimate_disk_space"),
              rRequired$disk,
              rAvailable$disk
            )
          )
        }


        if (length(info) > 0) {
          info <- HTML(paste("<div>",
            icon("info-circle"),
            info,
            "</div>",
            collapse = ""
          ))
          msgList <- tagList(tags$b(ams("srv_analysis_accessibility_validation_information")), info)
        }

        if (length(dubious) > 0) {
          dubious <- HTML(paste("<div>",
            icon("question-circle"),
            dubious,
            "</div>",
            collapse = ""
          ))
          msgList <- tagList(msgList, tags$b("Information:"), dubious)
        }
      }


      #
      # If no errors, naming datasets that will be produced.
      #

      if (length(err) == 0) {
        classMod <- character(0)

        switch(input$moduleSelector,
          "module_2" = {
            classMod <- c(
              "tScenarioOut",
              if (isAnisotropic) "rSpeed",
              if (isIsotropic) "rFriction",
              "rTravelTime",
              if (addNearest) "rNearest",
              "lAnalysisParameters"
            )
          },
          "module_3" = {
            classMod <- c(
              "tScenarioOut",
              if (isAnisotropic) "rSpeed",
              if (isIsotropic) "rFriction",
              "tCapacityStat",
              if (zonalPop) "tZonalStat",
              "rPopulationResidual",
              if (popBarrier) "rPopulationOnBarrier",
              "vCatchment",
              "lAnalysisParameters"
            )
          },
          "module_4" = {
            classMod <- c(
              "tScenarioOut",
              if (isAnisotropic) "rSpeed",
              if (isIsotropic) "rFriction",
              "tReferral",
              if (!refLimitClosest) "tReferralDist",
              "tReferralTime",
              if (refKeepNetDistLayer) "vReferralNetwork",
              "lAnalysisParameters"
            )
          },
          "module_5" = {
            classMod <- c()
          },
          "module_6" = {
            classMod <- c(
              "tScenarioOut",
              "rSpeed",
              "rFriction",
              "rPopulationResidual",
              "vFacilityNew",
              "tCapacityOut",
              "tCapacityStatNew",
              "vCatchmentNew",
              "tExclusionOut",
              "tSuitabilityOut",
              "lAnalysisParameters"
            )
          }
        )


        if (!module5) {
          # vNames has 4 group : ui; file; fileMapset and html version
          vNames <- amCreateNames(classMod, tagsClean, dataList)


          # save for launch analysis
          listen$outputNames <- vNames

          # display html version
          out <- tagList(
            tags$b(ams("srv_analysis_accessibility_validation_output_dataset")),
            HTML(paste("<div>",
              icon("sign-out-alt"),
              vNames$html,
              "<div/>",
              collapse = ""
            ))
          )
          #
        }
        # Set final message
        #
      } else {
        out <- character(0)
      }
      msgList <- tagList(msgList, out)
      amActionButtonToggle(
        session = session,
        "btnComputeAccessibility",
        disable = disBtn
      )
      amActionButtonToggle(
        session = session,
        "btnZonalStat",
        disable = disBtn
      )
      output$msgModule3 <- renderUI({
        msgList
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "validate_accessibility")
