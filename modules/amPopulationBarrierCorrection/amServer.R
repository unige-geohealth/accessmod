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

# Correct population on barrier - ui


idModule <- "module_toolbox"
#------------------------------------------------------------------------------#

# Update selectize when data list change

#------------------------------------------------------------------------------#

#
# Source helpers
#
source("modules/amPopulationBarrierCorrection/helper.R", local = T)
source("modules/amPopulationBarrierCorrection/amServer_validation.R", local = T)

observeEvent(input$btnPopCorCompute,
  {
    amErrorAction(
      title = "Compute population redistribution",
      pBarFinalRm = F,
      {
        amActionButtonToggle(
          session = session,
          id = "btnPopCorCompute",
          disable = TRUE
        )

        if (listen$popCorComputeDisabled) {
          stop(
            ams(
              id = "srv_pop_correction_invalid_inputs_error"
            )
          )
        }

        pBarTitle <- ams(
          id = "srv_pop_correction_main_title"
        )
        popOut <- listen$popCorOutputNames$file["rPopulation"]
        summaryDistrib <- listen$popCorOutputNames$file["tPopDistribAdjustSummary"]
        popIn <- input$selectPopCorPopulation
        zoneIn <- input$selectPopCorZones
        ldcIn <- input$selectPopCorLandCoverMerged
        modePopKnown <- isTRUE(input$toolbox_popcor_mode == "known")
        popCol <- input$selectPopCorZonesPopCol

        result <- amPopulationBarrierCorrection(
          modePopKnown = modePopKnown,
          inputZone = zoneIn,
          inputPopulation = popIn,
          inputLandCover = ldcIn,
          inputPopulationColumn = popCol,
          outputPopulation = popOut,
          outputSummary = summaryDistrib,
          progressCallback = function(percent, message) {
            pbc(
              id = "popCorrection",
              title = pBarTitle,
              visible = TRUE,
              percent = percent,
              text = message
            )
          },
          dbCon = grassSession$dbCon
        )

        pbc(
          id = "popCorrection",
          title = pBarTitle,
          visible = FALSE,
          percent = 100
        )

        #
        # Upate data list
        # Update outfiles, used in filter "last output"
        #
        amUpdateDataList(listen)
        listen$outFiles <- listen$popCorOutputNames$file

        #
        # Remove old tags
        #
        updateTextInput(session, "txtPopCorTags", value = "")

        #
        # Create ui output message.
        #

        outNames <- listen$popCorOutputNames$ui

        ulResult <- tags$ul(
          tags$li(
            tags$b(ams("srv_pop_correction_population_input")),
            round(result$popOrig, 2)
          ),
          tags$li(
            tags$b(ams("srv_pop_correction_population_output")),
            round(result$popFinal, 2)
          ),
          tags$li(
            tags$b(ams("srv_pop_correction_population_on_barrier_before")),
            round(result$popOnBarrierBefore, 2)
          ),
          tags$li(
            tags$b(ams("srv_pop_correction_population_on_barrier_after")),
            round(result$popOnBarrierAfter, 2)
          ),
          tags$li(
            tags$b(ams("srv_pop_correction_population_outside_zone")),
            round(result$popOutsideZone, 2)
          ),
          tags$li(
            tags$b(ams("srv_pop_correction_population_difference")),
            round(result$popDiff, 2),
            sprintf(
              ams("srv_pop_correction_percentage_original_pop"),
              round((result$popDiff / result$popOrig) * 100, 2)
            )
          ),
          tags$li(
            tags$b(ams("srv_pop_correction_zone_without_pop")),
            result$nZonesWithoutPop, "/", result$nZones
          )
        )

        outputDatasets <- tags$ul(
          HTML(paste("<li>", outNames, "</li>"))
        )
        msg <- sprintf(
          ams(
            id = "srv_pop_correction_process_timing"
          ),
          result$timing
        )
        msg <- tagList(
          p(msg),
          outputDatasets,
          p(
            ams(
              id = "srv_pop_correction_results"
            )
          ),
          ulResult
        )
        amMsg(session,
          type = "message",
          title = ams(
            id = "srv_pop_correction_process_finished"
          ),
          text = msg
        )
      }
    )
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_pop_cor_compute")
