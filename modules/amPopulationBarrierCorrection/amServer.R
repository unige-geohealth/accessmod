#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Correct population on barrier - ui


idModule = "module_toolbox"
#------------------------------------------------------------------------------#

# Update selectize when data list change

#------------------------------------------------------------------------------#

# 
# Source helpers
#
source("modules/amPopulationBarrierCorrection/helper.R", local = T)

#
# Update input
#
observe({
  amUpdateSelectChoice(
    idData = c('rLandCoverMerged'),
    idSelect = c("selectPopCorLandCoverMerged"),
    dataList = dataList
    )
  amUpdateSelectChoice(
    idData = c('rPopulation'),
    idSelect = c("selectPopCorPopulation"),
    dataList = dataList
    )
  amUpdateSelectChoice(
    idData = c('vZone'),
    idSelect = c("selectPopCorZones"),
    dataList = dataList
    )
}, suspended = TRUE) %>% amStoreObs(idModule, "update_pop_cor_input")


#
# Validation
#

observe({
  amErrorAction(title = 'validation pop correct',{
    err = character(0)
    info = character(0)
    dubious = character(0)
    out  = character(0)
    msgList = character(0)
    disableCompute = TRUE

    hasLdcMerged <- isTRUE(
      !is.null(
        amNameCheck(
          dataList,
          input$selectPopCorLandCoverMerged,
          'raster'
          )
        )
      )
    hasPop <- isTRUE(
      !is.null(
        amNameCheck(
          dataList,
          input$selectPopCorPopulation,
          'raster'
          )
        )
      )
    hasZone <- isTRUE(
      !is.null(
        amNameCheck(
          dataList,
          input$selectPopCorZones,
          'vector'
          )
        )
      )
    hasTags <- !amNoDataCheck(input$txtPopCorTags)

    tagsClean <- amGetUniqueTags(input$txtPopCorTags) 

    if(!hasLdcMerged) err <- c(err,
      ams(
        id = "srv_pop_correction_missing_lcm_layer",
        str = "Missing landcover merged layer",
        lang = language
        )
      )
    if(!hasPop) err <- c(err,
      ams(
        id = "srv_pop_correction_missing_population_layer",
        str = "Missing population layer",
        lang = language
        )
      )
    if(!hasZone) err <- c(err,
      ams(
        id = "srv_pop_correction_missing_zone_layer",
        str = "Missing zone layer",
        lang = language
        )
      )
    if(!hasTags) err <- c(err,
      ams(
        id = "srv_pop_correction_missing_tags",
        str = "Missing tag(s)",
        lang = language
        )
      )

    if(length(err)>0){
      # Simplification of the plural for translation purposes      
      # plur <- if(length(err)>1) "s" 
      err <- HTML(
        paste(
          "<div>",
          icon('exclamation-triangle'),
          err,
          '</div>',
          collapse = ""
          )
        )
      # msgList <- tagList(tags$b(sprintf('Issue%s:',plur)),err)
      msgList <- tagList(tags$b(
        ams(
          id = "srv_pop_correction_issues_text",
          str = "Issue(s):",
          lang = language
          )
        ),
        err)
      disableCompute <- TRUE
    }else{
      disableCompute <- FALSE
    }

    if(length(info)>0) {
      info <- HTML(
        paste(
          "<div>",
          icon('info-circle'),
          info,
          '</div>',
          collapse = ""
          )
        )
      msgList <- tagList(tags$b(
        ams(
          id = "srv_pop_correction_information_text_1",
          str = "Information:",
          lang = language
          )
        ),
        info
        )
    }

    if(length(dubious)>0) {
      dubious <- HTML(
        paste(
          "<div>",
          icon('question-circle'),
          dubious,
          '</div>',
          collapse = ""
          )
        )
      msgList <- tagList(msgList,tags$b(
        ams(
          id = "srv_pop_correction_information_text_2",
          str = "Information:",
          lang = language
          )
        ),
        dubious
        )
    }

    if(length(err)==0){

      classMod = c(
        "rPopulation"
        )

      # vNames has 4 group : ui; file; fileMapset and html version
      vNames <- amCreateNames(classMod,tagsClean,dataList)

      # save for launch analysis
      listen$popCorOutputNames <- vNames

      # display html version
      out <- tagList(
        tags$b(
          ams(
          id = "srv_pop_correction_output_dataset",
          str = "Output dataset:",
          lang = language
          )
        ), 
        HTML(
          paste(
            "<div>",
            icon('sign-out'),
            vNames$html,
            "<div/>",
            collapse = ""
            )
          )
        )
      }

    msgList <- tagList(msgList,out)

    amActionButtonToggle(session = session,
      id = 'btnPopCorCompute',
      disable = disableCompute
      )

    listen$popCorComputeDisabled <- disableCompute

    output$uiPopCorValidation <- renderUI({msgList})

    })
  })

observeEvent(input$btnPopCorCompute,{

  amErrorAction(title = "Compute population redistribution",
    pBarFinalRm = F,{

      amActionButtonToggle(session = session,
        id = 'btnPopCorCompute',
        disable = TRUE
        )

      if( listen$popCorComputeDisabled ) stop(
        ams(
          id = "srv_pop_correction_invalid_inputs_error",
          str = "Can't compute population correction, invalid inputs",
          lang = language
          )
        )

      pBarTitle <- ams(
        id = "srv_pop_correction_main_title",
        str = "Correct population on barriers",
        lang = language
        )
      popOut <- listen$popCorOutputNames$file['rPopulation']
      popIn <- input$selectPopCorPopulation
      zoneIn <- input$selectPopCorZones
      ldcIn <- input$selectPopCorLandCoverMerged


      result <- amPopulationBarrierCorrection(
        inputBorder = zoneIn, 
        inputPopulation = popIn, 
        inputLandCover = ldcIn, 
        outputPopulation = popOut,
        progressCallback = function(percent, message){
          pbc(
            id =  "popCorrection",
            title = pBarTitle,
            visible = TRUE,
            percent = percent,
            text    = message
            )
          }
        )

      pbc(
        id =  "popCorrection",
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
        tags$li(tags$b(
          ams(
            id = "srv_pop_correction_population_input",
            str = "Pop. input (sum) ",
            lang = language
            )
          ),
          round(result$popOrig, 2), ""),
        tags$li(tags$b(
          ams(
            id = "srv_pop_correction_population_on_barrier",
            str = "Pop. on barrier (sum)",
            lang = language
            )
          ),
          round(result$popOnBarrier, 2), ""),
        tags$li(tags$b(
          ams(
            id = "srv_pop_correction_population_output",
            str = "Pop. output (sum)",
            lang = language
            )
          ),
          round(result$popFinal, 2), ""),
        tags$li(tags$b(
          ams(
            id = "srv_pop_correction_population_difference",
            str = "Diff before/after",
            lang = language
            )
          ),
          round(result$popDiff, 2), 
          sprintf(
            ams(
              id = "srv_pop_correction_percentage_original_pop",
              str = " (%1$s%% of orig. pop)",
              lang = language
              ),
            round((result$popDiff/result$popOrig)*100, 2
            ))
          )
        )

      outputDatasets <- tags$ul(
        HTML(paste("<li>", outNames, "</li>"))
        )
      msg <- sprintf(
        ams(
          id = "srv_pop_correction_process_timing",
          str = "Process finished in %s minutes. Output data names:",
          lang = language
          ),
        result$timing
        )
      msg <- tagList(
        p(msg),
        outputDatasets,
        p(
          ams(
          id = "srv_pop_correction_results",
          str = "Results",
          lang = language
          )
        ),
        ulResult
        )
      amMsg(session,
        type = 'message',
        title = ams(
          id = "srv_pop_correction_process_finished",
          str = "Process finished",
          lang = language
          ),
        text = msg
        )
      })
  })





