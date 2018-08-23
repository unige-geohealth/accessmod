#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Correct population on barrier - ui


idModule="module_toolbox"
#------------------------------------------------------------------------------#

# Update selectize when data list change

#------------------------------------------------------------------------------#

# 
# Source helpers
#
source("modules/amPopulationBarrierCorrection/helper.R",local=T)

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
},suspended=TRUE) %>% amStoreObs(idModule,"update_pop_cor_input")


#
# Validation
#

observe({
  amErrorAction(title='validation pop correct',{
    err = character(0)
    info = character(0)
    dubious = character(0)
    out  = character(0)
    msgList = character(0)
    disableCompute = TRUE

    hasLdcMerged <- isTRUE(!is.null(amNameCheck(dataList,input$selectPopCorLandCoverMerged,'raster')))
    hasPop       <- isTRUE(!is.null(amNameCheck(dataList,input$selectPopCorPopulation,'raster')))
    hasZone      <- isTRUE(!is.null(amNameCheck(dataList,input$selectPopCorZones,'vector')))
    hasTags      <- !amNoDataCheck(input$txtPopCorTags)

    tagsClean  <- amGetUniqueTags(input$txtPopCorTags) 

    if(!hasLdcMerged) err <- c(err,'Missing landcover merged layer')
    if(!hasPop) err <- c(err,'Missing population layer')
    if(!hasZone) err <- c(err,'Missing zone layer')
    if(!hasTags) err <- c(err,'Missing tag(s)')

    if(length(err)>0){
      plur <- if(length(err)>1) "s"
      err <- HTML(paste("<div>",icon('exclamation-triangle'),err,'</div>',collapse=""))
      msgList <- tagList(tags$b(sprintf('Issue%s:',plur)),err)
      disableCompute <- TRUE
    }else{
      disableCompute <- FALSE
    }

    if(length(info)>0) {
      info <- HTML(paste("<div>",icon('info-circle'),info,'</div>',collapse=""))
      msgList <- tagList(tags$b("Information:"),info)
    }

    if(length(dubious)>0) {
      dubious <- HTML(paste("<div>",icon('question-circle'),dubious,'</div>',collapse=""))
      msgList <- tagList(msgList,tags$b("Information:"),dubious)
    }

    if(length(err)==0){

      classMod=c(
        "rPopulation"
        )

      # vNames has 4 group : ui; file; fileMapset and html version
      vNames <- amCreateNames(classMod,tagsClean,dataList)

      # save for launch analysis
      listen$popCorOutputNames <- vNames

      # display html version
      out <- tagList(
        tags$b('Output dataset:'), 
        HTML(paste("<div>",icon('sign-out'),vNames$html,"<div/>",collapse=""))
        )
    }

    msgList <- tagList(msgList,out)

    amActionButtonToggle(session=session,
      id = 'btnPopCorCompute',
      disable = disableCompute
      )

    listen$popCorComputeDisabled <- disableCompute

    output$uiPopCorValidation <-renderUI({msgList})

    })
})

observeEvent(input$btnPopCorCompute,{

  amErrorAction(title="Compute population resditribution",
    pBarFinalRm=F,{

      amActionButtonToggle(session=session,
        id = 'btnPopCorCompute',
        disable = TRUE
        )

      if( listen$popCorComputeDisabled ) stop("Can't compute population correction, invalid inputs")

      pBarTitle  <- "Correct population on barriers"
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
      updateTextInput(session,"txtPopCorTags",value="")

      # 
      # Create ui output message.
      #

      outNames <-  listen$popCorOutputNames$ui

      ulResult <- tags$ul(
        tags$li(tags$b("Pop. input (sum) "),round(result$popOrig,2),""),
        tags$li(tags$b("Pop. on barrier (sum)"),round(result$popOnBarrier,2),""),
        tags$li(tags$b("Pop. output (sum)"),round(result$popFinal,2),""),
        tags$li(tags$b("Diff before/after"),round(result$popDiff,2),sprintf(" ( %1$s%% of orig. pop)",round((result$popDiff/result$popOrig)*100,2)))
        )

      outputDatasets <- tags$ul(
        HTML(paste("<li>",outNames,"</li>"))
        )
      msg <- sprintf("Process finished in %s minutes. Output data names:",result$timing)
      msg <- tagList(
        p(msg),
        outputDatasets,
        p("Results"),
        ulResult
        )
      amMsg(session,type='message',title='Process finished',text=msg)
    })
})





