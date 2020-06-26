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

idModule = "module_toolbox"
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
# Populate population column
#
observe({
  zoneSel <- amNameCheck(dataList,input$selectPopCorZones,'vector')
  modePopKnown <- isTRUE(input$toolbox_popcor_mode == "known")
  fields <- list()
  # get field summary 
  isolate({
    if(length(zoneSel)>0 && modePopKnown){
      zoneFieldsSummary <- amGetFieldsSummary(
        dbCon = grassSession$dbCon,
        table = zoneSel,
        getUniqueVal = F
        )
      num <- zoneFieldsSummary$num
      idx <- zoneFieldsSummary$idx
      fields <- as.list(num[num %in% idx])
    }
  })
  updateSelectInput(
    session = session,
    inputId = 'selectPopCorZonesPopCol',
    choices = fields
    )
})

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
    modePopKnown <- isTRUE(input$toolbox_popcor_mode == "known")

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
    hasPopCol <- isTRUE(
      !amNoDataCheck(
        input$selectPopCorZonesPopCol
        )
      )
      
    hasTags <- !amNoDataCheck(input$txtPopCorTags)

    tagsClean <- amGetUniqueTags(input$txtPopCorTags) 

    if(!hasLdcMerged) err <- c(err,
      ams("srv_pop_correction_missing_lcm_layer")
      )
    if(!hasPop) err <- c(err,
      ams("srv_pop_correction_missing_population_layer")
      )
    if(!hasZone) err <- c(err,
      ams("srv_pop_correction_missing_zone_layer")
      )
    if(!hasTags) err <- c(err,
      ams("srv_pop_correction_missing_tags")
      )
    if(modePopKnown && !hasPopCol){
      err <- c(err,
        ams("srv_pop_correction_missing_population_column")
        )
    }

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
          id = "srv_pop_correction_issues_text"
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
          id = "srv_pop_correction_information_text_1"
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
          id = "srv_pop_correction_information_text_2"
          )
        ),
        dubious
        )
    }

    if(length(err)==0){

      classMod = c(
        "rPopulation",
        "tPopDistribAdjustSummary"
        )

      # vNames has 4 group : ui; file; fileMapset and html version
      vNames <- amCreateNames(classMod,tagsClean,dataList)

      # save for launch analysis
      listen$popCorOutputNames <- vNames

      # display html version
      out <- tagList(
        tags$b(
          ams(
          id = "srv_pop_correction_output_dataset"
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

}, suspended = TRUE) %>% amStoreObs(idModule, "update_pop_cor_validation")


