
#
# General validation and error message
#



# preventive field validation
# TODO: this validation step was written for one module:
# With almost all modules depending on it, this should be rewritten.
idModule = "module_analysis"

observe({
  amErrorAction(title = 'Module 2,3,4,6: validation',{
    #
    # init messages
    #

    err = character(0)
    info = character(0)
    dubious = character(0)
    out  = character(0)
    msgList = character(0)

    #
    # store current module
    #

    module2    <- isTRUE(input$moduleSelector == 'module_2')
    module3    <- isTRUE(input$moduleSelector == 'module_3')
    module4    <- isTRUE(input$moduleSelector == 'module_4')
    module5    <- isTRUE(input$moduleSelector == 'module_5')
    module6    <- isTRUE(input$moduleSelector == 'module_6')

    isAnisotropic <- isTRUE(input$typeAnalysis == 'anisotropic')
    isIsotropic <-  isTRUE(input$typeAnalysis == 'isotropic')

    if(module5){
      ttInRange <- TRUE
      maxTT <- 0

      # Check if data exist
      layerOkTT    <- isTRUE(!is.null(amNameCheck(dataList,input$travelTimeSelect,'raster')))
      layerOkZones <- isTRUE(!is.null(amNameCheck(dataList,input$zoneSelect,'vector')))
      layerOkPop   <- isTRUE(!is.null(amNameCheck(dataList,input$popSelect,'raster')))

      if(layerOkTT){
        maxTT <- round(amGetRasterStat(input$travelTimeSelect,c("max"))) 
        selectTT <- input$sliderTimeAnalysis
        ttInRange <- isTRUE(selectTT >= 0 && selectTT <= maxTT)
      }

    }else{

      #
      # Clean tags
      #

      tagsClean  <- amGetUniqueTags(input$costTag) 

      #
      # Control maps and values
      #

      # Check if data exist
      merged     <- isTRUE(!is.null(amNameCheck(dataList,input$mergedSelect,'raster')))
      hf         <- isTRUE(!is.null(amNameCheck(dataList,input$hfSelect,'vector')))
      pop        <- isTRUE(!is.null(amNameCheck(dataList,input$popSelect,'raster')))
      popRes     <- isTRUE(!is.null(amNameCheck(dataList,input$popResSelect,'raster')))


      # table validation
      hfOnBarrier <- any(tblHfSubset()$amOnBarrier=='yes')
      hfOnZero <- any(tblHfSubset()$amOnZero=='yes')

      if(module4){
        hfOnBarrier = hfOnBarrier || any(tblHfSubsetTo()$amOnBarrier=='yes') 
        hfOnZero = hfOnZero || any(tblHfSubsetTo()$amOnZero=='yes') 
        refLimitClosest = isTRUE(input$checkReferralLimitClosest)
      }


      # check if there is at least one facility selectected.
      hfNoSelected   <- isTRUE(!any(tblHfSubset()$amSelect))
      hfNoSelectedTo <- isTRUE(!any(tblHfSubsetTo()$amSelect))
      # check for speed of  0 kmh
      # tblModel     <- isTRUE(!any(hotToDf(input$speedRasterTable)$speed <1))
      #tblModelSpeed <- isTRUE(all(hotToDf(input$speedRasterTable)$speed > 0 ))
      # parameter validation
      unlimitedTT    <- isTRUE(
        input$maxTravelTime == 0
        )
      #wrongTT <- !isTRUE(module4) && isTRUE( 
      wrongTT <- isTRUE( 
        !is.numeric(input$maxTravelTime) || 
          amNoDataCheck(input$maxTravelTime) ||
          input$maxTravelTime < 0 ||
          input$maxTravelTime > 2147483647
        )
      #
      # Parameters control.
      #

      if(module3){
        # simple character control (user cannot put custom value)
        hfIdx         <- isTRUE(nchar(input$hfIdxField)>0)
        capField      <- isTRUE(nchar(input$hfCapacityField)>0)
        hfBuffer      <- isTRUE(input$hfOrder == 'circBuffer')
        popBuffer     <- isTRUE(input$popBufferRadius > listen$mapMeta$grid$nsres)
        zonalPop      <- isTRUE('zonalPop' %in% input$mod3param)

        if(zonalPop){
          zonalSelect <- isTRUE(!is.null(amNameCheck(dataList,input$zoneSelect,'vector')))
          zoneId      <- isTRUE(length(input$zoneId)>0)
          zoneLabel   <- isTRUE(length(input$zoneLabel)>0)
        }

        hfOrderInconsistency       <- isTRUE(input$hfOrder!='tableOrder' && !'rmPop' %in% input$mod3param)
        zonalCoverageInconsistency <- isTRUE(zonalPop && !'rmPop' %in% input$mod3param)
        # data overwrite warning module 3 : validate each output !
        # TODO: inform user of all provided output. Warning if risk of overwrite.
      }

      if(module6){
        tblCapTypeOk              <- TRUE
        tblCapMissingOk           <- TRUE
        tblCapOverlapOK           <- TRUE
        tblCapInRangeOk           <- TRUE
        tblCapGreaterThanPrevOk   <- TRUE
        tblCapWithoutButHfSelect  <- FALSE
        tblSuitOk                 <- FALSE
        tblSuitOnlyDynFac         <- FALSE
        tblSuitLayerMissing       <- character(0)
        tblSuitLayerOk            <- TRUE
        tblExclOk                 <- FALSE
        tblExclLayerMissing       <- character(0)
        tblExclLayerOk            <- TRUE
        tblCapBeginWithZero       <- TRUE
        tblCapMinMaxOk            <- TRUE
        tblCapLabelOk             <- TRUE
        popSelect                 <- TRUE
        maxScUpPopGoalNoLimit     <- FALSE
        maxScUpTimeNoLimit        <- FALSE
        maxScUpHfNoLimit          <- FALSE
        allScUpNoLimit            <- FALSE

        tblCapacityNew <- hotToDf(input$capacityTable)
        tblSuit <- hotToDf(input$suitabilityTable)
        tblExcl <- hotToDf(input$exclusionTable)
        withoutFacility <- isTRUE(input$useExistingHf == "FALSE")
        popResidualIsResidual <- isTRUE(amGetClass(input$popResidualSelect)=="rPopulationResidual")

        popNotResidualButHfSelect <- FALSE
        popResidualButNoHfSelect <- FALSE
        # options
        # computation limit
        maxScUpHf <- input$maxScUpNewHf
        maxScUpTime <- input$maxScUpTime
        maxScUpPopGoal <- input$maxScUpPopGoal




        # auto correction
        if(isTRUE(maxScUpPopGoal>100)){
          updateNumericInput(session,"maxScUpPopGoal",value = 100)
        }

        maxScUpHfNoLimit <- isTRUE(maxScUpHf<1)
        maxScUpTimeNoLimit <- isTRUE(maxScUpTime <1)
        maxScUpPopGoalNoLimit <- isTRUE(maxScUpPopGoal <1)


        allScUpNoLimit <- all(
          c(
            maxScUpPopGoalNoLimit,
            maxScUpHfNoLimit,
            maxScUpTimeNoLimit
            )
          )


        if(withoutFacility) {
          if(!hfNoSelected && hf){
            tblCapWithoutButHfSelect <- TRUE 
          }
          # manually validate hf layer and hf on barrier.
          hfNoSelected <- FALSE
          hfOnBarrier <- FALSE
          hfOnZero <- FALSE
          hf <- TRUE
        }else{
          # if there is hf select without a population residual
          if(!hfNoSelected && !popResidualIsResidual){
            popNotResidualButHfSelect <- TRUE

          }
          if(hfNoSelected && popResidualIsResidual){
            popResidualButNoHfSelect <- TRUE
          }
        }

        # validate suitability table 
        if(!is.null(tblSuit)){
          tblSuitOk <- nrow(na.omit(tblSuit))>0 
        }
        if(tblSuitOk){
          # if without facility and all layer in suitability are dynamic facility
          tblSuitOnlyDynFac <- withoutFacility && all(tblSuit$layer == config$dynamicFacilities) && !hfNoSelected && hf

          # validate layer names 
          suitLayers <- tblSuit$layer[! tblSuit$layer %in% config$dynamicLayers ] 
          tblSuitLayerMissing <- suitLayers[!sapply(suitLayers,amMapExists)]
          if( length(tblSuitLayerMissing) >0 ) {
            tblSuitLayerOk <- isTRUE( length(tblSuitLayerMissing) == 0)
          }
        }

        if(!is.null(tblExcl)){
          tblExclOk <- TRUE
        }


        if(tblExclOk){ 
          exclLayers <- tblExcl$layer[! tblExcl$layer %in% config$dynamicLayers ] 
          if( length(tblExclLayerMissing) >0 ) {
            tblExclLayerMissing <- exclLayers[!sapply(exclLayers,amMapExists)]
            tblExclLayerOk<- isTRUE( length(tblSuitLayerMissing) == 0)
          }
        }

        #  validate null
        if(!is.null(tblCapacityNew)){
          #  validate missing value
          tblCapMissingOk <-isTRUE(all(
              sapply(tblCapacityNew,function(x){a = all(stringr::str_length(x)>0)})
              ))

          if(tblCapMissingOk)(
            # validate type
            tblCapTypeOk <- all(
              is.numeric(tblCapacityNew$min),
              is.numeric(tblCapacityNew$max),
              is.numeric(tblCapacityNew$capacity), 
              is.character(tblCapacityNew$label)
              )

            )
          # validate overlap min max and capacity in range.
          if(tblCapMissingOk){

            # max greater than min
            tblCapMinMaxOk<-all(tblCapacityNew$min<tblCapacityNew$max)
            tblCapBeginWithZero <- isTRUE(tblCapacityNew$min[1] == 0)
            # checking previous row values
            nR<-nrow(tblCapacityNew)
            if(nR>1){
              for(i in 2:nR){
                # Capacity is greater than previous capacity 
                tblCapGreaterThanPrevOk <- all(tblCapGreaterThanPrevOk,
                  isTRUE(tblCapacityNew[i,'capacity']>tblCapacityNew[i-1,'capacity'])) 
                # min max+1 overlap
                tblCapOverlapOK<-all(tblCapOverlapOK,
                  isTRUE(tblCapacityNew[i,'min'] > tblCapacityNew[i-1,'max'])) 
              }
            }
            # capacity in min max range
            tblCapInRangeOk <- isTRUE(
              all(tblCapacityNew$capacity <= tblCapacityNew$max &
                  tblCapacityNew$capacity >= tblCapacityNew$min)
              )
            # unique labels
            tblCapLabelOk <- isTRUE(length(unique(tblCapacityNew$label)) == 
              length(tblCapacityNew$label))

          }
        }
      }


    }
    #
    # Collect messages in err and info 
    #


    if(module5){
      #
      # zonal analysis
      #
      if(!ttInRange) err = c(err,
        sprintf(
          ams(
            id = "srv_analysis_accessibility_travel_time_input",
            str = "Please enter a travel time between 0 and %1$s.",
            lang = language),
          maxTT
          )
        )
      if(!layerOkZones) err = c(err,
        ams(
           id = "srv_analysis_accessibility_missing_zone",
          str = "Zone layer missing.",
          lang = language
          )
        )
      if(!layerOkPop) err = c(err,
        ams(
          id = "srv_analysis_accessibility_missing_population",
          str = "Population layer missing.",
          lang = language
          )
        )
      if(!layerOkTT) err = c(err,
        ams(
          id = "srv_analysis_accessibility_missing_travel_time",
          str = "Travel time layer missing.",
          lang = language
          )
        )
    }else{
      #
      # Other modules
      #
      if(wrongTT) err = c(err,
        ams(
          id = "srv_analysis_accessibility_max_travel_time_input",
          str = "Please enter a valid maximum travel time between 0 and 2147483647",
          lang = language
          )
        )
      if(!hf) err = c(err,
        ams(
          id = "srv_analysis_accessibility_missing_facility_layer",
          str = "Facilities layer missing.",
          lang = language
          )
        ) 
      if(hfOnBarrier) err = c(err, 
        ams(
          id = "srv_analysis_accessibility_facilities_on_barrier",
          str = "There are facilities located on barrier. Unselect them or correct the original layer.",
          lang = language
          )
        )
      if(hfOnZero) err = c(err, 
        ams(
          id = "srv_analysis_accessibility_facilities_on_0kmh",
          str = "There are facilities located on a land cover area where a speed of 0 km/h is set. Unselect them or change the scenario to proceed",
          lang = language
          )
        )
      if(!merged) err = c(err,
        ams(
          id = "srv_analysis_accessibility_missing_merged_lc_warning",
          str = "Merged land cover layer missing.",
          lang = language
          )
        )
      if(unlimitedTT) info = c(info,
        ams(
          id = "srv_analysis_accessibility_max_travel_time_set_0min",
          str = "Maximum travel time set to zero. A value of zero will use the default travel time, which is currently defined as 32767 minutes ( 22 days, 18 hours and 7 minutes)",
          lang = language
          )
        )
      if(unlimitedTT && module2 ) info = c(info, 
        ams(
          id = "srv_analysis_accessibility_max_travel_time_warning",
          str = "Using a maximum travel time of zero, computed travel time greater than 32737 will be coded as -1",
          lang = language
          )
        )
      if(unlimitedTT && !module2 ) info = c(info, 
        ams(
          id = "srv_analysis_accessibility_travel_time_>32727_ignored",
          str = "Using a maximum travel time of zero, computed travel time greater than 32737 will be ignored",
          lang = language
          )
        )  
      if(module2 | module6){
        if(hfNoSelected) err = c(err, 
          ams(
            id = "srv_analysis_accessibility_select_facilities",
            str = "Please select at least one facility.",
            lang = language
            )
          )
        }
      if(module3 | module6){ 
        if(!pop) err = c(err,
          ams(
            id = "srv_analysis_accessibility_select_population",
            str = "Please select a population layer.",
            lang = language
            )
          )
        }

      if(module3){
        if(!hfIdx) err = c(err,
          ams(
            id = "srv_analysis_accessibility_no_group_warning",
            str = "No group/id field set for hf.",
            lang = language
            )
          )
        if(hfNoSelected) err = c(err, 
          ams(
            id = "srv_analysis_accessibility_select_one_facility_warning",
            str = "Select at least one facility.",
            lang = language
            )
          )
        if(!capField) err = c(err,
          ams(
            id = "srv_analysis_accessibility_set_capacity_warning",
            str = "No capacity field set for hf.",
            lang = language
            )
          )

        if(hfBuffer)if(!popBuffer) err = c(err,
          ams(
            id = "srv_analysis_accessibility_circular_buffer_warning",
            str = "Circular buffer must be higher than project resolution.",
            lang = language
            )
          )
        #if(!popBarrier) info = c(info,'Map of population on barrier will NOT be computed.')
        if(hfOrderInconsistency) info = c(info,
          ams(
            id = "srv_analysis_accessibility_facilities_processing_order",
            str = "If covered population is not removed at each iteration, facilities processing order should be set to 'Order from facilities table'.",
            lang = language
            )
          )
        if(zonalPop){
          if(!zonalSelect) err = c(err,
            ams(
              id = "srv_analysis_accessibility_select_zone_warning",
              str = "Please select a zone layer or uncheck the 'Generate zonal statistics' option under settings.",
              lang = language
              )
            )
          if(!zoneId) err = c(err,
            ams(
              id = "srv_analysis_accessibility_zonal_id_missing",
              str = "Zonal id column missing.",
              lang = language
              )
            )
          if(!zoneLabel) err = c(err,
            ams(
              id = "srv_analysis_accessibility_zonal_label_missing",
              str = "Zonal label column missing.",
              lang = language
              )
            )
        }
        if(zonalCoverageInconsistency) err = c(err,
          ams(
            id = "srv_analysis_accessibility_remove_covered_pop_warning",
            str = "If covered population is not removed at each iteration, zonal analysis could not be performed.",
            lang = language
            )
          )

        #
        # if check population
        #
        # population on barrier

        if( isTRUE(length(err) <1) &&
          isTRUE(popOnBarrierStat()$sum > 0) ) info = c(info,
            sprintf(
              ams(
                id = "srv_analysis_accessibility_pop_on_barrier_removed",
                str = "Population encoutered on barrier in %s cells for a total of %s individuals ( %s %% of the initial population ). This population will not be part of the analysis",
                lang = language),
              popOnBarrierStat()$cells,
              popOnBarrierStat()$sum,
              popOnBarrierStat()$percent
              )
            )

        }
      if(module4){
        if(hfNoSelected) err = c(err, 
          ams(
            id = "srv_analysis_accessibility_select_one_facility_from",
            str = "Select at least one facility in table 'FROM'.",
            lang = language
            )
          )
        if(hfNoSelectedTo) err = c(err,
          ams(
            id = "srv_analysis_accessibility_select_one_facility_to",
            str = "Select at least one facility in table 'TO'.",
            lang = language
            )
          )
        }
      if(module6){
        if(allScUpNoLimit){
          info = c(info, 
          ams(
            id = "srv_analysis_accessibility_scaling_up_unlimited",
            str = "All scaling up goals are set to 0 (or less) and are considered as unlimited. Scaling up analysis will stop when no more candidates are found or if 100% of the population is covered.",
            lang = language
            )
          )
        }else{
          if(maxScUpPopGoalNoLimit) info = c(info, 
            ams(
              id = "srv_analysis_accessibility_coverage_100percent",
              str = "Population coverage set to zero or less: coverage will be 100%",
              lang = language
              )
            )
          if(maxScUpTimeNoLimit) info = c(info, 
            ams(
              id = "srv_analysis_accessibility_processing_unlimited",
              str = "Time limitation set to zero or less: unlimited processing time.",
              lang = language
              )
            )
          if(maxScUpHfNoLimit)  info = c(info, 
            ams(
              id = "srv_analysis_accessibility_new_facilities_unlimited",
              str = "Number of facilities to create set to zero or less: unlimited facilities creation.",
              lang = language
              )
            )
          }

        if(popNotResidualButHfSelect) dubious = c(dubious, 
          ams(
            id = "srv_analysis_accessibility_facilities_residual_pop_warning",
            str = "Existing facilities have been selected while the selected residual population layer is not labelled as residual. Please check if this is correct before computing.",
            lang = language
            )
          )
        if(popResidualButNoHfSelect)  dubious = c(dubious, 
          ams(
            id = "srv_analysis_accessibility_residual_pop_no_facilities",
            str = "Population residual is of subclass 'residual', but no facilies has been selected. Please verify.",
            lang = language
            )
          )
        if(!withoutFacility) info = c(info,
          ams(
            id = "srv_analysis_accessibility_selected_facilities_verification",
            str = "The 'start using selected existing facilities' option has been checked. Please make sure that these facilities have been used to generate the residual population layer.",
            lang = language
            )
          )
        #if(hfNoSelected && !pop) err = c(err,'Scaling up : if no facility is selected, you must choose a population map.')
        #if(!hfNoSelected && popRes) err = c(err,'Scaling up : if .')
        if(!tblSuitLayerOk) err = c(err, 
          sprintf(
            ams(
              id = "srv_analysis_accessibility_suitability_table_missing_layer",
              str = "Table of suitability: layer missing: %s",
              lang = language),
            tblSuitLayerMissing
            )
          )
        if(!tblExclLayerOk) err = c(err, 
          sprintf(
            ams(
              id = "srv_analysis_accessibility_exclusion_table_missing_layer",
              str = "Table of exclusion: layer missing: %s",
              lang = language),
            tblExclLayerMissing
            )
          )
        if(!tblSuitOk) err = c(err, 
          ams(
            id = "srv_analysis_accessibility_suitability_table_missing_value",
            str = "Table of suitability factors: missing value",
            lang = language
            )
          )
        if(!tblCapMissingOk) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_table_missing_value",
            str = "Table of scaling up capacity: missing value",
            lang = language
            )
          )
        if(!tblCapTypeOk) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_table_type_error",
            str = "Table of scaling up capacity: type error.",
            lang = language
            )
          )
        if(!tblCapMinMaxOk) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_table_min_max_equality",
            str = "Table of scaling up capacity: min greater than or equal to max.",
            lang = language
            )
          )
        if(!tblCapBeginWithZero) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_table_first_min_value_0",
            str = "Table of scaling up capacity: the first minimal capacity value in column 'min' should be zero.",
            lang = language
            )
          )
        if(!tblCapGreaterThanPrevOk) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_capacity_not_incremental",
            str = "Table of scaling up capacity: capacity is not incremental",
            lang = language
            )
          )
        if(!tblCapInRangeOk) info = c(info,
          ams(
            id = "srv_analysis_accessibility_scaleup_values_not_in_range",
            str = "Table of scaling up capacity: there is capacity value(s) not in range [min,max].",
            lang = language
            )
          )
        if(!tblCapOverlapOK) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_min_value_greater_previous_max",
            str = "Table of scaling up capacity: min value can't be equal or less than previous max value.",
            lang = language
            )
          )
        if(tblCapWithoutButHfSelect) info = c(info, 
          ams(
            id = "srv_analysis_accessibility_start_with_empty_layer_warning",
            str = "Existing facilities have been selected while the \"start with empty layer\" is being checked. Those facilities will be ignored.",
            lang = language
            )
          )
        if(tblSuitOnlyDynFac) err = c(err,
          ams(
            id = "srv_analysis_accessibility_suitability_table_add_non_dynamic_layer",
            str = "Without existing facilities selected, dynamic facilities can't be the only layer in suitability table. Please add at least another non-dynamic layer.",
            lang = language
            )
          )
        if(!tblCapLabelOk) err = c(err,
          ams(
            id = "srv_analysis_accessibility_scaleup_capacity_duplicate_labels",
            str = "Table scaling up capacity: duplicate labels.",
            lang = language
            )
          )
        #if(hfNoSelected) err = c(err, "Select at least one facility.") 
      }

      # output name text. 
      if(!isTRUE(length(tagsClean)>0)){
        err <- c(err,
          ams(
            id = "srv_analysis_accessibility_add_tag_instruction",
            str = "Please enter at least one tag.",
            lang = language
            )
          )
        }
      }

    #
    # create HTML for validation message list.
    #

    if(length(err)>0){
      plur <- if(length(err)>1) "s"
      err <- HTML(paste("<div>",
        icon('exclamation-triangle'),
        err,
        '</div>',
        collapse = ""
        ))
      msgList <- tagList(tags$b(sprintf('Issue%s:', plur)), err)
      disBtn <- TRUE
    }else{
      disBtn <- FALSE

      #
      # Ressource validation
      #

      rEst <- amGetRessourceEstimate(input$hfSelect)

      rRequired <- rEst$required
      rAvailable <- rEst$available
      info <-c(info, 
        sprintf(
          ams(
            id = "srv_analysis_accessibility_estimate_required_memory",
            str = "Estimation of required memory %1$d MB ( available %2$d MB )",
            lang = language),
          rRequired$memory,
          rAvailable$memory
          )
        )
      info <- c(info, 
        sprintf(
          ams(
            id = "srv_analysis_accessibility_estimate_disk_space",
            str = "Estimation of disk space required = %1$d MB ( available %2$d MB )",
            lang = language),
          rRequired$disk,
          rAvailable$disk
          ))

      if(length(info)>0) {
        info <- HTML(paste("<div>",
          icon('info-circle'),
          info,
          '</div>',
          collapse = ""
          ))
        msgList <- tagList(tags$b("Information:"), info)
      }

      if(length(dubious)>0) {
        dubious <- HTML(paste("<div>",
          icon('question-circle'),
          dubious,
          '</div>',
          collapse = ""
          ))
        msgList <- tagList(msgList,tags$b("Information:"),dubious)
      }


    }

   
    #
    # If no errors, naming datasets that will be produced. 
    # 

    if(length(err)==0){

      classMod = character(0)

      switch(input$moduleSelector,
        "module_2"={classMod = c(
          "tScenarioOut",
          if(isAnisotropic) "rSpeed",
          if(isIsotropic) "rFriction",
          "rTravelTime"
          )},
        "module_3"={classMod = c(
          "tScenarioOut",
          if(isAnisotropic) "rSpeed",
          if(isIsotropic) "rFriction",
          "tCapacityStat",
          if(zonalPop) "tZonalStat",
          "rPopulationResidual",
          "rPopulationOnBarrier",
          "vCatchment"
          )},
        "module_4"={classMod = c(
          "tScenarioOut",
          if(isAnisotropic) "rSpeed",
          if(isIsotropic) "rFriction",
          "tReferral",
          if(!refLimitClosest) "tReferralDist",
          "tReferralTime"
          )},
        "module_5"={classMod = c(
          )},
        "module_6"={classMod = c(
          "tScenarioOut",
          if(isAnisotropic) "rSpeed",
          if(isIsotropic) "rFriction",
          "rPopulationResidual",
          "vFacilityNew",
          "tCapacityOut",
          "tCapacityStatNew",
          "vCatchmentNew",
          "tExclusionOut",
          "tSuitabilityOut"
          )}
          )


      if(!module5){

      # vNames has 4 group : ui; file; fileMapset and html version
      vNames <- amCreateNames(classMod,tagsClean,dataList)


      # save for launch analysis
      listen$outputNames <- vNames
      
      # display html version
      out <- tagList(
        tags$b('Output dataset:'), 
        HTML(paste("<div>",
          icon('sign-out'),
          vNames$html,
          "<div/>",
          collapse = ""
          ))
        )
      #
      }
      # Set final message 
      #
    }else{
      out = character(0)
    }
    msgList <- tagList(msgList,out)
    amActionButtonToggle(session = session,
      'btnComputeAccessibility',
      disable = disBtn
      )
    amActionButtonToggle(session = session,
    'btnZonalStat',
    disable = disBtn
    )
    output$msgModule3 <-renderUI({msgList})

})

},suspended = TRUE) %>% amStoreObs(idModule,"validate_accessibility")







