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

#'amCapacityAnalysis
#'@export
amCapacityAnalysis <- function(
  session = shiny:::getDefaultReactiveDomain(),
  preAnalysis = FALSE,
  inputSpeed,
  inputFriction,
  inputPop,
  inputMerged,
  inputHf,
  inputTableHf,
  inputZoneAdmin = NULL,
  outputPopResidual,
  outputHfCatchment,
  outputPopBarrier,
  outputTableZonal,
  outputTableCapacity,
  catchPath = NULL,
  removeCapted = FALSE,
  vectCatch = FALSE,
  popOnBarrier = FALSE,
  typeAnalysis,
  returnPath,
  maxCost,
  maxSpeed=0,
  maxCostOrder = NULL,
  radius,
  hfIdx,
  nameField,
  capField = NULL,
  ignoreCapacity = FALSE,
  addColumnPopOrigTravelTime = FALSE,
  addColumnsPopCoverageExtended = FALSE,
  orderField = NULL,
  zonalCoverage = FALSE,
  zoneFieldId = NULL,
  zoneFieldLabel = NULL,
  hfOrder = c('tableOrder','travelTime','circlBuffer'),
  hfOrderSorting = c('hfOrderDesc','hfOrderAsc'),
  pBarTitle = "Capacity Analysis",
  language = config$language
  ){

  amAnalysisSave('amCapacityAnalysis')

  on.exit({
    rmRastIfExists('tmp__*')
    rmVectIfExists('tmp__*') 
  })

  # if cat is set as index, change to cat_orig
  if(hfIdx==config$vectorKey){
    hfIdxNew = paste0(config$vectorKey,"_orig")
  }else{
    hfIdxNew = hfIdx
  }

  orderResult <- data.frame(
    id = character(0),
    value = numeric(0)
  )
  #
  # Labels
  #
  labelField = "amLabel"

  #
  # Compute hf processing order
  #

  if( hfOrder == "tableOrder" || preAnalysis == TRUE ){

    #
    # order by given field value, take index field values 
    #

    orderResult <- inputTableHf[ orderField ] %>%
      order(
        decreasing = hfOrderSorting == "hfOrderDesc" 
        ) %>%
    inputTableHf[ ., c( hfIdx, orderField ) ]

  }else{

    #
    # Do a pre analysis to sort hf with population coverage 
    #
    pbc(
      visible = TRUE,
      percent = 0,
      title   = pBarTitle,
      text    = ams(
        id = "analysis_capacity_process_order"
        ),
      timeOut = 1
    )


    # extract population under max time/distance
    preAnalysisResult <- amCapacityAnalysis(
      preAnalysis       = TRUE,
      inputSpeed        = inputSpeed,
      inputFriction     = inputFriction,
      inputPop          = inputPop,
      inputHf           = inputHf,
      inputTableHf      = inputTableHf,
      outputPopResidual = "tmp_nested_p",
      outputHfCatchment = "tmp_nested_catch",
      typeAnalysis      = ifelse(hfOrder=="circBuffer","circular",typeAnalysis),
      returnPath        = returnPath,
      radius            = radius,
      maxCost           = maxCostOrder,
      maxSpeed          = maxSpeed,
      hfIdx             = hfIdx,
      nameField         = nameField,
      capField          = capField,
      ignoreCapacity    = ignoreCapacity,
      removeCapted      = FALSE,
      vectCatch         = FALSE,
      orderField        = hfIdx,
      hfOrder           = "tableOrder",
      hfOrderSorting    = "hfOrderAsc",      
      pBarTitle         = ams(
        id = "analysis_capacity_geo_coverage_preanalysis"
      )
    )

    #
    # get popTimeMax column from capacity table
    #

    preAnalysisResult <- preAnalysisResult[["capacityTable"]][c(hfIdx,"amPopTravelTimeMax")]


    #
    # order by given field value, take index field values 
    #
    orderPosition <- order(
      preAnalysisResult$amPopTravelTimeMax,
      decreasing = isTRUE(hfOrderSorting =='hfOrderDesc')
    )
    orderResult <- preAnalysisResult[orderPosition,]

  }

  amOrderField <- switch(hfOrder,
    "tableOrder" = sprintf("amOrderValues_%s",amSubPunct(orderField)),
    "circBuffer" = sprintf("amOrderValues_popDistance%sm",radius),
    "travelTime" = sprintf("amOrderValues_popTravelTime%smin",maxCostOrder)
  )

  names(orderResult) <- c(
    hfIdx,
    amOrderField
  )

  orderId = orderResult[[hfIdx]]

  #
  #  Start message
  #
  pbc(
    visible = TRUE,
    percent = 0,
    title   = pBarTitle,
    text    = ams(
      id = "analysis_capacity_initialization"
      ),
    timeOut = 1
  )



  #
  # clean and initialize object outside the loop
  #


  # temp. variable
  tmpHf             <- 'tmp__h' # vector hf tmp
  tmpCost           <- 'tmp__c' # cumulative cost tmp
  tmpPop            <- 'tmp__p' # population catchment to substract
  tblOut            <- data.frame() # empty data frame for storing capacity summary
  tblPopByZone      <- data.frame()
  #popSum            <- amGetRasterStat(inputPop,"sum") # initial population sum
  #popCoveredPercent <- as.numeric(NA) # init percent of covered population
  inc               <- 100/length(orderId) # init increment for progress bar
  incN              <- 0 # init counter for progress bar
  tmpVectCatchOut   <- NA
  #inputPopInit      <- amRandomName("tmp_pop")
  # create residual population 

  amInitPopResidual(
    inputPopResidual = inputPop,
    inputFriction = inputFriction,
    inputSpeed = inputSpeed,
    outputPopResidual = outputPopResidual
  )


  #
  # Population on barrier : map and stat
  #
  if( popOnBarrier && !preAnalysis ){
    amMapPopOnBarrier(
      inputPop = inputPop,
      inputMerged = inputMerged,
      outputMap = outputPopBarrier
    )
  }


  #
  # Start loop on facilities according to defined order
  #
  for(i in orderId){

    #
    # Increment
    #
    incN = incN+1

    #
    # extract capacity and name
    #
    hfCap <- ifelse(
      test = ignoreCapacity,
      yes = 0,
      no = sum(inputTableHf[inputTableHf[hfIdx]==i,capField])
    )
    #
    hfName <- inputTableHf[inputTableHf[hfIdx]==i,nameField]

    #
    # Progress
    #
    msg  <- sprintf("%s/%s",
      incN,
      length(orderId)
    )

    pbc(
      visible = TRUE,
      percent = inc*(incN-1),
      title   = pBarTitle,
      text    = msg
    )


    #
    # extract temporary facility point
    #
    execGRASS(
      "v.extract",
      flags='overwrite',
      input=inputHf,
      where=sprintf(" %1$s = '%2$s'",hfIdx,i),
      output=tmpHf
    )

    #
    # compute cumulative cost map
    #
    switch(typeAnalysis,
      'anisotropic' = amAnisotropicTravelTime(
        inputSpeed       = inputSpeed,
        inputHf          = tmpHf,
        outputCumulative = tmpCost,
        returnPath       = returnPath,
        maxCost          = maxCost,
        maxSpeed         = maxSpeed,
        timeoutValue     = "null()"
        ),
      'isotropic' = amIsotropicTravelTime(
        inputFriction    = inputFriction,
        inputHf          = tmpHf,
        outputCumulative = tmpCost,
        maxCost          = maxCost,
        maxSpeed         = maxSpeed,
        timeoutValue     = "null()"
        ),
      'circular' = amCircularTravelDistance(
        inputHf          = tmpHf,
        outputBuffer     = tmpCost,
        radius           = radius
      )
    )


    #
    # Catchment analysis
    #
    listSummaryCatchment <- amCatchmentAnalyst(
      inputMapTravelTime            = tmpCost,
      inputMapPopInit               = inputPop,
      inputMapPopResidual           = outputPopResidual,
      outputCatchment               = outputHfCatchment,
      facilityCapacityField         = capField,
      facilityCapacity              = hfCap,
      facilityLabelField            = labelField,
      facilityLabel                 = NULL,
      facilityIndexField            = hfIdx,
      facilityId                    = i,
      facilityNameField             = nameField,
      facilityName                  = hfName,
      maxCost                       = maxCost,
      ignoreCapacity                = ignoreCapacity,
      addColumnPopOrigTravelTime    = addColumnPopOrigTravelTime,
      iterationNumber               = incN,
      removeCapted                  = removeCapted,
      vectCatch                     = vectCatch
    )

    # get actual file path to catchment
    tmpVectCatchOut <- listSummaryCatchment$amCatchmentFilePath

    # Add row to output table
    if( incN == 1 ) {
      tblOut = listSummaryCatchment$amCapacitySummary
    }else{
      tblOut = rbind(
        tblOut,
        listSummaryCatchment$amCapacitySummary
      )
    }

    # output message
    msg  <- sprintf("%s/%s",
      incN,
      length(orderId)
    )

    pbc(
      visible = TRUE,
      percent = inc*incN,
      title   = pBarTitle,
      text    = msg
    )
  } # end of loop 


  # merge ordering by column,circle or travel time with the capacity analysis
  tblOut <-  merge(orderResult,tblOut,by = hfIdx)
  tblOut <- tblOut[order(tblOut$amOrderComputed),]

  if(popOnBarrier && addColumnsPopCoverageExtended ){
    tblOut['amPopTotalOnBarrier'] <- amGetRasterStat(outputPopBarrier,'sum')
  }

  colOrder <- c(
    hfIdx,
    nameField,
    if(!ignoreCapacity) capField,
    amOrderField,
    'amOrderComputed',
    'amTravelTimeMax',
    'amPopTravelTimeMax',
    if(addColumnPopOrigTravelTime) "amPopOrigTravelTimeMax",
    'amCorrPopTime',
    'amTravelTimeCatchment',
    'amPopCatchmentTotal',
    'amCapacityRealised',
    'amCapacityResidual',
    'amPopCatchmentDiff',
    if(addColumnsPopCoverageExtended) 'amPopTotal',
    if(addColumnsPopCoverageExtended && popOnBarrier) 'amPopTotalOnBarrier',
    if(addColumnsPopCoverageExtended) 'amPopResidualBefore',
    if(addColumnsPopCoverageExtended) 'amPopResidualAfter',
    'amPopCoveredPercent'
  )

  tblOut <- tblOut[, colOrder]

  if(zonalCoverage){
    #
    # optional zonal coverage using admin zone polygon
    #
    pbc(
      visible = TRUE,
      percent = 100,
      title   = pBarTitle,
      text    = ams(
        id = "analysis_capacity_post_analysis"
      )
    )

    execGRASS('v.to.rast',
      input            = inputZoneAdmin,
      output           = 'tmp_zone_admin',
      type             = 'area',
      use              = 'attr',
      attribute_column = zoneFieldId,
      label_column     = zoneFieldLabel,
      flags            = c('overwrite')
    )

    tblAllPopByZone <- execGRASS(
      'r.univar',
      flags  = c('g','t','overwrite'),
      map    = inputPop,
      zones  = 'tmp_zone_admin', 
      intern = T
      ) %>%
    amCleanTableFromGrass(
      cols = c('zone','label','sum')
    )

    tblResidualPopByZone <- execGRASS(
      'r.univar',
      flags  = c('g','t','overwrite'),
      map    = outputPopResidual,
      zones  = 'tmp_zone_admin', 
      intern = T
      ) %>%
    amCleanTableFromGrass(
      cols = c('zone','label','sum')
    )

    tblPopByZone <- merge(
      tblResidualPopByZone,
      tblAllPopByZone,
      by = c('zone','label')
    )

    tblPopByZone$covered <- tblPopByZone$sum.y - tblPopByZone$sum.x
    tblPopByZone$percent <- (tblPopByZone$covered / tblPopByZone$sum.y) *100
    tblPopByZone$sum.x = NULL
    names(tblPopByZone) <- c(
      zoneFieldId,
      zoneFieldLabel,
      'amPopSum',
      'amPopCovered',
      'amPopCoveredPercent'
    )
  }


  if(vectCatch){
    #
    #  move catchemnt shp related file into one place
    #

    amMoveShp(
      shpFile = tmpVectCatchOut,
      outDir = config$pathShapes,
      outName = outputHfCatchment
    )

  }

  if(!removeCapted){
    #
    # Remove population residual
    #
    rmRastIfExists(outputPopResidual)
  }
  #
  # remove remaining tmp file (1 dash)
  #
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')

  #
  # finish process
  #
  pbc(
    visible = TRUE,
    percent = 100,
    title   = pBarTitle,
    text    = ams(
      id = "analysis_capacity_process_finished"
      ),
    timeOut = 2
  )


  out <-  list(
    capacityTable = tblOut,
    zonalTable = tblPopByZone
  )


  if(!preAnalysis){
    #
    # Local db connection
    #
    dbCon <- amMapsetGetDbCon()
    on.exit({
      dbDisconnect(dbCon)
    })

    #
    # Write summary table in db
    # 
    dbWriteTable(
      dbCon,
      outputTableCapacity,
      tblOut,
      overwrite = T
    )
    #
    # Write zonal stat table if exists
    #
    if(!is.null(tblPopByZone) && nrow(tblPopByZone) > 0){
      dbWriteTable(
        dbCon,
        outputTableZonal,
        tblPopByZone,
        overwrite = T
      )
    }

  }

  pbc(
    visible = FALSE,
  )

  return(out)

}


