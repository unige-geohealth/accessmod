
#'amCapacityAnalysis
#'@export
amCapacityAnalysis<-function(
  session=shiny:::getDefaultReactiveDomain(),
  inputSpeed,
  inputFriction,
  inputPop,
  inputHf,
  inputTableHf,
  inputZoneAdmin=NULL,
  outputPopResidual,
  outputHfCatchment,
  catchPath=NULL,
  removeCapted=FALSE,
  vectCatch=FALSE,
  typeAnalysis,
  returnPath,
  maxCost,
  maxCostOrder=NULL,
  radius,
  hfIdx,
  nameField,
  capField,
  orderField=NULL,
  zonalCoverage=FALSE,
  zoneFieldId=NULL,
  zoneFieldLabel=NULL,
  hfOrder=c('tableOrder','travelTime','circlBuffer'),
  hfOrderSorting=c('hfOrderDesc','hfOrderAsc'),
  pBarTitle="Capacity Analysis",
  dbCon=NULL
  ){


  # if cat is set as index, change to cat_orig
  if(hfIdx==config$vectorKey){
    hfIdxNew=paste0(config$vectorKey,"_orig")
  }else{
    hfIdxNew=hfIdx
  }



  orderResult <- data.frame(
    id=character(0),
    value=numeric(0)
    )


  #
  # Compute hf processing order
  #

  if(hfOrder == "tableOrder"){

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
      text    = "Starting pre-analysis to extract processing order.",
      timeOut = 1
      )


    # extract population under max time/distance
    preAnalysis <- amCapacityAnalysis(
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
      hfIdx             = hfIdx,
      nameField         = nameField,
      capField          = capField,
      orderField        = orderField,
      hfOrder           = "tableOrder",
      hfOrderSorting    = hfOrderSorting,      
      pBarTitle         = "Geographic Coverage : processing order pre-analysis"
      )

    #
    # get popTimeMax column from capacity table
    #

    preAnalysis <- preAnalysis[["capacityTable"]][c(hfIdx,"amPopTravelTimeMax")]


    #
    # order by given field value, take index field values 
    #

   orderResult <- preAnalysis["amPopTravelTimeMax"] %>%
     order(
      decreasing = hfOrderSorting == "hfOrderDesc"
      ) %>%
    preAnalysis[.,]

  }


  #
  # Keep values used for sorting and set a name
  #

  switch(hfOrder,
    "tableOrder"={
      names(orderResult) <- c(
        hfIdx,
        sprintf("am%sInit",amCamelCase(orderField))
        )
    },
    "circBuffer"={
      names(orderResult) <- c(
        hfIdx,
        sprintf("amPopInitBuffer%sm",radius)
        )
    },
    "travelTime"={
      names(orderResult) <- c(
        hfIdx,
        sprintf("amPopInitTravelTime%smin",maxCostOrder)
        )
    } 
)  


  orderId = orderResult[[hfIdx]]



  #
  #  Start message
  #



  pbc(
    visible = TRUE,
    percent = 0,
    title   = pBarTitle,
    text    = "Initialisation..."
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
  popSum            <- amGetRasterStat(inputPop,"sum") # initial population sum
  popCoveredPercent <- NA # init percent of covered population
  inc               <- 100/length(orderId) # init increment for progress bar
  incN              <- 0 # init counter for progress bar
  tmpVectCatchOut   <- NA
  #inputPopInit      <- amRandomName("tmp_pop")
  # create residual population 

 # amInitPop(
    #inputPop = inputPop,
    #inputFriction = inputFriction,
    #outputPopResidual = outputPopResidual,
    #outputPopInit = inputPopInit
    #)

  amInitPopResidual(
    inputPopResidual = inputPop,
    inputFriction = inputFriction,
    outputPopResidual = outputPopResidual
    )
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
    hfCap <- sum(inputTableHf[inputTableHf[hfIdx]==i,capField])
    #
    hfName <- inputTableHf[inputTableHf[hfIdx]==i,nameField]

    #
    # Progress
    #
    msg  <- sprintf("Evaluation of facility %s/%s",
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
        maxCost          = maxCost
        ),
      'isotropic' = amIsotropicTravelTime(
        inputFriction    = inputFriction,
        inputHf          = tmpHf,
        outputCumulative = tmpCost,
        maxCost          = maxCost
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
      inputMapTravelTime      = tmpCost,
      inputMapPopInit         = inputPop,
      inputMapPopResidual     = outputPopResidual,
      outputCatchment         = outputHfCatchment,
      facilityCapacityField   = capField,
      facilityCapacity        = hfCap,
      facilityIndexField      = hfIdx,
      facilityId              = i,
      facilityNameField       = nameField,
      facilityName            = hfName,
      totalPop                = totalPop,
      maxCost                 = maxCost,
      iterationNumber         = incN,
      removeCapted            = removeCapted,
      vectCatch               = vectCatch,
      dbCon                   = dbCon
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
    msg  <- sprintf("Evaluation of facility %s/%s",
      incN,
      length(orderId)
      )

    pbc(
      visible = TRUE,
      percent = inc*incN,
      title   = pBarTitle,
      text    = msg
      )
    # clean 
    rmRastIfExists('tmp__*')
    rmVectIfExists('tmp__*')


  } # end of loop 

  #
  # merge with starting order
  #
  # check if the order was kept and if all facilities were processed.
  if( !identical(tblOut[[hfIdx]], orderResult[[hfIdx]] ) ){
    stop("Order index does not match output capacity table index")
  }
 
 tblOut <- merge(
    x  = orderResult,
    y  = tblOut,
    by = hfIdx
    )

 # set column order (issue #98)
 tblOut <- tblOut[c(1,3,4,2,5,6:length(tblOut))]



 if(zonalCoverage){
    #
    # optional zonal coverage using admin zone polygon
    #

    pbc(
      visible = TRUE,
      percent = 100,
      title   = pBarTitle,
      text    = "Post analysis : zonal coverage..."
      )


    execGRASS('v.to.rast',
      input            = inputZoneAdmin,
      output           = 'tmp_zone_admin',
      type             = 'area',
      use              = 'attr',
      attribute_column = zoneFieldId,
      label_column     = zoneFieldLabel,
      flags            = c('overwrite'))

    tblAllPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags  = c('g','t','overwrite'),
        map    = inputPop,
        zones  = 'tmp_zone_admin', #
        intern = T
        ),sep='|',header=T)[,c('zone','label','sum')]

    tblResidualPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags  = c('g','t','overwrite'),
        map    = outputPopResidual,
        zones  = 'tmp_zone_admin', #
        intern = T
        ),sep='|',header=T)[,c('zone','label','sum')]

    tblPopByZone         <- merge(tblResidualPopByZone,tblAllPopByZone,by=c('zone','label'))
    tblPopByZone$covered <- tblPopByZone$sum.y - tblPopByZone$sum.x
    tblPopByZone$percent <- (tblPopByZone$covered / tblPopByZone$sum.y) *100
    tblPopByZone$sum.x=NULL
    names(tblPopByZone)<-c(zoneFieldId,zoneFieldLabel,'amPopSum','amPopCovered','amPopCoveredPercent')
  }


  if(vectCatch){
    #
    #  move catchemnt shp related file into one place
    #

    # base name file
    baseCatch <- gsub('.shp','',basename(tmpVectCatchOut))
    # list files
    allShpFiles <- list.files(
      dirname(tmpVectCatchOut),
      pattern=paste0('^',baseCatch,'\\.'),
      full.names=TRUE
      )
    # Copy each files in shp location.
    for( s in allShpFiles){
      sExt <- file_ext(s)
      newPathGrass <- file.path(
        catchPath,
        paste0(outputHfCatchment,'.',sExt)
        )
      newPath <- system(paste('echo',newPathGrass),intern=T)
      file.copy(s,newPath,overwrite=T) 
    }
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
      text    = "Process finished.",
      timeOut = 2
      )

    pbc(
      visible = FALSE,
      )

  return(
    list(
      capacityTable=tblOut,
      zonalTable=tblPopByZone
      )
    )
}


