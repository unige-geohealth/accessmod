

amTimeDist <- function( job  ){

  source('global.R')
  inputHfFrom   = job$inputHfFrom
  inputHfTo     = job$inputHfTo
  idFrom        = job$idFrom
  idListTo      = job$idListTo
  inputSpeed    = job$inputSpeed
  inputFriction = job$inputFriction
  maxCost       = job$maxCost
  maxSpeed      = job$maxSpeed
  typeAnalysis  = job$typeAnalysis
  permuted      = job$permuted
  unitCost      = job$unitCost
  unitDist      = job$unitDist
  limitClosest  = job$limitClosest
  resol         = job$resol
  origProject   = job$origProject
  nCores        = job$nCores
  tmpMapset     = "tmp_mapset_not_set"


  #
  # Sanitize options
  #
  if(isTRUE(permuted)){
    limitClosest <- FALSE
  }

  #
  # Output table
  #
  refDistTime <- list()


  #
  # Ref time dist to itself
  #
  sameFromTo <- identical(inputHfFrom,inputHfTo)


  #
  # Main script
  #
  tryCatch({
    #
    # Init temporary mapset
    #
    tmpMapset <- amRandomName("tmp_mapset")
    amMapsetInit(origProject,tmpMapset)
    on.exit({
      amMapsetRemove(tmpMapset,stringCheck='^tmp_')
    })
    amDebugMsg(paste("HF",idFrom,"start on mapset",amMapsetGet()))

    #
    # Temporary layers
    #
    tmpVector <- list(
      selectFrom      = amRandomName("tmp__ref_from"),
      selectTo        = amRandomName("tmp__ref_to"),
      netFrom         = amRandomName("tmp__net_from"),
      netAll          = amRandomName("tmp__net_all"),
      netDist         = amRandomName("tmp__net_dist")
      )
    tmpRaster <- list(
      travelTime      = amRandomName("tmp__cost"),
      travelDirection = amRandomName("tmp__dir"),
      drain           = amRandomName("tmp__drain")
      )
    #
    # subset hf from 
    #
    qSqlFrom <- sprintf("cat == '%s'",
      idFrom
      )

    execGRASS("v.extract",
      flags  = c('overwrite'),
      input  = inputHfFrom,
      where  = qSqlFrom,
      output = tmpVector$selectFrom
      )

    #
    # subset ref to itself 
    #
    if(sameFromTo){
      idListTo <- idListTo[!idListTo %in% idFrom]
    }

    #
    # Subseting target facilities
    #
    qSqlTo <- sprintf("cat IN ( %s )",
      paste0( 
        idListTo
        , collapse = ','
        )
      )

    execGRASS("v.extract",
      flags  = c('overwrite'),
      input  = inputHfTo,
      where  = qSqlTo,
      output = tmpVector$selectTo
      )

    switch(typeAnalysis,
      'anisotropic'      = amAnisotropicTravelTime(
        inputSpeed       = inputSpeed,
        inputHf          = tmpVector$selectFrom,
        inputStop        = tmpVector$selectTo,
        outputCumulative = tmpRaster$travelTime,
        outputDir        = tmpRaster$travelDirection,
        returnPath       = permuted,
        maxCost          = maxCost,
        maxSpeed         = maxSpeed,
        timeoutValue     = "null()",
        ratioMemory      = 1/nCores,
        rawMode          = TRUE # don't convert to minute, do not remove value above max cost
        ),
      'isotropic'        = amIsotropicTravelTime(
        inputFriction    = inputFriction,
        inputHf          = tmpVector$selectFrom,
        inputStop        = tmpVector$selectTo,
        outputCumulative = tmpRaster$travelTime,
        outputDir        = tmpRaster$travelDirection,
        maxCost          = maxCost,
        maxSpeed         = maxSpeed,
        timeoutValue     = "null()",
        ratioMemory      = 1/nCores,
        rawMode          = TRUE # don't convert to minute, do not remove value above max cost
        )
      )

    #
    # extact cost for each destination point
    #
    refTimeText = 
      execGRASS(
        'v.what.rast',
        map    = tmpVector$selectTo,
        raster = tmpRaster$travelTime,
        flags  = 'p',
        intern = T
        )

    refTime <- read.table(
      text            = refTimeText,
      sep             = '|',
      stringsAsFactor = F,
      na.strings      = "*",
      colClasses      = c(typeof(idFrom),"numeric")
      )

    # rename grass output
    names(refTime) <- c('cat_to',unitCost)

    # set "from" value
    refTime[['cat']] <- idFrom

    #
    # Convert units
    # 
    if( unitCost != 's' ){
      div<-switch(unitCost,
        's' = 1,
        'm' = 60,
        'h' = 3600,
        'd' = 86400
        )
      refTime[unitCost] <- refTime[unitCost]/div
    }
    refTime[unitCost] <- round(refTime[unitCost],2)

    #
    # Check if all destination are unreachable
    #
    emptyCheck <- all(sapply(refTime[,unitCost],amNoDataCheck))
    hasNoDest <- isTRUE(emptyCheck)

    #
    # extract distance
    #
    if( hasNoDest ){

      #
      # Use refTime as template for distances
      #
      refDist <- refTime
      names(refDist)[names(refDist) == unitCost] <- unitDist

    }else{

      #
      # subset closest destination point if needed
      #
      if( limitClosest ){


        #
        # Get closest in time, get position, subset,
        # NOTE: which.min does not keep ties.
        #
        minTime <- min(refTime[,unitCost],na.rm=TRUE)
        minPos <- which(refTime[,unitCost] == minTime)
        closestHf <- refTime[minPos,'cat_to']

        # 
        # Select all values that are not the closest
        # and remove theme from the layer
        #
        qSqlTo <- sprintf("cat not in (%s) "
          , paste(closestHf,collapse=",")
          )

        execGRASS(
          "v.edit",
          map   = tmpVector$selectTo,
          tool  = "delete",
          where = qSqlTo
          )
      }
      
      execGRASS('r.drain',
        input        = tmpRaster$travelTime,
        direction    = tmpRaster$travelDirection,
        output       = tmpRaster$drain,
        drain        = tmpRaster$drain,
        flags        = c('overwrite','c','d'),
        start_points = tmpVector$selectTo
        )
      #
      # Connect the starting facilities to the drain path
      #
      execGRASS('v.net',
        input      = tmpRaster$drain,
        points     = tmpVector$selectFrom,
        output     = tmpVector$netFrom,
        node_layer = '2',
        operation  = 'connect',
        threshold  = resol-1,
        flags      = 'overwrite'
        )

      #
      # Connect the destination facility to the network
      #
      execGRASS('v.net',
        input      = tmpVector$netFrom,
        points     = tmpVector$selectTo,
        output     = tmpVector$netAll,
        node_layer = '3',
        operation  = 'connect',
        threshold  = resol-1,
        flags      = 'overwrite'
        )

      #
      # Calculate distance on the net
      #
      execGRASS('v.net.distance',
        input      = tmpVector$netAll,
        output     = tmpVector$netDist,
        from_layer = '3', # calc distance from all node in 3 to layer 2 (start point)
        to_layer   = '2',
        intern     = T,
        flags      = 'overwrite'
        )
     
      #
      # Read and rename calculated distances
      # NOTE: column names returned are cat and tcat.
      #
      refDist <- amMapsetDbGetQuery(tmpMapset,tmpVector$netDist)
      names(refDist) <- c('cat_to','cat',unitDist)
      #
      # Convert distances
      #
      if(!unitDist=='m'){
        div<-switch(unitDist,
          'km' = 1000
          )
        refDist[,unitDist]<-refDist[,unitDist]/div
      }
      refDist[,unitDist] <- round(refDist[,unitDist],3)
    }

    #
    # Merge dist and time
    #
    
    refDistTime <- merge(
      refDist
      , refTime
      , by = c( 'cat', 'cat_to' )
      , all.y=T
      )

  })

  return(refDistTime)
}
