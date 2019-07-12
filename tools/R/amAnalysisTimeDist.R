

amTimeDist <- function( job  ){

  source('global.R')
  inputHfFrom = job$inputHfFrom
  inputHfTo = job$inputHfTo
  idFrom = job$idFrom
  idsTo = job$idsTo
  inputSpeed = job$inputSpeed
  inputFriction = job$inputFriction
  maxCost = job$maxCost
  maxSpeed = job$maxSpeed
  typeAnalysis = job$typeAnalysis
  idCol = job$idCol
  idColTo = job$idColTo
  hTimeUnit = job$hTimeUnit
  hDistUnit = job$hDistUnit
  unitCost = job$unitCost
  unitDist = job$unitDist
  limitClosest =job$limitClosest
  resol = job$resol
  origProject = job$origProject
  nCores = job$nCores

  tmpMapset="tmp_mapset_not_set"
  #
  # Output table
  #
  refDistTime <- data.frame()

  #
  # Main script
  #
  tryCatch(
    finally = {
      amMapsetRemove(tmpMapset,stringCheck='^tmp_')
    },{

      tmpMapset <- amRandomName("tmp_mapset")
      #
      # Init temporary mapset
      #
      amMapsetInit(origProject,tmpMapset)
      amDebugMsg(paste("HF",idFrom,"start on mapset",amMapsetGet()))
      #
      # Temporary layers
      #
      tmpVector <- list(
        selectFrom = amRandomName("tmp__ref_from"),
        selectTo = amRandomName("tmp__ref_to"),
        netFrom = amRandomName("tmp__net_from"),
        netAll = amRandomName("tmp__net_all"),
        netDist = amRandomName("tmp__net_dist")
        )
      tmpRaster <-list(
        travelTime = amRandomName("tmp__cost"),
        travelDirection = amRandomName("tmp__dir"),
        drain = amRandomName("tmp__drain")
        )
      #
      # subset hf from 
      #
      qSqlFrom <- sprintf("%s == '%s'",
        idCol,
        idFrom
        )

      execGRASS("v.extract",
        flags = c('overwrite'),
        input = inputHfFrom,
        where = qSqlFrom,
        output = tmpVector$selectFrom
        )
      #
      # subset hf to 
      #
      qSqlTo <- sprintf(" %1$s IN ( %2$s )",
        idCol,
        paste0(idsTo,collapse=',')
        )

      #amDebugMsg(paste("from",idFrom,"to",qSqlTo))

      execGRASS("v.extract",
        flags = c('overwrite'),
        input = inputHfTo,
        where = qSqlTo,
        output = tmpVector$selectTo
        )

      switch(typeAnalysis,
        'anisotropic' = amAnisotropicTravelTime(
          inputSpeed = inputSpeed,
          inputHf = tmpVector$selectFrom,
          inputStop = tmpVector$selectTo,
          outputCumulative = tmpRaster$travelTime, 
          outputDir = tmpRaster$travelDirection,
          returnPath = FALSE,
          maxCost = maxCost,
          maxSpeed = maxSpeed,
          timeoutValue = "null()",
          ratioMemory = 1/nCores
          ),
        'isotropic' = amIsotropicTravelTime(
          inputFriction = inputFriction,
          inputHf = tmpVector$selectFrom,
          inputStop = tmpVector$selectTo,
          outputCumulative = tmpRaster$travelTime,
          outputDir = tmpRaster$travelDirection,
          maxCost = maxCost,
          maxSpeed = maxSpeed,
          timeoutValue = "null()",
          ratioMemory = 1/nCores
          )
        )

      #
      # extact cost for each destination point
      #
      refTimeText = 
        execGRASS(
          'v.what.rast',
          map=tmpVector$selectTo,
          raster=tmpRaster$travelTime,
          flags='p',
          intern=T
          )

      refTime <- read.table(
        text = refTimeText,
        sep ='|',
        stringsAsFactor = F,
        na.strings = "*",
        colClasses = c(typeof(idFrom),"numeric")
        )

      # rename grass output
      names(refTime) <- c(idColTo,hTimeUnit)

      # set "from" value
      refTime[[idCol]] <- idFrom

      #
      # Convert units
      # 
      if( !unitCost =='m' ){
        div<-switch(unitCost,
          's'=1/60,
          'm'=1,
          'h'=60,
          'd'=24
          )
        refTime[hTimeUnit]<-refTime[hTimeUnit]/div
      }

      #
      # Check if all destination are unreachable
      #
      emptyCheck <- all(sapply(refTime[hTimeUnit],amNoDataCheck))
      hasNoDest <- isTRUE(emptyCheck)

      #
      # extract distance
      #
      if( hasNoDest ){

        #
        # Use refTime as template for distances
        #
        refDist <- refTime
        names(refDist)[names(refDist) == hTimeUnit] <- hDistUnit

      }else{

        #
        # subset closest destination point if needed
        #
        if( limitClosest ){

          # NOTE : which.min does not keep ties.
          #closestHf <- refTime[-which.min(refTime[,hTimeUnit]),idColTo]
          
          #
          # Get closest in time, get position, subset,
          #
          minTime <- min(refTime[,hTimeUnit])
          minPos <- which(refTime[,hTimeUnit] == minTime)
          closestHf <- refTime[-minPos,idColTo]

          # 
          # Select all values that are not the closest
          # and remove theme from the layer
          #
          qSqlTo <- sprintf(" %1$s in (%2$s) "
            , idCol
            , paste(closestHf,collapse=",")
            )

          execGRASS(
            "v.edit",
            map = tmpVector$selectTo,
            tool = "delete",
            where = qSqlTo
            )
        }

        execGRASS('r.drain',
          input = tmpRaster$travelTime,
          direction = tmpRaster$travelDirection,
          output =  tmpRaster$drain,
          drain = tmpRaster$drain,
          flags = c('overwrite','c','d'),
          start_points = tmpVector$selectTo
          )
        #
        # Connect the starting facilities to the drain path
        #
        execGRASS('v.net',
          input = tmpRaster$drain,
          points = tmpVector$selectFrom,
          output = tmpVector$netFrom,
          node_layer = '2',
          operation = 'connect',
          threshold = resol-1,
          flags = 'overwrite'
          )

        #
        # Connect the destination facility to the network
        #
        execGRASS('v.net',
          input = tmpVector$netFrom,
          points = tmpVector$selectTo,
          output = tmpVector$netAll,
          node_layer = '3',
          operation = 'connect',
          threshold = resol-1,
          flags = 'overwrite'
          )

        #
        # Calculate distance on the net
        #
        execGRASS('v.net.distance',
          input=tmpVector$netAll,
          output=tmpVector$netDist,
          from_layer='3', # calc distance from all node in 3 to layer 2 (start point)     
          to_layer='2',
          intern=T,
          flags='overwrite'
          )
        #
        # Read and rename calculated distances
        #
        refDist <- amMapsetDbGetQuery(tmpMapset,tmpVector$netDist)
        names(refDist)<-c(idColTo,idCol,hDistUnit)
        #
        # Convert distances
        #
        if(!unitDist=='m'){
          div<-switch(unitDist,
            'km'=1000
            )
          refDist[,hDistUnit]<-refDist[,hDistUnit]/div
        }

      }

      #
      # Merge dist and time
      #
      refDistTime <- merge(
        refDist
        , refTime
        , by=c( idCol, idColTo )
        , all.y=T
        )

      #amDebugMsg(paste("HF",idFrom,"finished on mapset",amMapsetGet()))
    })

  return(refDistTime)
}
