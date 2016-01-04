




#    #' Rescale friction  map
#    #' @param inputMask Set a mask to limit computation
#    #' @param inputFriction AccessMod frction map (time to go cross a cell on flat surface)
#    #' @return name of the raster map computed
#    #' @export
#    amScalingCoef_Friction <- function(inputMask,inputFriction){
#      tmpName=paste0("tmp_coef_friction")
#      if(!is.null(inputMask)) execGRASS('r.mask',raster=inputMask,flags='overwrite')
#      execGRASS('r.rescale.eq',flags='overwrite',input=inputFriction,output=tmpName,to=c(0L,100L))
#      exp=paste0(tmpName,"=100-",tmpName)
#      execGRASS('r.mapcalc',expression=exp,flags='overwrite')
#      if(!is.null(inputMask)) execGRASS('r.mask',flags='r')
#      return(tmpName) 
#    }
#


#' Update candidates layer by substracting exclusion layer 
#' 
#' @param inputCandidates Layer of candidates on which exclude cells
#' @param inputLayer Layer of exclusion
#' @param inputLayerType Type of data for the layer of exclusion: vector or raster
#' @param distance Distance of the buffer. If zero, use vector as exclusion mask
#' @param keep Selection strategy : keep value 'inside' or 'outside' the buffer
#' @return count available candidate left
amScalingUp_candidateExclude <- function(
  inputCandidates,
  inputLayer,
  inputLayerType=c('vector','raster'),
  distance=1000,
  keep=c('keepInside','keepOutside')
  ){
  # validation
  stopifnot(is.numeric(distance))
  keep <- match.arg(keep)
  inputLayerType <- match.arg(inputLayerType)
  # init vars
  tmpExcl <- amRandomName('tmp__exclusion')
  tmpExclBuffer <- amRandomName('tmp__exclusion','buffer')
  outputCandidates <- inputCandidates
  # Convert raster map
  if(inputLayerType=='vector'){
    execGRASS('v.to.rast',input=inputLayer,output=tmpExcl,use='val',value=1,flags='overwrite')
  }else{
    execGRASS('g.copy',raster=c(inputLayer,tmpExcl)) 
  }
  # Create buffer if necessary
  if(distance>0){
    execGRASS('r.buffer',input=tmpExcl,output=tmpExclBuffer,distances=c(distance),flags='overwrite')
  }else{
    execGRASS('g.rename',raster=c(tmpExcl,tmpExclBuffer),flags='overwrite')
  }
  # Apply keeping strategy
  if(keep == "keepInside"){
    expCandExcl = sprintf("%1$s = if(!isnull(%2$s),%1$s,null())",outputCandidates, tmpExclBuffer)
  }else{
    expCandExcl = sprintf("%1$s = if(isnull(%2$s),%1$s,null())",outputCandidates, tmpExclBuffer)
  } 
  
  execGRASS('r.mapcalc',expression=expCandExcl,flags='overwrite')
  # count remaining candidates
  countLeft <- amGetRasterStat(outputCandidates,'n')
  if(length(countLeft)<1)countLeft=0
  # Remove temporary raster
  rmRastIfExists(c(tmpExcl,tmpExclBuffer))  
  # return candidates count
  return(countLeft)
}

#' Iterate through table of exclusion and apply amScalingUp_candidateExclude on candidate layer.  
#'
#' Input table structure
#' #data.frame':  1 obs. of  5 variables:
#' $ select: logi TRUE
#' $ layer : chr "scaling_up_exclusion_vect__test_super@Burkina"
#' $ buffer: int 5
#' $ method: chr "inside"
#' $ type  : chr "vector"
#'
#' @param tableExclusion 
#' @param candidatesLayer Raster layer of candidates
#' @param unitDistance Distance unit in meter (m) or kilometer (m)
#' @return count available candidate left
amScalingUp_candidateExcludeTable <- function(
  inputCandidates,
  tableExclusion,
  unitDistance="km"){
  # validation
  stopifnot(unitDistance %in% c('m','km'))
  # init
  noMoreCandidates <- FALSE
  
  tbl <- tableExclusion
  tblEmpty <- nrow(tbl) < 1
  distMult <- ifelse(unitDistance == 'km',1000,1)
  countInit <- amGetRasterStat(inputCandidates,'n')
  countLeft <- 0
  skip <- FALSE
  # Iterate trough table
  if(!tblEmpty){
    for(i in 1:nrow(tbl)){
      # create shortcut for message
      l = tbl[i,'layer']
      t = tbl[i,'type'] 
      d = tbl[i,'buffer']*distMult
      m = tbl[i,'method']
      if(t == "raster"){
        skip <- amRastIsEmpty(l)
      }else{ 
        skip <- amVectIsEmpty(l)
      }
      if(skip){
      amDebugMsg(
            sprintf("Exclude cells : skipping empty layer %s (method=%s;buffer=%s;type=%s)",l,m,d,t)
            )
      }else{
        # eval candidates
        if(noMoreCandidates){
          amDebugMsg(
            sprintf("Exclude cells. No more candidates, skipping layer %s (method=%s;buffer=%s;type=%s)",l,m,d,t)
            )
        }else{
          countLeft <- amScalingUp_candidateExclude(
            inputCandidates = inputCandidates,
            inputLayer = l,
            inputLayerType = t,
            distance = d,
            keep = m)
          if(isTRUE(countLeft<1)){
            noMoreCandidates <- TRUE
          }
        }
      }
    }
  }else{
    countLeft = countInit
  }

  return(countLeft)
}



#' rescale to given range
#' @param inputRast Text raster name to rescale
#' @param outputRast Text output raster name
#' @param reverse Boolean Inverse the scale
#' @export
amRasterRescale <- function(inputMask=NULL,inputRast,outputRast,range=c(0L,1000L),weight=1,reverse=FALSE, setNullAsZero=TRUE){

  if(!is.null(inputMask)){ 
    rmRastIfExists("MASK")
    execGRASS("r.mask",raster=inputMask,flags="overwrite")
  }

  if(setNullAsZero){
    # sometimes, candidates will occurs were inputRast has no values.
    # In those case, we set minimal value to 0 to avoid totally empty output, wich could break
    # further analysis, especially multicriteria analysis
    expRmNull <- sprintf("%1$s = if(isnull(%1$s),0,%1$s)",inputRast)
    execGRASS("r.mapcalc",expression=expRmNull,flags="overwrite")
  }

  inMin <- amGetRasterStat(inputRast,"min") 
  inMax <- amGetRasterStat(inputRast,"max")



  if(reverse) {
    expr = " %1$s = ( %4$s - ((%2$s - %3$s) * (%4$s - %5$s ) / (%6$s - %3$s)) + %5$s) * %7$s "
  }else{
    expr = " %1$s = (((%2$s - %3$s) * (%4$s - %5$s ) / (%6$s - %3$s)) + %5$s) * %7$s "
  }

  exprRescale = sprintf(expr,
    outputRast, #1
    inputRast, #2
    inMin+1,     #3
    max(range),#4
    min(range),#5
    inMax-1,     #6
    weight     #7  
    )

  execGRASS("r.mapcalc",expression=exprRescale,flags="overwrite")


 
  if(!is.null(inputMask)){ 
    rmRastIfExists("MASK")
  } 
  return(outputRast)
}


#' Calc travel time on existing vector, create a rescaled map
#' @param inputMap Existing vector from where start analysis
#' @param inputSpeed Speed and transport mod map in accessmod format
#' @param inputFriction AccessMod friction map
#' @param typeAnalysis Type of analysis : anisotropic or isotropic
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_traveltime <- function(inputMask,inputMap,inputSpeed,inputFriction,typeAnalysis,towards=TRUE,weight=1,inverse=FALSE){
  tmpOut <- amRandomName("tmp__coef_travel_time")
  tmpA <- amRandomName('tmp__')
  # create a cumulative cost map on the whole region, including new hf sets at the end of this loop.
  typeAnalysis <- match.arg(typeAnalysis,c("anisotropic","isotropic"))
  if(amNoDataCheck(typeAnalysis))stop("Type analysis should be anisotropic or isotropic")
  switch(typeAnalysis,
    'anisotropic'= amAnisotropicTravelTime(
      inputSpeed       = inputSpeed,
      inputHf          = inputMap,
      outputCumulative = tmpA,
      returnPath       = towards,
      maxCost          = 0 #unlimited
      ),
    'isotropic'= amIsotropicTravelTime(
      inputFriction    = inputFriction,
      inputHf          = inputMap,
      outputCumulative = tmpA,
      maxCost          = 0
      )
    )
  amRasterRescale(inputMask,tmpA,tmpOut,config$scalingUpRescaleRange,weight,inverse)
  rmRastIfExists(tmpA)
  return(tmpOut)
}



#' Create a rescaled cumulative cost map
#' @param inputMask Set a mask to limit computation
#' @param inputPop Population map
#' @param radiusKm Radius of the analysis
#' @param mapResolution Map resolution in meter
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @param position Position of the layer
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_pop<-function(inputMask,inputMap,radiusKm,weight=1,inverse=FALSE){
  tmpOut <- amRandomName('tmp__coef_pop_density')
  tmpA <- amRandomName('tmp__')

  radiusKm = as.numeric(radiusKm)
  mapResolution = as.numeric(gmeta()$nsres)
  weight = as.numeric(weight)

  neighbourSize <- round((abs(radiusKm)*1000)/mapResolution)
  useMovingWindow <- isTRUE(neighbourSize != 0)
  # r.neighbors needs odd number
  if(isTRUE(useMovingWindow && neighbourSize %% 2 ==0)){
    message('Scaling up. Neighbour size is not odd (',neighbourSize,')., Added one cell to, as required by moving window algorithm.')
    neighbourSize <- neighbourSize +1
  }
  if(useMovingWindow){
    # create a density map using a  moving window sum of population on a radius
    execGRASS('r.neighbors',flags=c('c','overwrite'),input=inputMap,output=tmpA,method='sum',size=neighbourSize)
  }else{
    exp = sprintf("%s = %s",tmpA,inputMap)
    execGRASS('r.mapcalc',expression=exp)
  }

  amRasterRescale(inputMask,tmpA,tmpOut,config$scalingUpRescaleRange,weight,inverse)
  rmRastIfExists(tmpA)
  return(tmpOut)
}


#' Create a rescaled distance map
#' @param inputMask Set a mask to limit computation
#' @param inputMap A raster or vector  map from which compute euclidean distance. Vector map will be rasterized.
#' @param inputMapType Set if the input map is a vector or a raster
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @param position Position of the layer
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_dist<-function(inputMask,inputMap,inputMapType=c('vector','raster'),weight=1,inverse=FALSE){


  inputMapType <- match.arg(inputMapType)
  tmpOut <- amRandomName("tmp__coef_dist")
  tmpA <- amRandomName('tmp__')
  tmpB <- amRandomName('tmp__')

  if(inputMapType=='vector'){
      # Convert vector to raster
    execGRASS('v.to.rast',input=inputMap,output=tmpA,use='val',value=0,flags='overwrite')
  }else{
    # Filter usable value NOTE: why a second statement ? if(!isnull(%s),0) ?
    exprCoefDistVal <- sprintf("%s=if(isnull(%s),null(),0)",tmpA,inputMap) 
    execGRASS('r.mapcalc',expression=exprCoefDistVal) 
  }

  # compute grow distance from tmpA
  execGRASS('r.grow.distance',input=tmpA,distance=tmpB,metric="euclidean",flags="overwrite") 

  amRasterRescale(inputMask,tmpB,tmpOut,config$scalingUpRescaleRange,weight,inverse)
  rmRastIfExists(tmpA)
  rmRastIfExists(tmpB)
  return(tmpOut)
}


#' Create a rescaled version of generic suitability map
#' @param inputMap The raster map to convert
#' @param position Thw position of the layer
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @return name of the rescaled map
#' @export
amScalingUpCoef_generic <- function(inputMap,weight=1,inverse=FALSE){
  tmpOut <- amRandomName("tmp__coef_generic")
  amRasterRescale(inputMask,inputMap,tmpOut,config$scalingUpRescaleRange,weight,inverse)
  return(tmpOut)
}




#' Create temporary candidate cells based on non-null raster value
#' @param input Raster layer from which compute candidates
#' @param output Raster of candidates cells
#' @export
amScalingUp_createCandidatesTemp <- function(input=NULL,output=NULL){
  expCandTemp <- paste(output,"= if(!isnull(",input,"),1,null())")
  execGRASS('r.mapcalc',expression=expCandTemp,flags="overwrite") 
}

#' Create population residual based on non-null raster value
#' @param input Raster layer of population
#' @param output Raster layer of population residuall
#' @export
amScalingUp_createPopulationOut <- function(input=NULL,output=NULL){
  # exp <- paste(output,"= if(!isnull(",input,"),",input,",0)")
  expPopTemp <- paste(output,"= if((",input,">0),",input,",null())")
  execGRASS('r.mapcalc',expression=expPopTemp,flags="overwrite") 
}


#' Create composite index based on input coef
#' @param candidates Candidates raster layer (after exclusion process)
#' @param coefLayerTable Components of the composite index
amScalingUp_suitability <- function(
  inputCandidates,
  inputSpeed,
  inputFriction,
  outputSuitability,
  coefTable
  ){

  if(nrow(coefTable)<1){
    stop("Warning : no valid layer in suitability table")
  }

  layersCalc <- character(0)
  nLayer <- nrow(coefTable)
  coefTable$skip <- FALSE
  wSum = 0

  # skip empty layer
  for(i in 1:nLayer){
    l <- as.list(coefTable[i,])
    if(l$type == "raster"){
      coefTable[i,]$skip <- amRastIsEmpty(l$layer)
    }else{ 
      coefTable[i,]$skip <- amVectIsEmpty(l$layer)
    }
  }

  # get weight sum
  coefOut <- vector()

  for(i in 1:nLayer){
    l <- as.list(coefTable[i,])
    if(l$skip){
      amDebugMsg(paste("Scaling up suitability skipping empty layer",l$layer))
    }else{ 
      wSum = wSum + l$weight
      opt <- amParseOptions(l$options)
      l<-c(l,opt)
      switch(l$factor,
        "popsum"={
          coefOut[i] <- amScalingUpCoef_pop(
            inputMask      =  inputCandidates,
            inputMap       =  l$layer,
            radiusKm       =  l$r,
            weight         =  l$weight,
            inverse        =  isTRUE(l$p == "hvls")
            )
        },
        "dist"={
          coefOut[i] <- amScalingUpCoef_dist(
            inputMask     =  inputCandidates,
            inputMap      =  l$layer,
            inputMapType  =  l$type,
            weight        =  l$weight,
            inverse       =  isTRUE(l$p == "hvls")
            )
        },
        "traveltime"={
          coefOut[i] <- amScalingUpCoef_traveltime(
            inputMask      =  inputCandidates,
            inputMap       =  l$layer,
            inputSpeed     =  inputSpeed,
            inputFriction  =  inputFriction,
            typeAnalysis   =  l$t,
            towards        =  isTRUE(!l$d == "from"),
            weight         =  l$weight,
            inverse        =  isTRUE(l$p == "hvls")
            )
        },
        "priority"={
          coefOut[i] <- amScalingUpCoef_generic(
            inputMask =  inputCandidates,
            inputMap  =  l$layer,
            weight    =  l$weight,
            inverse   =  isTRUE(l$p == "hvls")
            )
        }
        )
    }
  }

  # get mean value from scaled value. NOTE: check if this is ok for a multicriteria analysis. 

  expSuitability <- sprintf(" %1$s = int((%2$s) / %3$s)",
    outputSuitability,
    paste(na.omit(coefOut),collapse="+"),
    wSum
    )


  execGRASS('r.mapcalc',expression=expSuitability,flags='overwrite') 
 if(amGetRasterStat(outputSuitability,"max")==0){
   browser()
 }
  

  rmRastIfExists(as.character(coefOut))

  return(NULL)

}



#' Find the best cell based on an exclusion procedure and an suitability map
#' @param inputFriction String. Name of friction map 
#' @param inputSpeed String. Name of speed map
#' @param inputTableExclusion. Data.frame. Table of exclusion rules.
#' @param inputTableSuitability. Data.frame. Table of suitability rules.
#' @param inputFacilities. String. Name of map that containing existing set of facilites or will contain new generated one.
#' @param inputCandidates. String. Name of map that containing available candidates cells.
#' @param outputBestCandidates. String. Name of vector map with the best suitable locatio based on exclusion and suitability.
#' @return # return a list with
#' candidatesBestVect: chr Best vector point map name 
#' suitabilityMap    : chr Suitability map name
#' suitabilityMax    : num Maximum suitability
#' nCandidates       : num Number of candidates
#' nBeforeExclusion  : num Number of cell available
#' nAfterExclusion   : num Number of cells after exclusion
#' msg               : chr Summary message
#' @export
 
amScalingUp_findBestCells<-function(
  inputFriction,
  inputSpeed,
  inputTableExclusion,
  inputTableSuitability,
  inputCandidates,
  outputBestCandidates,
  candidateCountInit
  ){

  res <- list()

  #
  # Layer name init
  #
  tmpSuitabilityLayer <- amRandomName("tmp__suitability")
  tmpBestCandidates <- amRandomName("tmp__candidates_best_rast")

  #
  # Create candidates raster and get count of remaining cells.
  #

  #
  # Apply exclusion rules on inputCandidates
  #

  # Get number of candidates before
  candidateCountBefore <- amGetRasterStat(inputCandidates,'n')
  # apply rules 
  amScalingUp_candidateExcludeTable(
    tableExclusion = inputTableExclusion,
    inputCandidates = inputCandidates
    )
  # Get candidates after
  candidateCountAfter <- amGetRasterStat(inputCandidates,'n')


  if(length(candidateCountAfter)<1 || candidateCountAfter<1){
    # oops, no more candidate.
    res$noMoreCandidates = TRUE
    return(res)
  }else{
    #
    # Create suitability map
    #
    amScalingUp_suitability(
      inputCandidate = inputCandidates,
      inputSpeed = inputSpeed,
      inputFriction = inputFriction,
      outputSuitability = tmpSuitabilityLayer, 
      coefTable = inputTableSuitability
      )

    # get max suitability. We expect 100 each time, as values are rescaled.
    suitMax <- amGetRasterStat(tmpSuitabilityLayer,"max")
   
    # select best candidates based on suitMax
    expCandSelect <- sprintf(
      "%1$s=if( %2$s >= %3$s , %2$s , null() )",
      tmpBestCandidates ,
      tmpSuitabilityLayer ,
      suitMax
      )

    res<-try(
      execGRASS("r.mapcalc",expression=expCandSelect,flags="overwrite")
      ,silent=T
      )


    # how much candidates left ?
    nCand <- amGetRasterStat(tmpBestCandidates,'n')

    # convert best candidates to vector
    execGRASS("r.to.vect",
      type="point",
      input=tmpBestCandidates,
      output=outputBestCandidates,
      column="amSuitability",
      flags="overwrite"
      )

    # create summary text
    nExcluded <- candidateCountInit-candidateCountAfter 

    txt <- sprintf("%1$s/%2$s candidate%3$s selected ( %4$s excluded ); max suitability = %5$s/%6$s",
      nCand,
      candidateCountAfter,
      ifelse(nCand>1,"s",""),
      nExcluded,
      as.integer(suitMax),
      max(config$scalingUpRescaleRange)
      )

    res = list(
      candidatesBestVect = outputBestCandidates,
      suitabilityMap = tmpSuitabilityLayer,
      suitabilityMax = suitMax,
      nCandidates = nCand,
      nBeforeExclusion=candidateCountBefore,
      nAfterExclusion=candidateCountAfter,
      msg = txt
      )
  return(res)
  }

}

  
#' create coverage for each candidate
#' @return table containing summary and raster layer generated
#' @export
amScalingUp_evalCoverage <- function(
  session,
  inputCandidates,
  inputSpeed,
  inputFriction,
  inputPopulation, # in scaling up = pop residual
  inputTableCapacity,
  iterationNumber,
  typeAnalysis,
  maxCost,
  maxFacilities,
  pBarTitle,
  pBarPercent,
  dbCon){

  # output candidate evaluation
  candidatesEval <-  list()

  # set iterration number
  i <- iterationNumber
  # Import candidates table
  exp <- sprintf("SELECT cat FROM %1$s LIMIT %2$s",inputCandidates,maxFacilities) 
  candidatesTable <- dbGetQuery(dbCon,exp)
  # get number of candidate to evaluate
  nCandidates <- nrow(candidatesTable)
  # if none, stop.
 # internal iteration
  pIter = 0

  #
  # Evaluate each candidate coverage individually
  #

  # for each candidates, extract a population coverage.
    for(j in candidatesTable$cat ){

      pIter = pIter + 1

      #
      # progress bar
      #

      msg <- sprintf("Evaluate candidate %1$s on %2$s.",pIter,nCandidates)

      pbc(
        visible = TRUE,
        percent = pBarPercent,
        title   = pBarTitle,
        text    = msg
    )


      #
      # Init
      #


      # set a candidate name
      candidateName <- sprintf("facility_%1$s_%2$s",i,j)
      # vector point : one hf
      hfTest <- amRandomName('tmp__hf_new_',j)
      # raster cumul by hf
      hfTestCumul <- amRandomName('tmp__travel_time_',j)
      
      #
      # Extraction
      #
      
      # extract unique candidate at a time
      execGRASS('v.extract',input=inputCandidates,output=hfTest,cats=paste(j),type='point',flags='overwrite')


      #
      # Cumulative cost
      #

      # For this candidate, analyse cumulative cost map without mask
      switch(typeAnalysis,
        'anisotropic'= amAnisotropicTravelTime(
          inputSpeed       = inputSpeed,
          inputHf          = hfTest,
          outputCumulative = hfTestCumul,
          returnPath       = TRUE,
          maxCost          = maxCost,
          minCost          = NULL),
        'isotropic'= amIsotropicTravelTime(
          inputFriction    = inputFriction,
          inputHf          = hfTest,
          outputCumulative = hfTestCumul,
          maxCost          = maxCost,
          minCost          = NULL
          )
        )
      
      #
      # Population by time isoline
      #
      
      # compute integer version of cumulative cost map to use with r.univar, by minutes

      exprTravelTimeInteger <- sprintf("%1$s = int( %1$s )",
        hfTestCumul
        )

      execGRASS('r.mapcalc',expression=exprTravelTimeInteger,flags='overwrite')
      # compute zonal statistic : time isoline as zone
      tblPopByZone<-read.table(
        text=execGRASS(
          'r.univar',
          flags  = c('g','t','overwrite'),
          map    = inputPopulation,
          zones  = hfTestCumul,
          intern = T
          ),sep='|',header=T)


      if(isTRUE(nrow(tblPopByZone)<1)){
      #
      # NO POPULATION FOUND
      #

      warning("amScalingUp_evalCoverage : no population found, set tblPopByZone to 0.")
      tblPopByZone <- data.frame(
        zone = amGetRasterStat(hfTestCumul,'max'),
        sum = 0
        )
      }

      # calculate cumulated sum of pop at each zone
      tblPopByZone$cumSum <- cumsum(tblPopByZone$sum)
      tblPopByZone <- tblPopByZone[c('zone','sum','cumSum')]
      # After cumulated sum, order was not changed, we can use tail/head to extract min max
      totalPop <- round(tail(tblPopByZone,n=1)$cumSum)

      #
      # Extract matching capacity
      #

      # Set capacity using capacity table
 
      hfCap <- inputTableCapacity[
        totalPop >= round(inputTableCapacity$min) &
        totalPop <= round(inputTableCapacity$max)
        ,]

      # If nothing match or multiple match take the more nearest.
      if(isTRUE(!nrow(hfCap)==1 && length(totalPop)>0)){
        warning("amScalingUp_evalCoverage : no matching capacity found, take the nerest.")
        # could be reversed...
        ran <- range(inputTableCapacity$capacity)

        if(abs(totalPop - max(ran))>=abs(totalPop- min(ran))){
          #take min
        hfCap <- inputTableCapacity[inputTableCapacity$capacity==min(ran),]
        }else{
          hfCap <- inputTableCapacity[inputTableCapacity$capacity==max(ran),]
        }
      }
     

      #
      # Return list
      #

      # bind current summary to previous
      candidatesEval[[candidateName]] <- list(
          amProcessingOrder    =  i,
          amRasterCumul        =  hfTestCumul,
          amVectorPoint        =  hfTest,
          amTimeMax            =  maxCost,
          amPopTimeMax         =  totalPop,
          amCapacity           =  hfCap$capacity,
          amLabel              =  hfCap$label,
          tblPopByZone         =  tblPopByZone
          )
    }
  return(candidatesEval)
}


amCatchmentAnalyst <- function(
  inputTablePopByZone,
  inputMapPopInit,
  inputMapPopResidual,
  inputMapTravelTime,
  outputCatchment,
  facilityId,
  facilityIndexField,
  facilityName,     
  facilityNameField,    
  facilityCapacity,
  facilityCapacityField,
  facilityLabel,         
  facilityLabelField,
  iterationNumber,
  totalPop,
  maxCost,
  removeCapted=TRUE,
  vectCatch=TRUE,
  dbCon
  ){

  #
  # Shortcut (translate to old code)
  #

  pbz <- inputTablePopByZone
  tmpCost <- inputMapTravelTime
  idxField <- facilityIndexField
  capacity <- facilityCapacity
  id <- facilityId
  label <- facilityLabel
  name <- facilityName
  incN <- iterationNumber
  outputPopResidual <- inputMapPopResidual


  tmpMask <- amRandomName("tmp__mask")
  tmpPopSub <- amRandomName("tmp__pop_sub") 
  # temp layers

  #
  # output values
  #
  # remaining capacity in HF.
  capacityResidual= as.numeric(NA) 
  propToRemove = as.numeric(NA)
  zMaxI = as.numeric(NA)
  zMaxO = as.numeric(NA)
  pMaxI = as.numeric(NA)
  pMaxO = as.numeric(NA)


  # check time vs pop correlation : 
  # - negative value = covered pop decrease with dist; 
  # - positive value = covered pop increase with dist
  
  corPopTime <- cor(pbz$zone,pbz$sum)
 
  #
  # Inner / outer ring
  #

  # population in first zone
  popFirstZone <- pbz[1,'sum']
  # get the travel time before the limit
  zInner <- pbz[ pbz$cumSum <= capacity, c('zone','sum')]
  zInner$cumSum <- cumsum(zInner$sum)
  # get the travel time that overpass capacity
  zOuter <- pbz[ pbz$cumSum > capacity, c('zone','sum')]
  zOuter$cumSum <- cumsum(zOuter$sum)
  # test number of row
  hasInnerZone <- nrow(zInner) > 0
  hasOuterZone <- nrow(zOuter) > 0
  # get max values
  if(hasInnerZone){
    zMaxI <- max(zInner$zone)
    pMaxI <- sum(zInner$sum)
  }
  if(hasOuterZone){
    zMaxO <- max(zOuter$zone)
    pMaxO <- sum(zOuter$sum)
  }

  #
  # Inner ring calculation : where population cumulative sum is lower or equal facility capacity 
  #

  
  if(hasInnerZone){
    #
    # Get the unused capacity
    #
    capacityResidual <- capacity - pMaxI
    #
    # create a mask with max inner zone. This is the catchment.
    #
    exprMaskInner <- sprintf("%1$s = if( %2$s <= %3$s, 1 , null() ) ",
      tmpMask,
      tmpCost,
      zMaxI
      )

    execGRASS('r.mapcalc',expression=exprMaskInner,flags='overwrite')
    #
    # remove population inside the inverse mask
    #
    if(removeCapted){
      execGRASS("r.mask",raster=tmpMask,flags=c("overwrite","i"))
      exprRmCaptInner <- sprintf("%1$s = %1$s ",
        outputPopResidual
        )
      execGRASS('r.mapcalc',expression=exprRmCaptInner,flags='overwrite')
      execGRASS("r.mask",flags=c("r"))
    }
    #
    # Create vector catchment
    #
    if(vectCatch){

      aCols = list()

      aCols[facilityIndexField] = id
      aCols[facilityNameField] = name
      aCols["type"] <- "inner"


      pathToCatchment <- amCatchPopToVect(
        outCatch          = outputCatchment,
        idField           = idxField,
        idPos             = id,
        incPos            = incN,
        tmpPop            = tmpMask,
        dbCon             = dbCon,
        listColumnsValues = aCols
        )
    }
  }
  #
  # reset residual :
  # if no inner ring has been computed, use capacity as the value to be removed from current or next zone.
  #
  if(is.na(capacityResidual)){
    capacityResidual = capacity
  }
  #
  # Outer ring calculation : where capacity was not full and there is population left
  #
  if(hasOuterZone && capacityResidual > 0 ){

    #
    # Set fraction of population to remove
    #

    if( pMaxO <= capacityResidual ){
      propToRemove <- 1
      capacityResidual <- capacityResidual-pMaxO
    }else{
      # take the first ring where pop outnumber capacityResidual 
      zOuter <- zOuter[ zOuter$cumSum > capacityResidual,][1,]
      # redefine zMaxO
      zMaxO <- max(zOuter$zone)
      propToRemove <- capacityResidual/zOuter$cumSum
      capacityResidual <- 0 
    }

    if(propToRemove < 0 || propToRemove > 1){
      stop("propToRemove not in range")
    }
    #
    # create a mask with max outer zone
    #

    exprMaskOuter <- sprintf("%1$s = if( %2$s <= %3$s, 1 , null() ) ",
      tmpMask,
      tmpCost,
      zMaxO
      )

    execGRASS('r.mapcalc',
      expression=exprMaskOuter,
      flags='overwrite'
      )


    if(removeCapted){
    #
    # calc subset the fraction of population to remove
    #
      exprSubsetPop <- sprintf(" %1$s = %2$s * ( %3$s - %3$s * %4$s)",
        tmpPopSub,
        tmpMask,
        outputPopResidual,
        propToRemove
        )

      execGRASS('r.mapcalc',
        expression=exprSubsetPop,
        flags="overwrite"
        )

   
      #
      # patch it with pop residual map
      # (first layer overwrite second layer)
      #

      execGRASS('r.patch',
        input=c(tmpPopSub,outputPopResidual),
        output=outputPopResidual,
        flags='overwrite')



    }
    if(vectCatch){


      aCols = list()

      aCols[facilityIndexField] = id
      aCols[facilityNameField] = name
      aCols["type"] <- "outer"

      pathToCatchment <- amCatchPopToVect(
        outCatch          = outputCatchment,
        idField           = idxField,
        idPos             = id,
        incPos            = incN,
        tmpPop            = tmpMask,
        dbCon             = dbCon,
        listColumnsValues = aCols
        )
    }
  }

  #
  # population coverage analysis.
  #
  if(removeCapted){
    popCoveredPercent <- amGetRasterPercent(outputPopResidual,inputMapPopInit)
  }
 
  #
  # Output capacity table
  #
  capacityDf=data.frame(
    id, # id of hf / group of hf
    name, 
    capacity, # capacity from hf table
    label,
    incN, # processing order position
    corPopTime, # corrrelation (pearson) between time (zone) and population (sum)
    capacityResidual, # capacity not filled
    capacity-capacityResidual,# capacity realised
    maxCost, # max allowed travel time (time)
    totalPop, # total population within max time (minutes)
   # popFirstZone, # population under start cell
    popCoveredPercent, # if covered pop removed, percent of total.
    zMaxI, # maximum travel time for the inner ring. below this, we have covered all patient
    zMaxO, # maximum travel time for outer ring. below this, we have covered a fraction of patient,
    propToRemove
    )
  # renaming table
  # TODO: set all names in config file
  names(capacityDf)<-c(
    facilityIndexField,
    facilityNameField,
    facilityCapacityField,
    facilityLabelField,
    'amProcessingRank',
    'amCorrPopTime',
    'amCapacityResidual',
    'amCapacityRealised',
    'amTimeMax',
    'amPopTimeMax',
    #'amPopFirstZone',
    'amPopCoveredPercent',
    'amTimeLimitInnerRing',
    'amTimeLimitOuterRing',
    'amPopPropRemovedOuterRing'
    )

  # result list

  msg <- sprintf("Extraction of the catchment for candidate %1$s done. %2$s %% of the population is covered. ",
    incN,
    round(popCoveredPercent,4)
    )


  list(
    amCatchmentFilePath=pathToCatchment,
    amCapacityTable=capacityDf,
    msg = msg
    )



}


#' Extract the best coverage evaluation
#' @param listEvalCoverage Output from amScalingUp_evalCoverage
#' @param criteria List item name containing the value to maximize
#' @return listEvalCoverage subset
amScalingUp_extractBest<-function(listEvalCoverage,criteria="amPopTimeMax"){
  # take the Best Candidate according to criteria.
  # NOTE: which.max return one item : in case of tie, this could be a problem

  maxSums <- lapply(listEvalCoverage,
    function(x){
      max(x$tblPopByZone)
    }
    )

  if(!all(maxSums==0)){
    lapply(listEvalCoverage,'[[',criteria)%>%
    which.max() %>%
    names() %>%
    listEvalCoverage[[.]]
  }else{
    return()
  }

}




#' Create or import to new facility layer
#' Import existing facility or create an empty facility layer
#' @param useExistingFacilities Boolean use existing facility layer
#' @param inputFacility Name of the facility layer to import
#' @param inputCatToImport Filter given cat (id). Eg. from input table
#' @param inputHfIdx Field name containing index to preserve
#' @param inputHfName Field name containing facility name to preserve
#' @param outputFacility Name of the facility layer to create
#' @param newColumnsGrass Fields name of the output layer.
#' @param dbCon Sqlite dbcon object
#' @return New facility map name 
#' @export
amScUpPop_createNewFacilityLayer <- function(
  useExistingFacility=FALSE,
  inputFacility,
  inputCatToImport,
  inputHfIdx,
  inputHfName,
  outputFacility,
  newColumnsDb,
  dbCon
  ){

  # If there is some facilities to import, extract them, else create new layer
  if(length(inputCatToImport)>0 && useExistingFacility){
    execGRASS('v.extract',
      input = inputFacility,
      output = outputFacility,
      cats = paste(as.character(inputCatToImport),collapse=','),
      flags = "overwrite"
      )
  }else{
    execGRASS("v.edit",
      tool = "create",
      map = outputFacility,
      flags = "overwrite"
      )
    execGRASS("v.db.addtable",
      map = outputFacility
      )
  }

  # Add additional fields. Could use
 colsName <- dbGetQuery(
    dbCon,
    sprintf(
      "PRAGMA TABLE_INFO(%s)",
      outputFacility 
      )  
    )$name
  
 newColumnsDb <- newColumnsDb[!sapply(newColumnsDb,'[[',1) %in% colsName]

  if(length(newColumnsDb)>1){
  columns <- sapply(
    newColumnsDb,
    paste,
    collapse=" ")%>%
  paste(.,collapse=",")

  execGRASS("v.db.addcolumn",
    map=outputFacility,
    columns=columns
    )
  }
  #BUG: dbWriteTable does not use field.types argument when the type is  not 
  #     in text,blob,integer,null, but grass does !
  #     Here we copy the complete table and re import to get correct field types.

  hfTable <- dbReadTable(
    dbCon,
    outputFacility
    )

  dbWriteTable(dbCon,
    outputFacility,
    hfTable,
    overwrite=TRUE
    )

  return(outputFacility)

}


#' Merge generated facility to output file
#' @param fieldsJoin Vector of name for a futur join between catchment, coverage table and facility point layer
#' @param listEvalCoverageBest  List containing result from previous computation
#' @param listSummaryCatchment List containing result from pevious computation
#' @param outputFacility Layer name of output facility points
#' @param dbCon Sqlite connection to update sqlite db
#' @return 
#' @export 
 amScalingUp_mergeNewHf <- function(
    fieldsJoin,
    listEvalCoverageBest,
    listSummaryCatchment,
    outputFacility,
    dbCon
    ){

    # short var names
    eb <- listEvalCoverageBest
    cs <- listSummaryCatchment 
    of <- outputFacility

    # extract values

    # best candidate vector layer name
    bc <- eb$amVectorPoint

    # capacity analysis table
    ct <- cs$amCapacityTable


    # NOTE: we have to update best candidate's attributes before the join. 
    # 2 Solutions : 
    #  - using v.db.addcolumn, add columns AND type for each attributes as in outputFacility 
    #  - create an empty df, update with value from best candidate, delete old table, rewrite.

    # Applied here : second solution

    # get and empty row with original column and types.
    cols <- dbGetQuery(
      dbCon,
      sprintf(
        "SELECT * 
        FROM %s 
        LIMIT 0",
        of
        )
      )

    cols[1,] <- rep(NA,length(cols))

    # get temporary candidate cat value 
    # NOTE: this is not always 1, in case of multiple candidates
    catBc <- dbGetQuery(
      dbCon,
      sprintf(
        "SELECT CAT
        FROM %s
        LIMIT 1",
        bc
        )
      )


    cols$cat <- as.integer(catBc)


    # transfer values from catchment capacity table to new hf
    cols[fieldsJoin] <- ct[fieldsJoin]

    # drop best candidate table

    # write new table
    dbWriteTable(
      dbCon,
      bc,
      cols,
      overwrite=TRUE
      )


    # append new hf to output file
    execGRASS("v.patch",flags=c("overwrite","e","a"),
      input=bc,
      output=of
      ) 
  }

#' Import temporary shapefile catchment to final directory
#' @param catchmentTempShp Full path to temp catchment file . eg. /tmp/super.shp
#' @param cathcmentsDir Directory path where are stored shapefile. eg. /home/am/data/shapefiles/
#' @param outputCatchment Name of the final catchment shapefile, without extension. e.g. catchments_001
#' @return Boolean Done
mvShp <- function(shpFile,outDir,outName){
  #
  # Collect all shp related file and copy them to final directory. 
  # NOTE: make sure that: 
  # - pattern of shapefile is unique in its directory

  # in case of variable in path, convert outdir to fullpath
  if(length(shpFile)<1){
    amDebugMsg("No catchment in list")
    return()
  }
  outDir <- system(sprintf("echo %s",outDir),intern=T)

  fe <- file.exists( shpFile )
  de <- dir.exists( outDir )
  so <- isTRUE( grep( ".*\\.shp$" , shpFile ) >0 )

  if( !fe ) warning( 
    sprintf("mvShp: %s input file does not exists",shpFile)
    )
  if( !de ) warning( 
    sprintf("mvShp: %s output directory does not exists",outDir)
    )
  if( !so ) warning( 
    sprintf("mvShp: %s input file does not have .shp extension",shpFile)
    )

  ok<-c(fe,de,so)

  if(all(ok)){
    # base name file for pattern.
    baseShape <- gsub('.shp','',basename(shpFile))
    # list files (we can also use )
    allShpFiles <- list.files(dirname(shpFile),pattern=paste0('^',baseShape),full.names=TRUE)
    # Copy each files in final catchment directory.
    for( s in allShpFiles){
      sExt <- file_ext(s)
      newPath <- file.path(outDir,paste0(outName,'.',sExt))
      file.copy(s,newPath,overwrite=T) 
    } 
  }
  return(all(ok))
}



#
#
#' Create new hf based on exclusion rules and multicriteria map
#' @export
amScalingUp<-function(
  session=shiny:::getDefaultReactiveDomain(),
  inputSpeed, # name of speed map based on scenario
  inputFriction, # name of friction map based on scenario
  inputPop, # name of input  population 
  inputPopResidual, # name of input population or population residual
  inputFacility, # name of input facilities layer.
  inputTableFacility, # table of facilities
  inputTableCapacity, # table of capacities
  inputTableExclusion, # table of exclusion layers
  inputTableSuitability, # table of suitability index items
  outputFacility, # name of the output facilities layer 
  outputPopResidual, # name of the output residual pop layer
  outputCatchment, # name of the output catchment layer
  outputCapacityAnalysis, # name of the output capacity analysis
  maxCost,# maximum travel time
  useExistingFacilities,# boolean import existing facilities
  facilityIndexField, # existing facility index field 
  facilityCapacityField, # existing facility capacity field
  facilityNameField, # existing facility name field
  typeAnalysis, # type of analysis : iso or anisotropic
  limitFacilitiesNumber, # max number of facilities
  limitProcessingTime, # maximum processing time
  limitPopCoveragePercent, # maximum population coverage in percent
  pBarTitle,
  dbCon
  ){
  #
  # Initialisation
  #
  # clean mask and set message
  pbc(
    visible=TRUE,
    percent=10,
    title=pBarTitle,
    text="Initialisation...")

  amMsg(session,"log",text=sprintf("Scaling up requested"))
  rmRastIfExists("MASK")

  # Keep initial state of arguments
  argInit <- as.list(environment())

  # Remove db and r6 classes (not parsable as json)
  rmDbClass <- function(x){ 
    !any(class(x) %in% c('SQLiteConnection','R6')) 
  }
  argInit <- argInit[
    sapply(argInit,rmDbClass)]

  # structure of the results list
  result = list(
    timing = list(),
    arguments = argInit,
    statistic = list(
      population = list()
      )
    )




  # set limits
  if(isTRUE(limitFacilitiesNumber < 1)) limitFacilitiesNumber <- 99999
  if(isTRUE(limitProcessingTime < 1)) limitProcessingTime <- 99999
  if(isTRUE(limitPopCoveragePercent < 1)) limitPopCoveragePercent <- 100

  # Set progression variables
  #progTot <- limitFacilitiesNumber
  progInit <- 10 # 10 are taken in previous step
  progNum <- 0 

  # Output vector containing new HF
  # set index column
  facilityIndexField <- "amScUpId"
  # set capacity field
  facilityCapacityField <- "amScUpCap"
  # set facility field
  facilityNameField <- "amScUpName"
  # set label field 
  facilityLabelField <- "amScUpLab"

  # set covered pupulation at given time
  facilityPopTimeField <- sprintf("amScUpPop%smin",as.integer(maxCost/60))

  #
  # temp layer
  #
  # Best candidates 
  tmpBestCandidates <-  amRandomName("tmp__best_candidates")
  # Population residual
  #tmpPopRes <- amRandomName("tmp_pop_residual") 
  tmpCandidates <- amRandomName("tmp_candidates")
  tmpCatchment <- amRandomName("tmp__catchment")

  # Reevaluate suitability map at each iteration
  # Suitability map will be modified at least one of those 
  # layer are given in table
  # - population density (population will be removed : change of density)
  # - generated facilities (new HF will be created)
  # or..
  # if exclusion area is modified : suitability range could change.
  redoSuitabilityMap <- TRUE 
  
  #
  listSummaryCatchment = list()
  tableCapacityStat = data.frame()
  #
  # Population stat and temporary layer creation
  #

  # Get initial percentage of population coverage.

  #result$statistic$population$initialCoverage <- coverageInit

  # convert population nulls to zeros
  #exp <- sprintf("%1$s=if(isnull(%2$s),0,%2$s)",tmpPop,inputPopResidual)
  #execGRASS("r.mapcalc",expression=exp,flags="overwrite")

  # 
  # Population residual temporary
  #

  # If residual pop is used in suitability by density, replace with temporary residual pop. This layer will change at each iteration.
  #  popLayers <- grep(
  #    inputPopResidual,
  #    inputTableSuitability$layer
  #    )
  #  inputTableSuitability[popLayers,'layer'] <- tmpPopRes

  # create tmpPop 
  amScalingUp_createPopulationOut(
    input = inputPopResidual,
    output = outputPopResidual
    )

  #
  # Facilities output layer column name 
  # (for the join with catchment and catchment summary)
  #  
  newFieldsList <- list(
    c(facilityNameField,"text"),
    c(facilityIndexField,"integer"),
    c(facilityCapacityField,"integer"),
    c(facilityLabelField,"text")
    )

  #
  # Create output facility layer
  #


  # check if we have output facility item in tables
  hasFacilityExclusion <- outputFacility %in% inputTableExclusion$layer
  hasFacilitySuitability <- outputFacility %in% inputTableSuitability$layer



  # Use existing facilities double check (already done in validation...)
  useExistingFacility <- isTRUE(
    nrow(inputTableFacility) > 0 && 
    amVectExists(inputFacility) &&
    useExistingFacilities
    )


  amScUpPop_createNewFacilityLayer(
    useExistingFacility = useExistingFacility,
    inputFacility       = inputFacility,
    inputCatToImport    = inputTableFacility$cat,
    inputHfIdx          = facilityIndexField,
    inputHfName         = facilityNameField,
    outputFacility      = outputFacility,
    newColumnsDb        = newFieldsList,
    dbCon               = dbCon
    )



  # create initial candidate layer :
  # has value = 1
  # is null = 0
  # note : could be residual population
  # after each iteration, a mask based on exclusion table will be applied.

  amScalingUp_createCandidatesTemp(
    input = inputSpeed,
    output = tmpCandidates
    )

  candidateCountInit <- amGetRasterStat(tmpCandidates,'n')


  nRules <- nrow(inputTableExclusion)+nrow(inputTableSuitability)


  quit <- FALSE
  start <- Sys.time()

  for(i in 1:limitFacilitiesNumber){


    if(!quit){
      #
      # LOOP START
      #

      progNum <- progNum + 1

      hfName <- sprintf("facility_%1$s",i)
      hfId <- i
      nCandidates <- amGetRasterStat(tmpCandidates,'n')
      pCoverage <- amGetRasterPercent(outputPopResidual,inputPop)
      elapsedMinutes <- as.numeric(difftime(Sys.time(),start,units="min"))


      #
      # Set progress bar percent
      #

      pBarMax <- max(
        c(
          pBarCov        <- pCoverage/limitPopCoveragePercent * 100,
          pBarPercentFac <- progNum/limitFacilitiesNumber * 100,
          pBarPercentTim <- elapsedMinutes/limitProcessingTime*100 
          )
        )

      progInc <- (100 - progInit) / 100  #10 percent are already lost in config step
      pBarPercent <- progInit + progInc * ifelse(pBarMax>=100,100,pBarMax)    


      pbc(
        visible=TRUE,
        percent=pBarPercent,
        title=pBarTitle,
        text=sprintf("Population coverage = %1$s %%",round(pCoverage,3))
        )


      reachedPop <- isTRUE(pCoverage >= limitPopCoveragePercent)
      reachedTime <- isTRUE(elapsedMinutes >= limitProcessingTime)
      reachedCandidates <- isTRUE(nCandidates < 1)


      if( reachedPop || reachedTime || reachedCandidates ){
        quit <- TRUE

        if(reachedPop){
          pbc(
            visible=TRUE,
            percent=100,
            title=pBarTitle,
            text=sprintf("Population coverage of %1$s %% reached. Cleaning...",limitPopCoveragePercent),
            timeOut=5
            )
        }
        if(reachedTime){
          pbc(
            visible=TRUE,
            percent=100,
            title=pBarTitle,
            text=sprintf("Processing time of %1$s reached. Cleaning...",limitProcessingTime),
            timeOut=5
            )
        }

        if(reachedCandidates){
          pbc(
            visible=TRUE,
            percent=100,
            title=pBarTitle,
            text=sprintf("No more candidates. Cleaning..."),
            timeOut=5
            )
        }

      }else{

        pbc(
          visible = TRUE,
          percent = pBarPercent,
          title   = pBarTitle,
          text    = sprintf(
            "Iteration number %1$s : find best candidates. Appling %2$s rules on %3$s cells, this may take a while.",
            progNum,
            nRules,
            nCandidates
            ),
          timeOut = 2
          )

        #
        # Search for best candidates
        #

        listEvalBest <- amScalingUp_findBestCells(
          inputFriction         = inputFriction,
          inputSpeed            = inputSpeed,
          inputTableExclusion   = inputTableExclusion,
          inputTableSuitability = inputTableSuitability,
          inputCandidates       = tmpCandidates,
          outputBestCandidates  = tmpBestCandidates,
          candidateCountInit    = candidateCountInit
          )


        if(!isTRUE(listEvalBest$noMoreCandidates)){
          pbc(
            visible = TRUE,
            percent = pBarPercent,
            title   = pBarTitle,
            text    = listEvalBest$msg,
            timeOut = 4
            )
          #
          # Evaluate selected candidates
          #

          listEvalCoverage <- amScalingUp_evalCoverage(
            session            = session,
            inputCandidates    = tmpBestCandidates,
            inputSpeed         = inputSpeed,
            inputFriction      = inputFriction,
            inputPopulation    = outputPopResidual,
            inputTableCapacity = inputTableCapacity,
            iterationNumber    = progNum,
            typeAnalysis       = typeAnalysis,
            maxCost            = maxCost,
            maxFacilities      = limitFacilitiesNumber,
            dbCon              = dbCon,
            pBarTitle          = pBarTitle,
            pBarPercent        = pBarPercent
            )

          pbc(
            visible = TRUE,
            percent = pBarPercent,
            title   = pBarTitle,
            text    = sprintf("Candidate evaluation finished.")
            )

          #
          # Select best
          #

          listEvalCoverageBest <- amScalingUp_extractBest(
            listEvalCoverage = listEvalCoverage,
            criteria         = "amPopTimeMax"
            )

          #
          # Catchment creation. 
          # NOTE: this function could be reused in capacity analysis. Be careful with variable names..
          #

      

          listSummaryCatchment <- amCatchmentAnalyst(
            inputTablePopByZone     = listEvalCoverageBest$tblPopByZone,
            inputMapPopInit         = inputPop,
            inputMapPopResidual     = outputPopResidual,
            inputMapTravelTime      = listEvalCoverageBest$amRasterCumul,
            outputCatchment         = tmpCatchment,
            facilityCapacityField   = facilityCapacityField,
            facilityCapacity        = listEvalCoverageBest$amCapacity,
            facilityLabelField      = facilityLabelField,
            facilityLabel           = listEvalCoverageBest$amLabel,
            facilityIndexField      = facilityIndexField,
            facilityId              = hfId,
            facilityNameField       = facilityNameField,
            facilityName            = hfName,
            totalPop                = listEvalCoverageBest$amPopTimeMax,
            maxCost                 = listEvalCoverageBest$amTimeMax,
            iterationNumber         = listEvalCoverageBest$amProcessingOrder,
            removeCapted            = TRUE,
            vectCatch               = TRUE,
            dbCon                   = dbCon
            )

          pbc(
            visible = TRUE,
            percent = pBarPercent,
            title   = pBarTitle,
            text    = listSummaryCatchment$msg,
            timeOut = 4
            )

          #
          # Populate or update output capacity table
          #

          if(i == 1){
            tableCapacityStat = listSummaryCatchment$amCapacityTable
          }else{
            tableCapacityStat = rbind(
              tableCapacityStat,
              listSummaryCatchment$amCapacityTable
              )
          }

          #
          # Populate hf vector
          #

          #facilityFields contain all fields,
          amScalingUp_mergeNewHf(
            fieldsJoin           = c(
              facilityIndexField,
              facilityNameField,
              facilityCapacityField,
              facilityLabelField
              ),
            listEvalCoverageBest = listEvalCoverageBest,
            listSummaryCatchment = listSummaryCatchment,
            outputFacility       = outputFacility,
            dbCon                = dbCon
            )

        }

        rmVectIfExists("tmp__*")
        rmRastIfExists("tmp__*")
      }
    }
  }

  ## export shapefile
  if(length(listSummaryCatchment)>0){
  mvShp(
    shpFile=listSummaryCatchment$amCatchmentFilePath,
    outDir=config$pathShapes,
    outName=outputCatchment
    )
  
 pbc(
    visible = TRUE,
    percent = 100,
    title   = pBarTitle,
    text    = "Catchements saved in database, closing.",
    timeOut = 2
    )
  }

  ## add new table containing coverage result
  if(length(tableCapacityStat)>0){
  dbWriteTable(dbCon,
    outputCapacityAnalysis,
    tableCapacityStat,
    overwrite=T
    )
 pbc(
    visible = TRUE,
    percent = 100,
    title   = pBarTitle,
    text    = "Capacity analysis table saved in database, closing.",
    timeOut = 2
    )

  }

 
  pbc(
    visible = FALSE,
    percent = 0,
    title   = "",
    text    = "",
    timeOut = 0
    )
  return()
}

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
  outputTableHf,
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
  capLabelField,
  orderField=NULL,
  zonalCoverage=FALSE,
  zoneFieldId=NULL,
  zoneFieldLabel=NULL,
  hfOrder=NULL,
  hfOrderSorting=NULL,
  dbCon=NULL
  ){


# if cat is set as index, change to cat_orig
  if(hfIdx=='cat'){
    hfIdxNew='cat_orig'
  }else{
    hfIdxNew=hfIdx
  }


  #
  # Compute hf processing order
  #

    hfOrderDecreasing<-ifelse(hfOrderSorting=='hfOrderDesc',TRUE,FALSE)
  # nested call if requested order is not given by input hf table
  # hfOrder could be 'tableOrder','travelTime' or 'circlBuffer'
  # If hfOrder is not 'tableOrder' or 'circBuffer', an isotropic or anisotropic will be done.
  # In this case, typeAnalysis will be set from parent function call.

    if(!hfOrder == 'tableOrder' && ! is.null(hfOrder)){
      # extract population under max time/distance
      popWithinDist <- amCapacityAnalysis(
        inputSpeed        = inputSpeed,
        inputFriction     = inputFriction,
        inputPop          = inputPop,
        inputHf           = inputHf,
        inputTableHf        = inputTableHf,
        outputPopResidual = 'tmp_nested_p',
        outputTableHf       = "tmp_nested_hf",
        outputHfCatchment = "tmp_nested_catch",
        typeAnalysis      = ifelse(hfOrder=='circBuffer','circular',typeAnalysis),
        returnPath        = returnPath,
        radius            = radius,
        maxCost           = maxCostOrder,
        hfIdx             = hfIdx,
        nameField         = nameField,
        capField          = capField,
        capLabelField     = capLabelField,
        orderField        = orderField,
        hfOrderSorting    = hfOrderSorting
        )[['capacityTable']][c(hfIdxNew,'amPopTimeMax')]
      # define the order based on hfOrderSorting
      orderId <- popWithinDist[order(
          popWithinDist$amPopTimeMax,
          decreasing=hfOrderDecreasing
          ),hfIdxNew]
    }else{
      orderId=unique(inputTableHf[order(
            inputTableHf[orderField],
            decreasing=hfOrderDecreasing
            ),hfIdx])
    }

    amMsg(session,'log',text=paste('Order process for',inputHf,'(',hfIdxNew,') will be',paste(orderId,collapse=',')))

  #
  # stop of orderId is not defined
  #

    if(amNoDataCheck(orderId)){
      log = list(
        message = "orderId bug.",
        orderField = orderField,
        decreasing = hfOrderDecreasing,
        hfIdx = hfIdx,
        inputHf = inputHf, 
        map=outputPopResidual,
        zones=tmpCost
        )
      log = HTML(listToHtml(log,h=5))
      amMsg(session,type='error',title='No order defined',text=log)
      return()
    }

  #
  # clean and initialize object outside the loop
  #
  
  
  # temp. variable
  tmpHf             <- 'tmp__h' # vector hf tmp
  tmpCost           <- 'tmp__c' # cumulative cost tmp
  tmpPop            <- 'tmp__p' # population catchment to substract
  tblOut            <- data.frame() # empty data frame for storing capacity summary
  amTtInner         <- 0 # init inner ring
  amTtOuter         <- 0 # init outer ring
  popSum            <- amGetRasterStat(inputPop,"sum") # initial population sum
  popCoveredPercent <- NA # init percent of covered population
  inc               <- 90/length(orderId) # init increment for progress bar
  incN              <- 0 # init counter for progress bar
  tmpVectCatchOut   <- NULL
  # create residual population 
  execGRASS('g.copy',raster=c(inputPop,outputPopResidual),flags='overwrite')

  #
  # Start loop on facilities according to defined order
  #
  for(i in orderId){
    incN=incN+1
    # extract temporary facility point
    qSql<-paste(hfIdx,"IN (",paste0("'",i,"'",collapse=','),")")
    execGRASS("v.extract",flags='overwrite',input=inputHf,where=qSql,output=tmpHf)
    # compute cumulative cost map
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
    # compute integer version of cumulative cost map to use with r.univar
    exprIntCost <- paste(tmpCost,'=int(',tmpCost,')')
    execGRASS('r.mapcalc',expression=exprIntCost,flags='overwrite')
    # compute zonal statistic : time isoline as zone
    tblPopByZone <- read.table(
      text=execGRASS(
        'r.univar',
        flags  = c('g','t','overwrite'),
        map    = outputPopResidual,
        zones  = tmpCost,
        intern = T
        ),sep='|',header=T)
  
    #
    # If table of population by zone not defined, return a message.
    #
    if(!exists("tblPopByZone")){
      log = list(
        inputHf = inputHf, 
        map = outputPopResidual,
        zones = tmpCost
        )
      log = HTML(listToHtml(log,h=5))
      amMsg(session,type='warning',title='Table of zonal population not generated',text=log)
      return()
    }
    # calculate cumulated sum of pop at each zone
    tblPopByZone$cumSum <- cumsum(tblPopByZone$sum)
    tblPopByZone <- tblPopByZone[c('zone','sum','cumSum')]


    #
    # get stat
    #

    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    totalPop <- tail(tblPopByZone,n=1)$cumSum
    # check time vs pop correlation : negative value = covered pop decrease with dist; positive value = covered pop increase with dist
    corPopTime <- cor(tblPopByZone[,c('zone','sum')]) 
    # extract hf total capacity. Sum if hfIdx is a group.
    hfCap <- sum(inputTableHf[inputTableHf[hfIdx]==i,capField])
    #
    hfLabel <- inputTableHf[inputTableHf[hfIdx]==i,capLabelField]
    #
    hfName <- inputTableHf[inputTableHf[hfIdx]==i,nameField]

 
    #
    # Inner / outer zone
    #


   listSummaryCatchment <- amCatchmentAnalyst(
          inputTablePopByZone     = tblPopByZone,
          inputMapPopInit         = inputPop,
          inputMapPopResidual     = outputPopResidual,
          inputMapTravelTime      = tmpCost,
          outputCatchment         = outputHfCatchment,
          facilityCapacityField   = capField,
          facilityCapacity        = hfCap,
          facilityLabelField      = capLabelField,
          facilityLabel           = hfLabel,
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



   tmpVectCatchOut <- listSummaryCatchment$amCatchmentFilePath


  tblOut<-rbind(tblOut,listSummaryCatchment$amCapacityTable)
  # clean and set progress bar
  progValue<-inc*incN+10
  amUpdateProgressBar(session,"cumulative-progress",round(inc*incN)+10)
  rmRastIfExists('tmp__*')
  rmVectIfExists('tmp__*')
  tblPopByZone=NULL
  } 
  

  #
  # end of loop
  #


  #
  # optional zonal coverage using admin zone polygon
  #

  if(zonalCoverage){
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


  #
  #  move catchemnt shp related file into one place
  #



  if(vectCatch){
    if(!file.exists(tmpVectCatchOut)){
      stop('Error : the output catchment area was requested but not created. Please report this bug and provide the dataset.')
    }
    # base name file
    baseCatch <- gsub('.shp','',basename(tmpVectCatchOut))
    # list files
    allShpFiles <- list.files(dirname(tmpVectCatchOut),pattern=paste0('^',baseCatch),full.names=TRUE)
    # Copy each files in shp location.
    for( s in allShpFiles){
      sExt <- file_ext(s)
      newPathGrass <- file.path(catchPath,paste0(outputHfCatchment,'.',sExt))
      newPath <- system(paste('echo',newPathGrass),intern=T)
      file.copy(s,newPath,overwrite=T) 
    }
  }

  if(!removeCapted)rmRastIfExists(outputPopResidual)

  # remove remaining tmp file (1 dash)
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')

  return(
    list(
      capacityTable=tblOut,
      zonalTable=tblPopByZone
      )
    )
}


#' amCatchPopToVect
#' handle population catchment area
#' @param idField Name of the facility id column.
#' @param idPos String id currently processed.
#' @param incPos Numeric increment position.
#' @param tmpPop Map raster name population catchment (mask)
#' @param outCatch Name of temporary grass and shapefile catchment file
#' @param listColumnsValue Alternative list of value to put into catchment attributes. Must be a named list.
#' @param dbCon  RSQlite connection to update value of catchment after vectorisation. 
#' @return Shapefile path
#' @export
amCatchPopToVect<-function(
  idField,
  idPos,
  incPos,
  tmpPop,
  outCatch="tmp__vect_catch",
  listColumnsValues=list(),
  dbCon){

  # NOTE: output catchment as vector, merged and easily readable by other GIS.
  # None of those methods worked at the time this script was written :
  # v.overlay :  geometry / topology ok, seems the way to go ! But... how to handle hundred of overlays ? 
  #              And grass doesn't like to work with non topological 'stacked' data. 
  # v.patch : produced empty area and topologic errors, even without topology building (b flag)
  # v.to.3d with groupId as height and v.patch after. V.patch transform back to 2d... with area errors.
  # r.to.rast3 :groupId as Z. doesn't produce anything + 3d interface really bugged.
  # NOTE: this was not possible to append catchnment to geojson.
  # NOTE: this could work with line instead of area. It was required to use area, but...

  idField <- ifelse(idField=="cat","cat_new",idField)
  # convert raster catchment to vector
  #   execGRASS('r.to.vect',
  #    input=tmpPop,
  #    output=outCatch,
  #    type='area',
  #    flags=c('overwrite','v'),
  #    column=idField)


  #
  # Export catched population to vector
  #

  execGRASS("r.to.vect",
    input  = tmpPop,
    output = outCatch,
    type   = "area",
    flags  = c("overwrite")
    )

  #
  # Dissolve result to have unique id by feature
  #
  outCatchDissolve <- amRandomName("tmp__catch_dissolve")
  execGRASS("v.dissolve",
    input  = outCatch,
    output = outCatchDissolve,
    column = "value",
    flags  = c("overwrite")
    )

  #
  # Create a table for catchment
  #

  execGRASS("v.db.addtable",
    map = outCatchDissolve
    )


  # So, export and append to shapefile ( reimport back after the loop. eerk.)
  tDir <- tempdir()
  tmpVectCatchOut <- file.path(tDir,paste0(outCatch,'.shp'))
  # for the first catchment : overwrite if exists, else append.
  if(incPos==1){
    if(file.exists(tmpVectCatchOut)){ 
      file.remove(tmpVectCatchOut)
    }
    outFlags=c('overwrite','m')
  }else{
    outFlags=c('a','m')
  }
  # update attribute table with actual ID.
  dbRec<-dbGetQuery(dbCon,paste('select * from',outCatchDissolve))

  if(length(listColumnsValues)>0){
    for(n in names(listColumnsValues)){
      dbRec[n] <- listColumnsValues[n]
    }
  }else{
    dbRec[idField] <- idPos
  }

  dbWriteTable(dbCon,outCatchDissolve,dbRec,overwrite=T)
  # export to shapefile. Append if incPos > 1
  execGRASS('v.out.ogr',
    input=outCatchDissolve,
    output=tmpVectCatchOut,
    format='ESRI_Shapefile',
    flags=outFlags,
    output_layer=outCatch
  )
  return(tmpVectCatchOut)
}


    #lco="SHPT=POLYGONZ",


#'amReferralTable
#'@export
amReferralTable<-function(session=shiny:::getDefaultReactiveDomain(),inputSpeed,inputFriction,inputHf,inputHfTo,inputTableHf,inputTableHfTo,idField,idFieldTo,labelField,labelFieldTo,typeAnalysis,resol,dbCon, unitCost=c('s','m','h'),unitDist=c('m','km'),outReferral,outNearestDist,outNearestTime){

  #TODO: describe input and what is returned.

  # check the clock
  timeCheckAll<-system.time({
    # set increment for the progress bar.
    incN=0
    inc=90/nrow(inputTableHf)
    ## subset value for table formating.
    #labelFrom <- inputTableHf[[labelField]]
    #labelTo <- inputTableHfTo[[labelFieldTo]]
    #indexFrom <- inputTableHf[[idField]]
    #indexTo <- inputTableHfTo[[idFieldTo]]

    # set output table header label
    hIdField <- paste0('from','__',amSubPunct(idField)) # amSubPunt to avoid unwanted char (accent, ponctuation..)
    hLabelField <- paste0('from','__',amSubPunct(labelField))
    hIdFieldTo <- paste0('to','__',amSubPunct(idFieldTo))
    hLabelFieldTo <- paste0('to','__',amSubPunct(labelFieldTo))
    hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idFieldTo))
    hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelFieldTo))
    hDistUnit <-paste0('distance','_',unitDist)
    hTimeUnit <- paste0('time','_',unitCost)

    # Create destination HF subset (To). 
    # NOTE: this has already be done outside for other functions.. but for coherence with origin HF (From) map, which need to be subseted in the loop, we also subset destination HF here.
    qSqlTo<-paste("cat IN (",paste0(inputTableHfTo$cat,collapse=','),")")
    execGRASS("v.extract",flags=c('overwrite'),input=inputHfTo,where=qSqlTo,output='tmp_ref_to')
  # cost and dist from one to all selected in table 'to'
  for(i in inputTableHf$cat){  
    timeCheck<-system.time({
      incN=incN+1
      qSqlFrom<-paste("cat==",i)
      # create temporary origine facility map (from) 
      execGRASS("v.extract",flags=c('overwrite'),input=inputHf,where=qSqlFrom,output='tmp__ref_from')
      # NOTE: only extract coordinate instead ? No.. we need points in network. 
      # create cumulative cost map for each hf : iso or aniso
      switch(typeAnalysis,
        'anisotropic'=amAnisotropicTravelTime(
          inputSpeed=inputSpeed,
          inputHf='tmp__ref_from',
          inputStop='tmp_ref_to',
          outputCumulative='tmp__cost', 
          outputDir='tmp__ref_dir',
          returnPath=FALSE,
          maxCost=0
          ),
        'isotropic'=amIsotropicTravelTime(
          inputFriction=inputFriction,
          inputHf='tmp__ref_from',
          inputStop='tmp_ref_to',
          outputCumulative='tmp__cost',
          outputDir='tmp__ref_dir',
          maxCost=0
          )
        )
      # extract time cost V1 = hf cat dest; V2 = time to reach hf
      refTime=execGRASS(
        'v.what.rast',
        map='tmp_ref_to',
        raster='tmp__cost',
        flags='p',
        intern=T
        )%>%
      gsub('\\*',NA,.) %>%
      na.omit %>%
      read.table(text=.,sep='|')
      # rename grass output
      names(refTime)<-c('tcat',hTimeUnit)
      #unit transformation 
      if(!unitCost =='m'){
        div<-switch(unitCost,
          's'=1/60,
          'm'=1,
          'h'=60,
          'd'=24
          )
        refTime[hTimeUnit]<-refTime[hTimeUnit]/div
      }
      refTime$cat=i
      # extract network to compute distance
      execGRASS('r.drain',
        input='tmp__cost',
        direction='tmp__ref_dir',
        output='tmp__drain',
        drain='tmp__drain',
        flags=c('overwrite','c','d'),
        start_points='tmp_ref_to'
        )
      # create new layer with start point as node
      execGRASS('v.net',
        input='tmp__drain',
        points='tmp__ref_from',
        output='tmp__net_from',
        node_layer='2',
        operation='connect',
        threshold=resol-1,
        flags='overwrite'
        )
      # create new layer with stop points as node
      execGRASS('v.net',
        input='tmp__net_from',
        points='tmp_ref_to',
        output='tmp__net_all',
        node_layer='3',
        operation='connect',
        threshold=resol-1,
        flags='overwrite'
        )
      # extrad distance for each end node.
      execGRASS('v.net.distance',
        input='tmp__net_all',
        output='tmp__net_dist',
        from_layer='3', # calc distance from all node in 3 to layer 2 (start point)     
        to_layer='2',
        intern=T,
        flags='overwrite'
        )
      # read attribute table of distance network.

      refDist<-dbReadTable(dbCon,'tmp__net_dist')
      # rename grass output
      names(refDist)<-c('tcat','cat',hDistUnit)
      # distance conversion
      if(!unitDist=='m'){
        div<-switch(unitDist,
          'km'=1000
          )
        refDist[hDistUnit]<-refDist[hDistUnit]/div
      }

      # using data.table. TODO: convert previouse data.frame to data.table.
      refTime<-as.data.table(refTime)
      setkey(refTime,cat,tcat)
      refDist<-as.data.table(refDist)
      setkey(refDist,cat,tcat)
      refTimeDist <- refDist[refTime]

      #create or update table
      if(incN==1){
        ref=refTimeDist
      }else{
        ref<-rbind(ref,refTimeDist)
      }
      # remove tmp map
      rmRastIfExists('tmp__*')
      rmVectIfExists('tmp__*')
    })
    amUpdateProgressBar(session,'cumulative-progress',inc*incN)
    print(timeCheck)
  }

# set key to ref
  setkey(ref,cat,tcat)

  # Remove tmp map
  rmVectIfExists('tmp_*')

  # mergin from hf subset table and renaming.
  valFrom<-inputTableHf[inputTableHf$cat %in% ref$cat, c('cat',idField,labelField)]
  names(valFrom)<-c('cat',hIdField,hLabelField)
  valFrom<-as.data.table(valFrom)
  setkey(valFrom,cat)

  valTo<-inputTableHfTo[inputTableHfTo$cat %in% ref$tcat,c('cat',idFieldTo,labelFieldTo)]
  names(valTo)<-c('tcat',hIdFieldTo,hLabelFieldTo)
  valTo<-as.data.table(valTo)
  setkey(valTo,'tcat')

  setkey(ref,cat)
  ref<-ref[valFrom]
  setkey(ref,tcat)
  ref<-ref[valTo]
  # set column subset and order
  #refOut<-ref[,c(hIdField,hIdFieldTo,hDistUnit,hTimeUnit,hLabelField,hLabelFieldTo),with=F]
  refOut<-ref[,c(
    hIdField,
    hLabelField,
    hIdFieldTo,
    hLabelFieldTo,
    hDistUnit,
    hTimeUnit
    ),with=F]

  # set expression to evaluate nested query by group
  expD<-parse(text=paste0(".SD[which.min(",hDistUnit,")]"))
  expT<-parse(text=paste0(".SD[which.min(",hTimeUnit,")]"))

  # Extract nearest feature by time and distance.
  refNearestDist<-refOut[,eval(expD),by=hIdField]
  refNearestTime<-refOut[,eval(expT),by=hIdField]

  })
 # Return meta data
  meta<-list(
    'Function'='amReferralTable',
    'AccessMod revision'=amGetVersionLocal(),
    'Date'=amSysTime(),
    'Timing'=as.list(timeCheckAll)$elapsed,
    'Iterations'=nrow(inputTableHf),
    'Arguments'=list(
      'input'=list(
        'map'=list(
          'cost'=list(
            'speed'=inputSpeed,
            'friction'=inputFriction
            ),
          'facilities'=list(
            'from'=inputHf,
            'to'=inputHfTo
            )
          ),
        'table'=list(
          'cat'=list(
            'from'=inputTableHf$cat,
            'to'=inputTableHfTo$cat
            ),
          'names'=list(
            'from'=names(inputTableHf),
            'to'=names(inputTableHfTo)
            )
          )
        ),
      'analysis'=typeAnalysis,
      'unit'=list(
        'distance'=unitDist,
        'cost'=unitCost
        ),
      'resol'=resol
      ),
    'Output'=list(
      outReferral,
      outNearestDist,
      outNearestTime
      ) 
    )

  dbWriteTable(dbCon,outReferral,refOut,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestDist,refNearestDist,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestTime,refNearestTime,overwrite=T,row.names=F)

 
}
