#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/


amSplitInGroups <- function(li,groupBy){

  nLi <- length(li)
  tbl <-  data.frame(
    id = 1:nLi,
    group = ceiling(1:nLi/groupBy)
    )

  groups <- list()

  for(i in 1:max(tbl$group)){
    ids <- tbl[ tbl$group == i,]$id
    groups <- c(groups,list(li[ids]))
  }
  return(groups)

}

#'amReferralTable
#'@export
amAnalysisReferral <- function(
  session = shiny:::getDefaultReactiveDomain(),
  inputSpeed,
  inputFriction,
  inputHfFrom,
  inputHfTo,
  inputTableHf,
  inputTableHfTo,
  idField,
  idFieldTo,
  labelField,
  labelFieldTo,
  typeAnalysis,
  resol,
  dbCon,
  outReferral,
  outNearestDist,
  outNearestTime,
  maxCost,
  maxSpeed = 0,
  limitClosest = FALSE,
  permuteGroups = FALSE,
  unitCost = c('s','m','h'),
  unitDist = c('m','km'),
  pBarTitle = "Referral analysis",
  origMapset = NULL,
  origProject = NULL,
  language = config$language
  ){

  amAnalysisSave('amAnalysisReferral')
  amTimer("start")
  tStart <- as.numeric(Sys.time()) # amTimer not available in loop

  #
  # Calculate the number of cores
  #
  nCores <- detectCores()
  #
  # Initiate cluster
  #
  cluster <- makeCluster(nCores,outfile = "")
  on.exit(stopCluster(cluster))




  #
  # If inverseGroups, remove limitclosest
  # set labels
  #
  if(isTRUE(permuteGroups)){
    
    limitclosest <- FALSE

    hIdField <- paste0('from','__',amSubPunct(idFieldTo)) 
    hLabelField <- paste0('from','__',amSubPunct(labelFieldTo))
    hIdFieldTo <- paste0('to','__',amSubPunct(idField))
    hLabelFieldTo <- paste0('to','__',amSubPunct(labelField))
    hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idField))
    hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelField))

    #
    # Switch table
    #
    tmpTbl <- inputTableHfTo
    inputTableHfTo <- inputTableHf
    inputTableHf <- tmpTbl

  }else{
    hIdField <- paste0('from','__',amSubPunct(idField)) 
    hLabelField <- paste0('from','__',amSubPunct(labelField))
    hIdFieldTo <- paste0('to','__',amSubPunct(idFieldTo))
    hLabelFieldTo <- paste0('to','__',amSubPunct(labelFieldTo))
    hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idFieldTo))
    hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelFieldTo))
  }

  #
  # Extract ids
  #
  idListFrom <- inputTableHf[,'cat']
  idListTo <- inputTableHfTo[,'cat']

  #
  # Set distance and time labels
  #
  hDistUnit <-paste0('distance','_',unitDist)
  hTimeUnit <- paste0('time','_',unitCost)


  #
  # Send progress state. Here, first message
  #
  pbc(
    visible = TRUE,
    timeOut = 10,
    percent = 1,
    title   = pBarTitle,
    text    = sprintf(
      ams(
        id = "analysis_referral_parallel_progress_state"
        ),
      length(idListFrom),
      length(idListTo)
      )
    )

  if(is.null(origMapset)){
    origMapset <- amMapsetGet()
  }
  if(is.null(origMapset)){
    stop(ams("analysis_referral_parallel_lack_mapset"))
  }
  if(is.null(origProject)){
    origProject <- amProjectGet()
  }
  if(is.null(origProject)){
    stop(ams("analysis_referral_parallel_lack_project" ))
  }

  #
  # Add suffix with original mapset if needed
  #
  mSuffix <- paste0('@',origMapset)
  hasSuffix <- function(l){grepl(mSuffix,l)}
  addSuffix <- function(l){paste0(l,mSuffix)}

  if(!hasSuffix(inputHfFrom)) inputHfFrom <- addSuffix(inputHfFrom)
  if(!hasSuffix(inputHfTo)) inputHfTo <- addSuffix(inputHfTo)
  if(!hasSuffix(inputSpeed)) inputSpeed <- addSuffix(inputSpeed)
  if(!hasSuffix(inputFriction)) inputFriction <- addSuffix(inputFriction)

  jobs <- lapply(idListFrom,function(id){

    if(length(idListTo)==0){
      stop(ams("analysis_referral_parallel_lack_destination"))
    }

    list(
      inputHfFrom   = inputHfFrom,
      inputHfTo     = inputHfTo,
      idFrom        = id,
      idListTo      = idListTo,
      permuted      = permuteGroups,
      inputSpeed    = inputSpeed,
      inputFriction = inputFriction,
      typeAnalysis  = typeAnalysis,
      maxCost       = maxCost,
      maxSpeed      = maxSpeed,
      unitCost      = unitCost,
      unitDist      = unitDist,
      limitClosest  = limitClosest,
      resol         = resol,
      origProject   = origProject,
      nCores        = nCores
      )
    })

  #
  # Split job to provide progression bar and opt-out if the 
  # user want to stop
  #
  jobsGroups <- amSplitInGroups(jobs,nCores)


  amTimeStamp(sprintf(
      ams(
        id = "analysis_referral_parallel_main_cores"
        ),
      nCores
      ))

  progressBeforeGroup <- function(i = 1, meanTime){
    n <- length(jobsGroups)
    if(i == 1){
      tEndEstimate = "";
    }else{
      tNow <- as.numeric(Sys.time())
      tDiff <- ( tNow - tStart ) / 60
      done <- i - 1
      left <- n - done
      tEndEstimate <- ceiling((tDiff / done) * left)
      tEndEstimate <- sprintf(ams("analysis_referral_parallel_time_remaining"),tEndEstimate)
    }

    txt <- sprintf(
      fmt = ams("analysis_referral_parallel_groups_cores")
      , i
      , n
      , nCores
      , tEndEstimate
      )

    pbc(
      visible = TRUE,
      percent = ((i/n)*100)-1,
      timeOut = 1,
      title   = pBarTitle,
      text    = txt 
      )
  }

  #
  # Main parallel loop
  #
  tryCatch({
    modeDebug <- FALSE
    idGrp <- 1;
    resDistTimeAll <- lapply(jobsGroups,function(jobsGroup){
      progressBeforeGroup(idGrp)
      if(modeDebug){
        out <- amTimeDist(jobsGroup[[idGrp]])
      }else{
        out <- parLapply(cluster, jobsGroup, amTimeDist)
      }
      idGrp <<- idGrp + 1  
      return(out) 
      })
  })

  #
  # Convert result list to table 
  #
  tblOut <- data.frame()

  #
  # Convert result list to table 
  #
  for(resGroup in resDistTimeAll){ 
    for(res in resGroup){
      if(amNoDataCheck(tblOut)){
        tblOut <- res
      }else{
        tblOut <- rbind(tblOut,res)
      }
    }
  }
  resDistTimeAll <- tblOut 

  pbc(
    visible = TRUE,
    percent = 99,
    timeOut = 5,
    title   = pBarTitle,
    text    = sprintf(
      ams(
        id = "analysis_referral_parallel_timing_tables"
        ),
      amTimer()
      )
    )

  #
  # set colname for dist and time
  #
  colnames(tblOut)[3] <- hDistUnit
  colnames(tblOut)[4] <- hTimeUnit

  #
  # Merge input hf table name
  #
  catIdField <- idField == 'cat'
  catIdFieldTo <- idFieldTo == 'cat'

  #
  # Merge label from hf 'from'
  #
  if(catIdField){
    colsFrom <- c('cat',labelField)
  }else{
    colsFrom <- c('cat',idField,labelField)
  }

  tblOut <- merge(
    x = inputTableHf[,colsFrom],
    y = tblOut,
    by.x = 'cat',
    by.y = 'cat'
    )

  if(!catIdField){
    tblOut$cat <- NULL
  }

  colnames(tblOut)[1] <- ifelse(permuteGroups, hIdFieldTo, hIdField )
  colnames(tblOut)[2] <- ifelse(permuteGroups, hLabelFieldTo, hLabelField)

  #
  # Merge label from hf 'to'
  #
  if(catIdFieldTo){
    colsTo <- c('cat',labelFieldTo)
  }else{
    colsTo <- c('cat',idFieldTo,labelFieldTo)
  }

  tblOut <- merge(
    x = inputTableHfTo[,colsTo],
    y = tblOut,
    by.x = 'cat',
    by.y = 'cat_to'
    )
  tblOut$cat_to <- NULL
  if(!catIdFieldTo){
    tblOut$cat <- NULL
  }

  colnames(tblOut)[1] <- ifelse(permuteGroups, hIdField, hIdFieldTo )
  colnames(tblOut)[2] <- ifelse(permuteGroups, hLabelField, hLabelFieldTo )

  #
  # Compute min by dist and min by time
  #
  minTimeByFrom <- as.formula(paste(hTimeUnit,"~",hIdField))
  minDistByFrom <- as.formula(paste(hDistUnit,"~",hIdField))
 


  tblMinTime <- merge(
    aggregate(
      minTimeByFrom,
      data = tblOut,
      min,
      drop = T
      ),
    tblOut
    )
  tblMinDist <- merge(
    aggregate(
      minDistByFrom,
      data = tblOut,
      min,
      drop = T
      ),
    tblOut
    )

  #
  # Column reorder
  #
  colsOrder <- c(
    hIdField,
    hLabelField,
    hIdFieldTo,
    hLabelFieldTo,
    hDistUnit,
    hTimeUnit
    )
  tblOut <- tblOut[order(tblOut[,hIdField]),colsOrder]
  tblMinDist <- tblMinDist[order(tblMinDist[,hIdField]),colsOrder]
  tblMinTime <- tblMinTime[order(tblMinTime[,hIdField]),colsOrder]

  #
  # Write tables
  #
  amTimeStamp("AM5 REFERRAL FINISHED YEAAAAH")
  if(dbIsValid(dbCon)){
    dbWriteTable(
      dbCon,
      outReferral,
      tblOut,
      overwrite = T,
      row.names = F
      )
    dbWriteTable(
      dbCon,
      outNearestTime,
      tblMinTime,
      overwrite = T,
      row.names = F
      )
    if(!limitClosest) dbWriteTable(
      dbCon,
      outNearestDist,
      tblMinDist,
      overwrite = T,
      row.names = F
      )
  }
  pbc(
    percent = 100,
    visible = FALSE
    )


  return(list(
      minDist = tblMinDist,
      minTime = tblMinTime,
      all = tblOut,
      limitClosest = limitClosest
      ))

}


