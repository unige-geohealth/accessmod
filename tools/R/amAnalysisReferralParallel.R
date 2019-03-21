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
  limitClosest,
  resol,
  dbCon,
   outReferral,
  outNearestDist,
  outNearestTime,
  maxCost,
  unitCost = c('s','m','h'),
  unitDist = c('m','km'),
  pBarTitle = "Referral analysis",
  origMapset = NULL,
  origProject = NULL,
  language = config$language
  ){

  amAnalysisSave('amAnalysisReferral')

  amTimer("start")

  # Calculate the number of cores
  nCores <- detectCores()
  # Initiate cluster
  cluster <- makeCluster(nCores,outfile = "")
  on.exit(stopCluster(cluster))
  #
  # set output table label
  #
  hIdField <- paste0('from','__',amSubPunct(idField)) # amSubPunt to avoid unwanted char (accent, ponctuation..)
  hLabelField <- paste0('from','__',amSubPunct(labelField))
  hIdFieldTo <- paste0('to','__',amSubPunct(idFieldTo))
  hLabelFieldTo <- paste0('to','__',amSubPunct(labelFieldTo))
  hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idFieldTo))
  hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelFieldTo))
  hDistUnit <-paste0('distance','_',unitDist)
  hTimeUnit <- paste0('time','_',unitCost)

  #
  # If start hf is the same as dest, don't compute ref to itsef.
  #
  identicalFromTo <- identical(inputHfFrom,inputHfTo)

  #
  # set local identifier columns
  #
  idCol <- config$vectorKey
  idColTo <- paste0(config$vectorKey,"_to")

  #
  # Get ids list for origin and destination
  #

  #listFrom <- inputTableHf[,idField]
  #listTo <- inputTableHfTo[,idFieldTo]
  listFrom <- inputTableHf[,idCol]
  listTo <- inputTableHfTo[,idCol]


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
        id = "analysis_referral_parallel_progress_state",
        str = "Compute referral from  %1$s facilities to %2$s facilities. Please be patient. Click on stop button to interrupt.",
        lang = language
        ),
      length(listFrom),
      length(listTo)
      )
    )

  if(is.null(origMapset)){
    origMapset <- amMapsetGet()
  }
  if(is.null(origMapset)) stop(
    ams(
      id = "analysis_referral_parallel_lack_mapset",
      str = "No mapset found",
      lang = language
      )
    )
  if(is.null(origProject)){
    origProject <- amProjectGet()
  }
  if(is.null(origProject)) stop(
    ams(
      id = "analysis_referral_parallel_lack_project",
      str = "No project found",
      lang = language
      )
    )

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

  jobs <- lapply(listFrom,function(id){

    #
    # Don't ccompute distance to self
    #
    if(identicalFromTo){
      listToSub <- listTo[!listTo == id]
    }else{
      listToSub <- listTo
    }

    if(length(listToSub)==0){
      stop(
        ams(
          id = "analysis_referral_parallel_lack_destination",
          str = "Unexpected issue: there is no destination.",
          lang = language
          )
        )
      }

    list(
      inputHfFrom = inputHfFrom,
      inputHfTo = inputHfTo,
      idFrom = id,
      idsTo = listToSub,
      inputSpeed = inputSpeed,
      inputFriction = inputFriction,
      typeAnalysis = typeAnalysis,
      maxCost = maxCost,
      idCol = idCol,
      idColTo = idColTo,
      hTimeUnit = hTimeUnit,
      hDistUnit = hDistUnit,
      unitCost = unitCost,
      unitDist = unitDist,
      limitClosest =limitClosest,
      resol = resol,
      origProject = origProject,
      nCores = nCores
      )
    })


  #
  # Split job to provide progression bar and opt-out if the 
  # user want to stop
  #
  jobsGroups <- amSplitInGroups(jobs,nCores)

  amTimeStamp(sprintf(
    ams(
      id = "analysis_referral_parallel_main_cores",
      str = "AM5 REFERRAL: START PSOCK CLUSTER ON %1$s CORES",
      lang = language
      ),
    nCores
    ))

  progressGroup <- function(i = 1){
    n <- length(jobsGroups)
    pbc(
      visible = TRUE,
      percent = ((i/n)*100)-1,
      timeOut = 1,
      title   = pBarTitle,
      text    = sprintf(
        ams(
          id = "analysis_referral_parallel_groups_cores",
          str = "Start parallel processing of group %1$s on %2$s using %3$s cores",
          lang = language
          ),
        i,
        n,
        nCores
        )
      )
    }

  #
  # Main parallel loop
  #
  tryCatch( 
    finally={
    amDebugMsg("Close cluster")
      },{
        idGrp <- 1;
        resDistTimeAll <- lapply(jobsGroups,function(jobsGroup){
          progressGroup(idGrp)
          out <- parLapply(cluster, jobsGroup, amTimeDist)
          idGrp <<- idGrp + 1  
          return(out) 
        })

      })
  amDebugMsg("test")
  #
  # Convert result list to table 
  #
  resDistTimeAllOut <- data.frame()
  
  for(resGroup in resDistTimeAll){
    for(res in resGroup){
      if(amNoDataCheck(resDistTimeAllOut)){
        resDistTimeAllOut <- res
      }else{
        resDistTimeAllOut <- rbind(resDistTimeAllOut,res)
      }
    }
  }
  resDistTimeAll <- resDistTimeAllOut 

  #
  # cleaning temp files
  #
  pbc(
    visible = TRUE,
    percent = 99,
    timeOut = 5,
    title   = pBarTitle,
    text    = sprintf(
      ams(
        id = "analysis_referral_parallel_timing_tables",
        str = "Referral analysis done in %s. Creation of output tables.",
        lang = language
        ),
      amTimer()
      )
    )
  #
  # Set final tables
  #

  tblFrom <- inputTableHf[,c(idCol,labelField)]
  tblTo <- inputTableHfTo[,c(idCol,labelFieldTo)]
  names(tblFrom) <- c(hIdField,hLabelField)
  names(tblTo) <- c(hIdFieldTo,hLabelFieldTo)
  tblOut <- merge(
    tblFrom,
    resDistTimeAll,
    by.x = hIdField,
    by.y = idCol,
    all = TRUE
    )
  tblOut <- merge(
    tblTo,
    tblOut,
    by.x = hIdFieldTo,
    by.y = idColTo,
    all = TRUE
    )

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
  # Column reorder (why..)
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


