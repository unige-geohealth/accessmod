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



#' Split in groups 
#' to ease opt-out and message priting during parallel loop 
#'
#' e.g. 4 cores, 10 jobs 
#' progress + opt-out  
#' [x,x,x,x]
#' progress + time +opt-out
#' [x,x,x,x]
#' progress + time +opt-out
#' [x,x]
#' end
#'
#' @param li {List} Jobs list 
#' @param groupBy {Number} Number of groups 
#' @return {List} Groupped list 
#' @export
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


#' Get parallel config 
#' 
#' @param parallel {Logical} Enable parallel mode 
#' @param nJobs {Numeric} Number of jobs
#' @param startPoints {Character} Layer name for starting point
#' @return {List}
#' @export
amClusterConfiguration <- function(
  parallel = TRUE,
  nJobs = data.frame(),
  startPoints = NULL
  ){
  out <- list(
    nCores = 1,
    cluster = NULL,
    parallel = FALSE
  )

  nCoresMax <- detectCores() - 1
  est <- amGetRessourceEstimate(startPoints)
  mA <- est$available$memory * 0.8
  mR <- est$required$memory
  nCoresMem <- floor( mA / mR )
  out$memoryPerCore <- mA

  if( !parallel || nJobs == 1 || nCoresMax < 2 || nCoresMem < 2 ){
    return(out)
  }

  if( nCoresMem >= nCoresMax ){
    nCores <- nCoresMax
  }else{
    nCores <- nCoresMem
  }

  out$cluster <- makeCluster(nCores,outfile = "") 
  out$nCores <- nCores
  out$parallel <- TRUE
  return(out)

}

#' Helper for showing proegress with ressource report
#'
#' @param i {Integer} Current group id
#' @param n {Integer} Total group number
#' @param pBarTitle {Character} Base progress message
#' @param tStart {Numeric} Start time (numeric)
#' @param nCores {Integer} Number of cores
#' @param free {Numeric} Memory available
#' @param memWorker {Numeric} Memory per worker
#' @param disk {Numeric} Current disk space left
progressBeforeGroup <- function(
  i = 1,
  n = 1,
  pBarTitle = "progress",
  tStart = as.numeric(Sys.time()),
  nCores = 0,
  free = 0,
  memWorker = 0,
  disk = 0
  ){

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
    , round(memWorker)
    , round(free)
    , round(disk)
    , tEndEstimate
  )

  pbc(
    visible = TRUE,
    percent = ((i/n)*100),
    timeOut = 1,
    title   = pBarTitle,
    text    = txt 
  )
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
  outReferral,
  outNearestDist,
  outNearestTime,
  outNetDist,
  maxCost,
  maxSpeed = 0,
  limitClosest = FALSE,
  permuteGroups = FALSE,
  keepNetDist = TRUE,
  snapToGrid = FALSE,
  unitCost = c('s','m','h'),
  unitDist = c('m','km'),
  pBarTitle = "Referral analysis",
  origMapset = NULL,
  origProject = NULL,
  language = config$language,
  parallel = NULL
  ){

  if(amNoDataCheck(parallel)){
    parallel = config$useParallel
  }

  amAnalysisSave('amAnalysisReferral')
  amTimer("start")


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
  # Local db connection
  #
  dbCon <- amMapsetGetDbCon()

  tStart <- as.numeric(Sys.time()) # amTimer not available in loop


  #
  # Create temp directory for networks
  #
  tmpDirNet <- file.path(tempdir(),amRandomName())
  mkdirs(tmpDirNet)
  #
  # Clear
  #
  on.exit({
    dbDisconnect(dbCon)
    unlink(tmpDirNet,recursive=TRUE)
    amMapsetRemoveAll(pattern="^tmp_")
    if("clusterConf" %in% ls()){
      if("cluster" %in% class(clusterConf$cluster)){
        stopCluster(clusterConf$cluster)
      }
    }
  })

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
  # Set net distance files (sp in rds) path
  #
  if(keepNetDist){
    keepNetDistPath <- tmpDirNet
  }else{
    keepNetDistPath <- NULL
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
      ams("analysis_referral_parallel_progress_state"),
      length(idListFrom),
      length(idListTo)
    )
  )
  #
  # Add suffix with original mapset if needed
  #
  addMapset <- function(name){
    hasSuffix <- grepl('@',name)
    if(!hasSuffix){
      return(sprintf('%s@%s',name,origMapset))
    }
    return(name)
  }
  inputHfFrom <- addMapset(inputHfFrom)
  inputHfTo <- addMapset(inputHfTo)
  inputSpeed <- addMapset(inputSpeed)
  inputFriction <- addMapset(inputFriction)

  #
  # Get parallel configuration
  #
  clusterConf <- amClusterConfiguration(
    nJobs = length(idListFrom),
    startPoints = inputHfFrom,
    parallel = parallel
  )


  #
  # Define jobs list
  #
  jobs <- lapply(idListFrom,function(id){

    if(length(idListTo)==0){
      stop(ams("analysis_referral_parallel_lack_destination"))
    }

    list(
      inputHfFrom     = inputHfFrom,
      inputHfTo       = inputHfTo,
      idFrom          = id,
      idListTo        = idListTo,
      permuted        = permuteGroups,
      inputSpeed      = inputSpeed,
      inputFriction   = inputFriction,
      typeAnalysis    = typeAnalysis,
      maxCost         = maxCost,
      maxSpeed        = maxSpeed,
      unitCost        = unitCost,
      unitDist        = unitDist,
      limitClosest    = limitClosest,
      resol           = resol,
      origProject     = origProject,
      keepNetDist     = keepNetDist,
      keepNetDistPath = keepNetDistPath,
      snapToGrid      = snapToGrid
    )
  })



  #
  # Split job to provide progression bar, opt out and memory allocation tunning
  # 
  jobsGroups <- amSplitInGroups(jobs,clusterConf$nCores)


  if(isTRUE(clusterConf$parallel)){
    amTimeStamp(sprintf(
        ams("analysis_referral_parallel_main_cores"),
        clusterConf$nCores
        ))
  }else{
    amTimeStamp(ams("analysis_referral_parallel_no_cluster"))
  }
 
  #
  # Show estimated remaining time
  #


  #
  # Main  loop
  #
  tryCatch({
    idGrp <- 1;
    resDistTimeAll <- lapply(jobsGroups,function(jobsGroup){
      #
      # Eval ressource between each groups to re-calibrate
      #
      nCores <- clusterConf$nCores
      free <- sysEvalFreeMbMem()
      disk <- sysEvalFreeMbDisk()
      memoryPerWorker <- free / nCores * 0.8
      #
      # Show porgress and repport ressources
      #
      progressBeforeGroup(
        i         = idGrp,
        n         = length(jobsGroups),
        pBarTitle = pBarTitle,
        tStart    = tStart,
        nCores    = nCores,
        free      = free,
        memWorker = memoryPerWorker,
        disk      = disk
      )

      if(clusterConf$parallel){
        out <- parLapply(
          cl     = clusterConf$cluster,
          X      = jobsGroup,
          fun    = amTimeDist,
          memory = memoryPerWorker
        )
      }else{
        out <- lapply(
          X      = jobsGroup,
          FUN    = amTimeDist,
          memory = memoryPerWorker
        )
      }
      idGrp <<- idGrp + 1  
      return(out) 
      })
  },
  error = function(e){
    stop(e)
  },
  finally = {
    #
    # Reset original mapset
    #
    amMapsetInit(origProject,origMapset)
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

  #
  # If keep network, merged all net gpkg 
  #
  if(keepNetDist){
    spDfNet <- NULL
    netFileList <- list.files(
      path = keepNetDistPath,
      pattern = 'tmp__net_dist*',
      full.names = TRUE
    )
    nNet <- length(netFileList)

    if(nNet > 0){

      netLayerName <- 'am_dist_net'
      netFileMerged <- sprintf('%1$s/tmp__net_dist_merged.gpkg',keepNetDistPath)

      for(i in 1:nNet){
        pbc(
          visible = TRUE,
          percent = 99,
          timeOut = 1,
          title   = pBarTitle,
          text    = sprintf(
            ams( "analysis_referral_parallel_out_net"),
            i,
            nNet
          )
        )
        netFile <- netFileList[[i]]

        if(i == 1 ){
          amOgrConvert(
            fileIn = netFile,
            fileOut = netFileMerged,
            layerName = netLayerName,
            format = "GPKG",
            overwrite = TRUE,
          )
        }else{
          amOgrConvert(
            fileIn = netFile,
            fileOut = netFileMerged,
            layerName = netLayerName,
            format = "GPKG",
            update = TRUE,
            append = TRUE
          )
        }
      }


      if(!amNoDataCheck(outNetDist)){
        pbc(
          visible = TRUE,
          percent = 99,
          timeOut = 5,
          title   = pBarTitle,
          text    = ams( "analysis_referral_parallel_out_net_write")
        )
        # overwrite make a lot of noise, remove layer now if exists
        rmVectIfExists(outNetDist)

        execGRASS("v.in.ogr",
          flags=c("o","overwrite","w","2"), # no proj check, overwrite, lowercase, 2d only,
          parameters = list(
            layer = netLayerName,
            input = netFileMerged,
            output = outNetDist,
            snap = 0.0001
          )
        )

      }
    }
  }

  tTotal = amTimer()$diff

  pbc(
    visible = TRUE,
    percent = 99,
    timeOut = 5,
    title   = pBarTitle,
    text    = sprintf(
      ams( "analysis_referral_parallel_timing_tables"),
      tTotal
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

  #
  # Check if no time or distance have been computed
  # (Aggregate does no like if all value are NA)
  #
  noRefTime <- all(is.na(tblOut[,hTimeUnit]))
  noRefDist <- all(is.na(tblOut[,hDistUnit]))

  if(noRefTime){
    tblMinTime <- tblOut[0,]
  }else{
    tblMinTime <- merge(
      aggregate(
        minTimeByFrom,
        data = tblOut,
        min,
        drop = T
        ),
      tblOut
    )
  }
  if(noRefDist){
    tblMinDist <- tblOut[0,]
  }else{
    tblMinDist <- merge(
      aggregate(
        minDistByFrom,
        data = tblOut,
        min,
        drop = T
        ),
      tblOut
    )
  }
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


