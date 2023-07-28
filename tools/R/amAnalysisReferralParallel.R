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


#' amReferralTable
#' @export
amAnalysisReferral <- function(
  inputHfFrom,
  inputHfTo,
  inputMerged,
  outputSpeed,
  outputFriction,
  outputReferral,
  outputNearestDist,
  outputNearestTime,
  outputNetDist,
  tableScenario,
  tableFacilities,
  tableFacilitiesTo,
  idField,
  idFieldTo,
  labelField,
  labelFieldTo,
  typeAnalysis,
  resol,
  maxTravelTime,
  knightMove = FALSE,
  useMaxSpeedMask = FALSE,
  limitClosest = FALSE,
  permuteGroups = FALSE,
  keepNetDist = TRUE,
  snapToGrid = FALSE,
  unitCost = c("s", "m", "h"),
  unitDist = c("m", "km"),
  roundingMethod = c("ceil", "round", "floor"),
  pBarTitle = "Referral analysis",
  parallel = NULL
) {
  amGrassSessionStopIfInvalid()

  roundingMethod <- match.arg(roundingMethod)
  mapset <- amGrassSessionGetLocation()
  location <- amGrassSessionGetMapset()

  amTimer("start")

  #
  # IF empty, use default parllel settings
  #
  if (isEmpty(parallel)) {
    parallel <- config$useParallel
  }


  tStart <- as.numeric(Sys.time()) # amTimer not available in loop

  #
  # Extract speed / friction map and max speed
  #
  switch(typeAnalysis,
    "anisotropic" = {
      amCreateSpeedMap(
        tableScenario,
        inputMerged,
        outputSpeed
      )
    },
    "isotropic" = {
      amCreateFrictionMap(
        tableScenario,
        inputMerged,
        outputFriction,
        mapResol = gmeta()$nsres
      )
    }
  )
  maxSpeed <- ifelse(isTRUE(useMaxSpeedMask), max(tableScenario$speed), 0)

  #
  # Create temp directory for networks
  #
  keepNetDistPath <- file.path(tempdir(), amRandomName())
  mkdirs(keepNetDistPath)

  #
  # Generic clear
  #
  on_exit_add({
    unlink(keepNetDistPath, recursive = TRUE)
    rmVectIfExists("tmp_*")
    amMapsetRemoveAll(pattern = "^tmp_")
    if ("clusterConf" %in% ls()) {
      if ("cluster" %in% class(clusterConf$cluster)) {
        stopCluster(clusterConf$cluster)
      }
    }
  })

  #
  # Subset selection
  #
  tableFacilities <- tableFacilities[
    tableFacilities$amSelect,
  ]
  tableFacilitiesTo <- tableFacilitiesTo[
    tableFacilitiesTo$amSelect,
  ]

  #
  # Set distance and time labels
  #
  hDistUnit <- sprintf("distance_%s", unitDist)
  hTimeUnit <- sprintf("time_%s", unitCost)

  #
  # If permuteGroups
  # - Remove limitclosest
  # - Swap id/labels
  # - Swap tables
  # - The time dist module will work in reverse
  if (isTRUE(permuteGroups)) {
    limitclosest <- FALSE
    swap("idField", "idFieldTo")
    swap("labelField", "labelFieldTo")
    swap("tableFacilities", "tableFacilitiesTo")
    swap("inputHfFrom", "inputHfTo")
  }
  #
  # Table labels
  # - Used at the end
  #
  dirTo <- ifelse(permuteGroups, "from", "to")
  dirFrom <- ifelse(permuteGroups, "to", "from")

  hIdField <- sprintf("%s__%s", dirFrom, amSubPunct(idField))
  hLabelField <- sprintf("%s__%s", dirFrom, amSubPunct(labelField))
  hIdFieldTo <- sprintf("%s__%s", dirTo, amSubPunct(idFieldTo))
  hLabelFieldTo <- sprintf("%s__%s", dirTo, amSubPunct(labelFieldTo))

  #
  # Extract ids
  # - Using "cat" as id
  # - Merging with facilities table later to match selected label
  #
  idListFrom <- tableFacilities[tableFacilities$amSelect, "cat"]
  idListTo <- tableFacilitiesTo[tableFacilitiesTo$amSelect, "cat"]

  #
  # Send progress state. Here, first message
  #
  pbc(
    visible = TRUE,
    timeOut = 10,
    percent = 1,
    title = pBarTitle,
    text = sprintf(
      ams("analysis_referral_parallel_progress_state"),
      length(idListFrom),
      length(idListTo)
    )
  )
  #
  # Add suffix with original mapset if needed
  #
  addMapset <- function(name) {
    hasSuffix <- isTRUE(grepl("@", name))
    if (!hasSuffix) {
      return(sprintf("%s@%s", name, mapset))
    }
    return(name)
  }
  inputHfFrom <- addMapset(inputHfFrom)
  inputHfTo <- addMapset(inputHfTo)
  outputSpeed <- addMapset(outputSpeed)
  outputFriction <- addMapset(outputFriction)

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
  jobs <- lapply(idListFrom, function(id) {
    if (length(idListTo) == 0) {
      stop(ams("analysis_referral_parallel_lack_destination"))
    }

    list(
      location        = location,
      mapset          = mapset,
      inputHfFrom     = inputHfFrom,
      inputHfTo       = inputHfTo,
      idFrom          = id,
      idListTo        = idListTo,
      permuted        = permuteGroups,
      inputSpeed      = outputSpeed,
      inputFriction   = outputFriction,
      typeAnalysis    = typeAnalysis,
      knightMove      = knightMove,
      maxTravelTime   = maxTravelTime,
      maxSpeed        = maxSpeed,
      unitCost        = unitCost,
      unitDist        = unitDist,
      limitClosest    = limitClosest,
      resol           = resol,
      keepNetDist     = keepNetDist,
      keepNetDistPath = keepNetDistPath,
      snapToGrid      = snapToGrid
    )
  })

  #
  # Split job to provide progression bar, opt out and memory allocation tunning
  #
  jobsGroups <- amSplitInGroups(jobs, clusterConf$nCores)


  if (isTRUE(clusterConf$parallel)) {
    amTimeStamp(sprintf(
      ams("analysis_referral_parallel_main_cores"),
      clusterConf$nCores
    ))
  } else {
    amTimeStamp(ams("analysis_referral_parallel_no_cluster"))
  }

  #
  # Main  loop
  #
  tryCatch(
    {
      idGrp <- 1
      resDistTimeAll <- lapply(jobsGroups, function(jobsGroup) {
        #
        # Eval ressource between each groups to re-calibrate
        #
        nCores <- clusterConf$nCores
        free <- sysEvalFreeMbMem()
        disk <- sysEvalFreeMbDisk()
        memoryPerWorker <- 0.5 * (free / nCores)
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

        if (clusterConf$parallel) {
          out <- parLapply(
            cl     = clusterConf$cluster,
            X      = jobsGroup,
            fun    = amTimeDist,
            memory = memoryPerWorker
          )
        } else {
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
    error = function(e) {
      stop(e)
    },
    finally = {
      amDebugMsg("End of jobs")
    }
  )

  #
  # Convert result list to table
  #
  tblOut <- data.frame()

  #
  # Convert result list to table
  #
  for (resGroup in resDistTimeAll) {
    for (res in resGroup) {
      if (isEmpty(tblOut)) {
        tblOut <- res
      } else {
        tblOut <- rbind(tblOut, res)
      }
    }
  }
  resDistTimeAll <- tblOut

  tTotal <- amTimer()$diff

  pbc(
    visible = TRUE,
    percent = 99,
    timeOut = 5,
    title = pBarTitle,
    text = sprintf(
      ams("analysis_referral_parallel_timing_tables"),
      tTotal
    )
  )

  #
  # set colname for dist and time
  # ------------------------------------
  #  cat_from cat_to distance_km time_m
  #         1      7      72.464 649.11
  #         7      1      72.992 703.79
  #
  # Final form will be, when permuted
  # ----------------------------------------------------------
  #  from__cat from__name to__cat to__name distance_km time_m
  #         1       ChHC       7     ChRH      72.992 703.79
  #         7       ChRH       1     ChHC      72.464 649.11
  #
  colnames(tblOut) <- c("cat_from", "cat_to", hDistUnit, hTimeUnit)

  #
  # Merge input hf table name
  #
  catIdField <- idField == "cat"
  catIdFieldTo <- idFieldTo == "cat"

  #
  # Merge label from hf 'from'
  #
  if (catIdField) {
    # ex. "cat","name"
    colsFrom <- c("cat", labelField)
  } else {
    # ex. "cat","uid","name"
    colsFrom <- c("cat", idField, labelField)
  }

  tblOut <- merge(
    x = tableFacilities[, colsFrom],
    y = tblOut,
    by.x = "cat",
    by.y = "cat_from"
  )

  tblOut$cat_from <- NULL
  if (!catIdField) {
    tblOut$cat <- NULL
  }


  colnames(tblOut)[1] <- hIdField
  colnames(tblOut)[2] <- hLabelField



  #
  # Merge label from hf 'to'
  #
  if (catIdFieldTo) {
    # ex. "cat","name"
    colsTo <- c("cat", labelFieldTo)
  } else {
    # ex. "cat","uid","name"
    colsTo <- c("cat", idFieldTo, labelFieldTo)
  }

  tblOut <- merge(
    x = tableFacilitiesTo[, colsTo],
    y = tblOut,
    by.x = "cat",
    by.y = "cat_to"
  )
  tblOut$cat_to <- NULL
  if (!catIdFieldTo) {
    tblOut$cat <- NULL
  }

  colnames(tblOut)[1] <- hIdFieldTo
  colnames(tblOut)[2] <- hLabelFieldTo

  #
  # If permuted, swap
  #
  if (permuteGroups) {
    swap("hIdField", "hIdFieldTo")
    swap("hLabelField", "hLabelFieldTo")
  }


  #
  # Compute min by dist and min by time
  #
  minTimeByFrom <- as.formula(paste(hTimeUnit, "~", hIdField))
  minDistByFrom <- as.formula(paste(hDistUnit, "~", hIdField))

  #
  # Check if no time or distance have been computed
  # (Aggregate does no like if all value are NA)
  #
  noRefTime <- all(is.na(tblOut[, hTimeUnit]))
  noRefDist <- all(is.na(tblOut[, hDistUnit]))

  if (noRefTime) {
    tblMinTime <- tblOut[0, ]
  } else {
    tblMinTime <- merge(
      aggregate(
        minTimeByFrom,
        data = tblOut,
        min,
        drop = TRUE
      ),
      tblOut
    )
  }
  if (noRefDist) {
    tblMinDist <- tblOut[0, ]
  } else {
    tblMinDist <- merge(
      aggregate(
        minDistByFrom,
        data = tblOut,
        min,
        drop = TRUE
      ),
      tblOut
    )
  }
  #
  # Column reorder
  # Order requested in #51
  #
  colsOrder <- c(
    hIdField,
    hLabelField,
    hIdFieldTo,
    hLabelFieldTo,
    hDistUnit,
    hTimeUnit
  )

  tblOut <- amSortByCol(tblOut, c(hIdField, hIdFieldTo))[, colsOrder]
  tblMinDist <- amSortByCol(tblMinDist, c(hIdField, hIdFieldTo))[, colsOrder]
  tblMinTime <- amSortByCol(tblMinTime, c(hIdField, hIdFieldTo))[, colsOrder]

  #
  # Local db connection
  #
  dbCon <- amMapsetGetDbCon()
  on_exit_add({
    dbDisconnect(dbCon)
  })

  #
  # Write tables
  #
  dbWriteTable(
    dbCon,
    outputReferral,
    tblOut,
    overwrite = TRUE,
    row.names = FALSE
  )
  dbWriteTable(
    dbCon,
    outputNearestTime,
    tblMinTime,
    overwrite = TRUE,
    row.names = FALSE
  )
  if (!limitClosest) {
    if (isEmpty(outputNearestDist)) {
      warning("No table name for outputNearestDist")
    } else {
      dbWriteTable(
        dbCon,
        outputNearestDist,
        tblMinDist,
        overwrite = TRUE,
        row.names = FALSE
      )
    }
  }

  #
  # If keep network, merged all net gpkg
  #
  if (keepNetDist) {
    spDfNet <- NULL
    netFileList <- list.files(
      path = keepNetDistPath,
      pattern = "tmp__net_dist*",
      full.names = TRUE
    )
    nNet <- length(netFileList)

    if (nNet > 0) {
      netLayerName <- "am_dist_net"
      netFileMerged <- sprintf("%1$s/tmp__net_dist_merged.gpkg", keepNetDistPath)

      for (i in 1:nNet) {
        pbc(
          visible = TRUE,
          percent = 99,
          timeOut = 1,
          title = pBarTitle,
          text = sprintf(
            ams("analysis_referral_parallel_out_net"),
            i,
            nNet
          )
        )
        netFile <- netFileList[[i]]

        if (i == 1) {
          amOgrConvert(
            fileIn = netFile,
            fileOut = netFileMerged,
            layerName = netLayerName,
            format = "GPKG",
            overwrite = TRUE,
          )
        } else {
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

      if (isEmpty(outputNetDist)) {
        warning("No table name for outputNetDist")
      } else {
        pbc(
          visible = TRUE,
          percent = 99,
          timeOut = 5,
          title   = pBarTitle,
          text    = ams("analysis_referral_parallel_out_net_write")
        )
        # overwrite make a lot of noise, remove layer now if exists
        rmVectIfExists(outputNetDist)

        execGRASS("v.in.ogr",
          flags = c(
            "o",
            "overwrite",
            "w",
            "2"
          ), # no proj check, overwrite, lowercase, 2d only,
          parameters = list(
            layer = netLayerName,
            input = netFileMerged,
            output = outputNetDist,
            snap = 0.0001
          )
        )

      }
    }
  }

  pbc(
    percent = 100,
    visible = FALSE
  )

  amTimeStamp("AM5 REFERRAL FINISHED YEAAAAH")

  return(list(
    minDist = tblMinDist,
    minTime = tblMinTime,
    all = tblOut,
    limitClosest = limitClosest
  ))
}



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
amSplitInGroups <- function(li, groupBy) {
  nLi <- length(li)
  tbl <- data.frame(
    id = 1:nLi,
    group = ceiling(1:nLi / groupBy)
  )

  groups <- list()

  for (i in 1:max(tbl$group)) {
    ids <- tbl[tbl$group == i, ]$id
    groups <- c(groups, list(li[ids]))
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
amClusterConfiguration <- function(parallel = TRUE,
  nJobs = data.frame(),
  startPoints = NULL) {
  out <- list(
    nCores = 1,
    cluster = NULL,
    parallel = FALSE
  )

  nCoresMax <- detectCores() - 1
  est <- amGetRessourceEstimate(startPoints)
  mA <- est$available$memory * 0.8
  mR <- est$required$memory
  nCoresMem <- floor(mA / mR)
  out$memoryPerCore <- mA

  if (!parallel || nJobs == 1 || nCoresMax < 2 || nCoresMem < 2) {
    return(out)
  }

  if (nCoresMem >= nCoresMax) {
    nCores <- nCoresMax
  } else {
    nCores <- nCoresMem
  }

  out$cluster <- makeCluster(nCores, outfile = "")
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
progressBeforeGroup <- function(i = 1,
  n = 1,
  pBarTitle = "progress",
  tStart = as.numeric(Sys.time()),
  nCores = 0,
  free = 0,
  memWorker = 0,
  disk = 0) {
  if (i == 1) {
    tEndEstimate <- ""
  } else {
    tNow <- as.numeric(Sys.time())
    tDiff <- (tNow - tStart) / 60
    done <- i - 1
    left <- n - done
    tEndEstimate <- ceiling((tDiff / done) * left)
    tEndEstimate <- sprintf(ams("analysis_referral_parallel_time_remaining"), tEndEstimate)
  }

  txt <- sprintf(
    fmt = ams("analysis_referral_parallel_groups_cores"),
    i,
    n,
    nCores,
    round(memWorker),
    round(free),
    round(disk),
    tEndEstimate
  )


  pbc(
    visible = TRUE,
    percent = ((i - 1) / n) * 100 + 1,
    timeOut = 1,
    title   = pBarTitle,
    text    = txt
  )
}
