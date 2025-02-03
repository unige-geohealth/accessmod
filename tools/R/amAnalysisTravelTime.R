#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

ratioRessources <- 0.5
int16Max <- (2^15) - 1
int32Max <- (2^31) - 1
# R limitation : int32Max seconds. E.g. as.integer(int32Max + 1) -> error
# int32Max = 68 years. ( int32Max / 60 / 60 / 24 / 365 )

amTravelTimeAnalysis <- function(
  inputMerged,
  inputHf,
  outputTravelTime,
  outputNearest,
  outputFriction,
  outputSpeed,
  tableScenario,
  tableFacilities,
  typeAnalysis,
  towardsFacilities,
  maxTravelTime,
  addNearest = FALSE,
  joinField = "cat",
  knightMove = FALSE,
  useMaxSpeedMask = FALSE,
  roundingMethod = c("ceil", "round", "floor"),
  timeoutValue
) {
  roundingMethod <- match.arg(roundingMethod)

  amGrassSessionStopIfInvalid()

  on_exit_add({
    # e.g. temproary inputHfFinal
    rmVectIfExists("tmp_*")
  })


  #
  # If needed, create new temp. layer with subset
  # else, return the original facilities layer
  #
  inputHfFinal <- amFacilitiesSubset(
    tableFacilities = tableFacilities,
    inputFacilities = inputHf
  )

  #
  # Unset output nearest if not wanted
  #
  if (!addNearest) {
    outputNearest <- NULL
    joinField <- NULL
  }

  #
  # Set maxSpeed
  #
  maxSpeed <- ifelse(isTRUE(useMaxSpeedMask), max(tableScenario$speed), 0)

  #
  # Build speed or friction map + compute travel time
  #
  switch(typeAnalysis,
    "anisotropic" = {
      amCreateSpeedMap(
        tableScenario,
        inputMerged,
        outputSpeed
      )

      amAnisotropicTravelTime(
        inputSpeed = outputSpeed,
        inputHf = inputHfFinal,
        outputTravelTime = outputTravelTime,
        outputNearest = outputNearest,
        towardsFacilities = towardsFacilities,
        knightMove = knightMove,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        timeoutValue = timeoutValue,
        roundingMethod = roundingMethod,
      )
    },
    "isotropic" = {
      amCreateFrictionMap(
        tableScenario,
        inputMerged,
        outputFriction,
        mapResol = gmeta()$nsres
      )

      amIsotropicTravelTime(
        inputFriction = outputFriction,
        inputHf = inputHfFinal,
        outputTravelTime = outputTravelTime,
        outputNearest = outputNearest,
        knightMove = knightMove,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        timeoutValue = timeoutValue,
        roundingMethod = roundingMethod,
      )
    }
  )

  # Check if 'outputNearest' variable is set and chosenField is not 'cat'
  if (amRastExists(outputNearest) && joinField != "cat") {
    amUpdateCatValue(
      vectorName = inputHfFinal,
      rasterName = outputNearest,
      joinField = joinField
    )
  }
}



#' Update raster 'cat' values based on a chosen vector field.
#'
#' This function recodes the values of a raster based on the values from a
#' chosen field of a vector layer. It's particularly useful when you want to
#' assign raster values based on attributes from a vector map.
#'
#' @param vectorName A character string specifying the name of the vector layer.
#' @param rasterName A character string specifying the name of the raster layer.
#' @param joinField A character string specifying the field name from the
#'   vector layer used for recoding. The default is 'cat', but since the
#'   function checks agains this default and raises an error if it's 'cat',
#'   you should provide a different field name.
#'
#' @return No return value. The function updates the raster in-place.
#'
#' @export
amUpdateCatValue <- function(vectorName, rasterName, joinField = "cat") {
  if (joinField == "cat") {
    stop("amUpdateCatValue : joinField can't be 'cat' ")
  }

  rasterNameCopy <- amRandomName("tmp__raster_cat")

  on_exit_add({
    rmRastIfExists(rasterNameCopy)
  })

  execGRASS("g.copy", raster = c(rasterName, rasterNameCopy))

  # Get unique values from chosen field
  recordsJson <- execGRASS("v.db.select",
    map = vectorName,
    columns = c("cat", joinField),
    format = "json",
    intern = TRUE
  )
  records <- fromJSON(recordsJson)
  values <- records$records

  # Create recode rules
  recode_rules <- paste0(
    values$cat, ":",
    values$cat, ":",
    values[[joinField]]
  )

  tmpFile <- tempfile()
  write(recode_rules, tmpFile)

  execGRASS("r.recode",
    input = rasterNameCopy,
    output = rasterName,
    rules = tmpFile,
    flags = "overwrite"
  )
}


#' amIsotropicTraveTime
#' @export
amIsotropicTravelTime <- function(
  inputFriction,
  inputHf,
  inputStop = NULL,
  inputCoord = NULL,
  outputDir = NULL,
  outputTravelTime = NULL,
  outputNearest = NULL,
  maxTravelTime = 0,
  maxSpeed = 0,
  minTravelTime = NULL,
  timeoutValue = -1L,
  getMemDiskRequirement = FALSE,
  roundingMethod = c("ceil", "round", "floor"),
  knightMove = FALSE,
  ratioMemory = 1,
  memory = NULL, # if set, absolute max memory
  rawMode = FALSE
) {
  roundingMethod <- match.arg(roundingMethod)

  vInfo <- amParseOptions(
    execGRASS(
      "v.info",
      flags = c("t"),
      map = inputHf,
      intern = TRUE
    )
  )
  vHasLines <- as.numeric(vInfo$lines) > 0
  tmpStart <- NULL
  if (vHasLines) {
    tmpStart <- amRandomName("tmp__raster_start")
    on_exit_add({
      rmRastIfExists(tmpStart)
    })
    suppressWarnings({
      execGRASS(
        "v.to.rast",
        input = inputHf,
        output = tmpStart,
        use = "val",
        value = 1
      )
    })
    inputRaster <- tmpStart
    inputHf <- NULL
  } else {
    inputRaster <- NULL
  }

  # default memory allocation
  free <- 300
  disk <- 10
  # dynamic memory allocation
  tryCatch(
    {
      free <- sysEvalFreeMbMem()
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  tryCatch(
    {
      disk <- as.integer(sysEvalFreeMbDisk())
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  if (isEmpty(memory)) {
    memory <- as.integer(free * ratioRessources * ratioMemory)
  }

  #
  # Maximum value  === int32Max
  #
  if (maxTravelTime * 60 > int32Max) {
    maxTravelTime <- int32Max / 60
  }

  amParam <- list(
    input = inputFriction,
    output = outputTravelTime,
    nearest = outputNearest,
    start_points = inputHf,
    start_raster = inputRaster,
    start_coordinates = inputCoord,
    stop_points = inputStop,
    outdir = outputDir,
    max_cost = as.integer(maxTravelTime * 60), # max cost in sec
    memory = as.integer(memory)
  )

  amParam <- amParam[!sapply(amParam, isEmpty)]

  diskRequire <- disk
  memRequire <- free

  tryCatch(
    {
      testSysLimit <- execGRASS("r.cost",
        parameters = amParam,
        flags = c("i", "overwrite"),
        intern = TRUE
      )
      # Sample output
      # [1] "Will need at least 1.02 MB of disk space"
      # [2] "Will need at least 1.50 MB of memory"
      # [3] "16 of 16 segments are kept in memory"
      diskRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("disk space", testSysLimit)
          ]
        )
      )
      memRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("of memory", testSysLimit)
          ]
        )
      )
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  if (!getMemDiskRequirement && diskRequire > disk * ratioRessources) {
    stop(
      sprintf(
        "Insufficient disk space. Required= %1$s MB, Available= %2$s MB",
        diskRequire,
        disk
      )
    )
  }
  if (!getMemDiskRequirement && memRequire > free * ratioRessources) {
    stop(
      sprintf(
        "Insufficient memory. Required= %1$s MB, Available= %2$s MB",
        memRequire,
        free
      )
    )
  }

  if (!getMemDiskRequirement) {
    amMsg(
      type = "log",
      text = sprintf(
        "Memory required for r.cost = %1$s MB. Memory available = %2$s MB. Disk space required = %3$s MB. Disk space available = %4$s MB",
        memRequire,
        free,
        diskRequire,
        disk
      )
    )
  }

  if (!getMemDiskRequirement) {
    if (maxSpeed > 0 && maxTravelTime > 0) {
      on_exit_add({
        amSpeedBufferRegionRestore()
      })
      amSpeedBufferRegionInit(
        c(inputHf, inputStop),
        maxSpeed / 3.6,
        maxTravelTime * 60
      )
    }

    #
    # Remove stops if not on current region
    #
    if (isNotEmpty(inputStop)) {
      tblStopTest <- amGetRasterValueAtPoint(
        inputStop,
        config$mapDem
      )
      hasNoStopInRegion <- isEmpty(tblStopTest)

      if (hasNoStopInRegion) {
        amParam$stop_points <- NULL
      }
    }

    flags <- flags <- c(
      "overwrite",
      ifelse(knightMove, "k", "")
    )
    flags <- flags[!flags %in% character(1)]

    res <- execGRASS("r.cost",
      parameters = amParam,
      flags = flags
    )
    if (res == 137) {
      stop("Process used too much ressources.")
    }

    if (!rawMode) {
      amCleanTravelTime(
        map = outputTravelTime,
        maxTravelTime = maxTravelTime,
        minTravelTime = minTravelTime,
        timeoutValue = timeoutValue,
        roundingMethod = roundingMethod,
        convertToMinutes = TRUE
      )
    }
  } else {
    return(
      list(
        required = list(
          memory = memRequire,
          disk = diskRequire
        ),
        available = list(
          memory = free,
          disk = disk
        )
      )
    )
  }
}

#' amAnisotropicTravelTime
#' @param maxTravelTime maximum cost in minute
#' @export
amAnisotropicTravelTime <- function(
  inputSpeed,
  inputHf,
  inputCoord = NULL,
  inputStop = NULL,
  outputDir = NULL,
  outputTravelTime = NULL,
  outputNearest = NULL,
  towardsFacilities = FALSE,
  maxTravelTime = 0,
  minTravelTime = NULL,
  maxSpeed = 0,
  knightMove = FALSE,
  timeoutValue = "null()",
  roundingMethod = c("ceil", "round", "floor"),
  getMemDiskRequirement = FALSE,
  ratioMemory = 1,
  memory = NULL, # if set, absolute max memory
  rawMode = FALSE # skip minute conversion; skip value removal above maxTravelTime
) {
  roundingMethod <- match.arg(roundingMethod)

  flags <- c(
    c("overwrite", "s"),
    ifelse(towardsFacilities, "t", ""),
    ifelse(knightMove, "k", "")
  )
  flags <- flags[!flags %in% character(1)]

  # default memory allocation
  free <- 300
  disk <- 10

  # dynamic memory allocation
  tryCatch(
    {
      free <- as.integer(sysEvalFreeMbMem())
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  tryCatch(
    {
      disk <- as.integer(sysEvalFreeMbDisk())
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  #
  # Convert vector line starting point to raster
  #
  vInfo <- amParseOptions(
    execGRASS(
      "v.info",
      flags = c("t"),
      map = inputHf,
      intern = TRUE
    )
  )

  vHasLines <- as.numeric(vInfo$lines) > 0

  tmpStart <- NULL

  if (vHasLines) {
    tmpStart <- amRandomName("tmp__raster_start")
    on_exit_add({
      rmRastIfExists(tmpStart)
    })
    suppressWarnings({
      execGRASS(
        "v.to.rast",
        input = inputHf,
        output = tmpStart,
        use = "val",
        value = 1
      )
    })
    inputRaster <- tmpStart
    inputHf <- NULL
  } else {
    inputRaster <- NULL
  }

  #
  # set
  #
  if (isEmpty(memory)) {
    memory <- as.integer(free * ratioRessources * ratioMemory)
  }

  #
  # Maximum value  === int32Max
  #
  if (maxTravelTime * 60 > int32Max) {
    warning(sprintf("Maximum travel time reached: use %s seconds as limit", int32Max))
    maxTravelTime <- int32Max / 60
  }

  amParam <- list(
    elevation = config$mapDem,
    friction = inputSpeed,
    output = outputTravelTime,
    nearest = outputNearest,
    start_points = inputHf,
    start_raster = inputRaster,
    start_coordinates = inputCoord,
    stop_points = inputStop,
    outdir = outputDir,
    memory = as.integer(memory),
    max_cost = as.integer(maxTravelTime * 60) # max cost in seconds.
  )

  amParam <- amParam[!sapply(amParam, isEmpty)]

  diskRequire <- 0
  memRequire <- 0

  tryCatch(
    {
      testSysLimit <- execGRASS("r.walk.accessmod",
        parameters = amParam,
        flags = c("i", flags),
        intern = TRUE
      )
      # Sample output
      # [1] "Will need at least 1.02 MB of disk space"
      # [2] "Will need at least 1.50 MB of memory"
      # [3] "16 of 16 segments are kept in memory"
      diskRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("disk space", testSysLimit)
          ]
        )
      )
      memRequire <- as.integer(
        gsub(
          "[a-zA-Z]",
          "",
          testSysLimit[
            grepl("of memory", testSysLimit)
          ]
        )
      )
    },
    error = function(cond) {
      warning(cond$message)
    }
  )

  if (!getMemDiskRequirement && diskRequire > disk * ratioRessources) {
    stop(
      sprintf(
        "Insufficient disk space. Required= %1$s MB, Available= %2$s MB",
        diskRequire,
        disk
      )
    )
  }
  if (!getMemDiskRequirement && memRequire > free * ratioRessources) {
    stop(
      sprintf(
        "Insufficient memory. Required= %1$s MB, Available= %2$s MB",
        memRequire,
        free
      )
    )
  }

  if (!getMemDiskRequirement) {
    amMsg(
      type = "log",
      text = sprintf(
        "Memory required for r.walk.accessmod = %1$s MB. Memory available = %2$s MB. Disk space required = %3$s MB. Disk space available = %4$s MB",
        memRequire,
        free,
        diskRequire,
        disk
      )
    )
  }

  if (!getMemDiskRequirement) {
    if (maxSpeed > 0 && maxTravelTime > 0) {
      on_exit_add({
        amSpeedBufferRegionRestore()
      })
      if (towardsFacilities) {
        amSpeedBufferRegionInit(
          c(inputHf, inputStop),
          maxSpeed / 3.6,
          maxTravelTime * 60
        )
      } else {
        amSpeedBufferRegionInit(
          c(inputHf),
          maxSpeed / 3.6,
          maxTravelTime * 60
        )
      }
    }

    #
    # Remove stops if not on current region
    #
    if (isNotEmpty(inputStop)) {
      tblStopTest <- amGetRasterValueAtPoint(
        inputStop,
        config$mapDem
      )
      hasNoStopInRegion <- isEmpty(tblStopTest)

      if (hasNoStopInRegion) {
        amParam$stop_points <- NULL
      }
    }

    #
    # Launch analysis
    #
    res <- execGRASS("r.walk.accessmod",
      parameters = amParam,
      flags = flags
    )
    if (res == 137) {
      stop("Process used too much ressources.")
    }

    if (!rawMode) {
      amCleanTravelTime(
        map = outputTravelTime,
        maxTravelTime = maxTravelTime,
        minTravelTime = minTravelTime,
        timeoutValue = timeoutValue,
        roundingMethod = roundingMethod,
        convertToMinutes = TRUE
      )
    }
  } else {
    return(
      list(
        required = list(
          memory = memRequire,
          disk = diskRequire
        ),
        available = list(
          memory = free,
          disk = disk
        )
      )
    )
  }
}


#' clean travel time map
#' @param map Raster travel time map
#' @param maxTravelTime Number. Maximum cost/travel time in minutes
#' @param minTravelTime Number. Minium cost/travel time in minutes
#' @param convertToMinutes Boolean. Convert the cleaned map to minutes
#' @param timeoutValue Number Integer to use as timeout remplacement value when maxTravelTime = 0
amCleanTravelTime <- function(map,
  maxTravelTime = 0,
  minTravelTime = NULL,
  convertToMinutes = TRUE,
  roundingMethod = c("ceil", "round", "floor"),
  timeoutValue = "null()"
) {
  roundingMethod <- match.arg(roundingMethod)

  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.

  unlimitedMode <- maxTravelTime == 0 || maxTravelTime >= int32Max
  maxSeconds <- 0
  divider <- 1
  timeoutMinutesLimit <- 0
  timeoutMinutesValue <- timeoutValue
  cutSecondsStart <- 0
  cutSecondsEnd <- 0
  hasTimeout <- FALSE

  if (convertToMinutes) {
    divider <- 60
  }

  if (unlimitedMode) {
    timeoutMinutesLimit <- int16Max
    cutSecondsEnd <- timeoutMinutesLimit * divider
  } else {
    timeoutMinutesLimit <- int32Max
    timeoutMinutesValue <- "null()"
    cutSecondsEnd <- maxTravelTime * divider
  }

  if (isEmpty(minTravelTime)) {
    cutSecondsStart <- 0
  } else {
    cutSecondsStart <- minTravelTime * divider
  }

  if (isEmpty(roundingMethod)) {
    roundingMethod <- "ceil"
  }

  #
  # NOTE mapcalc has a bug where value bigger than 2147483647 are NOT handled
  #
  cmd <- sprintf(
    " %1$s = %1$s >= %2$f && %1$s <= %3$f ? int(%7$s( %1$s / %6$f )) : %1$s / %6$f > %4$f ? %5$s : null() ",
    map # 1
    , cutSecondsStart # 2
    , cutSecondsEnd # 3
    , timeoutMinutesLimit # 4
    , timeoutMinutesValue # 5
    , divider # 6
    , roundingMethod # 7
  )

  execGRASS(
    "r.mapcalc",
    expression = cmd,
    flags = c("overwrite")
  )
}


#' Build HF layer based on selection table
#'
#' @param tableFacilities Facilities table with config$vectorKey attr
#' @param inputFacilities Facilities layer name
#' @return Name of the final facility layer
amFacilitiesSubset <- function(tableFacilities, inputFacilities) {
  #
  # If the table is empty, use all
  # - can happen in replay mode where no validation is done
  #
  emptyTable <- isEmpty(tableFacilities)

  if (emptyTable) {
    return(inputFacilities)
  }

  #
  # Get current id and selected id
  #
  idHfAll <- tableFacilities[, config$vectorKey]
  idHfSelect <- tableFacilities[
    tableFacilities$amSelect,
    config$vectorKey
  ]

  hasAllSelect <- identical(idHfSelect, idHfAll)

  if (hasAllSelect) {
    # No need for a subset
    return(inputFacilities)
  }

  #
  # Creating a subset
  #
  inputHfFinal <- amRandomName("tmp__")
  amCreateLayerSubset(
    input_vector = inputFacilities,
    output_vector = inputHfFinal,
    id_list = idHfSelect,
    keep = TRUE
  )

  return(inputHfFinal)
}


#' Get ressources estimations
#' @param input start vector
#' @return {List} list(required=list(memory,disk),available=list(memory,disk))
amGetRessourceEstimate <- function(hf) {
  out <- list(
    required = list(
      memory = 300,
      disk = 10
    ),
    available = list(
      memory = sysEvalFreeMbMem(),
      disk = sysEvalFreeMbDisk()
    )
  )

  if (isNotEmpty(hf)) {
    out <- amAnisotropicTravelTime(
      inputSpeed = config$mapDem,
      inputHf = hf,
      outputTravelTime = "tmp_test",
      getMemDiskRequirement = TRUE
    )
  }

  return(out)
}
