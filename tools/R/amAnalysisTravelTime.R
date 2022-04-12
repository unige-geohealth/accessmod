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
  useMaxSpeedMask = FALSE,
  timeoutValue
) {
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
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        timeoutValue = timeoutValue
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
        inputFriction = inputFriction,
        inputHf = inputHfFinal,
        outputTravelTime = outputTravelTime,
        outputNearest = outputNearest,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        timeoutValue = timeoutValue
      )
    }
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
  ratioMemory = 1,
  memory = NULL, # if set, absolute max memory
  rawMode = FALSE) {
  vInfo <- amParseOptions(
    execGRASS(
      "v.info",
      flags = c("t"),
      map = inputHf,
      intern = T
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
    memory <- as.integer(free * 0.8 * ratioMemory)
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
        intern = T
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

  if (!getMemDiskRequirement && diskRequire > disk * 0.8) {
    stop(
      sprintf(
        "Insufficient disk space. Required= %1$s MB, Available= %2$s MB",
        diskRequire,
        disk
      )
    )
  }
  if (!getMemDiskRequirement && memRequire > free * 0.8) {
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
    if (!isEmpty(inputStop)) {
      tblStopTest <- amGetRasterValueAtPoint(
        inputStop,
        config$mapDem
      )
      hasNoStopInRegion <- isEmpty(tblStopTest)

      if (hasNoStopInRegion) {
        amParam$stop_points <- NULL
      }
    }

    execGRASS("r.cost",
      parameters = amParam,
      flags = "overwrite"
    )

    if (!rawMode) {
      amCleanTravelTime(
        map = outputTravelTime,
        maxTravelTime = maxTravelTime,
        minTravelTime = minTravelTime,
        timeoutValue = timeoutValue,
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
  timeoutValue = "null()",
  getMemDiskRequirement = FALSE,
  ratioMemory = 1,
  memory = NULL, # if set, absolute max memory
  rawMode = FALSE # skip minute conversion; skip value removal above maxTravelTime
) {
  flags <- c(c("overwrite", "s"), ifelse(towardsFacilities, "t", ""))
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
      intern = T
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
    memory <- as.integer(free * 0.8 * ratioMemory)
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
        intern = T
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

  if (!getMemDiskRequirement && diskRequire > disk * 0.8) {
    stop(
      sprintf(
        "Insufficient disk space. Required= %1$s MB, Available= %2$s MB",
        diskRequire,
        disk
      )
    )
  }
  if (!getMemDiskRequirement && memRequire > free * 0.8) {
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
    if (!isEmpty(inputStop)) {
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
    execGRASS("r.walk.accessmod",
      parameters = amParam,
      flags = flags
    )

    if (!rawMode) {
      amCleanTravelTime(
        map = outputTravelTime,
        maxTravelTime = maxTravelTime,
        minTravelTime = minTravelTime,
        timeoutValue = timeoutValue,
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
  timeoutValue = "null()") {
  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.

  int16Max <- (2^16) / 2 - 1
  int32Max <- (2^32) / 2 - 1
  unlimitedMode <- maxTravelTime == 0
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

  #
  # NOTE mapcalc has a bug where value bigger than 2147483647 are NOT handled
  #

  cmd <- sprintf(
    " %1$s = %1$s >= %2$d && %1$s <= %3$d ? round((( %1$s / %6$f) - (( %1$s / %6$f ) %% 1))) : %1$s / %6$d > %4$d ? %5$s : null() ",
    map # 1
    , cutSecondsStart # 2
    , cutSecondsEnd # 3
    , timeoutMinutesLimit # 4
    , timeoutMinutesValue # 5
    , divider # 6
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
  # WORKAROUND for solving the issue #209
  # That produced a "Argument list to long in v.extract"
  # The error visible was "Cannot open connection", but it's
  # unrelated to the actual error.
  # Strategy :
  # Using smallest subset OR if all selected, don't extract

  idHfAll <- tableFacilities[, config$vectorKey]
  idHfSelect <- tableFacilities[
    tableFacilities$amSelect,
    config$vectorKey
  ]

  fName <- amRandomName("tmp__")
  idHfNotSelect <- idHfAll[!idHfAll %in% idHfSelect]
  hasMoreSelect <- length(idHfNotSelect) < length(idHfSelect)
  hasAllSelect <- identical(idHfSelect, idHfAll)
  inputHfFinal <- ifelse(hasAllSelect, inputFacilities, fName)

  if (!hasAllSelect) {
    if (hasMoreSelect) {
      qSql <- sprintf(
        " %1$s NOT IN ( %2$s )",
        config$vectorKey,
        paste0("'", idHfNotSelect, "'", collapse = ",")
      )
    } else {
      qSql <- sprintf(
        " %1$s IN ( %2$s )",
        config$vectorKey,
        paste0("'", idHfSelect, "'", collapse = ",")
      )
    }

    #
    # Create a temporay copy
    #
    execGRASS(
      "v.extract",
      flags = "overwrite",
      input = inputFacilities,
      where = qSql,
      output = inputHfFinal
    )
  }

  return(inputHfFinal)
}
