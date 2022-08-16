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

# Time distance analysis

amTimeDist <- function(job, memory = 300) {
  suppressPackageStartupMessages(source("global.R"))

  inputHfFrom <- job$inputHfFrom
  inputHfTo <- job$inputHfTo
  idFrom <- job$idFrom
  idListTo <- job$idListTo
  inputSpeed <- job$inputSpeed
  inputFriction <- job$inputFriction
  maxTravelTime <- job$maxTravelTime
  maxSpeed <- job$maxSpeed
  typeAnalysis <- job$typeAnalysis
  knightMove <- job$knightMove
  permuted <- job$permuted
  unitCost <- job$unitCost
  unitDist <- job$unitDist
  limitClosest <- job$limitClosest
  resol <- job$resol
  location <- job$location
  mapset <- job$mapset
  keepNetDist <- job$keepNetDist
  keepNetDistPath <- job$keepNetDistPath
  snapToGrid <- job$snapToGrid

  tmpMapset <- amRandomName("tmp_mapset")
  tblDefault <- data.frame(NA, idFrom, NA, NA)
  unitNetCost <- sprintf("time_%s", unitCost)
  unitNetDist <- sprintf("dist_%s", unitDist)

  names(tblDefault) <- c(
    "cat",
    "cat_to",
    unitDist,
    unitCost
  )

  #
  # Clean on exit
  #
  on_exit_add({
    if (amMapsetExists(tmpMapset)) {
      amMapsetRemove(
        mapset = tmpMapset,
        stringCheck = "^tmp_",
        location = location
      )
    }
  })

  logDiagnostic <- function(err) {
    diagnosticLog <- list(
      message = err$message,
      call = deparse(err$call),
      ressources = amGetRessourceEstimate(inputHfTo),
      freeMemMb = sysEvalFreeMbMem(),
      freeDiskMb = sysEvalFreeMbDisk(),
      job = job,
      mapsetDetected = amGrassSessionGetMapset(),
      mapsetRequested = mapset,
      mapsetAll = amMapsetGetAll(),
      rasters = execGRASS("g.list",
        type = "raster",
        intern = T
      ),
      vectors = execGRASS("g.list",
        type = "vector",
        intern = T
      )
    )

    strDiagnosticLog <- toJSON(diagnosticLog, auto_unbox = T)

    amMsg(
      type = "log",
      title = "Referral failure log",
      text = strDiagnosticLog
    )
  }


  #
  # Main script
  #
  amGrassNS(
    mapset = mapset,
    location = location,
    resetRegion = FALSE,
    {
      tryCatch(
        {
          #
          # Create temporary mapset
          #
          amMapsetCreate(
            mapset = tmpMapset,
            switch = TRUE
          )
          ok <- amMapsetExists(tmpMapset)

          if (!ok) {
            stop(sprintf("Mapset %s not created", tmpMapset))
          }

          #
          # Sanitize options
          #
          if (isTRUE(permuted)) {
            limitClosest <- FALSE
          }

          #
          # Network threshold
          #
          netThreshold <- resol / cos(45 * pi / 180)

          #
          # Output table
          #
          refDistTime <- list()

          #
          # List cat closer than resolution : they will be excluded if distance
          # is requested, but a linear distance will be used as fallback from
          # dfDistFromTo table
          #
          catToClose <- c()
          dfDistFromTo <- data.frame()
          #
          # Ref time dist to itself
          #
          sameFromTo <- identical(inputHfFrom, inputHfTo)

          #
          # Temporary layers
          #
          tmpVector <- list(
            selectFrom      = amRandomName("tmp__ref_from"),
            selectTo        = amRandomName("tmp__ref_to"),
            netFrom         = amRandomName("tmp__net_from"),
            netAll          = amRandomName("tmp__net_all"),
            netAllNodes     = amRandomName("tmp__net_all"),
            netDist         = amRandomName("tmp__net_dist"),
            drain           = amRandomName("tmp__drain")
          )
          tmpRaster <- list(
            travelTime      = amRandomName("tmp__cost"),
            travelDirection = amRandomName("tmp__dir"),
            drain           = amRandomName("tmp__drain"),
            selectFrom      = amRandomName("tmp__ref_from"),
            selectTo        = amRandomName("tmp__ref_to")
          )
          #
          # subset hf from
          #
          qSqlFrom <- sprintf(
            "cat == '%s'",
            idFrom
          )

          execGRASS("v.extract",
            flags  = c("overwrite"),
            input  = inputHfFrom,
            where  = qSqlFrom,
            output = tmpVector$selectFrom
          )

          #
          # subset ref to itself
          #
          if (sameFromTo) {
            idListTo <- idListTo[!idListTo %in% idFrom]
          }
          if (isEmpty(idListTo)) {
            return(tblDefault)
          }

          #
          # Subseting target facilities
          #
          qSqlTo <- sprintf(
            "cat IN ( %s )",
            paste0(
              idListTo,
              collapse = ","
            )
          )

          execGRASS("v.extract",
            flags  = c("overwrite"),
            input  = inputHfTo,
            where  = qSqlTo,
            output = tmpVector$selectTo
          )


          #
          # Travel time callback
          # Use raw mode :
          # - Don't convert to minute
          # - Don't trim values
          #
          refTravelTime <- function() {
            switch(typeAnalysis,
              "anisotropic" = amAnisotropicTravelTime(
                inputSpeed = inputSpeed,
                inputHf = tmpVector$selectFrom,
                inputStop = tmpVector$selectTo,
                outputTravelTime = tmpRaster$travelTime,
                outputDir = tmpRaster$travelDirection,
                towardsFacilities = permuted,
                maxTravelTime = maxTravelTime,
                maxSpeed = maxSpeed,
                knightMove = knightMove,
                timeoutValue = "null()",
                memory = memory,
                rawMode = TRUE
              ),
              "isotropic" = amIsotropicTravelTime(
                inputFriction = inputFriction,
                inputHf = tmpVector$selectFrom,
                inputStop = tmpVector$selectTo,
                outputTravelTime = tmpRaster$travelTime,
                outputDir = tmpRaster$travelDirection,
                maxTravelTime = maxTravelTime,
                maxSpeed = maxSpeed,
                knightMove = knightMove,
                timeoutValue = "null()",
                memory = memory,
                rawMode = TRUE
              )
            )
            ok <- amRastExists(tmpRaster$travelTime)
            return(ok)
          }

          #
          # Travel time
          # - multiple try: sometimes some weird things occur with memory /
          #  disk usage in workers... As it's blocking and the whole process
          #  could fail, better try mutiple time before giving up...
          #
          nTry <- 5
          while (nTry > 0) {
            ok <- refTravelTime()
            if (ok) {
              nTry <- -1
            } else {
              nTry <- nTry - 1
              if (nTry <= 0) {
                stop("No travel time produced")
              }
            }
          }


          #
          # extact cost for each destination point
          #
          refTimeRaw <- execGRASS(
            "v.what.rast",
            map    = tmpVector$selectTo,
            raster = tmpRaster$travelTime,
            flags  = "p",
            intern = T
          )

          if (isEmpty(refTimeRaw)) {
            return(tblDefault)
          }

          refTime <- amCleanTableFromGrass(
            refTimeRaw,
            header = FALSE,
            na.strings = "*",
            colClasses = c(typeof(idFrom), "numeric")
          )

          # rename grass output
          names(refTime) <- c("cat_to", unitCost)

          # set "from" value
          refTime[["cat"]] <- idFrom

          #
          # Convert units
          #
          if (unitCost != "s") {
            div <- switch(unitCost,
              "s" = 1,
              "m" = 60,
              "h" = 3600,
              "d" = 86400
            )
            refTime[unitCost] <- refTime[unitCost] / div
          }
          refTime[unitCost] <- round(refTime[unitCost], 2)

          #
          # Check if all destination are unreachable
          #
          emptyCheck <- all(sapply(refTime[, unitCost], isEmpty))
          hasNoDest <- isTRUE(emptyCheck)

          #
          # Use refTime as template for distances
          #
          refDist <- refTime[c(), ]
          names(refDist)[names(refDist) == unitCost] <- unitDist

          #
          # At least one destination : compute distance
          #
          if (!hasNoDest) {
            tryCatch(
              {
                #
                # Remove destination not in range OR not closest
                #
                catTo <- na.omit(refTime[, "cat_to"])
                if (limitClosest) {
                  #
                  # Get closest in time, get position, subset,
                  # NOTE: which.min does not keep ties.
                  #
                  minTime <- min(refTime[, unitCost], na.rm = TRUE)
                  minPos <- which(refTime[, unitCost] == minTime)
                  catToKeep <- refTime[minPos, "cat_to"]
                } else {
                  #
                  # Limit in max cost range
                  #
                  inRange <- TRUE
                  if (maxTravelTime > 0) {
                    inRange <- refTime[, unitCost] <= maxTravelTime
                  }
                  catToKeep <- na.omit(refTime[inRange, "cat_to"])
                }

                doSubset <- !all(catTo %in% catToKeep)

                if (doSubset) {
                  qSqlTo <- sprintf(
                    "cat not in (%s) ",
                    paste(catToKeep, collapse = ",")
                  )

                  execGRASS(
                    "v.edit",
                    map   = tmpVector$selectTo,
                    tool  = "delete",
                    where = qSqlTo
                  )
                }

                countToLeft <- amGetTableFeaturesCount(
                  tmpVector$selectTo,
                  c("points")
                )$count

                #
                # Continue only if there is still destinations
                #
                if (countToLeft > 0) {

                  #
                  # Built paths
                  # NOTE: this should be followed by
                  # `v.clean type=line tool=rmdupl,break flags=c`,
                  # See https://grasswiki.osgeo.org/wiki/Vector_topology_cleaning)
                  # but it's very slow and the result is not that good : a lot of
                  # lines are still duplicated.
                  #
                  execGRASS("r.drain",
                    input        = tmpRaster$travelTime,
                    direction    = tmpRaster$travelDirection,
                    output       = tmpRaster$drain, # raster drain
                    drain        = tmpVector$drain,
                    flags        = c("overwrite", "c", "d"),
                    start_points = tmpVector$selectTo
                  )

                  countLine <- amGetTableFeaturesCount(
                    tmpVector$drain,
                    c("lines")
                  )$count


                  if (isTRUE(countLine == 0) && isTRUE(countToLeft == 1)) {
                    #
                    # Drain has no line:
                    # - Add a small line between facilities
                    # - This could happen in a1 (same cell), a2 (snap)
                    #
                    #               1             2
                    #        ┌─────────────┬─────────────┐
                    #        │             │             │
                    #        │        y    │             │
                    #        │             │     xy      │
                    #    a   │     x       │             │
                    #        │             │             │
                    #        ├─────────────┼─────────────┤
                    #        │             │             │
                    #        │             │             │
                    #    b   │     x───────┼─────►y      │
                    #        │             │             │
                    #        │             │             │
                    #        └─────────────┴─────────────┘
                    #
                    sdfFrom <- readVECT(tmpVector$selectFrom)
                    sdfTo <- readVECT(tmpVector$selectTo)

                    #
                    # Create at least a distance of 2.8m
                    # -> sqrt(2^2+2^2)
                    #
                    sdfFrom@coords <- sdfFrom@coords + 1
                    sdfTo@coords <- sdfTo@coords - 1

                    tmpLine <- Line(
                      rbind(
                        sdfTo@coords,
                        sdfFrom@coords
                      )
                    )
                    spLine <- SpatialLines(
                      list(
                        Lines(
                          list(tmpLine),
                          ID = "net"
                        )
                      )
                    )
                    spData <- data.frame(id = "net", row.names = "net")
                    spdfLine <- SpatialLinesDataFrame(spLine, spData)

                    writeVECT(
                      spdfLine,
                      driver = "GPKG",
                      vname  = tmpVector$drain,
                      v.in.ogr_flags = c("o", "overwrite")
                    )
                  }


                  netFlagsConnect <- c("overwrite")

                  if (snapToGrid) {
                    netFlagsConnect <- c(netFlagsConnect, "s")
                  }

                  #
                  # Build network with drain result
                  # and from points
                  #
                  execGRASS("v.net",
                    input      = tmpVector$drain,
                    points     = tmpVector$selectFrom,
                    output     = tmpVector$netFrom,
                    node_layer = "2",
                    operation  = "connect",
                    threshold  = netThreshold,
                    flags      = netFlagsConnect
                  )

                  #
                  # Connect the destination facility to the network
                  #
                  execGRASS("v.net",
                    input      = tmpVector$netFrom,
                    points     = tmpVector$selectTo,
                    output     = tmpVector$netAll,
                    node_layer = "3",
                    operation  = "connect",
                    threshold  = netThreshold,
                    flags      = netFlagsConnect
                  )

                  #
                  # Connect the destination facility to the network
                  #
                  execGRASS("v.net",
                    input      = tmpVector$netAll,
                    output     = tmpVector$netAllNodes,
                    node_layer = "4",
                    operation  = "nodes",
                    threshold  = netThreshold,
                    flags      = c("overwrite")
                  )

                  #
                  # Calculate distance on the net
                  #
                  execGRASS("v.net.distance",
                    input      = tmpVector$netAllNodes,
                    output     = tmpVector$netDist,
                    from_layer = "3", # calc distance from all node in 3 to layer 2 (start point)
                    to_layer   = "2",
                    flags      = c("overwrite")
                  )

                  #
                  # Read and rename calculated distances
                  #
                  refDist <- amMapsetDbGetQuery(tmpMapset, tmpVector$netDist)
                  names(refDist) <- c("cat_to", "cat", unitDist)

                  if (keepNetDist) {
                    #
                    # Export network
                    #
                    tmpVectOut <- sprintf(
                      "%1$s_%2$s.gpkg",
                      tmpVector$netDist,
                      idFrom
                    )
                    netFilePath <- file.path(
                      keepNetDistPath,
                      tmpVectOut
                    )
                    tblFeaturesCount <- amGetTableFeaturesCount(
                      tmpVector$netDist
                    )
                    isNetEmpty <- tblFeaturesCount[
                      tblFeaturesCount$type == "lines",
                    ]$count == 0

                    if (!isNetEmpty) {

                      #
                      # Get the network in memory
                      #
                      spNetDist <- readVECT(tmpVector$netDist,
                        type = "line",
                        driver = "GPKG",
                        ignore.stderr = TRUE
                      )

                      #
                      # Merge time + clean
                      #
                      tmpRefTime <- na.omit(refTime[, c("m", "cat_to")])
                      spNetDist <- merge(
                        spNetDist,
                        tmpRefTime,
                        by.x = "cat",
                        by.y = "cat_to"
                      )
                      names(spNetDist) <- c(
                        "cat_to",
                        "cat_from",
                        unitNetDist,
                        unitNetCost
                      )
                      #
                      # Convert distances
                      #
                      if (!unitDist == "m") {
                        div <- switch(unitDist,
                          "km" = 1000
                        )
                        spNetDist@data[, unitNetDist] <- spNetDist@data[, unitNetDist] / div
                      }
                      spNetDist@data[, unitNetDist] <- round(spNetDist@data[, unitNetDist], 3)
                      #
                      # Write layer (to be merged outside worker)
                      #
                      writeOGR(
                        spNetDist,
                        dsn = netFilePath,
                        layer = "am_dist_net",
                        driver = "GPKG"
                      )
                    }
                  }
                }
              },
              error = function(e) {
                #
                # Avoid stopping the worker, but send a warning
                if ("debug" %in% config$logMode) {
                  stop(e)
                } else {
                  warning(e)
                }
              }
            )
          }

          #
          # Convert distances
          #
          if (!unitDist == "m") {
            div <- switch(unitDist,
              "km" = 1000
            )
            refDist[, unitDist] <- refDist[, unitDist] / div
          }
          refDist[, unitDist] <- round(refDist[, unitDist], 4)

          #
          # Merge dist and time
          #
          refDistTime <- merge(
            refDist,
            refTime,
            by = c("cat", "cat_to"),
            all.y = T
          )

          return(refDistTime)
        },
        error = function(e) {
          logDiagnostic(e)
          if ("debug" %in% config$logMode) {
            stop(e)
          } else {
            warning(e)
          }
        }
      )
    }
  )

  return(tblDefault)
}
