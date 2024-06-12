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
  # travel time rounding method (not used, as RAW mode)
  # -> use ceiling for tables
  roundingMethod <- job$roundingMethod

  tmpMapset <- amRandomName("tmp_mapset")
  tblDefault <- data.frame(NA, idFrom, NA, NA)
  distDivider <- 1
  if (!unitDist == "m") {
    distDivider <- switch(unitDist,
      "km" = 1000
    )
  }

  names(tblDefault) <- c(
    "cat_from",
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
        intern = TRUE
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
          # 1. resol / 2: This threshold sets the max distance
          # for a point to connect to the network to half of
          # the resolution of the raster data.
          #  +───┬───┐
          #  │   │   │
          #  ├───+   │
          #  │       │
          #  └───────┘
          #
          # 2. resol / cos(45 * pi / 180): This threshold
          # scales the max distance to the diagonal of the
          # raster cell.
          #
          # ┌──────+
          # │     /│
          # │    / │
          # │   +  │
          # │      │
          # │      │
          # └──────┘
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
            path            = amRandomName("tmp__path")
          )
          tmpRaster <- list(
            travelTime      = amRandomName("tmp__cost"),
            travelDirection = amRandomName("tmp__dir"),
            selectFrom      = amRandomName("tmp__ref_from"),
            selectTo        = amRandomName("tmp__ref_to")
          )
          tmpTable <- list(
            catFilter = amRandomName("tmp__cat")
          )
          #
          # subset hf from
          #
          amCreateLayerSubset(
            input_vector = inputHfFrom,
            output_vector = tmpVector$selectFrom,
            id_list = idFrom
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
          # Create layer based on selected id
          #
          amCreateLayerSubset(
            input_vector = inputHfTo,
            output_vector = tmpVector$selectTo,
            id_list = idListTo,
            keep = TRUE
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
                roundingMethod = roundingMethod,
                memory = memory,
                rawMode = TRUE # cancels rounding method
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
                roundingMethod = roundingMethod,
                memory = memory,
                rawMode = TRUE # cancels rounding method
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
            intern = TRUE
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
          refTime[["cat_from"]] <- idFrom

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
          refTime[unitCost] <- as.integer(ceiling(refTime[, unitCost]))

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
                  catToRemove <- catTo[!catTo %in% catToKeep]
                  amLayerDeleteCat(
                    input_vector = tmpVector$selectTo,
                    id_list = catToRemove
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
                  # - stop point is r.walk.accessmod start point
                  #
                  # NOTE: this should be followed by v.clean  with rmdupl,break
                  # but it's very slow and the result is not that good
                  # a lot of lines are still duplicated.
                  # See
                  # - https://grasswiki.osgeo.org/wiki/Vector_topology_cleaning
                  execGRASS("r.path",
                    input        = tmpRaster$travelDirection,
                    vector_path  = tmpVector$path,
                    flags        = c("overwrite"),
                    start_points = tmpVector$selectTo
                  )

                  countLine <- amGetTableFeaturesCount(
                    tmpVector$path,
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
                    sdfFrom <- readVECT(tmpVector$selectFrom,
                      type = "point",
                      driver = "GPKG",
                      ignore.stderr = TRUE
                    )
                    sdfTo <- readVECT(tmpVector$selectTo,
                      type = "point",
                      driver = "GPKG",
                      ignore.stderr = TRUE
                    )

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
                      vname = tmpVector$path,
                      v.in.ogr_flags = c("o", "overwrite")
                    )
                  }

                  netFlagsConnect <- c("overwrite")

                  if (snapToGrid) {
                    netFlagsConnect <- c(netFlagsConnect, "s")
                  }

                  #
                  # Build network with r.path result
                  # and from points
                  #
                  execGRASS("v.net",
                    input      = tmpVector$path,
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
                  # - distance from all node in
                  #          -> 2 (start)
                  #          -> 3 (many destination)
                  #
                  execGRASS("v.net.distance",
                    input      = tmpVector$netAllNodes,
                    output     = tmpVector$netDist,
                    from_layer = "3",
                    to_layer   = "2",
                    flags      = c("overwrite")
                  )

                  #
                  # Rename tcat
                  #
                  execGRASS("db.execute",
                    sql = sprintf(
                      "ALTER TABLE %s ADD COLUMN cat_to integer",
                      tmpVector$netDist
                    )
                  )
                  execGRASS("db.execute",
                    sql = sprintf(
                      "ALTER TABLE %s ADD COLUMN cat_from integer",
                      tmpVector$netDist
                    )
                  )
                  execGRASS("db.execute",
                    sql = sprintf(
                      "UPDATE %s SET cat_to = cat",
                      tmpVector$netDist
                    )
                  )
                  execGRASS("db.execute",
                    sql = sprintf(
                      "UPDATE %s SET cat_from = tcat",
                      tmpVector$netDist
                    )
                  )
                  execGRASS("db.execute",
                    sql = sprintf(
                      "ALTER TABLE %s DROP COLUMN tcat",
                      tmpVector$netDist
                    )
                  )
                  #
                  # Convert dist + round
                  #
                  execGRASS("db.execute",
                    sql = sprintf(
                      "ALTER TABLE %s ADD COLUMN %s integer",
                      tmpVector$netDist,
                      unitDist
                    )
                  )
                  execGRASS("db.execute",
                    sql = sprintf(
                      "UPDATE %s SET %s = CEILING(dist/%d)",
                      tmpVector$netDist,
                      unitDist,
                      distDivider
                    )
                  )
                  execGRASS("db.execute",
                    sql = sprintf(
                      "ALTER TABLE %s DROP COLUMN dist",
                      tmpVector$netDist
                    )
                  )

                  #
                  # Read and rename calculated distances
                  #
                  refDist <- amMapsetDbGetQuery(tmpMapset, tmpVector$netDist)
                  refDist$cat <- NULL

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
                      spNetDist <- merge(
                        spNetDist[, c("cat_to", "cat_from", unitDist)],
                        refTime,
                        by = c("cat_from", "cat_to")
                      )

                      if (!permuted) {
                        names(spNetDist) <- c(
                          "from__cat",
                          "to__cat",
                          unitDist,
                          unitCost
                        )
                      } else {
                        #
                        # from / to swap
                        #
                        names(spNetDist) <- c(
                          "to__cat",
                          "from__cat",
                          unitDist,
                          unitCost
                        )
                        #
                        # Reorder so from is first
                        #
                        spNetDist <- spNetDist[
                          ,
                          c(
                            "from__cat",
                            "to__cat",
                            unitDist,
                            unitCost
                          )
                        ]
                      }


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
          # Merge dist and time
          #
          refDistTime <- merge(
            x = refDist,
            y = refTime,
            all.y = TRUE,
            by = c("cat_from", "cat_to")
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
