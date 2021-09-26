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



amTimeDist <- function( job, memory = 300 ){

  source('global.R')
  inputHfFrom     = job$inputHfFrom
  inputHfTo       = job$inputHfTo
  idFrom          = job$idFrom
  idListTo        = job$idListTo
  inputSpeed      = job$inputSpeed
  inputFriction   = job$inputFriction
  maxCost         = job$maxCost
  maxSpeed        = job$maxSpeed
  typeAnalysis    = job$typeAnalysis
  permuted        = job$permuted
  unitCost        = job$unitCost
  unitDist        = job$unitDist
  limitClosest    = job$limitClosest
  resol           = job$resol
  origProject     = job$origProject
  keepNetDist     = job$keepNetDist
  keepNetDistPath = job$keepNetDistPath

  tmpMapset <- amRandomName("tmp_mapset")
 

  #
  # Main script
  #
  on.exit({
    amMapsetRemove(tmpMapset,stringCheck='^tmp_')
  })

  tryCatch({

    #
    # Init temporary mapset
    #
    amMapsetInit(origProject,tmpMapset)
    #
    # Sanitize options
    #
    if(isTRUE(permuted)){
      limitClosest <- FALSE
    }

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
    sameFromTo <- identical(inputHfFrom,inputHfTo)

    #
    # Temporary layers
    #
    tmpVector <- list(
      selectFrom      = amRandomName("tmp__ref_from"),
      selectTo        = amRandomName("tmp__ref_to"),
      netFrom         = amRandomName("tmp__net_from"),
      netAll          = amRandomName("tmp__net_all"),
      netDist         = amRandomName("tmp__net_dist")
    )
    tmpRaster <- list(
      travelTime      = amRandomName("tmp__cost"),
      travelDirection = amRandomName("tmp__dir"),
      drain           = amRandomName("tmp__drain")
    )
    #
    # subset hf from 
    #
    qSqlFrom <- sprintf("cat == '%s'",
      idFrom
    )

    execGRASS("v.extract",
      flags  = c('overwrite'),
      input  = inputHfFrom,
      where  = qSqlFrom,
      output = tmpVector$selectFrom
    )
    
    #
    # subset ref to itself 
    #
    if(sameFromTo){
      idListTo <- idListTo[!idListTo %in% idFrom]
    }

    #
    # Subseting target facilities
    #
    qSqlTo <- sprintf("cat IN ( %s )",
      paste0( 
        idListTo
        , collapse = ','
      )
    )


    execGRASS("v.extract",
      flags  = c('overwrite'),
      input  = inputHfTo,
      where  = qSqlTo,
      output = tmpVector$selectTo
    )

    tt <- function(){
      switch(typeAnalysis,
        'anisotropic'      = amAnisotropicTravelTime(
          inputSpeed       = inputSpeed,
          inputHf          = tmpVector$selectFrom,
          inputStop        = tmpVector$selectTo,
          outputCumulative = tmpRaster$travelTime,
          outputDir        = tmpRaster$travelDirection,
          returnPath       = permuted,
          maxCost          = maxCost,
          maxSpeed         = maxSpeed,
          timeoutValue     = "null()",
          memory           = memory,
          rawMode          = TRUE # don't convert to minute, do not remove value above max cost
          ),
        'isotropic'        = amIsotropicTravelTime(
          inputFriction    = inputFriction,
          inputHf          = tmpVector$selectFrom,
          inputStop        = tmpVector$selectTo,
          outputCumulative = tmpRaster$travelTime,
          outputDir        = tmpRaster$travelDirection,
          maxCost          = maxCost,
          maxSpeed         = maxSpeed,
          timeoutValue     = "null()",
          memory           = memory,
          rawMode          = TRUE # don't convert to minute, do not remove value above max cost
        )
      )
    }

    #
    # Travel time
    #
    nTry <- 5
    while(nTry > 0){
      tt()
      ok <- amRastExists(tmpRaster$travelTime)
      if(ok){
        nTry = -1
      }else{
        conf <- list(
          iter = nTry,
          ressources = amGetRessourceEstimate(inputHfTo),
          freeMemMb = sysEvalFreeMbMem(),
          freeDiskMb = sysEvalFreeMbDisk(),
          inputHfTo = inputHfTo,
          inputHfFrom = inputHfFrom,
          mapset =  amMapsetGet(),
          rasters = execGRASS('g.list',
            type = "raster",
            intern = T
            ),
          vectors = execGRASS('g.list',
            type = "vector",
            intern = T
            ),
          sqlTo = qSqlTo,
          sqlFrom = qSqlFrom,
          args = list(
            inputSpeed       = inputSpeed,
            inputHf          = tmpVector$selectFrom,
            inputStop        = tmpVector$selectTo,
            outputCumulative = tmpRaster$travelTime,
            outputDir        = tmpRaster$travelDirection,
            returnPath       = permuted,
            maxCost          = maxCost,
            maxSpeed         = maxSpeed,
            timeoutValue     = "null()",
            memory           = memory,
            rawMode          = TRUE
          )
        )
        conf$toExists <- amVectExists(tmpVector$selectTo)
        conf$fromExists <- amVectExists(tmpVector$selectFrom)
        conf <- jsonlite::toJSON(conf,auto_unbox=T)
        tmpFile <- sprintf('/tmp/%s.log',as.numeric(Sys.time()))
        write(conf, tmpFile)
        nTry <- nTry - 1
      }
    }

    #
    # extact cost for each destination point
    #
    refTime <- execGRASS(
      'v.what.rast',
      map    = tmpVector$selectTo,
      raster = tmpRaster$travelTime,
      flags  = 'p',
      intern = T
      ) %>%
    amCleanTableFromGrass(
      header = FALSE,
      na.strings = "*",
      colClasses = c(typeof(idFrom),"numeric")
    )

    # rename grass output
    names(refTime) <- c('cat_to',unitCost)

    # set "from" value
    refTime[['cat']] <- idFrom

    #
    # Convert units
    # 
    if( unitCost != 's' ){
      div<-switch(unitCost,
        's' = 1,
        'm' = 60,
        'h' = 3600,
        'd' = 86400
      )
      refTime[unitCost] <- refTime[unitCost]/div
    }
    refTime[unitCost] <- round(refTime[unitCost],2)

    #
    # Check if all destination are unreachable
    #
    emptyCheck <- all(sapply(refTime[,unitCost],amNoDataCheck))
    hasNoDest <- isTRUE(emptyCheck)

    #
    # Use refTime as template for distances
    #
    refDist <- refTime[c(),]
    names(refDist)[names(refDist) == unitCost] <- unitDist

    #
    # Extract network and distance
    # Known issue 
    #  - if distance from -> to is less than resolution, for now, we report 
    #    euclidean distance without building network branch: r.drain can't works.
    #    NOTE: in this case, v.net + "arcs" could be used to connect points ?
    #     
    #               │
    #  ┌────────────┼────────────┐
    #  │            │            │
    #  │            │   x ─┐     │
    #  │            │      │     │
    #  │            │      x     │
    #  │            │            │
    #──┼────────────┼────────────┼──
    #  │            │            │
    #  │            │            │
    #  │            │            │
    #  │            │            │
    #  │            │            │
    #  └────────────┼────────────┘
    #               │ 
    #
    if( !hasNoDest ){
    
      tryCatch({

        #
        # subset closest destination point if needed
        #
        if( limitClosest ){
          #
          # Get closest in time, get position, subset,
          # NOTE: which.min does not keep ties.
          #
          minTime <- min(refTime[,unitCost],na.rm=TRUE)
          minPos <- which(refTime[,unitCost] == minTime)
          catToKeep <- refTime[minPos,'cat_to']
        }else{
          #
          # Limit in max cost range
          #
          inRange <- TRUE
          if(maxCost > 0){
            inRange <- refTime[,unitCost] <= maxCost
          }
          catToKeep <- na.omit(refTime[inRange,'cat_to'])
        }

        # 
        # Select all values that are not the closest or in range
        # and remove them from the layer
        #
        qSqlTo <- sprintf("cat not in (%s) "
          , paste(catToKeep,collapse=",")
        )

        execGRASS(
          "v.edit",
          map   = tmpVector$selectTo,
          tool  = "delete",
          where = qSqlTo
        )

        countToLeft <- amGetTableFeaturesCount(
          tmpVector$selectTo,
          c('points')
          )$count
        

        if( countToLeft > 0 ){
          #
          # Get linear distance and remove distance less than resol 
          #
          dfDistFromTo <- execGRASS('v.distance',
            flags     = 'p',
            from      = tmpVector$selectFrom,
            from_type = 'point',
            to        = tmpVector$selectTo,
            to_type   = 'point',
            upload    = c('cat','dist'),
            intern    = TRUE
            ) %>% 
          amCleanTableFromGrass(
            header = TRUE,
            na.strings = "*",
            colClasses = c("numeric","numeric","numeric")
          )

          catToClose <- dfDistFromTo[
            dfDistFromTo$dist <= resol,
            c('cat')
            ]

          if(!amNoDataCheck(catToClose)){

            qSqlTo <- sprintf("cat in (%s) "
              , paste(catToClose,collapse=",")
            )

            execGRASS(
              "v.edit",
              map   = tmpVector$selectTo,
              tool  = "delete",
              where = qSqlTo
            )

          }
        }
        
        countToLeft <- amGetTableFeaturesCount(
          tmpVector$selectTo,
          c('points')
          )$count
       
        #
        # Continue only if there is still destinations
        #
        if( countToLeft > 0 ){

          #
          # Built paths
          #
          execGRASS('r.drain',
            input        = tmpRaster$travelTime,
            direction    = tmpRaster$travelDirection,
            output       = tmpRaster$drain,
            drain        = tmpRaster$drain,
            flags        = c('overwrite','c','d'),
            start_points = tmpVector$selectTo
          )

          #
          # Connect the starting facilities to the drain path
          #
          execGRASS('v.net',
            input      = tmpRaster$drain,
            points     = tmpVector$selectFrom,
            output     = tmpVector$netFrom,
            node_layer = '2',
            operation  = 'connect',
            threshold  = resol-1,
            flags      = 'overwrite'
          )

          #
          # Connect the destination facility to the network
          #
          execGRASS('v.net',
            input      = tmpVector$netFrom,
            points     = tmpVector$selectTo,
            output     = tmpVector$netAll,
            node_layer = '3',
            operation  = 'connect',
            threshold  = resol-1,
            flags      = 'overwrite'
          )

          #
          # Calculate distance on the net
          #
          execGRASS('v.net.distance',
            input      = tmpVector$netAll,
            output     = tmpVector$netDist,
            from_layer = '3', # calc distance from all node in 3 to layer 2 (start point)
            to_layer   = '2',
            intern     = T,
            flags      = 'overwrite'
          )

          #
          # Read and rename calculated distances
          #
          refDist <- amMapsetDbGetQuery(tmpMapset,tmpVector$netDist)
          names(refDist) <- c('cat_to','cat',unitDist)

          #
          # Export to origin project
          #
          if(keepNetDist){
            tmpVectOut <- sprintf(
              '%1$s_%2$s.gpkg',
              tmpVector$netDist,
              idFrom
            )
            netFilePath <- file.path(
              keepNetDistPath,
              tmpVectOut
            )
            tblFeaturesCount <- amGetTableFeaturesCount(tmpVector$netDist)
            isNetEmpty <- tblFeaturesCount[
              tblFeaturesCount$type=='lines',
              ]$count == 0

            if(!isNetEmpty){

              spNetDist <- readVECT(tmpVector$netDist,
                type = 'line', 
                driver = "GPKG",
                ignore.stderr = TRUE
              )
              #
              # Renaming "cat" in GRASS is not possible
              #
              tmpRefTime <- na.omit(refTime[,c('m','cat_to')])
              spNetDist <- merge(spNetDist,tmpRefTime, by.x ='cat', by.y='cat_to')
              unitNetCost  <- sprintf('time_%s',unitCost)
              unitNetDist <- sprintf('dist_%s',unitDist)
              names(spNetDist) <- c(
                'cat_to',
                'cat_from',
                unitNetDist,
                unitNetCost
              )
              #
              # Convert distances
              #
              if(!unitDist=='m'){
                div <- switch(unitDist,
                  'km' = 1000
                )
                spNetDist@data[,unitNetDist]<-spNetDist@data[,unitNetDist]/div
              }
              spNetDist@data[,unitNetDist] <- round(spNetDist@data[,unitNetDist],3)
              
              #
              # Write layer (to be merged outside worker)
              #
              writeOGR(
                spNetDist, 
                dsn = netFilePath, 
                layer = 'am_dist_net',
                driver = "GPKG"
              )
            }
          }
          
        }
      },error=function(e){
        #
        # Avoid stopping the worker, but send a warning
        #
        warning(e) 
      })
    }
   

    #
    # catToClose = ref that are less than resol m away
    # -> not counted in refDist, so we add them here
    #    
    if(!amNoDataCheck(catToClose)){

      for(c in catToClose){
        distm <- dfDistFromTo[
          dfDistFromTo$cat == c,
          'dist'
          ]
        refDistToClose <- data.frame(
          cat_to = c,
          cat = idFrom,
          km = distm
        )
        refDist <- rbind(refDist,refDistToClose)
      }
    }

    #
    # Convert distances
    #
    if(!unitDist=='m'){
      div<-switch(unitDist,
        'km' = 1000
      )
      refDist[,unitDist]<-refDist[,unitDist]/div
    }
    refDist[,unitDist] <- round(refDist[,unitDist],4)
    
    #
    # Merge dist and time
    #

    refDistTime <- merge(
      refDist
      , refTime
      , by = c( 'cat', 'cat_to' )
      , all.y=T
    )

  },
  error = function(e){
    warning(e)
    tblEmpty <- data.frame(NA,NA,NA,NA)
    names(tblEmpty) <- c(
      'cat_to',
      'cat_from',
      unitNetDist,
      unitNetCost
    )
    return(tblEmpty)
  })


  return(refDistTime)
}
