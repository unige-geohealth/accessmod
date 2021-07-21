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

amTimeDist <- function( job  ){

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
  nCores          = job$nCores
  tmpMapset       = "tmp_mapset_not_set"
  keepNetDist     = job$keepNetDist
  keepNetDistPath = job$keepNetDistPath

  #
  # Main script
  #
  tryCatch(
    finally = {
      amMapsetRemove(tmpMapset,stringCheck='^tmp_')
    },
    {


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
      # Ref time dist to itself
      #
      sameFromTo <- identical(inputHfFrom,inputHfTo)


      #
      # Init temporary mapset
      #
      tmpMapset <- amRandomName("tmp_mapset")
      amMapsetInit(origProject,tmpMapset)

      #
      # Temporary layers
      #
      tmpVector <- list(
        selectFrom      = amRandomName("tmp__ref_from"),
        selectTo        = amRandomName("tmp__ref_to"),
        netFrom         = amRandomName("tmp__net_from"),
        netAll          = amRandomName("tmp__net_all"),
        netDist         = amRandomName("tmp__net_dist"),
        netDistAll      = "tmp__net_dist_all"
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
          ratioMemory      = 1/nCores,
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
          ratioMemory      = 1/nCores,
          rawMode          = TRUE # don't convert to minute, do not remove value above max cost
        )
      )

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
      # extract distance
      #
      if( hasNoDest ){

        #
        # Use refTime as template for distances
        #
        refDist <- refTime
        names(refDist)[names(refDist) == unitCost] <- unitDist

      }else{

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
          closestHf <- refTime[minPos,'cat_to']

          # 
          # Select all values that are not the closest
          # and remove theme from the layer
          #
          qSqlTo <- sprintf("cat not in (%s) "
            , paste(closestHf,collapse=",")
          )

          execGRASS(
            "v.edit",
            map   = tmpVector$selectTo,
            tool  = "delete",
            where = qSqlTo
          )
        }

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
          tmpVectOut <- sprintf('%1$s_%2$s.rds',tmpVector$netDist,idFrom)
          pathVectOut <- file.path(keepNetDistPath,tmpVectOut)
          tblFeaturesCount <- amGetTableFeaturesCount(tmpVector$netDist)
          isNetEmpty <- tblFeaturesCount[tblFeaturesCount$type=='lines',]$count == 0
          
          if(!isNetEmpty){
            spNetDist <- readVECT(tmpVector$netDist,
              type = 'line', 
              driver = "ESRI Shapefile"
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
            # Using independant RDS file as write can be done
            # at any time in paralel mode. Append them outside paralel loop
            #
            saveRDS(spNetDist,pathVectOut)
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
        refDist[,unitDist] <- round(refDist[,unitDist],3)
      }

      #
      # Merge dist and time
      #

      refDistTime <- merge(
        refDist
        , refTime
        , by = c( 'cat', 'cat_to' )
        , all.y=T
      )

    })

  return(refDistTime)
}
