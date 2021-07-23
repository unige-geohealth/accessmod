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


#' Set inner ring 
#' 
#' set residual pop map to zero at given value of travelTime 
#' 
#' @param inputMapPopResidual Residual population map tu update 
#' @param inputMapTravelTime Travel time map 
#' @param lowerOrEqualToZone Time limit
amInnerRing <- function(inputMapTravelTime,inputMapPopResidual,lowerOrEqualToZone=0, value=0){
  expInner <- sprintf(
    "%1$s = if( !isnull(%2$s) &&& %2$s <= %3$s, %4$s, %1$s )",
    inputMapPopResidual,
    inputMapTravelTime,
    lowerOrEqualToZone,
    value
  )
  if(amNoDataCheck(expInner)){
    stop('amInnerRing issue, empty expression!')
  }
  execGRASS('r.mapcalc',
    expression = expInner,
    flags = 'overwrite'
  )
}

#' Set outer ring 
#' 
#' Reduce population at given travel time iso band 
#' 
#' @param inputMapPopResidual Residual population map tu update 
#' @param inputMapTravelTime Travel time map 
#' @param lowerOrEqualToZone Time limit
amOuterRing <- function(inputMapTravelTime,inputMapPopResidual,propToRemove=0,zone=0){
  expOuter <- sprintf(
    "%1$s = if( !isnull(%2$s) &&& %2$s == %4$s,  %1$s - %1$s * %3$s, %1$s )",
    inputMapPopResidual,
    inputMapTravelTime,
    propToRemove,
    zone
  )
 if(amNoDataCheck(expOuter)){
    stop('amOuterRing issue, empty expression!')
  }
  execGRASS('r.mapcalc',
    expression = expOuter,
    flags = 'overwrite'
  )
}


#' Compute catchment from a table of cumulated population by cumulated cost map
#' @param inputTablePopByZone Table containing at least zone, sum, and cumSum columns from an zonal analysis between an isotropic or anisotropic cumulative cost layer (travel time) and a population layer.
#' @param inputMapPopInit Name of the layer containing the original population
#' @param inputMapPopResidual Name of the layer containing the residual population (could be the original population but this layer will be modified)
#' @param inputMapTravelTime Name of the layer containing the travel time
#' @param outputCatchment Name of the layer for the output vector catchment
#' @param facilityId Id of the facility analysed
#' @param facilityIndexField Name of the field/column of containing Id's
#' @param facilityName Name of the facility
#' @param facilityNameField Name of field/column containing the facilities name
#' @param facilityCapacity Capacity of the facility
#' @param facilityCapacityField Name of the column containint capacities
#' @param facilityLabel (optional) Label describing the capacity
#' @param facilityLabelField (optional) Name of the column for the label describing the capacity
#' @param iterationNumber Number (integer) of the iteration currently processed. Is used to determine if the shapefile in output should be overwrite or if we append the geometry to it
#' @param maxCost Maximum cost allowed 
#' @param ignoreCapacity Ignore capacity, use maximum population.
#' @param removeCapted Should this analysis remove capted population ?
#' @param vectCatch Should this analysis create a shapefile as output ?
#' @return A named list Containing the capacity analysis (amCapacityTable), the path to the shapefile (amCatchmentFilePath) and a message (msg). 
#' @export 
amCatchmentAnalyst <- function(
  inputTablePopByZone = NULL,
  inputMapTravelTime,
  inputMapPopInit,
  inputMapPopResidual,
  outputCatchment,
  facilityId,
  facilityIndexField,
  facilityName,     
  facilityNameField,    
  facilityCapacity,
  facilityCapacityField,
  facilityLabel = NULL,
  facilityLabelField,
  iterationNumber,
  maxCost,
  ignoreCapacity = FALSE,
  addColumnPopOrigTravelTime = FALSE,
  removeCapted = TRUE,
  vectCatch = TRUE,
  language = config$language
  ){


  #
  # Check input before going further
  #
  if(!ignoreCapacity && amNoDataCheck(facilityCapacity)){
    stop(sprintf(ams('analysis_catchment_error_capacity_not_valid'),facilityId))
  }


  #
  # Init variables 
  #
  
  # population residual, not covered by this facility
  outputMapPopResidualPatch <- 'tmp__map_residual_patch'
  outputMapPopResidual <- inputMapPopResidual

  # retrieve the path to the catchment
  pathToCatchment <- file.path(tempdir(),paste0(outputCatchment,'.shp'))

  # travel time / cost map
  travelTime <- inputMapTravelTime 

  # limit of the travel time map used to create the vector catchment
  timeLimitVector <- as.numeric(NA)

  # correlation between the population and the time zone
  #   negative value = covered pop decrease with dist,
  #   positive value = covered pop increase with dist
  corPopTime <- as.numeric(NA)

  # popByZone inner ring
  pbzIn <- as.numeric(NA)

  # popByZone outer ring
  pbzOut <- as.numeric(NA)

  # capacity of the facility at start, will be updated with the capacity not used
  capacityResidual <- facilityCapacity

  # capacity realised
  capacityRealised <- as.numeric(NA)

  # total pop in catchment area
  popCatchment <- as.numeric(NA)

  # total pop in maximum travel time area
  popTravelTimeMax <- as.numeric(NA)

  # population of the first zone with at least one indivual
  popTravelTimeMin <- as.numeric(NA)

  # total pop in maximum travel time area with original population
  popOrigTravelTimeMax <- as.numeric(NA)

  # percent of the initial population not in population residual
  popCoveredPercent <- as.numeric(NA)

  # population in the catchment not covered (outer ring residual)
  popNotIncluded <- as.numeric(NA)

  # other pop reporting 
  popResidualBefore <- as.numeric(NA)
  popResidualAfter <- as.numeric(NA)

  # population by zone is empty
  isEmpty <- TRUE

  # If pop by zone is not given, extract it
  if(is.null(inputTablePopByZone)){
    pbz <- amGetRasterStatZonal(
        mapZones = inputMapTravelTime,
        mapValues = inputMapPopResidual
    )
  }else{
    pbz <- inputTablePopByZone
  }

  #
  # Total pop under travel time with original population
  #
  if(addColumnPopOrigTravelTime){
    tryCatch(
      finally = {
        #
        # mask remove
        #
        hasMask <- amRastExists('MASK')
        if(hasMask){
          execGRASS('r.mask',
            flags="r"
          )
        }
      },
      {
        #
        # Set a mask to extract catchment
        #
        execGRASS('r.mask',
          raster   = inputMapTravelTime
        )
        popOrigTravelTimeMax <- amGetRasterStat(inputMapPopInit, 'sum');
      })
  }

  # check if whe actually have zone
  isEmpty <- isTRUE( nrow(pbz) == 0 )

  # starting population

  popTotal <- amGetRasterStat_cached(inputMapPopInit,"sum")
  popTotalNotOnBarrier <- amGetRasterStat_cached(inputMapPopResidual,"sum")
  popResidualBefore <- amGetRasterStat(inputMapPopResidual,"sum")
  #
  # get stat
  #
  if( !isEmpty ){
    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    popTravelTimeMax <- tail(pbz,n=1)$cumSum
    #popTravelTimeMin <- head(pbz,n=1)$cumSum
    popTravelTimeMin <- head(pbz[pbz$cumSum>0,],n=1)$cumSum

    # if ignore capacity, use all
    if(ignoreCapacity){
      facilityCapacity <- popTravelTimeMax 
      capacityResidual <- popTravelTimeMax 
    }

    # Check time vs pop correlation :
    corPopTime <- cor(pbz$zone,pbz$sum)

    #
    # Last iso band where pop is lower or equal the capacity
    #
    pbzIn <-  pbz[ pbz$cumSum <= facilityCapacity, ] %>% tail(1)
    if(amNoDataCheck(pbzIn)){
      #
      # Capacity is lower than pop in first zone
      # popInner is zero in the zoneInner ( first zone )
      #
      popInner <- 0
      zoneInner <- head(pbz,n=1)$zone
    }else{
      popInner <- pbzIn$cumSum
      zoneInner <- pbzIn$zone
    }
    
    
    #
    # First iso bad where pop is greater than the capacity
    #
    pbzOut <- pbz[ pbz$cumSum >  facilityCapacity, ] %>% head(1)
    popOuter <- pbzOut$cumSum
    popOuterBand <- pbzOut$sum 
    zoneOuter <- pbzOut$zone


    # Case evaluation. 
    # e.g.
    # Capacity = 10 (pop)
    # Time limit = 4 (zone)

    #     A         | B           | C         | D
    #     z  p      | z  p        | z  p      | z  p
    #     ----      | ----        | ----      | ----
    #     0 1       | 0 11 ¯ 0;10 | 0 1       | 0 2
    #     1 3       | 1 13        | 1 3       | 1 4
    #     2 5       | 2 20        | 2 4       | 2 9
    #     3 9 _ 9;1 | 3 25        | 3 5       | 3 10 -- 10;0
    #     4 15      | 4 50        | 4 6 _ 6;0 | 4 11
    #  

    #   case       | A   | B   | C  | D
    #   -------------------------------- 
    #   inner      | 9   | 0   | 6  | 10
    #   outer      | 1   | 10  | 0  | 0
    #   -------------------------------- 
    #   residual   | 0   | 0   | 4  | 0
    #   catchment  | out | out | in | in
    #   pop remove |  T  |  F  | T  | T
    #   out ratio  |  T  |  T  | F  | F

    # ignore capacity
    isE <- isTRUE(ignoreCapacity)

    # D
    # Inner catchment match the facility capacity
    isD <- !isE && isTRUE( facilityCapacity == popInner  )

    # C 
    # Capacity greater than max available pop  
    isC <- !isE && isTRUE( facilityCapacity > popTravelTimeMax ) 

    # test for B
    isB <- !isE && isTRUE( facilityCapacity < popTravelTimeMin ) 

    # test for A
    isA <- !isE && isTRUE( popInner > 0 && popOuter > 0 ) 

    type <- NULL

    if(isE){
      type <- "E"

      timeLimitVector  <- maxCost
      facilityCapacity <- popTravelTimeMax 
      capacityResidual <- 0 

      if(removeCapted){
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
      }


    }else if(isD){
      type <- "D"

      capacityResidual <-  0
      timeLimitVector  <- zoneInner

      if(removeCapted){
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
      }

    }else if(isC){
      type <- "C"

      capacityResidual <- facilityCapacity - popTravelTimeMax
      timeLimitVector  <- zoneInner

      if(removeCapted){
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )
      }

    }else if(isB){
      type <- "B"

      capacityResidual <- 0
      timeLimitVector  <- zoneOuter

      if(removeCapted){
        amOuterRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          propToRemove = ( facilityCapacity - popInner ) / popOuterBand,
          zone = zoneOuter
        )
      }

    }else if(isA){
      type <- "A"

      capacityResidual <- 0
      timeLimitVector  <- zoneOuter


      if(removeCapted){
        amInnerRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          lowerOrEqualToZone = zoneInner
        )

        amOuterRing(
          inputMapTravelTime = inputMapTravelTime,
          inputMapPopResidual = inputMapPopResidual,
          propToRemove = ( facilityCapacity - popInner ) / popOuterBand,
          zone = zoneOuter
        )
      }

    }else{
      amMsg(
        type = 'warning',
        text = paste('amCatchmentAnalyst. Catchment type not found.',
          'facilityId:',facilityId,
          'facilityCapacity:',facilityCapacity,
          'popInner:',popInner,
          'popOuter:', popOuter,
          'popTravelTimeMin:', popTravelTimeMin,
          'popTravelTimeMax:',popTravelTimeMax
        )
      )
    }

    #cat("Type", type, " | ", "id", facilityId, "\n" );

    #
    # get other value to return
    #
    capacityRealised <- facilityCapacity - capacityResidual
    popCatchment <- max(pbz[pbz$zone <= timeLimitVector,"cumSum"])
    popNotIncluded <- round( popCatchment - capacityRealised, 6)

    if(vectCatch){
      #
      # Extract the catchment as vector
      #
      tryCatch(
        finally = {
          #
          # mask remove
          #
          hasMask <- amRastExists('MASK')
          if(hasMask){
            execGRASS('r.mask',
              flags="r"
            )
          }
        },
        {
          #
          # Set a mask to extract catchment
          #
          execGRASS('r.mask',
            raster   = inputMapTravelTime,
            maskcats = sprintf("0 thru %s", timeLimitVector),
            flags    = c('overwrite')
          )
          #
          # Catchment additional attributes
          #
          aCols <- list()
          aCols[facilityIndexField] <- facilityId
          aCols[facilityNameField] <- facilityName
          aCols['type'] <- type
          
          #
          # extraction process
          #
          amRasterToShape(
            pathToCatchment   = pathToCatchment,
            idField           = facilityIndexField,
            idPos             = facilityId,
            incPos            = iterationNumber,
            inputRaster       = inputMapTravelTime,
            outputShape       = outputCatchment,
            listColumnsValues = aCols
          )
        })
    }

  }

  # Population covered
  #
  # ( 1346 tot - 0 residual ) / 1346 => 100% coverage 
  # ( 1346 tot - 673 residual ) / 1346 => 50% coverage
  #
  popResidualAfter <- amGetRasterStat(inputMapPopResidual,"sum")
  popCoveredPercent <- ( popTotalNotOnBarrier - popResidualAfter ) / popTotalNotOnBarrier * 100

  #
  # Output capacity table
  #
  outList <- list(
    amId                     = facilityId,
    amOrderComputed          = iterationNumber,
    amName                   = facilityName,
    amTravelTimeMax          = maxCost,
    amPopTravelTimeMax       = popTravelTimeMax,
    amCorrPopTime            = corPopTime,
    amLabel                  = facilityLabel,
    amCapacity               = facilityCapacity,
    amTravelTimeCatchment    = timeLimitVector,
    amPopCatchmentTotal      = popCatchment,
    amCapacityRealised       = capacityRealised,
    amCapacityResidual       = capacityResidual,
    amPopCatchmentDiff       = popNotIncluded,
    amPopCoveredPercent      = popCoveredPercent,
    amPopTotal               = popTotal,
    amPopTotalNotOnBarrier   = popTotalNotOnBarrier,
    amPopResidualAfter       = popResidualAfter,
    amPopResidualBefore      = popResidualBefore
  )

  #
  # renaming table
  #
  names(outList)[names(outList)=="amId"] <- facilityIndexField
  names(outList)[names(outList)=="amName"] <- facilityNameField
  names(outList)[names(outList)=="amLabel"] <- facilityLabelField

  if(!ignoreCapacity){
    names(outList)[names(outList)=="amCapacity"] <- facilityCapacityField
  }

  outList <- outList[!sapply(outList,is.null)]

  if(addColumnPopOrigTravelTime){
    outList$amPopOrigTravelTimeMax <- popOrigTravelTimeMax
  }

  #
  # Result message
  #
  msg <- sprintf(
    ams("analysis_catchment_result_msg"),
    iterationNumber,
    round(popCoveredPercent,4)
  )

  list(
    amCatchmentFilePath = pathToCatchment,
    amCapacitySummary = as.data.frame(outList),
    msg = msg
  )

}

