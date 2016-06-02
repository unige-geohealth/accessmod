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
#' @param totalPop within the max travel time. (TODO: this could be extracted from table pop by zone) 
#' @param maxCost Maximum cost allowed 
#' @param removeCapted Should this analysis remove capted population ?
#' @param vectCatch Should this analysis create a shapefile as output ?
#' @param dbCon Active SQLite connection 
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
  totalPop,
  maxCost,
  removeCapted = TRUE,
  vectCatch = TRUE,
  dbCon
  ){


  #
  # Case evaluation
  #

  # Example:
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
  # 
  #

  #
  # set variables 
  #
  # population residual, not covered by this facility
  outputPopResidual <- inputMapPopResidual 
  # retrieve the path to the catchment
  pathToCatchment <- file.path(tempdir(),paste0(outputCatchment,'.shp'))
  # travel time / cost map
  travelTime <- inputMapTravelTime 
  # limit of the travel time map used to create the vector catchment
  timeLimitVector <- 0
  # correlation between the population and the time zone
    # negative value = covered pop decrease with dist,
    # positive value = covered pop increase with dist
  corPopTime <- 0
  # popByZone inner ring
  pbzIn <- 0
  # popByZone outer ring
  pbzOut <- 0
  # capacity of the facility at start, will be updated with the capacity not used
  capacityResidual <- facilityCapacity
  # capacity realised
  capacityRealised <- 0
  # total pop in catchment area
  popCatchment <- 0
  # total pop in maximum travel time area
  popTravelTimeMax <- 0
  # population of the first zone with at least one indivual
  popTravelTimeMin <- 0
  # percent of the initial population not in population residual
  popCoveredPercent <- 0
  # population in the catchment not covered (outer ring residual)
  popNotIncluded <- 0


  # If pop by zone is not given, extract it
  if(is.null(inputTablePopByZone)){
  #
  # compute integer version of cumulative cost map to use with r.univar
  #
  exprIntCost <- sprintf("%1$s = int( %1$s )",travelTime)
  execGRASS('r.mapcalc',expression=exprIntCost,flags='overwrite')
  #
  # compute zonal statistic : time isoline as zone
  #
  pbz <- read.table(
    text=execGRASS(
      'r.univar',
      flags  = c('g','t','overwrite'),
      map    = outputPopResidual,
      zones  = travelTime,
      intern = T
      ),sep='|',header=T)

  pbz$cumSum <- cumsum(pbz$sum)

  pbz <- pbz[c('zone','sum','cumSum')]

  }else{
    pbz <- inputTablePopByZone
  }




  # check if whe actually have zone
  isEmpty <- isTRUE( nrow(pbz) == 0 )


  #
  # get stat
  #

  if( !isEmpty ){
    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    popTravelTimeMax <- tail(pbz,n=1)$cumSum
    popTravelTimeMin <- head(pbz,n=1)$cumSum
    # Check time vs pop correlation :
    corPopTime <- cor(pbz$zone,pbz$sum)

    # get key zones
    pbzIn <-  pbz[ pbz$cumSum <= facilityCapacity, ] %>% tail(1)
    pbzOut <- pbz[ pbz$cumSum >  facilityCapacity, ] %>% head(1)
  }



  if( !isEmpty ) {

    #
    # Set main logic
    #

    # test for D
    isD <- isTRUE( pbzIn$cumSum == facilityCapacity )
    # test for C
    isC <- isTRUE( nrow(pbzOut) == 0 && nrow(pbzIn) > 0 && !isD ) 
    # test for B
    isB <- isTRUE( nrow(pbzIn) == 0 && nrow(pbzOut) > 0 ) 
    # test for A
    isA <- isTRUE( nrow(pbzIn) > 0 && nrow(pbzOut) > 0 ) 

    # vector catchment time limit
    if( isA || isB ){
      #
      # Set pop to remove in outer ring and catchment limit
      #
      timeLimitVector <- pbzOut$zone
      popCovered <- ifelse(isTRUE(pbzIn$cumSum>0),pbzIn$cumSum,0)
      propToRemove <-  (facilityCapacity - popCovered) / pbzOut$sum
    }

    if( isD ){
      timeLimitVector <- pbzIn$zone
      propToRemove <- 0 
    }

    # set limit for case C
    if( isC ){
      capacityResidual  <- facilityCapacity - pbzIn$cumSum
      timeLimitVector   <- maxCost 
      propToRemove      <- 0 
    }else{
      capacityResidual  <- 0
    }


    if( isA || isB || isC || isD ){
      # get other value to return
      capacityRealised <- facilityCapacity - capacityResidual
      popCatchment <- max(pbz[pbz$zone<=timeLimitVector,"cumSum"])
      popNotIncluded <- round( popCatchment - capacityRealised, 6)
    }


    if(removeCapted){
      if( !isB ){
        #
        # Remove pop from inner zone
        #
        # isnull handle null and &&& ignore null
        expInner <- sprintf("%1$s = if(!isnull(%2$s) &&& %2$s <= %3$s, 0, %4$s )",
          outputPopResidual,
          inputMapTravelTime,
          pbzIn$zone,
          inputMapPopResidual
          )
        execGRASS('r.mapcalc',expression=expInner,flags='overwrite')
      }

      if( isA || isB ){
        #
        # Remove prop in outer zone
        #
        expOuter <- sprintf("%1$s = if(!isnull(%2$s) &&& %2$s == %3$s,  %4$s - %4$s * %5$s , %4$s) ",
          outputPopResidual,
          inputMapTravelTime,
          pbzOut$zone,
          inputMapPopResidual,
          propToRemove
          )

        execGRASS('r.mapcalc',expression=expOuter,flags='overwrite')
      }
    }


    if(vectCatch){
      #
      # Extract the catchment as vector
      #
      execGRASS('r.mask',
        raster = inputMapTravelTime,
        maskcats = sprintf("0 thru %s",timeLimitVector),
        flags=c('overwrite')
        )
      # Catchment additional attributes
      aCols = list()
      aCols[facilityIndexField] = facilityId
      aCols[facilityNameField] = facilityName
      # extraction process
      amRasterToShape(
        pathToCatchment   = pathToCatchment,
        idField           = facilityIndexField,
        idPos             = facilityId,
        incPos            = iterationNumber,
        inputRaster       = inputMapTravelTime,
        outputShape       = outputCatchment,
        listColumnsValues = aCols,
        dbCon             = dbCon
        )
      # mask remove
      execGRASS('r.mask',
        flags="r"
        )
    }



  }

  #
  # population coverage analysis.
  #
  if(removeCapted){
    popCoveredPercent <- 
      amGetRasterPercent(outputPopResidual,inputMapPopInit)
  }
  #
  # Output capacity table
  #
  tblOut <- list(
    amId                  = facilityId,
    amRankComputed        = iterationNumber,
    amName                = facilityName,
    amTravelTimeMax       = maxCost,
    amPopTravelTimeMax    = popTravelTimeMax,
    amCorrPopTime         = corPopTime,
    amLabel               = facilityLabel,
    amCapacity            = facilityCapacity,
    amTravelTimeCatchment = timeLimitVector,
    amPopCatchmentTotal   = popCatchment,
    amCapacityRealised    = capacityRealised,
    amCapacityResidual    = capacityResidual,
    amPopCatchmentDiff    = popNotIncluded,
    amPopCoveredPercent   = popCoveredPercent
    )

  #
  # renaming table
  #
  names(tblOut)[names(tblOut)=="amId"] <- facilityIndexField
  names(tblOut)[names(tblOut)=="amName"] <- facilityNameField
  names(tblOut)[names(tblOut)=="amCapacity"] <- facilityCapacityField
  names(tblOut)[names(tblOut)=="amLabel"] <- facilityLabelField
  
  tblOut <- tblOut[!sapply(tblOut,amNoDataCheck)]

  # result list

  msg <- sprintf("Extraction of the catchment for facility %1$s done. %2$s %% of the population is covered. ",
    iterationNumber,
    round(popCoveredPercent,4)
    )

  list(
    amCatchmentFilePath=pathToCatchment,
    amCapacitySummary=as.data.frame(tblOut),
    msg = msg
    )

}

