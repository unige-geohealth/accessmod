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
#' @param iterationNumber Number (integer) of the iteration currently processed. Is used to determine if the shapefile in output should be overwrite or if we append the geometry to it
#' @param totalPop within the max travel time. (TODO: this could be extracted from table pop by zone) 
#' @param maxCost Maximum cost allowed 
#' @param removeCapted Should this analysis remove capted population ?
#' @param vectCatch Should this analysis create a shapefile as output ?
#' @param dbCon Active SQLite connection 
#' @return A named list Containing the capacity analysis (amCapacityTable), the path to the shapefile (amCatchmentFilePath) and a message (msg). 
#' @export 
amCatchmentAnalyst <- function(
  inputTablePopByZone,
  inputMapPopInit,
  inputMapPopResidual,
  inputMapTravelTime,
  outputCatchment,
  facilityId,
  facilityIndexField,
  facilityName,     
  facilityNameField,    
  facilityCapacity,
  facilityCapacityField,
  facilityLabel,         
  facilityLabelField,
  iterationNumber,
  totalPop,
  maxCost,
  removeCapted=TRUE,
  vectCatch=TRUE,
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
  # Shortcut
  #
  pbz <- inputTablePopByZone
  outputPopResidual <- inputMapPopResidual
  popCoveredPercent <- NA
  corPopTime <- NA    
  capacityResidual <- facilityCapacity
  pathToCatchment <- file.path(tempdir(),paste0(outputCatchment,'.shp'))


  # set init values
  corPopTime <- cor(pbz$zone,pbz$sum)
  pbzFirst <- pbz %>% head(1)
  pbzIn <-  pbz[ pbz$cumSum <= facilityCapacity, ] %>% tail(1)
  pbzOut <- pbz[ pbz$cumSum >  facilityCapacity, ] %>% head(1)


  isEmpty <- isTRUE( nrow(pbz) ==0 )


  if( !isEmpty ) {
    # test for D
    isD <- isTRUE( pbzIn$cumSum == facilityCapacity )
    # test for C
    isC <- isTRUE( nrow(pbzOut) == 0 && nrow(pbzIn) > 0 && !isD ) 
    # test for B
    isB <- isTRUE( nrow(pbzIn) == 0 && nrow(pbzOut) > 0 ) 
    # test for A
    isA <- isTRUE( nrow(pbzIn) > 0 && nrow(pbzOut) > 0 ) 

    if(!sum(c(isC,isD,isA,isB)) == 1){
      stop("amCatchmentAnalyst encountered an unexpected case for facility id '%s' ", facilityId )
    }

   
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



    if(removeCapted){
      if( !isB ){
        #
        # Remove pop from inner zone
        #
        # isnull handle null and &&& ignore null
        expInner <- sprintf("%1$s = if(!isnull(%2$s) &&& %2$s <= %3$s, null(), %4$s )",
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
        maskcats = sprintf("1 thru %s",timeLimitVector),
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

    #
    # population coverage analysis.
    #
    if(removeCapted){
      popCoveredPercent <- amGetRasterPercent(outputPopResidual,inputMapPopInit)
    }else{
      popCoveredPercent <- 0
    }

  }

  #
  # Output capacity table
  #
  capacityDf=data.frame(
    facilityId, 
    facilityName, 
    facilityCapacity, # capacity from hf table
    facilityLabel,
    iterationNumber, # processing order position
    corPopTime, # corrrelation (pearson) between time (zone) and population (sum)
    capacityResidual, # capacity not filled
    facilityCapacity-capacityResidual,# capacity realised
    maxCost, # max allowed travel time (time)
    ifelse(length(totalPop)==0,0,totalPop), # total population within max time (minutes)
    popCoveredPercent # if covered pop removed, percent of total.
  )


  # renaming table
  # TODO: set all names in config file
  names(capacityDf)<-c(
    facilityIndexField,
    facilityNameField,
    facilityCapacityField,
    facilityLabelField,
    'amProcessingRank',
    'amCorrPopTime',
    'amCapacityResidual',
    'amCapacityRealised',
    'amTimeMax',
    'amPopTimeMax',
    'amPopCoveredPercent'
    )


  # result list

  msg <- sprintf("Extraction of the catchment for candidate %1$s done. %2$s %% of the population is covered. ",
    iterationNumber,
    round(popCoveredPercent,4)
    )

  list(
    amCatchmentFilePath=pathToCatchment,
    amCapacityTable=capacityDf,
    msg = msg
    )

}

