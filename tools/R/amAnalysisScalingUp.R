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

#
#
#' Create new hf based on exclusion rules and multicriteria map
#' @export
amAnalysisScalingUp <- function(
  inputMerged,
  inputPop, # name of input  population
  inputPopResidual, # name of input population or population residual
  inputFacility, # name of input facilities layer.

  outputFriction, # base friction map
  outputSpeed, # base speed map
  outputFacility, # name of the output facilities layer
  outputPopResidual, # name of the output residual pop layer
  outputCatchment, # name of the output catchment layer
  outputCapacityAnalysis, # name of the output capacity analysis

  tableCapacity, # table of capacities
  tableExclusion, # table of exclusion layers
  tableSuitability, # table of suitability index items
  tableScenario, # Speed / model table
  tableFacilities, # facilities table

  # Imported from table
  # idHfSelect, # selected id of input facilities



  maxTravelTime, # maximum travel time
  # maxSpeed = 0, # maximum speed to build process radius
  useMaxSpeedMask = FALSE,
  useExistingFacilities, # boolean import existing facilities
  # Not used ?
  # facilityIndexField, # existing facility index field
  # facilityCapacityField, # existing facility capacity field
  # facilityNameField, # existing facility name field

  typeAnalysis, # type of analysis : iso or anisotropic
  knightMove = FALSE,
  limitFacilitiesNumber, # max number of facilities
  limitProcessingTime, # maximum processing time
  limitPopCoveragePercent, # maximum population coverage in percent
  roundingMethod = c("ceil", "round", "floor"),
  pBarTitle,
  language = config$language
) {
  roundingMethod <- match.arg(roundingMethod)

  #
  # Stop if invoked outside of session
  #
  amGrassSessionStopIfInvalid()

  #
  # Local db connection
  #
  dbCon <- amMapsetGetDbCon()
  on_exit_add({
    dbDisconnect(dbCon)
  })

  #
  # Set maxSpeed
  #
  maxSpeed <- ifelse(isTRUE(useMaxSpeedMask), max(tableScenario$speed), 0)

  #
  # Extract facilities id to import
  #
  idHfSelect <- tableFacilities[
    tableFacilities$amSelect,
    config$vectorKey
  ]


  #
  # Create both speed / friction
  #
  amCreateSpeedMap(
    tableScenario,
    inputMerged,
    outputSpeed
  )
  amCreateFrictionMap(
    tableScenario,
    inputMerged,
    outputFriction,
    mapResol = gmeta()$nsres
  )

  #
  # Clean / format exclusion table
  #
  tableExclusion$layer <- as.character(tableExclusion$layer)
  # Replace dynamic facility name by given layer name
  tableExclusion$layer[
    tableExclusion$layer == config$dynamicFacilities
  ] <- outputFacility

  # Get type (raster or vector) for each layer.
  if (nrow(tableExclusion) > 0) {
    exclType <- sapply(tableExclusion$layer, function(x) {
      amGetType(x)
    })
  } else {
    exclType <- character(0)
  }
  tableExclusion$type <- exclType

  #
  # Clean / format suitability table
  #

  tableSuitability$layer <- as.character(tableSuitability$layer)
  # replace temp new facility name by actual new layer
  tableSuitability$layer[
    tableSuitability$layer == config$dynamicFacilities
  ] <- outputFacility
  # replace temp pop name by actual new layer name
  tableSuitability$layer[
    tableSuitability$layer == config$dynamicPopulation
  ] <- outputPopResidual
  # Get type (raster or vector) for each layer.
  tableSuitability$type <- sapply(
    tableSuitability$layer,
    function(x) {
      amGetType(x)
    }
  )


  #
  # Initialisation
  #
  # clean mask and set message
  pbc(
    visible = TRUE,
    percent = 0,
    title = pBarTitle,
    text = ams(
      id = "analysis_scaleup_initialisation_message"
    )
  )

  rmRastIfExists("MASK")

  on_exit_add({
    rmVectIfExists("tmp__*")
    rmRastIfExists("tmp__*")
  })

  # Keep initial state of arguments
  argInit <- as.list(environment())

  # Remove db and r6 classes (not parsable as json)
  rmDbClass <- function(x) {
    !any(class(x) %in% c("SQLiteConnection", "R6"))
  }
  argInit <- argInit[
    sapply(argInit, rmDbClass)
  ]

  # structure of the results list
  result <- list(
    timing = list(),
    arguments = argInit,
    statistic = list(
      population = list()
    )
  )


  # set limits
  if (isTRUE(
    limitFacilitiesNumber < 1 ||
      limitFacilitiesNumber > 1e6
  )
  ) {
    limitFacilitiesNumber <- 1e6
  }

  if (isTRUE(
    limitProcessingTime < 1 ||
      limitProcessingTime > 1e6
  )
  ) {
    limitProcessingTime <- 1e6
  }

  if (isTRUE(
    limitPopCoveragePercent < 1 ||
      limitPopCoveragePercent > 99
  )
  ) {
    limitPopCoveragePercent <- 100
  }

  # Set progression variables
  progInit <- 0 # value in percent for the proggression bar
  progNum <- 0

  # Output vector containing new HF
  # NOTE: some columns will be present in shapefile : max 10 char
  # set index column
  facilityIndexField <- "amId"
  # set capacity field
  facilityCapacityField <- "amCapacity"
  # set facility field
  facilityNameField <- "amName"
  # set label field
  facilityLabelField <- "amLabel"

  # set covered pupulation at given time
  facilityPopTimeField <- sprintf(
    "amPop%sminScalingUp",
    as.integer(maxTravelTime)
  )

  #
  # temp layer
  #
  # Best candidates
  tmpBestCandidates <- amRandomName("tmp__best_candidates")
  # Population residual
  tmpCandidates <- amRandomName("tmp_candidates")
  tmpCatchment <- amRandomName("tmp__catchment")

  # Reevaluate suitability map at each iteration
  # Suitability map will be modified at least one of those
  # layer are given in table
  # - population density (population will be removed : change of density)
  # - generated facilities (new HF will be created)
  # or..
  # if exclusion area is modified : suitability range could change.
  redoSuitabilityMap <- TRUE

  listSummaryCatchment <- list()
  tableCapacityStat <- data.frame()
  #
  # Population stat and temporary layer creation
  #

  # Get initial percentage of population coverage.

  #
  # Population residual temporary
  #

  # If residual pop is used in suitability by density,
  # replace with temporary residual pop. This layer will change at each iteration.
  #  popLayers <- grep(
  #    inputPopResidual,
  #    tableSuitability$layer
  #    )
  #  tableSuitability[popLayers,'layer'] <- tmpPopRes

  # create population residual

  amInitPopResidual(
    inputPop = inputPop,
    inputPopResidual = inputPopResidual,
    inputFriction = outputFriction,
    inputSpeed = outputSpeed,
    outputPopResidual = outputPopResidual
  )

  #
  # Facilities output layer column name
  # (for the join with catchment and catchment summary)
  #
  newFieldsList <- list(
    c(facilityNameField, "character"),
    c(facilityIndexField, "integer"),
    c(facilityCapacityField, "integer"),
    c(facilityLabelField, "character")
  )

  #
  # Create output facility layer
  #


  # check if we have output facility item in tables
  hasFacilityExclusion <- outputFacility %in% tableExclusion$layer
  hasFacilitySuitability <- outputFacility %in% tableSuitability$layer



  # Use existing facilities double check (already done in validation...)
  useExistingFacility <- isTRUE(
    length(idHfSelect) > 0 &&
      amVectExists(inputFacility) &&
      useExistingFacilities
  )


  amScUpPop_createNewFacilityLayer(
    useExistingFacility = useExistingFacility,
    inputFacility = inputFacility,
    inputIdToImport = idHfSelect,
    inputHfIdx = facilityIndexField,
    inputHfName = facilityNameField,
    outputFacility = outputFacility,
    newColumnsDb = newFieldsList,
    dbCon = dbCon
  )


  # create initial candidate layer :
  # has value = 1
  # is null = 0
  # note : could be residual population
  # after each iteration, a mask based on exclusion table will be applied.

  amScalingUp_createCandidatesTemp(
    input = ifelse(typeAnalysis == "anisotropic", outputSpeed, outputFriction),
    output = tmpCandidates
  )

  # Get number of initial candidates
  candidateCountInit <- amGetRasterStat(tmpCandidates, "n")

  # Get number of rules used
  nRules <- nrow(tableExclusion) + nrow(tableSuitability)

  # Var to read at each iteration and stop the process if TRUE
  quit <- FALSE

  # Keep the starting time
  start <- Sys.time()

  # Iteration thru the number of facility requested
  for (i in 1:limitFacilitiesNumber) {
    if (!quit) {
      #
      # LOOP START
      #

      progNum <- progNum + 1

      hfName <- sprintf("facility_%1$s", i)
      hfId <- i
      nCandidates <- amGetRasterStat(tmpCandidates, "n")
      pCoverage <- amGetRasterPercent(outputPopResidual, inputPop)
      elapsedMinutes <- as.numeric(difftime(Sys.time(), start, units = "min"))


      #
      # Set progress bar percent
      #

      pBarMax <- max(
        c(
          pBarCov <- pCoverage / limitPopCoveragePercent * 100,
          pBarPercentFac <- progNum / limitFacilitiesNumber * 100,
          pBarPercentTim <- elapsedMinutes / limitProcessingTime * 100
        )
      )

      progInc <- (100 - progInit) / 100 # 10 percent are already lost in config step
      pBarPercent <- progInit + progInc * ifelse(pBarMax >= 100, 100, pBarMax)


      pbc(
        visible = TRUE,
        percent = pBarPercent,
        title = pBarTitle,
        text = sprintf(
          ams(
            id = "analysis_scaleup_population_coverage_percentage"
          ),
          round(pCoverage, 3)
        )
      )


      reachedPop <- isTRUE(pCoverage >= limitPopCoveragePercent)
      reachedTime <- isTRUE(elapsedMinutes >= limitProcessingTime)
      reachedCandidates <- isTRUE(nCandidates < 1)


      if (reachedPop || reachedTime || reachedCandidates) {
        quit <- TRUE

        if (reachedPop) {
          pbc(
            visible = TRUE,
            percent = 100,
            title = pBarTitle,
            text = sprintf(
              ams(
                id = "analysis_scaleup_population_coverage_reached"
              ),
              limitPopCoveragePercent
            )
          )
        }
        if (reachedTime) {
          pbc(
            visible = TRUE,
            percent = 100,
            title = pBarTitle,
            text = sprintf(
              ams(
                id = "analysis_scaleup_processing_time"
              ),
              limitProcessingTime
            )
          )
        }

        if (reachedCandidates) {
          pbc(
            visible = TRUE,
            percent = 100,
            title = pBarTitle,
            text = sprintf(
              ams(
                id = "analysis_scaleup_final_candidates"
              )
            )
          )
        }
      } else {
        pbc(
          visible = TRUE,
          percent = pBarPercent,
          title = pBarTitle,
          text = sprintf(
            ams(
              id = "analysis_scaleup_iteration_finding_candidates"
            ),
            progNum,
            nRules,
            nCandidates
          )
        )

        #
        # Search for the best candidate(s)
        #

        listEvalBest <- amScalingUp_findBestCells(
          inputFriction = outputFriction,
          inputSpeed = outputSpeed,
          tableExclusion = tableExclusion,
          tableSuitability = tableSuitability,
          inputCandidates = tmpCandidates,
          outputBestCandidates = tmpBestCandidates,
          candidateCountInit = candidateCountInit,
          roundingMethod = roundingMethod
        )


        if (!isTRUE(listEvalBest$noMoreCandidates)) {
          #
          # If there is still some candidates, continue
          #
          pbc(
            visible = TRUE,
            percent = pBarPercent,
            title   = pBarTitle,
            text    = listEvalBest$msg
          )
          #
          # Evaluate selected candidates
          #
          listEvalCoverage <- amScalingUp_evalCoverage(
            inputCandidates = tmpBestCandidates,
            inputSpeed = outputSpeed,
            inputFriction = outputFriction,
            inputPopulation = outputPopResidual,
            tableCapacity = tableCapacity,
            iterationNumber = progNum,
            typeAnalysis = typeAnalysis,
            knightMove = knightMove,
            maxTravelTime = maxTravelTime,
            maxSpeed = maxSpeed,
            maxFacilities = limitFacilitiesNumber,
            dbCon = dbCon,
            roundingMethod = roundingMethod,
            pBarTitle = pBarTitle,
            pBarPercent = pBarPercent
          )

          pbc(
            visible = TRUE,
            percent = pBarPercent,
            title = pBarTitle,
            text = sprintf(
              ams(
                id = "analysis_scaleup_evaluation_finished"
              )
            )
          )

          #
          # Select best
          #

          listEvalCoverageBest <- amScalingUp_extractBest(
            listEvalCoverage = listEvalCoverage,
            criteria         = "amPopTimeMax"
          )

          #
          # No population given the current paramenters
          # Could happen with slow speed and loose suitability
          #
          hasNoPopBest <- listEvalCoverageBest$amPopTimeMax < 1
          if (hasNoPopBest) {
            #
            # Remove cell from candaidates
            #
            amScalingUp_candidateExclude(
              inputCandidates = tmpCandidates,
              inputLayer = listEvalCoverageBest$amVectorPoint,
              inputLayerType = "vector",
              distance = 1,
              keep = c("keep_outside")
            )
          } else {
            #
            # Catchment creation.
            #

            listSummaryCatchment <- amCatchmentAnalyst(
              inputTablePopByZone = listEvalCoverageBest$tblPopByZone,
              inputMapPopInit = inputPop,
              inputMapPopResidual = outputPopResidual,
              inputMapTravelTime = listEvalCoverageBest$amRasterCumul,
              outputCatchment = tmpCatchment,
              facilityId = hfId,
              facilityIndexField = facilityIndexField,
              facilityName = hfName,
              facilityNameField = facilityNameField,
              facilityCapacity = listEvalCoverageBest$amCapacity,
              facilityCapacityField = facilityCapacityField,
              facilityLabel = listEvalCoverageBest$amLabel,
              facilityLabelField = facilityLabelField,
              maxTravelTime = listEvalCoverageBest$amTimeMax,
              iterationNumber = listEvalCoverageBest$amProcessingOrder,
              removeCapted = TRUE,
              vectCatch = TRUE
            )


            pbc(
              visible = TRUE,
              percent = pBarPercent,
              title   = pBarTitle,
              text    = listSummaryCatchment$msg
            )

            #
            # Populate or update output capacity table
            #

            if (i == 1) {
              tableCapacityStat <- listSummaryCatchment$amCapacitySummary
            } else {
              tableCapacityStat <- rbind(
                tableCapacityStat,
                listSummaryCatchment$amCapacitySummary
              )
            }

            #
            # Populate hf vector
            #

            amScalingUp_mergeNewHf(
              outputFacility           = outputFacility,
              newFacility              = listEvalCoverageBest$amVectorPoint,
              tableNewFacilityCapacity = listSummaryCatchment$amCapacitySummary,
              facilityIndexField       = facilityIndexField,
              facilityNameField        = facilityNameField,
              facilityLabelField       = facilityLabelField,
              facilityCapacityField    = facilityCapacityField,
              dbCon
            )
          }
        }

        rmVectIfExists("tmp__*")
        rmRastIfExists("tmp__*")
      }
    }
  }

  if (length(listSummaryCatchment) > 0) {
    #
    # export shapefile
    #
    amMoveShp(
      shpFile = listSummaryCatchment$amCatchmentFilePath,
      outDir = config$pathShapes,
      outName = outputCatchment
    )

    pbc(
      visible = TRUE,
      percent = 100,
      title = pBarTitle,
      text = ams(
        id = "analysis_scaleup_catchment_saved_message"
      )
    )
  }

  if (length(tableCapacityStat) > 0) {
    #
    # Write table capacity stat if there is at least one line
    #
    dbWriteTable(dbCon,
      outputCapacityAnalysis,
      tableCapacityStat,
      overwrite = T
    )
    pbc(
      visible = TRUE,
      percent = 100,
      title = pBarTitle,
      text = ams(
        id = "analysis_scaleup_table_saved_message"
      )
    )
  }

  #
  # End of the process
  #

  pbc(
    visible = FALSE,
    percent = 0,
    title   = "",
    text    = ""
  )
  return()
}


#' Merge generated facility to output file
#' @param outputFacility Name of the output facility layer
#' @param newFacility Name fo the new facility layer
#' @param tableNewFacilityCapacity Table with the new capacity information
#' @param facilityIndexField Name of the index field
#' @param facilityNameField Name of the name field
#' @param facilityLabelField Name of the label field
#' @param facilityCapacityField Name of the capacity field
#' @return
#' @export
amScalingUp_mergeNewHf <- function(outputFacility,
  newFacility,
  tableNewFacilityCapacity,
  facilityIndexField,
  facilityNameField,
  facilityLabelField,
  facilityCapacityField,
  dbCon,
  language = config$language) {
  #
  # salite does not allow renaming. With small dataset, we can do it manually
  #


  # get old values
  datOld <- sprintf("SELECT * FROM %s", outputFacility) %>%
    dbGetQuery(dbCon, .)

  # get new generated facility id
  catNew <- sprintf(
    "SELECT %1$s FROM %2$s",
    config$vectorKey,
    newFacility
  ) %>%
    dbGetQuery(dbCon, .)

  # create an empty row using columns from outputFacility
  datNew <- sprintf("SELECT * FROM %s limit 0 ", outputFacility) %>%
    dbGetQuery(dbCon, .)

  datNew[1, names(datNew)] <- NA
  datNew[1, config$vectorKey] <- as.integer(catNew)

  # get capacity found in catchemnt analysis
  datCap <- tableNewFacilityCapacity

  # set field to be present in table
  mergeFields <- c(
    facilityIndexField,
    facilityNameField,
    facilityCapacityField,
    facilityLabelField
  )
  # thw table of new facility should contain some of the field from capacity table.
  # Update those field with cap value
  datNew[mergeFields] <- datCap[mergeFields]

  # overwrite the table of the new facility
  dbWriteTable(dbCon,
    newFacility,
    datNew,
    overwrite = TRUE
  )

  # merge old facility with new one
  execGRASS("v.patch",
    input = newFacility,
    output = outputFacility,
    flags = c("overwrite", "a", "e")
  )
}









#' Create or import to new facility layer
#' Import existing facility or create an empty facility layer
#' @param useExistingFacilities Boolean use existing facility layer
#' @param inputFacility Name of the facility layer to import
#' @param inputIdToImport Filter given id. Eg. from input table
#' @param inputHfIdx Field name containing index to preserve
#' @param inputHfName Field name containing facility name to preserve
#' @param outputFacility Name of the facility layer to create
#' @param newColumnsGrass Fields name of the output layer.
#' @param dbCon Sqlite dbcon object
#' @return New facility map name
#' @export
amScUpPop_createNewFacilityLayer <- function(useExistingFacility = FALSE,
  inputFacility,
  inputIdToImport,
  inputHfIdx,
  inputHfName,
  outputFacility,
  newColumnsDb,
  dbCon) {
  #
  # If there is at least one facility to keep, extract them, else
  # create a new one
  #
  if (length(inputIdToImport) > 0 && useExistingFacility) {
    #
    # v.extract in amCreateLayerSubset can't work if input==output :
    # ->  make a copy if needed
    #
    if (isTRUE(inputFacility %in% outputFacility)) {
      inputFacilityTemp <- amRandomName("tmp")
      execGRASS(
        "g.copy",
        vector = c(inputFacility, inputFacilityTemp)
      )
      inputFacility <- inputFacilityTemp
    }

    #
    # Extract seletion
    #
    amCreateLayerSubset(
      input_vector = inputFacility,
      output_vector = outputFacility,
      id_list = inputIdToImport,
      keep = TRUE
    )
  } else {
    #
    # Create a vector and add an empty table entry with default key
    #
    amCreateEmptyVector(dbCon,
      mapName = outputFacility,
      indexName = config$vectorKey
    )
  }

  #
  # Extract full table to set type
  #
  hfTable <- dbReadTable(
    dbCon,
    outputFacility
  )

  colsName <- names(hfTable)

  #
  # Force types
  #
  for (colSet in newColumnsDb) {
    col <- colSet[[1]]
    colClass <- colSet[[2]]
    if (!col %in% colsName) {
      if (nrow(hfTable) == 0) {
        hfTable[, col] <- as(NULL, colClass)
      } else {
        hfTable[, col] <- as(NA, colClass)
      }
    } else {
      hfTable[, col] <- as(hfTable[, col], colClass)
    }
  }

  #
  # Replace the full table
  #
  dbWriteTable(dbCon,
    outputFacility,
    hfTable,
    overwrite = TRUE
  )

  return(outputFacility)
}


#' create coverage for each candidate
#' @param inputCandidates Name of candidates point vector  map
#' @param inputSpeed Name of speed raster map
#' @param inputPopulation Name of (residual) population map
#' @param tableCapacity Table of user defined capacity table
#' @param iterationNumber Integer of currently processed iteration
#' @param typeAnalysis Name of the analysis to use : anisotropic or isotropic
#' @param maxTravelTime Travel time max in minutes
#' @param maxSpeed Maximum speed used to limit analysis radius
#' @param maxFacilities Maximum facilities to process
#' @param pBarTitle Title of the progress bar
#' @param pBarPercent Initial progress bar percent
#' @param roundingMethod Rounding method for iso/anisotropic
#' @param dbCon Db sqite connection object
#' @return table containing summary and raster layer generated
#' @export
amScalingUp_evalCoverage <- function(
  inputCandidates,
  inputSpeed,
  inputFriction,
  inputPopulation, # in scaling up = pop residual
  tableCapacity,
  iterationNumber,
  typeAnalysis,
  knightMove = FALSE,
  maxTravelTime,
  maxSpeed,
  maxFacilities,
  pBarTitle,
  pBarPercent,
  dbCon,
  roundingMethod = c("ceil", "round", "floor"),
  language = config$language) {
  roundingMethod <- match.arg(roundingMethod)

  # output candidate evaluation
  candidatesEval <- list()

  # set iterration number
  i <- iterationNumber
  # Import candidates table
  exp <- sprintf(
    "SELECT %1$s FROM %2$s",
    config$vectorKey,
    inputCandidates
  )
  candidatesTable <- dbGetQuery(dbCon, exp)
  # get number of candidate to evaluate
  nCandidates <- nrow(candidatesTable)
  # if none, stop.
  # internal iteration
  pIter <- 0

  #
  # Evaluate each candidate coverage individually
  #

  # for each candidates, extract a population coverage.
  for (j in candidatesTable[[config$vectorKey]]) {
    pIter <- pIter + 1

    #
    # progress bar
    #

    msg <- sprintf(
      ams(
        id = "analysis_scaleup_evaluate_candidate"
      ),
      pIter,
      nCandidates
    )

    pbc(
      visible = TRUE,
      percent = pBarPercent,
      title   = pBarTitle,
      text    = msg
    )


    #
    # Init
    #


    # set a candidate name
    candidateName <- sprintf(
      ams(
        id = "analysis_scaleup_rename_candidates"
      ), i, j
    )
    # vector point : one hf
    hfTest <- amRandomName("tmp__hf_new_", j)
    # raster cumul by hf
    hfTestCumul <- amRandomName("tmp__travel_time_", j)

    #
    # Extraction
    #

    # extract unique candidate at a time
    amCreateLayerSubset(
      input_vector = inputCandidates,
      output_vector = hfTest,
      id_list = j
    )

    #
    # Cumulative cost
    #

    # For this candidate, analyse cumulative cost map without mask
    switch(typeAnalysis,
      "anisotropic" = amAnisotropicTravelTime(
        inputSpeed = inputSpeed,
        inputHf = hfTest,
        outputTravelTime = hfTestCumul,
        towardsFacilities = TRUE,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        knightMove = knightMove,
        minTravelTime = NULL,
        timeoutValue = "null()",
        roundingMethod = roundingMethod
      ),
      "isotropic" = amIsotropicTravelTime(
        inputFriction = inputFriction,
        inputHf = hfTest,
        outputTravelTime = hfTestCumul,
        maxTravelTime = maxTravelTime,
        maxSpeed = maxSpeed,
        knightMove = knightMove,
        minTravelTime = NULL,
        timeoutValue = "null()",
        roundingMethod = roundingMethod
      )
    )

    #
    # Population by time isoline
    #

    # compute integer version of cumulative cost map to use with r.univar, by minutes

    exprTravelTimeInteger <- sprintf(
      "%1$s = round( %1$s )",
      hfTestCumul
    )

    execGRASS(
      "r.mapcalc",
      expression = exprTravelTimeInteger,
      flags = "overwrite"
    )

    # compute zonal statistic : time isoline as zone
    tblPopByZone <- amGetRasterStatZonal(
      mapZones = hfTestCumul,
      mapValues = inputPopulation
    )

    if (isTRUE(nrow(tblPopByZone) < 1)) {
      #
      # NO POPULATION FOUND
      #

      warning("amScalingUp_evalCoverage: no population found, set tblPopByZone to 0.")
      tblPopByZone <- data.frame(
        zone = amGetRasterStat(hfTestCumul, "max"),
        sum = 0
      )
    }

    # calculate cumulated sum of pop at each zone
    tblPopByZone$cumSum <- cumsum(tblPopByZone$sum)
    tblPopByZone <- tblPopByZone[c("zone", "sum", "cumSum")]
    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    totalPop <- tail(tblPopByZone, n = 1)$cumSum

    #
    # Extract matching capacity
    #

    # Set capacity using capacity table

    hfCap <- tableCapacity[
      totalPop >= round(tableCapacity$min) &
        totalPop <= round(tableCapacity$max),
    ]

    # If nothing match or multiple match take the nearest.
    noMatchCap <- isTRUE(!nrow(hfCap) == 1 && length(totalPop) > 0)
    if (noMatchCap) {
      warning("amScalingUp_evalCoverage: no matching capacity found, take the nearest.")
      # could be reversed...
      ran <- range(tableCapacity$capacity)
      takeMin <- isTRUE(
        abs(totalPop - max(ran)) >=
          abs(totalPop - min(ran))
      )
      if (takeMin) {
        hfCap <- tableCapacity[tableCapacity$capacity == min(ran), ]
      } else {
        hfCap <- tableCapacity[tableCapacity$capacity == max(ran), ]
      }
    }


    #
    # Return list
    #

    # bind current summary to previous
    candidatesEval[[candidateName]] <- list(
      amProcessingOrder = i,
      amRasterCumul = hfTestCumul,
      amVectorPoint = hfTest,
      amTimeMax = maxTravelTime,
      amPopTimeMax = totalPop,
      amCapacity = hfCap$capacity,
      amLabel = hfCap$label,
      tblPopByZone = tblPopByZone
    )
  }
  return(candidatesEval)
}


#' Calc travel time on existing vector, create a rescaled map
#' @param inputMap Existing vector from where start analysis
#' @param inputSpeed Speed and transport mod map in accessmod format
#' @param inputFriction AccessMod friction map
#' @param typeAnalysis Type of analysis : anisotropic or isotropic
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_traveltime <- function(inputMask,
  inputMap,
  inputSpeed,
  inputFriction,
  typeAnalysis,
  knightMove = FALSE,
  towards = TRUE,
  weight = 1,
  inverse = FALSE,
  roundingMethod = c("ceil", "round", "floor"),
  language = config$language
) {
  roundingMethod <- match.arg(roundingMethod)

  tmpOut <- amRandomName("tmp__coef_travel_time")
  tmpA <- amRandomName("tmp__")
  on_exit_add({
    rmRastIfExists(tmpA)
  })
  # create a cumulative cost map on the whole region, including new hf sets at the end of this loop.
  typeAnalysis <- match.arg(typeAnalysis, c("anisotropic", "isotropic"))
  if (isEmpty(typeAnalysis)) {
    stop(
      ams(
        id = "analysis_scaleup_analysis_type_warning"
      )
    )
  }
  switch(typeAnalysis,
    "anisotropic" = amAnisotropicTravelTime(
      inputSpeed = inputSpeed,
      inputHf = inputMap,
      knightMove = knightMove,
      outputTravelTime = tmpA,
      towardsFacilities = towards,
      maxTravelTime = 0, # unlimited
      roundingMethod = roundingMethod,
      timeoutValue = "null()"
    ),
    "isotropic" = amIsotropicTravelTime(
      inputFriction = inputFriction,
      inputHf = inputMap,
      knightMove = knightMove,
      outputTravelTime = tmpA,
      maxTravelTime = 0,
      roundingMethod = roundingMethod,
      timeoutValue = "null()"
    )
  )
  amRasterRescale(
    inputMask = inputMask,
    inputRast = tmpA,
    outputRast = tmpOut,
    range = config$scalingUpRescaleRange,
    weight = weight,
    reverse = inverse,
    nullHandlerMethod = "max"
  )
  return(tmpOut)
}



#' Create a rescaled cumulative cost map
#' @param inputMask Set a mask to limit computation
#' @param inputPop Population map
#' @param radiusKm Radius of the analysis
#' @param mapResolution Map resolution in meter
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @param position Position of the layer
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_pop <- function(inputMask,
  inputMap,
  radiusKm,
  weight = 1,
  inverse = FALSE) {
  tmpOut <- amRandomName("tmp__coef_pop_density")
  tmpA <- amRandomName("tmp__coef_pop_nonull")
  tmpB <- amRandomName("tmp__coef_pop_to_rescale")
  on_exit_add({
    rmRastIfExists(tmpA)
    rmRastIfExists(tmpB)
  })
  radiusKm <- as.numeric(radiusKm)
  mapResolution <- as.numeric(gmeta()$nsres)
  weight <- as.numeric(weight)

  neighbourSize <- round((abs(radiusKm) * 1000) / mapResolution)
  useMovingWindow <- isTRUE(neighbourSize != 0)
  # r.neighbors needs odd number
  if (isTRUE(useMovingWindow && neighbourSize %% 2 == 0)) {
    neighbourSize <- neighbourSize + 1
  }

  # in this coefficient, null should be converted to zero.
  # maybe r.null on a copy could do the trick also
  exp <- sprintf(
    "%1$s = if( !isnull( %2$s ), if( isnull( %3$s ), 0, %3$s))",
    tmpA,
    inputMask,
    inputMap
  )

  execGRASS(
    "r.mapcalc",
    expression = exp,
    flags = "overwrite"
  )

  if (useMovingWindow) {
    # create a density map using a  moving window sum of population on a radius
    execGRASS("r.neighbors",
      flags = c("c", "overwrite"),
      input = inputMap,
      output = tmpA,
      method = "sum",
      size = neighbourSize
    )
  }

  amRasterRescale(
    inputMask = inputMask,
    inputRast = tmpA,
    outputRast = tmpOut,
    range = config$scalingUpRescaleRange,
    weight = weight,
    reverse = inverse,
    nullHandlerMethod = "none"
  )
  return(tmpOut)
}


#' Create a rescaled distance map
#' @param inputMask Set a mask to limit computation
#' @param inputMap A raster or vector  map from which compute euclidean distance. Vector map will be rasterized.
#' @param inputMapType Set if the input map is a vector or a raster
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @param position Position of the layer
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_dist <- function(inputMask,
  inputMap,
  inputMapType = c("vector", "raster"),
  weight = 1,
  inverse = FALSE) {
  inputMapType <- match.arg(inputMapType)
  tmpOut <- amRandomName("tmp__coef_dist")
  tmpA <- amRandomName("tmp__")
  tmpB <- amRandomName("tmp__")

  on_exit_add({
    rmRastIfExists(tmpA)
    rmRastIfExists(tmpB)
  })

  if (inputMapType == "vector") {
    # Convert vector to raster
    execGRASS("v.to.rast",
      input = inputMap,
      output = tmpA,
      use = "val",
      value = 0,
      flags = "overwrite"
    )
  } else {
    # Filter usable value NOTE: why a second statement ? if(!isnull(%s),0) ?
    exprCoefDistVal <- sprintf("%s = if(isnull(%s),null(),0)", tmpA, inputMap)
    execGRASS("r.mapcalc", expression = exprCoefDistVal)
  }

  # compute grow distance from tmpA
  execGRASS(
    "r.grow.distance",
    input = tmpA,
    distance = tmpB,
    metric = "euclidean",
    flags = "overwrite"
  )

  amRasterRescale(
    inputMask = inputMask,
    inputRast = tmpB,
    outputRast = tmpOut,
    range = config$scalingUpRescaleRange,
    weight = weight,
    reverse = inverse,
    nullHandlerMethod = "none"
  )


  return(tmpOut)
}


#' Create a rescaled version of generic suitability map
#' @param inputMap The raster map to convert
#' @param position Thw position of the layer
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @return name of the rescaled map
#' @export
amScalingUpCoef_generic <- function(inputMask,
  inputMap,
  weight = 1,
  inverse = FALSE) {
  tmpOut <- amRandomName("tmp__coef_generic")

  amRasterRescale(
    inputMask = inputMask,
    inputRast = inputMap,
    outputRast = tmpOut,
    range = config$scalingUpRescaleRange,
    weight = weight,
    reverse = inverse,
    nullHandlerMethod = "none"
  )

  return(tmpOut)
}




#' Create temporary candidate cells based on non-null raster value
#' @param input Raster layer from which compute candidates
#' @param output Raster of candidates cells
#' @export
amScalingUp_createCandidatesTemp <- function(input = NULL,
  output = NULL) {
  expCandTemp <- paste(output, "= if(!isnull(", input, "),1,null())")
  execGRASS(
    "r.mapcalc",
    expression = expCandTemp,
    flags = "overwrite"
  )
}

#' Initialize output residual population layer
#'
#' Create the residual population layer (outputPopResidual) where friction layer is greated than 0 (remove population on barrier).
#'
#' @param inputPopResidual Raster layer of initial residual population
#' @param inputFriction Raster layer of speed (or friction)
#' @param inputFriction Raster layer of friction (or speed)
#' @param outputPopResidual Raster layer of population residual to update and return at the end
#' @export
amInitPopResidual <- function(inputPop = NULL,
  inputPopResidual = NULL,
  inputFriction = NULL,
  inputSpeed = NULL,
  outputPopResidual = NULL) {
  inputTest <- inputFriction

  if (isEmpty(inputTest) || !amRastExists(inputTest)) {
    inputTest <- inputSpeed
    if (!amRastExists(inputTest)) {
      stop("amInitPopResidual: no valid test layer")
    }
  }

  if (isEmpty(inputPopResidual)) {
    inputPopResidual <- inputPop
  }

  expPopResidual <- sprintf(
    "%1$s = if(((%2$s > 0)&&&(%3$s > 0)), %2$s,0)",
    outputPopResidual,
    inputPopResidual,
    inputTest
  )

  if (isEmpty(expPopResidual)) {
    stop(
      ams("analysis_scaleup_missing_population_residual")
    )
  }

  execGRASS(
    "r.mapcalc",
    expression = expPopResidual,
    flags = "overwrite"
  )
}


#' Create composite index based on input coef
#' @param candidates Candidates raster layer (after exclusion process)
#' @param coefLayerTable Components of the composite index
amScalingUp_suitability <- function(inputCandidates,
  inputSpeed,
  inputFriction,
  outputSuitability,
  coefTable,
  roundingMethod = c("ceil", "round", "floor"),
  language = config$language
) {
  roundingMethod <- match.arg(roundingMethod)
  if (nrow(coefTable) < 1) {
    stop(
      ams(
        id = "analysis_scaleup_missing_layer_warning"
      )
    )
  }

  layersCalc <- character(0)
  nLayer <- nrow(coefTable)
  coefTable$skip <- FALSE
  wSum <- 0

  # skip empty layer
  for (i in 1:nLayer) {
    l <- as.list(coefTable[i, ])
    if (l$type == "raster") {
      coefTable[i, ]$skip <- amRastIsEmpty(l$layer)
    } else {
      coefTable[i, ]$skip <- amVectIsEmpty(l$layer)
    }
  }

  # get weight sum
  coefOut <- vector()

  on_exit_add({
    rmRastIfExists(as.character(coefOut))
  })

  for (i in 1:nLayer) {
    l <- as.list(coefTable[i, ])
    if (l$skip) {
      amDebugMsg(paste("Scaling up suitability skipping empty layer", l$layer))
    } else {
      wSum <- wSum + l$weight
      opt <- amParseOptions(l$options)
      l <- c(l, opt)
      switch(l$factor,
        "popsum" = {
          coefOut[i] <- amScalingUpCoef_pop(
            inputMask = inputCandidates,
            inputMap = amNoMapset(l$layer),
            radiusKm = l$r,
            weight = l$weight,
            inverse = isTRUE(l$p == "hvls")
          )
        },
        "dist" = {
          coefOut[i] <- amScalingUpCoef_dist(
            inputMask = inputCandidates,
            inputMap = amNoMapset(l$layer),
            inputMapType = l$type,
            weight = l$weight,
            inverse = isTRUE(l$p == "hvls")
          )
        },
        "traveltime" = {
          coefOut[i] <- amScalingUpCoef_traveltime(
            inputMask = inputCandidates,
            inputMap = amNoMapset(l$layer),
            inputSpeed = inputSpeed,
            inputFriction = inputFriction,
            typeAnalysis = l$t,
            roundingMethod = roundingMethod,
            knightMove = isTRUE(l$k == "n16"),
            towards = isTRUE(!l$d == "from"),
            weight = l$weight,
            inverse = isTRUE(l$p == "hvls")
          )
        },
        "priority" = {
          coefOut[i] <- amScalingUpCoef_generic(
            inputMask = inputCandidates,
            inputMap = amNoMapset(l$layer),
            weight = l$weight,
            inverse = isTRUE(l$p == "hvls")
          )
        }
      )
    }
  }

  # get mean value from scaled value. NOTE: check if this is ok for a multicriteria analysis.

  expSuitability <- sprintf(
    " %1$s = int((%2$s) / %3$s)",
    outputSuitability,
    paste(na.omit(coefOut), collapse = "+"),
    wSum
  )
  execGRASS(
    "r.mapcalc",
    expression = expSuitability,
    flags = "overwrite"
  )

  if (amGetRasterStat(outputSuitability, "max") == 0) {
    amDebugToJs("Max suitability = 0 !")
  }



  return(NULL)
}



#' Find the best cell based on an exclusion procedure and an suitability map
#' @param inputFriction String. Name of friction map
#' @param inputSpeed String. Name of speed map
#' @param tableExclusion. Data.frame. Table of exclusion rules.
#' @param tableSuitability. Data.frame. Table of suitability rules.
#' @param inputFacilities. String. Name of map that containing existing set of facilites or will contain new generated one.
#' @param inputCandidates. String. Name of map that containing available candidates cells.
#' @param outputBestCandidates. String. Name of vector map with the best suitable locatio based on exclusion and suitability.
#' @return # return a list with
#' candidatesBestVect: chr Best vector point map name
#' suitabilityMap    : chr Suitability map name
#' suitabilityMax    : num Maximum suitability
#' nCandidates       : num Number of candidates
#' nBeforeExclusion  : num Number of cell available
#' nAfterExclusion   : num Number of cells after exclusion
#' msg               : chr Summary message
#' @export

amScalingUp_findBestCells <- function(inputFriction,
  inputSpeed,
  tableExclusion,
  tableSuitability,
  inputCandidates,
  outputBestCandidates,
  candidateCountInit,
  roundingMethod = c("ceil", "round", "floor"),
  language = config$language

) {
  roundingMethod <- match.arg(roundingMethod)

  res <- list()

  #
  # Layer name init
  #
  tmpSuitabilityLayer <- amRandomName("tmp__suitability")
  tmpBestCandidates <- amRandomName("tmp__candidates_best_rast")

  #
  # Create candidates raster and get count of remaining cells.
  #

  #
  # Apply exclusion rules on inputCandidates
  #

  # Get number of candidates before
  candidateCountBefore <- amGetRasterStat(inputCandidates, "n")
  # apply rules
  amScalingUp_candidateExcludeTable(
    tableExclusion = tableExclusion,
    inputCandidates = inputCandidates
  )
  # Get candidates after
  candidateCountAfter <- amGetRasterStat(inputCandidates, "n")


  if (length(candidateCountAfter) < 1 || candidateCountAfter < 1) {
    # oops, no more candidate.
    res$noMoreCandidates <- TRUE
    return(res)
  } else {
    #
    # Create suitability map
    #

    amScalingUp_suitability(
      inputCandidate = inputCandidates,
      inputSpeed = inputSpeed,
      inputFriction = inputFriction,
      outputSuitability = tmpSuitabilityLayer,
      coefTable = tableSuitability,
      roundingMethod = roundingMethod
    )

    # get max suitability. We expect 100 each time, as values are rescaled.
    suitMax <- amGetRasterStat(tmpSuitabilityLayer, "max")
    # select best candidates based on suitMax
    expCandSelect <- sprintf(
      "%1$s=if( %2$s >= %3$s , %2$s , null() )",
      tmpBestCandidates,
      tmpSuitabilityLayer,
      suitMax
    )

    res <- try(
      execGRASS(
        "r.mapcalc",
        expression = expCandSelect,
        flags = "overwrite"
      ),
      silent = T
    )


    # how much candidates left ?
    nCand <- amGetRasterStat(tmpBestCandidates, "n")

    # convert best candidates to vector
    execGRASS("r.to.vect",
      type = "point",
      input = tmpBestCandidates,
      output = outputBestCandidates,
      column = "amSuitability",
      flags = "overwrite"
    )

    # create summary text
    nExcluded <- candidateCountInit - candidateCountAfter

    txt <- sprintf(
      ams(
        id = "analysis_scaleup_summary_selected"
      ),
      nCand,
      candidateCountAfter,
      ifelse(nCand > 1, "s", ""),
      nExcluded,
      as.integer(suitMax),
      max(config$scalingUpRescaleRange)
    )

    res <- list(
      candidatesBestVect = outputBestCandidates,
      suitabilityMap = tmpSuitabilityLayer,
      suitabilityMax = suitMax,
      nCandidates = nCand,
      nBeforeExclusion = candidateCountBefore,
      nAfterExclusion = candidateCountAfter,
      msg = txt
    )
    return(res)
  }
}



#' Extract the best coverage evaluation
#' @param listEvalCoverage Output from amScalingUp_evalCoverage
#' @param criteria List item name containing the value to maximize
#' @return listEvalCoverage subset
amScalingUp_extractBest <- function(listEvalCoverage,
  criteria = "amPopTimeMax") {
  # take the Best Candidate according to criteria.
  select <- lapply(listEvalCoverage, "[[", criteria) %>%
    which.max() %>%
    names() %>%
    listEvalCoverage[[.]]
}


#' Update candidates layer by substracting exclusion layer
#'
#' @param inputCandidates Layer of candidates on which exclude cells
#' @param inputLayer Layer of exclusion
#' @param inputLayerType Type of data for the layer of exclusion: vector or raster
#' @param distance Distance of the buffer. If zero, use vector as exclusion mask
#' @param keep Selection strategy : keep value 'inside' or 'outside' the buffer
#' @return count available candidate left
amScalingUp_candidateExclude <- function(inputCandidates,
  inputLayer,
  inputLayerType = c("vector", "raster"),
  distance = 1000,
  keep = c("keep_inside", "keep_outside")) {
  # validation
  stopifnot(is.numeric(distance))
  keep <- match.arg(keep)
  inputLayerType <- match.arg(inputLayerType)
  # init vars
  tmpExcl <- amRandomName("tmp__exclusion")
  tmpExclBuffer <- amRandomName("tmp__exclusion", "buffer")

  on_exit_add({
    # Remove temporary raster
    rmRastIfExists(c(tmpExcl, tmpExclBuffer))
  })

  outputCandidates <- inputCandidates
  # Convert raster map
  if (inputLayerType == "vector") {
    execGRASS(
      "v.to.rast",
      input  = inputLayer,
      output = tmpExcl,
      use    = "val",
      value  = 1,
      flags  = "overwrite"
    )
  } else {
    execGRASS(
      "g.copy",
      raster = c(inputLayer, tmpExcl)
    )
  }
  # Create buffer if necessary
  if (distance > 0) {
    execGRASS(
      "r.buffer",
      input     = tmpExcl,
      output    = tmpExclBuffer,
      distances = c(distance),
      flags     = "overwrite"
    )
  } else {
    execGRASS(
      "g.rename",
      raster = c(tmpExcl, tmpExclBuffer),
      flags  = "overwrite"
    )
  }
  # Apply keeping strategy
  if (keep == "keep_inside") {
    expCandExcl <- sprintf(
      "%1$s = if(!isnull(%2$s),%1$s,null())",
      outputCandidates,
      tmpExclBuffer
    )
  } else {
    expCandExcl <- sprintf(
      "%1$s = if(isnull(%2$s),%1$s,null())",
      outputCandidates,
      tmpExclBuffer
    )
  }

  execGRASS(
    "r.mapcalc",
    expression = expCandExcl,
    flags      = "overwrite"
  )
  # count remaining candidates
  countLeft <- amGetRasterStat(outputCandidates, "n")
  if (length(countLeft) < 1) {
    countLeft <- 0
  }
  # return candidates count
  return(countLeft)
}

#' Iterate through table of exclusion and apply amScalingUp_candidateExclude on candidate layer.
#'
#' Input table structure
#' #data.frame':  1 obs. of  5 variables:
#' $ select: logi TRUE
#' $ layer : chr "scaling_up_exclusion_vect__test_super@Burkina"
#' $ buffer: int 5
#' $ method: chr "inside"
#' $ type  : chr "vector"
#'
#' @param tableExclusion
#' @param candidatesLayer Raster layer of candidates
#' @param unitDistance Distance unit in meter (m) or kilometer (m)
#' @return count available candidate left
amScalingUp_candidateExcludeTable <- function(inputCandidates,
  tableExclusion,
  unitDistance = "km") {
  # validation
  stopifnot(unitDistance %in% c("m", "km"))
  # init
  noMoreCandidates <- FALSE

  tbl <- tableExclusion
  tblEmpty <- nrow(tbl) < 1
  distMult <- ifelse(unitDistance == "km", 1000, 1)
  countInit <- amGetRasterStat(inputCandidates, "n")
  countLeft <- 0
  skip <- FALSE
  # Iterate trough table
  if (!tblEmpty) {
    for (i in 1:nrow(tbl)) {
      # create shortcut for message
      l <- tbl[i, "layer"]
      t <- tbl[i, "type"]
      d <- tbl[i, "buffer"] * distMult
      m <- tbl[i, "method"]
      if (t == "raster") {
        skip <- amRastIsEmpty(l)
      } else {
        skip <- amVectIsEmpty(l)
      }
      if (skip) {
        amDebugMsg(
          sprintf(
            "Exclude cells : skipping empty layer %s (method=%s;buffer=%s;type=%s)",
            l, m, d, t
          )
        )
      } else {
        # eval candidates
        if (noMoreCandidates) {
          amDebugMsg(
            sprintf(
              "Exclude cells. No more candidates, skipping layer %s (method=%s;buffer=%s;type=%s)",
              l, m, d, t
            )
          )
        } else {
          countLeft <- amScalingUp_candidateExclude(
            inputCandidates = inputCandidates,
            inputLayer = l,
            inputLayerType = t,
            distance = d,
            keep = m
          )
          if (isTRUE(countLeft < 1)) {
            noMoreCandidates <- TRUE
          }
        }
      }
    }
  } else {
    countLeft <- countInit
  }

  return(countLeft)
}
