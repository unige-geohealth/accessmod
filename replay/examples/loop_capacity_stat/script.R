#!/bin/Rscript
#
# Kitchen sink exemple : almost every possible topic covered
# - Not the easiest
# - For demonstration
# - For reference
#

#
# Load accessmod environement
#
source("global.R")

#
# Defin paths and config
#
pathConfig <- "/batch/config.json"
pathData <- "/batch/data.json"
pathProject <- "/batch/project.am5p"
pathOut <- "/batch/out"
pathSummary <- "/batch/out/summary.json"

conf <- amAnalysisReplayParseConf(pathConfig)
data <- fromJSON(pathData)
scenarios <- data$scenarios
traveltime <- c(30, 60, 120, 240)

#
# Create a summary table
# - updated at each iteration
# - exported as JSON
#
dfSummary <- data.frame(
  id = character(0),
  traveltime = numeric(0),
  populationResidual = numeric(0),
  populationOrig = numeric(0),
  transport = character(0)
)

#
# Import project
# - Required if no DB is available
# - Name must match the one in config
amAnalisisReplayImportProject(
  archive = pathProject,
  name = conf$location,
  owerwrite = TRUE
)

#
# Start grass name space
# - Usefull for retrieving project data raster stat,
#   between replays
# - Not required for amAnalysisReplayExec
#
amGrassNS(
  location = conf$location,
  mapset = conf$mapset,
  {
    #
    # Loop over time and scenarios
    #
    for (s in names(scenarios)) {
      confLocal <- conf
      confLocal$args$tableScenario <- scenarios[[s]]

      for (tt in traveltime) {
        #
        # Create id for this iteration
        #
        id <- sprintf("%s_%s", s, tt)

        #
        # Update travel time param
        #
        confLocal$args$maxTravelTime <- tt

        #
        # Print timestamp
        #
        amTimeStamp(id)

        #
        # Set output dir
        #
        pathDirOut <- file.path(pathOut, id)
        mkdirs(pathDirOut)
        pathProjectOut <- file.path(pathDirOut, "project_out.am5p")

        #
        # Get population sum before coverage
        # from raster in GRASS DB
        #
        sumPopBefore <- amGetRasterStat(
          "rPopulation__demo_patients",
          metric = "sum"
        )

        #
        # Launch replay
        #
        amAnalysisReplayExec(confLocal,
          exportProjectDirectory = pathProjectOut,
          exportDirectory = pathDirOut
        )

        #
        # Retrieve stats from raster in GRASS DB
        #
        sumPopAfter <- amGetRasterStat(
          "rPopulationResidual__batch",
          metric = "sum"
        )

        #
        # Create line summary
        #
        dfRow <- data.frame(
          id = id,
          traveltime = tt,
          populationResidual = sumPopAfter,
          populationOrig = sumPopBefore
        )
        dfSummary <- rbind(dfSummary, dfRow)
      }
    }
  }
)

#
# Write Summary as JSON file
#
summaryjson <- toJSON(dfSummary, auto_unbox = TRUE)
write(summaryjson, pathSummary)

#
# End message
#
amTimeStamp("Finished")
