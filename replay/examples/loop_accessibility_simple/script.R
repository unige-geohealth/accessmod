#!/bin/Rscript
#
# Moderate exemple 
# - Loop over scenarios
# - Loop over travel time 
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
# Import project
# - Required if no DB is available
# - Name must match the one in config
amAnalisisReplayImportProject(
  archive = pathProject,
  name = conf$location,
  owerwrite = TRUE
)

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
    # Launch replay
    #
    amAnalysisReplayExec(confLocal,
      exportProjectDirectory = pathProjectOut,
      exportDirectory = pathDirOut
    )
  }
}

#
# End message
#
amTimeStamp("Finished")
