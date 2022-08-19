# reload global.R, if code change. Not required for batch processing
source("global.R")
tmpPathConf <- tempfile("am_replay", fileext = "json")
config <- fromJSON(pathConfig)
config$location <- projectName
config$mapset <- projectName
config$args$parallel <- TRUE
config$args$useMaxSpeedMask <- TRUE
config$args$snapToGrid <- TRUE
config$args$maxTravelTime <- 0
tblFrom <- config$args$tableFacilities
tblFrom <- tblFrom[tblFrom$amSelect, ]
tblFrom <- tblFrom[c(1:4), ]
config$args$tableFacilities <- tblFrom
write(toJSON(config, auto_unbox = T), tmpPathConf)
amAnalysisReplayExec(tmpPathConf)
