source("global.R")
pathConfig <- "./replay/sample/vietnam_386/config.json"
# Copied using docker compose cp ~/Downloads/Vietnam.am5p am5_dev:/data/project.am5p
pathProject <- "/data/shared/Vietnam.am5p"
projectName <- "vietnam_386"

i <- function(){
  source("replay/sample/vietnam_386/import.R",local=T)
}

r <- function() {
  source("replay/sample/vietnam_386/run.R", local = T)
}
print("Import using i(), run using r()")
