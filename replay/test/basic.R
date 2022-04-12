rr <- function() {
  source("global.R")
  f_json <- "/lAnalysisParameters__test_last10_permute_vil_to_hf.json"
  f_dir <- "/accessmodConfigs"
  project <- "/issue363"
  db_loc <- "/data/dbgrass"
  f_path <- paste0(db_loc, project, project, f_dir, f_json)
  amAnalysisReplayExec(f_path, exportDirectory = "/data/shared")
}

rr <- function() {
  source("global.R")
  f_json <- "/lAnalysisParameters__testref.json"
  f_dir <- "/accessmodConfigs"
  project <- "/demo3"
  db_loc <- "/data/dbgrass"
  f_path <- paste0(db_loc, project, project, f_dir, f_json)
  amAnalysisReplayExec(f_path, exportDirectory = "/data/shared")
}

rr <- function() {
  source("global.R")
  f_json <- "/lAnalysisParameters__replay.json"
  f_dir <- "/accessmodConfigs"
  project <- "/demo3"
  db_loc <- "/data/dbgrass"
  f_path <- paste0(db_loc, project, project, f_dir, f_json)
  amAnalysisReplayExec(f_path)
}

