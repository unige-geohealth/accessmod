print("Testing Best Coverage Analysis")

# --------------------------------------------------------------- #
# Series of tests for demo location, best coverage module
# --------------------------------------------------------------- #

# Base configuration
conf_base <- amAnalysisReplayParseConf("tests/best_coverage/config.json")

# List of configurations
config_list <- list(
  "conf_with_admin" = conf_base,
  "conf_no_admin" = {
    conf <- conf_base
    conf$args$adminCheck <- FALSE
    conf
  }
)

# Reference files for each configuration
ref_files <- list(
  "conf_with_admin" = "tests/best_coverage/result_table_with_admin.xlsx",
  "conf_no_admin" = "tests/best_coverage/result_table.xlsx"
)

# if TRUE, overwrite previous validation files
# -> in testing mode, turn to FALSE
#
init <- FALSE

# Location and mapset based on first config
location <- config_list[["conf_with_admin"]]$location
mapset <- config_list[["conf_with_admin"]]$mapset

amGrassNS(
  location = location,
  mapset = mapset,
  {
    for (k in names(config_list)) {
      conf <- config_list[[k]]
      file_valid_path <- ref_files[[k]]

      dirs <- replayExec(conf)
      res <- replayImport(dirs, "tBestCoverage__test_best_coverage")

      if (isTRUE(init)) {
        export(res, file_valid_path)
        res_valid <- res
      } else {
        res_valid <- import(file_valid_path)
      }

      amtest$check(
        sprintf("Best Coverage : validation for %s", k),
        isTRUE(all_equal(res_valid, res))
      )
    }
  }
)
