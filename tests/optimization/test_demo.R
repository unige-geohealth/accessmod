print("Testing Optimization Analysis")

# --------------------------------------------------------------- #
# Series of tests for demo location, optimization module
# --------------------------------------------------------------- #

# Base configuration
conf_base <- amAnalysisReplayParseConf("tests/optimization/config.json")

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
  "conf_with_admin" = "tests/optimization/data/result_table_with_admin.xlsx",
  "conf_no_admin" = "tests/optimization/data/result_table.xlsx"
)

# if TRUE, overwrite reference files instead of comparing against them
# -> must be FALSE in CI / normal test runs
#
init <- FALSE

if (isTRUE(init)) {
  warning("init = TRUE: reference files will be overwritten, not compared")
}

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
      print(conf)

      dirs <- replayExec(conf)
      res <- replayImport(dirs, "tOptimization__test_optimization")

      if (isTRUE(init)) {
        export(res, file_valid_path)
        res_valid <- res
      } else {
        res_valid <- import(file_valid_path)
      }

      amtest$check(
        sprintf("Optimization : validation for %s", k),
        isTRUE(all.equal(res_valid, res))
      )
    }
  }
)
