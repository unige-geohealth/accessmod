print("Testing Referral Analysis")

# --------------------------------------------------------------- #
# Series of tests for demo location, referral module
# --------------------------------------------------------------- #
data_path <- "tests/referral/data"

# Base configuration
conf_base <- amAnalysisReplayParseConf("tests/referral/config.json")

# List of configurations
config_list <- list(
  "conf_init" = conf_base,
  "conf_init_permuted" = {
    conf <- conf_base
    conf$args$permuteGroups <- TRUE
    conf
  },
  "conf_all" = {
    conf <- conf_base
    conf$args$parallel <- TRUE
    conf$args$tableFacilitiesTo$amSelect <- TRUE
    conf$args$tableFacilities$amSelect <- TRUE
    conf
  },
  "conf_all_permuted" = {
    conf <- conf_base
    conf$args$parallel <- TRUE
    conf$args$permuteGroups <- TRUE
    conf$args$tableFacilitiesTo$amSelect <- TRUE
    conf$args$tableFacilities$amSelect <- TRUE
    conf
  }
)

# if TRUE, overwrite previous validation files
# -> in testing mode, turn to FALSE
#
init <- FALSE

# Location and mapset based on first config
location <- config_list[["conf_init"]]$location
mapset <- config_list[["conf_init"]]$mapset

# Analysis files for the exports : table, vectors, etc..
exportDir <- file.path(tempdir(), amRandomName())
mkdirs(exportDir, mustWork = FALSE)

#
# Helper to exec + import table
#
execImport <- function(conf) {
  exportedDirs <- amAnalysisReplayExec(conf,
    exportDirectory = exportDir
  )
  res_file <- list.files(
    exportedDirs$tReferral__referral,
    full.names = TRUE
  )
  res <- import(res_file)
}

amGrassNS(
  location = location,
  mapset = mapset,
  {
    #
    # Set / Compare analysis
    #
    for (k in names(config_list)) {
      conf <- config_list[[k]]

      file_valid_path <- sprintf("%s/result_%s.xlsx", data_path, k)
      res <- execImport(conf)

      if (isTRUE(init)) {
        export(res, file_valid_path)
        res_valid <- res
      } else {
        res_valid <- import(file_valid_path)
      }

      amtest$check(
        sprintf("Referral : validation for %s", k),
        isTRUE(all_equal(res_valid, res))
      )
    }

    #
    # Advanced comparison
    #
    cols <- c("from__cat", "to__cat", "time_m")
    a <- execImport(config_list[["conf_all"]])
    b <- execImport(config_list[["conf_all_permuted"]])

    amtest$check(
      "Referral : ids + time,  w/ & w/o permutation should be equal",
      isTRUE(all_equal(a[, cols], b[, cols]))
    )

    #
    # Distance diff +/- 1km
    # - v.net.dist can produce +/- 1 resolution error
    # - this should not happen, but can't figure out what happen
    # - seems stable
    #
    diff <- abs(a$distance_km - b$distance_km)
    amtest$check(
      "Referral : w/ w/o permultation, max 1*resol distance threshold",
      isTRUE(all(diff <= 1))
    )
  }
)
