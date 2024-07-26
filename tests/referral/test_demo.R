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
init <- TRUE

# Location and mapset based on first config
location <- config_list[["conf_init"]]$location
mapset <- config_list[["conf_init"]]$mapset


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

      dirs <- replayExec(conf)
      res <- replayImport(dirs, "tReferral__referral")


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
    cols <- c("from__cat", "to__cat")
    dirsA <- replayExec(config_list[["conf_all"]])
    dirsB <- replayExec(config_list[["conf_all_permuted"]])
    a <- replayImport(dirsA, "tReferral__referral")
    b <- replayImport(dirsB, "tReferral__referral")
    aNet <- replayImport(dirsA, "vReferralNetwork__referral")
    bNet <- replayImport(dirsB, "vReferralNetwork__referral")

    aNetMerged <- inner_join(aNet, a, by = c("from__cat", "to__cat"))
    bNetMerged <- inner_join(bNet, b, by = c("from__cat", "to__cat"))
    aNetMultiline <- st_combine(aNetMerged)
    bNetMultiline <- st_combine(bNetMerged)

    #
    # Table and net should have the same number of lines
    #
    nRowsOk <- all(c(
      nrow(a),
      nrow(b),
      nrow(aNet),
      nrow(bNet),
      nrow(aNetMerged),
      nrow(bNetMerged)
    ) == nrow(a))

    amtest$check(
      "Referral : w/ & w/o permutation shoud have the same n rows",
      isTRUE(nRowsOk)
    )

    #
    # Tables should be identical
    #
    amtest$check(
      "Referral : ids + time,  w/ & w/o permutation should be equal",
      isTRUE(all_equal(a[, cols], b[, cols]))
    )



    #
    # Geom diff
    #
    abDiff <- st_difference(aNetMultiline, bNetMultiline)

    amtest$check(
      "Referral : geom w & w/o permutation shoud be equal",
      isTRUE(length(abDiff) == 0)
    )


    #
    # Distance diff, with threshold
    # - v.net.distance seems to have minor issues with distance
    #   depending on direction traveled
    # - Distances delta should not vary more that a given amount, and
    #   never proprortionaly to the non-permuted distance
    # TODO: document this more in depth
    #
    threshold_dist <- 1
    diff <- abs(a$distance_km - b$distance_km)
    amtest$check(
      "Referral : w/ w/o permultation, max 1*resol distance threshold",
      isTRUE(all(diff <= threshold_dist))
    )
  }
)
