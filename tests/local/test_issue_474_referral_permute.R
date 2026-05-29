print("Testing local regression: issue #474 referral permutation")

fixtureDir <- "/data/shared/474_ref_permute_issue"
archive <- file.path(fixtureDir, "Referral_TEST.am5p")

if (!file.exists(archive)) {
  amtest$check(
    "issue #474 local fixture available",
    TRUE,
    sprintf("Skipped: %s not found", archive)
  )
} else {
  project <- "issue_474_ref_permute_local"
  projectPath <- file.path(pathDB, project)

  tryCatch(
    {
      amAnalysisReplayImportProject(
        archive,
        project,
        overwrite = TRUE
      )

      replay <- file.path(
        pathDB,
        project,
        project,
        "accessmodConfigs",
        "lAnalysisParameters__Referral_TEST.json"
      )

      conf <- amAnalysisReplayParseConf(replay)
      conf$location <- project
      conf$mapset <- project

      conf$args$parallel <- FALSE
      conf$args$tableFacilitiesTo$amSelect <-
        conf$args$tableFacilitiesTo$cat %in% c(200, 201)

      tag <- "issue474_local"
      conf$args$outputSpeed <- paste0("rSpeed__", tag)
      conf$args$outputFriction <- paste0("rFriction__", tag)
      conf$args$outputReferral <- paste0("tReferral__", tag)
      conf$args$outputNearestDist <- paste0("tReferralDist__", tag)
      conf$args$outputNearestTime <- paste0("tReferralTime__", tag)
      conf$args$outputNetDist <- paste0("vReferralNetwork__", tag)

      amAnalysisReplayExec(conf, exportDirectory = NULL)
      referral <- amGrassNS(
        location = project,
        mapset = project,
        {
          amMapsetDbGetQuery(project, conf$args$outputReferral)
        }
      )

      amtest$check(
        "issue #474 referral permutation completes",
        is.data.frame(referral) && nrow(referral) > 0
      )

      amtest$check(
        "issue #474 referral permutation keeps travel time",
        "time_m" %in% names(referral) && any(!is.na(referral$time_m))
      )

      amtest$check(
        "issue #474 referral permutation returns NA distance for missing path",
        "distance_km" %in% names(referral) &&
          any(is.na(referral$distance_km))
      )

      amtest$check(
        "issue #474 referral permutation preserves valid distances",
        "distance_km" %in% names(referral) &&
          any(!is.na(referral$distance_km))
      )
    },
    error = function(e) {
      amtest$check(
        "issue #474 referral permutation completes",
        FALSE,
        e$message
      )
    },
    finally = {
      if (dir.exists(projectPath)) {
        unlink(projectPath, recursive = TRUE)
      }
    }
  )
}
