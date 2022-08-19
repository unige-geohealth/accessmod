print("Testing Accessibility Analysis")

# --------------------------------------------------------------- #
# Series of tests for demo location, with a scenario
# using only motorized transport : no effect of DEM, so
# isotropic vs anisotropic should be equal.
# --------------------------------------------------------------- #

conf <- amAnalysisReplayParseConf(
  "tests/accessibility/config_demo_motorized.json"
)

travelTime <- conf$args$outputTravelTime
maxTravelTime <- conf$args$maxTravelTime
knightMove <- c("knight", "standard")
analysis <- c("isotropic", "anisotropic")

means <- list()
nulls <- list()

amGrassNS(
  location = conf$location,
  mapset = conf$mapset,
  {
    for (k in knightMove) {
      for (a in analysis) {
        id <- sprintf("%s_%s", k, a)
        conf$args$knightMove <- k == "knight"
        conf$args$typeAnalysis <- a
        amAnalysisReplayExec(conf)
        means[id] <- amGetRasterStat(travelTime, metric = "mean")
        nulls[id] <- amGetRasterStat(travelTime, metric = "null")
        max <- amGetRasterStat(travelTime, metric = "max")
        desc <- sprintf("Max travel time ok: %s", id)
        amtest$check(desc, max <= maxTravelTime)
      }
    }
  }
)

amtest$check(
  "Motorized scenario, knight mode, iso == aniso",
  identical(means$knight_isotropic, means$knight_anisotropic)
)

amtest$check(
  "Motorized scenario, standard mode, iso == aniso",
  identical(means$standard_isotropic, means$standard_anisotropic)
)

amtest$check(
  "Expected mean travel time, k + iso",
  identical(round(means$knight_isotropic, 3), round(28.53036, 3))
)

amtest$check(
  "Expected mean travel time, s + aniso",
  identical(round(means$standard_anisotropic, 3), round(28.42772, 3))
)

amtest$check(
  "Expected nulls in travel time, k + iso",
  identical(nulls$knight_isotropic, 31829)
)

amtest$check(
  "Expected nulls in travel time, k + aniso",
  identical(nulls$knight_anisotropic, nulls$knight_isotropic)
)

amtest$check(
  "Expected nulls in travel time, s + aniso",
  identical(nulls$standard_anisotropic, 32283)
)

amtest$check(
  "Expected nulls in travel time, s + iso",
  identical(nulls$standard_isotropic, nulls$standard_anisotropic)
)
