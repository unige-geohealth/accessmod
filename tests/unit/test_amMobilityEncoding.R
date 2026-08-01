#
# Unit tests for the R/C encoded mobility raster contract.
#

amtest$check(
  "Mobility encoding: walking at 3.2 km/h",
  identical(amEncodeModeSpeed("WALKING", 3.2), 1003200)
)

amtest$check(
  "Mobility encoding: bicycling at 12 km/h",
  identical(amEncodeModeSpeed("BICYCLING", 12), 2012000)
)

amtest$check(
  "Mobility encoding: motorized at 3.6 km/h",
  identical(amEncodeModeSpeed("MOTORIZED", 3.6), 3003600)
)

amtest$check(
  "Mobility encoding: bicycle speed above LUT range is rejected",
  inherits(try(amEncodeModeSpeed("BICYCLING", 100.5), silent = TRUE),
    "try-error"
  )
)
