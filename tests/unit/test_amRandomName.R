#
# Unit test: amRandomName
#
# Template for pure-function unit tests.
# No GRASS, no config, no DB — just R.
# amRandomName is available because global.R is sourced by start.R before this runs.
#

amtest$check(
  "amRandomName: returns a non-empty character string",
  {
    r <- amRandomName()
    is.character(r) && nchar(r) > 0
  }
)

amtest$check(
  "amRandomName: prefix is prepended with separator",
  {
    r <- amRandomName(prefix = "rTravelTime")
    grepl("^rTravelTime_", r)
  }
)

amtest$check(
  "amRandomName: suffix is appended with separator",
  {
    r <- amRandomName(suffix = "demo")
    grepl("_demo$", r)
  }
)

amtest$check(
  "amRandomName: prefix and suffix combined",
  {
    r <- amRandomName(prefix = "rTravelTime", suffix = "demo")
    grepl("^rTravelTime_", r) && grepl("_demo$", r)
  }
)

amtest$check(
  "amRandomName: two calls produce different names",
  {
    # Collision probability: (1/25)^20 ≈ 0 — safe to assert
    amRandomName() != amRandomName()
  }
)
