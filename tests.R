source("global.R")
#
# Basic testing
# TODO:
#  - Probably better use tinytest, but oriented packages
#  - testthat downloads FULL CRAN. Not wanted here..
#

# issues updated during tests
amtest <- AmTests$new()

amtest$script("tests/accessibility/test_demo_motorized.R")
amtest$print()
amtest$getResult(asBoolean = FALSE)
