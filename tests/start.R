source("global.R")

#
# Launch testing
#
amtest <- AmTests$new()

amtest$script("tests/accessibility/test_demo_motorized.R")
amtest$print()
amtest$getResult(asBoolean = FALSE)
