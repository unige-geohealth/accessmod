source("global.R")
source("tests/helpers.R")

#
# Launch testing
#

amtest <- AmTests$new()

amtest$script("tests/accessibility/test_demo_motorized.R")
amtest$script("tests/referral/test_demo.R")

amtest$print()
amtest$getResult(asBoolean = FALSE)
