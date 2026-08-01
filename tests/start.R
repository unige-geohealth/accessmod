source("global.R")

#
# Set language
# - Some filename export have translated name : not compatible with tests
#
config$language <- "en"
amTranslateSetSavedLanguage(config$language)

#
# Load tests helpers
#
source("tests/helpers.R")

# Get argument, eg. filepath

args <- commandArgs(trailingOnly = TRUE)
output_file <- args[1]

#
# Launch testing
# -> if output_file is set, write result in that
#
amtest <- AmTests$new()

amtest$script("tests/unit/test_amRandomName.R")
amtest$script("tests/unit/test_amGrassPseudoSession.R")
amtest$script("tests/unit/test_amGrass85Compatibility.R")
amtest$script("tests/unit/test_amMobilityEncoding.R")
amtest$script("tests/grass85/test_demo.R")
amtest$script("tests/project_crud/test_demo.R")
amtest$script("tests/accessibility/test_demo_motorized.R")
amtest$script("tests/referral/test_demo.R")
amtest$script("tests/optimization/test_demo.R")

amtest$printResults()

if (!is.null(output_file)) {
  amtest$saveResultJsonToFile(output_file)
} else {
  amtest$getResultJson()
}
