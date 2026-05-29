source("global.R")

#
# Set language
# - Some filename export have translated name : not compatible with tests
#
config$language <- "en"
amTranslateSetSavedLanguage(config$language)

source("tests/helpers.R")

args <- commandArgs(trailingOnly = TRUE)
output_file <- args[1]

amtest <- AmTests$new()

amtest$script("tests/local/test_issue_474_referral_permute.R")

amtest$printResults()

if (!is.null(output_file)) {
  amtest$saveResultJsonToFile(output_file)
} else {
  amtest$getResultJson()
}

if (length(amtest$issues) > 0) {
  quit(status = 1)
}

quit(status = 0)
