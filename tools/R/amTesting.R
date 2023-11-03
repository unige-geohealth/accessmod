library(R6)
library(jsonlite)
library(magrittr)

#' AmTests: Minimalist Testing Framework for R
#'
#' This class provides a simple interface for performing and tracking automated
#' tests in R. It includes methods to manually check conditions, run test
#' scripts, and report results in a JSON format.
#'
#' @examples
#' # Example 1: Direct test
#' my_test <- AmTests$new()
#' my_test$check("Example test - should fail", 1 > 2, "1 is not gt than 2")
#' my_test$check("Example test - should pass", 2 > 1)
#' my_test$getResultJson() # Outputs JSON result
#'
#' # Example 2: Running a test script
#' # Assuming 'my_test_script.R' exists and contains test code
#' my_test$script("/path/to/my_test_script.R")
#' my_test$printResults() # Prints a summary of all test results
#' my_test$getResultJson() # Outputs JSON result
#'
#' @export AmTests
AmTests <- R6Class("AmTests",
  public = list(
    issues = character(0),
    success = character(0),
    results = data.frame(
      description = character(0),
      success = logical(0),
      message = character(0),
      stringsAsFactors = FALSE
    ),

    #' Perform a test and record the result
    check = function(description, success, message = "") {
      self$addResult(description, success, message)
      if (success) {
        self$addSuccess(description)
      } else {
        self$addIssue(description)
      }
    },

    #' Execute a test script from a file
    script = function(file) {
      if (file.exists(file)) {
        tryCatch(
          {
            source(file, local = TRUE)
          },
          error = function(e) {
            self$check(file, FALSE, e$message)
          }
        )
      } else {
        self$check(file, FALSE, "File does not exist.")
      }
    },

    #' Output test results as JSON
    getResultJson = function() {
      list(
        pass = length(self$issues) == 0,
        details = self$results
      ) %>%
        toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
        cat()
    },

    #' Print summary of test results
    printResults = function() {
      print(self$results)
    },

    # Internal: Add a test result
    addResult = function(description, success, message) {
      self$results <- rbind(self$results, data.frame(
        description, success, message,
        stringsAsFactors = FALSE
      ))
    },

    # Internal: Record a passing test
    addSuccess = function(description) {
      self$success <- c(self$success, description)
    },

    # Internal: Record a failing test
    addIssue = function(description) {
      self$issues <- c(self$issues, description)
    }
  )
)
