library(R6)

#' Basic testing framework
#' @example :
#' mytest <- AmTests$new()
#' mytest.check("hello work",1 > 2)
#' mytest.script('/tmp/test.R')
#' mytest.getResult()
#' > TRUE
#' mytest.print()
#' > <data.frame results>
AmTests <- R6Class(
  public = list(
    #' Launch a test
    #' @param {character} description : Description of the test
    #' @param {logical}success : Boolean indicating success
    #' @param {character} message : message of the test
    check = function(
      description,
      success,
      message = character(1)
    ) {
      self$addResult(description, success, message)
      if (success) {
        self$addSucess(description)
      } else {
        self$addIssue(description)
      }
    },
    #' Launch a test script
    #' @param {character} file : path to the test file
    script = function(file) {
      tryCatch(
        {
          source(file, local = TRUE)
        },
        error = function(e) {
          self$check(file, FALSE, e$message)
        }
      )
    },
    #' Get tests result
    #' @param {logical} asBoolean : result as boolean. If false: text
    #' @return {logical} result
    getResult = function(asBoolean = TRUE) {
      if (length(self$issues) > 0) {
        if (asBoolean) {
          return(FALSE)
        } else {
          print("Failed")
        }
      } else {
        if (asBoolean) {
          return(TRUE)
        } else {
          print("Success!")
        }
      }
    },
    #' Return summary table of all the tests
    #' @return {data.frame} Results table
    print = function() {
      print(self$results)
    },
    #' Internal : store result
    addResult = function(
      description,
      success,
      message = character(1)
    ) {
      result <- data.frame(
        description = description,
        success = success,
        message = message
      )
      self$results <- rbind(self$results, result)
    },
    #' Internal : store issue
    addIssue = function(description) {
      self$issues <- c(self$issues, description)
    },
    #' Internal : store success
    addSucess = function(description) {
      self$success <- c(self$success, description)
    },
    #' Internal stores
    issues = character(0),
    success = character(0),
    results = data.frame(
      description = character(0),
      success = logical(0),
      message = character(0)
    )
  )
)
