#'
#' Hack space :
#'  - help debug base function
#'  - ovewrite package functions
#'  - additional behaviour
#'  - etc..
#'

#' Reassign object in given package (hack)
#' -> replacement can access original version
#'    using _<suffix>. Ex. is.finite_orig(...)
#'
#' @param pkgName Package name
#' @param name Object name
#' @param repl Replacement
#' @return void
#' @examples
#' amReasign("base", "is.finite", function(...) {
#'   cat("hey", is.finite_orig(...))
#' })
#'
amReasign <- function(pkgName, name, repl, suffix = "orig") {
  ePkg <- as.environment(sprintf("package:%s", pkgName))
  eGlob <- globalenv()
  name_orig <- sprintf("%s_%s", name, suffix)
  has_bkp <- !is.null(eGlob[[name_orig]])

  if (!has_bkp) {
    assign(
      x = name_orig,
      value = ePkg[[name]],
      envir = eGlob
    )
  }

  unlockBinding(name, ePkg)

  assign(
    x = name,
    value = repl,
    envir = ePkg
  )

  lockBinding(name, ePkg)
}

#' Reset TZ env variable before format
#' ⚠️ Something write TZ env variable and corrupts it !
#' probably https://github.com/wch/r-source/blob/tags/R-4-1-1/src/main/datetime.c
#' We can't do anything excepts rewrite back the correct TZ...
reset_tz <- function() {
  tz <- options("tz")
  if (is.null(tz)) {
    tz <- "UTC"
  }
  if (!identical(Sys.getenv("TZ"), tz)) {
    Sys.setenv(TZ = tz)
  }
}

amReasign("base", "format", function(...) {
  reset_tz()
  format_orig(...)
})


#'
#' gsub will fail if encoding is not utf8: it happens
#' with the TZ issue. Uncomment this to intercept here
#'
if (FALSE) {
  # if TRUE : debug mode
  amReasign("base", "gsub", function(...) {
    tryCatch(
      {
        gsub_orig(...)
      },
      error = function(e) {
        #
        # Use parent.frame(1-4) and .tracestac(1-5) to
        # to inspect what's wrong
        #
        browser()
        stop(e)
      }
    )
  })
}
