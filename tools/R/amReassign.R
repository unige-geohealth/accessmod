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


#' Reassign system2 to make use of amGrass sessions
#' -> workaround to sessions from rgrass
#'
amReasign("base", "system2", function(...) {
  strenv <- ""
  args <- list(...)
  amg <- amGrassSessionGet()

  for (n in c("mapset", "location_name", "gisrc", "gis_lock", "grass_overwrite")) {
    if (!isEmpty(n)) {
      strenv <- paste0(strenv, "export ", toupper(n), "=", amg[[n]], ";")
    }
  }

  if (isEmpty(args$stdout)) {
    args$stdout <- TRUE
  }

  args$env <- strenv

  do.call(
    "system2_orig",
    args
  )
})

#' Reassign system to make use of amGrass sessions
#' -> workaround to sessions from rgrass
#'
amReasign("base", "system", function(...) {
  strenv <- ""
  args <- list(...)
  amg <- amGrassSessionGet()

  for (n in c("mapset", "location_name", "gisrc", "gis_lock", "grass_overwrite")) {
    if (!isEmpty(n)) {
      strenv <- paste0(strenv, "export ", toupper(n), "=", amg[[n]], ";")
    }
  }
  args[[1]] <- paste0(strenv, args[[1]])

  do.call(
    "system_orig",
    args
  )
})



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

#' Auto remove layer if overwrite is true, to avoid warnings
#'
amReasign("rgrass7", "execGRASS", function(...) {
  args <- list(...)
  cmd <- args[[1]]
  flags <- args$flags
  output <- args$output
  cmdIgnore <- c("v.patch")

  if (isNotEmpty(flags) && isNotEmpty(output)) {
    if ("overwrite" %in% flags && !cmd %in% cmdIgnore) {
      type <- amGuessOutputType(args[[1]])
      if (isNotEmpty(type)) {
        rmLayerIfExists(output, type = type)
      }
    }
  }

  do.call("execGRASS_orig", args)
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
        # to inspect what's wrong using browser()
        stop(e)
      }
    )
  })
}
