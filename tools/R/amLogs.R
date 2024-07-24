#' Message
#'
#' @param session Shiny session
#' @param type Message type : error, warnin, message, log, ui
#' @param text Message text
#' @param subtitle Optional subtitle
#' @param logDile For type 'log', file to write in
amMsg <- function(session = shiny:::getDefaultReactiveDomain(),
  type = c("error", "warning", "message", "log", "ui"),
  text,
  title = NULL,
  subtitle = NULL,
  logFile = config$pathLog,
  ...) {
  type <- match.arg(type)
  if (is.null(title)) title <- type
  if (is.null(subtitle)) subtitle <- type
  stopifnot(!length(logFile) == 0)

  if ("html" %in% class(text) || "shiny.tag.list" %in% class(text)) {
    textLog <- amCleanHtml(paste(text))
  } else {
    textLog <- text
  }

  textLog <- gsub("[\r\n]", "", textLog)
  textLog <- gsub("\"", "", textLog, fixed = T)
  textLog <- gsub("  ", "", textLog)

  if (!type == "ui") {
    # NOTE: why not write.table...append=T = or fwrite ?
    write(
      paste(
        amSysTime(),
        "\t",
        type,
        "\t",
        textLog,
        collapse = " "
      ),
      file = logFile,
      append = TRUE
    )
  }

  if (type == "log") {
    return(NULL)
  }

  if (type == "error") {
    pbc(visible = FALSE)
  }

  amUpdateModal(
    panelId = "amModal",
    html = text,
    title = title,
    subtitle = subtitle,
    ...
  )
}

# read only a subset of last lines
amReadLogs <- function(logFile = config$pathLog,
  nToKeep = config$nLogDefault) {
  tblOut <- data.frame(
    "time" = character(0),
    "type" = character(0),
    "msg" = character(0)
  )
  # ┌────────────┐ <- oldest
  # │            │
  # │            │
  # ├────────────┤ <- nToKeep
  # └────────────┘ <- newest
  raw <- system(
    sprintf(
      "tail -n %s %s",
      nToKeep,
      config$pathLog
    ),
    intern = T
  )
  tbl <- read.csv(
    text = raw,
    header = F,
    stringsAsFactors = F,
    sep = "\t"
  )
  if (nrow(tbl) > 0) {
    names(tbl) <- names(tblOut)
    tblOut <- rbind(tblOut, tbl)
  }

  tblOut <- tblOut[order(tblOut$time, decreasing = T), ]

  return(tblOut)
}

#' Display a time stamp for CLI
#'
#' @param text Text to display in the middle
#' @example amTimeStamp("demo")
#' # ------------------------------- DEMO ------------------------------- #
#'                          2022-08-19@15_08_11
#' # -------------------------------------------------------------------- #
amTimeStamp <- function(text = NULL) {
  if (is.null(text)) text <- "AccessMod"
  w <- 68
  t <- amSysTime()
  u <- toupper(text)
  uS <- (w - nchar(u) - 2) / 2
  tS <- (w - nchar(t) - 2) / 2
  sideH <- paste(rep("-", uS), collapse = "")
  sideT <- paste(rep(" ", tS), collapse = "")
  head <- paste("#", sideH, u, sideH, "#", collapse = "")
  body <- paste(" ", sideT, t, sideT, " ", collapse = "")
  sideF <- paste(rep("-", nchar(head) - 4), collapse = "")
  foot <- paste("#", sideF, "#", collapse = "")
  cat(c(head, body, foot, collapse = ""), sep = "\n")
}

#' Custom debug message.
#'
#' @param ... anything printable
amDebugMsg <- function(...) {
  mode <- config$logMode
  if ("debug" %in% mode) {
    msg <- jsonlite::toJSON(
      list(...),
      auto_unbox = T,
      pretty = T
    )
    cat(paste("{ debug", amSysTime(), "}", msg), sep = "\n")
  }
}
amDebugMsgPerf <- function(title, time) {
  mode <- config$logMode
  if ("perf" %in% mode) {
    cat(sprintf("{ perf %s } %s\n", title, time))

    pExists <- file.exists(config$pathPerf)

    write.table(data.frame(t = Sys.time(), a = title, d = time),
      config$pathPerf,
      sep = ",",
      row.names = FALSE,
      col.names = !pExists,
      append = pExists
    )
  }
}

# format Sys.time to avoid spaces.
amSysTime <- function(type = c("fancy", "compatible", "short")) {
  if (is.null(type)) type <- "fancy"
  type <- match.arg(type)
  t <- Sys.time()
  tf <- switch(type,
    "fancy" = "%Y-%m-%d@%H_%M_%S",
    "compatible" = "%Y_%m_%d_%H_%M_%S",
    "short" = "%Y%m%d%H%M%S"
  )
  format(t, format = tf)
}
