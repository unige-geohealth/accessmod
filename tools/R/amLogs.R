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

#' Return an empty log table with the stable display schema
#'
#' @return A zero-row data frame with time, type, and msg character columns.
amEmptyLogTable <- function() {
  data.frame(
    "time" = character(0),
    "type" = character(0),
    "msg" = character(0),
    stringsAsFactors = FALSE
  )
}

#' Read the last physical lines of a file
#'
#' @param path File to read.
#' @param n Maximum number of lines to return.
#' @return A character vector. Read failures return an empty vector.
amReadLastLines <- function(path, n) {
  n <- suppressWarnings(as.integer(n))
  if (
    length(path) != 1 || is.na(path) || !file.exists(path) ||
      length(n) != 1 || is.na(n) || n <= 0 || file.info(path)$size == 0
  ) {
    return(character(0))
  }

  tryCatch(
    {
      raw <- system2(
        "tail",
        args = c("-n", n, shQuote(path)),
        stdout = TRUE,
        stderr = FALSE
      )
      if (!is.null(attr(raw, "status"))) character(0) else raw
    },
    error = function(cond) character(0)
  )
}

#' Atomically clear a log file
#'
#' @param logFile Log file to clear.
#' @return Logical. TRUE when the empty file replaced the previous log.
amLogClear <- function(logFile = config$pathLog) {
  if (length(logFile) != 1 || is.na(logFile) || !dir.exists(dirname(logFile))) {
    stop("log directory is unavailable")
  }

  tmpLog <- tempfile(pattern = ".logs-empty-", tmpdir = dirname(logFile))
  on.exit(unlink(tmpLog), add = TRUE)

  if (!file.create(tmpLog)) {
    stop("could not create the empty log")
  }
  if (!file.rename(tmpLog, logFile)) {
    stop("could not replace the original log")
  }

  TRUE
}

#' Trim the global log at process startup
#'
#' The file is replaced only after a complete compacted copy has been written
#' in the same directory. This function intentionally warns to stderr rather
#' than writing failures back into the log it is maintaining.
#'
#' @param logFile Log file to compact.
#' @param trimTrigger Compact only above this number of physical lines.
#' @param nToRetain Number of newest physical lines to retain.
#' @return Logical. TRUE when the file was compacted, FALSE otherwise.
amLogTrim <- function(logFile = config$pathLog,
  trimTrigger = config$nLogTrimTrigger,
  nToRetain = config$nLogRetain) {
  trimTrigger <- suppressWarnings(as.integer(trimTrigger))
  nToRetain <- suppressWarnings(as.integer(nToRetain))

  if (
    length(logFile) != 1 || is.na(logFile) || !file.exists(logFile) ||
      length(trimTrigger) != 1 || is.na(trimTrigger) ||
      length(nToRetain) != 1 || is.na(nToRetain) ||
      nToRetain <= 0 || trimTrigger <= nToRetain
  ) {
    return(FALSE)
  }

  tryCatch(
    {
      if (R.utils::countLines(logFile) <= trimTrigger) {
        return(FALSE)
      }

      tmpLog <- tempfile(pattern = ".logs-", tmpdir = dirname(logFile))
      on.exit(unlink(tmpLog), add = TRUE)

      status <- system2(
        "tail",
        args = c("-n", nToRetain, shQuote(logFile)),
        stdout = tmpLog,
        stderr = FALSE
      )
      if (!identical(status, 0L) || !file.exists(tmpLog)) {
        stop("could not write the compacted log")
      }
      if (!file.rename(tmpLog, logFile)) {
        stop("could not replace the original log")
      }

      TRUE
    },
    error = function(cond) {
      warning(
        sprintf("AccessMod log compaction failed: %s", cond$message),
        call. = FALSE
      )
      FALSE
    }
  )
}

#' Read a subset of the newest valid log entries
#'
#' Empty logs are valid. Incomplete records can be left behind by an interrupted
#' append and are ignored without attempting to write into the same log.
amReadLogs <- function(logFile = config$pathLog,
  nToKeep = config$nLogDefault) {
  tblOut <- amEmptyLogTable()
  # ┌────────────┐ <- oldest
  # │            │
  # │            │
  # ├────────────┤ <- nToKeep
  # └────────────┘ <- newest
  raw <- amReadLastLines(logFile, nToKeep)
  raw <- raw[nzchar(trimws(raw))]
  if (length(raw) == 0) {
    return(tblOut)
  }

  firstTab <- regexpr("\t", raw, fixed = TRUE)
  afterFirst <- substring(raw, firstTab + 1L)
  secondTab <- regexpr("\t", afterFirst, fixed = TRUE)
  valid <- firstTab > 0L & secondTab > 0L
  valid <- valid & nzchar(trimws(substr(raw, 1L, firstTab - 1L)))
  valid <- valid & nzchar(trimws(substr(afterFirst, 1L, secondTab - 1L)))
  if (!any(valid)) {
    return(tblOut)
  }

  tblOut <- data.frame(
    time = trimws(substr(raw[valid], 1L, firstTab[valid] - 1L)),
    type = trimws(substr(afterFirst[valid], 1L, secondTab[valid] - 1L)),
    msg = trimws(substring(afterFirst[valid], secondTab[valid] + 1L)),
    stringsAsFactors = FALSE
  )
  tblOut <- tblOut[order(tblOut$time, decreasing = T), ]
  rownames(tblOut) <- NULL

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
