#
# Unit tests for global log reading and startup compaction.
#

emptyLogCases <- local({
  logDir <- tempfile("accessmod_logs_empty_")
  dir.create(logDir)
  on.exit(unlink(logDir, recursive = TRUE))

  missingLog <- file.path(logDir, "missing.log")
  zeroByteLog <- file.path(logDir, "zero.log")
  blankLog <- file.path(logDir, "blank.log")
  file.create(zeroByteLog)
  writeLines(c("", "   "), blankLog)

  lapply(c(missingLog, zeroByteLog, blankLog), amReadLogs)
})

amtest$check(
  "Logs: missing and empty files return the stable empty schema",
  all(vapply(emptyLogCases, function(x) {
    identical(names(x), c("time", "type", "msg")) &&
      nrow(x) == 0 && all(vapply(x, is.character, logical(1)))
  }, logical(1)))
)

parsedLogs <- local({
  logDir <- tempfile("accessmod_logs_parse_")
  dir.create(logDir)
  on.exit(unlink(logDir, recursive = TRUE))
  logFile <- file.path(logDir, "custom log.txt")

  writeLines(c(
    "2026-09-04@10_00_00 \t log \t oldest",
    "interrupted record",
    "2026-09-04@10_02_00 \t warning \t newest\twith a tab",
    "2026-09-04@10_01_00 \t message \t middle"
  ), logFile)

  list(
    all = amReadLogs(logFile, nToKeep = 10),
    tail = amReadLogs(logFile, nToKeep = 2)
  )
})

amtest$check(
  "Logs: custom files are parsed, ordered, and malformed records are skipped",
  identical(parsedLogs$all$time, c(
    "2026-09-04@10_02_00",
    "2026-09-04@10_01_00",
    "2026-09-04@10_00_00"
  )) && identical(parsedLogs$all$msg[[1]], "newest\twith a tab")
)

amtest$check(
  "Logs: nToKeep limits physical input lines",
  nrow(parsedLogs$tail) == 2 &&
    identical(parsedLogs$tail$type, c("warning", "message"))
)

largeLog <- local({
  logFile <- tempfile("accessmod_logs_large_")
  on.exit(unlink(logFile))
  writeLines(sprintf(
    "2026-09-04@10_00_00 \t log \t diagnostic message %05d",
    seq_len(5000)
  ), logFile)
  amReadLogs(logFile, nToKeep = 5000)
})

amtest$check(
  "Logs: vectorized parser preserves a 5000-line fixture",
  nrow(largeLog) == 5000 &&
    identical(largeLog$msg[[1]], "diagnostic message 00001") &&
    identical(largeLog$msg[[5000]], "diagnostic message 05000")
)

clearedLog <- local({
  logFile <- tempfile("accessmod_logs_clear_")
  on.exit(unlink(logFile))
  writeLines(c("old entry one", "old entry two"), logFile)

  cleared <- amLogClear(logFile)
  amMsg(
    type = "log",
    text = "Log history cleared by user.",
    logFile = logFile
  )
  list(cleared = cleared, logs = amReadLogs(logFile, nToKeep = 10))
})

amtest$check(
  "Logs: clearing atomically removes history and accepts one audit entry",
  isTRUE(clearedLog$cleared) &&
    nrow(clearedLog$logs) == 1 &&
    identical(clearedLog$logs$type, "log") &&
    identical(clearedLog$logs$msg, "Log history cleared by user.")
)

failedClear <- local({
  logFile <- tempfile("accessmod_logs_failed_clear_")
  on.exit(unlink(logFile))
  original <- c("old entry one", "old entry two")
  writeLines(original, logFile)

  hadFileRename <- exists("file.rename", envir = .GlobalEnv, inherits = FALSE)
  if (hadFileRename) oldFileRename <- get("file.rename", envir = .GlobalEnv)
  assign("file.rename", function(...) FALSE, envir = .GlobalEnv)
  on.exit({
    if (hadFileRename) {
      assign("file.rename", oldFileRename, envir = .GlobalEnv)
    } else {
      rm("file.rename", envir = .GlobalEnv)
    }
  }, add = TRUE)

  cleared <- try(amLogClear(logFile), silent = TRUE)
  list(cleared = cleared, lines = readLines(logFile, warn = FALSE))
})

amtest$check(
  "Logs: failed clearing leaves the original file intact",
  inherits(failedClear$cleared, "try-error") &&
    identical(failedClear$lines, c("old entry one", "old entry two"))
)

trimmedLog <- local({
  logDir <- tempfile("accessmod_logs_trim_")
  dir.create(logDir)
  on.exit(unlink(logDir, recursive = TRUE))
  logFile <- file.path(logDir, "logs.txt")
  original <- sprintf("line-%03d", seq_len(15))
  writeLines(original[-length(original)], logFile)
  cat(original[[length(original)]], file = logFile, append = TRUE)

  trimmed <- amLogTrim(logFile, trimTrigger = 12, nToRetain = 10)
  list(trimmed = trimmed, lines = readLines(logFile, warn = FALSE))
})

amtest$check(
  "Logs: startup compaction retains exactly the newest configured lines",
  isTRUE(trimmedLog$trimmed) &&
    identical(trimmedLog$lines, sprintf("line-%03d", 6:15))
)

untrimmedLog <- local({
  logFile <- tempfile("accessmod_logs_untrimmed_")
  on.exit(unlink(logFile))
  original <- sprintf("line-%03d", seq_len(12))
  writeLines(original, logFile)

  trimmed <- amLogTrim(logFile, trimTrigger = 12, nToRetain = 10)
  list(trimmed = trimmed, lines = readLines(logFile, warn = FALSE))
})

amtest$check(
  "Logs: startup compaction does nothing at the trigger",
  identical(untrimmedLog, list(
    trimmed = FALSE,
    lines = sprintf("line-%03d", seq_len(12))
  ))
)

failedTrim <- local({
  logFile <- tempfile("accessmod_logs_failed_trim_")
  on.exit(unlink(logFile))
  original <- sprintf("line-%03d", seq_len(15))
  writeLines(original, logFile)

  hadSystem2 <- exists("system2", envir = .GlobalEnv, inherits = FALSE)
  if (hadSystem2) oldSystem2 <- get("system2", envir = .GlobalEnv)
  assign("system2", function(...) 1L, envir = .GlobalEnv)
  on.exit({
    if (hadSystem2) {
      assign("system2", oldSystem2, envir = .GlobalEnv)
    } else {
      rm("system2", envir = .GlobalEnv)
    }
  }, add = TRUE)

  trimmed <- suppressWarnings(
    amLogTrim(logFile, trimTrigger = 12, nToRetain = 10)
  )
  list(trimmed = trimmed, lines = readLines(logFile, warn = FALSE))
})

amtest$check(
  "Logs: failed compaction leaves the original file intact",
  identical(failedTrim, list(
    trimmed = FALSE,
    lines = sprintf("line-%03d", seq_len(15))
  ))
)
