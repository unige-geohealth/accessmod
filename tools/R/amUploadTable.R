#' Upload new data
#' @param config {list} am5 config list
#' @param dataName {string} data name
#' @param dataFile {path} data file path
#' @param dataClass {string} data class
#' @param dbCom {dbcon} db connection object
#' @param pBarTitle {string} progress bar title
#' @name upload_data
#' @export
amUploadTable <- function(config, dataName, dataFile, dataClass, dbCon, pBarTitle) {
  tbl <- import(dataFile)

  if (!exists("tbl")) {
    stop(paste("AccessMod could not read the provided file. Try another compatible format:", config$filesAccept$table))
  }

  progressBarControl(
    visible = TRUE,
    percent = 30,
    title = pBarTitle,
    text = "Data validation..."
  )
  # remove column containing NA's
  # search for expected column names
  aNames <- config$tableColNames[[dataClass]]

  if (is.null(aNames)) stop(paste("No entry found in config for class:", dataClass))

  # count remaining row
  hasRow <- nrow(tbl) > 0

  if (!hasRow) {
    stop(
      sprintf("Table %s doesn't have row(s).", dataName)
    )
  }

  tNames <- tolower(names(tbl))
  if (!all(aNames %in% tNames)) {
    aNamesP <- paste(aNames, collapse = "; ", sep = " ")
    tNamesP <- paste(tNames, collapse = "; ", sep = " ")
    errMsg <- sprintf(
      "Importation of %s : dataset of class %s shoud contains columns named \n %s. Columns name of the provided file:\n %s",
      basename(dataFile),
      dataClass,
      aNamesP,
      tNamesP
    )
    stop(errMsg)
  }
  names(tbl) <- tNames
  tbl <- tbl[, aNames] # keep only needed columns


  progressBarControl(
    visible = TRUE,
    percent = 90,
    title = pBarTitle,
    text = "Writing in db..."
  )
  dbWriteTable(dbCon, dataName, tbl, overwrite = TRUE)
  amDebugMsg("Table", dataName, " written in DB")
}

