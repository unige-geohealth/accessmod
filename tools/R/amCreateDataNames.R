
#' Create list of name for ui, file, file with mapset and html (with validation)
#' @param classes Base classes to which append tags
#' @param tag Character vector containing some tags
#' @param dataList List of existing data name
#' @param outHtmlString Output a list of html string instead of tags
#' @export
amCreateNames <- function(classes, tag, dataList, outHtmlString = TRUE) {
  resFile <- character(0)
  resFileMapset <- character(0)
  resUi <- character(0)
  if (outHtmlString) {
    resHtml <- character(0)
  } else {
    resHtml <- tagList()
  }

  # keep unique tags
  tag <- amGetUniqueTags(tag)
  # add tag function
  addTags <- function(class, f = TRUE, m = TRUE) {
    out <- ""
    sepT <- config$sepTagRepl
    sepF <- config$sepTagFile
    sepC <- config$sepClass
    if (f) {
      if (m) {
        tags <- paste(tag, collapse = sepF)
        id <- paste(c(class, tags), collapse = sepC)
        out <- amAddMapset(id)
      } else {
        paste(c(class, paste(tag, collapse = sepF)), collapse = sepC)
      }
    } else {
      paste0(class, " [", paste(tag, collapse = sepT), "]")
    }
  }


  for (i in classes) {
    resFile[i] <- addTags(i, T, F)
    resFileMapset[i] <- addTags(i, T, T)
    resUi[i] <- addTags(amClassListInfo(i), F, F)
    type <- amClassListInfo(i, "type")
    hasDataList <- !isEmpty(dataList)
    hasData <- hasDataList && isTRUE(resFileMapset[i] %in% dataList[[type]])

    if (hasData) {
      if (outHtmlString) {
        resHtml[i] <- sprintf(
          " %s <b style=\"color:#FF9900\"> (overwrite warning)</b> ",
          resUi[i]
        )
      } else {
        resHtml <- tagList(
          resHtml,
          tags$b(
            class = "text-warning",
            resUi[i],
            "(overwrite warning)"
          )
        )
      }
    } else {
      if (outHtmlString) {
        resHtml[i] <- sprintf(
          "%s <b style=\"color:#00CC00\"> (ok)</b>",
          resUi[i]
        )
      } else {
        resHtml <- tagList(
          resHtml,
          tags$b(
            class = "text-info",
            resUi[i],
            "(ok)"
          )
        )
      }
    }
  }

  list(
    ui = resUi,
    file = resFile,
    fileMapset = resFileMapset,
    html = resHtml
  )
}
