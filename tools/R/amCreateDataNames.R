
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

  for (i in classes) {
    resFile[i] <- amAddTag(i, tag, T, F)
    resFileMapset[i] <- amAddTag(i, tag, T, T)
    resUi[i] <- amAddTag(amClassListInfo(i), tag, F, F)
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

#' Add tags to class
#'
#' @param class Input class. E.g. tScenarioOut
#' @param tags Vector of classes ex. c("a","b","c")
#' @param fileMode Target filename ex. tScenarioOut__a_b_c
#' @param mapsetMode Target layername ex. rSpeed__a_b_c@demo
#' @return Formated name
amAddTag <- function(class, tags, fileMode = TRUE, mapsetMode = FALSE) {
  out <- ""
  sepT <- config$sepTagRepl
  sepF <- config$sepTagFile
  sepC <- config$sepClass
  if (fileMode) {
    if (mapsetMode) {
      tags <- paste(tags, collapse = sepF)
      id <- paste(c(class, tags), collapse = sepC)
      out <- amAddMapset(id)
    } else {
      out <- paste(c(class, paste(tags, collapse = sepF)), collapse = sepC)
    }
  } else {
    out <- paste0(class, " [", paste(tags, collapse = sepT), "]")
  }
  out
}
