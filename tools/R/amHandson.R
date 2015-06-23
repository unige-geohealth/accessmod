# Forked from https://github.com/AnalytixWare/ShinySky
# Licence:The MIT License (MIT)


#' hot.to.df
#' 
#' Converts the table data passed from the client-side into a data.frame
#' 
#' @param b The input$hotable_id value.
#'   
#' @export
hot.to.df <- function(b) {
  require(plyr)
  # if theres is no data
  if (length(b$data) == 0) 
    return() 

  col.names <- unlist(b$colHeaders)
  #i = 0
  f <- function(x) {
   # i <<- i + 1
    #
    null.pos <- sapply(x,is.null)

    x[null.pos] <- NA

    xx <- data.frame(x, stringsAsFactors = F)
    colnames(xx) <- col.names
    xx
  }

  bb <- ldply(b$data, f)
  colnames(bb) <- col.names
  bb
}


#' hotable
#' 
#' Creates a hotable (handsontable)
#' 
#' @param id The id used to refer to the table input$id or output$id
#'   
#' @export
hotable <- function(id) {
  tagList(        
    #singleton(tags$head(tags$link(href = "shinysky/handsontable/0.10.3/jquery.handsontable.full.css", rel = "stylesheet"))),
    #singleton(tags$head(tags$script(src = "shinysky/handsontable/0.10.3/jquery.handsontable.full.js"))),
    #singleton(tags$head(tags$script(src = "shinysky/hotable.js"))),
    div(id = id, class = "hotable")
    )

}

#' renderHotable
#' 
#' Renders the hotable.
#' 
#' @param expr The computation that leads to an output
#' @param env The R environment in which to create the dataset
#' @param quoted Pass to the exprToFunction
#' @param options Pass to the exprToFunction
#' @param readOnly A vector of TRUE/FALSE values to indicate which of the 
#'   columns should be readonly. If numeric vector, select col number to set as readOnly.
#' @param fixedCols A vector of integer of columns number to fix.
#' 

#' @export
renderHotable <- function(expr, env = parent.frame(), quoted = FALSE, 
  options = NULL, readOnly = NULL, fixedCols=1, stretched=c('all','last','none')) {
  func <- shiny::exprToFunction(expr, env, quoted)

  function() {
    df <- func()  # the dataframe returned        
    if (is.null(df)) {
      return()
    }
    
    if (nrow(df) == 0) {
      return()
    } 
        
    json <- NULL
    json$colHeaders <- colnames(df)
    columns <- NULL
    types <- sapply(df, typeof)

    l <- length(types)

    if(is.null(readOnly)){
      readOnly <- rep(TRUE,length.out = l)
    }else{
      if(is.logical(readOnly)){
        readOnly <- rep(readOnly[1],length.out = l)
      }else if(is.numeric(readOnly)){
        readOnly<-1:l %in% readOnly
      }
    }


    for (i in 1:l) {
        if (types[i] == "double") {
        columns[[i]] <- list(type = "numeric", format = "0,0.00", readOnly = readOnly[i])
      } else if (types[i] == "logical") {
        columns[[i]] <- list(type = "checkbox", readOnly = FALSE)
      } else {
        columns[[i]] <- list(readOnly = readOnly[i])
      }
    }
    json$columns <- columns
    json$data <- df
    json$fixedCols <- fixedCols
    json$stretched <- stretched
    json
  }
} 
