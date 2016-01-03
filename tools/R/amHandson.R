# Forked from https://github.com/AnalytixWare/ShinySky
# Licence:The MIT License (MIT)


#' hot.to.df
#' 
#' Converts the table data passed from the client-side into a data.frame
#' 
#' @param b The input$hotable_id value.
#'   
#' @export
hot.to.df <- function(b,colNames=NULL) {
  require(plyr)
  # if theres is no data

  if (length(b$data) == 0){
    return() 
  }
  
  if(is.null(colNames)){
  col.names <- unlist(b$colHeaders)
  }else{
  col.names <- colNames
  }

  if( ! length(col.names) == length(b$data[[1]])){
  stop("hot.to.df : number of column and number column header do not match. use colNames parameter?")
  }
  
  f <- function(x) {
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
hotable <- function(id,width="100%",height="100%") {
  tagList(        
    div(style=paste("width:",width,";height:",height,";overflow:auto;"),
      div(id = id,  class = "hotable")
      )
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
renderHotable <- function(
  expr, 
  env = parent.frame(), 
  quoted = FALSE, 
  options = NULL, 
  readOnly = NULL, 
  hide = NULL,
  fixedCols=1, 
  nSpareRow=0,
  maxRows=NULL,
  stretched=c('all','last','none')) 
{
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


    if(!is.null(maxRows) && is.numeric(maxRows)){
      maxRows <- as.integer(maxRows)
    }else{
      maxRows <- nrow(df)
    }

    for (i in 1:l) {
      if(! i %in% hide ){
        if (types[i] == "double") {
          columns[[i]] <- list(type = "numeric", format = "0,0.00", readOnly = readOnly[i])
        } else if (types[i] == "logical") {
          columns[[i]] <- list(type = "checkbox", readOnly = FALSE)
        } else {
          columns[[i]] <- list(readOnly = readOnly[i])
        }
      }
    }

    json$colHeaders <- colnames(df) 
    json$columns <- columns
    json$data <- df
    json$fixedCols <- fixedCols
    json$stretched <- stretched
    json$nSpareRow <- 0
    json$maxRows <- maxRows 
    json
  }
} 
