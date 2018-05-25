#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/



#' hotToDf
#' 
#' Converts the table data passed from the client-side into a data.frame
#' 
#' @param b The input$hotable_id value.
#'   
#' @export
hotToDf <- function(b,colNames=NULL,debug=F) {

  if (length(b$data) == 0){
    return() 
  }

   df = as.data.frame(jsonlite::fromJSON(b$data),stringsAsFactors=FALSE)

  if(!is.null(colNames)){
  colnames(df) <- colNames
  }
  return(df)
  
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
  columnHeaders = NULL,
  hide = NULL,
  fixedCols=1, 
  nSpareRow=0,
  maxRows=NULL,
  stretched=c('all','last','none'),
  dropDown = list("mode"=c("WALKING","MOTORIZED","BICYCLING")),
  idToolsFilter = NULL
  )
{
  func <- shiny::exprToFunction(expr, env, quoted)

  function() {

    df <- func()  # the dataframe returned        
    if (is.null(df)) {
      return()
    }
   
    fixedCols <- NULL
        
    json <- NULL
    
    columns <- NULL
    
    types <- sapply(df, typeof)

    colNames <- colnames(df)
    
    if( is.null(columnHeaders)){
      columnHeaders = colNames
    }

    l <- length(types)

    if(is.null(readOnly)){
      readOnly <- rep(TRUE,l)
    }else{
      if(is.logical(readOnly)){
        if(length(readOnly) != length(df)){
          readOnly <- rep(readOnly[1],l)
        }
      }else if(is.numeric(readOnly)){
        readOnly <- 1:l %in% readOnly
      }
    }

    if(!is.null(maxRows) && is.numeric(maxRows)){
      maxRows <- as.integer(maxRows)
    }else{
      maxRows <- nrow(df)
    }

    for (i in 1:l) {

      columns[[i]] = list(
        data = colNames[i],
        readOnly = readOnly[i]
        )

      if( i  %in% hide ){
        columns[[i]]$width=1
      }else{
        columns[[i]]$width=NULL
      }

      if(!is.null(colNames) && !is.null(dropDown[[colNames[i]]])){
        columns[[i]]$type <- "dropdown"
        columns[[i]]$source <- dropDown[[colNames[i]]]
      }else if (types[i] == "double") {
        columns[[i]]$type = "numeric"
        columns[[i]]$format = "0,0.00"  
      } else if (types[i] == "logical") {
        columns[[i]]$type = "checkbox"
      } 

    }

  return(list(
      colHeaders = columnHeaders,
      columns = columns,
      data =   jsonlite::toJSON(df),
      fixedCols = fixedCols,
      stretched = stretched,
      nSpareRow = 0,
      maxRows = maxRows,
      idToolsFilter = idToolsFilter
      ))

  }
} 



