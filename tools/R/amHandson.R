#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.


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
hotable <- function(id,width="100%",height="50vh") {

  style <- sprintf('height:%s; width:%s; overflow:hidden; background: #f0f0f0; ',height,width)
  div(
    class="handson_tbl_container",       
    div(
      class="handson_tbl_tools_container"
      ),
    div(
      class="handson_tbl_table_container",
      style = "border-bottom:1px solid #ccc",
      div(
        class = "hotable hot handsontable htRowHeaders htColumnHeaders",
        style = style,
        id = id
      )
    )
  )
}

#' Update value of a column based on conditional
#' @param id Id of the table
#' @param col Column name to update
#' @param set Value to set the column with
#' @param whereCol Column to test
#' @param whereOp Operator to use : one of == => <= > < !=
#' @param whereVal Value to compare with
hotableUpdateValByCond <- function(id,col,set,whereCol,whereOp,whereVal,session=shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage("hotableSetColValuesByCond",
    list(
      id = id,
      col = col,
      set = set,
      whereCol = whereCol,
      whereOp = whereOp,
      whereVal = whereVal
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
#' @param toolsConditionalColumn List of configuration for conditional row selection. e.g.
#' list(
#'  idColumn = "cat",
#'  column = "amSelect",
#'  valueSet = TRUE,
#'  valueUnset = FALSE,
#'  columnSelectInput = !names(tbl) == 'amSelect'
#'  )
#'
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
  fixedCols = 2, 
  nSpareRow = 0,
  maxRows = NULL,
  stretched = c('all','last','none'),
  dropDown = list("mode"=c("WALKING","MOTORIZED","BICYCLING")),
  toolsConditionalColumn = NULL
)
{
  func <- shiny::exprToFunction(expr, env, quoted)

  function() {

    df <- func()  # the dataframe returned        
    if (is.null(df)) {
      return()
    }

    #fixedCols <- NULL

    json <- NULL

    columns <- NULL
    fId <- sapply(df, is.factor)
    df[fId] <- lapply(df[fId],as.character)
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
        header = columnHeaders[i],
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
      }else if (types[i] == "double" || types[i] == "integer" ) {
        columns[[i]]$type = "numeric"
        columns[[i]]$format = "0,0.00"
      } else if (types[i] == "logical") {
        columns[[i]]$type = "checkbox"
      }

    }

    return(list(
        colHeaders = columnHeaders,
        columns = columns,
        data =   jsonlite::toJSON(df, digits=10),
        fixedCols = fixedCols,
        stretched = stretched,
        nSpareRow = 0,
        maxRows = maxRows,
        toolsConditionalColumn = toolsConditionalColumn
        ))

  }
} 



