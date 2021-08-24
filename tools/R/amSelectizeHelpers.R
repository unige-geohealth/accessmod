
#' Update select input after validation
#' @param session Shiny session
#' @param idData AccessMod data identifier 
#' @param idSelect Shiny select input to update
#' @param dataList AccessMod reactive dataList
#' @param addChoices Additional choices (will also be used as select item name)
#' @param emptySelected Force empty selected
#' @param selected Set selected item
#' @param debug Use debug mode (devel only)
#' @export
amUpdateSelectChoice<-function(
  session = shiny::getDefaultReactiveDomain(),
  idData = NULL,
  idSelect = NULL,
  dataList = NULL,
  addChoices = NULL,
  emptySelected = TRUE,
  selected = NULL,
  debug = FALSE
  ){

  if(is.null(idData) | is.null(idSelect) | is.null(dataList)) {
    amDebugMsg("amUpdateSelect Choice for",idSelect,"has null in idData, idSelect or dataList") 
    return()
  }

  dat <- amListData(idData,dataList)

  if(!is.null(addChoices)){
    dat  <- c(addChoices,dat)
  }

  if(length(dat) == 0) dat = config$defaultNoData 

  selectNew <- dat[1]
  hasSelected <- !is.null(selected) && selected %in% dat

  for(id in idSelect){
    if(hasSelected){
      selectNew <- selected
    }else{
      #
      # Keep the same selected value if it exists
      #
      selectOld <- session$input[[id]]
      hasSelectOld <- selectOld %in% dat 
      if( hasSelectOld ){
        selectNew <- selectOld 
      }
      if( !hasSelectOld && emptySelected ){
        selectNew <- ''
      }
    }

    cached <- dataList$cache_selectize[[id]]

    params <- list(
      inputId = id,
      choices = dat,
      selected = selectNew,
      options = list(
        placeholder = ams('placeholder_enter_value')
      )
    )

    hasChange <- !identical(cached,params)

    if(hasChange){
      do.call(amUpdateSelectizeInput,params)
      dataList$cache_selectize[[id]] <- params
    }

  }
}

#' Wrapper around updateSelectizeInput
#' @param inputId
#' @param choices
#' @param selected
#' @param options
amUpdateSelectizeInput <- function(
  inputId = NULL, 
  choices = NULL, 
  selected = NULL,
  options = NULL
  ){
  updateSelectizeInput(
    session = shiny::getDefaultReactiveDomain(),
    inputId = inputId,
    choices =  choices,
    selected = selected,
    options = options
  )
}


