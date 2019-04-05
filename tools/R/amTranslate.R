#' Search for single word to translate
#'
#' @param id {Character} Translate id
#' @param str {Character} Text for the default language
#' @param lang {Character} Lang to use for the translation
#' @return Translated word
amTranslateSingle = function(id){
  if(amNoDataCheck(id)) stop("No id")

  language <- amTranslateGetSavedLanguage()
  languageDefault <- config$languageDefault
  dict <- config$dict
  item <- dict[dict$id==id,]
  if(amNoDataCheck(item)){
    return(id)
  }
  #
  # Get language tronslation for this item or fallback
  #
  str <- item[,c(language)]

  if(amNoDataCheck(str)){
    str <- item[,c(languageDefault)]
  }
  if(amNoDataCheck(str)){
    str <- id
  }

  return(str)
}
ams <- amTranslateSingle

#' Get saved language
#'
#' @return language code
amTranslateGetSavedLanguage <- function(){
  if(!file.exists(config$pathLanguageFile)){
    amTranslateSetSavedLanguage(config$languageDefault)
  }
  language <- readLines(config$pathLanguageFile)
  if(amNoDataCheck(language)){
    language <- config$languageDefault
  }
  return(language)
}

#' Set saved language
#' 
#' @param {character} language Language code
#' @return language code
amTranslateSetSavedLanguage <- function(language){
  if(amNoDataCheck(language)){
    language  = config$languageDefault
  }
  write(language,config$pathLanguageFile)
  return(language)
}

#' Update dict languages columns
#'
#' @rdname amTranslateSingle
amTranslateDictUpdateLanguages <-function(){
  languages <- config$dictLanguages
  dict <- config$dict
  for(l in languages){
    #
    # In case of new language: fill with language default
    #
    if( !isTRUE(l %in% names(dict)) ){
      newCol <- data.frame(new = sapply(
          1:nrow(dict),
          function(x){
            character(1)
          })
        )
      names(newCol) <- l
      dict <- cbind(dict,newCol)
      amTranslateWriteDict(dict)
    }
  }
}

#' Write dictionnary
#'
#' @param dict {Dataframe} dictionnary to write
amTranslateWriteDict <- function(dict){
  config$dict <<- dict
  dictJSON <- jsonlite::toJSON(dict,auto_unbox=TRUE,pretty=TRUE)
  write(dictJSON,file=config$pathDictMain)
}


#' Update default text for a given item
#'
#' @rdname amTranslateSingle
amTranslateTag <- function(id,children=NULL){
  str <- amTranslateSingle(id)
  tags$span(`data-amt_id`= id,str,children)
}
amt <- amTranslateTag 

#' Client side translation
#'
amTranslateSetLanguageClient <- function(lang=config$languageDefault,session=shiny:::getDefaultReactiveDomain()){
  session$sendCustomMessage(
    type="amSetLanguage",
    list(
      lang = lang,
      langInit = config$languageDefault,
      langDefault = config$languageDefault
      )
    )
}
