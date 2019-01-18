#' Search for single word to translate
#'
#' @param id {Character} Translate id
#' @param str {Character} Text for the default language
#' @param lang {Character} Lang to use for the translation
#' @return Translated word
amTranslateSingle = function(id,str="",lang=config$langInit){
  if(amNoDataCheck(id)) stop("No id")

  langDefault <- config$langDefault
  rebuildMode <- config$dictRebuildMode
  languages <- config$dictLanguages

  if(amNoDataCheck(config$dict)){
    config$dict <<- amTranslateInitDict(config$pathDictMain)
  }

  dict <- config$dict
  hasStr <- !amNoDataCheck(str)
  lang <- ifelse(amNoDataCheck(lang),langDefault,lang)
  str <- ifelse(hasStr,str,id)
  item <- dict[dict$id==id,]

  if( rebuildMode ){
    #
    # Check that all languages have
    # a dedicated column and create 
    # one if needed.
    # NOTE: this should not be done 
    # at each query. Maybe once at start ?
    #
    amTranslateDictUpdateLanguages()
  }

  if( amNoDataCheck(item) ){
    if( rebuildMode && hasStr ){
      #
      # Add new item
      #
      item <- amTranslateDictAddItem(id,str)
    }else{
      #
      # Return str as default
      #
      return(str)
    }
  }
  if( rebuildMode && hasStr ){

    if(str != item[,langDefault]){
      #
      # Update default item
      #
      item <- amTranslateDictUpdateDefaultItem(id,str) 
    }
  }


  item <- dict[dict$id==id,]
  #
  # Get language tronslation for this item or fallback
  #
  translated <- item[,c(lang)]

  if(amNoDataCheck(translated)){
    translated <- item[,c(langDefault)]
  }
  if(amNoDataCheck(translated)){
    translated <- str
  }

  return(translated)
}

#' Init dictionnary
#' 
#' @param pathDict {Character} Path where to load or save dictionnary
#' @return {Dataframe} dictionnary
amTranslateInitDict = function(pathDict){

  languages <- config$dictLanguages
  pathDict <- normalizePath(pathDict)
  dict <- data.frame(id=character(1))
  for(l in languages){
    dict[,l] <- character(1)
  }
  names(dict)<-c("id",languages)

  if(!file.exists(pathDict)){
    write(jsonlite::toJSON(dict),pathDict)
  }
  sDict <- as.data.frame(jsonlite::fromJSON(pathDict))

  if(!amNoDataCheck(sDict)){
    dict <- sDict
  }
  return(dict)
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


#' Add new dict item and set defaut to str
#'
#' @rdname amTranslateSingle
amTranslateDictAddItem <-function(id,str){
  languages <- config$dictLanguages
  pathDictFile <- config$pathDictMain
  langDefault <- config$langDefault
  dict <- config$dict
  item <- data.frame(id=id)

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
    }
    #
    # In case of new language: fill with language default
    #
    item[l] = ifelse(l==langDefault,str,character(1))
  }
  dict <- rbind(item,dict)
  amTranslateWriteDict(dict)
  item <- dict[dict$id==id,]
  return(item)
}

#' Update default text for a given item
#'
#' @rdname amTranslateSingle
amTranslateDictUpdateDefaultItem <-function(id,str){
  langDefault <- config$langDefault
  dict <- config$dict
  dict[dict$id==id,c(langDefault)] <- str 
  amTranslateWriteDict(dict)
  item <- dict[dict$id==id,]
  return(item)
}

#' Write dictionnary
#'
#' @param dict {Dataframe} dictionnary to write
amTranslateWriteDict <- function(dict){
  config$dict <<- dict
  dictJSON <- jsonlite::toJSON(dict,auto_unbox=TRUE,pretty=TRUE)
  write(dictJSON,file=config$pathDictMain)
}


#' Translate multiple word
#'
#' @param ids {List|Character} List or vector of id to translate
#' @param lang {Character} Target lang
amTranslateMultiple <- function(ids,lang=config$langInit){
  sapply(ids,amTranslateSingle,i,"",lang);
}

#' Update default text for a given item
#'
#' @rdname amTranslateSingle
amTranslateTag <- function(id,str,lang=config$langInit){
  str <- amTranslateSingle(id,str,lang)
  tags$span(`data-amt_id`= id,str)
}
amt <- amTranslateTag 

#' Client side translation
#'
amTranslateSetLanguageClient <- function(lang=config$langInit,session=shiny:::getDefaultReactiveDomain()){
  session$sendCustomMessage(
    type="amSetLanguage",
    list(
      lang = lang,
      langInit = config$langInit,
      langDefault = config$langDefault
      )
    )
}
