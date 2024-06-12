#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

#' Search for single word to translate
#'
#' @param id {Character} Translate id
#' @param str {Character} Text for the default language
#' @param lang {Character} Lang to use for the translation
#' @return Translated word
amTranslateSingle <- function(id) {
  if (isEmpty(id)) {
    if (id != config$defaultNoData) {
      stop("No id")
    }
  }

  language <- amTranslateGetSavedLanguage()
  languageDefault <- config$languageDefault
  dict <- config$dict
  item <- dict[dict$id == id, ]
  if (isEmpty(item)) {
    return(id)
  }
  #
  # Get language tronslation for this item or fallback
  #
  str <- item[, c(language)]

  if (isEmpty(str)) {
    str <- item[, c(languageDefault)]
  }
  if (isEmpty(str)) {
    str <- id
  }

  return(str)
}
ams <- amTranslateSingle

#' Get saved language
#'
#' @return language code
amTranslateGetSavedLanguage <- function() {
  if (!file.exists(config$pathLanguageFile)) {
    amTranslateSetSavedLanguage(config$languageDefault)
  }
  language <- readLines(config$pathLanguageFile)
  if (isEmpty(language)) {
    language <- config$languageDefault
  }
  return(language)
}

#' Set saved language
#'
#' @param {character} language Language code
#' @return language code
amTranslateSetSavedLanguage <- function(language) {
  if (isEmpty(language)) {
    language <- config$languageDefault
  }
  write(language, config$pathLanguageFile)
  return(language)
}

#' Update dict languages columns
#'
#' @rdname amTranslateSingle
amTranslateDictUpdateLanguages <- function() {
  languages <- config$dictLanguages
  dict <- config$dict
  for (l in languages) {
    #
    # In case of new language: fill with language default
    #
    if (!isTRUE(l %in% names(dict))) {
      newCol <- data.frame(new = sapply(
        1:nrow(dict),
        function(x) {
          character(1)
        }
      ))
      names(newCol) <- l
      dict <- cbind(dict, newCol)
      amTranslateWriteDict(dict)
    }
  }
}

#' Write dictionnary
#'
#' @param dict {Dataframe} dictionnary to write
amTranslateWriteDict <- function(dict) {
  config$dict <<- dict
  dictJSON <- jsonlite::toJSON(dict, auto_unbox = TRUE, pretty = TRUE)
  write(dictJSON, file = config$pathDictMain)
}


#' Update default text for a given item
#'
#' @rdname amTranslateSingle
amTranslateTag <- function(id, children = NULL) {
  str <- amTranslateSingle(id)
  tags$span(`data-amt_id` = id, str, children)
}

amt <- amTranslateTag

#' Client side translation
#'
amTranslateSetLanguageClient <- function(lang = config$languageDefault, session = shiny:::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    type = "amSetLanguage",
    list(
      lang = lang,
      langInit = config$languageDefault,
      langDefault = config$languageDefault
    )
  )
}

amTranslateDefault <- function() {
  names(config$defaultNoData) <<- ams("no_data")
  names(config$defaultWithoutData) <<- ams("without_data")
  names(config$dynamicFacilities) <<- ams("vOutputFacility")
  names(config$dynamicPopulation) <<- ams("rOutputPopulation")
}
