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



#' Trigger change dataListUpdate listener 
#'
#'
amUpdateDataList <- function(listen) {
  listen$dataListUpdate <- runif(1)
}


#' Search and organise data into dataList reactive function
#' @param dataList Reactive values object that will hold available data list
#' @return populate dataList
#' @export
amUpdateDataListObject <- function(dataList) {

  sessionValid <- amGrassSessionIsValid()

  if (!sessionValid) {
    return()
  }

  mapset <- amGrassSessionGetEnv('mapset')

  dbCon <- amMapsetGetDbCon()

  on_exit_add({
    dbDisconnect(dbCon)
  })

  rmTableIfExists("^tmp_*")
  rmVectIfExists("^tmp_*")
  rmRastIfExists("^tmp_*")
  archivesSelect <- amGetArchiveList(
    config$pathArchiveGrass,
    config$archiveBaseName
  )

  # extract data of type table only from sqlite
  sqlTables <- dbListTables(dbCon)

  # get all class of type table
  tblClasses <- config$dataClass[config$dataClass$type == "table", "class"]

  # filter only tables with valid table classes
  tables <- unlist(sapply(tblClasses, function(x) {
    v <- grep(paste0(x, config$sepClass), sqlTables, value = T)
  }))

  if (length(tables) > 0) {
    # create selectize input. E.g table_model__p003 >>
    # named list element :  $`table_model [p003]`
    # value [1] "table_model__p003@p_500_m"
    tablesSelect <- amCreateSelectList(
      dName = tables,
      sepTag = config$sepTagFile,
      sepClass = config$sepClass,
      mapset = mapset
    )
  } else {
    tablesSelect <- NULL
  }
  vectorsSelect <- amCreateSelectList(
    dName = execGRASS("g.list", type = "vector", intern = TRUE),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )
  rastersSelect <- amCreateSelectList(
    dName = execGRASS("g.list", type = "raster", intern = TRUE),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )
  shapesSelect <- amCreateSelectList(
    dName = names(amGetShapesList()),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )
  configSelect <- amCreateSelectList(
    dName = names(amGetConfigList()),
    sepTag = config$sepTagFile,
    sepClass = config$sepClass,
    mapset = mapset
  )

  #
  # Clean unnamed stuff : no class found, or invalid
  #

  # Remove unamed raster
  if (!is.null(rastersSelect)) {
    rastToRemove <- rastersSelect[is.na(names(rastersSelect))]
    if (isTRUE(length(rastToRemove) > 0)) {
      sapply(rastToRemove, function(x) {
        x <- unlist(strsplit(x, config$sepMapset))[1]
        message(paste("removing unnamed file", x))
        rmRastIfExists(x)
      })
    }
  }

  # Remove unamed vector
  if (!is.null(vectorsSelect)) {
    vectToRemove <- vectorsSelect[is.na(names(vectorsSelect))]
    if (isTRUE(length(vectToRemove)) > 0) {
      sapply(vectToRemove, function(x) {
        x <- unlist(strsplit(x, config$sepMapset))[1]
        message(paste("removing unnamed file", x))
        rmVectIfExists(x)
      })
    }
  }

  # Remove unamed tables
  if (!is.null(tablesSelect)) {
    tableToRemove <- tablesSelect[is.na(names(tablesSelect))]
    if (isTRUE(length(tableToRemove) > 0)) {
      sapply(tableToRemove, function(x) {
        x <- unlist(strsplit(x, config$sepMapset))[1]
        message(paste("removing unnamed file", x))
        sql <- paste("DROP TABLE IF EXISTS", x)
        dbGetQuery(dbCon, sql)
      })
    }
  }

  # Remove unamed shapefile
  if (!is.null(shapesSelect)) {
    shapesToRemove <- unique(c(
      shapesSelect[is.na(names(shapesSelect))],
      shapesSelect[grep(paste0("^NA", config$sepClass), shapesSelect)]
    ))
    if (isTRUE(length(shapesToRemove) > 0)) {
      sToRm <- amNoMapset(shapesToRemove)
      p <- system(paste("echo", config$pathShapes), intern = T)
      allShapes <- list.files(p)
      toRm <- sapply(sToRm, function(x) {
        file.path(p, allShapes[grep(paste0("^", x), allShapes)])
      }, simplify = T)
      toRm <- toRm[file.exists(toRm)]
      if (length(toRm) > 0) {
        message(paste("removing file", paste(toRm, sep = ",")))
        file.remove(toRm)
      }
    }
  }

  # Remove unnamed configs
  if (!is.null(configSelect)) {
    configToRemove <- unique(c(
      configSelect[is.na(names(configSelect))],
      configSelect[grep(paste0("^NA", config$sepClass), configSelect)]
    ))
    if (isTRUE(length(configToRemove) > 0)) {
      rToRm <- amNoMapset(configToRemove)
      p <- system(paste("echo", config$pathConfigs), intern = T)
      allConfigs <- list.files(p)
      toRm <- sapply(rToRm, function(x) {
        file.path(p, allConfigs[grep(paste0("^", x), allConfigs)])
      }, simplify = T)
      toRm <- toRm[file.exists(toRm)]
      if (length(toRm) > 0) {
        message(paste("removing file", paste(toRm, sep = ",")))
        file.remove(toRm)
      }
    }
  }

  dataList$raster <- rastersSelect
  dataList$vector <- vectorsSelect
  dataList$shape <- shapesSelect
  dataList$table <- tablesSelect
  dataList$archive <- archivesSelect
  dataList$config <- configSelect
  
  dataList$df <- rbind(
    amDataListToDf(tablesSelect, config$sepClass, "table"),
    amDataListToDf(vectorsSelect, config$sepClass, "vector"),
    amDataListToDf(rastersSelect, config$sepClass, "raster"),
    amDataListToDf(shapesSelect, config$sepClass, "shape"),
    amDataListToDf(configSelect, config$sepClass, "config")
  )

  dataList$tags <- amGetUniqueTags(dataList$df$tag)

}


# remove mapset part. E.g. for sqlite.
amNoMapset <- function(amData, sepMap = config$sepMapset) {
  amData <- as.character(amData)
  res <- unlist(strsplit(amData, paste0("(", sepMap, ").+")))
  if (length(res) == 0) {
    res <- NULL
  }
  res
}

# add mapset to a data name
amAddMapset <- function(amData, sepMap = config$sepMapset) {
  mapset <- paste0(sepMap, execGRASS("g.mapset", flags = "p", intern = T))
  return(paste0(amData, mapset))
}

# Get data class
#' @param amData Data name to evaluate
#' @param sepClass Class separator
#' @return Class name for given data
#' @export
amGetClass <- function(amData = NULL, sepClass = config$sepClass) {
  if (isEmpty(amData)) {
    return()
  }
  as.character(strsplit(unlist(amData), paste0("(\\", sepClass, ").*")))
}



#' Get type of data for given layer
#'
#' @param amData Data name to evaluate
#' @param config Accessmod configuration list
#' @return Type of data (vector, raster, table..)
#' @export
amGetType <- function(amData = NULL) {
  if (isEmpty(amData)) {
    return()
  }
  if (isTRUE(amData == config$newFacilitiesShort)) {
    return("vector")
  }
  class <- amGetClass(amData, config$sepClass)
  type <- config$dataClass[config$dataClass$class == class, "type"]
  return(type)
}


#' Get data tag
#'
#' @param amData name vector or list to evaluate
#' @return tags
amGetTag <- function(amData, type = "ui") {
  if (type == "ui") {
    if (is.list(amData)) amData <- names(amData)
    tmp <- gsub(".+(?=\\[)|(\\[)|(\\])", "", amData, perl = T)
    tmp <- gsub("\\_", " ", tmp)
    tmp
  } else {
    tag <- unlist(strsplit(amData, paste0("\\", config$sepClass)))[[2]]
    tag <- unlist(strsplit(tag, config$sepTagFile))
    unique(tag)
  }
}


# create data.frame version of dataList
# amDataList = vector of dataSet
# sepClass = class separator (double dash)
# type = type attribute in resulting data.frame
amDataListToDf <- function(amDataList, sepClass = config$sepClass, type = "raster") {
  if (is.null(amDataList) || length(amDataList) < 1) {
    return(NULL)
  }
  cla <- amGetClass(amDataList, sep = sepClass)
  tag <- amGetTag(amDataList)
  name <- amNoMapset(amDataList)
  display <- names(amDataList)
  displayCla <- amClassListInfo(cla)
  data.frame(
    class = cla,
    tags = tag,
    type = type,
    searchCol = paste(type, displayCla, cla, tag),
    origName = name,
    displayName = display,
    displayClass = displayCla
  )
}




# Create a subset of the data frame.
amDataSubset <- function(pattern = "", type = NULL, amDataFrame) {
  if (nchar(pattern) > 0) {
    pattern <- amSubPunct(pattern, "|")
    tbl <- amDataFrame[grep(pattern, amDataFrame$searchCol), ]
  } else {
    tbl <- amDataFrame
  }
  if (!is.null(type)) tbl <- tbl[tbl$type %in% type, ]
  tbl
}


#' amGetUniqueTag
#' get unique tags from string
#' @param x string containing tags. Ex. "new new; myTag2"
#' @param sorted should the resulting vector be sorted?
#' @return  vector of unique tags.
#' @export
amGetUniqueTags <- function(x, ordered = FALSE) {
  if (isEmpty(x)) {
    return()
  }
  if (length(x) > 1) x <- unlist(x)
  x <- paste(x)
  x <- amSubPunct(x, sep = ";")
  x <- unique(unlist(strsplit(x, ";")))
  if (ordered == TRUE) {
    x <- x[order(x)]
  }
  return(x)
}

#' amSubQuote
#'
#' Remove simple and double quote and newlines. This can be usefull in message
#' send to javascript functions, popup message, etc.. For complete removal of
#' non ascii character, use amSubPunct
#'
#' @param txt character vector
#' @export
amSubQuote <- function(txt) {
  txt <- gsub("\"", " ", txt)
  txt <- gsub("\'", " ", txt)
  txt <- gsub("\n", " ", txt)
  return(txt)
}

#' amSubPunct
#' remove all unwanted characters, remplace by sep of choice
#' @param vect character vector
#' @param rmTrailingSep remove unwanted trailing replacement separator
#' @param rmLeadingSep remove unwanted leanding replacement  separator
#' @param rmDuplicateSep remove duplicated replacement separator
#' @export
amSubPunct <- function(vect,
  sep = "_",
  rmTrailingSep = T,
  rmLeadingSep = T,
  rmDuplicateSep = T,
  debug = F) {
  # vect<-gsub("'",'',iconv(vect, to='ASCII//TRANSLIT'))
  res <- gsub("[[:punct:]]+|[[:blank:]]+", sep, vect) # replace punctuation by sep
  res <- gsub("\n", "", res)
  if (rmDuplicateSep) {
    if (nchar(sep) > 0) {
      res <- gsub(paste0("(\\", sep, ")+"), sep, res) # avoid duplicate
    }
  }
  if (rmLeadingSep) {
    if (nchar(sep) > 0) {
      res <- gsub(paste0("^", sep), "", res) # remove trailing sep.
    }
  }
  if (rmTrailingSep) {
    if (nchar(sep) > 0) {
      res <- gsub(paste0(sep, "$"), "", res) # remove trailing sep.
    }
  }
  res
}
# example :
# amSubPunct('hérétique:crasy#namer*ßuss','_')
# [1] "heretique_crasy_namer_ssuss"


#' Update GRASS raster/vector name and SQLITE table name
#'
#' This function updates names based on modified tags field in data list.
#' It expects a working GRASS environment and an AccessMod config list.
#'
#' @param dataListOrig data.frame with columns: "type", "displayClass", "tags", "origName", "class"
#' @param dataListUpdate data.frame with columns: "type", "displayClass", "tags", "origName", "class"
#' @param dbCon Path to SQLite database
#' @param config AccessMod configuration list
#'
#' @return logical TRUE if updates were made, FALSE otherwise
#' @export
amUpdateDataListName <- function(dataListOrig, dataListUpdate, dbCon, config) {
  if (is.null(dataListOrig) || is.null(dataListUpdate)) {
    return(FALSE)
  }

  tblO <- as.data.frame(lapply(dataListOrig, as.character), stringsAsFactors = FALSE)
  tblU <- as.data.frame(lapply(dataListUpdate, as.character), stringsAsFactors = FALSE)

  if (amCheckInvalidDataInUpdate(tblU)) {
    stop('Rename data: there is NA, missing char or "-" in update table')
  }

  tblM <- anti_join(tblU, tblO)
  if (nrow(tblM) == 0) {
    return(FALSE)
  }

  msgs <- amProcessDataRenaming(tblM, config, dbCon)
  amDisplayRenameResults(msgs)

  return(TRUE)
}

# Internal functions
amCheckInvalidDataInUpdate <- function(tbl) {
  return(any(vapply(tbl, function(x) any(isEmpty(x) | x == "-"), logical(1))))
}

amProcessDataRenaming <- function(tblM, config, dbCon) {
  msgs <- apply(tblM, 1, function(x) {
    if (x["class"] != amGetClass(config$mapDem)) {
      amRenameData(
        type  = x["type"],
        new   = amNewName(x["class"], x["tags"]),
        old   = x["origName"],
        dbCon = dbCon
      )
    }
  })
  return(msgs[!sapply(msgs, is.null)])
}

amDisplayRenameResults <- function(msgs) {
  if (length(msgs) > 0) {
    uiMsg <- tags$div(
      style = "max-height:300px;overflow-y:scroll;",
      tags$ul(
        HTML(paste("<li>", msgs, "</li>", collapse = ""))
      )
    )
    amMsg(type = "ui", title = "Rename", subtitle = "Result", text = uiMsg)
  }
}

#' amRenameData
#'
#' Function to handle data renaming in GRASS and SQLite database, if data exists.
#'
#' @param type raster,vector or table
#' @param old old name
#' @param new new name
#' @param dbCon RSQLite database connection
#' @param session Shiny session
#'
#' @export
amRenameData <- function(type, old = "", new = "", dbCon = NULL, session = getDefaultReactiveDomain()) {
  if (!type %in% c("raster", "vector", "table", "shape", "list") || old == "" || new == "") {
    return()
  }
  msgRename <- ""
  renameOk <- FALSE

  switch(type,
    "raster" = {
      rL <- execGRASS("g.list", type = "raster", intern = T)
      if (!tolower(new) %in% tolower(rL) && old %in% rL) {
        execGRASS("g.rename", raster = paste(old, new, sep = ","))
        renameOk <- TRUE
      } else {
        renameOk <- FALSE
      }
    },
    "vector" = {
      vL <- execGRASS("g.list", type = "vector", intern = T)
      if (!tolower(new) %in% tolower(vL) && old %in% vL) {
        execGRASS("g.rename", vector = paste(old, new, sep = ","))
        renameOk <- TRUE
      } else {
        renameOk <- FALSE
      }
    },
    "table" = {
      if (is.null(dbCon)) {
        return()
      }
      tL <- dbListTables(dbCon)
      if (!tolower(new) %in% tolower(tL) && old %in% tL) {
        dbExecute(dbCon, paste("ALTER TABLE", old, "RENAME TO", new))
        renameOk <- TRUE
      } else {
        renameOk <- FALSE
      }
    },
    "shape" = {
      sL <- amGetShapesList()
      pathShapes <- system(sprintf("echo %s", config$pathShapes), intern = T)
      if (!tolower(new) %in% tolower(names(sL)) && old %in% names(sL)) {
        # sL did not return all related files to this layer : get these.
        allShpFiles <- amGetShapesList(pattern = sprintf("^%s\\.", old))
        # sorry for this.
        for (s in allShpFiles) {
          sExt <- file_ext(s)
          newPath <- file.path(pathShapes, paste0(new, ".", sExt))
          file.rename(s, newPath)
        }
        renameOk <- TRUE
      } else {
        renameOk <- FALSE
      }
    },
    "config" = {
      cL <- amGetConfigList()
      configPath <- system(sprintf("echo %s", config$pathConfigs), intern = T)
      if (!tolower(new) %in% tolower(names(cL)) && old %in% names(cL)) {
        newName <- file.path(jsonPath, sprintf("%s.json", new))
        oldName <- cL[[old]]
        file.rename(oldName, newName)
        renameOk <- TRUE
      } else {
        renameOk <- FALSE
      }
    }
  )

  if (renameOk) {
    msg <- paste("Renamed", old, "to", new, ".")
  } else {
    msg <- paste("Rename", old, "to", new, " not necessary. Duplicated tags, empty tags or already existing name.")
  }

  return(msg)
}



#' function to extract display class info
#' @param class Class identifier
#' @param ls list id and class
#' @param dc dataClass table
#' @export
amClassInfo <- function(class = NULL, ls = FALSE, dc = config$dataClass) {
  lang <- amTranslateGetSavedLanguage()
  dc <- config$dataClass
  if (ls) {
    dc[, c("class", lang, "type")]
  } else {
    dc[dc$class == class, c("class", lang, "type")][1, ]
  }
}


#' Get data class info
#' @param class Data class
#' @param value Value to retrieve, by default, language specific class
#' @export
amClassListInfo <- function(class = NULL, value = NULL) {
  vals <- c("type", "colors", "importable", "internal")
  lang <- amTranslateGetSavedLanguage()
  res <- character(0)
  if (!is.null(class)) {
    for (i in class) {
      if (is.null(value)) {
        res <- c(res, config$dataClassList[[i]][[lang]])
      } else {
        if (!value %in% vals) {
          amDebugMsg(paste("value must be in ", paste(vals, collapse = ";")))
          return()
        }
        res <- c(res, config$dataClassList[[i]][[value]])
      }
    }
    return(res)
  }
}





#' Create data list for ui
#' @param class AccessMod class to look for
#' @param dl Config data list to retrieve match
#' @export
amListData <- function(class = NULL, dl = dataList, shortType = TRUE) {
  datAll <- character(0)
  for (i in class) {
    d <- amClassInfo(class = i)
    dType <- d[, "type"]
    dat <- grep(paste0("^", d[, "class"], "__"), dl[[dType]], value = T)
    if (!isTRUE(is.null(dat) || length(dat) == 0)) {
      if (shortType) {
        dType <- substr(dType, 0, 1)
      }
      names(dat) <- paste0("(", dType, ") ", names(dat))
      datAll <- c(dat, datAll)
    }
  }
  return(datAll)
}

#' Get a list of archive available (based on env varibles)
#'
#' @return list of archive files
amGetArchiveList <- function(archivesPath = config$pathArchiveGrass, baseName = NULL) {
  amGrassSessionStopIfInvalid()
  archivesPath <- system(paste("echo", archivesPath), intern = TRUE)
  # if archive directory doesn't exist, create it.
  dir.create(archivesPath, showWarnings = FALSE)
  archivesPath <- normalizePath(archivesPath)
  # add ressource for shiny
  if (!isEmpty(baseName)) {
    addResourcePath(
      prefix = baseName,
      directoryPath = archivesPath
    )
  }
  #
  # List archive sorted chronologically
  # list.file sort alphabetically
  # list.files(archivesPath,pattern='*.zip')
  cmd <- sprintf('ls %s -th | grep "\\.zip$"', archivesPath)
  # suppressWarnings: status 1 is returned when directory is empty.. ignore that
  suppressWarnings(system(cmd, intern = T))
}

amGetShapesList <- function(
  pattern = ".shp$",
  shapePath = config$pathShape
) {
  amGrassSessionStopIfInvalid()
  shapePath <- system(paste("echo", shapePath), intern = TRUE)
  # if  directory doesn't exist, create it.
  dir.create(shapePath, showWarnings = FALSE)
  shapePath <- normalizePath(shapePath)
  shapeList <- list.files(shapePath, pattern = pattern, full.names = T)
  if (length(shapeList) > 0) {
    nameShape <- gsub(".shp", "", basename(shapeList))
    names(shapeList) <- nameShape
    as.list(shapeList)
  } else {
    list()
  }
}

amGetConfigList <- function(
  pattern = ".json$",
  configPath = config$pathConfigs
) {
  # path need grass environment variables, as defined in config.R
  amGrassSessionStopIfInvalid()
  configPath <- system(paste("echo", configPath), intern = TRUE)
  # if  directory doesn't exist, create it.
  dir.create(configPath, showWarnings = FALSE)
  configPath <- normalizePath(configPath)
  listConfig <- list.files(configPath, pattern = pattern, full.names = T)
  if (length(listConfig) > 0) {
    nameConfig <- gsub(".json", "", basename(listConfig))
    names(listConfig) <- nameConfig
    as.list(listConfig)
  } else {
    list()
  }
}

amFilterDataTag <- function(
  namesToFilter,
  prefixSep = "__",
  tagSep = "_",
  tagSepRepl = " ",
  filterTag,
  filterText
) {
  # table with splitted names into prefix/suffix(tags) parts parts..
  exprTag <- paste0(".+?", prefixSep) # search characters before prefix separator
  exprPrefix <- paste0("?", prefixSep, ".+") # search character after prefix separator
  tagsTable <- data.frame(
    prefix = gsub(exprPrefix, "", namesToFilter),
    tags = gsub(tagSep, tagSepRepl, gsub(exprTag, "", namesToFilter, perl = T)),
    name = namesToFilter,
    stringsAsFactors = F
  )
  # add column with pasted prefix and tags.
  # E.g. "land_cover reclass 2010"
  # instead of land_cover__reclass_2010
  tagsTable$nameFilter <- paste(tagsTable$prefix, tagsTable$tags)
  # first filter based on text field : any part of name, OR logic.
  # use any punctuation char in text filter as string split character
  if (!is.null(filterText) && !filterText == "") {
    filterText <- unlist(strsplit(amSubPunct(filterText, ","), ","))
    rowsFiltText <- unlist(sapply(filterText, grep, tagsTable$nameFilter))
  } else {
    rowsFiltText <- NULL
  }
  # second filter based on tags : whole words in name, AND logic.
  if (!is.null(filterTag) && !filterTag == "") {
    exprFilter <- paste0("(?=.*\\b", filterTag, "\\b)", collapse = "")
    rowsFiltTag <- grep(exprFilter, tagsTable$nameFilter, perl = T)
  } else {
    rowsFiltTag <- NULL
  }
  rowsFilt <- unique(c(rowsFiltTag, rowsFiltText))

  if (!is.null(filterTag) && !filterTag == "" || !is.null(filterText) && !filterText == "") {
    tagsTable <- tagsTable[rowsFilt, ]
  }


  return(tagsTable)
}


# formating new name
amNewName <- function(class, tags, sepClass = config$sepClass, sepTag = config$sepTagFile) {
  tags <- paste(tags, collapse = sepTag)
  tags <- amSubPunct(tags, sepTag)
  paste0(c(class, tags), collapse = sepClass)
}
# amNewName('land_cover',c('test','2012'),"$","_")
# return:
# [1] "land_cover$test_2012"


amNameCheck <- function(dataList, name, class = c("vector", "raster", "table"), sepMap = config$sepMapset, dbCon = NULL) {
  class <- match.arg(class)
  name <- as.character(name)
  nameNoMapset <- unlist(strsplit(name, paste0("(", sepMap, ").+")))
  if (length(nameNoMapset) == 0) {
    return(NULL)
  }
  if (class == "table") {
    if (all(nameNoMapset %in% dbListTables(dbCon))) {
      return(nameNoMapset)
    } else {
      return(NULL)
    }
  } else {
    if (all(name %in% dataList[[class]])) {
      return(nameNoMapset)
    } else {
      return(NULL)
    }
  }
}



amTagsFileToDisplay <- function(fileN, sepClass = config$sepClass, sepTag = config$sepTagFile) {
  cla <- amClassListInfo(unlist(strsplit(fileN, paste0("\\", sepClass)))[[1]])
  tag <- unlist(strsplit(fileN, paste0("\\", sepClass)))[[2]]
  tag <- paste0("[", gsub(sepTag, " ", tag), "]")
  paste(cla, tag)
}



# create list usable to populate select input
# Here, we want th make sure that each project will contain unique set of value for its input.
# We append the name of the mapset(project) to each name and create a user-friendly version to display in ui.
amCreateSelectList <- function(dName, sepTag = config$sepTagUi, sepClass = config$sepClass, sepMap = config$sepMapset, mapset) {
  amErrorAction(title = "amCreateSelectList", {
    if (length(dName) == 0) {
      return(NULL)
    }
    # add mapset at the end of each data name
    # ex. cumulative_cost__test -> cumulative_cost__test@burkina
    l <- as.list(paste0(dName, sepMap, mapset))
    lN <- character(0)
    err <- character(0)
    for (n in dName) {
      # test if data name containe class separator, else flag as err
      if (isTRUE(length(grep(config$sepClass, n)) > 0)) {
        dat <- unlist(strsplit(n, sepMap))[[1]]
        vals <- unlist(strsplit(dat, paste0("\\", sepClass)))
        if (length(vals) == 2) {
          displayName <- amTagsFileToDisplay(dat)
          lN <- c(lN, displayName)
        } else {
          lN <- c(lN, NA)
          err <- c(err, n)
        }
      } else {
        lN <- c(lN, NA)
        err <- c(err, n)
      }
    }
    if (length(err) > 1) {
      warning(
        paste(
          " Some data name from project", mapset, "(",
          paste(err, collapse = ","),
          ") doesn't match AccessMod naming convention and will be ignored."
        )
      )
    }

    names(l) <- lN
    return(l)
  })
}


