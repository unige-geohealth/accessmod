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


#' Reorder df using vector of colum names
#' @param df {data.frame} input
#' @param colNames {Character} List of names
#'
#' @return {data.frame}
amSortByCol <- function(df, colNames) {
  df[do.call(order, df[, colNames]), ]
}

#' Swap variable values
#'
#' @param name1 {Character} name as string
#' @param name2 {Character} name as string
swap <- function(name1, name2) {
  e <- parent.frame()
  tmp <- get(name1, envir = e)
  assign(name1, get(name2, envir = e), envir = e)
  assign(name2, tmp, envir = e)
}


#' Wrapper for on.exit() : add should always be true
#'
#' @param expr Expression to evaluate
#' @param env Environment in which evalaute the expression
on_exit_add <- function(expr, env = NULL) {
  args <- list(expr = substitute(expr), add = TRUE)
  if (isEmpty(env)) {
    env <- parent.frame()
  }
  do.call("on.exit", args, envir = env)
}


#' Wrapper for system zip command ( utils::zip fails )
#'
#' @param archivePath {Character} Output filepath
#' @param files {Character} Files to include in archive
amZip <- function(archivePath,
  files) {
  curWd <- getwd()
  on_exit_add({
    setwd(curWd)
  })
  aWd <- dirname(files)[[1]]
  files <- basename(files)
  setwd(aWd)
  cmd <- Sys.getenv("R_ZIPCMD", "zip")
  system2(cmd, c("-r9X", archivePath, files))
}


#' Guess grass cmd output type based on interface description
#'
#' @param cmd {String} GRASS command
#' @return type {String}
amGuessOutputType <- function(cmd) {
  typeOut <- c()
  params <- parseGRASS(cmd)$parameters
  types <- c("vector", "raster")

  for (param in params) {
    if (param[["name"]] == "output") {
      for (type in types) {
        if (grepl(type, param[["desc"]])) {
          typeOut <- c(type, typeOut)
        }
      }
    }
  }
  return(unique(typeOut))
}



# wrapper around Sys.sleep. Sleep in milisecond
amSleep <- function(t = 100) {
  Sys.sleep(t / 1000)
}

#' Use system grep to return list of file matching grep exp
#' @param exp {character} Regex expression
#' @param fixed {boolean} search for fixed string
#' @param ext {string} search for file with this extension
#' @export
amGrep <- function(exp, fixed = TRUE, ext = NULL) {
  cmd <- ifelse(fixed, "grep -RFl", "grep -REL")
  if (!is.null(ext)) {
    cmd <- sprintf("%1$s %2$s", cmd, paste(sprintf("--include \\*.%1$s", ext), collapse = ""))
  }
  system(sprintf("%1$s '%2$s' .", cmd, exp))
}



#' Remove manually temp grass in defined mapset or current mapset
#' @param location {Character} location where to remove temp
#' @return null
amCleanGrassTemp <- function(location = NULL) {
  dbase <- config$pathGrassDataBase
  location <- amGrassSessionGetEnv("LOCATION_NAME")
  tempDir <- ".tmp"
  pLocation <- file.path(dbase, location)
  mapsets <- list.dirs(pLocation, recursive = F)
  for (mapset in mapsets) {
    tmpPath <- file.path(pLocation, mapset, ".tmp")
    if (dir.exists(tmpPath)) {
      content <- file.path(tmpPath, "*")
      unlink(content, recursive = T, force = T)
    }
  }
}

#' Remove files in AccessMod cache folder
#'
#' @return
amCleanCacheFiles <- function() {
  cacheFiles <- list.files(config$pathCacheDir, full.names = T)
  if (length(cacheFiles) > 0) {
    unlink(cacheFiles)
  }
  amMapsetRemoveAll(pattern = "^tmp_")
}

amCleanArchivesFiles <- function() {
  archivesPath <- system(sprintf("echo %s", config$pathArchiveGrass), intern = T)
  archivesFiles <- list.files(archivesPath, full.names = T)
  if (length(archivesFiles) > 0) {
    unlink(archivesFiles)
  }
  return(length(archivesFiles))
}

#' Get table count of features types
#'
#' @param vect {character} Vector layer
#' @param vect {character} Types to return
#' @return {data.frame} table of count by feature
amGetTableFeaturesCount <- function(vect, types = c("areas", "lines", "points")) {
  if (!amVectExists(vect)) {
    return(data.frame(type = character(0), count = numeric(0)))
  }
  tbl <- execGRASS(
    "v.info",
    map    = vect,
    flags  = "t",
    intern = T
  ) %>%
    amCleanTableFromGrass(
      sep = "=",
      col.names = c("type", "count")
    )
  tbl <- tbl[tbl$type %in% types, ]
  return(tbl)
}

#' Clean and read table from grass strings output
#'
#' @param {Character} text Grass text output
#' @param {Character} sep Character used as separator
#' @param {Logical} header Use first line as header
#' @param {Vector} cols Optional column selection
#' @return {data.frame}
#' @export
amCleanTableFromGrass <- function(text, sep = "|", header = TRUE, cols = NULL, ...) {
  tbl <- amSubQuote(text) %>%
    read.table(
      text = .,
      sep = sep,
      header = isTRUE(header),
      stringsAsFactor = FALSE,
      ...
    )

  if (!isEmpty(cols)) {
    tbl <- tbl[cols]
  }
  return(tbl)
}

#' Time interval evaluation
#' @param action "start" or "stop" the timer
#' @param timerTitle Title to be displayed in debug message
#' @return
amTimer <- function(action = "stop", timerTitle = "timer") {
  diff <- 0
  env <- parent.frame(1)

  if (action == "start") {
    env$.mxTimer <- list(
      time = Sys.time(),
      title = timerTitle
    )
  } else {
    if (!is.null(env$.mxTimer)) {
      diff <- as.numeric(difftime(
        Sys.time(),
        env$.mxTimer$time,
        units = "s"
      ))
      timerTitle <- env$.mxTimer$title
      diff <- round(diff, 3)
    }
  }

  return(list(
    diff = diff,
    title = timerTitle
  ))
}




#' Get a list of available location in DB
#'
#' @return list location from GrassDB
amGetGrassListLoc <- function() {
  grassDataBase <- config$pathGrassDataBase
  list.dirs(
    grassDataBase,
    recursive = F,
    full.names = F
  )
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


# function to create selectize compatible list of value
selectListMaker <- function(vect, default) {
  vect <- c(default, vect)
  vect <- amSubPunct(vect)
}

# this function get the columns corresponding to type INTEGER or CHARACTER for a given
# grass db table.
grassDbColType <- function(grassTable, type = "INTEGER") {
  if (!type %in% c("INTEGER", "CHARACTER")) {
    stop("type in grassDbColType should be INTEGER or CHARACTER")
  }
  desc <- execGRASS("db.describe", table = grassTable, intern = T)
  grepSub <- grep("^(column:)|^(type:)", desc)
  desc <- as.data.frame(t(matrix(desc[grepSub], nrow = 2)))
  names(desc) <- c("column", "type")
  desc$column <- gsub("^column:", "", desc$column)
  desc$type <- gsub("^type:", "", desc$type)
  desc <- desc[desc$type %in% type, ]$column
  desc
}



# http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
amCleanHtml <- function(htmlString) {
  return(gsub("<.*?>", "", paste(htmlString)))
}


#' Message
#'
#' @param session Shiny session
#' @param type Message type : error, warnin, message, log, ui
#' @param text Message text
#' @param subtitle Optional subtitle
#' @param logDile For type 'log', file to write in
amMsg <- function(session = shiny:::getDefaultReactiveDomain(),
  type = c("error", "warning", "message", "log", "ui"),
  text,
  title = NULL,
  subtitle = NULL,
  logFile = config$pathLog,
  ...) {
  type <- match.arg(type)
  if (is.null(title)) title <- type
  if (is.null(subtitle)) subtitle <- type
  stopifnot(!length(logFile) == 0)

  if ("html" %in% class(text) || "shiny.tag.list" %in% class(text)) {
    textLog <- amCleanHtml(paste(text))
  } else {
    textLog <- text
  }

  textLog <- gsub("[\r\n]", "", textLog)
  textLog <- gsub("\"", "", textLog, fixed = T)
  textLog <- gsub("  ", "", textLog)

  if (!type == "ui") {
    # NOTE: why not write.table...append=T = or fwrite ?
    write(
      paste(
        amSysTime(),
        "\t",
        type,
        "\t",
        textLog,
        collapse = " "
      ),
      file = logFile,
      append = TRUE
    )
  }

  if (type == "log") {
    return(NULL)
  }

  if (type == "error") {
    pbc(visible = FALSE)
  }

  amUpdateModal(
    panelId = "amModal",
    html = text,
    title = title,
    subtitle = subtitle,
    ...
  )
}

# read only a subset of last lines
amReadLogs <- function(logFile = config$pathLog,
  nToKeep = config$nLogDefault) {
  tblOut <- data.frame(
    "time" = character(0),
    "type" = character(0),
    "msg" = character(0)
  )
  # ┌────────────┐ <- oldest
  # │            │
  # │            │
  # ├────────────┤ <- nToKeep
  # └────────────┘ <- newest
  raw <- system(
    sprintf(
      "tail -n %s %s",
      nToKeep,
      config$pathLog
    ),
    intern = T
  )
  tbl <- read.csv(
    text = raw,
    header = F,
    stringsAsFactors = F,
    sep = "\t"
  )
  if (nrow(tbl) > 0) {
    names(tbl) <- names(tblOut)
    tblOut <- rbind(tblOut, tbl)
  }

  tblOut <- tblOut[order(tblOut$time, decreasing = T), ]

  return(tblOut)
}







# function to control input file extensions.
# for each type and ext, write new rules here.
# file extension is given by file_ext (package tools) or grep command.
amValidateFileExt <- function(mapNames, mapType) {
  # need access to am config
  stopifnot(exists("config"))
  # require validation vector in config files, e.g. shpExtMin
  mN <- basename(mapNames) # list of map names to be validated.
  mT <- mapType # vect or rast
  fE <- file_ext(mN) # list of file extension in map list
  # vector files
  if (mT == "vect") {
    # rule 1 : if it's a shapefile, it must have minimal set of  file extensions.
    if ("shp" %in% fE) {
      valid <- all(amSubPunct(config$fileShpExtMin, "") %in% fE)
      if (!valid) {
        stop(paste(
          "Accessmod shapefile validation error:
            Trying to import invalid shapefile dataset.
            Minimum required file extensions are :", paste(config$fileShpExtMin, collapse = ", ")
        ))
      }
    }
    # rule 2 : if it's a shapefile, none of the extensions must be present more than once
    if ("shp" %in% fE) {
      valid <- all(!duplicated(fE))
      if (!valid) {
        stop(
          "Accessmod shapefile validation error:
          Duplicated files type detected. Please add only one map at a time.
          "
        )
      }
    }
  }

  # raster files
  if (mT == "rast") {
    if ("adf" %in% fE) {
      valid <- all(config$fileAdfMin %in% mN)
      if (!valid) {
        stop(paste(
          "Accessmod esri binary grid validation:
            Trying to import invalid adf file dataset.
            Minimum required files are:", paste(config$fileAdfMin, collapse = ", ")
        ))
      }
    }
    if ("img" %in% fE) {
      fES <- amSubPunct(fE)
      fEMin <- amSubPunct(config$fileImgMin)
      valid <- all(fEMin %in% fES)
      if (!valid) {
        stop(
          sprintf("
            Accessmod ERDAS img file validation:
            Trying to import invalid file dataset.
            Min. required file extension are: %s", paste(config$fileImgMin))
        )
      }
    }
  }
}


amLayerExists <- function(filter = "",
  mapset = NULL,
  type = c("raster", "vector")) {
  if (isEmpty(filter)) {
    return(FALSE)
  }
  tryCatch(
    {
      filter <- strsplit(filter, "@")[[1]][[1]]
      filter <- paste0(filter, "*")
      if (isEmpty(mapset)) {
        mapset <- amGrassSessionGetMapset()
      }
      layers <- execGRASS("g.list",
        type = type,
        pattern = filter,
        mapset = mapset,
        intern = TRUE
      )
      return(!isEmpty(layers))
    },
    error = function(e) {
      warning(e)
      return(FALSE)
    }
  )
}

amRastExists <- function(filter = "", mapset = NULL) {
  return(amLayerExists(filter, mapset, "raster"))
}

amVectExists <- function(filter = "", mapset = NULL) {
  return(amLayerExists(filter, mapset, "vector"))
}


rmLayerIfExists <- function(filter = "", type = c("vector", "raster")) {
  tryCatch(
    {
      if (isEmpty(filter)) {
        return()
      }
      filter <- paste(filter, collapse = ",")
      layerList <- execGRASS("g.list",
        type = type,
        pattern = filter,
        intern = TRUE
      )
      if (length(layerList) > 0) {
        execGRASS("g.remove",
          flags = c("b", "f"),
          type = type,
          pattern = paste0(filter, sep = "|")
        )
      }
    },
    error = function(e) {
      warning(e)
    }
  )
}



rmRastIfExists <- function(filter = "") {
  return(rmLayerIfExists(filter, "raster"))
}

rmVectIfExists <- function(filter = "", names = "") {
  return(rmLayerIfExists(filter, "vector"))
}

rmTableIfExists <- function(filter = "") {
  tryCatch(
    {
      if (isEmpty(filter)) {
        return()
      }

      # List SQLite tables
      tables <- unlist(strsplit(
        execGRASS("db.tables", flags = "p", intern = TRUE),
        "\n"
      ))

      # Filter tables by the given pattern
      tables_to_remove <- grep(filter, tables, value = TRUE)

      # Drop each matching table
      for (table in tables_to_remove) {
        execGRASS("db.execute",
          sql = sprintf("DROP TABLE %s;", table)
        )
      }
    },
    error = function(e) {
      warning(e)
    }
  )
}

amMapExists <- function(map, mapset = NULL) {
  if (isEmpty(mapset)) {
    mapset <- amGrassSessionGetMapset()
  }
  res <- amNoMapset(map) %>%
    execGRASS("g.list",
      type = c("vector", "raster"),
      pattern = .,
      mapset = mapset,
      intern = TRUE
    )
  isTRUE(length(res) > 0)
}



amRastIsEmpty <- function(rast) {
  if (amRastExists(rast)) {
    length(amGetRasterStat(rast, "sum")) == 0
  } else {
    TRUE
  }
}


amVectIsEmpty <- function(vect) {
  if (amVectExists(vect)) {
    dat <- execGRASS("v.info", map = vect, flags = "t", intern = T)
    dat <- amParseOptions(paste(dat, collapse = ";"))
    all(sapply(dat, function(x) {
      x == 0 || x == "0"
    }))
  } else {
    TRUE
  }
}



# creation of a file to import color rules in GRASS. Assume a numeric null value.
# Geotiff only allow export color table for byte and UNint16 data type. So,
# the maximum value (null..) will be 65535. Both data type don't allow negetive numbers.
createColorTable <- function(maxVals, nullVals = 65535, paletteFun, filePath) {
  valQuant <- c(quantile(0:maxVals), nullVals)
  colorMap <- t(col2rgb(paletteFun(6)))
  colGrass <- character()
  for (i in 1:nrow(colorMap)) {
    rN <- valQuant[i]
    vN <- paste(colorMap[i, ], collapse = ":")
    tN <- paste(rN, vN, "\n", collapse = " ")
    colGrass <- c(colGrass, tN)
  }

  write(colGrass, file = filePath)
}


getSqlitePath <- function(sqliteExpr) {
  # example of sqliteExpr: '$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'
  system(paste("echo", sqliteDB), intern = T)
}


#' amPackageManager
#'
#' Manage package from within a shiny session : install or load if exists.
#' This function display a progress bar on top of the shiny app if package is installed.
#'
#'
#' @param pkgCran vector of packages from CRAN
#' @param pkgLocal vector of packages from local archive directory
#' @param libPath path to R library
#' @param pathLocalPkg path to directory containing .tar.gz packages archives
#' @return none.
#' @export
amPackageManager <- function(pkgCran, pkgGit) {
  # which package is missing ?
  pkgCranM <- pkgCran[!pkgCran %in% installed.packages()]
  pkgGitM <- pkgGit[!names(pkgGit) %in% installed.packages()]
  pkgCranL <- length(pkgCranM)
  pkgGitL <- length(pkgGitM)

  # isntall missing from CRAN
  if (pkgCranL > 0) {
    inc <- 1 / pkgCranL
    msgUpdate <- "Updating CRAN packages"
    # with Progress use shiny::getDefaultReactiveDomain() as session object,
    # no need to provide one here.
    withProgress(message = msgUpdate, value = 0.1, {
      amMsg(session, "log", msgUpdate)
      for (p in pkgCranM) {
        install.packages(pkgs = p, repos = "http://cran.rstudio.com/")
        incProgress(inc, detail = p)
      }
    })
  }
  if (pkgGitL > 0) {
    inc <- 1 / pkgGitL
    msgUpdate <- "Updating GITHUB packages"
    withProgress(message = msgUpdate, value = 0.1, {
      amMsg(session, "log", msgUpdate)
      for (p in pkgGitM) {
        install_github(p)
        incProgress(inc, detail = p)
      }
    })
  }
  # load libraries. all at once or require inside function ?
  # best practice seems inside function, but not sure if this method
  # is the most efficient. TODO: check this.
  lapply(pkgCran, require, character.only = TRUE)
  lapply(pkgLocal, require, character.only = TRUE)
}

#' R list to html
#' @param listInput list in inptu
#' @param htL List to append to
#' @param h Value of the first level of html header
#' @param exclude list named item to exclude
#' @export
listToHtml <- function(listInput, htL = "", h = 2, exclude = NULL) {
  hS <- paste0("<H", h, "><u>", collapse = "") # start
  hE <- paste0("</u></H", h, ">", collapse = "") # end
  h <- h + 1 # next
  if (is.list(listInput)) {
    nL <- names(listInput)
    nL <- nL[!nL %in% exclude]
    htL <- append(htL, "<ul>")
    for (n in nL) {
      # htL<-append(htL,c('<li>',n,'</li>'))
      htL <- append(htL, c(hS, n, hE))
      subL <- listInput[[n]]
      htL <- listToHtml(subL, htL = htL, h = h, exclude = exclude)
    }
    htL <- append(htL, "</ul>")
  } else if (is.character(listInput) || is.numeric(listInput)) {
    htL <- append(htL, c("<li>", paste(listInput, collapse = ","), "</li>"))
  }
  return(paste(htL, collapse = ""))
}









# getClientDateStamp <- function(){
# triggerClientTime()
# time <- retrieveClientTime()
# test <- try(silent=T,
# date <- as.POSIXct(time$clientPosix,origin="1970-01-01")
# )
## NOTE: sometimes, this fail. Probably because httpuv:::service trick
# if("try-error" %in% class(test)){
# date <- Sys.time()
# }
# amSubPunct(date)
# }


#
# Get client timestamp
# @param {Session} session
# @return {POSIXct} client timestamp
#
getClientDateStamp <- function(session = getDefaultReactiveDomain()) {
  #
  # timeZone : set in accessmod.js
  offset <- session$input$timeOffset
  if (isEmpty(offset)) {
    offset <- 1
  }
  timeOrig <- Sys.time()
  time <- timeOrig - offset * 60
  return(time)
}


#' encode in base64
amEncode <- function(text) {
  base64enc::base64encode(charToRaw(as.character(text)))
}

amDecode <- function(base64text) {
  rawToChar(base64enc::base64decode(base64text))
}


# format Sys.time to avoid spaces.
amSysTime <- function(type = c("fancy", "compatible", "short")) {
  if (is.null(type)) type <- "fancy"
  type <- match.arg(type)
  t <- Sys.time()
  tf <- switch(type,
    "fancy" = "%Y-%m-%d@%H_%M_%S",
    "compatible" = "%Y_%m_%d_%H_%M_%S",
    "short" = "%Y%m%d%H%M%S"
  )
  format(t, format = tf)
}

#' Display a time stamp for CLI
#'
#' @param text Text to display in the middle
#' @example amTimeStamp("demo")
#' # ------------------------------- DEMO ------------------------------- #
#'                          2022-08-19@15_08_11
#' # -------------------------------------------------------------------- #
amTimeStamp <- function(text = NULL) {
  if (is.null(text)) text <- "AccessMod"
  w <- 68
  t <- amSysTime()
  u <- toupper(text)
  uS <- (w - nchar(u) - 2) / 2
  tS <- (w - nchar(t) - 2) / 2
  sideH <- paste(rep("-", uS), collapse = "")
  sideT <- paste(rep(" ", tS), collapse = "")
  head <- paste("#", sideH, u, sideH, "#", collapse = "")
  body <- paste(" ", sideT, t, sideT, " ", collapse = "")
  sideF <- paste(rep("-", nchar(head) - 4), collapse = "")
  foot <- paste("#", sideF, "#", collapse = "")
  cat(c(head, body, foot, collapse = ""), sep = "\n")
}





#' Upload new data
#' @param config {list} am5 config list
#' @param dataName {string} data name
#' @param dataFile {path} data file path
#' @param dataClass {string} data class
#' @param dbCom {dbcon} db connection object
#' @param pBarTitle {string} progress bar title
#' @name upload_data
#' @export
amUploadTable <- function(config, dataName, dataFile, dataClass, dbCon, pBarTitle) {
  tbl <- import(dataFile)

  if (!exists("tbl")) {
    stop(paste("AccessMod could not read the provided file. Try another compatible format:", config$filesAccept$table))
  }

  progressBarControl(
    visible = TRUE,
    percent = 30,
    title = pBarTitle,
    text = "Data validation..."
  )
  # remove column containing NA's
  # search for expected column names
  aNames <- config$tableColNames[[dataClass]]

  if (is.null(aNames)) stop(paste("No entry found in config for class:", dataClass))

  # count remaining row
  hasRow <- nrow(tbl) > 0

  if (!hasRow) {
    stop(
      sprintf("Table %s doesn't have row(s).", dataName)
    )
  }

  tNames <- tolower(names(tbl))
  if (!all(aNames %in% tNames)) {
    aNamesP <- paste(aNames, collapse = "; ", sep = " ")
    tNamesP <- paste(tNames, collapse = "; ", sep = " ")
    errMsg <- sprintf(
      "Importation of %s : dataset of class %s shoud contains columns named \n %s. Columns name of the provided file:\n %s",
      basename(dataFile),
      dataClass,
      aNamesP,
      tNamesP
    )
    stop(errMsg)
  }
  names(tbl) <- tNames
  tbl <- tbl[, aNames] # keep only needed columns


  progressBarControl(
    visible = TRUE,
    percent = 90,
    title = pBarTitle,
    text = "Writing in db..."
  )
  dbWriteTable(dbCon, dataName, tbl, overwrite = TRUE)
  amDebugMsg("Table", dataName, " written in DB")
}




amErrHandler <- function(session = shiny:::getDefaultReactiveDomain(),
  errMsgTable,
  call,
  conditionMsg,
  title = NULL,
  type = "warning") {
  #
  # in all case, return message as log.
  #
  textDefault <- tagList(
    tags$p(conditionMsg),
    tags$p("Call"),
    tags$p(call)
  )
  amMsg(
    session,
    type = "log",
    text = textDefault,
    title = title
  )
  errMsg <- data.frame()

  # try to find a registered simplified message to display in UI
  tryCatch(
    {
      errMsg <- errMsgTable[
        sapply(errMsgTable$cond, grepl, as.character(conditionMsg)),
      ]
    },
    error = function(cond) {
      amMsg(
        session,
        type = "log",
        text = cond$message,
        title = "Error handling issue"
      )
    }
  )

  # replace original message
  if (nrow(errMsg) > 0) {
    for (i in 1:nrow(errMsg)) {
      if (errMsg[i, "type"] != "discarded") {
        amMsg(
          session,
          type = tolower(errMsg[i, "type"]),
          text = errMsg[i, "text"],
          title = title
        )
      }
    }
    # if no match found in msg table, return
    # original text and type found by amErrorAction
  } else {
    amMsg(
      session,
      type = type,
      text = textDefault,
      title = title
    )
  }
}





amErrorAction <- function(expr,
  errMsgTable = config$msgTableError,
  quotedActionError = NULL,
  quotedActionWarning = NULL,
  quotedActionMessage = NULL,
  quotedActionFinally = NULL,
  title,
  warningToLog = TRUE,
  messageToLog = TRUE,
  pBarFinalRm = TRUE,
  session = shiny:::getDefaultReactiveDomain()) {
  withCallingHandlers(
    {
      tryCatch(
        {
          expr
        },
        # error : stop process, eval error quoted function, return condition to amErrHandler
        error = function(cond) {
          msg <- cond$message
          call <- paste(deparse(cond$call), collapse = " ")
          if (!is.null(quotedActionError)) eval(quotedActionError)
          amErrHandler(session, errMsgTable,
            conditionMsg = msg,
            title = title,
            call = call,
            type = "error"
          )

          if (pBarFinalRm) {
            progressBarControl(percent = 100)
          }
          return()
        }
      )
    },
    # warning, don't stop process, but return condition to amErrHandler
    warning = function(cond) {
      msg <- amSubQuote(cond$message)
      call <- paste(deparse(cond$call), collapse = "")

      if (!is.null(quotedActionWarning)) eval(quotedActionWarning)
      if (!warningToLog) {
        amErrHandler(session, errMsgTable,
          conditionMsg = msg,
          title = title,
          call = call,
          type = "warning"
        )
      } else {
        amErrHandler(session, errMsgTable,
          conditionMsg = msg,
          title = title,
          call = call,
          type = "log"
        )
      }

      return()
    },
    # simple message : don't stop, write in log. usa amMsg(type=message for a real message.)
    message = function(cond) {
      msg <- amSubQuote(cond$message)
      if (is.null(quotedActionMessage)) eval(quotedActionMessage)


      if (!messageToLog) {
        amMsg(session,
          text = msg,
          title = title,
          type = "message"
        )
      } else {
        amMsg(session,
          text = msg,
          title = title,
          type = "log"
        )
      }

      return()
    },
    finally = {
      if (!is.null(quotedActionFinally)) {
        eval(quotedActionFinally)
      }
    }
  )
}



amGetLocationProj <- function() {
  # ignore NADS grid ref.
  # NOTE: maybe not a good idea, but without it, we get this error
  # Error in .spTransform_Polygon(input[[i]], to_args = to_args, from_args = from_args,  :
  #  error in pj_transform: failed to load datum shift file
  projGrass <- toString(CRS(getLocationProj(ignore.stderr = T)))
  return(projGrass)
}




#
#
# Upload raster
#

#' Upload a raster file in AccessMod
#'
#' @param config {List} AccessMod Configuraition list by default
#' @param dataInput {Character} Main file to use
#' @param dataFiles {List} Others files, depending on the format
#' @param dataClass {String} Class of the data
#' @param pBarTitle {String} Progress bar title
amUploadRaster <- function(
  config,
  dataInput,
  dataName,
  dataFiles,
  dataClass,
  pBarTitle
) {
  #
  # get map meta before importation
  #
  pMetaBefore <- amMapMeta()
  pBarTitle <- "Raster importation"

  progressBarControl(
    visible = TRUE,
    percent = 10,
    title = pBarTitle,
    text = "Validation..."
  )

  isDem <- isTRUE(dataClass == amGetClass(config$mapDem))
  isLdc <- isTRUE(
    dataClass == "rLandCoverMerged" || dataClass == "rLandCover"
  )
  currentMapset <- amGrassSessionGetMapset()

  #
  # raster validation.
  #
  amValidateFileExt(dataFiles, "rast")

  dMeta <- list()

  for (file in dataFiles) {
    tryCatch(
      {
        if (isEmpty(dMeta)) {
          dMeta <- gdalinfo(file, raw_output = FALSE)
          dMeta$proj <- as.character(gdalsrsinfo(file, as.CRS = TRUE))
        }
      },
      error = function(cond) {
        return(NULL)
      }
    )
  }

  if (isEmpty(dMeta)) {
    stop("Raster uploader: Missing raster meta information")
  }

  srsDest <- ifelse(isDem,
    dMeta$proj,
    amGetLocationProj()
  )

  on_exit_add({
    for (f in dataFiles) {
      if (file.exists(f)) {
        file.remove(f)
      }
    }
    for (f in dataInput) {
      if (file.exists(f)) {
        file.remove(f)
      }
    }
    amRegionReset()
  })

  progressBarControl(
    visible = TRUE,
    percent = 40,
    title = pBarTitle,
    text = "Validation succeeded. Importation in database..."
  )

  if (isEmpty(dMeta$driver)) {
    stop("Raster uploader: Missing driver info")
  }

  if (isDem) {
    dataName <- strsplit(config$mapDem, "@")[[1]][[1]]
    amGrassSessionUpdate(mapset = "PERMANENT")
    on_exit_add({
      amGrassSessionUpdate(mapset = currentMapset)
    })
  }

  execGRASS(
    "r.in.gdal",
    band = 1,
    input = dMeta$file,
    output = dataName,
    flags = c("overwrite", "quiet"),
    title = dataName
  )

  #
  # Reset project extent
  #
  if (isDem) {
    progressBarControl(
      visible = TRUE,
      percent = 80,
      title = pBarTitle,
      text = "Set project resolution and extent based on new DEM"
    )
    amRegionReset()
  }

  #
  # Convert land cover to integer
  #
  if (isLdc) {
    ldcMeta <- execGRASS("r.info",
      map = dataName,
      flags = c("g"),
      intern = TRUE
    )

    ldcMeta <- read.csv(
      text = ldcMeta,
      sep = "=",
      header = FALSE
    )

    isCell <- isTRUE(ldcMeta[ldcMeta$V1 == "datatype", 2] == "CELL")

    if (!isCell) {
      progressBarControl(
        visible = TRUE,
        percent = 80,
        title = pBarTitle,
        text = "LandCover value are not in integer, convert values"
      )

      exp <- sprintf(
        "%1$s = round(%1$s)",
        dataName
      )

      execGRASS(
        "r.mapcalc",
        expression = exp,
        flags = c("overwrite")
      )
    }
  }

  #
  # Set colors
  #
  colorsTable <- config$dataClass[
    config$dataClass$class == dataClass,
    "colors"
  ]

  if (!isEmpty(colorsTable)) {
    progressBarControl(
      visible = TRUE,
      percent = 85,
      title = pBarTitle,
      text = "Set color table"
    )

    colConf <- as.list(strsplit(colorsTable, "&")[[1]])
    if (length(colConf) == 2) {
      cN <- c("color", "flag")
    } else {
      cN <- c("color")
    }
    names(colConf) <- cN
  }
  if (!isEmpty(colorsTable)) {
    execGRASS(
      "r.colors",
      map = dataName,
      flags = colConf$flag,
      color = colConf$color
    )
  }

  #
  # Last progress bar info
  #
  progressBarControl(
    visible = TRUE,
    percent = 90,
    title = pBarTitle,
    text = "Importation succeeded... Cleaning..."
  )

  #
  # Set importation summary list
  #
  dMeta$nullCells <- amGetRasterStat(dataName, metric = "null_cells")

  pMetaAfter <- amMapMeta()

  #
  # meta data about uploaded data and project
  #
  out <- list(
    projectBefore = list(
      resolution = list(
        y = pMetaBefore$grid$nsres,
        x = pMetaBefore$grid$ewres
      ),
      projection = pMetaBefore$orig$proj
    ),
    projectAfter = list(
      resolution = list(
        y = pMetaAfter$grid$nsres,
        x = pMetaAfter$grid$ewres
      ),
      projection = pMetaAfter$orig$proj
    ),
    data = list(
      resolution = list(
        x = abs(dMeta$res.x),
        y = abs(dMeta$res.y)
      ),
      projection = dMeta$proj,
      numberOfNulls = dMeta$nullCells
    )
  )

  return(out)
}



#
# Upload vectors
#
#
amUploadVector <- function(dataInput, dataName, dataFiles, pBarTitle) {
  # TODO: validate extent

  tryReproj <- TRUE
  # helper function to validate file based on extension

  progressBarControl(
    visible = TRUE,
    percent = 20,
    title = pBarTitle,
    text = "Attributes validation and cleaning"
  )


  amValidateFileExt(dataFiles, "vect")
  origShpFilePath <- dataFiles[grepl(".shp$", dataFiles)]
  origDbfFilePath <- dataFiles[grepl(".dbf$", dataFiles)]
  origCpgFilePath <- dataFiles[grepl(".cpg$", dataFiles)]
  origShpBaseName <- basename(substr(origShpFilePath, 0, nchar(origShpFilePath) - 4))

  tmpDataBase <- file.path(tempdir(), paste0(dataName, ".dbf"))
  tmpDirShape <- file.path(tempdir(), paste(dataName))
  tmpDataPath <- file.path(tmpDirShape, paste0(dataName, ".shp"))

  encoding <- "ISO8859-1"


  #
  # Data cleaning :
  #   Remove old cat_ column for from old version of accessmod
  #   Update custom key (e.g. cat by default) with unique id
  #   Replace columnn of type date (bug with sqlite and grass) by column of type string
  #   Write spatial with correct encoding. (ogr fails to read cpg file in GDAL 1.11.3, grass produce invalid char)
  #
  projDest <- sp::CRS(amGetLocationProj())


  origData <- import(origDbfFilePath)

  # remove old cat or cat_ column
  origData <- subset(origData, select = !names(origData) %in% c("cat_"))
  # add key column

  origData[, config$vectorKey] <- 1L:nrow(origData)

  # issue with dates #157
  posDate <- grep("[dD]ate", sapply(origData, class))
  if (length(posDate) > 0) {
    for (i in posDate) {
      origData[, i] <- as.character(origData[, i])
    }
  }

  export(origData, origDbfFilePath)

  progressBarControl(
    visible = TRUE,
    percent = 20,
    title = pBarTitle,
    text = "Cleaned file written, upload in database"
  )

  if (!isEmpty(origCpgFilePath)) {
    encoding <- readLines(origCpgFilePath, warn = F)
  }

  dir.create(tmpDirShape)

  amOgrConvert(
    fileIn = origShpFilePath,
    fileOut = tmpDataPath,
    toSrs = projDest,
    format = "ESRI Shapefile",
    overwrite = TRUE
  )

  execGRASS("v.in.ogr",
    flags = c("overwrite", "w", "2"), # overwrite, lowercase, 2d only,
    parameters = list(
      input = tmpDataPath,
      key = config$vectorKey,
      output = dataName,
      snap = 0.0001
    )
  )

  unlink(dataFiles)
  unlink(tmpDirShape)
  return(NULL)
}

amUpdateDataList <- function(listen) {
  listen$dataListUpdate <- runif(1)
}

#' Custom debug message.
#'
#' @param ... anything printable
amDebugMsg <- function(...) {
  mode <- config$logMode
  if ("debug" %in% mode) {
    msg <- jsonlite::toJSON(
      list(...),
      auto_unbox = T,
      pretty = T
    )
    cat(paste("{ debug", amSysTime(), "}", msg), sep = "\n")
  }
}
amDebugMsgPerf <- function(title, time) {
  mode <- config$logMode
  if ("perf" %in% mode) {
    cat(sprintf("{ perf %s } %s\n", title, time))

    pExists <- file.exists(config$pathPerf)

    write.table(data.frame(t = Sys.time(), a = title, d = time),
      config$pathPerf,
      sep = ",",
      row.names = FALSE,
      col.names = !pExists,
      append = pExists
    )
  }
}





amMapMeta <- function() {
  # TODO: use one grid list, name this after
  meta <- list()
  gL <- gmeta()
  meta$location <- gL$LOCATION_NAME
  projGrass <- amGetLocationProj()
  proj <- list(
    orig = projGrass,
    latlong = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  )

  #
  # build bounding box polygon
  # ( extent format -> -180,180,-90,90 )
  #
  locExtent <- extent(gL$w, gL$e, gL$s, gL$n)
  bbx <- as(locExtent, "SpatialPolygons")
  proj4string(bbx) <- proj$orig

  #
  # Keep project and unprojected bbox in the same format
  #
  bbxSp <- list(
    orig = bbx,
    latlong = spTransform(bbx, CRS(proj$latlong))
  )
  #
  # For each one, create a summary list
  #
  for (p in names(proj)) {
    bx <- bbxSp[[p]]
    bxD <- bbox(bx)

    xMin <- bxD[1]
    xMax <- bxD[3]
    yMin <- bxD[2]
    yMax <- bxD[4]

    meta <- c(
      meta,
      structure(
        list(
          list(
            "proj" = proj[[p]],
            "bbx" = list(
              "ext" = list(
                "x" = list(
                  "min" = xMin,
                  "max" = xMax
                ),
                "y" = list(
                  "min" = yMin,
                  "max" = yMax
                )
              ),
              "center" = c((yMax + yMin) / 2, (xMax + xMin) / 2)
            )
          )
        ),
        names = p
      )
    )
  }

  grid <- gL[names(gL) %in% c("nsres", "ewres", "rows", "cols", "cells")]
  meta$grid <- lapply(grid, as.numeric)
  meta$bbxSp <- bbxSp
  return(meta)
}

# extract spatial polygons from mapMeta
amBboxSp <- function(mapMeta, proj = c("orig", "latlong")) {
  proj <- match.arg(proj)
  bbx <- as(extent(mapMeta[[proj]]$bbx$ext), "SpatialPolygons")
  proj4string(bbx) <- CRS(mapMeta[[proj]]$proj)
  bbx
}

## extract geojson from mapMeta
# amBboxGeoJson<-function(mapMeta,proj=c('orig','latlong')){
# proj<-match.arg(proj)
# bbx<-as(extent(mapMeta[[proj]]$bbx$ext),"SpatialPolygons")
# bbxStyle<-list(
# fillColor = "black",
# fillOpacity = 0.5,
# opacity=0.1,
# weight = 1,
# color = "#000000"
# )
## bbx<-fromJSON(geojson_json(bbx)[[1]])
# bbx<-geojson_list(bbx)
# worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
# bbxCoord<-bbx$features[[1]]$geometry$coordinates[[1]]
# bbx$features[[1]]$geometry$coordinates<-list(worldCoord,bbxCoord)
# bbx$style<-bbxStyle
# return(bbx)
# }

## extract geojson from mapMeta
# amSpotlightGeoJson<-function(raster){
# bbx<-as(extent(mapMeta[[proj]]$bbx$ext),"SpatialPolygons")
# bbxStyle<-list(
# fillColor = "black",
# fillOpacity = 0.5,
# opacity=0.1,
# weight = 1,
# color = "#000000"
# )
# bbx<-geojson_list(bbx)
# worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
# bbxCoord<-bbx$features[[1]]$geometry$coordinates[[1]]
# bbx$features[[1]]$geometry$coordinates<-list(worldCoord,bbxCoord)
# bbx$style<-bbxStyle
# return(bbx)
# }




# find  one cell diagonal bridge between multiple raster maps (e.g. road) and destination map (e.g. merged lcv)
# warning : only tested from rasterized lines with densified option.
amBridgeFinder <- function(fromMap, toMap, bridgeMap) {
  #
  # If the cell of one from map is not null
  #
  exprOneFromAsValue <- paste(
    sprintf(
      "!isnull(%1$s)",
      fromMap
    ),
    collapse = " || "
  )

  # Analyse diagonal value to extract bridge
  #
  # X=non-null cell in <road_map>; N=null in <merged_map>; A=non-null cell in <merged_map>
  # X will be set as null in fallowing cases:
  #
  # X N   N X   A N   N A
  # N A   A N   N X   X N
  #
  exprDiag <- sprintf("
    isnull(%1$s[0,-1]) &&
      !isnull(%1$s[1,-1]) &&
      isnull(%1$s[1,0]) ||

      isnull(%1$s[0,1]) &&
      !isnull(%1$s[1,1]) &&
      isnull(%1$s[1,0]) ||

      isnull(%1$s[-1,0]) &&
      !isnull(%1$s[-1,1]) &&
      isnull(%1$s[0,1]) ||

      isnull(%1$s[0,-1]) &&
      !isnull(%1$s[-1,-1]) &&
      isnull(%1$s[-1,0])
    ", toMap)

  exprBridge <- sprintf("if(%1$s,if(%2$s,1,null()),null())", exprOneFromAsValue, exprDiag)

  execGRASS("r.mapcalc",
    expression = sprintf(
      "%1$s=%2$s",
      bridgeMap,
      gsub("\\n", "", exprBridge)
    ),
    flags = "overwrite"
  )
  stat <- execGRASS("r.univar",
    map = bridgeMap,
    flags = "t",
    intern = T
  ) %>%
    amCleanTableFromGrass()

  nBridges <- stat[1, "non_null_cells"]
  if (!isEmpty(nBridges) || isTRUE(nBridges > 0)) {
    amDebugMsg(paste(
      "Accessmod found", nBridges,
      "one cell diagonal bridges.
          Output control map is", bridgeMap
    ))
  }
}

# remove cell defined in bridgeMap from removeFromMap.
amBridgeRemover <- function(bridgeMap, removeFromMap) {
  tmpRules <- tempfile()
  write(execGRASS("r.category", map = removeFromMap, intern = T), tmpRules)
  expr <- paste0(removeFromMap, "=if(!isnull(", bridgeMap, "),null(),", removeFromMap, ")")
  execGRASS("r.mapcalc", expression = expr, flags = "overwrite")
  execGRASS("r.category", map = removeFromMap, rules = tmpRules)
  amDebugMsg(paste("Bridges from", bridgeMap, "removed from", removeFromMap))
}

# https://gist.github.com/jmarhee/8530768
amMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}






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




# function to handle grass naming to am
#


# formating new name
amNewName <- function(class, tags, sepClass = config$sepClass, sepTag = config$sepTagFile) {
  tags <- paste(tags, collapse = sepTag)
  tags <- amSubPunct(tags, sepTag)
  paste0(c(class, tags), collapse = sepClass)
}
# amNewName('land_cover',c('test','2012'),"$","_")
# return:
# [1] "land_cover$test_2012"





## sample names
# sampleName<-c("land_cover_table~[super,super]@malawi_90_m",
#                       "land_cover~[super,super]@malawi_90_m",
#                                          "population~[super,new]@malawi_90_m")
## sample names
# sampleName<-c("land_cover_table$test_super",
#                "land_cover$super_super",
#                              "population$super_new")
#
#


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





#
#
#
#
## get all available tags from a list
# amGetUniqueTags<-function(amData){
#  if(is.list(amData))amData<-names(amData)
#  unique(unlist(strsplit(unlist(amData),'.(\\[)|(\\])|(\\+)|.(@)|(,)')))
# }
# example :
#  > dList$raster
# $`land_cover_table [test+super]`
# [1] "land_cover_table$test_super@malawi90m"
#
# $`land_cover [super+super]`
# [1] "land_cover$super_super@malawi90m"
#
# $`population [super+new]`
# [1] "population$super_new@malawi90m"
#
# amGetUniqueTags(dList$raster)
# return :
#
# [1] "land_cover_table" "test"             "super"            "land_cover"       "population"
# [6] "new"


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


# amGetTag<-function(amData){
#  if(is.list(amData))amData<-names(amData)
#  tmp<-gsub(".+(?=\\[)|(\\[)|(\\])","",amData,perl=T)
#  tmp<-gsub("\\_"," ",tmp)
#  tmp
# }


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


#' amUpdateDataListName
#'
#' Update GRASS raster/vector name and SQLITE table name,
#' based on modified tags field in data list. This function expect
#' a working GRASS environment and an accessmod config list.
#'
#' @param dataListOrig table with columns: "type displayClass tags origName class" .
#' @param dataListUpdate table with columns: "ttype displayClass tags origName class". If it contains modified tags value, origName is set as old name, new name is formed based on class and tags.
#' @param dbCon: path to sqlite db
#'
# @export
amUpdateDataListName <- function(dataListOrig, dataListUpdate, dbCon, config) {
  if (!is.null(dataListOrig) && !is.null(dataListUpdate)) {
    tblO <- dataListOrig
    tblU <- dataListUpdate
    tblO[] <- lapply(tblO, as.character)
    tblU[] <- lapply(tblU, as.character)
    # test for empty or incorrect table
    if (any(sapply(tblU, function(x) isTRUE(isEmpty(x) || x == "-")))) {
      stop('Rename data : there is NA, missing char or "-" in update table')
    } else {
      # search for new tags
      tblM <- anti_join(tblU, tblO)
      if (!isTRUE(nrow(tblM) > 0)) {
        return(FALSE)
      }
      #  rename and get a list of changes
      msgs <- apply(tblM, 1, function(x) {
        # if not class DEM
        if (!x["class"] == amGetClass(config$mapDem)) {
          amRenameData(
            type  = x["type"],
            new   = amNewName(x["class"], x["tags"]),
            old   = x["origName"],
            dbCon = dbCon
          )
        }
      })

      # send a msg to ui
      uiMsg <- tags$div(
        style = "max-height:300px;overflow-y:scroll;",
        tags$ul(
          HTML(paste("<li>", msgs, "</li>"))
        )
      )
      amMsg(type = "ui", title = "Rename", subtitle = "Result", text = uiMsg)
      return(TRUE)
    }
  }
  return(FALSE)
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
        dbGetQuery(dbCon, paste("ALTER TABLE", old, "RENAME TO", new))
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


#' Create empty new vector in grass
#' @param dbCon Sqlite db connection object
#' @param mapName Name of the layer to create
#' @param indexName Name of the column containing index value. Default is cat.
amCreateEmptyVector <- function(
  dbCon,
  mapName = amRandomName("tmp"),
  indexName = config$vectorKey
) {
  rmVectIfExists(mapName)

  execGRASS("v.edit",
    tool = "create",
    map = mapName,
    flags = "overwrite"
  )

  emptyTable <- data.frame(cat = integer(0))

  names(emptyTable) <- indexName

  dbWriteTable(dbCon,
    name = mapName,
    value = emptyTable,
    overwrite = TRUE
  )

  execGRASS("v.db.connect",
    map = mapName,
    table = mapName,
    flags = c("o")
  )
}




####### SECTION accessiblity analysis

# function extract field summary from SQLite table :
# - numeric fields,
# - character fields,
# - index candidate : (unique values & only character and integer & n=nRow )
# - uniques values by fields
amGetFieldsSummary <- function(table, dbCon, getUniqueVal = T) {
  stopifnot(table %in% dbListTables(dbCon))

  # get full table
  tblSample <- dbGetQuery(
    dbCon,
    sprintf(
      "SELECT * FROM %1$s %2$s",
      table,
      ifelse(getUniqueVal, "", "LIMIT 1000")
    )
  )

  # number of row
  nR <- nrow(tblSample)

  #
  # get Unique values
  #
  uniqueVal <- lapply(tblSample, function(x) {
    x <- unique(x)
    sort(x)
  })

  #
  # test for index
  #
  isIndex <- sapply(uniqueVal, function(x) {
    length(x) == nR
  })

  #
  # classes
  #
  classes <- sapply(tblSample, class)

  indexes <- names(isIndex[isIndex])
  numerics <- names(classes[classes %in% c("integer", "numeric")])
  integers <- names(classes[classes %in% c("integer")])
  characters <- names(classes[classes %in% c("character")])

  # return summary
  list(
    int = integers,
    num = numerics,
    char = characters,
    idx = indexes,
    val = uniqueVal
  )
}


#' amMapPopOnBarrier
#'
#' Ask grass if there is population located on barrier (null cells)
#' @param inputPop population layer name
#' @param inputMerged merged landcover layer name
#' @param inputFriction Friction layer name
#' @param inputSpeed Speed layer name
#' @param outputMap output layer name containing cells on barrier
#' @export
amMapPopOnBarrier <- function(inputPop,
  inputMerged = NULL,
  inputFriction = NULL,
  inputSpeed = NULL,
  outputMap = "tmp_out_pop_barrier") {
  inputTest <- inputMerged

  if (!amRastExists(inputTest)) {
    inputTest <- inputSpeed
  }

  if (!amRastExists(inputTest)) {
    inputTest <- inputFriction
  }

  expr <- sprintf(
    "%1$s = if(!isnull(%2$s) && isnull(%3$s),%2$s,null())",
    outputMap,
    inputPop,
    inputTest
  )

  execGRASS("r.mapcalc", expression = expr, flags = "overwrite")
}




#' Evaluate disk space available
#' @return disk space available in MB
sysEvalFreeMbDisk <- function() {
  # free <- system('df --output=avail -BM "$PWD" | sed "1d;s/[^0-9]//g"',intern=T)
  # Alpine
  #                                                  * - > $4
  # Filesystem           1M-blocks      Used Available Use% Mounted on
  # overlay                 120695    117784         0 100% /
  free <- system("df -BM $GISDBASE | tail -n1 | awk '{print $4}'", intern = T)
  return(as.integer(free))
}

#' Evaluate disk space total
#' @return disk space available in MB
sysEvalSizeMbDisk <- function() {
  # free <- system('df --output=size -BM "$PWD" | sed "1d;s/[^0-9]//g"',intern=T)
  #
  # Alpine
  #                                        * -> $3
  # Filesystem           1M-blocks      Used Available Use% Mounted on
  # overlay                 120695    117784         0 100% /
  used <- system("df -BM $GISDBASE | tail -n1 | awk '{print $3}'", intern = T)
  return(as.integer(used))
}

#' Evalutate memory available. This is experimental
#' @return Available memory in MB
sysEvalFreeMbMem <- function() {
  sys <- Sys.info()["sysname"]
  free <- 300

  switch(sys,
    "Darwin" = {
      memTot <- as.integer(system("sysctl hw.memsize | awk '{ print $2 / (2^10)^2}'", intern = T))
      memActive <- as.integer(system("vm_stat | awk '/^Pages active/ { print ($3 * 4096) / (2^10)^2}'", intern = T))
      memFree <- as.integer(system("vm_stat | awk '/^Pages free/ { print ($3 * 4096) / (2^10)^2}'", intern = T))
      memPurgeable <- as.integer(system("vm_stat | awk '/^Pages purgeable/ { print ($3 * 4096) / (2^10)^2}'", intern = T))

      free <- memTot - memActive
    },
    "Linux" = {
      memTot <- as.integer(system("cat /proc/meminfo | awk '/^MemTotal:/ {print $2/ (2^10)}'", intern = T))
      memActive <- as.integer(system("cat /proc/meminfo | awk '/^Active:/ {print $2/ (2^10)}'", intern = T))
      memFree <- as.integer(system("cat /proc/meminfo | awk '/^MemFree:/ {print $2/ (2^10)}'", intern = T))
      memCached <- as.integer(system("cat /proc/meminfo | awk '/^Cached:/ {print $2/(2^10)}'", intern = T))

      free <- memTot - memActive
    }
  )

  return(as.integer(free))
}



#' Get ressources estimations
#' @param input start vector
#' @return {List} list(required=list(memory,disk),available=list(memory,disk))
amGetRessourceEstimate <- function(hf) {
  out <- list(
    required = list(
      memory = 300,
      disk = 10
    ),
    available = list(
      memory = sysEvalFreeMbMem(),
      disk = sysEvalFreeMbDisk()
    )
  )

  if (!isEmpty(hf)) {
    out <- amAnisotropicTravelTime(
      inputSpeed = config$mapDem,
      inputHf = hf,
      outputTravelTime = "tmp_test",
      getMemDiskRequirement = TRUE
    )
  }

  return(out)
}
#' amCircularTravelDistance
#' @export
amCircularTravelDistance <- function(inputHf, outputBuffer, radius) {
  suppressWarnings({
    execGRASS("v.to.rast", input = inputHf, output = "tmp_buffer", use = "val", value = 1, flags = "overwrite")
  })
  execGRASS("r.buffer", input = "tmp_buffer", output = outputBuffer, distances = radius, flags = "overwrite")
  # create one unique zone.
  expr <- paste(outputBuffer, "=if(!isnull(", outputBuffer, "),1,null())")
  execGRASS("r.mapcalc", expression = expr, flags = "overwrite")
}


#' Parse scaling up coefficient options
#' @param opt String of option with paired argument separated by sepAssign, separated by given sepItem
#' @param sepAssign Character. Separator of assignement. Default is "="
#' @param sepItem Character. Separarator of items. Default is ";"
amParseOptions <- function(opt, sepItem = ";", sepAssign = "=") {
  optList <- list()
  if (!is.null(opt)) {
    opt <- unlist(strsplit(opt, sepItem))
    if (length(opt) > 0) {
      opt <- strsplit(opt, sepAssign)
      for (o in opt) {
        l <- length(o)
        optList[o[l - 1]] <- o[l]
      }
    }
  }
  return(optList)
}


#' amGetRasterStat
#'
#' Extract cells stat using r.univar
#'
#' @param rasterMap grass raster map name
#' @param stats Stat to compute. Should be in c('n','cells','max','mean','stdev','coeff_var','null_cells','min','range','mean_of_abs','variance','sum','percentile')
#' @param quantile Percentiles to extract
#' @return cells stat
#' @export
amGetRasterStat <- function(
  rasterMap,
  metric = c(
    "n", "cells", "max", "mean", "stddev", "coeff_var",
    "null_cells", "min", "range", "mean_of_abs", "variance",
    "sum", "percentile"
  ),
  percentile = 99) {
  # validation
  if (!amRastExists(rasterMap)) {
    return()
  }
  stopifnot(length(metric) == 1)
  # set options
  metric <- match.arg(metric)
  # if quantiles use r.quantile
  if (isTRUE("percentile" %in% metric)) {
    val <- amParseOptions(execGRASS("r.quantile", input = rasterMap, percentiles = percentile, intern = T), sepAssign = ":")
  } else {
    val <- amParseOptions(execGRASS("r.univar", map = rasterMap, flags = "g", intern = T))[[metric]]
  }
  val <- as.numeric(val)

  if (isTRUE(length(val) == 0 || isEmpty(val))) val <- 0L

  return(val)
}

#' Get cumulative sum table based on raster (cell) zonal stat
#'
#' pbz table give the sum of person by travel time iso band
#'
#' @param mapValues Raster for values
#' @param mapZones Raster for zones
amGetRasterStatZonal <- function(mapValues, mapZones) {
  #
  # compute integer version of cumulative cost map to use with r.univar
  #
  ttIsCell <- amRasterMeta(mapZones)[["datatype"]] == "CELL"

  if (!ttIsCell) {
    exprIntCost <- sprintf(
      "%1$s = %1$s >= 0 ? round( %1$s ) : null() ",
      mapZones
    )
    execGRASS("r.mapcalc", expression = exprIntCost, flags = "overwrite")
  }

  #
  # compute zonal statistic : time isoline as zone
  #
  zStat <- execGRASS(
    "r.univar",
    flags  = c("g", "t", "overwrite"),
    map    = mapValues,
    zones  = mapZones,
    intern = T
  ) %>%
    amCleanTableFromGrass()

  #
  # rm na/nan (case when corresponding zone have no value)
  # -> column sum selection is arbitrary, but used in cumSum, which
  # fails when having na/nan value.
  zStat <- zStat[!is.na(zStat$sum), ]
  zStat$cumSum <- cumsum(zStat$sum)
  zStat[c("zone", "sum", "cumSum")]
}


#' Get the percentage from two raster
#' @param numerator Numerator
#' @param denominator Denominator
#' @return Percentage of one raster to another
#' @export
amGetRasterPercent <- function(numerator, denominator) {
  if (numerator == denominator) {
    return(0)
  }
  denSum <- amGetRasterStat(denominator, "sum")
  numSum <- amGetRasterStat(numerator, "sum")
  return((denSum - numSum) / denSum * 100)
}



#' Compose random char name
#' @param prefix Prefix of the resulting string
#' @param suffix Suffix of the resultiing string
#' @param n Number of random letters
#' @param collapse Character to join strings
#' @return String with random letters
#' @export
amRandomName <- function(prefix = NULL, suffix = NULL, n = 20, cleanString = FALSE, collapse = "_") {
  if (cleanString) {
    prefix <- amSubPunct(prefix, "_")
    suffix <- amSubPunct(suffix, "_")
  }
  rStr <- paste(letters[round(runif(n) * 24)], collapse = "")
  str <- c(prefix, rStr, suffix)
  paste(str, collapse = collapse)
}

#' Check for no data
#' @param val Vector to check
#' @export
amNoDataCheck <- function(val = NULL) {
  isTRUE(
    is.null(val)
  ) ||
    isTRUE(
      isTRUE(is.data.frame(val) && nrow(val) == 0) ||
        isTRUE(is.list(val) && (length(val) == 0)) ||
        isTRUE(!is.list(val) && is.vector(val) && (
          length(val) == 0 ||
            isTRUE(val[[1]] %in% config$defaultNoData) ||
            is.na(val[[1]]) ||
            nchar(val[[1]], allowNA = TRUE) == 0))
    )
}

isEmpty <- function(val = NULL) {
  amNoDataCheck(val)
}
isNotEmpty <- function(val = NULL) {
  !amNoDataCheck(val)
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













#' Create a sortable list from stack items names for the landcover merge double sortable input
#' @param x Raster stack members list
#' @export
amListToSortableLi <- function(x) {
  n <- names(x)
  if (is.null(n)) n <- x
  tagList(lapply(setNames(n, n), function(i) {
    val <- x[[i]]
    # get the type (line,area,grid,point) from stack item
    # e.g. getp 'line' from rStackRoad__Main_road_test_line@malawi_90_m
    type <- gsub(".*_([a-z]*?)@.*", "\\1", val)
    geomIcon <- ""
    switch(type,
      "line" = {
        geomIcon <- "icon-grid_line"
      },
      "area" = {
        geomIcon <- "icon-grid_area"
      },
      "grid" = {
        geomIcon <- "icon-grid_full"
      },
      "point" = {
        geomIcon <- "icon-grid_point"
      }
    )
    # class <- paste("list-group-item am_dbl_srt_item",class,sep=" ")
    # tags$div(class=class,`data-input`=x[[i]],i)
    tags$div(
      class = "list-group-item am_dbl_srt_item",
      `data-input` = x[[i]],
      tags$span(i),
      tags$span(
        class = geomIcon
      )
    )
  }))
}


#' Produce a concatenated CamelCase version of a string
#' @param x String to convert
#' @param fromStart Boolean : should the first letter be upercased )
#' @export
amCamelCase <- function(x, fromStart = T) {
  template <- sprintf(
    "(\\s%s)([a-zA-Z0-9])",
    ifelse(fromStart, "|^", "")
  )
  replacement <- "\\U\\2\\E"
  gsub(template, replacement, x, perl = T)
}



#' amRasterToShape
#'
#' Extract area from raster and create a shapefile or
#' append to it if the files already exist.
#'
#' @param idField Name of the facility id column.
#' @param idPos String id currently processed.
#' @param append Append to existing.
#' @param inputRaster Raster to export
#' @param outCatch Name of shapefile layer
#' @param listColumnsValue Alternative list of value to
#'        put into catchment attributes. Must be a named list.
#' @return Shapefile path
#' @export
amRasterToShape <- function(
  pathToCatchment,
  idField,
  idPos,
  append = FALSE,
  inputRaster,
  outputShape = "tmp__vect_catch",
  listColumnsValues = list(),
  oneCat = TRUE
) {
  #
  # Local db connection
  #
  dbCon <- amMapsetGetDbCon()
  on_exit_add({
    dbDisconnect(dbCon)
  })

  idField <- ifelse(
    idField == config$vectorKey,
    paste0(config$vectorKey, "_join"),
    idField
  )

  listColumnsValues[idField] <- idPos
  listColumnsValues <- listColumnsValues[
    !names(listColumnsValues) %in% config$vectorKey
  ]

  tmpRaster <- amRandomName("tmp__r_to_shape")
  tmpVectDissolve <- amRandomName("tmp__vect_dissolve")

  on_exit_add({
    rmVectIfExists(tmpVectDissolve)
    rmVectIfExists(outputShape)
    rmRastIfExists(tmpRaster)
  })

  execGRASS("g.copy", raster = c(inputRaster, tmpRaster))

  if (oneCat) {
    expOneCat <- sprintf("%1$s = !isnull(%1$s) ? 1 : null()", tmpRaster)
    execGRASS("r.mapcalc", expression = expOneCat, flags = "overwrite")
  }

  #
  # Export input raster to vector
  #
  execGRASS("r.to.vect",
    input  = tmpRaster,
    output = outputShape,
    type   = "area",
    flags  = c("overwrite")
  )

  #
  # Dissolve result to have unique id by feature
  #
  execGRASS("v.dissolve",
    input  = outputShape,
    output = tmpVectDissolve,
    column = "value",
    flags  = c("overwrite")
  )

  #
  # Create a table for catchment
  #

  execGRASS("v.db.addtable",
    map = tmpVectDissolve
  )

  outPath <- pathToCatchment

  # for the first catchment : overwrite if exists, else append.
  if (append) {
    outFlags <- c("a", "m", "s")
  } else {
    # overwrite returns warnings...
    if (file.exists(outPath)) {
      file.remove(outPath)
    }
    outFlags <- c("overwrite", "m", "s")
  }
  #
  # update attributes
  #
  dbRec <- dbGetQuery(
    dbCon,
    sprintf(
      "select * from %s",
      tmpVectDissolve
    )
  )

  if (length(listColumnsValues) > 0) {
    for (n in names(listColumnsValues)) {
      dbRec[n] <- listColumnsValues[n]
    }
  } else {
    dbRec[idField] <- idPos
  }

  # rewrite
  dbWriteTable(dbCon, tmpVectDissolve, dbRec, overwrite = T)

  # export to shapefile.
  execGRASS("v.out.ogr",
    input = tmpVectDissolve,
    output = outPath,
    format = "ESRI_Shapefile",
    flags = outFlags,
    output_layer = outputShape
  )

  return(outPath)
}



#' rescale to given range
#' @param inputRast Text raster name to rescale
#' @param outputRast Text output raster name
#' @param reverse Boolean Inverse the scale
#' @export
amRasterRescale <- function(
  inputMask = NULL,
  inputRast,
  outputRast,
  range = c(0L, 10000L),
  weight = 1,
  reverse = FALSE,
  nullHandlerMethod = c("none", "min", "max")
) {
  if (amRastExists(inputMask)) {
    rmRastIfExists("MASK")
    execGRASS("r.mask", raster = inputMask, flags = "overwrite")
  }


  inMin <- amGetRasterStat(inputRast, "min")
  inMax <- amGetRasterStat(inputRast, "max")


  if (nullHandlerMethod %in% c("min", "max")) {
    # Input mask (candidate) can occurs were input raster ( map to rescale ) has NULL values.
    # we convert null to highest or lowest values depending on the scaling rescaling mode
    # This will work with travel time, as unreachead area could be seen as high priority,
    val <- ifelse(nullHandlerMethod == "min", inMin, inMax)
    execGRASS("r.null", map = inputRast, null = val)
  }


  # http://support.esri.com/cn/knowledgebase/techarticles/detail/30961
  if (inMin == inMax) {
    exprRescale <- sprintf(
      "%1$s = (%2$s * %3$s) * %4$s",
      outputRast,
      median(range),
      inputMask, # Mask does not seems to be applied there, so add it in the expression.
      weight
    )
  } else {
    if (reverse) {
      expr <- " %1$s = ( %8$s *( %4$s - ((%2$s - %3$s) * (%4$s - %5$s ) / (%6$s - %3$s)) + %5$s)) * %7$s "
    } else {
      expr <- " %1$s = ( %8$s * (((%2$s - %3$s) * (%4$s - %5$s ) / (%6$s - %3$s)) + %5$s)) * %7$s "
    }
    exprRescale <- sprintf(
      expr,
      outputRast, # 1
      inputRast, # 2
      inMin, # 3
      max(range), # 4
      min(range), # 5
      inMax, # 6
      weight, # 7
      inputMask # 8 mask does not seems to be applied in first case (first expr). Add it here to be sure.
    )
  }
  execGRASS("r.mapcalc", expression = exprRescale, flags = "overwrite")

  if (!is.null(inputMask)) {
    rmRastIfExists("MASK")
  }
  return(outputRast)
}




#' Import temporary shapefile catchment to final directory
#' @param shpFile Full path to temp catchment file . eg. /tmp/super.shp
#' @param outDir Directory path where are stored shapefile. eg. /home/am/data/shapefiles/
#' @param outName Name of the final catchment shapefile, without extension. e.g. catchments_001
#' @return Boolean Done
amMoveShp <- function(shpFile, outDir, outName) {
  #
  # Collect all shp related file and copy them to final directory.
  # NOTE: make sure that:
  # - pattern of shapefile is unique in its directory

  # in case of variable in path, convert outdir to fullpath
  if (length(shpFile) < 1) {
    return()
  }
  outDir <- system(sprintf("echo %s", outDir), intern = T)

  fe <- file.exists(shpFile)
  de <- dir.exists(outDir)
  so <- isTRUE(grep(".*\\.shp$", shpFile) > 0)

  if (!fe) {
    warning(
      sprintf("amMoveShp: %s input file does not exists", shpFile)
    )
  }
  if (!de) {
    warning(
      sprintf("amMoveShp: %s output directory does not exists", outDir)
    )
  }
  if (!so) {
    warning(
      sprintf("amMoveShp: %s input file does not have .shp extension", shpFile)
    )
  }

  ok <- c(fe, de, so)

  if (all(ok)) {
    # base name file for pattern.
    baseShape <- gsub(".shp", "", basename(shpFile))
    # list files (we can also use )
    allShpFiles <- list.files(dirname(shpFile), pattern = paste0("^", baseShape), full.names = TRUE)
    # Copy each files in final catchment directory.
    for (s in allShpFiles) {
      sExt <- file_ext(s)
      newPath <- file.path(outDir, paste0(outName, ".", sExt))
      file.copy(s, newPath, overwrite = T)
    }
  }
  return(all(ok))
}

#' Read or set cateogries from grass raster source
#' @param raster {string} Name of the raster layer to query
#' @param cateogies {vector} List of category to set
#' @export
amGetRasterCategory <- function(raster = NULL) {
  if (isEmpty(raster)) stop("No raster map name provided")

  tbl <- data.frame(integer(0), character(0))

  tblText <- execGRASS("r.category",
    map = raster,
    intern = T
  )

  if (!isEmpty(tblText)) {
    tbl <- read.csv(
      text = tblText,
      sep = "\t",
      header = F,
      stringsAsFactors = F
    )
    if (ncol(tbl) == 2) {
      tbl[, 1] <- as.integer(tbl[, 1])
    }
  }
  names(tbl) <- c("class", "label")
  return(tbl)
}

amTestLanguage <- function() {
  ams("tool_map_relocate_changes_count")
}

#' Get raster meta info
#'
#' @param {Character} raster Raster layer id
#' @return {data.frame} Raster info:
#' north          south           east           west          nsres
#' "-1632035.586" "-1828035.586"  "811692.6445"  "584692.6445"         "1000"
#'         ewres           rows           cols          cells       datatype
#'        "1000"          "196"          "227"        "44492"         "CELL"
#'         ncats
#'           "0"
#' @export
amRasterMeta <- function(raster = NULL) {
  tblMeta <- execGRASS("r.info",
    map = raster,
    flags = "g",
    intern = T
  ) %>%
    amCleanTableFromGrass(
      sep = "=",
      header = FALSE,
      col.names = c("name", "value")
    )
  out <- tblMeta$value
  names(out) <- tblMeta$name
  return(out)
}

#' Convert string of number separated by a comma to a number
#'
#' @param str {Charcter} String of numbers e.g "0,12,0012,10"
#' @param sep {Character} String of separator, default = ','
#' @param min {Numeric} Minimum number
#' @param max {Numeric} Maximum number
#' @param default {Numeric} Default number
#' @param asCeiling {Logical} asCeiling : round to the next intger
#' @return {Numeric}
amSplitToNum <- function(str, sep = ",", min = -Inf, max = Inf, default = 0, asCeiling = T) {
  if (is.numeric(str)) {
    return(str)
  }
  if (isEmpty(str)) {
    return(default)
  }
  suppressWarnings({
    out <- strsplit(str, sep)[[1]] %>%
      sapply(., as.numeric) %>%
      as.numeric(.) %>%
      na.omit(.)
  })
  out <- out[out >= min & out <= max]

  if (isEmpty(out)) {
    out <- default
  }
  if (isTRUE(as.integer)) {
    out <- ceiling(out)
  }
  return(out)
}

#' Subset of column in a data.frame
#' Ignore na, remove duplicates
#'
#' @param df data.frame
#' @param cols column names
#' @return data.frame
amTableSubsetCols <- function(df, cols) {
  cols <- na.omit(cols)
  cols <- unique(cols)
  cols <- cols[cols %in% names(df)]
  return(df[cols])
}
