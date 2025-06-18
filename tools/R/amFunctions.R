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


#' Remove unwanted character for mapcalc
#' @param text 
#'
amRmN <-function(text){
   gsub("\\n", "",text)
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



# function to create selectize compatible list of value
selectListMaker <- function(vect, default) {
  vect <- c(default, vect)
  vect <- amSubPunct(vect)
}


# http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
amCleanHtml <- function(htmlString) {
  return(gsub("<.*?>", "", paste(htmlString)))
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

# https://gist.github.com/jmarhee/8530768
amMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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


amTestLanguage <- function() {
  ams("tool_map_relocate_changes_count")
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
