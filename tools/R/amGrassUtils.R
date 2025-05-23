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
  #
  # We removed amSubQuote() and added quote="" to solve the quotation issue
  # -> if this would re-introduce bugs, reevaluate amSubQuote with smarter
  #    'in word' quotation
  #
  tbl <- 
    read.table(
      text = text,
      sep = sep,
      quote = "",
      header = isTRUE(header),
      stringsAsFactor = FALSE,
      ...
    )

  if (isNotEmpty(cols)) {
    tbl <- tbl[cols]
  }
  return(tbl)
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
      return(isNotEmpty(layers))
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

getSqlitePath <- function(sqliteExpr) {
  # example of sqliteExpr: '$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'
  system(paste("echo", sqliteDB), intern = T)
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

  if (isNotEmpty(tblText)) {
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

# extract spatial polygons from mapMeta
amBboxSf <- function(mapMeta, proj = c("orig", "latlong")) {
  proj <- match.arg(proj)
  bbox <- mapMeta$bbxSp[[proj]]
  return(st_as_sf(bbox))
}

amMapMeta <- function() {
  # TODO: use one grid list, name this after
  meta <- list()
  gL <- gmeta()
  meta$location <- gL$LOCATION_NAME
  projGrassWkt <- getLocationProj()

  proj <- list(
    orig = projGrassWkt,
    latlong = st_crs("EPSG:4326")$wkt
  )

  #
  # build bounding box polygon
  # ( extent format -> -180,180,-90,90 )
  #
  rExtent <- rast()
  crs(rExtent) <- proj$orig
  ext(rExtent) <- c(gL$w, gL$e, gL$s, gL$n)
  bbx <- vect(ext(rExtent))
  crs(bbx) <- proj$orig
  bbxLatLong <- project(bbx, proj$latlong)
  #
  # Keep project and unprojected bbox in the same format
  #
  bbxSp <- list(
    orig = bbx,
    latlong = bbxLatLong
  )

  #
  # For each one, create a summary list
  #
  for (p in names(proj)) {
    bx <- bbxSp[[p]]
    bxD <- ext(bx)

    # (xmin, xmax, ymin, ymax)
    xMin <- as.numeric(bxD[1])
    xMax <- as.numeric(bxD[2])
    yMin <- as.numeric(bxD[3])
    yMax <- as.numeric(bxD[4])

    meta <- c(
      meta,
      structure(
        list(
          list(
            "proj" = proj[[p]],
            "bbx" = list(
              "ext" = list(
                "x" = list(
                  "min" = xMin * 1,
                  "max" = xMax * 1
                ),
                "y" = list(
                  "min" = yMin * 1,
                  "max" = yMax * 1
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

####### SECTION accessiblity analysis

#' Summarize Table Fields from SQLite Database
#'
#' This function provides a summary of the fields in a given SQLite table.
#' It categorizes fields based on their data type in R and checks if a field
#' is an index based on its unique values.
#'
#' @param table A character string specifying the name of the table in the
#' SQLite database.
#' @param dbCon A database connection object to the SQLite database.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{int}: Vector of field names that are of type integer.
#'     \item \code{num}: Vector of field names that are of type numeric .
#'     \item \code{char}: Vector of field names that are of type character.
#'     \item \code{idx}: Vector of field names that are considered indexes.
#'     \item \code{intIdx}: Vector of field names that are both integers and
#' indexes.
#'   }
#'
#'
#' @export
amGetFieldsSummary <- function(table, dbCon) {
  if (!table %in% dbListTables(dbCon)) {
    stop(paste("Table", table, "not found in database."))
  }

  # Query only a single row to get a sense of the R types
  tblSample <- dbGetQuery(
    dbCon,
    sprintf("SELECT * FROM %s LIMIT 1", table)
  )

  # Identify data types
  classes <- sapply(tblSample, class)
  numerics <- names(classes[classes %in% c("integer", "numeric")])
  integers <- names(classes[classes == "integer"])
  characters <- names(classes[classes == "character"])

  # Check for indexes based on unique values
  total_rows <- dbGetQuery(
    dbCon,
    sprintf("SELECT COUNT(*) count FROM %s", table)
  )$count

  unique_counts <- sapply(names(tblSample), function(col) {
    dbGetQuery(
      dbCon,
      sprintf("SELECT COUNT(DISTINCT %1$s) as count FROM %2$s", col, table)
    )$count
  })

  indexes <- names(unique_counts[unique_counts == total_rows])
  intIdx <- intersect(indexes, integers)

  # Return summary
  list(
    int = integers,
    num = numerics,
    char = characters,
    idx = indexes,
    intIdx = intIdx
  )
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
