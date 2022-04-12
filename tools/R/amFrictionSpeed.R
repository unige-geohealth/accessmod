#' amCreateSpeedMap
#'
#' @export
amCreateSpeedMap <- function(tbl, mapMerged, mapSpeed) {
  # creation of new classes for speed map (class+km/h), used in r.walk.accessmod
  # Exemples of rules:
  # oldClasses = newClasses \t newlabels
  # 1 2 3 = 1002 \t WALKING:2
  # 4 =  2020 \t BICYCLING:20
  # 1002 = 3080 \t MOTORIZED:80
  tbl[, "newClass"] <- integer()
  # for each row of the model table...
  for (i in 1:nrow(tbl)) {
    # ... get the mode
    mod <- tbl[i, "mode"]
    # ... corrsponding to the predefined value listTranspMod + given speed
    tbl[i, "newClass"] <- (
      as.integer(
        config$listTranspMod[[mod]]$rastVal
      ) + tbl[i, "speed"]
    ) * 1000
  }


  #
  # Ignore speed = 0 in reclass
  #
  tbl <- tbl[tbl$speed != 0, ]

  #
  # For all other classes, create a reclass
  #
  uniqueNewClass <- unique(tbl$newClass)
  reclassRules <- character()
  for (u in uniqueNewClass) {
    oldClasses <- tbl[tbl$newClass == u, "class"]
    modeSpeedLabel <- paste(
      tbl[
        tbl$newClass == u,
        c("mode", "speed")
      ][1, ],
      collapse = ":"
    )
    classRule <- paste(
      paste(
        oldClasses,
        collapse = " "
      ), "=",
      u,
      "\t",
      modeSpeedLabel
    )
    reclassRules <- c(reclassRules, classRule)
  }

  tmpFile <- tempfile()
  write(reclassRules, tmpFile)
  #
  # Reclass the merged landcover
  #
  execGRASS("r.reclass",
    input = mapMerged,
    output = mapSpeed,
    rules = tmpFile,
    flags = "overwrite"
  )
}

#' amCreateFrictionMap
#'
#' @export
amCreateFrictionMap <- function(tbl, mapMerged, mapFriction, mapResol) {
  amDebugMsg("amCreateFrictionMap")

  # creaction of new classes for cost map (seconds) used in r.cost.
  tbl[, "newClass"] <- numeric()
  tbl[, "mode"] <- "isotropic"

  #
  # Ignore speed = 0 in reclass
  #
  tbl <- tbl[tbl$speed != 0, ]


  # for each row of the model table...
  for (i in 1:nrow(tbl)) {
    # km/h to s/m
    # the time to cover one unit of distance * actual i
    # distance (map resolution) == cost to cross a given cell.
    tbl[i, "newClass"] <- (1 / (tbl[i, "speed"] / 3.6)) * mapResol
  }

  # unique new class
  uniqueNewClass <- unique(tbl$newClass)
  reclassRules <- character()
  categoryRules <- character()


  for (u in uniqueNewClass) {
    oldClasses <- tbl[tbl$newClass == u, "class"]
    modeSpeedLabel <- paste(
      tbl[
        tbl$newClass == u,
        c("mode", "speed")
      ][1, ],
      collapse = ":"
    )
    reclassRule <- paste0(oldClasses, ":", oldClasses, ":", u, ":", u)
    reclassRules <- c(reclassRules, reclassRule)
    catLabel <- paste(
      paste(tbl[tbl$newClass == u, ]$label, collapse = "/"),
      u, "[s]/", mapResol, "[m]"
    )
    categoryRule <- paste0(u, ":", catLabel)
    categoryRules <- c(categoryRules, categoryRule)
  }

  tmpFile <- tempfile()
  write(reclassRules, tmpFile)
  execGRASS("r.recode",
    input = mapMerged,
    output = mapFriction,
    rules = tmpFile,
    flags = "overwrite"
  )

  write(categoryRules, tmpFile)
  execGRASS("r.category",
    map = mapFriction,
    separator = ":",
    rules = tmpFile
  )
}





