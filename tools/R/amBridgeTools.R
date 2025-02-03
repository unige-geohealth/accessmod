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
  if (isNotEmpty(nBridges) || isTRUE(nBridges > 0)) {
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
