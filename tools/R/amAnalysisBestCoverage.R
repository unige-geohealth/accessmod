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


#' amAnalysisBestCoverage
#'
#' Select facilities that offer the best population coverage.
#' @export
amAnalysisBestCoverage <- function(
  inputCatchment,         # Name of the input catchment vector layer
  inputPopulation,        # Name of the input population raster layer
  inputFacilities = NULL, # (Optional) Name of the facilities vector for admin check
  inputAdmin = NULL,      # (Optional) Name of the admin boundaries vector for admin check
  outputBestCoverage,     # Base name for the output table
  idField,                # Name of the facility ID column in catchment and facility layers
  adminColName = NULL,    # Name of the admin unit column in the admin layer
  nTot,                   # Total number of facilities to select
  adminCheck = FALSE,     # Whether to ensure a minimum number of facilities per admin unit
  npAdmin = NULL,         # Minimum number of facilities per admin unit
  pBarTitle               # Title for the progress bar
) {
  amGrassSessionStopIfInvalid()

  on_exit_add({
    rmRastIfExists("tmp__*")
    rmVectIfExists("tmp__*")
  })

  pbc(
    visible = TRUE,
    percent = 0,
    title = pBarTitle,
    text = ams("analysis_best_coverage_loading_inputs")
  )

  # Set region to population raster
  execGRASS("g.region", raster = inputPopulation)

  # Create working copy of population raster
  tmpPop <- "tmp__pop_work"
  execGRASS("r.mapcalc",
    expression = sprintf("%s = %s", tmpPop, inputPopulation),
    flags = "overwrite"
  )

  # Load catchment shapefile
  catchmentPath <- amGetShapesList(inputCatchment)[[1]]
  tempCatch <- sf::st_read(catchmentPath, quiet = TRUE)
  catch_cols <- colnames(tempCatch)
  has_join_id <- sprintf("%1$s_join", idField) %in% catch_cols
  has_id <- idField %in% catch_cols

  if (!isTRUE(has_join_id) && !isTRUE(has_id)) {
    stop(paste(idField, "is not a valid column name in the catchment shapefile."))
  }

  if (adminCheck) {
    # Load admin boundaries
    admin <- sf::st_read(amGrassVectPath(inputAdmin), quiet = TRUE)
    if (!adminColName %in% colnames(admin)) {
      stop(paste(adminColName, "is not a valid column name in the admin shapefile."))
    }

    # Load facilities (read_VECT returns SpatVector; convert to sf for sf:: methods below)
    hf <- sf::st_as_sf(read_VECT(inputFacilities))
    if (!idField %in% colnames(hf)) {
      stop(paste(idField, "is not a valid column name in the facility shapefile."))
    }

    # Create an admin column in the catchment attribute table
    tempCatch[, adminColName] <- NA
    for (i in 1:nrow(admin)) {
      adminSubName <- sf::st_drop_geometry(admin[i, adminColName])[1, 1]
      hfSub <- sf::st_drop_geometry(suppressWarnings(hf[sf::st_intersects(admin[i, ], hf, sparse = FALSE), ]))
      tempCatch[sf::st_drop_geometry(tempCatch[, idField])[, 1] %in% hfSub[, idField], adminColName] <- adminSubName
    }
    units <- na.omit(unique(sf::st_drop_geometry(tempCatch[, adminColName])[, 1]))
    hfCounts <- data.frame(admin = units, count = 0)
    finalTable <- data.frame(matrix(ncol = 3, nrow = nTot))
    names(finalTable) <- c("Facility name", "Population covered", "Region")
  } else {
    finalTable <- data.frame(matrix(ncol = 2, nrow = nTot))
    names(finalTable) <- c("Facility name", "Population covered")
  }

  # Deduplicate catchments by geometry
  tempCatch <- tempCatch[!duplicated(sf::st_geometry(tempCatch)), ]

  pbc(
    visible = TRUE,
    percent = 5,
    title = pBarTitle,
    text = ams("analysis_best_coverage_main_alg")
  )

  # Rasterize each catchment polygon as an individual mask
  nCatch <- nrow(tempCatch)
  maskNames <- character(nCatch)
  for (j in seq_len(nCatch)) {
    maskName <- sprintf("tmp__catch_mask_%d", j)
    maskNames[j] <- maskName
    write_VECT(terra::vect(tempCatch[j, ]), "tmp__catch_single", flags = c("overwrite"))
    execGRASS("v.to.rast",
      input = "tmp__catch_single",
      output = maskName,
      use = "val",
      value = 1,
      flags = "overwrite"
    )
  }

  pbc(
    visible = TRUE,
    percent = 10,
    title = pBarTitle,
    text = ams("analysis_best_coverage_main_alg")
  )

  # Extract initial population for each catchment
  tempCatch$totalpop <- 0
  for (j in seq_len(nCatch)) {
    rmRastIfExists("MASK")
    execGRASS("r.mask", raster = maskNames[j])
    tempCatch$totalpop[j] <- amGetRasterStat(tmpPop, "sum")
  }
  rmRastIfExists("MASK")
  tempCatch$totalpop0 <- tempCatch$totalpop

  # Track remaining indices
  remaining <- seq_len(nCatch)

  i <- 0
  while (i < nTot & length(remaining) > 0) {
    if (i > 0) {
      # Re-extract population from modified population raster
      for (ri in seq_along(remaining)) {
        j <- remaining[ri]
        rmRastIfExists("MASK")
        execGRASS("r.mask", raster = maskNames[j])
        tempCatch$totalpop[j] <- amGetRasterStat(tmpPop, "sum")
      }
      rmRastIfExists("MASK")
    }

    # Select best facility
    popValues <- tempCatch$totalpop[remaining]

    if (adminCheck) {
      notComplete <- hfCounts$admin[which(hfCounts$count < npAdmin)]
      if (length(notComplete) == 0) {
        bestIdx <- which.max(popValues)
      } else {
        tempAdmin <- sf::st_drop_geometry(tempCatch[remaining, adminColName])[, 1]
        validRows <- tempAdmin %in% notComplete
        bestIdx <- which(popValues == max(popValues[validRows], na.rm = TRUE))
      }
      if (length(bestIdx) > 1) {
        bestIdx <- bestIdx[which.max(tempCatch$totalpop0[remaining[bestIdx]])]
      }
      selAdmin <- sf::st_drop_geometry(tempCatch[remaining[bestIdx], adminColName])[1, 1]
      if (!is.na(selAdmin)) {
        hfCounts$count[hfCounts$admin == selAdmin] <- hfCounts$count[hfCounts$admin == selAdmin] + 1
      }
    } else {
      bestIdx <- which.max(popValues)
    }

    selectedRow <- remaining[bestIdx]

    i <- i + 1
    finalTable[i, "Facility name"] <- sf::st_drop_geometry(tempCatch[selectedRow, idField])[1, 1]
    finalTable[i, "Population covered"] <- tempCatch$totalpop[selectedRow]
    if (adminCheck) {
      finalTable[i, "Region"] <- sf::st_drop_geometry(tempCatch[selectedRow, adminColName])[1, 1]
    }

    # Zero out population in the selected catchment area
    execGRASS("r.mapcalc",
      expression = sprintf(
        "%1$s = if(!isnull(%2$s), null(), %1$s)",
        tmpPop, maskNames[selectedRow]
      ),
      flags = "overwrite"
    )

    # Remove selected from remaining
    remaining <- remaining[-bestIdx]

    pbc(
      visible = TRUE,
      percent = 10 + (i / nTot) * 90,
      title = pBarTitle,
      text = paste(i, "/", nTot)
    )
  }

  colNamesFT <- colnames(finalTable)
  finalTable <- finalTable[complete.cases(finalTable), ]
  if(nrow(finalTable) > 0) {
    finalTable$Rank <- 1:nrow(finalTable)
    finalTable$cumul <- cumsum(finalTable[, "Population covered"])
    finalTable <- finalTable[, c("Rank", colNamesFT, "cumul")]
    colnames(finalTable)[ncol(finalTable)] <- "Cumulative sum"
  }

  pbc(
    visible = TRUE,
    percent = 100,
    title = pBarTitle,
    text = ams("analysis_process_finished")
  )

  dbCon <- amMapsetGetDbCon()
  on_exit_add({
    dbDisconnect(dbCon)
  })
  dbWriteTable(
    dbCon,
    outputBestCoverage,
    finalTable,
    overwrite = T
  )

  pbc(visible = FALSE)
  return(finalTable)
}
