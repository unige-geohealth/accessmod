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


#' amBestCoverage_assignAdminCol
#'
#' Assign admin region label and ID to each catchment row via spatial join.
#' @param catchSf sf; catchment layer
#' @param admin sf; admin boundaries layer
#' @param adminColName character; label column name in admin
#' @param adminIdColName character; ID column name in admin
#' @param hf sf; health facility points layer
#' @param idFieldCatchment character; ID column name in catchSf
#' @param idFieldHf character; ID column name in hf
#' @return catchSf with adminColName and adminIdColName columns populated
#' @export
amBestCoverage_assignAdminCol <- function(catchSf, admin, adminColName, adminIdColName, hf, idFieldCatchment, idFieldHf) {
  catchSf[, adminColName] <- NA
  catchSf[, adminIdColName] <- NA
  for (i in seq_len(nrow(admin))) {
    adminName <- sf::st_drop_geometry(admin[i, adminColName])[1, 1]
    adminId <- sf::st_drop_geometry(admin[i, adminIdColName])[1, 1]
    hfInAdmin <- sf::st_drop_geometry(
      suppressWarnings(hf[sf::st_intersects(admin[i, ], hf, sparse = FALSE), ])
    )
    matchRows <- sf::st_drop_geometry(catchSf[, idFieldCatchment])[, 1] %in% hfInAdmin[, idFieldHf]
    catchSf[matchRows, adminColName] <- adminName
    catchSf[matchRows, adminIdColName] <- adminId
  }
  return(catchSf)
}


#' amBestCoverage_rasterizeMasks
#'
#' Rasterize each catchment polygon as an individual GRASS mask raster.
#' NOTE: catchments may heavily overlap; raster masks avoid GRASS topology issues.
#' @param catchSf sf; deduplicated catchment layer
#' @param nCatch integer; number of catchments
#' @return character vector of GRASS raster mask names (tmp__ prefix)
#' @export
amBestCoverage_rasterizeMasks <- function(catchSf, nCatch) {
  tmpMaskNames <- character(nCatch)
  for (j in seq_len(nCatch)) {
    tmpMaskName <- sprintf("tmp__catch_mask_%d", j)
    tmpMaskNames[j] <- tmpMaskName
    write_VECT(terra::vect(catchSf[j, ]), "tmp__catch_single", flags = c("overwrite"))
    execGRASS("v.to.rast",
      input = "tmp__catch_single",
      output = tmpMaskName,
      use = "val",
      value = 1,
      flags = "overwrite"
    )
  }
  return(tmpMaskNames)
}


#' amBestCoverage_selectBestIdx
#'
#' Select the index of the best facility for one algorithm iteration.
#' Applies admin constraint when required, breaks ties using initial population.
#' @param popCurrent numeric; current population values for remaining catchments
#' @param catchSf sf; catchment layer (all rows, indexed by remainingIdx)
#' @param remainingIdx integer; indices of still-available catchments in catchSf
#' @param adminCheck logical; whether admin quota constraint is active
#' @param tblAdminCounts data.frame; tracks selected count per admin unit (cols: admin, count)
#' @param npAdmin integer; minimum selections required per admin unit
#' @param adminColName character; column in catchSf holding admin unit labels
#' @return list(bestIdx = integer position in remainingIdx, tblAdminCounts = updated table)
#' @export
amBestCoverage_selectBestIdx <- function(popCurrent, catchSf, remainingIdx, adminCheck, tblAdminCounts, npAdmin, adminColName) {
  if (adminCheck) {
    adminNotComplete <- tblAdminCounts$admin[which(tblAdminCounts$count < npAdmin)]
    if (length(adminNotComplete) == 0) {
      bestIdx <- which.max(popCurrent)
    } else {
      adminLabels <- sf::st_drop_geometry(catchSf[remainingIdx, adminColName])[, 1]
      adminValidRows <- adminLabels %in% adminNotComplete
      bestIdx <- which(popCurrent == max(popCurrent[adminValidRows], na.rm = TRUE))
    }
    if (length(bestIdx) > 1) {
      bestIdx <- bestIdx[which.max(catchSf$totalpop0[remainingIdx[bestIdx]])]
    }
    selectedAdmin <- sf::st_drop_geometry(catchSf[remainingIdx[bestIdx], adminColName])[1, 1]
    if (!is.na(selectedAdmin)) {
      tblAdminCounts$count[tblAdminCounts$admin == selectedAdmin] <-
        tblAdminCounts$count[tblAdminCounts$admin == selectedAdmin] + 1
    }
  } else {
    bestIdx <- which.max(popCurrent)
  }
  return(list(bestIdx = bestIdx, tblAdminCounts = tblAdminCounts))
}


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
  idFieldCatchment,       # ID column name in the catchment shapefile
  idFieldHf,              # ID column name in the facility GRASS vector
  adminColName = NULL,    # Label column name in the admin layer
  adminIdColName = NULL,  # ID column name in the admin layer
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
  catchSf <- sf::st_read(catchmentPath, quiet = TRUE)
  if (!idFieldCatchment %in% colnames(catchSf)) {
    stop(paste(idFieldCatchment, "is not a valid column name in the catchment shapefile."))
  }

  if (adminCheck) {
    # Load admin boundaries (read_VECT returns SpatVector; convert to sf for sf:: methods below)
    admin <- sf::st_as_sf(read_VECT(inputAdmin))
    if (!adminColName %in% colnames(admin)) {
      stop(paste(adminColName, "is not a valid column name in the admin shapefile."))
    }
    if (!adminIdColName %in% colnames(admin)) {
      stop(paste(adminIdColName, "is not a valid column name in the admin shapefile."))
    }

    # Load facilities (read_VECT returns SpatVector; convert to sf for sf:: methods below)
    hf <- sf::st_as_sf(read_VECT(inputFacilities))
    if (!idFieldHf %in% colnames(hf)) {
      stop(paste(idFieldHf, "is not a valid column name in the facility shapefile."))
    }

    catchSf <- amBestCoverage_assignAdminCol(catchSf, admin, adminColName, adminIdColName, hf, idFieldCatchment, idFieldHf)

    adminUnits <- na.omit(unique(sf::st_drop_geometry(catchSf[, adminColName])[, 1]))
    if (npAdmin * length(adminUnits) > nTot) {
      stop("npAdmin * number of administrative units > nTot")
    }
    tblAdminCounts <- data.frame(admin = adminUnits, count = 0)
    tblResult <- data.frame(matrix(ncol = 4, nrow = nTot))
    names(tblResult) <- c("amFacilityName", "amPopCovered", "amAdminRegion", "amAdminId")
  } else {
    tblResult <- data.frame(matrix(ncol = 2, nrow = nTot))
    names(tblResult) <- c("amFacilityName", "amPopCovered")
  }

  # Deduplicate catchments by geometry
  catchSf <- catchSf[!duplicated(sf::st_geometry(catchSf)), ]

  pbc(
    visible = TRUE,
    percent = 5,
    title = pBarTitle,
    text = ams("analysis_best_coverage_main_alg")
  )

  # Rasterize each catchment polygon as an individual GRASS mask
  nCatch <- nrow(catchSf)
  tmpMaskNames <- amBestCoverage_rasterizeMasks(catchSf, nCatch)

  pbc(
    visible = TRUE,
    percent = 10,
    title = pBarTitle,
    text = ams("analysis_best_coverage_main_alg")
  )

  # Extract initial population for each catchment
  catchSf$totalpop <- 0
  for (j in seq_len(nCatch)) {
    rmRastIfExists("MASK")
    execGRASS("r.mask", raster = tmpMaskNames[j])
    catchSf$totalpop[j] <- amGetRasterStat(tmpPop, "sum")
  }
  rmRastIfExists("MASK")
  catchSf$totalpop0 <- catchSf$totalpop

  # Track remaining indices
  remainingIdx <- seq_len(nCatch)

  i <- 0
  while (i < nTot & length(remainingIdx) > 0) {
    if (i > 0) {
      # Re-extract population from modified population raster
      for (ri in seq_along(remainingIdx)) {
        j <- remainingIdx[ri]
        rmRastIfExists("MASK")
        execGRASS("r.mask", raster = tmpMaskNames[j])
        catchSf$totalpop[j] <- amGetRasterStat(tmpPop, "sum")
      }
      rmRastIfExists("MASK")
    }

    # Select best facility for this iteration
    popCurrent <- catchSf$totalpop[remainingIdx]
    sel <- amBestCoverage_selectBestIdx(
      popCurrent, catchSf, remainingIdx,
      adminCheck,
      if (adminCheck) tblAdminCounts else NULL,
      if (adminCheck) npAdmin else NULL,
      if (adminCheck) adminColName else NULL
    )
    bestIdx <- sel$bestIdx
    if (adminCheck) {
      tblAdminCounts <- sel$tblAdminCounts
    }

    selectedIdx <- remainingIdx[bestIdx]

    i <- i + 1
    tblResult[i, "amFacilityName"] <- sf::st_drop_geometry(catchSf[selectedIdx, idFieldCatchment])[1, 1]
    tblResult[i, "amPopCovered"] <- catchSf$totalpop[selectedIdx]
    if (adminCheck) {
      tblResult[i, "amAdminRegion"] <- sf::st_drop_geometry(catchSf[selectedIdx, adminColName])[1, 1]
      tblResult[i, "amAdminId"] <- sf::st_drop_geometry(catchSf[selectedIdx, adminIdColName])[1, 1]
    }

    # Zero out population in the selected catchment area
    execGRASS("r.mapcalc",
      expression = sprintf(
        "%1$s = if(!isnull(%2$s), null(), %1$s)",
        tmpPop, tmpMaskNames[selectedIdx]
      ),
      flags = "overwrite"
    )

    # Remove selected from remaining
    remainingIdx <- remainingIdx[-bestIdx]

    pbc(
      visible = TRUE,
      percent = 10 + (i / nTot) * 90,
      title = pBarTitle,
      text = paste(i, "/", nTot)
    )
  }

  tblResultCols <- colnames(tblResult)
  tblResult <- tblResult[complete.cases(tblResult), ]
  if (nrow(tblResult) > 0) {
    tblResult$amRank <- seq_len(nrow(tblResult))
    tblResult$amPopCoveredCumul <- cumsum(tblResult[, "amPopCovered"])
    tblResult <- tblResult[, c("amRank", tblResultCols, "amPopCoveredCumul")]
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
    tblResult,
    overwrite = T
  )

  pbc(visible = FALSE)
  return(tblResult)
}
