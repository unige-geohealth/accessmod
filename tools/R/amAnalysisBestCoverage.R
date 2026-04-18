#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling
#    physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser
#    (GeoHealth group, University of Geneva)
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


#' amBestCoverage_buildDuplicateGroups
#'
#' Before deduplication, find facilities that share identical catchment
#' geometries and build a lookup of grouped labels.
#' Used to report "Facility A // Facility B" instead of silently
#' discarding the duplicate.
#'
#' @param catchments sf; full catchment layer (before deduplication)
#' @param idFieldCatchment character; ID column in catchments
#' @return named character vector: primaryName -> "A // B // C"
#'   (only entries with more than one facility per geometry)
#' @export
amBestCoverage_buildDuplicateGroups <- function(
  catchments,
  idFieldCatchment
) {
  geomEquals <- sf::st_equals(catchments, sparse = TRUE)
  groups <- character(0)

  # st_equals() includes self-match:
  # 1: 1       -> unique geometry
  # 1: 1, 4    -> rows 1 and 4 share one geometry
  for (i in seq_len(nrow(catchments))) {
    equalIdx <- sort(as.integer(geomEquals[[i]]))

    if (length(equalIdx) > 1 && equalIdx[1] == i) {
      namesRow <- catchments[equalIdx, idFieldCatchment]
      facilityNames <- sf::st_drop_geometry(namesRow)[, 1]
      primaryName <- facilityNames[1]

      groups[primaryName] <- paste(
        facilityNames,
        collapse = " // "
      )
    }
  }

  return(groups)
}


#' amBestCoverage_assignAdminCol
#'
#' Assign an admin region to each catchment row by spatially joining
#' health facility points against administrative boundaries.
#'
#' @param catchments sf; catchment layer
#' @param adminBoundaries sf; administrative boundaries
#' @param adminColName character; label column in adminBoundaries
#' @param adminIdColName character; ID column in adminBoundaries
#' @param facilities sf; health facility points
#' @param idFieldCatchment character; ID column in catchments
#' @param idFieldHf character; ID column in facilities
#' @return catchments with adminColName and adminIdColName columns populated
#' @export
amBestCoverage_assignAdminCol <- function(
  catchments,
  adminBoundaries,
  adminColName,
  adminIdColName,
  facilities,
  idFieldCatchment,
  idFieldHf
) {
  catchments[, adminColName] <- NA
  catchments[, adminIdColName] <- NA

  for (i in seq_len(nrow(adminBoundaries))) {
    adminRow <- adminBoundaries[i, ]
    adminLabel <- sf::st_drop_geometry(adminRow[, adminColName])[1, 1]
    adminId <- sf::st_drop_geometry(adminRow[, adminIdColName])[1, 1]

    intersecting <- sf::st_intersects(
      adminRow,
      facilities,
      sparse = FALSE
    )
    facilitiesInAdmin <- facilities[intersecting, ]
    facilitiesInAdmin <- sf::st_drop_geometry(facilitiesInAdmin)

    catchmentIds <- sf::st_drop_geometry(
      catchments[, idFieldCatchment]
    )[, 1]
    inThisAdmin <- catchmentIds %in% facilitiesInAdmin[, idFieldHf]

    catchments[inThisAdmin, adminColName] <- adminLabel
    catchments[inThisAdmin, adminIdColName] <- adminId
  }

  return(catchments)
}


#' amBestCoverage_getMissingFacilityIds
#'
#' Return catchment IDs that cannot be found in the selected facility layer.
#' Admin assignment only needs every catchment row to match one facility row;
#' extra facilities in the point layer are allowed.
#'
#' @param catchments sf; catchment layer
#' @param facilities sf; health facility points
#' @param idFieldCatchment character; ID column in catchments
#' @param idFieldHf character; ID column in facilities
#' @return character vector of missing catchment IDs
#' @export
amBestCoverage_getMissingFacilityIds <- function(
  catchments,
  facilities,
  idFieldCatchment,
  idFieldHf
) {
  idsCatchment <- unique(na.omit(sf::st_drop_geometry(
    catchments
  )[, idFieldCatchment]))
  idsFacility <- unique(na.omit(sf::st_drop_geometry(
    facilities
  )[, idFieldHf]))

  return(as.character(setdiff(idsCatchment, idsFacility)))
}


#' amBestCoverage_checkFacilityMatch
#'
#' Ensure every catchment identifier is present in the selected facility layer
#' before the admin constraint is applied.
#'
#' @param catchments sf; catchment layer
#' @param facilities sf; health facility points
#' @param idFieldCatchment character; ID column in catchments
#' @param idFieldHf character; ID column in facilities
#' @return invisible(TRUE) if all catchment IDs can be matched
#' @export
amBestCoverage_checkFacilityMatch <- function(
  catchments,
  facilities,
  idFieldCatchment,
  idFieldHf
) {
  missingIds <- amBestCoverage_getMissingFacilityIds(
    catchments = catchments,
    facilities = facilities,
    idFieldCatchment = idFieldCatchment,
    idFieldHf = idFieldHf
  )

  if (length(missingIds) > 0) {
    idsPreview <- paste(utils::head(missingIds, 5), collapse = ", ")
    if (length(missingIds) > 5) {
      idsPreview <- paste0(idsPreview, ", ...")
    }

    stop(
      sprintf(
        ams("analysis_best_coverage_missing_facility_ids"),
        idFieldCatchment,
        idFieldHf,
        length(missingIds),
        idsPreview
      )
    )
  }

  return(invisible(TRUE))
}


#' amBestCoverage_extractPopulation
#'
#' Extract exact covered population for each catchment polygon.
#' Uses exactextractr polygon extraction so partially covered cells
#' contribute proportionally to the sum.
#'
#' @param populationRaster SpatRaster; population raster
#' @param catchments sf; catchment layer
#' @return numeric vector; exact population sum for each catchment row
#' @export
amBestCoverage_extractPopulation <- function(
  populationRaster,
  catchments
) {
  if (nrow(catchments) == 0) {
    return(numeric(0))
  }

  popExtract <- exactextractr::exact_extract(
    populationRaster,
    catchments,
    fun = "sum",
    progress = FALSE
  )

  popValues <- as.numeric(popExtract)
  popValues[is.na(popValues)] <- 0

  return(popValues)
}


#' amBestCoverage_extractPopulationChunked
#'
#' Extract exact covered population in chunks to reduce memory pressure and
#' allow progress updates on large inputs.
#'
#' @param populationRaster SpatRaster; population raster
#' @param catchments sf; catchment layer
#' @param chunkSize integer; number of polygons per chunk
#' @param pBarTitle character; progress bar title
#' @param percentFrom numeric; progress start
#' @param percentTo numeric; progress end
#' @export
amBestCoverage_extractPopulationChunked <- function(
  populationRaster,
  catchments,
  chunkSize = 100,
  pBarTitle = NULL,
  percentFrom = NULL,
  percentTo = NULL
) {
  nCatchments <- nrow(catchments)

  if (nCatchments == 0) {
    return(numeric(0))
  }

  popValues <- numeric(nCatchments)
  chunkStart <- seq(1, nCatchments, by = chunkSize)

  for (i in seq_along(chunkStart)) {
    idxStart <- chunkStart[i]
    idxEnd <- min(idxStart + chunkSize - 1, nCatchments)
    idx <- idxStart:idxEnd

    popValues[idx] <- amBestCoverage_extractPopulation(
      populationRaster = populationRaster,
      catchments = catchments[idx, ]
    )

    if (
      isNotEmpty(pBarTitle) &&
      isNotEmpty(percentFrom) &&
      isNotEmpty(percentTo)
    ) {
      chunkProgress <- i / length(chunkStart)
      percentNow <- percentFrom + (percentTo - percentFrom) * chunkProgress

      pbc(
        visible = TRUE,
        percent = percentNow,
        title = pBarTitle,
        text = ams("analysis_best_coverage_main_alg")
      )
    }
  }

  return(popValues)
}


#' amBestCoverage_selectCandidate
#'
#' Select the next catchment according to the standalone best coverage
#' rules, including admin quotas and tie-breaking on initial coverage.
#'
#' @param catchments sf; current candidate catchments
#' @param adminCheck logical; enforce admin quotas
#' @param adminColName character; admin label column in catchments
#' @param npAdmin integer; minimum number of facilities per admin unit
#' @param adminCounts data.frame; quota counter table
#' @return list with selected row index and updated adminCounts
#' @export
amBestCoverage_selectCandidate <- function(
  catchments,
  adminCheck,
  adminColName,
  npAdmin,
  adminCounts
) {
  if (adminCheck) {
    adminsIncomplete <- adminCounts$admin[
      adminCounts$count < npAdmin
    ]

    if (length(adminsIncomplete) == 0) {
      candidateIndex <- which(
        catchments$totalPop == max(catchments$totalPop)
      )
    } else {
      adminValues <- sf::st_drop_geometry(catchments[, adminColName])[, 1]
      rowsValid <- adminValues %in% adminsIncomplete

      candidateIndex <- which(
        catchments$totalPop ==
          max(catchments$totalPop[rowsValid])
      )
    }
  } else {
    candidateIndex <- which(
      catchments$totalPop == max(catchments$totalPop)
    )
  }

  if (length(candidateIndex) > 1) {
    candidateIndex <- candidateIndex[
      which(
        catchments$initialPop[candidateIndex] ==
          max(catchments$initialPop[candidateIndex])
      )
    ][1]
  }

  if (adminCheck) {
    selectedAdmin <- sf::st_drop_geometry(
      catchments[candidateIndex, adminColName]
    )[1, 1]

    adminCounts$count[adminCounts$admin == selectedAdmin] <-
      adminCounts$count[adminCounts$admin == selectedAdmin] + 1
  }

  return(list(
    candidateIndex = candidateIndex,
    adminCounts = adminCounts
  ))
}


#' amBestCoverage_reduceOverlaps
#'
#' Remove the part of each remaining catchment already covered by the
#' selected catchment and update population only for changed geometries.
#'
#' @param catchments sf; remaining candidate catchments
#' @param selectedCatchment sf; chosen catchment
#' @param populationRaster SpatRaster; population raster
#' @param catchmentsRemoved sf; catchments fully contained in selectedCatchment
#' @return list with updated catchments and catchmentsRemoved
#' @export
amBestCoverage_reduceOverlaps <- function(
  catchments,
  selectedCatchment,
  populationRaster,
  catchmentsRemoved
) {
  if (nrow(catchments) == 0) {
    return(list(
      catchments = catchments,
      catchmentsRemoved = catchmentsRemoved
    ))
  }

  intersectingIndex <- which(
    sf::st_intersects(
      catchments,
      selectedCatchment,
      sparse = FALSE
    )[, 1]
  )

  if (length(intersectingIndex) == 0) {
    return(list(
      catchments = catchments,
      catchmentsRemoved = catchmentsRemoved
    ))
  }

  idxChanged <- integer(0)

  for (i in intersectingIndex) {
    # Difference only the geometry. Attributes remain attached to the row.
    catchmentReducedGeom <- sf::st_difference(
      sf::st_geometry(catchments[i, ]),
      sf::st_geometry(selectedCatchment)
    )

    # Empty geometry means the candidate was fully covered by the selected
    # catchment. Keep it aside so nTot can still be reached later if needed.
    if (all(sf::st_is_empty(catchmentReducedGeom))) {
      catchments$totalPop[i] <- 0
      catchmentsRemoved <- rbind(
        catchmentsRemoved,
        catchments[i, ]
      )
      next
    }

    catchmentsReduced <- catchments[i, ]
    sf::st_geometry(catchmentsReduced) <- catchmentReducedGeom
    catchments[i, ] <- catchmentsReduced
    idxChanged <- c(idxChanged, i)
  }

  if (length(idxChanged) > 0) {
    catchments$totalPop[idxChanged] <- amBestCoverage_extractPopulation(
      populationRaster = populationRaster,
      catchments = catchments[idxChanged, ]
    )
  }

  catchments <- catchments[
    catchments$totalPop > 0,
  ]

  return(list(
    catchments = catchments,
    catchmentsRemoved = catchmentsRemoved
  ))
}


#' amAnalysisBestCoverage
#'
#' Select the facilities that offer the best cumulative population coverage.
#' Greedy algorithm: at each step the facility whose catchment covers the most
#' remaining population is selected. Its geometry is then subtracted from the
#' remaining catchments so overlapping population is not double-counted.
#'
#' Facilities sharing an identical catchment geometry are reported as a
#' group ("Facility A // Facility B") rather than silently discarded,
#' so the result remains auditable.
#'
#' Output columns:
#' adminCheck = FALSE -> amRank, amFacilityName, amPopCovered, amPopCoveredCumul
#' adminCheck = TRUE  -> amRank, amFacilityName, amPopCovered,
#'                       amAdminRegion, amAdminId, amPopCoveredCumul
#' @export
amAnalysisBestCoverage <- function(
  inputCatchment,
  inputPopulation,
  inputFacilities = NULL,
  inputAdmin = NULL,
  outputBestCoverage,
  idFieldCatchment,
  idFieldHf,
  adminColName = NULL,
  adminIdColName = NULL,
  nTot,
  adminCheck = FALSE,
  npAdmin = NULL,
  pBarTitle
) {
  amGrassSessionStopIfInvalid()

  pbc(
    visible = TRUE,
    percent = 0,
    title = pBarTitle,
    text = ams("analysis_best_coverage_loading_inputs")
  )

  #
  # Load inputs
  #
  execGRASS("g.region",
    raster = inputPopulation
  )
  on_exit_add({
    amRegionReset()
  })

  populationRaster <- read_RAST(inputPopulation)

  catchmentPath <- amGetShapesList(inputCatchment)[[1]]
  catchments <- sf::st_read(
    catchmentPath,
    quiet = TRUE
  )

  if (!idFieldCatchment %in% colnames(catchments)) {
    stop(paste(
      idFieldCatchment,
      "is not a valid column name in the catchment shapefile."
    ))
  }

  #
  # Admin constraint setup
  #
  if (adminCheck) {
    adminBoundaries <- sf::st_as_sf(read_VECT(inputAdmin))
    facilities <- sf::st_as_sf(read_VECT(inputFacilities))

    if (!adminColName %in% colnames(adminBoundaries)) {
      stop(paste(
        adminColName,
        "is not a valid column name in the admin shapefile."
      ))
    }
    if (!adminIdColName %in% colnames(adminBoundaries)) {
      stop(paste(
        adminIdColName,
        "is not a valid column name in the admin shapefile."
      ))
    }
    if (!idFieldHf %in% colnames(facilities)) {
      stop(paste(
        idFieldHf,
        "is not a valid column name in the facility shapefile."
      ))
    }

    amBestCoverage_checkFacilityMatch(
      catchments = catchments,
      facilities = facilities,
      idFieldCatchment = idFieldCatchment,
      idFieldHf = idFieldHf
    )

    catchments <- amBestCoverage_assignAdminCol(
      catchments = catchments,
      adminBoundaries = adminBoundaries,
      adminColName = adminColName,
      adminIdColName = adminIdColName,
      facilities = facilities,
      idFieldCatchment = idFieldCatchment,
      idFieldHf = idFieldHf
    )

    adminUnitCol <- sf::st_drop_geometry(
      catchments[, adminColName]
    )[, 1]
    adminUnits <- unique(adminUnitCol)

    if (npAdmin * length(adminUnits) > nTot) {
      stop("npAdmin * number of administrative units > nTot")
    }

    adminCounts <- data.frame(
      admin = adminUnits,
      count = 0
    )

    result <- data.frame(
      matrix(ncol = 4, nrow = nTot)
    )
    names(result) <- c(
      "amFacilityName",
      "amPopCovered",
      "amAdminRegion",
      "amAdminId"
    )
  } else {
    adminCounts <- data.frame()

    result <- data.frame(
      matrix(ncol = 2, nrow = nTot)
    )
    names(result) <- c(
      "amFacilityName",
      "amPopCovered"
    )
  }

  #
  # Detect identical geometries before deduplication, then restore grouped
  # labels only in the final table.
  #
  duplicateGroups <- amBestCoverage_buildDuplicateGroups(
    catchments = catchments,
    idFieldCatchment = idFieldCatchment
  )

  if (length(duplicateGroups) > 0) {
    amMsg(
      type = "log",
      text = paste(
        length(duplicateGroups),
        "group(s) of facilities share an identical catchment geometry."
      )
    )
  }

  #
  # Main loop works on one row per geometry.
  #
  isDuplicate <- duplicated(sf::st_geometry(catchments))
  catchments <- catchments[!isDuplicate, ]

  pbc(
    visible = TRUE,
    percent = 5,
    title = pBarTitle,
    text = ams("analysis_best_coverage_main_alg")
  )

  #
  # initialPop is tie-break only. totalPop is updated after overlap removal.
  #
  catchments$totalPop <- amBestCoverage_extractPopulationChunked(
    populationRaster = populationRaster,
    catchments = catchments,
    chunkSize = 100,
    pBarTitle = pBarTitle,
    percentFrom = 5,
    percentTo = 10
  )
  catchments$initialPop <- catchments$totalPop

  pbc(
    visible = TRUE,
    percent = 10,
    title = pBarTitle,
    text = ams("analysis_best_coverage_main_alg")
  )

  #
  # Greedy selection loop
  #
  selected <- 0
  # Fully contained catchments are appended at the end to preserve legacy
  # standalone behaviour when unique residual coverage is exhausted.
  catchmentsRemoved <- catchments[0, ]

  while (selected < nTot && nrow(catchments) > 0) {
    candidateSelection <- amBestCoverage_selectCandidate(
      catchments = catchments,
      adminCheck = adminCheck,
      adminColName = adminColName,
      npAdmin = npAdmin,
      adminCounts = adminCounts
    )

    candidateIndex <- candidateSelection$candidateIndex
    adminCounts <- candidateSelection$adminCounts
    selected <- selected + 1

    facilityRow <- catchments[candidateIndex, ]
    facilityName <- sf::st_drop_geometry(
      facilityRow[, idFieldCatchment]
    )[1, 1]

    result[selected, "amFacilityName"] <- facilityName
    result[selected, "amPopCovered"] <- catchments$totalPop[candidateIndex]

    if (adminCheck) {
      result[selected, "amAdminRegion"] <- sf::st_drop_geometry(
        facilityRow[, adminColName]
      )[1, 1]
      result[selected, "amAdminId"] <- sf::st_drop_geometry(
        facilityRow[, adminIdColName]
      )[1, 1]
    }

    selectedCatchment <- catchments[candidateIndex, ]
    catchments <- catchments[-candidateIndex, ]

    overlapReduction <- amBestCoverage_reduceOverlaps(
      catchments = catchments,
      selectedCatchment = selectedCatchment,
      populationRaster = populationRaster,
      catchmentsRemoved = catchmentsRemoved
    )

    catchments <- overlapReduction$catchments
    catchmentsRemoved <- overlapReduction$catchmentsRemoved

    pbc(
      visible = TRUE,
      percent = 10 + (selected / nTot) * 90,
      title = pBarTitle,
      text = paste(selected, "/", nTot)
    )
  }

  #
  # If the requested number was not reached, append catchments fully
  # contained in already selected ones, preserving standalone behaviour.
  #
  nToAdd <- nTot - selected

  if (nToAdd > 0 && nrow(catchmentsRemoved) > 0) {
    nToAdd <- min(nToAdd, nrow(catchmentsRemoved))

    for (i in seq_len(nToAdd)) {
      selected <- selected + 1

      facilityRow <- catchmentsRemoved[i, ]
      facilityName <- sf::st_drop_geometry(
        facilityRow[, idFieldCatchment]
      )[1, 1]

      result[selected, "amFacilityName"] <- facilityName
      result[selected, "amPopCovered"] <- catchmentsRemoved$totalPop[i]

      if (adminCheck) {
        result[selected, "amAdminRegion"] <- sf::st_drop_geometry(
          facilityRow[, adminColName]
        )[1, 1]
        result[selected, "amAdminId"] <- sf::st_drop_geometry(
          facilityRow[, adminIdColName]
        )[1, 1]
      }
    }
  }

  #
  # Substitute grouped names where identical catchments were found.
  #
  for (i in seq_len(selected)) {
    facilityName <- result$amFacilityName[i]

    if (!is.na(facilityName) && facilityName %in% names(duplicateGroups)) {
      result$amFacilityName[i] <- duplicateGroups[[facilityName]]
    }
  }

  #
  # Finalise result table
  #
  resultCols <- colnames(result)
  result <- result[complete.cases(result), ]

  if (nrow(result) > 0) {
    result$amRank <- seq_len(nrow(result))
    result$amPopCoveredCumul <- cumsum(result$amPopCovered)
    result <- result[
      ,
      c("amRank", resultCols, "amPopCoveredCumul")
    ]
  }

  pbc(
    visible = TRUE,
    percent = 100,
    title = pBarTitle,
    text = ams("analysis_process_finished")
  )

  #
  # Write to database
  #
  dbCon <- amMapsetGetDbCon()
  on_exit_add({
    dbDisconnect(dbCon)
  })

  dbWriteTable(
    dbCon,
    outputBestCoverage,
    result,
    overwrite = TRUE
  )

  pbc(visible = FALSE)

  return(result)
}
