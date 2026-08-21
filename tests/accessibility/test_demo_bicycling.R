print("Testing Bicycling Accessibility Reference")

fixtureDir <- "tests/accessibility/data/bicycling_only"
configFile <- file.path(fixtureDir, "config.json")
referenceFile <- file.path(fixtureDir, "travel_time_reference.img")
updateReference <- identical(
  Sys.getenv("AM_TEST_UPDATE_REFERENCES"),
  "true"
)

conf <- amAnalysisReplayParseConf(configFile)
travelTime <- conf$args$outputTravelTime

amAnalysisReplayExec(conf)

comparison <- amGrassNS(
  location = conf$location,
  mapset = conf$mapset,
  {
    referenceMap <- amRandomName("tmp__bicycle_reference")
    differenceMap <- amRandomName("tmp__bicycle_diff")

    tryCatch(
      {
        if (isTRUE(updateReference)) {
          exportDir <- file.path(tempdir(), amRandomName("bicycle_reference"))
          dir.create(exportDir, recursive = TRUE, showWarnings = FALSE)
          exportedDir <- amExportData(
            dataName = travelTime,
            exportDir = exportDir,
            formatRasterOut = "hfa"
          )
          exportedReference <- list.files(
            exportedDir,
            pattern = "\\.img$",
            full.names = TRUE
          )

          if (length(exportedReference) != 1L || !file.copy(
            exportedReference,
            referenceFile,
            overwrite = TRUE
          )) {
            stop("Unable to update bicycling travel-time reference raster")
          }
        }

        execGRASS(
          "r.in.gdal",
          input = normalizePath(referenceFile, mustWork = TRUE),
          output = referenceMap,
          flags = c("o", "overwrite", "quiet")
        )

        generatedMeta <- amRasterMeta(travelTime)
        referenceMeta <- amRasterMeta(referenceMap)
        metadataFields <- c(
          "north", "south", "east", "west", "nsres", "ewres",
          "rows", "cols", "datatype"
        )

        generatedStats <- amGrassRasterStats(travelTime)
        referenceStats <- amGrassRasterStats(referenceMap)
        statisticFields <- c(
          "n", "null_cells", "min", "max", "mean", "sum"
        )

        differenceExpression <- sprintf(
          paste0(
            "%1$s = if(isnull(%2$s) && isnull(%3$s), null(), ",
            "if(isnull(%2$s) || isnull(%3$s) || %2$s != %3$s, 1, null()))"
          ),
          differenceMap,
          travelTime,
          referenceMap
        )
        execGRASS(
          "r.mapcalc",
          expression = differenceExpression,
          flags = "overwrite"
        )

        list(
          metadataEqual = isTRUE(all.equal(
            generatedMeta[metadataFields],
            referenceMeta[metadataFields],
            tolerance = 0,
            check.attributes = FALSE
          )),
          statisticsEqual = isTRUE(all.equal(
            generatedStats[statisticFields],
            referenceStats[statisticFields],
            tolerance = 0,
            check.attributes = FALSE
          )),
          metadataMessage = paste(
            capture.output(print(rbind(
              generated = unlist(generatedMeta[metadataFields]),
              reference = unlist(referenceMeta[metadataFields])
            ))),
            collapse = " "
          ),
          statisticsMessage = paste(
            capture.output(print(rbind(
              generated = unlist(generatedStats[statisticFields]),
              reference = unlist(referenceStats[statisticFields])
            ))),
            collapse = " "
          ),
          differenceCount = amGetRasterStat(differenceMap, "n")
        )
      },
      finally = {
        rmRastIfExists(c(referenceMap, differenceMap))
      }
    )
  }
)

amtest$check(
  "Bicycling reference: raster geometry matches",
  comparison$metadataEqual,
  if (isTRUE(comparison$metadataEqual)) "" else comparison$metadataMessage
)

amtest$check(
  "Bicycling reference: distribution statistics match",
  comparison$statisticsEqual,
  if (isTRUE(comparison$statisticsEqual)) "" else comparison$statisticsMessage
)

amtest$check(
  "Bicycling reference: every value and NULL cell matches",
  isTRUE(comparison$differenceCount == 0),
  if (isTRUE(comparison$differenceCount == 0)) {
    ""
  } else {
    sprintf("Different cells: %s", comparison$differenceCount)
  }
)
