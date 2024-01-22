#' Create a file name for export.
#' @param dataName Name of the data, e.g. tExclusionOut__access_all
#' @param language Two letter language id, e.g. 'en'
#' @return name formated for export
#' @export
amGetNameConvertExport <- function(name, language = "en") {
  language <- amTranslateGetSavedLanguage()
  class <- config$dataClass[
    config$dataClass$class == amGetClass(name),
    language
  ]
  tags <- amGetTag(name, type = "file")
  type <- amGetType(name)
  amSubPunct(paste(type, class, paste(tags, collapse = "_")))
}


#' Export data to dir
#'
#' @return {Character} path to exported data directory
amExportData <- function(
  dataName = NULL,
  type = NULL,
  dataNameOut = NULL,
  dbCon = NULL,
  exportDir = tempdir(),
  formatVectorOut = "shp",
  formatRasterOut = "hfa",
  formatTableOut = "csv",
  formatListOut = "json",
  language = "en",
  stopOnError = TRUE
) {
  exportDirData <- NULL

  #
  # Stop on error catch
  #
  tryCatch(
    {
      if (isEmpty(dataName)) {
        stop("dataName is mendatory")
      }

      if (isEmpty(type)) {
        type <- amGetType(dataName)
        if (isEmpty(type)) {
          stop("type not defined")
        }
      }

      if (isEmpty(dataNameOut)) {
        dataNameOut <- amGetNameConvertExport(
          name = dataName,
          language = language
        )
      }

      # Create output directory, where to store data
      exportDirData <- file.path(exportDir, dataNameOut)

      if (dir.exists(exportDirData)) {
        removeDirectory(exportDirData, recursive = TRUE)
      }

      dir.create(exportDirData, showWarnings = FALSE)


      # default export function for grass.
      # If other formats are requested, add other preformated command here.
      switch(type,
        "config" = {
          pattern <- sprintf("%s.json", dataName)
          lList <- amGetConfigList(pattern = pattern)
          if (isEmpty(lList)) {
            msg <- sprintf(
              "Export of %s failed: config file not found.",
              dataName
            )
            stop(msg)
          }
          if (length(lList) > 1) {
            msg <- sprintf(
              "Oups, multiple occurences found for pattern '%s'",
              pattern
            )
            stop(msg)
          }
          fileName <- sprintf("%s.%s", dataNameOut, formatListOut)
          fileOut <- file.path(exportDirData, fileName)
          file.copy(lList[[dataName]], fileOut)
        },
        "shape" = {
          allShpFiles <- amGetShapesList(pattern = sprintf("^%s", dataName))


          if (isEmpty(allShpFiles)) {
            msg <- sprintf(
              "Export of %s failed: vector files not found.",
              dataName
            )
            stop(msg)
          }


          for (shpP in allShpFiles) {
            sExt <- file_ext(shpP)
            fileName <- sprintf("%s.%s", dataNameOut, sExt)
            fileOut <- file.path(exportDirData, fileName)
            file.copy(shpP, fileOut)
          }
        },
        "vector" = {
          infoName <- sprintf("%s_info.txt", dataNameOut)
          infoPath <- file.path(exportDirData, infoName)

          if (!amVectExists(dataName)) {
            stop(sprintf("Layer %s not found, stop export", dataName))
          }

          vInfo <- execGRASS("v.info",
            map = dataName,
            intern = TRUE
          )

          write(vInfo, infoPath)
          switch(formatVectorOut,
            "sqlite" = {
              fileName <- sprintf("%s.sqlite", dataNameOut)
              filePath <- file.path(exportDirData, fileName)
              if (file.exists(filePath)) {
                unlink(filePath)
              }
              # Export referral path  requires "m" multi feature
              execGRASS("v.out.ogr",
                input  = dataName,
                output = filePath,
                flags  = c("overwrite", "m"),
                format = "SQLite",
                dsco   = "SPATIALITE=yes"
              )
            },
            "kml" = {
              fileName <- sprintf("%s.kml", dataNameOut)
              filePath <- file.path(exportDirData, fileName)
              if (file.exists(filePath)) {
                unlink(filePath)
              }
              execGRASS("v.out.ogr",
                input  = dataName,
                output = filePath,
                flags  = c("overwrite", "m"),
                format = "KML"
              )
            },
            "gpkg" = {
              fileName <- sprintf("%s.gpkg", dataNameOut)
              filePath <- file.path(exportDirData, fileName)
              if (file.exists(filePath)) {
                unlink(filePath)
              }
              execGRASS("v.out.ogr",
                input        = dataName,
                output       = filePath,
                output_layer = fileName,
                flags        = c("overwrite", "m"),
                format       = "GPKG"
              )
            },
            "shp" = {
              fileName <- dataNameOut # grass will export to a directory.
              filePath <- file.path(exportDirData, fileName)
              if (filePath %in% list.dirs(exportDirData)) {
                unlink(filePath, recursive = TRUE)
              }
              # Export referral path  requires "m" multi feature
              execGRASS("v.out.ogr",
                input        = dataName,
                output       = exportDirData,
                output_layer = dataNameOut,
                flags        = c("overwrite", "m"),
                format       = "ESRI_Shapefile"
              )
            }
          )
        },
        "raster" = {
          # grass data related report
          reportName <- sprintf("%s_report.txt", dataNameOut)
          reportPath <- file.path(exportDirData, reportName)

          # overwrite existing,
          # f force event if data loss (float -> byte = loss),
          # c do not add color table,
          # m do not add non-standard metadata
          rasterFlags <- c("overwrite", "f", "c", "m")

          #
          # Case expoting the dem
          #
          mapsets <- c(amGrassSessionGetMapset(), "PERMANENT")
          if (!amRastExists(dataName, mapset = mapsets)) {
            stop(sprintf("Layer %s not found, stop export", dataName))
          }


          if (file.exists(reportPath)) {
            unlink(reportPath)
          }

          execGRASS("r.report",
            map = dataName,
            units = c("k", "p"),
            output = reportPath,
            flags = "overwrite"
          )
          switch(formatRasterOut,
            "tiff" = {
              # tiff with UInt16 data (integer in 0-65535)
              # this could lead to lost of information.
              fileName <- sprintf("%s.GeoTIFF", dataNameOut)
              filePath <- file.path(exportDirData, fileName)
              execGRASS("r.out.gdal",
                flags = rasterFlags,
                input = dataName,
                output = filePath,
                format = "GTiff",
                createopt = "TFW=YES"
              )
            },
            "hfa" = {
              # hfa
              fileName <- sprintf("%s.img", dataNameOut)
              filePath <- file.path(exportDirData, fileName)
              execGRASS("r.out.gdal",
                flags = rasterFlags,
                input = dataName,
                output = filePath,
                format = "HFA",
                createopt = "COMPRESSED=YES"
              )
            }
          )
        },
        "table" = {
          dbCon <- amMapsetGetDbCon()
          on.exit({
            dbDisconnect(dbCon)
          })
          fileName <- sprintf("%s.xlsx", dataNameOut)
          filePath <- file.path(exportDirData, fileName)
          q <- sprintf("SELECT * FROM %s;", dataName)
          tbl <- dbGetQuery(dbCon, q)

          if (isEmpty(tbl)) {
            stop(sprintf("Table %s not found, stop export", dataName))
          }

          rio::export(tbl, filePath)
        }
      )
    },
    error = function(cond) {
      exportDirData <<- NULL
      if (stopOnError) {
        stop(cond)
      } else {
        warning(cond$message)
      }
    }
  )
  return(exportDirData)
}
