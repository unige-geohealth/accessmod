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
