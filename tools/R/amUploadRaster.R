#' Upload a raster file in AccessMod
#'
#' @param config {List} AccessMod Configuraition list by default
#' @param dataInput {Character} Main file to use
#' @param dataFiles {List} Others files, depending on the format
#' @param dataClass {String} Class of the data
#' @param pBarTitle {String} Progress bar title
#' @export
amUploadRaster <- function(
  config,
  dataInput,
  dataName,
  dataFiles,
  dataClass,
  pBarTitle
) {
  #
  # get map meta before importation
  #
  pMetaBefore <- amMapMeta()
  pBarTitle <- "Raster importation"

  progressBarControl(
    visible = TRUE,
    percent = 10,
    title = pBarTitle,
    text = "Validation..."
  )

  isDem <- isTRUE(dataClass == amGetClass(config$mapDem))
  isLdc <- isTRUE(
    dataClass == "rLandCoverMerged" || dataClass == "rLandCover"
  )
  currentMapset <- amGrassSessionGetMapset()

  #
  # raster validation.
  #
  amValidateFileExt(dataFiles, "rast")

  dMeta <- list()

  for (file in dataFiles) {
    tryCatch(
      {
        if (isEmpty(dMeta)) {
          dMeta <- gdalinfo(file, raw_output = FALSE)
          dMeta$proj <- as.character(gdalsrsinfo(file, as.CRS = TRUE))
        }
      },
      error = function(cond) {
        return(NULL)
      }
    )
  }

  if (isEmpty(dMeta)) {
    stop("Raster uploader: Missing raster meta information")
  }

  srsDest <- ifelse(isDem,
    dMeta$proj,
    amGetLocationProj()
  )

  on_exit_add({
    for (f in dataFiles) {
      if (file.exists(f)) {
        file.remove(f)
      }
    }
    for (f in dataInput) {
      if (file.exists(f)) {
        file.remove(f)
      }
    }
    amRegionReset()
  })

  progressBarControl(
    visible = TRUE,
    percent = 40,
    title = pBarTitle,
    text = "Validation succeeded. Importation in database..."
  )

  if (isEmpty(dMeta$driver)) {
    stop("Raster uploader: Missing driver info")
  }

  if (isDem) {
    dataName <- strsplit(config$mapDem, "@")[[1]][[1]]
    amGrassSessionUpdate(mapset = "PERMANENT")
    on_exit_add({
      amGrassSessionUpdate(mapset = currentMapset)
    })
  }

  execGRASS(
    "r.in.gdal",
    band = 1,
    input = dMeta$file,
    output = dataName,
    flags = c("overwrite", "quiet"),
    title = dataName
  )

  #
  # Reset project extent
  #
  if (isDem) {
    progressBarControl(
      visible = TRUE,
      percent = 80,
      title = pBarTitle,
      text = "Set project resolution and extent based on new DEM"
    )
    amRegionReset()
  }

  #
  # Convert land cover to integer
  #
  if (isLdc) {
    ldcMeta <- execGRASS("r.info",
      map = dataName,
      flags = c("g"),
      intern = TRUE
    )

    ldcMeta <- read.csv(
      text = ldcMeta,
      sep = "=",
      header = FALSE
    )

    isCell <- isTRUE(ldcMeta[ldcMeta$V1 == "datatype", 2] == "CELL")

    if (!isCell) {
      progressBarControl(
        visible = TRUE,
        percent = 80,
        title = pBarTitle,
        text = "LandCover value are not in integer, convert values"
      )

      exp <- sprintf(
        "%1$s = round(%1$s)",
        dataName
      )

      execGRASS(
        "r.mapcalc",
        expression = exp,
        flags = c("overwrite")
      )
    }
  }

  #
  # Set colors
  #
  colorsTable <- config$dataClass[
    config$dataClass$class == dataClass,
    "colors"
  ]

  if (!isEmpty(colorsTable)) {
    progressBarControl(
      visible = TRUE,
      percent = 85,
      title = pBarTitle,
      text = "Set color table"
    )

    colConf <- as.list(strsplit(colorsTable, "&")[[1]])
    if (length(colConf) == 2) {
      cN <- c("color", "flag")
    } else {
      cN <- c("color")
    }
    names(colConf) <- cN
  }
  if (!isEmpty(colorsTable)) {
    execGRASS(
      "r.colors",
      map = dataName,
      flags = colConf$flag,
      color = colConf$color
    )
  }

  #
  # Last progress bar info
  #
  progressBarControl(
    visible = TRUE,
    percent = 90,
    title = pBarTitle,
    text = "Importation succeeded... Cleaning..."
  )

  #
  # Set importation summary list
  #
  dMeta$nullCells <- amGetRasterStat(dataName, metric = "null_cells")

  pMetaAfter <- amMapMeta()

  #
  # meta data about uploaded data and project
  #
  out <- list(
    projectBefore = list(
      resolution = list(
        y = pMetaBefore$grid$nsres,
        x = pMetaBefore$grid$ewres
      ),
      projection = pMetaBefore$orig$proj
    ),
    projectAfter = list(
      resolution = list(
        y = pMetaAfter$grid$nsres,
        x = pMetaAfter$grid$ewres
      ),
      projection = pMetaAfter$orig$proj
    ),
    data = list(
      resolution = list(
        x = abs(dMeta$res.x),
        y = abs(dMeta$res.y)
      ),
      projection = dMeta$proj,
      numberOfNulls = dMeta$nullCells
    )
  )

  return(out)
}

