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
  # Remove files on exist
  #
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

  loc_meta <- pMetaBefore
  loc_bbox <- loc_meta$bbxSp$orig
  loc_proj <- loc_meta$orig$proj
  loc_resol <- c(loc_meta$grid$esres, loc_meta$grd$nsres)
  rast_upload <- rast(dataFiles)
  rast_proj <- crs(rast_upload)
  rast_bbox <- as.polygons(ext(rast_upload), crs = crs(rast_upload))
  rast_resol <- res(rast_upload)

  src_proj <- ifelse(isDem,
    rast_proj,
    loc_proj
  )

  is_metric <- linearUnits(rast_upload) > 0L

  if (!is_metric) {
    stop("Imported raster should use metric format")
  }

  if (!isDem) {
    same_proj <- st_crs(rast_proj) == st_crs(loc_proj)

    if (!same_proj) {
      stop("Imported raster projection does not match location projection")
    }

    extent_match <- amExtentsMatch(loc_bbox, rast_bbox)
    loc_bbox_sf <- st_as_sf(loc_bbox)
    rast_bbox_sf <- st_as_sf(rast_bbox)

    if (!extent_match) {
      stop("Imported raster extent is not within location extent")
    }
  }

  progressBarControl(
    visible = TRUE,
    percent = 40,
    title = pBarTitle,
    text = "Validation succeeded. Importation in database..."
  )

  if (isDem) {
    dataName <- strsplit(config$mapDem, "@")[[1]][[1]]
    amGrassSessionUpdate(mapset = "PERMANENT")
    on_exit_add({
      amGrassSessionUpdate(mapset = currentMapset)
    })
  }

  write_RAST(
    rast_upload,
    vname = dataName,
    flags = c("overwrite", "quiet"),
    overwrite = TRUE
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
  rast_nulls <- amGetRasterStat(dataName, metric = "null_cells")

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
        x = abs(rast_resol[1]),
        y = abs(rast_resol[2])
      ),
      projection = rast_proj,
      numberOfNulls = rast_nulls
    )
  )

  return(out)
}
