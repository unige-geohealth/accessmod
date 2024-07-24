# function to control input file extensions.
# for each type and ext, write new rules here.
# file extension is given by file_ext (package tools) or grep command.
amValidateFileExt <- function(mapNames, mapType) {
  # need access to am config
  stopifnot(exists("config"))
  # require validation vector in config files, e.g. shpExtMin
  mN <- basename(mapNames) # list of map names to be validated.
  mT <- mapType # vect or rast
  fE <- file_ext(mN) # list of file extension in map list
  # vector files
  if (mT == "vect") {
    # rule 1 : if it's a shapefile, it must have minimal set of  file extensions.
    if ("shp" %in% fE) {
      valid <- all(amSubPunct(config$fileShpExtMin, "") %in% fE)
      if (!valid) {
        stop(paste(
          "Accessmod shapefile validation error:
            Trying to import invalid shapefile dataset.
            Minimum required file extensions are :", paste(config$fileShpExtMin, collapse = ", ")
        ))
      }
    }
    # rule 2 : if it's a shapefile, none of the extensions must be present more than once
    if ("shp" %in% fE) {
      valid <- all(!duplicated(fE))
      if (!valid) {
        stop(
          "Accessmod shapefile validation error:
          Duplicated files type detected. Please add only one map at a time.
          "
        )
      }
    }
  }

  # raster files
  if (mT == "rast") {
    if ("adf" %in% fE) {
      valid <- all(config$fileAdfMin %in% mN)
      if (!valid) {
        stop(paste(
          "Accessmod esri binary grid validation:
            Trying to import invalid adf file dataset.
            Minimum required files are:", paste(config$fileAdfMin, collapse = ", ")
        ))
      }
    }
    if ("img" %in% fE) {
      fES <- amSubPunct(fE)
      fEMin <- amSubPunct(config$fileImgMin)
      valid <- all(fEMin %in% fES)
      if (!valid) {
        stop(
          sprintf("
            Accessmod ERDAS img file validation:
            Trying to import invalid file dataset.
            Min. required file extension are: %s", paste(config$fileImgMin))
        )
      }
    }
  }
}


