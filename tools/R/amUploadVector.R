#
# Upload vectors
#
#
amUploadVector <- function(dataInput, dataName, dataFiles, pBarTitle) {
  on_exit_add({
    for (f in dataFiles) {
      if (file.exists(f)) {
        file.remove(f)
      }
    }
  })
  progressBarControl(
    visible = TRUE,
    percent = 20,
    title = pBarTitle,
    text = "Attributes validation and cleaning"
  )
  # validate if multifiles
  amValidateFileExt(dataFiles, "vect")
  #
  # Comparison / extent validation
  #
  loc_meta <- amMapMeta()
  loc_proj <- loc_meta$orig$proj
  loc_bbox <- loc_meta$bbxSp$orig
  vect_upload <- vect(dataInput)
  vect_proj <- crs(vect_upload)
  vect_bbox <- as.polygons(ext(vect_upload), crs = crs(vect_upload))
  proj_match <- st_crs(vect_bbox) == st_crs(loc_proj)
  if (!proj_match) {
    vect_upload <- project(vect_upload, loc_proj)
  }
  extent_match <- amExtentsMatch(loc_bbox, vect_upload)
  if (!extent_match) {
    stop("Imported vector extent is not within location extent")
  }
  # Remove cat and cat_ columns if they exist
  vect_upload <- vect_upload[, !names(vect_upload) %in% c("cat", "cat_")]
  write_VECT(
    vect_upload,
    dataName,
    flags = c("overwrite")
  )
  return(NULL)
}
