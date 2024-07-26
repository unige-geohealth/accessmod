#
# Helper to exec + import table
#
replayExec <- function(conf) {
  #
  # Analysis files for the exports : table, vectors, etc..
  # -> amAnalysisReplayExport will create tmp directory
  # but not with using amAnalysisReplayExec
  # patching this here
  exportDir <- file.path(tempdir(), amRandomName())
  mkdirs(exportDir, mustWork = FALSE)
  dirs <- amAnalysisReplayExec(conf,
    exportDirectory = exportDir,
    formatVectorOut = "gpkg",
    formatRasterOut = "tiff"
  )
  return(dirs)
}


replayImport <- function(dirs, key) {
  res_file <- list.files(
    dirs[[key]],
    full.names = TRUE
  )
  data <- switch(amGetType(key),
    "table" = {
      import(res_file)
    },
    "vector" = {
      gpkg <- res_file[grepl(".gpkg$", res_file)]
      st_read(gpkg)
    }
  )
  return(data)
}
