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
    exportDirectory = exportDir
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
      # default to shapefile
      shp <- res_file[grepl(".shp$", res_file)]
      readOGR(shp)
    }
  )
  return(data)
}
