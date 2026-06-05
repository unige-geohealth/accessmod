#
# Upload vectors
#
#


# Detect Integer64 columns via ogrinfo JSON output and cast them to integer
# (int32) in the terra SpatVector before GRASS import. Both sf and terra
# silently convert OGR Integer64 to R numeric, losing the type; ogrinfo is the
# only reliable source of the original OGR type. GRASS v.in.ogr would otherwise
# store those columns as DOUBLE PRECISION, making them invisible to INTEGER
# selectors. Emits a warning if any coercion occurs.
amCoerceIntegers <- function(vect, dataInput) {

  fields <- amOgrFields(dataInput)

  if (is.null(fields)) {
    return(vect)
  }

  field_types <- sapply(fields, `[[`, "type")
  int64_names <- sapply(fields[field_types == "Integer64"], `[[`, "name")
  int64_names <- intersect(int64_names, names(vect))

  if (length(int64_names) > 0) {
    vals <- terra::values(vect)

    for (col in int64_names) {
      vals[[col]] <- as.integer(vals[[col]])
    }

    terra::values(vect) <- vals

    warning(
      paste0("vector_integer64_coerced:", paste(int64_names, collapse = ", "))
    )
  }

  vect
}
amVectorGeomType <- function(vect) {
  type <- terra::geomtype(vect)
  type <- tolower(type)

  switch(type,
    "points" = "point",
    "lines" = "line",
    "polygons" = "polygon",
    type
  )
}

amVectorClassGeometry <- function(dataName) {
  dataClass <- amGetClass(dataName)
  geometry <- amClassListInfo(dataClass, "geometry")
  if (isEmpty(geometry)) {
    return(character(0))
  }
  trimws(unlist(strsplit(geometry, ",")))
}

amValidateVectorGeometry <- function(vect, dataName) {
  expected <- amVectorClassGeometry(dataName)
  if (isEmpty(expected)) {
    return(invisible(TRUE))
  }

  actual <- amVectorGeomType(vect)
  if (!actual %in% expected) {
    stop(sprintf(
      "Invalid vector geometry for %s: expected %s, got %s",
      amGetClass(dataName),
      paste(expected, collapse = ", "),
      actual
    ))
  }

  invisible(TRUE)
}

amImportVectorToGrass <- function(vect, dataName) {
  geom_type <- amVectorGeomType(vect)

  if (identical(geom_type, "polygon")) {
    tmp_gpkg <- tempfile(fileext = ".gpkg")
    on_exit_add({
      if (file.exists(tmp_gpkg)) {
        unlink(tmp_gpkg)
      }
    })

    terra::writeVector(
      vect,
      tmp_gpkg,
      filetype = "GPKG",
      overwrite = TRUE
    )

    execGRASS("v.in.ogr",
      input  = tmp_gpkg,
      output = dataName,
      flags  = c("overwrite")
    )
  } else {
    write_VECT(
      vect,
      dataName,
      flags = c("overwrite")
    )
  }
}

amFeatureCollectionInputPath <- function(dataInput, dataFiles) {
  dataFiles <- dataFiles[file.exists(dataFiles)]
  ext <- tolower(file_ext(dataFiles))
  mainFiles <- dataFiles[ext %in% c("shp", "gpkg", "sqlite", "spatialite")]

  if (length(mainFiles) != 1) {
    stop("Imported feature collection must contain exactly one vector dataset")
  }

  mainFiles[[1]]
}

amUploadFeatureCollection <- function(dataInput, dataName, dataFiles, pBarTitle) {
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

  amValidateFileExt(dataFiles, "vect")
  dataInput <- amFeatureCollectionInputPath(dataInput, dataFiles)

  loc_meta <- amMapMeta()
  loc_proj <- loc_meta$orig$proj
  loc_bbox <- loc_meta$bbxSp$orig
  vect_upload <- vect(dataInput)
  vect_upload <- amCoerceIntegers(vect_upload, dataInput)
  amValidateVectorGeometry(vect_upload, dataName)

  vect_proj <- crs(vect_upload)
  vect_bbox <- as.polygons(ext(vect_upload), crs = vect_proj)
  proj_match <- st_crs(vect_bbox) == st_crs(loc_proj)
  if (!proj_match) {
    vect_upload <- project(vect_upload, loc_proj)
  }
  extent_match <- amExtentsMatch(loc_bbox, vect_upload)
  if (!extent_match) {
    stop("Imported feature collection extent is not within location extent")
  }

  out_dir <- system(sprintf("echo %s", config$pathShapes), intern = TRUE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(out_dir, paste0(dataName, ".gpkg"))
  if (file.exists(out_path)) {
    file.remove(out_path)
  }

  terra::writeVector(
    vect_upload,
    out_path,
    filetype = "GPKG",
    overwrite = TRUE
  )

  return(NULL)
}

amUploadShape <- amUploadFeatureCollection

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
  vect_upload <- amCoerceIntegers(vect_upload, dataInput)
  amValidateVectorGeometry(vect_upload, dataName)

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
  amImportVectorToGrass(vect_upload, dataName)

  # Post-import topology check: warn if polygon layer has mixed area + line
  # primitives — these layers import without error but fail at export with
  # "Mixing IDs of areas and primitives" (GRASS v.out.ogr).
  topo <- amGetTableFeaturesCount(dataName, types = c("areas", "lines"))
  nAreas <- topo$count[topo$type == "areas"]
  nLines <- topo$count[topo$type == "lines"]
  if (isTRUE(nAreas > 0) && isTRUE(nLines > 0)) {
    warning("topology_mixed_area_primitive")
  }

  return(NULL)
}
