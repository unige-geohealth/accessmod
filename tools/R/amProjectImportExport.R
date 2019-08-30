
fileExtProject <- config$fileArchiveProjectDb
pathDB <- config$pathGrassDataBase
pathCache <- config$pathCacheDir

amProjectExport = function(idProject){
  fileName <- sprintf('%1$s.%2$s',idProject,fileExtProject)
  pathExport <- file.path(pathCache,fileName)
  pathProject <- file.path(pathDB,idProject)

  if(!dir.exists(pathProject)){
   stop('Project to export not found')
  }
  if(file.exists(pathExport)){
    unlink(pathExport)
  }
  #
  # If zip from app folder, paths are absolute. 
  #
  curwd <- getwd()
  setwd(pathDB)
  on.exit(setwd(curwd))
  zip(pathExport, idProject)
  setwd(curwd)
  return(pathExport)
}

amProjectImport <- function(fileProject,name){
  name <- amSubPunct(name,'_')
  projects <- amGetGrassListLoc(config$pathGrassDataBase)
  isNameValid <- !amNoDataCheck(name) && !isTRUE(name %in% projects)
  isExtValid <- identical(
    file_ext(fileProject$name),
    fileExtProject
    )
  fileType <- system(sprintf('file -b --mime-type %s',fileProject$datapath),intern=T)
  isTypeValid <- identical(fileType,'application/zip')

  if(isExtValid && isTypeValid && isNameValid){
    tmpDir <- file.path(tempdir(),amRandomName('import'))
    dir.create(tmpDir)
    on.exit({
      unlink(tmpDir)
    })
    unzip(fileProject$datapath, exdir = tmpDir)
    projOldName <- list.files(tmpDir)[[1]]
    #
    # Rename location to new name
    #
    file.rename(
      file.path(tmpDir,projOldName),
      file.path(tmpDir,name)
      )
    #
    # Rename default mapsed to new name
    #
    file.rename(
      file.path(tmpDir,name,projOldName),
      file.path(tmpDir,name,name)
      )
    
    #
    # Move into local db
    #
    file.copy(
      from = file.path(tmpDir,name),
      to = file.path(pathDB),
      overwrite = FALSE,
      recursive = TRUE
      )


  }else{
    stop('Invalid importation. Check name, extension and type')
  }
  return(NULL)
}


#' Create a new project from a dem raster
#' 
#' @param newDem {List} Upload list
#' @param newProjectName {Character} New project name
#' @param onProgress {Function} Callback to update a progress bar. Takes 3 arguments : text, percent, timeout
#'
amProjectCreateFromDem <- function(newDem,newProjectName,onProgress=function(text,percent,timout){}){ 
  #
  # Order files by size,
  #
  newDem <- newDem[with(newDem, order(-size)),]
  tmpDir <- dirname(newDem[1,'datapath'])
  newDem$newPath <- file.path(tmpDir,newDem$name)
  file.rename(newDem$datapath,newDem$newPath)
  #
  # Validate
  #
  amValidateFileExt(newDem$name,'rast')
  # 
  # take the first raster (heavier) as the base map
  #
  tmpMapPath <- newDem[1,'newPath']
  
  #
  # Test for projection issues 
  #
  r <- raster( tmpMapPath )
  destProj<-proj4string(r) 

  onProgress(
    text = "Test data projection",
    percent = 4
    )

  if(amNoDataCheck(destProj)){
    stop(msgNoProj)
  }

  if(!length(grep('+to_meter|+units=m',destProj))>0){
    stop(
      "No metric parameter found. Please make sure that your data is projected in metric format."
      )
  }
  
  onProgress(
    text="Conversion in SpatialGrid",
    percent = 6
    )

  # empty grid for the default WIND object
  sg <- as(r,'SpatialGrid')

  onProgress(
    text = "Init new project session",
    percent = 10
    )

  unset.GIS_LOCK()
  unlink_.gislock()
  gHome <- file.path(tempdir(),newProjectName)
  dir.create(gHome,showWarnings=F)
  
  initGRASS(
    gisBase         = config$pathGrassBase70, # binary files (grass 7.0)
    home            = gHome, # where store lock file
    gisDbase        = config$pathGrassDataBase, # local grass database
    location        = newProjectName, # rsession
    mapset          = 'PERMANENT', # PERMANENT for dem.
    SG              = sg, #spatial grid as templte for extent and res
    override        = TRUE
    )

  execGRASS('g.proj',flags='c',proj4=destProj)
  execGRASS('db.connect',driver='sqlite',database=config$pathSqliteDB)
  execGRASS('g.gisenv',flags='s')

  onProgress(
    text = "Importation in database",
    percent = 10
    )

  execGRASS('r.in.gdal',
    input = tmpMapPath,
    band = 1,
    output = config$mapDem,
    flags = c('overwrite','quiet'),
    title = paste(newProjectName,'DEM')
    )

  execGRASS("g.region",
    raster = config$mapDem
    )

  onProgress(
    text = "Importation done. Set colors and remove temp files ",
    percent = 80
    )

  execGRASS(
    'r.colors',
    map = config$mapDem,
    color = 'elevation'
    )

  unset.GIS_LOCK()
  unlink_.gislock()
  
  file.remove(tmpMapPath)

  onProgress(
    percent = 100
    )
}
