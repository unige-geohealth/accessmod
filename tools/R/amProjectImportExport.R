
fileExtProject <- config$fileArchiveProjectDb
pathDB <- config$pathGrassDataBase
pathCache <- config$pathCacheDir

#
# Convert sqlite path to relative path
#
amUpdateSqliteDbPath = function(idProject){
  dbStrRel <- "$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db"
  dbStrAbs <- sprintf("$GISDBASE/%1$s/%1$s/sqlite.db",idProject)
  dbPath <- system(sprintf("echo %1$s", dbStrAbs), intern=T)
  dbDirPath <- dirname(dbPath)
  hasDb <- file.exists(dbPath)
  if(hasDb){
    dbLinks <- list.files(path=dbDirPath,pattern='dbln',recursive=T,full.names=T,all.files=T)
    for(fdb in dbLinks){
      tryCatch({
        dbTbl <- read.table(fdb,stringsAsFactors=F,sep="|")
        dbTbl$V4 <- dbStrRel
        strDb <- paste(dbTbl,collapse="|")
        write(strDb, file=fdb)
      },error=function(e){
        warning(e)
      })
    }
  }
}

amProjectExport = function(idProject){
  fileName <- sprintf('%1$s.%2$s',idProject,fileExtProject)
  #
  # e.g. "/srv/shiny-server/data/cache/test.am5p
  #
  pathExport <- file.path(pathCache,fileName)
  pathProject <- file.path(pathDB,idProject)
  curwd <- getwd()

  on.exit({
    setwd(curwd)
  })

  if(!dir.exists(pathProject)){
   stop('Project to export not found')
  }
  if(file.exists(pathExport)){
    unlink(pathExport)
  }
 
  #
  # Update db path with relative db path
  #
  amUpdateSqliteDbPath(idProject)

  #
  # If zip from app folder, paths are absolute. 
  #
  setwd(pathDB)
  zip(pathExport, idProject)
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

    #
    # Update db links with relative path
    #
    amUpdateSqliteDbPath(name)

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
    percent = 15
    )

  execGRASS('r.in.gdal',
    input = tmpMapPath,
    band = 1,
    output = config$mapDem,
    flags = c('overwrite','quiet'),
    title = paste(newProjectName,'DEM')
    )

  amRegionReset()

  onProgress(
    text = "Set colors and remove temp files",
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
    text = "Done",
    percent = 100
    ) 


}



