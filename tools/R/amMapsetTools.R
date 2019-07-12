

#' Get current mapset
#'
amMapsetGet <- function(){
  system("echo $MAPSET",intern=T)
}

#' Get current project
#' @note duplicate of execGRASS('g.gisenv',get='LOCATION_NAME'), but it returns errors
amProjectGet <-function(){
  system("echo $LOCATION_NAME",intern=T)
}
#' Get all existing mapset
#
amMapsetGetAll <- function(){
  allMapset = execGRASS('g.mapset',flags='l',intern=T)
  strsplit(allMapset,' ')[[1]]
}

#' Switch to another mapset
#'
#' @param {Character} Name of another mapset
#' @param {Logical} useMapDb Use individual map db instead of classic /location/mapset/sqlite.db databse
#'
amMapsetSet <- function(mapset,useMapDb=FALSE){
  allMapsets <- amMapsetGetAll()
  currMapset <- amMapsetGet()

  if(is.null(mapset) || mapset == currMapset) return()

  if( !mapset %in% allMapsets ){
    execGRASS('g.mapset',flags='c',mapset=mapset)
  }else{
    execGRASS('g.mapset',mapset=mapset,flags="quiet")
  }

  if(isTRUE(useMapDb)){
    #dbPath <- paste0("'$GISDBASE/$LOCATION_NAME/",mapset,"/vector/$MAP/sqlite.db'")
    dbPath <- paste0("'$GISDBASE/$LOCATION_NAME/",mapset,"/sqlite.db'")
  }else{
    dbPath <- "'$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'"
  }
  execGRASS('db.connect',driver='sqlite', database= dbPath)
  execGRASS('g.region', raster=config$mapDem) 
}

#' Eval an expression in another mapset
#' 
#' @param {Character} mapset Target mapset
#' @param {Expression} expression Expression to evaluate
#' @param {Character} origMapset Original mapset to switch back after
amMapsetDo <-function(mapset,expr,origMapset=NULL){
  allMapsets <- amMapsetGetAll()
  hasOrigMapset <- !is.null(origMapset)

  if(!hasOrigMapset || !origMapset %in% allMapsets){
    origMapset <- amMapsetGet()
  }
  out <- list()
  tryCatch(
    finally={
      amMapsetSet(origMapset,FALSE)
    },
    {
    amMapsetSet(mapset,useMapDb=TRUE)
    out <- eval(expr)
    })

  return(out)
}

#' Eval an expression in another mapset
#' 
#' @param {Character} mapset Mapset to remove
#' @param {Character} stringCheck Security Regex test before removing
#'
amMapsetRemove <-function(mapset,stringCheck="^tmp_"){
  if(!grepl(stringCheck,mapset)){
    warning(paste("amMapsetRemove can't remove mapset ", mapset, ". String check did not match."))
  }else{
    path <- system(paste0("echo $GISDBASE/$LOCATION_NAME/",mapset),intern=T)
    unset.GIS_LOCK()
    unlink_.gislock()
    remove_GISRC()
    if(dir.exists(path)){
      unlink(path,recursive=T)
    }
  }
}

#' get request from mapset map db
#' 
#' @param {Character} mapset Mapset to remove
#' @param {Character} layer Table name
#' @param {Character} query
#'
amMapsetDbGetQuery <- function(mapset,layer,query=NULL){

  out <- data.frame()
  if(is.null(query)) query = paste0("SELECT * FROM ",layer)

  #dPath <- system(paste0("echo $GISDBASE/$LOCATION_NAME/",mapset,"/",layer,"/sqlite.db"),intern=T)
  dPath <- system(paste0("echo $GISDBASE/$LOCATION_NAME/",mapset,"/sqlite.db"),intern=T)
  if(file.exists(dPath)){

    dbCon <- dbConnect(RSQLite::SQLite(),dPath)

    out <- dbGetQuery(dbCon,query)
  }
  return(out)
}

#' Create a new session in a ramdom mapset, based on a base project
#'
#' Usefull for doing parallel computing. Maybe.
#'
#' @param project {Character} project name
amMapsetInit <- function(project,mapset){
 
  if(is.null(mapset)){
    mapset <- amRandomName("tmp_mapset")
  }
  gHome <- file.path(tempdir(),project,mapset)

  dir.create(gHome,showWarnings=F,recursive=T)

  unset.GIS_LOCK()
  unlink_.gislock()
 
  initGRASS(
    gisBase  = config$pathGrassBase70,
    gisDbase = config$pathGrassDataBase,
    home     = gHome,
    location = project,
    mapset   = mapset,
    override = TRUE
    )
  Sys.setenv(GRASS_SKIP_MAPSET_OWNER_CHECK=TRUE)
  dbPath <- "'$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'"
  #amTimeStamp(paste(" GRASS SESSION ",system("echo $GISDBASE/$LOCATION_NAME/$MAPSET",intern=T)))
  execGRASS('db.connect',driver='sqlite', database= dbPath)

  execGRASS('g.region', raster=config$mapDem) 
  return(mapset)
}



