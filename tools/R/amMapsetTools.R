#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.


#' Reset AccessMod region
#' @param {Character} rasters Rasters to set the region
#' @param {Character} vectors vectors to set the region
amRegionSet <- function(rasters=character(0),vectors=character(0)){

  hasRasters <- !amNoDataCheck(rasters)
  hasVectors <- !amNoDataCheck(vectors)

  if(!hasRasters && !hasVectors){
   return
  }


  rasterAlign <- ifelse(hasRasters,rasters[1],character(0))
  
  execGRASS('g.region',flags='d')

  execGRASS('g.region',
    raster = rasters,
    vector = vectors,
    align = config$mapDem
    #zoom = config$mapDem
    )
}

#' Reset AccessMod region
#' 
amRegionReset <- function(){
  amRegionSet(config$mapDem)
}




#' Get current mapset
#' @param {Logical} sys Use system
#'
amMapsetGet <- function(sys=TRUE){
  if(!sys){
    return(execGRASS('g.mapset',flags='p',intern=T))
  }else{
    return(system("echo $MAPSET",intern=T))
  }
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
#'
amMapsetSet <- function(mapset){
  allMapsets <- amMapsetGetAll()
  currMapset <- amMapsetGet()

  if(is.null(mapset) || mapset == currMapset) return()

  if( !mapset %in% allMapsets ){
    execGRASS('g.mapset',flags='c',mapset=mapset)
  }else{
    execGRASS('g.mapset',mapset=mapset,flags="quiet")
  }
  
  dbPath <- paste0("'$GISDBASE/$LOCATION_NAME/",mapset,"/sqlite.db'")
  execGRASS('db.connect',driver='sqlite', database= dbPath)
  amRegionReset()
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
      amMapsetSet(origMapset)
    },
    {
    amMapsetSet(mapset)
    out <- eval(expr)
    })

  return(out)
}

#' Remove a mapset by name
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

#' Remove multiple mapset by name
#' 
#' @param {Character} pattern 
#'
amMapsetRemoveAll <- function(pattern="^tmp_"){
  tmpMapset <- amMapsetGetAll()
  for(m in tmpMapset){
    if(grepl(pattern,m)){
      amMapsetRemove(m, pattern)
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

  dPath <- system(paste0("echo $GISDBASE/$LOCATION_NAME/",mapset,"/sqlite.db"),intern=T)
  if(file.exists(dPath)){

    dbCon <- dbConnect(RSQLite::SQLite(),dPath)
    on.exit({
      dbDisconnect(dbCon)
    })
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

  on.exit({
    amRegionReset()
  })

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

  return(mapset)
}



