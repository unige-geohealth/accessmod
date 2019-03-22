
amGetFacilitiesTable <-function(mapHf,mapMerged,mapPop,tblSpeed,dbCon){
  # mapHf : vector map of facilities
  # map merged : raster landcover merged map
  # mapPop : raster map of population
  # Return value :
  # Facilitie attribute table with additional columns :
  # amOnBarrier : check if facilities is located on barrier (no landcover value)
  # amOnZero : check if facilities is located on landcover cell with speed of zero
  # amCatLandCover : get value of merged land cover for each facilities.
  # amPopCell : count population in cells where facilities are located.
  if( !amRastExists(mapMerged) || !amVectExists(mapHf)) return(NULL)

  tblAttribute <- dbGetQuery(dbCon,paste('select * from',mapHf))

  if(nrow(tblAttribute) == 0) return(NULL)
  #
  # check if HF are located on barrier by querying merged land cover values.
  #

  tbl <- amGetFacilitiesTableWhatRast(mapHf,mapMerged)
  names(tbl) <- c('cat','amCatLandCover')
  tbl$amOnBarrier <- is.na( tbl$amCatLandCover )

  if(!amNoDataCheck(tblSpeed)){
    classWithZero <- tblSpeed[tblSpeed$speed == 0,]$class
    tbl$amOnZero <-  tbl$amCatLandCover %in% classWithZero
  }else{
    tbl$amOnZero <- 'unset'
  }
  #
  # count population on facilities sites
  #
  if(!is.null(mapPop)){
    pop <- amGetFacilitiesTableWhatRast(mapHf,mapPop)
    names(pop) <- c('cat','amPopCell')
    pop[is.na(pop$amPopCell),'amPopCell'] <- 0
    #
    # merge results
    #
    tbl<-merge(tbl,pop,by='cat')
  }
  # 
  # copy hf attribute table from SQLite db.
  #
  tblAttribute <- dbGetQuery(dbCon,paste('select * from',mapHf))

  #
  # merge accessmod table with attribute table
  #
  tbl <- merge(tbl,tblAttribute,by='cat')

  return(tbl)

}

#amGetRasterValueByLatLong <- function(mapRaster,lat,lng,projDb,projMap){

  #spPoint <- SpatialPoints(data.frame(lat=lat,lng=lng))


        #projOrig = listen$mapMeta$orig$proj,

  #execGRASS()


#}

#'
amGetFacilitiesTableWhatRast <- function(mapHf, mapRaster){
  tbl <- read.table(
    text = execGRASS("v.what.rast"
      , map = mapHf
      , raster = mapRaster
      , flags = 'p'
      , intern = T
      )
    , sep = "|"
    , stringsAsFactors = FALSE
    , na.strings = "*"
    , colClasses = c("integer","numeric")
    )

  names(tbl) <- c('cat','val')
  return(tbl)
}
