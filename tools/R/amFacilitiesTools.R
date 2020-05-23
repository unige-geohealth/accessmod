
amGetFacilitiesTable <-function(mapHf,mapMerged,mapPop,mapDem,tblSpeed,dbCon){
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
    tblPop <- amGetFacilitiesTableWhatRast(mapHf,mapPop)
    names(tblPop) <- c('cat','amPopCell')
    tblPop[is.na(tblPop$amPopCell),'amPopCell'] <- 0
    #
    # merge results
    #
    tbl <- merge(tbl,tblPop,by='cat')
  }

  #
  # Check DEM values
  #
  if(!is.null(mapDem)){
    tblDem <- amGetFacilitiesTableWhatRast(mapHf,mapDem)
    names(tblDem) <- c('cat','amDemValue')
    tblDem$amOutsideDem <- is.na( tblDem$amDemValue )
    #
    # merge results
    #
    tbl<-merge(tbl,tblDem,by='cat')
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


amGetRasterValueAtPoint <- function(inputPoint, inputRaster){

  data = execGRASS("v.what.rast"
    , map = inputPoint
    , raster = inputRaster
    , flags = 'p'
    , intern = T
  )

  if(amNoDataCheck(data)){
    tbl <- data.frame(V1=character(0),v2=character(0))
  }else{

    tbl <- read.table(
      text = data,
      , sep = "|"
      , stringsAsFactors = FALSE
      , na.strings = "*"
      , colClasses = c("integer","numeric")
    )
  }

  names(tbl) <- c('cat','val')
  return(tbl)

}


amGetFacilitiesTableWhatRast <- function(mapHf, mapRaster){

  on.exit({
    amRegionReset()
  })

  amRegionSet(mapRaster,mapHf)

  tbl <- amGetRasterValueAtPoint(mapHf, mapRaster)

  return(tbl)

}
