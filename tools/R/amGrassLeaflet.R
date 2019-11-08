
amGrassLatLongPreview <- function(
  raster = NULL, # map name to preview. ex. land_cover
  bbxSpLatLongLeaf, # bbx sp object with current region in lat/long (bbxLeafToSp(input$<map>_bounds))
  bbxSpLatLongOrig, # bbx sp object with current region in projected format
  mapCacheDir, # relative path to cache directory eg. ../data/cache. Must exists
  width, # maximum resolution of final file.
  projOrig,
  projDest
  ){

  #
  # simple time diff
  #
  toc <- function(){
    if(exists('toc')){
      start <- tic
      time <- Sys.time()
      diff <- time - start
      diff
    }
  }
  tic <- Sys.time()

  if(!is.null(bbxSpLatLongLeaf) && !is.null(bbxSpLatLongOrig)){
    #
    # get intersection between leaflet extent and project extent
    #
    bbxSpLatLongInter <- gIntersection(
      bbxSpLatLongOrig,
      bbxSpLatLongLeaf
      )

    if( is.null(bbxSpLatLongInter) ) return(NULL)

    bbxMatLatLongInter <- bbxSpLatLongInter@bbox
    bbxMatLatLongInterRound <- round(bbxMatLatLongInter,3)

    #
    # Cache file names
    #
    cacheMap <- file.path(
      mapCacheDir,
      paste0(
        raster,
        "__",
        paste0(
          bbxMatLatLongInterRound,
          collapse = "_"
          ),'.png')
      )
    cacheLegend <- file.path(
      mapCacheDir , paste0("legend_",raster,'.png')
      )
    #
    # If the cache file does not exists, create it.
    #
    if(!file.exists(cacheMap)){

      tryCatch(
        finally = {
          amRegionReset()
          rmRastIfExists('MASK*')
          rmRastIfExists('tmp_*')
          rmVectIfExists('tmp_*')
        },
        {

          #
          # Get bbox in current projection
          #
          bbxSpProjInter <- spTransform(
            bbxSpLatLongInter,
            CRS(projOrig)
            )
          bbxMatProjInter <- bbxSpProjInter@bbox

          #
          # Set resolution and extent
          #
          res <- diff(bbxMatProjInter['x',]) / width 

          execGRASS('g.region',
            e=paste(bbxMatProjInter['x','max']),
            w=paste(bbxMatProjInter['x','min']),
            n=paste(bbxMatProjInter['y','max']),
            s=paste(bbxMatProjInter['y','min']),
            res=paste(ceiling(res)) 
            )

          x = doGRASS('g.region',
            e=paste(bbxMatProjInter['x','max']),
            w=paste(bbxMatProjInter['x','min']),
            n=paste(bbxMatProjInter['y','max']),
            s=paste(bbxMatProjInter['y','min']),
            res=paste(ceiling(res)) 
            )
          amDebugMsg(x);
          #
          # r.out.png : faster than r.out.gdal and more reliable
          # 
          execGRASS('r.out.png',
            input = raster,
            output = cacheMap,
            compression = 0,
            flags = c('overwrite','t')
            )
          amDebugMsg(
            'Retrieving done. in ',
            format(toc(),units='s'),
            'using a resolution of',
            round(res)
            )
        })
    }

    return(list(
        pngMap = cacheMap,
        pngLegend = cacheLegend,
        bbx = bbxMatLatLongInter
        ))
  }
}

amRastQueryByLatLong<-function(coord,rasterName,projOrig,projDest,nullValue='-'){
  coord<-SpatialPoints(data.frame(coord['x'],coord['y']))
  #proj4string(coord)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
  proj4string(coord)<-projDest
  #coord<-spTransform(coord,CRS(getLocationProj()))@bbox[,'max']
  coord<-spTransform(coord,CRS(projOrig))@bbox[,'max']
  suppressWarnings({
    val<-execGRASS('r.what'
      , map = rasterName
      , coordinates = c(coord[1],coord[2])
      , flags = c('c','quiet','f')
      , null_value = nullValue
      , intern = TRUE
      )
  })
  val <- read.table(
    text = val,
    sep = "|",
    stringsAsFactors = FALSE
    )
  names(val) <- c('long','lat','lab','value','cat label')
  val$value <- val$value
  val$lab <- NULL
  return(val)
}
# conversion of leaflet bounding box to sp object:
#  Leaflet has no bounding limit and sp does, crop leaflet box.
# to use this as standard bouding box, set CRS.
amBbxLeafToSp<-function(bbxLeaflet){
  if(!is.null(bbxLeaflet)){
    proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
    east<-pmax(pmin(bbxLeaflet$east,180),-180)
    west<-pmax(pmin(bbxLeaflet$west,180),-180)
    south<-pmax(pmin(bbxLeaflet$south,90),-90)
    north<-pmax(pmin(bbxLeaflet$north,90),-90)
    ext<-extent(c(east,west,south,north))
    ext<-as(ext,'SpatialPolygons')
    proj4string(ext)<-CRS(proj4dest)
    return(ext)
  }else{
    return(null)
  }
}


