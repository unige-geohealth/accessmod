#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## General parameters and configuration list 

# load main packages.

#CRAN

# shiny options 
options(
  shiny.maxRequestSize = 300*1024^2 
  #,shiny.trace=TRUE
  )

# output config list:
config<-list()
#used in update script.
config$repository="https://github.com/fxi/AccessMod_shiny"



# List of packages to load or install from CRAN
# NOTE: this could be handled manually now, as packrat take care of keeping 
# package in sync with main project...

#
#config$packagesCran= c(
#  "tools",
#  "R.utils",        # used in amReadLogs to read last subset lines
#  "shiny",          # latest CRAN version of shiny
#  "devtools",       # latest CRAN version of devtools NOTE: use github instead?
#  "rgrass7",        # R interface to GRASS GIS
#  "htmltools",      # html tools, companion of shiny.
#  "data.table",     # faster than data.frame for large data processing.
#  "raster",         # class and function for raster map
#  "rgdal",          # intern gdal command
#  "rgeos",          # map manipulation
#  "maps",           # download and display generic maps
#  "gdalUtils",      # launch system gdal command from R
#  "RSQLite",        # interface to SQLITE database
#  "plyr",           # data manipulation
#  "pingr",          # ping remote server. Used in update process
#  'V8'              #v8 javascript engine. only used by geojsonio. Find alternative?
#)
#
#config$packagesGithub<-c(
#  'Rcpp'="RcppCore/Rcpp",# Lastest version of Rcpp
#  'leaflet'="fxi/AccessMod_leaflet-shiny",
#  'shinydashboard'="rstudio/shinydashboard",# UI
#  'geojsonio'="ropensci/geojsonio",
#  'readxl'="hadley/readxl", # https://github.com/hadley/readxl read excel
#  'rio'="leeper/rio" #https://github.com/leeper/rio universal table import 
#  )
#

## List of packages to load or install from local git (e.g /src-pkg/<name>)
#config$packagesGithub<-c(
#  'Rcpp',           # c++ code in R. Used in rio/readxl
#  'leaflet',        # leaflet map in shiny (fxi fork)
#  'shinydashboard', # UI dashboard.
#  'geojsonio',      # read write geojson/topojson(interface)* need V8
#  'readxl',         # https://github.com/hadley/readxl read excel
#  'rio'             # https://github.com/leeper/rio universal table import
#)
#

#config$pathLocalPkg<- normalizePath("src_pkg")



#Paths
# base directory.
config$pathModule<-normalizePath('modules/')
config$pathGrassHome<-normalizePath('../logs/')
config$pathGrassDataBase<-normalizePath('../data/grass/')
config$pathCacheDir<-normalizePath('../data/cache')
# as we use packrat now, no need for additional libs
#config$pathLib<-normalizePath('../libs/')
# set local lib as first choice:
#.libPaths( c(config$pathLib, .libPaths())) 
# sqlite database
# get sqlite path after grass init : system(paste("echo",sqliteDB),intern=TRUE)
config$pathSqliteDB<-'$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'


# create directories if necessary.
dir.create(showWarnings=F,recursive=T,config$pathGrassDataBase)
dir.create(showWarnings=F,config$pathGrassHome)
#dir.create(showWarnings=F,config$pathLib)
dir.create(showWarnings=F,config$pathCacheDir)


# set other grass variables
# TODO: check if is use:
#grassMapset<-"PERMANENT"
grassRcFile<-file.path(config$pathGrassHome,'.grassrc6')
# unset gis_lock on startup
#unset.GIS_LOCK()

# standard dem name. Default project grid. 
config$mapDem<-"dem__dem@PERMANENT"

# grass binaries and libs
config$os<-Sys.info()['sysname']

switch(config$os,
  'Darwin'={
    config$pathGrassBase70="/usr/local/Cellar/grass-70/7.0.0/grass-7.0.0"
    config$pathGrassBase64="/usr/local/Cellar/grass-64/6.4.4_1/grass-6.4.4"

  },
  "Linux"={
    config$pathGrassBase70="/usr/local/grass-7.0.0"
    config$pathGrassBase64="/usr/lib/grass64"
  } 
  )


# store archive in mapset.
# get archive path AFTER grass init, with grass environment running : system(paste("echo",archives),intern=TRUE)
config$pathArchiveGrass<-'$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodArchives'
config$archiveBaseName<-'accessmodArchive'


# log file. Create it does not exist 
config$pathLog<-normalizePath(file.path(config$pathGrassHome,'logs.txt'))
if(!file.exists(config$pathLog)) write("",config$pathLog)


#global variables
config$amLocation<-""
config$amTitle<-'Accessmod 5.0' # todo : include GIT version.

#standard message 
# TODO: These message are depreciated. Check and update function where they are used.
# TODO: create real localisation ?  (*.po/*.mo) 
config$msgNoLocation=list(en="Please select or create a project")
config$msgNoLocMapset=list(en="Please select or create project.")
config$msgNoProj<-'No projection information found. Make sure your dataset contains such information : .prj file, adf.prj, worldFile, complete metadata or similar.'
config$msgNotMetric<-'No metric projection information found. Make sur your dataset is projected using a metric coordinate system.'


# Registery of encountered errors NOT GENERATED BY ACCESSMOD. 
# Add new hint and translation in human language to avoid users to panic.
# This list, with function errHandler(), should dispatch current error to warning, log, or error condition.
# only warning and error (as set here or in amMsg() function) will create an alert for the user.
# structure :
# a. cond : hint of the registered condition. As the condition could be concatenated: (e.g. "SimpleWarning: 23 feature missing in map coverage@demo500m" ), extract only the useful part: "feature missing in map".  
# b. desc : Description to understand the error from the developer point of view.
# c. type : how accessMod should interprets the condition : log or error ?
# c. text : replacement text for the user.

config$msgTableError<-as.data.frame(rbind(
    c(
      cond="Field <projection> missing",
      desc="Can be produced when grass didn't found location metadata after g.region -3 -c. Need for reloading them from DEM",
      type='error',
      text="AccessMod did not recognize meta data for the current project. Please reload them in settings module:button 'Reload spatial settings'"),
    c(
      cond="is a base map for",
      desc="small warning when removing tmp map used in mask.",
      type='log',
      text='base map removed'
      ),
    c(
      cond= "No areas selected from vector map",
      desc=" v.to.rast warning if convert area is selected, but not found in geometries.",
      type='log',
      text='Vector map conversion to raster without area'
      ),
    c(
      cond="Datum <unknown>",
      desc=" maybe a wrong text. TODO: control this one. The text could be the original message.",
      type='log',
      text='Datum <unknown> not recognised by GRASS'
      ),
    c(
      cond="file does not exists",
      desc="Error after upload wrong file (type doesn't match extension, no metadata) OR error during gdal operation OR shiny internal fileUpload i/o problem.",
      type="error",
      text="file not recognized, make sure you have uploaded a supported raster files, with all its dependencies."
      ),
    c(
      cond="already exists and",
      desc= "Warning/error that occurs  with flag 'overwrite' and map exists.", 
      type="log",
      text="Map has been overwritten"
      ),
    c(
      cond="had status 1",
      desc=" generic error : could be everything. For now, tell the users it could be due to a bad formated dataset.",
      type="Error",
      text="Process has been aborded. Check your data for anomalies, e.g.: extent, CRS, non-conform values."
      ),
    c(
      cond="not appear to match current location",
      desc=" if the map crs is clearly wrong",
      type="error",
      text="The map CRS did not match current location."
      ),
    c(
      cond="Cannot create a RasterLayer object from this file",
      desc=" Error during importation if the package raster cant read metadata.",
      type="error",
      text="File not recognized, make sure you have loaded a supported raster map format, with all dependencies."
      ),
    c(
      cond="were not modified because modification would damage",
      desc= "v.clean say that some features has not ben cleaned to preserve topology",
      type='log',
      text='v.clean topology warning : some features have not been cleaned'
      ),
    c(
      cond="are written only when -c flag is given",
      desc='Warning when exporting empty area (inner rings) to shapefile',
      type="log",
      text="v.out.ogr found empty area (inner rings) to export in shapefile."
      ),
    c(
      cond="WARNING: Number of duplicate centroids",
      desc='duplicate centroids',
      type="log",
      text="v.in.ogr found duplicate centroïd"
      ),
    c(
      cond="Column name <cat> renamed to <cat_>",
      desc='Grass use <cat> as index column name. Any atribute table with <cat> as column name will be renamed to <cat_>',
      type="log",
      text="Column name <cat> renamed to <cat_>"
      ),
    c(
      cond="Input data contains 3D features",
      desc="Vector map uploaded contain 3d features. As accessmod works in 3d, ignoring 3d with -2 flag",
      type="log",
      text="Accessmod has converted 3D features in 2D."
      )
    )
  )


# verbose mode. 
# TODO: check if this is used
config$verbMod<-TRUE

# file extension allowed See also validateFilExt in fun/helper.R
config$fileAdf<-c('dblbnd.adf','hdr.adf','prj.adf','vat.adf','w001001.adf','w001001x.adf')
config$fileAdfMin<-c('prj.adf','w001001.adf','hdr.adf')
config$fileShpExt<-c('.shp','.dbf','.prj','.sbn','.sbx','xml','.shx')
config$fileShpExtMin<-c('.shp','.prj','.dbf','.shx')
config$filesAccept<-list(
  "vector"=c('.sqlite','.spatialite',config$fileShpExt),
  "raster"=c('.adf','.geotiff','.GeoTIFF','.tiff'),
  "table"=c('.xls','.csv','.xlsx','.ods','.tsv','.dta','.psv','.dbf','.rds','.RData','.json','.xml')
  )
config$fileAcceptMultiple<-list(
  "vector" = TRUE,
  "raster" = TRUE,
  "table" = FALSE 
  )

config$tableColNames<-list(
  'table_model'=c('class','label','speed','mode'),
  'table_land_cover'=c('class','label'),
  'table_stack_road'=c('class','label')
  )

# table of data class.
# id : identifier. Do not modify.
# class : class name visible by the user, used to create data names. Could be changed.
# type : raster, vector, table, report
# color : default grass color table (for raster)
# allowNew : visible in new data import
# internal : hidden from in manage data
config$dataClass<-read.table(text=paste("
    id               , class                          , type   , colors       , allowNew , internal\n
    amDem            , dem                            , raster , elevation    , FALSE    , FALSE\n
    amLcv            , land_cover                     , raster , random       , TRUE     , FALSE\n
    amLcvM           , land_cover_merged              , raster , random       , TRUE     , FALSE\n
    amLcvMB          , land_cover_merged_bridge       , raster , random       , FALSE    , TRUE\n
    amPop            , population                     , raster , population&e , TRUE     , FALSE\n
    amPopRes         , population_residual            , raster , population&e , FALSE    , FALSE\n
    amPopBar         , population_on_barrier          , raster , population&e , FALSE    , FALSE\n
    amBar            , barrier                        , vector ,              , TRUE     , FALSE\n
    amRoad           , road                           , vector ,              , TRUE     , FALSE\n
    amHf             , health_facilities              , vector ,              , TRUE     , FALSE\n
    amHfCatch        , health_facilities_catchment    , vector ,              , FALSE    , FALSE\n
    amZone           , zone_admin                     , vector ,              , TRUE     , FALSE\n
    amSpeed          , speed                          , raster , bcyr&e       , FALSE    , TRUE\n
    amFric           , friction                       , raster , bcyr&e       , FALSE    , TRUE\n
    amCumCost        , cumulative_cost                , raster , slope        , FALSE    , FALSE\n
    amLcvTable       , table_land_cover               , table  ,              , TRUE     , FALSE\n
    amModTbl         , table_model                    , table  ,              , TRUE     , FALSE\n
    amRefTbl         , table_referral                 , table  ,              , FALSE    , FALSE\n
    amRefTblDist     , table_referral_nearest_by_dist , table  ,              , FALSE    , FALSE\n
    amRefTblTime     , table_referral_nearest_by_time , table  ,              , FALSE    , FALSE\n
    amCapTbl         , table_capacity                 , table  ,              , FALSE    , FALSE\n
    amZoneCovTbl     , table_zonal_coverage           , table  ,              , FALSE    , FALSE\n
    amStackRoad      , stack_road                     , raster , random       , FALSE    , TRUE\n
    amStacLcv        , stack_land_cover               , raster , random       , FALSE    , TRUE\n
    amStacBar        , stack_barrier                  , raster , random       , FALSE    , TRUE\n
    "),
    sep=',',
    header=TRUE,
    colClasses=c('character','character','character','character','logical','logical'),
    strip.white=TRUE
    )

  #' function to extract class by id
  #' @param id identifier
  #' @param ls list id and class
  #' @param dc dataClass table
  #' @export
  amClassInfo <- function(id=NULL,ls=FALSE,dc=config$dataClass){
    if(ls){ 
      dc[,c('id','class','type')]
    }else{
      dc[dc$id==id,c('id','class','type')][1,]
    }
  }

  amListData <- function(id=NULL,dl=dataList){
    d=amClassInfo(id=id)
    grep(paste0('^',d[,'class'],'__'),dl[[d[,'type']]],value=T)
  }


  # character separator
  config$sepTagUi='+' #NOTE: depreciated. Using sepTagFile or tags in bracket.
  config$sepTagFile='_'
  config$sepClass='__'
  config$sepTagRepl=' '
  config$sepMapset='@' 

  # max row table preview 
  #NOTE: used only in road table, to prevent thousand combination of cat/label in table.
  config$maxRowPreview<-50

  # allowed mode of transportation. As required by r.walk.accessmod.
  # KEYWORD=list(raster value=<key value to distinguish mode from speed>)
  config$listTranspMod<-list(
    WALKING=list(rastVal=1000),
    BICYCLING=list(rastVal=2000),
    MOTORIZED=list(rastVal=3000)
    )


  # color palettes #NOTE: depreciated. Use config$dataClass['colors'] instead.
  config$paletteBlue<-colorRampPalette(c("#FFFFFF","#8C8CB2","#004664","#000632","#000000"))


  # icons
  # icon/favicon, defined in ui.R
  config$iconSmall<-img(src="logo/icons/logo24x24.png")
  config$iconMedium<-img(src="logo/icons/logo32x32.png")
  config$iconLarge<-img(src="logo/icons/logo128x128.png")
  config$iconHuge<-img(src="logo/icons/logo648x648.png")

  # order config list
  config<-config[sort(names(config))]

