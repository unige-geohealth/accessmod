#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## General parameters and configuration list 

# load main packages.


#
# CONFIGURATION LIST 
#

config <- list()

#
# set sqlite location with
#
# find . -name "dbln" -type f -print0 | xargs -0 sed -i 's/\/srv\/shiny-server/\/home\/administrator\/Documents\/accessmod/g'
#
# general configuration
#

#
# Log mode %in% c("debug", "perf")
#
#config$logMode = c("debug","perf");
#config$logMode = c("debug");
config$logMode = c();

#git remote
config$repository="https://github.com/fxi/AccessMod_shiny"


# grass binaries and libs
config$os<-Sys.info()[['sysname']]
config$hostname <- Sys.info()[['nodename']]
config$hostnameVm <- "accessmod"
config$isVmVersion <- identical(config$hostnameVm,config$hostname)
config$isDevVersion <- isTRUE(getwd() == "/srv/shiny-server/accessmod_dev")
config$isProdVersion <- !config$isDevVersion

# shiny options 
config$maxUploadSize = 2000
options(
  shiny.maxRequestSize = config$maxUploadSize*1024^2 
  )
# default raster DEM name 
config$mapDem<-"rDem__dem@PERMANENT"
# progress bar default id
config$pBarId = "pbar"
# default time out
config$pBarTimeOut = 0
# default vector key
config$vectorKey = "cat"
# scaling up range of suitability
config$scalingUpRescaleRange = c(0L,10000L)
# character separator
config$sepTagFile='_'
config$sepClass='__'
config$sepTagRepl=' '
config$sepMapset='@' 

# max row table preview 
config$maxRowPreview <- 50
# allowed mode of transportation. As required by r.walk.accessmod.
# KEYWORD=list(raster value=<key value to distinguish mode from speed>)
config$listTranspMod<-list(
  WALKING=list(rastVal=1000),
  BICYCLING=list(rastVal=2000),
  MOTORIZED=list(rastVal=3000)
  )

config$defaultTranspMode = "WALKING"
#
# Default paths
#

# GRASS GIS paths, depends on the system configuration. Default are :
switch(config$os,
  'Darwin'={
    config$pathGrassBase70="/usr/local/Cellar/grass7/7.2.0/grass-7.2.0"
  },
  "Linux"={
    config$pathGrassBase70="/usr/local/grass-7.2.0"
  } 
)


# name from the web serverver 
config$archiveBaseName<-'accessmodArchive'


#
# Web prefix
#
config$prefixCache <- 'cache'
config$prefixDict <- 'dict'

#
# base directory.
#
config$pathModule<-normalizePath('modules/')
config$pathModuleManager <- file.path(config$pathModule,"amManageModules","amServer.R")
config$pathGrassHome<-normalizePath('../logs/')
config$pathGrassDataBase<-normalizePath('../data/grass/')
config$pathCacheDir<-normalizePath('../data/cache/')
#
# dictionary and language parameters
#
config$pathDictDir <- normalizePath('www/dict')
config$pathDictMain <- file.path(config$pathDictDir,'main.json')
config$pathClasses <- file.path(config$pathDictDir,'classes.json')
config$pathLanguageFile <- normalizePath('.language',mustWork=F) 

#
# create directories if necessary.
#
dir.create(showWarnings=F,recursive=T,config$pathGrassDataBase)
dir.create(showWarnings=F,config$pathGrassHome)
dir.create(showWarnings=F,config$pathCacheDir)

#
# Add ressource path
#
shiny::addResourcePath(config$prefixCache, config$pathCacheDir)
shiny::addResourcePath(config$prefixDict, config$pathDictDir)

#
# default language
# NOTE: see in tools/R/amTranslate.R : a function already exists for doing this.
#
config$language <- "en"
if(file.exists(config$pathLanguageFile)){
  language <- readLines(config$pathLanguageFile)
  if( length(language) == 1 && nchar(language) == 2 ){
    config$language <- language
  }
}
config$languageDefault <- "en"
#config$dictLanguages <- list("English"="en","Français"="fr","Español"="es","Deutch"="de")
config$dictLanguages <- list("English"="en","Français"="fr")
#
# NOTE: to update the dictionnary after adding language use :
#  amTranslateDictUpdateLanguages()
#
config$dict <- jsonlite::fromJSON(config$pathDictMain)
config$dataClass <- jsonlite::fromJSON(config$pathClasses)

#
# path to set after grass session started ( need grass env. variables )
# to retrieve correct path, use system(paste("echo",sqliteDB),intern=TRUE)
#

# sqlite database
config$pathSqliteDB<-'$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'
# path to archives
config$pathArchiveGrass<-'$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodArchives'
# path to shapefile
config$pathShapes <- '$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodShapes'
# path to lists
config$pathLists <- '$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodLists'
# rc file
grassRcFile<-file.path(config$pathGrassHome,'.grassrc6')
# store archive in mapset. Path generated inside a GRASS environment only.
# get archive path  ex. system(paste("echo",archives),intern=TRUE)

# log file. Create it does not exist 
config$pathLog<-normalizePath(file.path(config$pathGrassHome,'logs.txt'))
if(!file.exists(config$pathLog)) write("",config$pathLog)

config$pathPerf<-normalizePath(file.path(config$pathCacheDir,'perf.csv'))
if(!file.exists(config$pathPerf)) write("",config$pathPerf)



#global variables
config$amLocation<-""
config$amTitle<-'Accessmod 5.0' # todo : include GIT version. NOTE: check "modules/amManageSettings/amServer.R"


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
    c(cond="simpleWarning",
      desc="grass send a simpleWarning, nothing important",
      type="discarded",
      text=""
      ),
    c(
      cond="pBarQuit",
      desc="The user pressed the exit button in the progressbar panel",
      type="warning",
      text="The user requested the end of the process."
      ),
    c(
      cond="Projection of dataset does not appear to match current",
      desc="Projection does not match",
      type="error",
      text="The projection of this dataset does not appear to match current project"),
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

# file extension allowed See also validateFilExt in fun/helper.R
config$fileArchiveProjectDb <- c('am5pdb');
config$fileArchiveAnalysisConfig <- c('am5ac');
config$fileAdf<-c('dblbnd.adf','hdr.adf','prj.adf','vat.adf','w001001.adf','w001001x.adf')
config$fileAdfMin<-c('prj.adf','w001001.adf','hdr.adf')
config$fileShpExt<-c('.shp','.dbf','.prj','.sbn','.sbx','.xml','.shx','.cpg')
config$fileShpExtMin<-c('.shp','.prj','.dbf','.shx')
config$fileImgMin<-c('.img')
config$filesAccept<-list(
  "vector"=c('.sqlite','.spatialite',config$fileShpExt),
  "raster"=c('.adf','.geotiff','.GeoTIFF','.tiff','.img'),
  "table"=c('.xls','.csv','.xlsx','.ods','.tsv','.dta','.psv','.dbf','.rds','.RData','.json','.xml')
  )
config$fileAcceptMultiple<-list(
  "vector" = TRUE,
  "raster" = TRUE,
  "table" = FALSE 
  )

#
# Set gdal type for raster
# https://grass.osgeo.org/grass77/manuals/r.out.gdal.html
#
config$rasterDataTypes <- c(
  "Byte",			
  "UInt16",		
  "Int16",	
  "UInt32",		
  "Int32",	
  "Float32",	
  "Float64"
  )

names(config$rasterDataTypes) <- c(
  "Byte (0-255)",	
  "UInt16 (integer 0 to 65'535)",		
  "Int16 (integer -32'768 to 32'767 )",
  "UInt32 (integer 0 to 4'294'967'295)",	
  "Int32 (integer 0 to 4'294'967'295)",
  "Float32 (float -3.4E38 to 3.4E38)",
  "Float64 (float -1.79E308 to 1.79E308)"
  )

config$rasterDataTypesDefault <- 'Float32';


# control table col names and type for tables
config$tableColNames<-list(
  'tScenario'=c('class','label','speed','mode'),
  'tLandCover'=c('class','label'),
  'tStackRoad'=c('class','label'),
  'tStack'=c('class','label'),
  'tCapacity'=c('min','max','label','capacity'),
  'tExclusion'=c('layer','buffer','method'),
  'tSuitability'=c('factor','layer','weight','options')
  )

config$tableColType<-list(
  'tScenario'=c('integer','character','integer','character'),
  'tLandCover'=c('integer','character'),
  'tStackRoad'=c('integer','character'),
  'tCapacity'=c('numeric','numeric','character','numeric'),
  'tExclusion'=c('character','numeric','character'),
  'tSuitability'=c('character','character','numeric','character')
  )





# get a version grouped by class with class id as key
config$dataClassList <- dlply(config$dataClass,.(class),c)


config$defaultNoData <- "no_data"
config$defaultWithoutData <- "no_data"
config$defaultNoDataCheck <- c(config$defaultNoData,config$defaultWithoutData)
config$dynamicFacilities <- "vOutputFacility"
config$dynamicPopulation <- "rOutputPopulation"
config$dynamicLayers <- c(config$dynamicFacilities,config$dynamicPopulation)

#
# icons
#
config$iconSmall<-img(src="logo/icons/logo24x24.png")
config$iconMedium<-img(src="logo/icons/logo32x32.png")
config$iconLarge<-img(src="logo/icons/logo128x128.png")
config$iconHuge<-img(src="logo/icons/logo648x648.png")
config$iconWhoSvg<-img(src="logo/who.svg",style="width:100%; max-height:40px;")
config$iconWho<-img(src="logo/icons/WHO-EN-C-H.png")
config$iconWhoSmall<-img(src="logo/icons/WHO-EN-C-H_small.png",width='95%')
config$helpTitle = tags$span(icon("info-circle"),"AccessMod 5")

# order config list
config<-config[sort(names(config))]



