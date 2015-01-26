#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## global parameters

#Paths
# base directory.
grassHome<-'../logs/'
grassDataBase<-'../data/grass/'
rLocLibs<-'../libs/'
.libPaths( c( .libPaths(), rLocLibs) ) 

#r.utils : create directories if necessary
mkdirs(grassHome)
mkdirs(grassDataBase)
mkdirs(rlibs)

# set other grass variables
grassMapset<-"PERMANENT"
grassRcFile<-file.path(grassHome,'.grassrc6')
# unset gis_lock on startup
#unset.GIS_LOCK()

# grass binaries and libs
os<-system("uname",intern=TRUE)
if(os=="Darwin"){
  grassBase70="/usr/local/Cellar/grass-70/7.0.0beta3/grass-7.0.0beta3/"
  grassBase64="/usr/local/Cellar/grass-64/6.4.4_1/grass-6.4.4"
}else{
  # expect to be run on linux.. so default are :
  grassBase70="/usr/local/grass-7.0.0beta3"
  grassBase64="/usr/lib/grass64"
}

# sqlite database
# get sqlite path after grass init : system(paste("echo",sqliteDB),intern=TRUE)
sqliteDB<-'$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'

# store archive in mapset.
# get archive path after grass init : system(paste("echo",archives),intern=TRUE)
archiveGrass<-'$GISDBASE/$LOCATION_NAME/$MAPSET/accessmodArchives'
archiveBaseName<-'accessmodArchive'


# log file. must create it does not exist ?
logPath<-file.path(grassHome,'logs.txt')
if(!file.exists(logPath)) write("",logPath)
logPath<-normalizePath(logPath)



#global variables
location<-""
title<-'Accessmod 5.0' # todo : include GIT version.

#standard message. 
#TODO: create real localisation ?  (*.po/*.mo) 
msgNoLocation=list(en="Please select or create a project")
msgNoLocMapset=list(en="Please select or create project.")
msgNoProj<-'No projection information found. Make sure your dataset contains such information : .prj file, adf.prj, worldFile, complete metadata or similar.'
msgNotMetric<-'No metric projection information found. Make sur your dataset is projected using a metric coordinate system.'




# ui dimension. New method : use class and CSS file
#
#dimsbw=3 # sidebarpanel width
#dimmpw=9 # main panel width
#stybtn="width:95%" # btn style
#stytxt="width:90%" # btn style
#dimselw="100%" # selectinput width
#
# verbose mode
verbMod<-TRUE

# toggle
#showNewLoc=1

# file size limitation
options(shiny.maxRequestSize = 300*1024^2)

# file extension allowed See also validateFilExt in fun/helper.R
adfFiles<-c('dblbnd.adf','hdr.adf','prj.adf','vat.adf','w001001.adf','w001001x.adf')
adfFilesMin<-c('prj.adf','w001001.adf','hdr.adf')
shpExt<-c('.shp','.dbf','.prj','.sbn','.sbx','xml','.shx')
shpExtMin<-c('.shp','.prj','.dbf','.shx')
#acceptVector<-c('.sqlite','.spatialite',shpExt)
#acceptTable<-c('.xls','.csv')


acceptFiles<-list(
  "vect"=c('.sqlite','.spatialite',shpExt),
  "rast"=c('.adf','.geotiff','.GeoTIFF','.tiff'),
  "table"=c('.xls','.csv','.xlsx')
  )

acceptMultiple<-list(
  "vect" = TRUE,
  "rast" = TRUE,
  "table" = FALSE 
  )

acceptColNames<-list(
  'table_model'=c('class','label','speed','mode'),
  'table_land_cover'=c('class','label'),
  'table_stack_road'=c('class','label')
  )



# available class of map. 
#dataClassList<-list(
#  "dem"=list(type='rast', allowNewDataset=FALSE),
#  "stack_road"=list(type='rast',allowNewDataset=FALSE),
#  "stack_land_cover"=list(type='rast',allowNewDataset=FALSE),
#  "land_cover"=list(type='rast',allowNewDataset=TRUE),
#  "population"=list(type='rast',allowNewDataset=TRUE),
#  "barrier"=list(type='vect',allowNewDataset=TRUE),
#  "road"=list(type='vect',allowNewDataset=TRUE),
#  "health_facilities"=list(type='vect',allowNewDataset=TRUE),
#  "speed"=list(type='vect',allowNewDataset=FALSE),
#  "merged"=list(type='rast',allowNewDataset=FALSE),
#  "cumulative_cost"=list(type='rast',allowNewDataset=FALSE),
#  "table_land_cover"=list('table',allowNewDataset=TRUE),
#  "table_model"=list(type='table',allowNewDataset=TRUE)
#  ) 
#

# table of available class, and which are allowed as new dataset input.
# Weird method to input a new table, but.. This table could/will be stored in csv file
# or in a database.. 
dataClass<-read.table(text=paste("
id , class             , type  , allowNew\n
1  , dem               , rast  , FALSE\n
2  , land_cover        , rast  , TRUE\n
3  , population        , rast  , TRUE\n
4  , barrier           , vect  , TRUE\n
5  , road              , vect  , TRUE\n
6  , health_facilities , vect  , TRUE\n
7  , speed             , rast  , FALSE\n
8  , merged            , rast  , FALSE\n
9  , cumulative_cost   , rast  , FALSE\n
10 , table_land_cover  , table , TRUE\n
11 , table_model       , table , TRUE\n
12 , stack_road        , rast  , FALSE\n
13 , stack_land_cover   , rast  , FALSE\n
"),
sep=',',
header=TRUE,
colClasses=c('integer','character','character','logical'),
strip.white=TRUE
)

# character separator
#charTagUi='+'
#charTagFile='_'
#charTagSep='__'

sepTagUi='+'
sepTagFile='_'
sepTagPrefix='__'

# max row table preview
maxRowPreview<-50

# allowed mode of transportation. required as it by r.walk.accessmod.
transpModList<-list(
  WALKING=list(rastVal=1000),
  BICYCLING=list(rastVal=2000),
  NONE=list(rastVal=3000)
  )

#analysisChoicesList<-list(
#  anisotropic=list(fun='r.walk.accessmod'),
#  isotropic=list(fun='r.walk.accessmod')
#  )


# color palettes
paletteBlue<-colorRampPalette(c("#FFFFFF","#8C8CB2","#004664","#000632","#000000"))


# incons
# icon/favicon, defined in ui.R
iconSmall<-img(src="logo/icons/logo24x24.png")
iconMedium<-img(src="logo/icons/logo32x32.png")
iconLarge<-img(src="logo/icons/logo128x128.png")
iconHuge<-img(src="logo/icons/logo648x648.png")


