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

#r.utils : create directories if necessary
mkdirs(grassHome)
mkdirs(grassDataBase)

# set other grass variables
grassMapset<-"PERMANENT"
grassRcFile<-file.path(grassHome,'.grassrc6')
# unset gis_lock on startup
unset.GIS_LOCK()

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


# log file. must create it does not exist ?
logPath<-file.path(grassHome,'logs.txt')
if(!file.exists(logPath)) write("",logPath)
logFile<-normalizePath(logPath)



#global variables
location<-""
title<-'Acessmod 5.0' # todo : include GIT version.

#standard message. 
#TODO: create real localisation ?  (*.po/*.mo) 
msgNoLocation=list(en="Please set a location")
msgNoLocMapset=list(en="Please set a location and mapset.")

# ui dimension
dimsbw=4 # sidebarpanel width
dimmpw=8 # main panel width
stybtn="width:95%" # btn style
stytxt="width:90%" # btn style
dimselw="100%" # selectinput width

# verbose mode
verbMod<-TRUE

# toggle
showNewLoc=1




# file size limitation
options(shiny.maxRequestSize = 300*1024^2)

# file extension allowed See also validateFilExt in fun/helper.R
acceptRaster<-c('.adf','.geotiff','.GeoTIFF','.tiff')
shpExt<-c('.shp','.dbf','.prj','.sbn','.sbx','xml','.shx')
acceptVector<-c('.sqlite','.spatialite',shpExt)


# reactive Meta data.
mapMetaList<-reactiveValues()
mapMetaList<-reactiveValues(type=NA,class=NA,tags=NA)

# available class of map. 
mapClassList<-list(
  land_cover=list(type='rast'),
  population=list(type='rast'),
  barrier=list(type='vect'),
  road=list(type='vect'),
  health_facilities=list(type='vect')
) 

# character separator
charTag='+'
charTagGrass='__'

# max row table preview
maxRowPreview<-15

# allowed mode of transportation. required as it by r.walk.accessmod.
transpModList<-list(
  WALKING=list(rastVal=1000),
  BICYCLING=list(rastVal=2000),
  NONE=list(rastVal=3000)
  )

analysisChoicesList<-list(
  anisotropic=list(fun='r.walk.accessmod'),
  isotropic=list(fun='r.walk.accessmod')
  )


# color palettes
paletteBlue<-colorRampPalette(c("#FFFFFF","#8C8CB2","#004664","#000632","#000000"))
