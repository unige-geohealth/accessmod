## global parameters



#global variables
location<-""
title<-'Acessmod 5.0' # todo : include GIT version.

#standard message.
msgNoLocation=list(en="Please set a location")
msgNoLocMapset=list(en="Please set a location and mapset.")

# ui dimension
dimsbw=4 # sidebarpanel width
dimmpw=8 # main panel width
stybtn="width:95%" # btn style
stytxt="width:90%" # btn style
dimselw="100%" # selectinput width


# initialisation check
initOK =F 

# verbose mode
verbMod<-TRUE

# toggle
showNewLoc=1


# grass global var
os<-system("uname",intern=TRUE)
if(os=="Darwin"){
  grassBase70="/usr/local/Cellar/grass-70/7.0.0beta3/grass-7.0.0beta3/"
  grassBase64="/usr/local/Cellar/grass-64/6.4.4_1/grass-6.4.4"
}else{
  # expect to be run on linux.. so default are :
  grassBase70="/usr/lib/grass70"
  grassBase64="/usr/lib/grass64"
}

grassDataBase<-normalizePath("../data/grass/")
grassMapset<-"PERMANENT"

# same level as data
logFile<-normalizePath('../logs/logs.txt')

# grass home for gisrc
grassHome<-normalizePath('../logs/')



acceptRaster<-c('.adf','.geotiff','.GeoTIFF','.tiff')
acceptVector<-c('.shp','.dbf','.sqlite','.spatialite','.prj','.sbn','.sbx','.xml','.shx')


# reactive Meta data.
mapMetaList<-reactiveValues()
mapMetaList<-reactiveValues(type=NA,class=NA,tags=NA)

# available class of map. 
# TODO: transfer this in global.R or in config file ?
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

# allowed mode of transportation
transpModList<-list(
  WALKING=list(rastVal='1000'),
  BICYCLING=list(rastVal='2000'),
  NONE=list(rastVal='3000')
  )



