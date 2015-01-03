#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# additional custom reusable helper functions


# Forms check for mapset or location, msg if missing
renderUiLocMapsetCheck<-function(input,msg='',ui){
  renderUI({
    if(
      input$location=='select' || 
      is.null(input$location) || 
      input$mapset=='select' || 
      is.null(input$mapset)
      ){   
      tags$p(msg)
    }else{
      ui
    }
  }) 
}


# Forms check for location, msg if  missing
renderUiLocationCheck<-function(input,msg='',ui){
  renderUI({
    if(
      input$location=='select' || is.null(input$location) 
      ){   
      tags$p(msg)
    }else{
      ui
    }
  }) 
}


#redefine actionButton from shiny: add style
btn<-function (inputId, label, icon = NULL,sty=NULL, ...)
{
  tags$button(id = inputId, type = "button", class = "btn action-button", style=sty,
    list(icon, label))
}

# redefine inputText from shiny: add style
txt<-function (inputId, label, value = "",sty=NULL)
{
  tagList(label %AND% tags$label(label, `for` = inputId), tags$input(id = inputId,
      type = "text", value = value, style=sty))
}

## redefine shiny:::`%AND%` add nothing,but needed in upload
`%AND%` <-  function (x, y)
{
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

# redefine file input : add style. Doesn't work ?
upload<-function (inputId, label, multiple = FALSE, accept = NULL,sty=NULL)
{
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", style=sty)
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  tagList(label %AND% tags$label(label), inputTag, tags$div(id = paste(inputId,
        "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
      tags$div(class = "bar"), tags$label()))
}




# GRASS helper functions :
# list location from GrassDB
grassListLoc<-function(grassDataBase)
  list.dirs(grassDataBase,recursive=F, full.names=F)

# list mapset from GRASS DB
grassListMapset<-function(grassDataBase,location)
  list.dirs(file.path(grassDataBase,location),full.names=F,recursive=F)


# clean all space and punctuation, replace by selected char, default is underscore.
autoSubPunct<-function(vect,sep='_')gsub("[[:punct:]]+|[[:space:]]+",sep,vect)

getTagsBack<-function(mapList,uniqueTags=F,includeBase=F){
  # TODO : one expr for this. 
  # ^   match start of string
  # .*? search and stop for a condition
  # __  match cond

  tags<-gsub("_"," ",gsub("^.*?__","",mapList))

  if(includeBase){
    tags = c(tags,gsub("?__.+$",'',mapList))
  }

  if(length(tags)==0 || is.null(tags)){
    return(NULL)
  }else{
    if(uniqueTags)tags<-na.omit(unique(unlist(strsplit(tags,"\\s"))))
    return(tags)
  }
}





# function to create selectize compatible list of value
selectListMaker<-function(vect,default){ 
  vect<-c(default,vect)
  vect<-autoSubPunct(vect)
  #paste(vect,"=",vect)
}

# this function get the columns corresponding to type INTEGER or CHARACTER for a given
# grass db table.
grassDbColType<-function(grassTable,type='INTEGER'){
  if(!type %in% c('INTEGER','CHARACTER')) stop('type in grassDbColType should be INTEGER or CHARACTER')
  desc<-execGRASS('db.describe',table=grassTable,intern=T)
  grepSub<-grep("(column)|(type)",desc)
  desc<-as.data.frame(t(matrix(desc[grepSub],nrow=2)))
  names(desc)<-c('column','type')
  desc$column<-gsub('column:','',desc$column)
  desc$type<-gsub('type:','',desc$type)
  desc<-desc[desc$type %in% type,]$column
  desc
}

# messages Accessmod
msg<-function(accessModMsg='NULL',verbose=TRUE,logFile=logPath){
  output$messageAccessMod<-renderText(accessModMsg)
  # verbose only for the logs table ? 
  if(!is.null(accessModMsg) && !accessModMsg=='' && verbose == TRUE){
    accessModMsg<-gsub("[\r\n]","",accessModMsg)
    message(accessModMsg)
    write(paste(Sys.time(),'\t',accessModMsg,'\t',verbose,collapse=' '),file=logFile,append=TRUE)
  }
}

# read only a subset of last lines
readLogs<-function(logFile,nToKeep=300){
  library(R.utils)
  nMsg<-countLines(logFile)
  nToSkip<-nMsg-nToKeep
  read.csv(logFile,sep='\t', header=FALSE, skip=nToSkip)
}





# control if location is arleady took. Worth a new function ? only used in newLoc 
ifNewLocAvailable<-function(newLoc){
  if(newLoc %in% grassListLoc(grassDataBase) || autoSubPunct(newLoc) %in% grassListLoc(grassDataBase)){
    msg(paste('New location requested already in database:',newLoc),verbose=TRUE)
    return(FALSE)
  }else{
    msg(paste('New location available:',newLoc),verbose=FALSE)
    return(TRUE)
  }
}


# add dependencies to an existing shiny function
addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
    script = "jquery-ui.min.js",
    stylesheet = "jquery-ui.min.css")

  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}

# function to control input file extensions. 
# for each type and ext, write new rules here.
validateFileExt<-function(fileExtensions,mapType){
  mT<-mapType # vect or rast
  fE<-fileExtensions # list of file extension
  # vector files
  if(mT=='vect'){
    # rule 1 : if it's a shapefile, it must have dbf,prj,shx file extensions.
    if('shp' %in% fE){
      valid<-all(c('prj' %in% fE,'dbf' %in% fE, 'shx' %in% fE))
      if(!valid) stop(
        'Accessmod vector validation:
        Trying to import invalid shapefile.
        Minimum required file extensions are : .shp, .prj .dbf and .shx'
        )
    }
    # rule 2 : if it's a shapefile, none of the extensions must be present more than once
    if('shp' %in% fE){
      valid<-all(!duplicated(fE))
      if(!valid) stop(
        'Accessmod vector validation:
        Duplicated file extensions detected. Please add only one map at a time. 
        '
        )
    }
  }
  # raster files
  if(mT=='rast'){
    NULL

  }

}

# function to remove raster based on pattern
rmRastIfExists<-function(pattern=''){
  rastList <- execGRASS('g.mlist',type='rast',pattern=pattern,intern=TRUE)
  if(length(rastList)>0){
    execGRASS('g.mremove',flags=c('b','f'),type='rast',pattern=pattern)
  }
}

rmVectIfExists<-function(pattern=''){
  vectList <- execGRASS('g.mlist',type='vect',pattern=pattern,intern=TRUE)
  if(length(vectList)>0){
    execGRASS('g.mremove',flags=c('b','f'),type='vect',pattern=pattern)
  }
}

# creation of a file to import color rules in GRASS. Assume a numeric null value.
# Geotiff only allow export color table for byte and UNint16 data type. So,
# the maximum value (null..) will be 65535. Both data type don't allow negetive numbers. 
createColorTable<-function(maxVals,nullVals=65535,paletteFun,filePath){
  valQuant<-c(quantile(0:maxVals),nullVals)
  colorMap<-t(col2rgb(paletteFun(6)))
  colGrass<-character()
  for(i in 1:nrow(colorMap)){
    rN<-valQuant[i]
    vN<-paste(colorMap[i,],collapse=':')
    tN<-paste(rN,vN,'\n',collapse=' ')
    colGrass<-c(colGrass,tN)
  }

  write(colGrass,file = filePath)
}





appUpdate<-function(){
  system('git pull')
}

appVersion<-function(){
  system('git rev-list HEAD --count',intern=T)
}




listToHtml<-function(listInput,htL='',h=2){
  hS<-paste0('<H',h,'><u>',collapse='')
  hE<-paste0('</u></H',h,'>',collapse='')
  h=h+1
  if(is.list(listInput)){
    nL<-names(listInput)
    htL<-append(htL,'<ul>')
    for(n in nL){
      #htL<-append(htL,c('<li>',n,'</li>'))
      htL<-append(htL,c(hS,n,hE))
      subL<-listInput[[n]]
      htL<-listToHtml(subL,htL=htL,h=h)
    }
    htL<-append(htL,'</ul>')
  }else{
    htL<-append(htL,c('<li>',paste(listInput,collapse=','),'</li>'))
  }
  return(paste(htL,collapse=''))
}


exportGrass<-function(map,exportDir,type,vectFormat='shp',rastFormat='tiff'){
  require(spgrass6)
  # default export function for grass.
  # be careful with this function : it uses unlink recursivly on provided filepath !
  # If other formats are requested, add other preformated command here.
  tryCatch({
    if(type=='vect'){
      switch(vectFormat,
        sqlite={
          fileName<-paste0(map,'.sqlite')
          filePath<-file.path(exportDir,fileName)
          if(file.exists(filePath))unlink(filePath)
          execGRASS('v.out.ogr',
            input=map,
            dsn=filePath,
            flags=c('overwrite'),
            format="SQLite",
            dsco='SPATIALITE=yes')
        },
        kml={
          fileName<-paste0(map,'.kml')
          filePath<-file.path(exportDir,fileName)
          if(file.exists(filePath))unlink(filePath)
          execGRASS('v.out.ogr',
            input=map,
            dsn=filePath,
            flags=c('overwrite'),
            format="KML") 
        },
        shp={
          fileName<-map  # grass will export to a directory.
          filePath<-file.path(exportDir,fileName)
          if(filePath %in% list.dirs(exportDir))unlink(filePath,recursive=TRUE)
          execGRASS('v.out.ogr',
            input=map,
            dsn=filePath,
            flags=c('overwrite'),
            format="ESRI_Shapefile")
        }
        )
    }else{
      switch(rastFormat,
        tiff={
          fileName<-paste0(map,'.GeoTIFF')
          filePath<-file.path(exportDir,fileName)
          execGRASS('r.out.gdal',
            flags =c('c','t','overwrite','f'),
            input=map,
            output=filePath,
            format="GTiff",
            nodata=-9999,
            type='Float32')
        
        } # note : with force flags, Integer could lead to data loss !
        ) 
    }
  },
  error=function(c)message(c)
  ) 
  return(fileName)
}

