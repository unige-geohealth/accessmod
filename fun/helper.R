#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# additional custom reusable helper functions


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


## redefine file input : add style. Doesn't work ?
# NOTE: see amfileinput instead
#upload<-function (inputId, label, multiple = FALSE, accept = NULL,sty=NULL)
#{
#  inputTag <- tags$input(id = inputId, name = inputId, type = "file", style=sty)
#  if (multiple)
#    inputTag$attribs$multiple <- "multiple"
#  if (length(accept) > 0)
#    inputTag$attribs$accept <- paste(accept, collapse = ",")
#  tagList(label %AND% tags$label(label), inputTag, tags$div(id = paste(inputId,
#        "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
#      tags$div(class = "bar"), tags$label()))
#}
#



# GRASS helper functions :
# list location from GrassDB
grassListLoc<-function(grassDataBase)
  list.dirs(grassDataBase,recursive=F, full.names=F)

# list mapset from GRASS DB
grassListMapset<-function(grassDataBase,location)
  list.dirs(file.path(grassDataBase,location),full.names=F,recursive=F)


# clean all space and punctuation, replace by selected char, default is underscore.
autoSubPunct<-function(vect,sep='_'){
  vect<-gsub("'",'',iconv(vect, to='ASCII//TRANSLIT'))
    gsub("[[:punct:]]+|[[:blank:]]+",sep,vect)
}

#getTagsBack<-function(mapList,uniqueTags=F,includeBase=F){
#  # TODO : one expr for this. 
#  # ^   match start of string
#  # .*? search and stop for a condition
#  # __  match cond
#
#  # removing prefix
#  tags<-gsub("_"," ",gsub("^.*?__","",mapList))
#
#  # but if requested, give prefix as tag
#  if(includeBase){
#    tags = c(tags,gsub("?__.+$",'',mapList))
#  }
#
#  if(length(tags)==0 || is.null(tags)){
#    return(NULL)
#  }else{
#    if(uniqueTags)tags<-na.omit(unique(unlist(strsplit(tags,"\\s"))))
#    return(tags)
#  }
#}
#
## extract tag and/or prefix from map names with prefix and
## tags separated by double underscore.
#getTagsBack2<-function(mapList,type=c('both','prefix','tags'),prefixSep="__",tagSep='_'){
#  # TODO : one expr for this. 
#  # ^   match start of string
#  # .*? search and stop for a condition
#  # __  match cond
#
#  exprTag<-paste0("^.+?",prefixSep)
#  exprPrefix<-paste0("?",prefixSep,'.+$')
#
#  type<-match.arg(type)
#  if(!is.null(mapList)&&length(mapList)>0){
#
#    # removing prefix
#    tags<-unique(unlist(strsplit(gsub(tagSep," ",gsub(exprTag,"",mapList)),"\\s")))
#    prefix<-gsub(exprPrefix,'',mapList)
#
#    switch(type,
#      'both'=out<-c(tags=tags,prefix=prefix),
#      'prefix'=out<-c(prefix=prefix),
#      'tags'=out<-c(tags=tags)
#      )
#    return(out)
#  }
#}
#

# extract tag and/or prefix from map names with prefix and
# tags separated by double underscore.
# return a named list :
# filterList$tagsTable with prefix (e.g. land_cover), tags (test), name (land_cover__test), name filter (landcover test)
amFilterDataTag<-function(namesToFilter,prefixSep="__",tagSep='_',tagSepRepl=' ',filterTag,filterText){

  if(!is.null(filterTag) && !filterTag==""){
    filterAll<-filterTag
  }else{
    filterAll=NULL #prefer null than ""
  }

  if(!is.null(filterText) && !filterText==""){
    filterAll<-c(autoSubPunct(filterText,'|'),filterAll)
  }

  exprTag<-paste0(".+?",prefixSep)
  exprPrefix<-paste0("?",prefixSep,'.+')

  #table with separated tags from prefix, and prefix without tags
  tagsTable<-data.frame(
    prefix=gsub(exprPrefix,'',namesToFilter),
    tags=gsub(tagSep,tagSepRepl,gsub(exprTag,"",namesToFilter,perl=T)),
    name=namesToFilter,
    stringsAsFactors=F
    )

  # add column with pasted prefix and tags. 
  # E.g. "land_cover reclass 2010"
  # instead of land_cover__reclass_2010
  tagsTable$nameFilter<-paste(tagsTable$prefix,tagsTable$tags)


  # filtering
  if(!is.null(filterAll)){ 
    exprFilter<-paste0('(?=.*\\b',filterAll,'\\b)',collapse='')
    #exprFilter<-paste0('(\\b',filterAll,'\\b)')
    tagsTable<-tagsTable[grep(exprFilter,tagsTable$nameFilter,perl=T),]
  }

 # # unique tags to populate selectize input.
 # tagsUnique<-c(
 #   unique(tagsTable$prefix), # e.g c(road, landcover, barrier)
 #   unique(unlist(strsplit(tagsTable$tags,tagSepRepl))) # e.g. c(secondary, cumulative)
 #   )

  tagsTable
 # list(
 #   tagsTable=tagsTable,
 #   tagsUnique=tagsUnique
 #   )
}





#
#  # TODO : one expr for this. 
#  # ^   match start of string
#  # .*? search and stop for a condition
#  # __  match cond
#
#  
#  type<-match.arg(type)
#  if(!is.null(mapList)&&length(mapList)>0){
#
#    # removing prefix
#    tags<-unique(unlist(strsplit(gsub(tagSep," ",gsub(exprTag,"",mapList)),"\\s")))
#    prefix<-gsub(exprPrefix,'',mapList)
#
#    switch(type,
#      'both'=out<-c(tags=tags,prefix=prefix),
#      'prefix'=out<-c(prefix=prefix),
#      'tags'=out<-c(tags=tags)
#      )
#    return(out)
#  }
#}
#
#

getUniqueTagString<-function(x,sepIn,sepOut,ordered=TRUE){
  #x =string containing tags : ex. test+super+super
  #sepIn separator in input string  e.g. +
  #sepOut separator in output string  e.g. _
  # return : unique ordered tag e.g. super_test
  if(length(x)==1){
    x<-autoSubPunct(x,sep=sepIn)
    x<-t(read.table(text=x,sep=sepIn))[,1]
    if(ordered==TRUE){
      x<-x[order(x)]
    }
    return(paste0(na.omit(unique(x)),collapse=sepOut))
  }else{
    stop('getUniqueTagString: length of input not 1 ')
  }
}



# function to create selectize compatible list of value
selectListMaker<-function(vect,default){ 
  vect<-c(default,vect)
  vect<-autoSubPunct(vect)
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
# trying to convert warning, error and message to logs.
#msg<-function(accessModMsg='NULL',verbose=TRUE,logFile=logPath){
#  output$messageAccessMod<-renderUI({
#    if(length(grep('[eE]rror',accessModMsg))>0){
#      tags$div(class = "alert alert-danger",accessModMsg) 
#    }else{
#      if(length(grep('[wW]arning',accessModMsg))>0){
#        tags$div(class = "alert alert-warning",accessModMsg) 
#      }else{
#        p('')
#      } 
#    } 
#  })
#  # verbose only for the logs table ? 
#  if(!is.null(accessModMsg) && !accessModMsg=='' && verbose == TRUE){
#    accessModMsg<-gsub("[\r\n]","",accessModMsg)
#    message(accessModMsg)
#    write(paste(Sys.time(),'\t',accessModMsg,'\t',verbose,collapse=' '),file=logFile,append=TRUE)
#  }
#}
msg<-function(accessModMsg='NULL',msgTitle=NULL,verbose=TRUE,logFile=logPath){
  output$messageAccessMod<-renderUI({
    if(length(grep('[eE]rror',accessModMsg))>0){
      createAlert(session, inputId = "alert_anchor",
        title=msgTitle,
        message = accessModMsg,
        type = "danger",
        dismiss = TRUE,
        block = FALSE,
        append = TRUE
        )
      #tags$div(class = "alert alert-danger",accessModMsg) 
    }else{
      if(length(grep('[wW]arning',accessModMsg))>0){
        createAlert(session, inputId = "alert_anchor",
          message = accessModMsg,
          type = "warning",
          dismiss = TRUE,
          block = FALSE,
          append = TRUE
          )

        # tags$div(class = "alert alert-warning",accessModMsg) 
      }else{
        p('')
      } 
    } 
  })
  # verbose only for the logs table ? 
  if(!is.null(accessModMsg) && !accessModMsg=='' && verbose == TRUE){
    accessModMsg<-gsub("[\r\n]","",accessModMsg)
    message(accessModMsg)
    write(paste(Sys.time(),'\t',accessModMsg,'\t',verbose,collapse=' '),file=logFile,append=TRUE)
  }
}

# read only a subset of last lines
readLogs<-function(logFile,nToKeep=300){
  tryCatch({
    library(R.utils)
    nMsg<-countLines(logFile)
    nToSkip<-nMsg-nToKeep
    read.csv(logFile,sep='\t', header=FALSE, skip=nToSkip) 
  },error=function(c)msg(c)
  )
}






# control if location is arleady took. Worth a new function ? only used in newLoc 
ifNewLocAvailable<-function(newLoc){
  if(newLoc %in% grassListLoc(grassDataBase) || autoSubPunct(newLoc) %in% grassListLoc(grassDataBase)){
    msg(paste('Warning: New project requested already in database:',newLoc),verbose=TRUE)
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
# file extension is given by file_ext (package tools) or grep command.
validateFileExt<-function(mapNames,mapType){
  # require validation vector in config files, e.g. shpExtMin
  mN<-basename(mapNames) # list of map names to be validated.
  mT<-mapType # vect or rast
  fE<-file_ext(mN) # list of file extension in map list
  # vector files
  if(mT=='vect'){
    # rule 1 : if it's a shapefile, it must have minimal set of  file extensions.
    if('shp' %in% fE){
      valid<-all(autoSubPunct(shpExtMin,'') %in% fE)
      if(!valid){
        stop(paste(
          'Accessmod shapefile validation error:
          Trying to import invalid shapefile dataset.
          Minimum required file extensions are :',paste(shpExtMin,collapse=', ' )
          )
        )}
    }
    # rule 2 : if it's a shapefile, none of the extensions must be present more than once
    if('shp' %in% fE){
      valid<-all(!duplicated(fE))
      if(!valid) stop(
        'Accessmod shapefile validation error:
        Duplicated files type detected. Please add only one map at a time. 
        '
        )
    }
  }

  # raster files
  if(mT=='rast'){
    if('adf' %in% fE){
      valid<-all(adfFiles%in% mN)
      if(!valid)stop(paste(
          "Accessmod esri binary grid validation:
          Trying to import invalid adf file dataset.
          Minimum required files are:",paste(adfFiles,collapse=', ')  
          ))
    }
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



getSqlitePath<-function(sqliteExpr){
  # example of sqliteExpr: '$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'
system(paste("echo",sqliteDB),intern=T)
}


appUpdate<-function(){
  system('git pull')
}

appVersion<-function(){
  system('git rev-list HEAD --count',intern=T)
}

packageManager<-function(pkgCran, pkgGit, libPath=NULL){
  tryCatch({
    # which package is missing ?
    pkgCranM <- pkgCran[!pkgCran %in% installed.packages()]
    pkgGitM <- pkgGit[!names(pkgGit) %in% installed.packages()]
    message('Missing packages:',pkgCranM,pkgGitM)
    pkgCranL <- length(pkgCranM)
    pkgGitL <- length(pkgGitM)
    if(pkgCranL>0){
      inc <- 1/pkgCranL
      msgUpdate<-'Updating CRAN packages'
      withProgress(message = msgUpdate, value = 0.1, {
        msg(msgUpdate)
        for(p in pkgCranM){ 
          install.packages(p,lib=libPath)
          incProgress(inc,detail=p)
        }
          })
    }
    if(pkgGitL>0){
      inc <- 1/pkgGitL
      msgUpdate<-'Updating GITHUB packages'
      withProgress(message = msgUpdate, value = 0.1, {
        msg(msgUpdate)
        for(p in pkgGitM){ 
          install_github(p,lib=libPath)
          incProgress(inc,detail=p)
        }
          })
    } 
    # load libraries 
    lapply(pkgCran, require, character.only=TRUE)
    lapply(names(pkgGit), require, character.only=TRUE)

  },error=function(c)msg(c))
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
  reportName<-paste0(map,'_report.txt')
  reportPath<-file.path(exportDir,reportName)
  infoName<-paste0(map,'_info.txt')
  infoPath<-file.path(exportDir,infoName)

  # default export function for grass.
  # be careful with this function : it uses unlink recursivly on provided filepath !
  # If other formats are requested, add other preformated command here.
  tryCatch({
    if(type=='vect'){
      vInfo<-execGRASS('v.info',map=map,intern=TRUE)
      write(vInfo,infoPath)
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
      return(c(fileName,infoName))
    }else{
      rInfo<-execGRASS('r.info',map=map,intern=TRUE)
      write(rInfo,infoPath)
      execGRASS('r.report',map=map,units=c('k','p'), output=reportPath, flags='overwrite')

      switch(rastFormat,
        tiff={
          # tiff with UInt16 data (integer in 0-65535)
          # this could lead to lost of information. 
          fileName<-paste0(map,'.GeoTIFF')
          reportPath<-paste0(map,'_report.txt')
          infoPath<-paste0(map,'_info.txt')
          filePath<-file.path(exportDir,fileName)
          execGRASS('r.out.gdal',
            flags =c('overwrite','f'),
            input=map,
            output=filePath,
            format="GTiff",
            nodata=65535,
            type='UInt16')

        } # note : with force flags, Integer could lead to data loss !
        ) 

      return(c(fileName,infoName,reportName))
    }
  },
  error=function(c)message(c)
  ) 
}


# TODO: find a way to update class 


updateStyle<-function(id,type='e',element='border'){
  # updateStyleBorder expects a reactiveStyle reactive object in parent env.
  # id = css id (string)
  # type = style id (string)
  # colors examples :
  #orange=rgb( 249, 235,200) 
  #red =rgb(205, 52,34)
  #green = rgb(49,172,0)
  #blue = rgb(18,112,200)
    if(element=='border'){
      sty<-switch(type,
        #error
        'e'="{
        width:100%;
        border-color: rgb(205,52,34);
        box-shadow: inset 0 1px 1px rgba(205,52,34, 0.075), 0 0 8px rgba(205,52,34, 0.6);}",
        # ok
        'o'="{
        width:100%;
        border-color: rgb(49,172,0);
        box-shadow: inset 0 1px 1px rgba(49,172,0,0.075), 0 0 8px rgba(49,172,0,0.6);}",
        # warning
        'w'="{
        width:100%;
        border-color: rgb(249, 235,200);
        box-shadow: inset 0 1px 1px rgba(249, 235,200,0.075), 0 0 8px rgba(249, 235,200,0.6);}",
        # info
        'i'="{
        width:100%;
        border-color: rgb(18,112,200 );
        box-shadow: inset 0 1px 1px rgba(18,112,200,0.075), 0 0 8px rgba(18,112,200,0.6);}"
        )
      sty<-paste0('#',id,' ',sty)
      listen$reactiveStyle[[id]]<-tags$style(type='text/css',sty)
      return(NULL)
}
}


# change class of object using jquerry

#toggleClass<-function(id,class){
#  scpt<-sprintf("$('%s').toggleClass('%s')",id,class)
#  #listen$toggleClassList[[id]]
#  output$js<-tags$script(HTML(scpt))
#  NULL
#}

# update hint text by class. Use directly jquery.. :/
# example:
# # UI
# tags$div(id='hintTest',p('test'))
# # SERVER
# hint('hintTest','This is a text message.')
hint<-function(hintId,text,iconFontAwesome='info-circle'){
txt<-p(icon(iconFontAwesome),text)
singleton(paste(''))
output$js<-tags$script(paste('$( "p" ).text( "<b>Some</b> new text." );'))

}


## custom function to upload files, based on fileinput
#amFileInput<-function (inputId, label, btnTxt='Browse',  multiple = FALSE, accept = NULL, style =NULL,disable=FALSE)
#{
#  style <- match.arg(style, c("", "primary", "info", "success", "warning", "danger", "inverse", "link"))
#
#  if(!disable){
#    inputTag <-
#      #tags$label(class= paste0("btn btn-", style, " browse-btn span4"),style=paste('width:',width,';'),
#      tags$label(class= paste0("btn btn-", style, " btn-browse"),
#        #tags$input(type = "file", style='width:0;height:0;opacity:0',id=inputId,name=inputId),
#        tags$input(type = "file",id=inputId,name=inputId),
#        btnTxt
#        )
#    if (multiple)
#      inputTag$children[[1]]$attribs$multiple <- "multiple"
#    if (length(accept) > 0)
#      inputTag$children[[1]]$attribs$accept <- paste(accept, collapse = ",")
#
#  }else{
#    inputTag <-
#      #tags$label(class= paste0("btn btn-", style, " browse-btn span4"),style=paste('width:',width,';'),
#      tags$label(class= paste0("btn btn-", style, " browse-btn"),btnTxt) 
#  }
#  #tagList(label %AND% tags$label(label), inputTag, tags$div(id = paste(inputId,
#  tagList(inputTag, tags$div(id = paste(inputId,
#        "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
#      tags$div(class = "bar"), tags$label()))
#}


# custom function to upload files, based on fileinput
##amFileInput<-function (inputId, label, btnTxt='Browse',  multiple = FALSE, accept = NULL, style =NULL)
#{
#  style <- match.arg(style, c("", "primary", "info", "success", "warning", "danger", "inverse", "link"))
#
#    inputTag <-
#      #tags$label(class= paste0("btn btn-", style, " browse-btn span4"),style=paste('width:',width,';'),
#      tags$label(class= paste0("btn fileInOut btn-", style, " browse-btn"),
#        tags$input(type = "file", style='display:none',id=inputId,name=inputId),
#        btnTxt
#        )
#    if (multiple)
#      inputTag$children[[1]]$attribs$multiple <- "multiple"
#    if (length(accept) > 0)
#      inputTag$children[[1]]$attribs$accept <- paste(accept, collapse = ",")
#
#   #tagList(label %AND% tags$label(label), inputTag, tags$div(id = paste(inputId,
#  tagList(inputTag, tags$div(id = paste(inputId,
#        "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
#      tags$div(class = "bar"), tags$label()))
#}

amFileInput<-function (inputId, label, style = NULL,disabled = FALSE, fileAccept=NULL, multiple=FALSE){
  style <- match.arg(style, c("", "primary", "info", "success", "warning", "danger", "inverse", "link"))
  
  inputTag<-tags$input(
          type=ifelse(disabled,'reset','file'),
          class='upload',
          accept=paste(fileAccept,collapse=','),
          id=inputId,
          name=inputId)

  if(multiple) inputTag$attribs$multiple='multiple'

 spanTag<-tags$span(label) 
  
inputClass<-div(class=c('btn-browse btn'),
   tList<- tagList(
     spanTag,
     inputTag
     )
   )
  if (disabled){
    inputClass$attribs$class <- paste(inputClass$attribs$class,"disabled")
  }
  if (!is.null(style)) {
    inputClass$attribs$class <- paste(inputClass$attribs$class,paste0("btn-", tolower(style)))
  }
#browser()
 tagList(inputClass, tags$div(id = paste(inputId,
        "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
      tags$div(class = "bar"), tags$label()))

  #return(shinyBS:::sbsHead(inputClass))
}


#amFileInput2<-function(inputId,label,btnTxt,multiple=FALSE, accept = NULL, style=NULL){
#
#  tagList(
#    div(class="fileInOut btn btn-success",
#      tagList(
#        
#      tags$span(btnTxt),
#      tags$input(type='file',class="upload",id=inputId, name=inputId)
#        )
#      )
#    
#    )
##<div class="fileUpload btn btn-primary">
##    <span>Upload</span>
##    <input type="file" class="upload" />
##    </div>
##
#}





# contextual panel
panel<-function(style="default",heading="",body=""){
style <- match.arg(style, c("", "primary", "info", "success", "warning", "danger", "inverse", "link"))
panStyle<-paste0('panel panel-',style)
tagList(
 tags$div(class=panStyle,
   tags$div(class="panel-heading",heading),
   tags$div(class="panel-body",body)
   ) 
  )
}

# format Sys.time to avoid spaces. 
getSysTime<-function(){
  format(Sys.time(),'%Y-%m-%d@%H_%M_%S')
}



