#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# additional custom reusable helper functions

#
##redefine actionButton from shiny: add style
#btn<-function (inputId, label, icon = NULL,sty=NULL, ...)
#{
#  tags$button(id = inputId, type = "button", class = "btn action-button", style=sty,
#    list(icon, label))
#}
#
## redefine inputText from shiny: add style
#txt<-function (inputId, label, value = "",sty=NULL)
#{
#  tagList(label %AND% tags$label(label, `for` = inputId), tags$input(id = inputId,
#      type = "text", value = value, style=sty))
#}
#
### redefine shiny:::`%AND%` add nothing,but needed in upload
#`%AND%` <-  function (x, y)
#{
#  if (!is.null(x) && !is.na(x))
#    if (!is.null(y) && !is.na(y))
#      return(y)
#  return(NULL)
#}
#

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


# wrapper around Sys.sleep. 
amSleep<-function(t=100){
  Sys.sleep(t/1000)
}



# GRASS helper functions :
# list location from GrassDB
grassListLoc<-function(grassDataBase)
  list.dirs(grassDataBase,recursive=F, full.names=F)

# list mapset from GRASS DB
grassListMapset<-function(grassDataBase,location)
  list.dirs(file.path(grassDataBase,location),full.names=F,recursive=F)


# clean all space and punctuation, replace by selected char, default is underscore.


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
amFilterDataTag_orig<-function(namesToFilter,prefixSep="__",tagSep='_',tagSepRepl=' ',filterTag,filterText){

  if(!is.null(filterTag) && !filterTag==""){
    filterAll<-filterTag
  }else{
    filterAll=NULL 
  }

  if(!is.null(filterText) && !filterText==""){
    filterTextVect<-unlist(strsplit(amSubPunct(filterText,','),','))
    filterAll<-c(filterTextVect,filterAll)
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
    exprFilter<-paste0('(?=.*\\b',filterAll,'\\b)',collapse='||')
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

amFilterDataTag<-function(namesToFilter,prefixSep="__",tagSep='_',tagSepRepl=' ',filterTag,filterText){
  # table with splitted names into prefix/suffix(tags) parts parts..
  exprTag<-paste0(".+?",prefixSep) # search characters before prefix separator
  exprPrefix<-paste0("?",prefixSep,'.+') # search character after prefix separator
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
  # first filter based on text field : any part of name, OR logic.
  # use any punctuation char in text filter as string split character
  if(!is.null(filterText) && !filterText==""){
    filterText<-unlist(strsplit(amSubPunct(filterText,','),','))
    rowsFiltText<-unlist(sapply(filterText,grep,tagsTable$nameFilter))
  }else{
    rowsFiltText=NULL
  }
  # second filter based on tags : whole words in name, AND logic.
  if(!is.null(filterTag) && !filterTag==""){
    exprFilter<-paste0('(?=.*\\b',filterTag,'\\b)',collapse='')
    rowsFiltTag<-grep(exprFilter,tagsTable$nameFilter,perl=T)
  }else{
    rowsFiltTag=NULL
  }
  rowsFilt<-unique(c(rowsFiltTag,rowsFiltText))
  
  if(!is.null(filterTag) && !filterTag=="" || !is.null(filterText) && !filterText==""){ 
    tagsTable<-tagsTable[rowsFilt,]
  }


  return(tagsTable) 
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



# function to create selectize compatible list of value
selectListMaker<-function(vect,default){ 
  vect<-c(default,vect)
  vect<-amSubPunct(vect)
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
amMsg<-function(session,type=c('error','warning','message','log','ui'),text,title=NULL,logFile=logPath){
  type<-match.arg(type)
  if(is.null(title))title=type
  stopifnot(!length(logFile)==0)
  if(!type=="ui"){ 
    text<-gsub('[\r\n]',' ',text)
    write(paste(amSysTime(),'\t',type,'\t',text,collapse=' '),file=logFile,append=TRUE)
  }
  if(type == 'log')return(NULL)
  text<-gsub("\"","",text,fixed=T)
  amSweetAlert(session, text,title,img="logo/icons/logo128x128.png",timer=2000)
}

# read only a subset of last lines
amReadLogs<-function(logFile,nToKeep=300){
  tryCatch({
    library(R.utils)
    nMsg<-countLines(logFile)
    nToSkip<-nMsg-nToKeep
    read.csv(logFile,sep='\t', header=FALSE, skip=nToSkip,stringsAsFactors=F) 
  },error=function(c)amMsg(session,'error',c)
  )
}






# control if location is arleady took. Worth a new function ? only used in newLoc 
ifNewLocAvailable<-function(newLoc){
  if(newLoc %in% grassListLoc(grassDataBase) || amSubPunct(newLoc) %in% grassListLoc(grassDataBase)){
    return(FALSE)
  }else{
    return(TRUE)
  }
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
      valid<-all(amSubPunct(shpExtMin,'') %in% fE)
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
      valid<-all(adfFilesMin%in% mN)
      if(!valid)stop(paste(
          "Accessmod esri binary grid validation:
          Trying to import invalid adf file dataset.
          Minimum required files are:",paste(adfFilesMin,collapse=', ')  
          ))
    }
  }
}


amRastExists<-function(filter=''){
  filter=paste0(filter,'*')
length(execGRASS('g.list',type='raster',pattern=filter,intern=TRUE))>0
}


# function to remove raster based on pattern
rmRastIfExists<-function(filter=''){
 filter=paste(filter,collapse=',') 
  rastList <- execGRASS('g.list',type='raster',pattern=filter,intern=TRUE)
  if(length(rastList)>0){
    print(filter)
    execGRASS('g.remove',flags=c('b','f'),type='raster',pattern=paste0(filter,sep='|'))
  }
}

rmVectIfExists<-function(filter='',names=''){

 filter=paste(filter,collapse=',') 
  vectList <- execGRASS('g.list',type='vector',pattern=filter,intern=TRUE)
  if(length(vectList)>0){
    execGRASS('g.remove',flags=c('b','f'),type='vector',pattern=paste0(filter,sep='|'))
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


amAppUpdate<-function(){
  browser()
  system('git merge FETCH_HEAD')
}

amAppVersion<-function(){
  system('git rev-list HEAD --count',intern=T)
}

amRemoteVersion<-function(){
  system('git fetch origin master')
  system('git rev-list FETCH_HEAD --count',intern=T)
}


packageManager<-function(pkgCran, pkgGit, libPath){
  tryCatch({
    # which package is missing ?
    pkgCranM <- pkgCran[!pkgCran %in% installed.packages()]
    pkgGitM <- pkgGit[!names(pkgGit) %in% installed.packages()]
    pkgCranL <- length(pkgCranM)
    pkgGitL <- length(pkgGitM)
    if(pkgCranL>0){
      inc <- 1/pkgCranL
      msgUpdate<-'Updating CRAN packages'
      withProgress(message = msgUpdate, value = 0.1, {
        amMsg(session,'log',msgUpdate)
        for(p in pkgCranM){ 
          install.packages(p, lib=libPath)
          incProgress(inc,detail=p)
        }
          })
    }
    if(pkgGitL>0){
      inc <- 1/pkgGitL
      msgUpdate<-'Updating GITHUB packages'
      withProgress(message = msgUpdate, value = 0.1, {
        amMsg(session,'log',msgUpdate)
        for(p in pkgGitM){ 
          with_lib(libPath, 
            install_github(p)
            )
          incProgress(inc,detail=p)
        }
          })
    } 
    # load libraries 
    lapply(pkgCran, require, character.only=TRUE)
    lapply(names(pkgGit), require, character.only=TRUE)

  },error=function(c)amMsg(session,'error',c))
}



listToHtml<-function(listInput,htL='',h=2, exclude=NULL){
  hS<-paste0('<H',h,'><u>',collapse='') #start 
  hE<-paste0('</u></H',h,'>',collapse='') #end
  h=h+1 #next
  if(is.list(listInput)){
    nL<-names(listInput)
    nL <- nL[!nL %in% exclude]
    htL<-append(htL,'<ul>')
    for(n in nL){
      #htL<-append(htL,c('<li>',n,'</li>'))
      htL<-append(htL,c(hS,n,hE))
      subL<-listInput[[n]]
      htL<-listToHtml(subL,htL=htL,h=h,exclude=exclude)
    }
    htL<-append(htL,'</ul>')
  }else if(is.character(listInput) || is.numeric(listInput)){
    htL<-append(htL,c('<li>',paste(listInput,collapse=','),'</li>'))
  }
  return(paste(htL,collapse=''))
}

#listToHtmlDesc<-function(listInput,h=1,htL='',exclude=NULL){
#  dS<-'<dl>' #start 
#  dE<-'</dl>'
#  iS<-'<dd>'
#  iE<-'</dd>'
#  tS<-'<dt>'
#  tE<-'</dt>'
#  h=h+1 #next
#  if(is.list(listInput)){
#    nL<-names(listInput)
#    nL <- nL[!nL %in% exclude]
#    htL<-append(htL,dS)
#    for(n in nL){
#      #htL<-append(htL,c('<li>',n,'</li>'))
#      htL<-append(htL,c(tS,n,tE))
#      subL<-listInput[[n]]
#      htL<-listToHtmlDesc(subL,htL=htL,h=h,exclude=exclude)
#    }
#    htL<-append(htL,dE) # last step
#  }else if(is.character(listInput) || is.numeric(listInput)){
#    htL<-append(htL,c(iS,paste(listInput,collapse=','),iE))
#  }
#  return(paste(htL,collapse=''))
#}


amExportData<-function(dataName,exportDir,type,vectFormat='shp',rastFormat='tiff',tableFormat='csv',dbCon=NULL){
  reportName<-paste0(dataName,'_report.txt')
  reportPath<-file.path(exportDir,reportName)
  infoName<-paste0(dataName,'_info.txt')
  infoPath<-file.path(exportDir,infoName)

  # default export function for grass.
  # be careful with this function : it uses unlink recursivly on provided filepath !
  # If other formats are requested, add other preformated command here.
  switch(type,
    'vector'={
      vInfo<-execGRASS('v.info',map=dataName,intern=TRUE)
      write(vInfo,infoPath)
      switch(vectFormat,
        'sqlite'={
          fileName<-paste0(dataName,'.sqlite')
          filePath<-file.path(exportDir,fileName)
          if(file.exists(filePath))unlink(filePath)
          execGRASS('v.out.ogr',
            input=dataName,
            output=filePath,
            flags=c('overwrite'),
            format="SQLite",
            dsco='SPATIALITE=yes')
        },
        'kml'={
          fileName<-paste0(dataName,'.kml')
          filePath<-file.path(exportDir,fileName)
          if(file.exists(filePath))unlink(filePath)
          execGRASS('v.out.ogr',
            input=dataName,
            output=filePath,
            flags=c('overwrite'),
            format="KML") 
        },
        'shp'={
          fileName<-dataName  # grass will export to a directory.
          filePath<-file.path(exportDir,fileName)
          if(filePath %in% list.dirs(exportDir))unlink(filePath,recursive=TRUE)
          execGRASS('v.out.ogr',
            input=dataName,
            output=filePath,
            flags=c('overwrite'),
            format="ESRI_Shapefile",
            dsco="ADJUST_TYPE=YES"
            )
        }
        )
      return(c(fileName,infoName))
    },
    'raster'={
      rInfo<-execGRASS('r.info',map=dataName,intern=TRUE)
      write(rInfo,infoPath)
      execGRASS('r.report',map=dataName,units=c('k','p'), output=reportPath, flags='overwrite')
      switch(rastFormat,
        tiff={
          # tiff with UInt16 data (integer in 0-65535)
          # this could lead to lost of information. 
          fileName<-paste0(dataName,'.GeoTIFF')
          reportPath<-paste0(dataName,'_report.txt')
          infoPath<-paste0(dataName,'_info.txt')
          filePath<-file.path(exportDir,fileName)
          execGRASS('r.out.gdal',
            flags =c('overwrite','f'),
            input=dataName,
            output=filePath,
            format="GTiff",
            #nodata=65535,
            createopt='TFW=YES'
            #type='UInt16') ## preserve cell type. Not a good idea for non-discrete values.
            )

        } # note : with force flags, Integer could lead to data loss !
        ) 

      return(c(fileName,infoName,reportName))
    },
    'table'={
      fileName<-paste0(dataName,'.csv')
      filePath<-file.path(exportDir,fileName)
      q<-paste('SELECT * FROM',dataName,';')
      tbl<-dbGetQuery(dbCon,q)
      write.csv(tbl,filePath)
      return(c(fileName))
    }
    )
}


#
#
#updateStyle<-function(id,type='e',element='border'){
#  # updateStyleBorder expects a reactiveStyle reactive object in parent env.
#  # id = css id (string)
#  # type = style id (string)
#  # colors examples :
#  #orange=rgb( 249, 235,200) 
#  #red =rgb(205, 52,34)
#  #green = rgb(49,172,0)
#  #blue = rgb(18,112,200)
#  if(element=='border'){
#    sty<-switch(type,
#      #error
#      'e'="{
#      width:100%;
#      border-color: rgb(205,52,34);
#      box-shadow: inset 0 1px 1px rgba(205,52,34, 0.075), 0 0 8px rgba(205,52,34, 0.6);}",
#      # ok
#      'o'="{
#      width:100%;
#      border-color: rgb(49,172,0);
#      box-shadow: inset 0 1px 1px rgba(49,172,0,0.075), 0 0 8px rgba(49,172,0,0.6);}",
#      # warning
#      'w'="{
#      width:100%;
#      border-color: rgb(249, 235,200);
#      box-shadow: inset 0 1px 1px rgba(249, 235,200,0.075), 0 0 8px rgba(249, 235,200,0.6);}",
#      # info
#      'i'="{
#      width:100%;
#      border-color: rgb(18,112,200 );
#      box-shadow: inset 0 1px 1px rgba(18,112,200,0.075), 0 0 8px rgba(18,112,200,0.6);}"
#      )
#    sty<-paste0('#',id,' ',sty)
#    listen$reactiveStyle[[id]]<-tags$style(type='text/css',sty)
#    return(NULL)
#}
#}
#

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



#hint<-function(hintId,text,iconFontAwesome='info-circle'){
#  txt<-p(icon(iconFontAwesome),text)
#  val<-tags$script(paste('$( "p" ).text( "<b>Some</b> new text." );'))
#  session$sendCustomMessage(
#    type="jsCode",
#    list(code=val)
#    )
#}
#



amRestart<-function(session){
  session$sendCustomMessage(
    type="jsCode",
    list(code='location.reload();')
    )
}

# update text by id
amUpdateText<-function(session,id,text){
  if(is.null(text) || text==""){
    return(NULL)
  }else{
    val<-paste0("$('#",id,"').html(\"",gsub("\"","\'",text),"\");")
    session$sendCustomMessage(
      type="jsCode",
      list(code=val)
      )
  }
}

## http://stackoverflow.com/questions/20637248/shiny-4-small-textinput-boxes-side-by-side
#amInlineSelect<-function(inputId, label, choices = "",selected=""){
#  div(style="display:inline-block",
#    tags$label(label, `for` = inputId), 
#    tags$select(id = inputId, value=choices, selected ,class="input-small"))
#}
#


amSweetAlert<-function(session, text, title=NULL,imgUrl=NULL,timer=NULL){
  #require sweetAlert.js and sweetAlert.css
  items<-list()
  if(!is.null(title))items$title<-paste0("title:'",title,"'")
  if(!is.null(img))items$img<-paste0("imageUrl:'",imgUrl,"'")
  if(!is.null(timer) && is.integer(timer))items$timer<-pastae0("timer:'",timer,"'")
  items$text<-paste0("text:\"",text,"\"")

  val<-paste("swal({",paste0(items,collapse=','),"})")
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )

}

# link selected archive to a new window location. The browser should as to download.
#TODO: as it's rendered in the same window, it could break shiny application, or reset it. Make sure that's not a problem with standard browser. Works with webkit browser.
amGetData<-function(session,dataPath){
  if(!is.null(dataPath) && !dataPath==""){
  val<-paste0("window.location.assign('",dataPath,"');")
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )
  }
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
#
#amFileInput<-function (inputId, label, style = NULL,disabled = FALSE, fileAccept=NULL, multiple=FALSE){
#  style <- match.arg(style, c("", "primary", "info", "success", "warning", "danger", "inverse", "link"))
#  
#  inputTag<-tags$input(
#          type=ifelse(disabled,'reset','file'),
#          class='upload',
#          accept=paste(fileAccept,collapse=','),
#          id=inputId,
#          name=inputId)
#
#  if(multiple) inputTag$attribs$multiple='multiple'
#
# spanTag<-tags$span(label) 
#  
#inputClass<-div(class=c('btn-browse btn'),
#   tList<- tagList(
#     spanTag,
#     inputTag
#     )
#   )
#  if (disabled){
#    inputClass$attribs$class <- paste(inputClass$attribs$class,"disabled")
#  }
#  if (!is.null(style)) {
#    inputClass$attribs$class <- paste(inputClass$attribs$class,paste0("btn-", tolower(style)))
#  }
##browser()
# tagList(inputClass, tags$div(id = paste(inputId,
#        "_progress", sep = ""), class = "progress progress-striped active shiny-file-input-progress",
#      tags$div(class = "bar"), tags$label()))
#
#  #return(shinyBS:::sbsHead(inputClass))
#}


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
#panel<-function(style="default",heading="",body=""){
#  style <- match.arg(style, c("", "primary", "info", "success", "warning", "danger", "inverse", "link"))
#  panStyle<-paste0('panel panel-',style)
#  tagList(
#    tags$div(class=panStyle,
#      tags$div(class="panel-heading",heading),
#      tags$div(class="panel-body",body)
#      ) 
#    )
#}

# format Sys.time to avoid spaces. 
amSysTime<-function(type=c('fancy','compatible','short')){
  if(is.null(type))type='fancy'
  type=match.arg(type)
  switch(type,
    'fancy'=format(Sys.time(),'%Y-%m-%d@%H_%M_%S'),
    'compatible'=format(Sys.time(),'%Y_%m_%d_%H_%M_%S'),
    'short'=format(Sys.time(),'%Y%m%d%H%M%S')
  )
}


amTimeStamp<-function(text=NULL){
  if(is.null(text))text='AccessMod'
  w=68
  t<-amSysTime()
  u<-toupper(text)
  uS<-(w-nchar(u)-2)/2
  tS<-(w-nchar(t)-2)/2
  sideH<-paste(rep('-',uS),collapse='')
  sideT<-paste(rep(' ',tS),collapse='')
  head<-paste('#',sideH,u,sideH,'#',collapse='')
  body<-paste(' ',sideT,t,sideT,' ',collapse='')
  sideF<-paste(rep('-',nchar(head)-4),collapse='')
  foot<-paste('#',sideF,'#',collapse='')
  cat(c(head,body,foot,collapse=''),sep='\n')
}

# jquery toggle:
# "$('#",id,"').prop('disabled', function(i, v) { return !v; });",
#        "$('#",id,"').removeClass('btn-default btn-danger');")



#https://gist.github.com/xiaodaigh/6810928
# check if use of toggleClass could be a better choice.
amActionButtonToggle <- function(id,session,disable=TRUE) {
  addDefault<-paste0("$('#",id,"').addClass('btn-default').removeClass('btn-danger').prop('disabled',false);")
  addDanger<-paste0("$('#",id,"').addClass('btn-danger').removeClass('btn-default').prop('disabled',true);")

  val<-ifelse(disable,addDanger,addDefault)
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )
}





# amUpdateProgressBar : update amPgoressBar
# session = shiny session
# idBar = id set with amProgressBar()
# amout = value from 0 to 100
# hide = hide progress bar
amUpdateProgressBar<-function(session,idBar,amount=0,final=F){
  a<-as.integer(amount)
  if(a>100 || a <0)warning('amUpdateProgressBar amount not allowed')
  finalStep<-ifelse(a>99||final,paste0(",function(){$(this).width('0%')}"),"")
  #val<-paste0("$('#",idBar,"').width('",amount,"%');$('#",idBar,"').height('",h,"%');")

  prog<-paste0(
    "jQuery.fx.interval = 100;",
    "$('#",idBar,"')",
    ".stop(true,true)",
    ".animate({width:'",a,"%'},500",
    finalStep,
    ");")
  session$sendCustomMessage(
    type='jsCode',
    list(code=prog)
    )
}







amFileInputUpdate<-function(id,session,accepts=NULL,multiple=NULL){
  accepts<-paste(accepts,collapse=',')
  multiple<-ifelse(multiple,'true','false')
  accepts<-paste0("$('input#",id,"').prop('accept','",accepts,"');")
  multiple<-paste0("$('input#",id,"').prop('multiple',",multiple,");")
  val=paste(accepts,multiple)
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )
}



#amFileInput<-function (inputId, label, fileAccept=NULL, multiple=FALSE){
#  inputTag<-tags$input(
#    type='file',
#    class='upload',
#    accept=paste(fileAccept,collapse=','),
#    id=inputId,
#    name=inputId)
#  if(multiple) inputTag$attribs$multiple='multiple'
#  spanTag<-tags$span(label)
#  inputClass<-tags$button(
#    class=c('btn-browse btn btn-default'),
#    id=inputId,
#    tList<- tagList(
#      spanTag,
#      inputTag
#      )
#    )
#  tagList(
#    amProgressBar(inputId),
#    inputClass
#    )
#}
#






  #        stopifnot(length(colorSetting)==2)

#
#
# upload tables
#
#
amUploadTable<-function(dataName,dataFile,dataClass,listen){
  message('Upload table process for ',dataName)
  fE<-file_ext(dataFile)
  switch(fE,
    'csv'={
      tbl=read.csv(dataFile, header=TRUE,sep=',')
      if(nrow(tbl)<4 || ncol < 2)
        stop(paste('Importation of ',basename(dataFile),' : number of rows <4 or number of columns < 2. Make sure that the cells in your table is separated by commas [,] characters (csv=comma separated values) and that your table is more than 4x2 cells.'))
    },
    'xls'=tbl<-read.xls(dataFile),
    'xlsx'=tbl<-read.xls(dataFile) 
    )
  aNames<-acceptColNames[[dataClass]]
  tNames<-tolower(names(tbl))
  names(tbl)<-tNames
  if(!all(aNames %in% tNames))
    stop(paste('Importation of ',basename(dataFile),' : dataset of class ',dataClass,' should contain columns named ',paste(aNames,collapse=';'),'. The provided file contains those columns :',paste(tNames,collapse=';'),'.'))
  dbWriteTable(isolate(listen$dbCon),dataName,tbl,overwrite=TRUE)
  amUpdateDataList(listen)
}

amErrHandler<-function(errMsgList,conditionMsg,title=NULL){
  stopifnot(class(errMsgList)=="error_list")
  errFound<-sapply(names(errMsgList),grep,conditionMsg)
  errMsg<-errMsgList[errFound %in% 1]
  if(length(errMsg)==0){#error was not in list. return original condition message.
    amMsg(session,type='warning',text=conditionMsg,title=title)
  }else if(length(errMsg)>0){
    for(i in 1:length(errMsg)){ 
      amMsg(session,type=tolower(errMsg[[i]]$type),errMsg[[i]]$text,title=title)
    }
  }else{
    stop(paste("amErrHandler encounter unexpected case. ConditionMsg=",conditionMsg,". Name of error available=",paste(names(errMsgList),collapse=',')))
  }
}



amErrorAction <- function(expr,quotedActionError=NULL,quotedActionWarning=NULL,quotedActionMessage=NULL, title){
  withCallingHandlers({
    tryCatch({
      expr
    },
    # error : stop process, eval error quoted function, return condition to amErrHandler
    error = function(cond){
      if(!is.null(quotedActionError))eval(quotedActionError)
      amErrHandler(errMsgList,paste(cond),title=title)
  })},
    # warning, don't stop process, but return condition to amErrHandler
    warning= function(cond){
      if(!is.null(quotedActionWarning))eval(quotedActionWarning)
      amErrHandler(errMsgList,paste(cond),title=title)
    },
    # simple message : don't stop, write in log
    message= function(cond){
      if(is.null(quotedActionMessage))eval(quotedActionMessage)
      amMsg(session,'log',paste(cond),title=title)  
    }
    )
}

#
#
# Upload raster
#
#
amUploadRaster<-function(dataInput,dataName,dataFiles,colorsTable,listen){
  #dataInput=unique files or folder to give to gdal
  #dataName = name of output data
  #dataFile = actual list of files.
  tryReproj=TRUE
  if(!is.null(colorsTable)){
    colConf<-as.list(strsplit(colorsTable,'&')[[1]])
    if(length(colConf)==2){
      cN<-c('color','flag')
    }else{
      cN<-c('color')
    }
    names(colConf)<-cN
  }
  # raster validation.
  validateFileExt(dataFiles,'rast')
  # temp geotiff
  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.tiff'))
  gdalwarp(dataInput,
    dstfile=tmpDataPath,
    t_srs=if(tryReproj){getLocationProj()},
    dstnodata="-9999",
    output_Raster=FALSE,
    overwrite=TRUE)

  message('GDAL finished cleaning.')
  if(file.exists(tmpDataPath)){
    execGRASS('r.in.gdal',
      input=tmpDataPath,
      output=dataName,
      flags=c('overwrite','quiet'),
      title=dataName)
if(!is.null(colorsTable)){
  message(paste('Set color table to',colConf$color,'with flag=',colConf$flag))
    execGRASS('r.colors',map=dataName,flags=colConf$flag,color=colConf$color)
}
    message(paste("Manage data:",dataName,'loaded in accessmod.'))
  }else{
    stop('Manage data: process aborded, due to unresolved CRS or not recognized input files. Check files metadata and extent. Importation cancelled.')
  }
  # create a rasterlayer to get projection info from the file
  # (raster not loaded in memory)
  r<-raster(tmpDataPath)
  givenProj<-proj4string(r)
  if(!givenProj==getLocationProj()){
    message(paste(
        "Information:",
        dataName,
        "was imported successfully but did not match exactly the CRS of current project. See logs for details."))
    message(paste(
        "Manage data info. ",
        dataName,
        "Raster's proj4string:",
        givenProj,
        ". Accessmod current proj4string:",
        getLocationProj()))
  }
  file.remove(c(dataFiles, tmpDataPath))

  amUpdateDataList(listen)
  return(NULL)
}

amUploadNewProject<-function(newDem,newProjectName){ 
  # capture all error from now, from potentially error prone steps.
  message(paste('importation process for',newProjectName,'started'))
  # TODO:
  # 1. check for better method for filre recognition
  # 2. This function requires a lot of external variable: check if those must be passed as argument instead.
  newDem<-newDem[with(newDem, order(-size)),]
  tmpDir<-dirname(newDem[1,'datapath'])
  newDem$newPath<-file.path(tmpDir,newDem$name)
  file.rename(newDem$datapath,newDem$newPath)
  # raster validation.
  validateFileExt(newDem$name,'rast')
  # take the first raster (heavier) as the base map
  tmpMapPath<-newDem[1,'newPath']
  # test for projection issues 
  r<-raster(tmpMapPath)
  destProj<-proj4string(r) 
  if(is.na(destProj))stop(msgNoProj)
  if(!length(grep('+to_meter|+units=m',destProj))>0)stop(msgNotMetric)
  # get proj4string
  message(paste('Projection detected:',destProj));
  # empty grid for the default WIND object
  sg<-as(r,'SpatialGrid')
  # grass initialisation.
  message('Init new grass session')
   unset.GIS_LOCK()
      unlink_.gislock()
  initGRASS(gisBase = grassBase70, # binary files (grass 7.0)
    home            = grassHome, # where store lock file
    gisDbase        = grassDataBase, # local grass database
    location        = newProjectName, # rsession
    mapset          = 'PERMANENT', # PERMANENT for dem.
    SG              = sg, #spatial grid as templte for extent and res
    override        = TRUE)
  execGRASS('g.proj',flags='c',proj4=destProj)
  execGRASS('db.connect',driver='sqlite',database=sqliteDB)
  # set as default region
  message('Set grass environment and import DEM')
  execGRASS('g.gisenv',flags='s')
  amDebugMsg('tmpMapPath exists:',file.exists(tmpMapPath))
  execGRASS('r.in.gdal',
    input=tmpMapPath,
    output=configDem,
    flags=c('overwrite','quiet'),
    title=paste(newProjectName,'DEM')
    )
execGRASS('r.colors',map=configDem,color='elevation')
  message('Set default region based on DEM and set null values as zeros to enable accessibility calculation in sea region. ')
  execGRASS('g.region', raster=configDem)
   unset.GIS_LOCK()
      unlink_.gislock()
  message('Removing temp files.')
  file.remove(tmpMapPath)
}


#
# Upload vectors
#
#
amUploadVector<-function(dataInput, dataName, dataFiles,listen){
  tryReproj=TRUE
  # helper function to validate file based on extension
  validateFileExt(dataFiles,'vect')
  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.shp'))
  ogr2ogr(
    src_datasource_name=dataInput,
    dst_datasource_name=tmpDataPath,
    #where=input$dataSql,
    f="ESRI Shapefile",
    t_srs=if(tryReproj){getLocationProj()},
    overwrite=TRUE,
    verbose=TRUE)
  message('GDAL finished cleaning. Importation in GRASS.')
  execGRASS("v.in.ogr",
    flags=c("overwrite","w","2"), # overwrite, lowercase, 2d only,
    parameters=list(input=tmpDataPath, output=dataName, snap=0.0001)
    )

  message(paste(dataName,'loaded in accessmod.'))
  unlink(dataFiles)
  amUpdateDataList(listen)
  return(NULL)
}
#
#  # validate rules of input file.
#  #fE<-file_ext(dataNew$name)
#  # helper function to validate file based on extension
#  validateFileExt(dataInput,'vect')
#  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.shp'))
#  ogr2ogr(
#    src_datasource_name=dataInput,
#    dst_datasource_name=tmpDataPath,
#    where=input$dataSql,                                                                                                  f="ESRI Shapefile",
#    t_srs=if(tryReproj){getLocationProj()},
#    overwrite=TRUE,
#    verbose=TRUE)
#  msg('GDAL finished cleaning. Importation in GRASS.')
#  tryCatch({
#    execGRASS("v.in.ogr",
#      flags=c("overwrite","w","r","2"), # overwrite, lowercase, current region, 2d only,
#      parameters=list(dsn=tmpDataPath, output=dataName, snap=0.0001)
#      )
#    unlink(lF)
#    msg(paste('Module import:',dataName,'Imported in GRASS.'))
#    listen$uploadData<-sample(100,1)
#  },
#  error=function(cond){
#    file.remove(lF)
#    hintBadProjection<-'Projection of dataset does not appear to match current location.'
#    cndMsg <- conditionMessage(cond)
#    badProjection<-if(length(grep(hintBadProjection,cndMsg))>0){
#      msg('ERROR: The data projection is wrong or absent. Please match it with the base data (DEM)')
#    }else{
#      msg(cond)
#    }
#  }
#  )


#
#amPanelSimple<-function(...,width=9,fixed=F,noPadding=T){
#
#  tags$div(class=paste('col-sm-',as.integer(width)),
#    tags$div(class=paste0('box box-solid am-square',if(noPadding){'no-padding'},if(fixed){'am-fixed'}),
#      ...
#      )
#    )
#
#
#}

#amPanel<-function(...,width=9,fixed=F){
#  fixed<-ifelse(fixed,'am-fixed','')
#   tags$div(class=paste0('col-sm-',as.integer(width)),
#    tags$div(id=id,class=paste('box box-solid no-padding am-square',fixed),
#      tags$div(class='box-body',
#        ...
#        )
#      )
#    )
#}
#



amUpdateDataList<-function(listen){
  amDebugMsg('update list')
  listen$dataListUpdate<-runif(1)
}

amUpdateProjectList<-function(listen){
  amDebugMsg('update project')
  listen$projectListUpdate<-runif(1)
}


#
#tags$div(class='col-sm-9',
#      tags$div(class='box box-solid no-padding am-square',
#        tags$div(class='box-body',
#          h3('Project information'),
#          hr(),
#          infoPanel
#          )
#        )
#      )



amDebugMsg<-function(...){
  cat(paste('{ debug',amSysTime(),'}',...),sep='\n')

}



amGetGrassMeta<-function(crsOut=c('orig','latlong')){
  crsOut<-match.arg(crsOut)


  locationExt<-as(extent(gmeta2grd()),'SpatialPolygons')
  proj4orig<-getLocationProj(ignore.stderr=T)
  proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
  proj4string(locationExt)<-proj4orig
  if(crsOut=='latlong')
    locationExt<- spTransform(locationExt,CRS(proj4dest))
  bbx<-as.data.frame(locationExt@bbox)
  bbxCenter<-c((bbx['y','max']+bbx['y','min'])/2,(bbx['x','max']+bbx['x','min'])/2)
  bbxBound<-list(list(bbx['y','min'],bbx['x','min']),list(bbx['y','max'],bbx['x','max']))

  if(crsOut=='latlong'){
    # set a geojson bounding box with a inner ring.
    extStyle<-list(
      fillColor = "black",
      fillOpacity = 0.5,
      opacity=0.1,
      weight = 1,
      color = "#000000"
      )

    ext<-fromJSON(geojson_json(locationExt)[[1]])
    worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
    extCoord<-ext$features[[1]]$geometry$coordinates[[1]]
    ext$features[[1]]$geometry$coordinates<-list(worldCoord,extCoord)
    ext$style<-extStyle
  }else{
    ext=NULL
  }

  bbxList<-as.list(locationExt@bbox)
  names(bbxList)<-c('xmin','ymin','xmax','ymax')

  gL<-gmeta()
  metaList<-list(
    "North-south resolution:"               = gL$nsres,
    "East-west reolution"                   = gL$ewres,
    "Bounding box (xmin, xmax, ymin, ymax)" = bbxList,
    "Number of cells"                        = gL$cells,
    "Number of rows"                        = gL$rows,
    "Number of columns"                     = gL$cols
    )
  metaHtml<-listToHtml(metaList,h=6) 

  return(list(
      "bbxGeoJson"=ext,
        "bbxSp"=locationExt,
    "bbxDf"=bbx,
    "bbxLeaflet"=bbxBound,
    "bbxCenter"=bbxCenter,
    "summary"=metaList,
    "summaryHtml"=metaHtml,
    "projOrig"=getLocationProj(ignore.stderr=T)
    )
    )


}



amMapMeta<-function(){
  meta<-list()
  proj<-list(
    orig=getLocationProj(ignore.stderr=T),
    latlong='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
    )
  bbx<-as(extent(gmeta2grd()),'SpatialPolygons')
  proj4string(bbx)<-proj$orig
  bbxSp<-list(
    orig=bbx,
    latlong=spTransform(bbx,CRS(proj$latlong))
    )

  for(p in names(proj)){
    bx=bbxSp[[p]]
    bxD<-as.data.frame(bx@bbox)
    meta<-c(meta,
      structure(
        list(
          list(
            'proj'=proj[[p]],
            'bbx'=list(
              'ext'=list(
                'x'=list(
                  'min'=bxD['x','min'],
                  'max'=bxD['x','max']
                  ),
                'y'=list(
                  'min'=bxD['y','min'],
                  'max'=bxD['y','max']
                  )
                ),
              'center'=c((bxD['y','max']+bxD['y','min'])/2,(bxD['x','max']+bxD['x','min'])/2)
              ))   
          ),names=p
        )
      )
  }

  gL<-gmeta()

  meta$grid<-list(
    "North-south resolution:"               = gL$nsres,
    "East-west reolution"                   = gL$ewres,
    "Number of cells"                        = gL$cells,
    "Number of rows"                        = gL$rows,
    "Number of columns"                     = gL$cols
    )

  return(meta)
}

# extract spatial polygons from mapMeta
amBboxSp<-function(mapMeta,proj=c('orig','latlong')){
  proj<-match.arg(proj)
  bbx<-as(extent(mapMeta[[proj]]$bbx$ext),"SpatialPolygons")
  proj4string(bbx)<-CRS(mapMeta[[proj]]$proj)
  bbx
}

# extract geojson from mapMeta
amBboxGeoJson<-function(mapMeta,proj=c('orig','latlong')){
  proj<-match.arg(proj)
  bbx<-as(extent(mapMeta[[proj]]$bbx$ext),"SpatialPolygons")
  bbxStyle<-list(
      fillColor = "black",
      fillOpacity = 0.5,
      opacity=0.1,
      weight = 1,
      color = "#000000"
      )
    bbx<-fromJSON(geojson_json(bbx)[[1]])
    worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
    bbxCoord<-bbx$features[[1]]$geometry$coordinates[[1]]
    bbx$features[[1]]$geometry$coordinates<-list(worldCoord,bbxCoord)
    bbx$style<-bbxStyle
    return(bbx)
}

# extract geojson from mapMeta
amSpotlightGeoJson<-function(mapToPreview){
browser()


  bbx<-as(extent(mapMeta[[proj]]$bbx$ext),"SpatialPolygons")
  bbxStyle<-list(
      fillColor = "black",
      fillOpacity = 0.5,
      opacity=0.1,
      weight = 1,
      color = "#000000"
      )
    bbx<-fromJSON(geojson_json(bbx)[[1]])
    worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
    bbxCoord<-bbx$features[[1]]$geometry$coordinates[[1]]
    bbx$features[[1]]$geometry$coordinates<-list(worldCoord,bbxCoord)
    bbx$style<-bbxStyle
    return(bbx)
}


amAddOverlay<-function(session,mapId,imgBounds,imgUrl){
  imgBounds<-toJSON(imgBounds)
  var=paste0("L.imageOverlay('",imgUrl,"',",imgBounds,").addTo(",mapId,");")

  session$sendCustomMessage(
    type="jsCode",
    list(code=var)
    )
}




# find  one cell diagonal bridge between multiple raster maps (e.g. road) and destination map (e.g. merged lcv)
# warning : only tested from rasterized lines with densified option. 
amBridgeFinder<-function(fromMap,toMap,bridgeMap){
  stopifnot(length(bridgeMap)==1)
  execGRASS('r.mapcalc',expression=sprintf("%s=null()",bridgeMap),flags='overwrite')
  for(map in fromMap){
    expr<-do.call(sprintf,c(list("if(!isnull(%s),
          isnull(%s[0,-1]) &&
          !isnull(%s[1,-1]) && 
          isnull(%s[1,0]) ||
          isnull(%s[0,1]) && 
          !isnull(%s[1,1]) && 
          isnull(%s[1,0]) ||
          isnull(%s[-1,0]) && 
          !isnull(%s[-1,1]) && 
          isnull(%s[0,1]) ||
          isnull(%s[0,-1]) && 
          !isnull(%s[-1,-1]) && 
          isnull(%s[-1,0])?1:%s,%s)"),c(map,rep(toMap,12),rep(bridgeMap,2))
        ))
    #expr<-paste0(newMap,"=",newMap,"+",gsub("\\n","",expr))
    expr<-paste0(bridgeMap,"=",gsub("\\n","",expr))
    execGRASS('r.mapcalc',expression=expr,flags='overwrite')
  }
  stat<-read.table(text=execGRASS('r.univar',map=bridgeMap,flags='t',intern=T),sep="|",header=T)
  nBridges<-stat[1,"non_null_cells"]
  if(nBridges>0){
    message(paste(
        'Accessmod found',nBridges,
        'one cell diagonal bridges.
        Output control map is',bridgeMap))
  }
}

# remove cell defined in bridgeMap from removeFromMap.
amBridgeRemover<-function(bridgeMap,removeFromMap){
  tmpRules<-tempfile()
  write(execGRASS('r.category',map=removeFromMap,intern=T),tmpRules)
  expr<-paste0(removeFromMap,"=if(!isnull(",bridgeMap,"),null(),",removeFromMap,")")
  execGRASS('r.mapcalc',expression=expr,flags='overwrite')
  execGRASS('r.category',map=removeFromMap,rules=tmpRules)
  message(paste('Bridges from',bridgeMap,'removed from',removeFromMap))
}

#https://gist.github.com/jmarhee/8530768
amMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}




amNameCheck<-function(name,class=c('vector','raster','table')){
  class=match.arg(class)
  name<-as.character(name)
  nameNoMapset<-unlist(strsplit(name,paste0("(",sepMapset,").+")))
  if(length(nameNoMapset)==0)return(NULL)
  if(class=='table'){
    if(all(nameNoMapset %in% dbListTables(listen$dbCon))){
      return(nameNoMapset)
    }else{
      return(NULL)
    }
  }else{
    if(all(name %in% dataList[[class]])){
      return(nameNoMapset)
    }else{
      return(NULL)
    }
  }
}




# function to handle grass naming to am
# 


# formating new name
amNewName<-function(class,tags,sepClass,sepTag){
    tags<-paste(tags,collapse=sepTag)
  tags<-amSubPunct(tags,sepTag)
    paste0(c(class,tags),collapse=sepClass)
}
# amNewName('land_cover',c('test','2012'),"$","_")
# return:
# [1] "land_cover$test_2012"





## sample names
# sampleName<-c("land_cover_table~[super,super]@malawi_90_m",
#                       "land_cover~[super,super]@malawi_90_m",
#                                          "population~[super,new]@malawi_90_m")
## sample names
#sampleName<-c("land_cover_table$test_super",
#                "land_cover$super_super",
#                              "population$super_new")
#
#

# create list usable to populate select input
amCreateSelectList<-function(dName,sepTag,sepClass,mapset){
  amErrorAction(title='amCreateList',{
      sepMap="@"
      if(length(dName)==0)return(NULL)
      l=as.list(paste0(dName,sepMap,mapset))
      lN=character(0)
      for(n in dName){
        dat=unlist(strsplit(n,sepMap))[[1]]
        cla=unlist(strsplit(dat,paste0("\\",sepClass)))[[1]]  
        tag=unlist(strsplit(dat,paste0("\\",sepClass)))[[2]]
        tag=paste0("[",gsub(sepTag,' ',tag),"]")
        lN<-c(lN,paste(cla,tag))
      }
      names(l)<-lN
      return(l)  
    }
    )
}

# amDataNameList(sampleName)
# return :
# $`land_cover_table [test+super]`
# [1] "land_cover_table$test_super@malawi90m"
# 
# $`land_cover [super+super]`
# [1] "land_cover$super_super@malawi90m"
# 
# $`population [super+new]`
# [1] "population$super_new@malawi90m"
#
# In reactive dList:
# dList$raster<-amDataNameList(sampleName)
#

# NOTE: why function with similar names ? clean !
# from a tag vector, get unique tags and order them
amGetUniqueTag<-function(x,sepIn,sepOut,ordered=TRUE){
  #x =string containing tags : ex. test+super+super
  #sepIn separator in input string  e.g. +
  #sepOut separator in output string  e.g. _
  if(length(x)==1){
    x<-amSubPunct(x,sep=sepIn)
    x<-t(read.table(text=x,sep=sepIn))[,1]
    if(ordered==TRUE){
      x<-x[order(x)]
    }
    return(paste0(na.omit(unique(x)),collapse=sepOut))
  }else{
    stop('getUniqueTagString: length of input not 1 ')
  }
}
# return : unique ordered tag e.g. super_test instead of test+super+super





# get all available tags from a list
amGetUniqueTags<-function(amData){
    if(is.list(amData))amData<-names(amData)
  unique(unlist(strsplit(unlist(amData),'.(\\[)|(\\])|(\\+)|.(@)|(,)')))
}
# example :
#  > dList$raster
# $`land_cover_table [test+super]`
# [1] "land_cover_table$test_super@malawi90m"
# 
# $`land_cover [super+super]`
# [1] "land_cover$super_super@malawi90m"
# 
# $`population [super+new]`
# [1] "population$super_new@malawi90m"
#  
# amGetUniqueTags(dList$raster)
# return :
# 
# [1] "land_cover_table" "test"             "super"            "land_cover"       "population"      
# [6] "new" 


# remove mapset part. E.g. for sqlite.
amNoMapset<-function(amData,sepMap="@"){
    amData<-as.character(amData)
  res<-unlist(strsplit(amData,paste0("(",sepMap,").+")))
    if(length(res)==0)res=NULL
    res
}

# get class of data
amGetClass<-function(amData,sepClass){
    as.character(strsplit(unlist(amData),paste0('(\\',sepClass,').*')))
}
# amGetClass(dList$raster)
# return : 
# [1] "land_cover_table" "land_cover"       "population"
#

# get tag of data
amGetTag<-function(amData){
  if(is.list(amData))amData<-names(amData)
  tmp<-gsub(".+(?=\\[)|(\\[)|(\\])","",amData,perl=T)
  tmp<-gsub("\\_"," ",tmp)
  tmp
}
# amGetTag(dList$rast)
# return :
# [1] "super super" "super super" "super new" 

# create data.frame version of dataList
amDataListToDf<-function(amDataList,sepClass,type='raster'){
  if(is.null(amDataList)||length(amDataList)<1)return(NULL)
  cla=amGetClass(amDataList,sep=sepClass)
  tag=amGetTag(amDataList)
  name=amNoMapset(amDataList)
  data.frame(class=cla,
    tags=tag,
    type=type,
    searchCol=paste(type,cla,tag),
    origName=name
    )
}
# Example
# > amDataListToDf(dList$raster)
# returns:
# class        tags   type                          searchable
# 1 land_cover_table super super raster raster land_cover_table super super
# 2       land_cover super super raster       raster land_cover super super
# 3       population   super new raster         raster population super new
 

# Create a subset of the data frame.
amDataSubset<-function(pattern='',type=NULL,amDataFrame){
    if(nchar(pattern)>0){    
          pattern=amSubPunct(pattern,'|')
    tbl<-amDataFrame[grep(pattern,amDataFrame$searchCol),]
        #if(!selected=="" && typeof(selected) == logical)tbl<-tbl[selected,]
      }else{
            tbl<-amDataFrame
      }  
  if(!is.null(type))tbl<-tbl[tbl$type %in% type,]
    tbl
}


# remove all unwanted characters, remplace by sep of choice 
 amSubPunct<-function(vect,sep='_'){
      vect<-gsub("'",'',iconv(vect, to='ASCII//TRANSLIT'))
   gsub("[[:punct:]]+|[[:blank:]]+",sep,vect)
    }
# example :
# amSubPunct('hérétique:crasy#namer*ßuss','_')
# [1] "heretique_crasy_namer_ssuss"
 
