#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# additional custom reusable helper functions


# wrapper around Sys.sleep. Sleep in milisecond 
amSleep<-function(t=100){
  Sys.sleep(t/1000)
}

#' Use system grep to return list of file matching grep exp
#' @param exp {character} Regex expression
#' @param fixed {boolean} search for fixed string
#' @param ext {string} search for file with this extension
#' @export
amGrep <- function(exp,fixed=TRUE,ext=NULL){
  cmd <- ifelse(fixed,"grep -RFl","grep -REL")
  if(!is.null(ext)){
    cmd <- sprintf("%1$s %2$s",cmd,paste(sprintf("--include \\*.%1$s",ext),collapse=""))
  }
  system(sprintf("%1$s '%2$s' .",cmd,exp))
}




#' Time interval evaluation
#' @param action "start" or "stop" the timer
#' @param timerTitle Title to be displayed in debug message
#' @return
amTimer <- function(action=NULL,timerTitle=NULL){
  diff <- 0
  env <- parent.frame(1)

  if(is.null(timerTitle)) timerTitle = "timer"
  if(is.null(action)) action = "stop"

  if(action=="start"){

    env$.mxTimer <- list(
      time=Sys.time(),
      title=timerTitle
      )

  }else{

    if(!is.null(env$.mxTimer)){

      diff <- difftime(
        Sys.time(),
        env$.mxTimer$time
        )

      diff <- format(round(diff,3))

      amDebugMsg(
        sprintf('%1$s %2$s'
          , env$.mxTimer$title
          , diff
          )
        )

    }
  }

  return(diff)
}




# GRASS helper functions :
# list location from GrassDB
amGetGrassListLoc<-function(grassDataBase)
  list.dirs(grassDataBase,recursive=F, full.names=F)

# list mapset from GRASS DB
grassListMapset<-function(grassDataBase,location)
  list.dirs(file.path(grassDataBase,location),full.names=F,recursive=F)


grassReloadRegion<-function(demFile){
  execGRASS('g.region',flags='d')
  execGRASS('g.region',raster=demFile)
}


amGetArchiveList<-function(archivesPath,baseName){
  # archiveGrass need grass environment variables, as defined in config.R

  if(nchar(Sys.getenv("GISRC"))==0) stop("Need an active grass session")
  archivesPath<-system(paste('echo',archivesPath),intern=TRUE) 
  # if archive directory doesn't exist, create it.
  dir.create(archivesPath,showWarnings = FALSE)
  archivesPath<-normalizePath(archivesPath) 
  # add ressource for shiny 
  addResourcePath(
    prefix=baseName,
    directoryPath = archivesPath
    )
  # return archive list
  list.files(archivesPath)
}

amGetShapesList<-function(pattern=".shp$",shapePath=config$pathShape){
  # path need grass environment variables, as defined in config.R
  if(nchar(Sys.getenv("GISRC"))==0) stop("Need an active grass session")
  shapePath<-system(paste('echo',shapePath),intern=TRUE) 
  # if  directory doesn't exist, create it.
  dir.create(shapePath,showWarnings = FALSE)
  shapePath<-normalizePath(shapePath) 
  # add ressource for shiny 
  # return archive list
  shapeList<-list.files(shapePath,pattern=pattern,full.names=T)
  if(length(shapeList)>0){
    #nameCatch<-gsub('.shp',paste0('@',location),catchList)
    nameShape<-gsub('.shp','',basename(shapeList))
    names(shapeList) <- nameShape
    as.list(shapeList)
  }else{
    list()
  }
}

amGetListsList<-function(pattern=".json$",listPath=config$pathList){
  # path need grass environment variables, as defined in config.R
  if(nchar(Sys.getenv("GISRC"))==0) stop("Need an active grass session")
  listPath <- system(paste('echo',listPath),intern=TRUE) 
  # if  directory doesn't exist, create it.
  dir.create(listPath,showWarnings = FALSE)
  listPath<-normalizePath(listPath) 
  # add ressource for shiny 
  # return archive list
  listList<-list.files(listPath,pattern=pattern,full.names=T)
  if(length(listList)>0){
    nameList <- gsub('.json','',basename(listList))
    names(listList) <- nameList
    as.list(listList)
  }else{
    list()
  }
}

amFilterDataTag <- function(namesToFilter,prefixSep="__",tagSep='_',tagSepRepl=' ',filterTag,filterText){
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



#http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
amCleanHtml <- function(htmlString) {
  return(gsub("<.*?>", "", paste(htmlString)))
}



amMsg<-function(session=shiny:::getDefaultReactiveDomain(),type=c('error','warning','message','log','ui'),text,title=NULL,subtitle=NULL,logFile=config$pathLog,...){
  type<-match.arg(type)

  if(is.null(title))title=type
  if(is.null(subtitle))subtitle=type
  stopifnot(!length(logFile)==0)
  #clean for log

  if('html' %in% class(text) || 'shiny.tag.list' %in% class(text)){
    textLog=amCleanHtml(paste(text))
  }else{
    textLog=text
  }

  textLog<-gsub('[\r\n]','',textLog)
  textLog<-gsub("\"","",textLog,fixed=T)
  textLog<-gsub("  ","",textLog)

  if(!type=="ui"){ 
    write(paste(amSysTime(),'\t',type,'\t',textLog,collapse=' '),file=logFile,append=TRUE)
  }
  if(type =='log')return(NULL)


  amUpdateModal(panelId='amModal',html=text,title=title,subtitle=subtitle,...)

}

# read only a subset of last lines
amReadLogs<-function(logFile,nToKeep=300){
  tryCatch({
    nMsg<-countLines(logFile)
    nToSkip<-nMsg-nToKeep
    read.csv(logFile,sep='\t', header=FALSE, skip=nToSkip,stringsAsFactors=F) 
  },error=function(c)amMsg(session,'error',c)
  )
}






# control if location is arleady took. Worth a new function ? only used in newLoc 
ifNewLocAvailable<-function(newLoc){
  if(newLoc %in% amGetGrassListLoc(grassDataBase) || amSubPunct(newLoc) %in% amGetGrassListLoc(grassDataBase)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}


# function to control input file extensions. 
# for each type and ext, write new rules here.
# file extension is given by file_ext (package tools) or grep command.
amValidateFileExt<-function(mapNames,mapType){
  #need access to am config
  stopifnot(exists('config'))
  # require validation vector in config files, e.g. shpExtMin
  mN <- basename(mapNames) # list of map names to be validated.
  mT <- mapType # vect or rast
  fE <- file_ext(mN) # list of file extension in map list
  # vector files
  if(mT=='vect'){
    # rule 1 : if it's a shapefile, it must have minimal set of  file extensions.
    if('shp' %in% fE){
      valid <- all(amSubPunct(config$fileShpExtMin,'') %in% fE)
      if(!valid){
        stop(paste(
            'Accessmod shapefile validation error:
            Trying to import invalid shapefile dataset.
            Minimum required file extensions are :',paste(config$fileShpExtMin,collapse=', ' )
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
      valid<-all(config$fileAdfMin %in% mN)
      if(!valid)stop(paste(
          "Accessmod esri binary grid validation:
          Trying to import invalid adf file dataset.
          Minimum required files are:",paste(config$fileAdfMin,collapse=', ')  
          ))
    }
    if('img' %in% fE){
      valid <- amSubPunct(fE) == amSubPunct(config$fileImgMin)  
      if(!valid)stop(paste("
          Accessmod ERDAS img file validation:
          Trying to import invalid file dataset.
          Required file extension is:", paste(config$fileImgMin)
          ))
    }
  }
}


amRastExists<-function(filter=''){
  if(amNoDataCheck(filter))return(FALSE)
  filter <- strsplit(filter,"@")[[1]][[1]]
  filter <- paste0(filter,'*')
  length(execGRASS('g.list',type='raster',pattern=filter,intern=TRUE))>0
}

amVectExists<-function(filter=''){
  if(amNoDataCheck(filter))return(FALSE)
  filter <- strsplit(filter,"@")[[1]][[1]]
  filter=paste0(filter,'*')
  length(execGRASS('g.list',type='vector',pattern=filter,intern=TRUE))>0
}

amMapExists <- function(map){
  res <- amNoMapset(map) %>%
  execGRASS("g.list",type=c("vector","raster"),pattern=.,intern=TRUE)
  isTRUE(length(res) > 0)
}

amRastIsEmpty <- function(rast){
  if(amRastExists(rast)){
    length(amGetRasterStat(rast,"sum"))==0
  }else{
    TRUE
  }
}


amVectIsEmpty <- function(vect){
  if(amVectExists(vect)){
    dat <- execGRASS("v.info",map=vect,flags="t",intern=T)
    dat <- amParseOptions(paste(dat,collapse=";"))
    all(sapply(dat,function(x){x==0||x=="0"}))
  }else{
    TRUE
  }
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


# functions to handle update



amRestart<-function(session=shiny:::getDefaultReactiveDomain()){
  system("touch restart.txt")
  session$sendCustomMessage(
    type="jsCode",
    list(code='location.reload();')
    )
}

amUpdateApp<-function(){
  defMsg = "Update accessmod. Do not reload the page now."
  progressBarControl(
    visible=TRUE,
    percent=0,
    title=defMsg,
    text="Merging new source code.",
    timeOut=2
    )
  system('git merge FETCH_HEAD')
  progressBarControl(
    visible=TRUE,
    percent=30,
    title=defMsg,
    text="Source merged. Check dependencies.",
    timeOut=2
    )

  if(! config$checkPointDate %in% list.files(config$checkPointPath)){
    progressBarControl(
      visible=TRUE,
      percent=50,
      title=defMsg,
      text="Install and update dependencies, this could take more than 30 minutes.",
      timeOut=2
      ) 
    system(sprintf("Rscript -e 'checkpoint::checkpoint(\"%s\")'",config$checkPointDate))
  }

  progressBarControl(
    visible=TRUE,
    percent=90,
    title=defMsg,
    text="Restarting AccessMod.",
    timeOut=2
    )

  amRestart()
}

amGetAppVersionLocal<-function(){
  system("git rev-parse --verify HEAD | awk '{print substr($0,1,7)}'",intern=T)
}

amGetAppVersionFetched<-function(){
  fetched <- system("git rev-parse --verify FETCH_HEAD | awk '{print substr($0,1,7)}'",intern=T)
  if(amNoDataCheck(fetched)) amGetAppVersionLocal() 
}

amGetAppCurrentBranch<-function(){
  system("git branch | grep '*' |awk '{ print $2}'",intern=T)
}

amGetAppCurrentTag <- function(){
  readLines("version.txt")
}


getSqlitePath<-function(sqliteExpr){
  # example of sqliteExpr: '$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db'
  system(paste("echo",sqliteDB),intern=T)
}


#' amPackageManager
#'
#' Manage package from within a shiny session : install or load if exists. 
#' This function display a progress bar on top of the shiny app if package is installed. 
#' 
#'
#' @param pkgCran vector of packages from CRAN
#' @param pkgLocal vector of packages from local archive directory
#' @param libPath path to R library
#' @param pathLocalPkg path to directory containing .tar.gz packages archives
#' @return none.
#' @export
amPackageManager<-function(pkgCran, pkgGit){
  # which package is missing ?
  pkgCranM <- pkgCran[!pkgCran %in% installed.packages()]
  pkgGitM <- pkgGit[!names(pkgGit) %in% installed.packages()]
  pkgCranL <- length(pkgCranM)
  pkgGitL <- length(pkgGitM)

  # isntall missing from CRAN
  if(pkgCranL>0){
    inc <- 1/pkgCranL
    msgUpdate<-'Updating CRAN packages'
    # with Progress use shiny::getDefaultReactiveDomain() as session object,
    # no need to provide one here.
    withProgress(message = msgUpdate, value = 0.1, {
      amMsg(session,'log',msgUpdate)
      for(p in pkgCranM){ 
        install.packages(pkgs=p, repos="http://cran.rstudio.com/")
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
        install_github(p)
        incProgress(inc,detail=p)
      }
    })
  }
  # load libraries. all at once or require inside function ? 
  # best practice seems inside function, but not sure if this method
  # is the most efficient. TODO: check this.
  lapply(pkgCran, require, character.only=TRUE)
  lapply(pkgLocal, require, character.only=TRUE)
}

#' R list to html
#' @param listInput list in inptu
#' @param htL List to append to
#' @param h Value of the first level of html header
#' @param exclude list named item to exclude
#' @export
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



#' Create a file name for export. 
#' @param dataName Name of the data, e.g. tExclusionOut__access_all
#' @param language Two letter language id, e.g. 'en'
#' @return name formated for export
#' @export
amGetNameConvertExport<- function(name,language="en"){
  class <- config$dataClass[config$dataClass$class==amGetClass(name),language]
  tags <- amGetTag(name,type="file") 
  type <- amGetType(name)
  amSubPunct(paste(type,class,paste(tags,collapse="_")))
}






amExportData<-function(
  dataName,
  dataNameOut,
  exportDir,
  type,
  formatVectorOut='shp',
  formatRasterOut='hfa',
  formatTableOut='csv',
  formatListOut='json',
  dbCon=NULL
  ){
  
  # grass data related report 
  reportName<-paste0(dataNameOut,'_report.txt')
  reportPath<-file.path(exportDir,reportName)
  infoName<-paste0(dataNameOut,'_info.txt')
  infoPath<-file.path(exportDir,infoName)

  # default export function for grass.
  # If other formats are requested, add other preformated command here.
  switch(type,
    'list'={
      lList = amGetListsList(pattern=sprintf("%s.json",dataName))
      if(length(lList)<1){
        msg = sprintf("Export of %s failed: original file not found.",dataName)
        stop(msg)
      }
     if(length(lList)>1){
        msg = sprintf("Oups, mulitple occurences found for %s",dataName)
        stop(msg)
      }
      fileName <- paste0(dataNameOut,".",formatListOut)
      fileOut <- file.path(exportDir,fileName)
      file.copy(lList[[dataName]],fileOut)
    },
    'shape'={
      allShpFiles <- amGetShapesList(pattern=sprintf("^%s",dataName))
      for(shpP in allShpFiles){
        sExt <- file_ext(shpP)
        newPath <- file.path(exportDir,paste0(dataNameOut,'.',sExt))
        file.copy(shpP,newPath) 
      }
    },
    'vector'={
      vInfo<-execGRASS('v.info',map=dataName,intern=TRUE)
      write(vInfo,infoPath)
      switch(formatVectorOut,
        'sqlite'={
          fileName<-paste0(dataNameOut,'.sqlite')
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
          fileName<-dataNameOut  # grass will export to a directory.
          filePath<-file.path(exportDir,fileName)
          if(filePath %in% list.dirs(exportDir))unlink(filePath,recursive=TRUE)
          execGRASS('v.out.ogr',
            input=dataName,
            output=exportDir,
            output_layer=dataNameOut,
            flags=c('overwrite'),
            format="ESRI_Shapefile",
            dsco="ADJUST_TYPE=YES"
            )
        }
        )
    },
    'raster'={
      #rInfo<-execGRASS('r.info',map=dataName,intern=TRUE)
      #write(rInfo,infoPath)
      execGRASS('r.report',map=dataName,units=c('k','p'), output=reportPath, flags='overwrite')
      switch(formatRasterOut,
        'tiff'={
          # tiff with UInt16 data (integer in 0-65535)
          # this could lead to lost of information. 
          fileName<-paste0(dataNameOut,'.GeoTIFF')
          reportPath<-paste0(dataNameOut,'_report.txt')
          #infoPath<-paste0(dataNameOut,'_info.txt')
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
          # note : with force flags, Integer could lead to data loss !
        },
        'hfa' = {
          # hfa
          fileName<-paste0(dataNameOut,'.img')
          reportPath<-paste0(dataNameOut,'_report.txt')
          #infoPath<-paste0(dataNameOut,'_info.txt')
          filePath<-file.path(exportDir,fileName)
          execGRASS('r.out.gdal',
            flags =c('overwrite','f','c'),
            input=dataName,
            output=filePath,
            format="HFA"
            )

        }

        ) 

      return(c(fileName,infoName,reportName))
    },
    'table'={
      fileName<-paste0(dataNameOut,'.xlsx')
      filePath<-file.path(exportDir,fileName)
      q<-paste('SELECT * FROM',dataName,';')
      tbl<-dbGetQuery(dbCon,q)
      rio::export(tbl,filePath)
    }
    )
}


triggerClientTime <- function(session=shiny::getDefaultReactiveDomain()){
  serverTime = Sys.time()
  serverTimeZone = as.integer(strftime(serverTime,"%z"))/100
  session$sendCustomMessage(
    type="getClientTime",
    message=list(
      serverPosix = as.numeric(serverTime),
      serverTimeZone = serverTimeZone
      )
    )
  #NOTE: Check if this is clean 
  httpuv:::service()
}

retrieveClientTime <- function(session=shiny::getDefaultReactiveDomain()){
  #NOTE: Check if this is clean 
  httpuv:::service()
  session$input$clientTime
}


getClientDateStamp <- function(){
  triggerClientTime()
  time <- retrieveClientTime() 
  test <- try(silent=T,
    date <- as.POSIXct(time$clientPosix,origin="1970-01-01")
    )
  #NOTE: sometimes, this fail. Probably because httpuv:::service trick
  if("try-error" %in% class(test)){
    date <- Sys.time()
  }
  amSubPunct(date)
}



#' Update text by id
#'
#' Search for given id and update content. 
#' 
#' @param session Shiny session
#' @param id Id of the element
#' @param text New text
#' @export
amUpdateText<-function(id,text=NULL,ui=NULL,addId=FALSE,session=shiny:::getDefaultReactiveDomain()){
  if(is.null(text) && is.null(ui)){
    return(NULL)
  }else{
    if(is.null(ui)){
      textb64 <- amEncode(text)
      val=list(
        id = id,
        txt = textb64,
        addId = addId
        )
      session$sendCustomMessage(
        type="updateText",
        val
        )
    }else{
      session$output[[id]] <- renderUI(ui)
    }
  }
}


#' encode in base64
amEncode <- function(text){
  base64enc::base64encode(charToRaw(as.character(text)))
}

amDecode <- function(base64text){
  rawToChar(base64enc::base64decode(base64text))
}







#
# link selected archive to a new window location. The clientshould as to download.
#TODO: as it's rendered in the same window, it could break shiny application, or reset it. Make sure that's not a problem with standard client. Works with webkit client.
amGetData<-function(session=shiny:::getDefaultReactiveDomain(),dataPath){
  if(!is.null(dataPath) && !dataPath==""){
    #val<-paste0("window.location.assign('",dataPath,"');")
    val<-paste0("downloadFile('",dataPath,"');")
    session$sendCustomMessage(
      type="jsCode",
      list(code=val)
      )
  }
}







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

#https://gist.github.com/xiaodaigh/6810928
# check if use of toggleClass could be a better choice.
amActionButtonToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
session$sendCustomMessage(
    type="btnDisable",
    list(
      id=id,
      disable=disable
      )
    )
}


amActionLinkToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
session$sendCustomMessage(
    type="linkDisable",
    list(
      id=id,
      disable=disable
      )
    )
}



amActionButtonWarningToggle <- function(session=shiny:::getDefaultReactiveDomain(),id,warning=TRUE) {
  addWarning<-paste0("$('#",id,"').addClass('btn-warning').removeClass('btn-default');")
  addDefault<-paste0("$('#",id,"').addClass('btn-default').removeClass('btn-warning');")

  val<-ifelse(warning,addWarning,addDefault)
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )
}







amFileInputUpdate<-function(id,session=shiny:::getDefaultReactiveDomain(),accepts=NULL,multiple=NULL){
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


#' amBusyManage 
#' Manually set or remove busy class to shiny.
#' @param session current shiny session
#' @param busy true/false
#'
#' @export
amBusyManage <- function(session=shiny:::getDefaultReactiveDomain(),busy=FALSE){
  stopifnot(is.logical(busy))
  if(busy){
    js="amAddBusy()"
  }else{
    js="amRemoveBusy()"
  }
  session$sendCustomMessage(type='jsCode',list(code=js))
}


#' Upload new data
#' @param config {list} am5 config list
#' @param dataName {string} data name
#' @param dataFile {path} data file path
#' @param dataClass {string} data class
#' @param dbCom {dbcon} db connection object
#' @param pBarTitle {string} progress bar title
#' @name upload_data
#' @export
amUploadTable<-function(config,dataName,dataFile,dataClass,dbCon,pBarTitle){
  
  tbl<- import(dataFile)

  if(!exists('tbl')){
    stop(paste('AccessMod could not read the provided file. Try another compatible format:',config$filesAccept$table))
  }

  progressBarControl(
    visible=TRUE,
    percent=30,
    title=pBarTitle,
    text="Data validation..."
    )
  # remove column containing NA's
  # search for expected column names
  aNames<-config$tableColNames[[dataClass]]

  if(is.null(aNames)) stop(paste('No entry found in config for class:',dataClass))

  # count remaining row
  hasRow <- nrow(tbl)>0

  if(!hasRow){
    stop(
      sprintf("Table %s doesn't have row(s).",dataName)
      )
  }

  tNames<-tolower(names(tbl))
  if(!all(aNames %in% tNames)){
    aNamesP <- paste(aNames,collapse='; ',sep=" ")
    tNamesP <- paste(tNames,collapse="; ",sep=" ")
    errMsg <- sprintf(
      "Importation of %s : dataset of class %s shoud contains columns named \n %s. Columns name of the provided file:\n %s",
      basename(dataFile),
      dataClass,
      aNamesP,
      tNamesP
      )
    stop(errMsg)
  }
  names(tbl)<-tNames
  tbl<-tbl[,aNames] # keep only needed columns


  progressBarControl(
    visible=TRUE,
    percent=90,
    title=pBarTitle,
    text="Writting in db..."
    )
  dbWriteTable(dbCon,dataName,tbl,overwrite=TRUE)
  amDebugMsg("Table",dataName," written in DB")
}




amErrHandler<-function(session=shiny:::getDefaultReactiveDomain(),errMsgTable,call,conditionMsg,title=NULL,type='warning'){
  #
  # in all case, return message as log.
  #
  textDefault <- tagList(
    tags$p(conditionMsg),
    tags$p("Call"),
    tags$p(call)
    )
  amMsg(
    session,
    type='log',
    text=textDefault,
    title=title
    )
  # try to find a registered simplified message to display in UI

  errMsg <- errMsgTable[
    conditionMsg == errMsgTable$cond
    ,]

  # replace original message
  if(nrow(errMsg)>0){
    for(i in 1:nrow(errMsg)){ 
      if(errMsg[i,'type']!="discarded"){
        amMsg(
          session,
          type=tolower(errMsg[i,'type']),
          text=errMsg[i,'text'],
          title=title
          )
      }
    }
    # if no match found in msg table, return 
    # original text and type found by amErrorAction
  }else{
    amMsg(
      session,
      type=type,
      text=textDefault,
      title=title
      ) 
  }
}





amErrorAction <- function(
  expr,
  errMsgTable=config$msgTableError,
  quotedActionError=NULL,
  quotedActionWarning=NULL,
  quotedActionMessage=NULL, 
  quotedActionFinally=NULL, 
  title,
  warningToLog = TRUE,
  messageToLog = TRUE,
  pBarFinalRm = TRUE,
  session=shiny:::getDefaultReactiveDomain()
  ){
  withCallingHandlers({
    tryCatch({
      expr
    },
    # error : stop process, eval error quoted function, return condition to amErrHandler
    error = function(cond){
      msg <- cond$message
      call <- paste(deparse(cond$call),collapse=" ")
      if(!is.null(quotedActionError))eval(quotedActionError)
      amErrHandler(session,errMsgTable,
        conditionMsg=msg,
        title=title,
        call=call,
        type='error'
        )

      if(pBarFinalRm){
        progressBarControl(percent=100)
      }
      return()
  })},
    # warning, don't stop process, but return condition to amErrHandler
    warning= function(cond){
      msg <- amSubQuote(cond$message)
      call <- paste(deparse(cond$call),collapse="")

      if(!is.null(quotedActionWarning))eval(quotedActionWarning)
      if(!warningToLog){       
        amErrHandler(session,errMsgTable,
          conditionMsg=msg,
          title=title,
          call=call,
          type='warning'
          )
      }else{
        amErrHandler(session,errMsgTable,
          conditionMsg=msg,
          title=title,
          call=call,
          type='log'
          )
      }

      return()
    },
    # simple message : don't stop, write in log. usa amMsg(type=message for a real message.)
    message= function(cond){

      msg <- amSubQuote(cond$message)
      if(is.null(quotedActionMessage))eval(quotedActionMessage)


      if(!messageToLog){
        amMsg(session,
          text=msg,
          title=title,
          type="message"
          )  
      }else{
        amMsg(session,
          text=msg,
          title=title,
          type="log"
          )
      }

      return()
    },
    finally={ 
      if(!is.null(quotedActionFinally)){
        eval(quotedActionFinally)
      }
    }
    )
}



amGetLocationProj<-function(){
  # ignore NADS grid ref.
  # NOTE: maybe not a good idea, but without it, we get this error
  # Error in .spTransform_Polygon(input[[i]], to_args = to_args, from_args = from_args,  :
  #  error in pj_transform: failed to load datum shift file
  projGrass<-getLocationProj(ignore.stderr=T)
  projGrassTest<-unlist(strsplit(projGrass," "))
  testNadsPos<-grep("+nadgrid",projGrassTest)
  if(length(testNadsPos)>0){ 
    projGrass<-paste(projGrassTest[-testNadsPos],collapse=" ")
  }
  return(projGrass)
}




#
#
# Upload raster
#

#config,dataName,dataFile,dataClass,dbCon
amUploadRaster <- function(config,dataInput,dataName,dataFiles,dataClass,pBarTitle){
  
  #
  # get map meta before importation
  #
  pMetaBefore <- amMapMeta()

  #dataInput=unique files or folder to give to gdal
  #dataName = name of output data
  #dataFile = actual list of files.

  pBarTitle = "Raster importation"

  progressBarControl(
    visible=TRUE,
    percent=10,
    title=pBarTitle,
    text="Data validation..."
    )

  amDebugMsg('Start processing raster ',dataName)
  # retrieve default color table by class
  tryReproj <- FALSE
  isDem <- isTRUE(dataClass == amGetClass(config$mapDem))
  currentMapset <- execGRASS('g.mapset',flags='p',intern=TRUE)

  #
  # If color table exist in dataClass
  #
  colorsTable <- config$dataClass[config$dataClass$class==dataClass,'colors']   

  if(!amNoDataCheck(colorsTable)){
    colConf<-as.list(strsplit(colorsTable,'&')[[1]])
    if(length(colConf)==2){
      cN<-c('color','flag')
    }else{
      cN<-c('color')
    }
    names(colConf)<-cN
  }

  # raster validation.
  amValidateFileExt(dataFiles,'rast')

  dMeta <- gdalinfo(dataInput, raw_output=F)
  dMeta$proj = as.character(gdalsrsinfo(dataInput,as.CRS=T))

  srsDest =  ifelse(
    tryReproj && !isDem,
    amGetLocationProj(),
    dMeta$proj
    )

  # temp img
  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.img'))
  # reprojection if needed
  gdalwarp(
    srcfile = dataInput,
    dstfile = tmpDataPath,
    t_srs = srsDest,
    of = "HFA",
    dstnodata = "-9999",
    output_Raster = FALSE,
    overwrite = TRUE
    )

  progressBarControl(
    visible=TRUE,
    percent=40,
    title=pBarTitle,
    text="Validation succeeded. Importation in database..."
    )

  if(file.exists(tmpDataPath)){
    
    if(isDem){
      dataName <- strsplit(config$mapDem,'@')[[1]][[1]]
      execGRASS('g.mapset',mapset="PERMANENT")
    }

    execGRASS(
      'r.in.gdal',
      input=tmpDataPath,
      output=dataName,
      flags=c('overwrite','quiet'),
      title=dataName
      )

    if(!amNoDataCheck(colorsTable)){

      amDebugMsg(paste('Set color table to',colConf$color,'with flag=',colConf$flag))

      execGRASS(
        'r.colors',
        map=dataName,
        flags=colConf$flag,
        color=colConf$color
        )
    }


    #
    # Reset project extent
    #
    if(isDem){

      progressBarControl(
        visible=TRUE,
        percent=80,
        title=pBarTitle,
        text="Set project resolution and extent based on new DEM"
        )

      execGRASS('g.mapset',mapset=currentMapset)

      execGRASS(
        'g.region',
        raster=config$mapDem
        )
    }

    #
    # Last progress bar info
    #
    progressBarControl(
      visible=TRUE,
      percent=90,
      title=pBarTitle,
      text="Importation succeeded... Cleaning..."
      )

  }else{
    #
    # Output file is not found
    #
    stop('Manage data: process aborded, due to unresolved CRS or not recognized input files. Please check files metadata and extent. Importation cancelled.')
  }

  #
  # clean files
  #
  file.remove(c(dataFiles, tmpDataPath))

  #
  # Set importation summary list
  #

  dMeta$nullCells = amGetRasterStat(dataName,metric="null_cells")

  pMetaAfter = amMapMeta()

  #
  # meta data about uploaded data and project
  #
  out <- list(
    projectBefore = list(
      resolution = list(
        y = pMetaBefore$grid$nsres,
        x = pMetaBefore$grid$ewres
        ),
      projection = pMetaBefore[[c('orig','proj')]]
      ),
    projectAfter = list(
      resolution = list(
        y =  pMetaAfter$grid$nsres,
        x = pMetaAfter$grid$ewres
        ),
      projection = pMetaAfter[[c('orig','proj')]]
      ),
    data = list(
      resolution = list(
        x = abs(dMeta$res.x),
        y = abs(dMeta$res.y)
        ),
      projection = dMeta$proj,
      numberOfNulls = dMeta$nullCells
      )
    )

  return(out)
}

amUploadNewProject<-function(newDem,newProjectName,pBarTitle){ 
  stopifnot(exists('config')) # need info from config list
  # capture all error from now, from potentially error prone steps.
  amDebugMsg(paste('importation process for',newProjectName,'started'))
  # TODO:
  # 1. check for better method for filre recognition
  # 2. This function requires a lot of external variable: check if those must be passed as argument instead.
  newDem<-newDem[with(newDem, order(-size)),]
  tmpDir<-dirname(newDem[1,'datapath'])
  newDem$newPath<-file.path(tmpDir,newDem$name)
  file.rename(newDem$datapath,newDem$newPath)
  # raster validation.
  amValidateFileExt(newDem$name,'rast')
  # take the first raster (heavier) as the base map
  tmpMapPath<-newDem[1,'newPath']
  # test for projection issues 
  r <- raster( tmpMapPath )
  destProj<-proj4string(r) 

  progressBarControl(
    visible=TRUE,
    percent=4,
    title=pBarTitle,
    text="Testing projection data"
    )

  if(amNoDataCheck(destProj)){
    stop(msgNoProj)
  }

  if(!length(grep('+to_meter|+units=m',destProj))>0){
    stop(
      "No metric parameter found. Please make sure that your data is projected in metric format."
      )
  }
  
  amDebugMsg(paste('Projection detected:',destProj));

  progressBarControl(
    visible=TRUE,
    percent=6,
    title=pBarTitle,
    text="Conversion in SpatialGrid"
    )

  # empty grid for the default WIND object
  sg<-as(r,'SpatialGrid')

  # grass initialisation.
  amDebugMsg('Init new grass session')

  progressBarControl(
    visible=TRUE,
    percent=10,
    title=pBarTitle,
    text="Init new project session"
    )

  unset.GIS_LOCK()
  unlink_.gislock()
  gHome<-file.path(tempdir(),newProjectName)
  dir.create(gHome,showWarnings=F)
  
  initGRASS(
    gisBase = config$pathGrassBase70, # binary files (grass 7.0)
    #home            = config$pathGrassHome, # where store lock file
    home            = gHome, # where store lock file
    gisDbase        = config$pathGrassDataBase, # local grass database
    location        = newProjectName, # rsession
    mapset          = 'PERMANENT', # PERMANENT for dem.
    SG              = sg, #spatial grid as templte for extent and res
    override        = TRUE
    )

  execGRASS('g.proj',flags='c',proj4=destProj)
  execGRASS('db.connect',driver='sqlite',database=config$pathSqliteDB)
  # set as default region
  amDebugMsg('Set grass environment and import DEM')
  execGRASS('g.gisenv',flags='s')
  amDebugMsg('tmpMapPath exists:',file.exists(tmpMapPath))

  progressBarControl(
    visible=TRUE,
    percent=30,
    title=pBarTitle,
    text="Importation in database"
    )

  execGRASS('r.in.gdal',
    input=tmpMapPath,
    output=config$mapDem,
    flags=c('overwrite','quiet'),
    title=paste(newProjectName,'DEM')
    )

  execGRASS("g.region",
    raster = config$mapDem
    )

  progressBarControl(
    visible=TRUE,
    percent=50,
    title=pBarTitle,
    text="Filter values"
    )

  execGRASS(
    'r.mapcalc',
    expression = sprintf("%1$s = if(isnull( %1$s ), 0, %1$s)",amNoMapset(config$mapDem)),
    flags="overwrite"
    )

  progressBarControl(
    visible=TRUE,
    percent=80,
    title=pBarTitle,
    text="Importation done. Cleaning and testing... "
    )

  execGRASS(
    'r.colors',
    map=config$mapDem,
    color='elevation'
    )

  unset.GIS_LOCK()
  unlink_.gislock()
  
  file.remove(tmpMapPath)

}


#
# Upload vectors
#
#
amUploadVector<-function(dataInput, dataName, dataFiles, pBarTitle){

  # TODO: validate extent 

  tryReproj=TRUE
  # helper function to validate file based on extension

  progressBarControl(
    visible=TRUE,
    percent=20,
    title=pBarTitle,
    text="Attributes validation and cleaning"
    )


  amValidateFileExt(dataFiles,'vect')
  origShpFilePath <- dataFiles[grepl(".shp$",dataFiles)]
  origDbfFilePath <- dataFiles[grepl(".dbf$",dataFiles)]
  origCpgFilePath <- dataFiles[grepl(".cpg$",dataFiles)]
  origShpBaseName <- basename(substr(origShpFilePath,0,nchar(origShpFilePath)-4))

  tmpDataBase <- file.path(tempdir(),paste0(dataName,'.dbf'))
  tmpDirShape <- file.path(tempdir(),paste(dataName))
  tmpDataPath <- file.path(tmpDirShape,paste0(dataName,".shp"))

  encoding <- "ISO8859-1"


  #
  # Data cleaning :
  #   Remove old cat_ column for from old version of accessmod
  #   Update custom key (e.g. cat by default) with unique id 
  #   Replace columnn of type date (bug with sqlite and grass) by column of type string
  #   Write spatial with correct encoding. (ogr fails to read cpg file in GDAL 1.11.3, grass produce invalid char)
  # 
  projDest <- sp::CRS(amGetLocationProj())


  origData <- import(origDbfFilePath)

  # remove old cat or cat_ column
  origData <- subset(origData,select=!names(origData)%in%c("cat_"))
  # add key column
  
  origData[,config$vectorKey] <- 1L:nrow(origData)

  # issue with dates #157
  posDate <- grep("[dD]ate",sapply(origData, class))
  if(length(posDate)>0){
    for(i in posDate){
     origData[,i]<-as.character(origData[,i])
    }
  }

  export(origData,origDbfFilePath)

  progressBarControl(
    visible=TRUE,
    percent=20,
    title=pBarTitle,
    text="Cleaned file written, upload in database"
    )


  if(!amNoDataCheck(origCpgFilePath)){
    encoding <- readLines(origCpgFilePath,warn=F) 
  }
  
  dir.create(tmpDirShape)

  ogr2ogr(
    src_datasource_name = origShpFilePath,
    dst_datasource_name = tmpDataPath,
    f="ESRI Shapefile",
    t_srs = projDest,
    overwrite=TRUE,
    verbose=TRUE
    )

  execGRASS("v.in.ogr",
    flags=c("overwrite","w","2"), # overwrite, lowercase, 2d only,
    parameters=list(
      input=tmpDataPath,
      key=config$vectorKey,
      output=dataName,
      snap=0.0001
      )
    )

  unlink(dataFiles)
  unlink(tmpDirShape)
  return(NULL)
}

amUpdateDataList<-function(listen){
  amDebugMsg('update data list')
  listen$dataListUpdate<-runif(1)
}

amDebugToJs<-function(text,session=getDefaultReactiveDomain()){
  js <- jsonlite::toJSON(text)
  session$sendCustomMessage(
    type="jsDebug",
    list(code=js)
    )
}


amDebugMsg<-function(...){
  cat(paste('{ debug',amSysTime(),'}',...),sep='\n')

}


amMapMeta<-function(){
  #TODO: use one grid list, name this after
  meta<-list()
  gL<-gmeta()
  meta$location<-gL$LOCATION_NAME
  projGrass<-amGetLocationProj()
  proj<-list(
    orig=projGrass,
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

  grid <- gL[names(gL)%in%c('nsres','ewres','rows','cols','cells')]
  meta$grid <- lapply(grid,as.numeric)

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
  #bbx<-fromJSON(geojson_json(bbx)[[1]])
  bbx<-geojson_list(bbx)
  worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
  bbxCoord<-bbx$features[[1]]$geometry$coordinates[[1]]
  bbx$features[[1]]$geometry$coordinates<-list(worldCoord,bbxCoord)
  bbx$style<-bbxStyle
  return(bbx)
}

# extract geojson from mapMeta
amSpotlightGeoJson<-function(mapToPreview){
  bbx<-as(extent(mapMeta[[proj]]$bbx$ext),"SpatialPolygons")
  bbxStyle<-list(
    fillColor = "black",
    fillOpacity = 0.5,
    opacity=0.1,
    weight = 1,
    color = "#000000"
    )
  bbx<-geojson_list(bbx)
  worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
  bbxCoord<-bbx$features[[1]]$geometry$coordinates[[1]]
  bbx$features[[1]]$geometry$coordinates<-list(worldCoord,bbxCoord)
  bbx$style<-bbxStyle
  return(bbx)
}




# find  one cell diagonal bridge between multiple raster maps (e.g. road) and destination map (e.g. merged lcv)
# warning : only tested from rasterized lines with densified option. 
amBridgeFinder<-function(fromMap,toMap,bridgeMap){
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
  if(!amNoDataCheck(nBridges) || isTRUE(nBridges>0)){
    amDebugMsg(paste(
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
  amDebugMsg(paste('Bridges from',bridgeMap,'removed from',removeFromMap))
}

#https://gist.github.com/jmarhee/8530768
amMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}






amNameCheck<-function(dataList,name,class=c('vector','raster','table'),sepMap=config$sepMapset,dbCon=NULL){
  class=match.arg(class)
  name<-as.character(name)
  nameNoMapset<-unlist(strsplit(name,paste0("(",sepMap,").+")))
  if(length(nameNoMapset)==0)return(NULL)
  if(class=='table'){
    if(all(nameNoMapset %in% dbListTables(dbCon))){
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
amNewName<-function(class,tags,sepClass=config$sepClass,sepTag=config$sepTagFile){
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


amTagsFileToDisplay<-function(fileN,sepClass=config$sepClass,sepTag=config$sepTagFile){
  cla=amClassListInfo(unlist(strsplit(fileN,paste0("\\",sepClass)))[[1]])
  tag=unlist(strsplit(fileN,paste0("\\",sepClass)))[[2]]
  tag=paste0("[",gsub(sepTag,' ',tag),"]")
  paste(cla,tag)
}



# create list usable to populate select input
# Here, we want th make sure that each project will contain unique set of value for its input.
# We append the name of the mapset(project) to each name and create a user-friendly version to display in ui.
amCreateSelectList<-function(dName,sepTag=config$sepTagUi,sepClass=config$sepClass,sepMap=config$sepMapset,mapset){
  amErrorAction(title='amCreateSelectList',{
    if(length(dName)==0)return(NULL)
    # add mapset at the end of each data name
    # ex. cumulative_cost__test -> cumulative_cost__test@burkina
    l=as.list(paste0(dName,sepMap,mapset))    
    lN=character(0)
    err=character(0)
    for(n in dName){
      # test if data name containe class separator, else flag as err
      if(isTRUE(length(grep(config$sepClass,n))>0)){
        dat=unlist(strsplit(n,sepMap))[[1]]
        vals=unlist(strsplit(dat,paste0("\\",sepClass)))
        if(length(vals)==2){
          displayName <- amTagsFileToDisplay(dat)
          lN <- c(lN,displayName)
        }else{
          lN<-c(lN,NA)
          err=c(err,n)
        }
      }else{
        lN<-c(lN,NA)
        err=c(err,n)
      }
    }
    if(length(err)>1){ 
      warning(
        paste(" Some data name from project",mapset,"(",
          paste(err,collapse=','),
          ") doesn't match AccessMod naming convention and will be ignored."))
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
#amGetUniqueTag<-function(x,sepIn,sepOut,ordered=TRUE){
#  #x =string containing tags : ex. test+super+super
#  #sepIn separator in input string  e.g. +
#  #sepOut separator in output string  e.g. _
#  if(length(x)==1){
#    x<-amSubPunct(x,sep=sepIn)
#    x<-t(read.table(text=x,sep=sepIn))[,1]
#    if(ordered==TRUE){
#      x<-x[order(x)]
#    }
#    return(paste0(na.omit(unique(x)),collapse=sepOut))
#  }else{
#    stop('getUniqueTagString: length of input not 1 ')
#  }
#}
# return : unique ordered tag e.g. super_test instead of test+super+super



#' amGetUniqueTag
#' get unique tags from string
#' @param x string containing tags. Ex. "new new; myTag2"
#' @param sorted should the resulting vector be sorted?
#' @return  vector of unique tags.
#' @export
amGetUniqueTags<-function(x,ordered=FALSE){
  if(amNoDataCheck(x)) return()
  if(length(x)>1) x=unlist(x)
  x <- paste(x)
  x <- amSubPunct(x,sep=';')
  x <- unique(unlist(strsplit(x,';')))
  if(ordered==TRUE){
    x<-x[order(x)]
  }
  return(x)
}





#
#
#
#
## get all available tags from a list
#amGetUniqueTags<-function(amData){
#  if(is.list(amData))amData<-names(amData)
#  unique(unlist(strsplit(unlist(amData),'.(\\[)|(\\])|(\\+)|.(@)|(,)')))
#}
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
amNoMapset<-function(amData,sepMap=config$sepMapset){
  amData<-as.character(amData)
  res<-unlist(strsplit(amData,paste0("(",sepMap,").+")))
  if(length(res)==0)res=NULL
  res
}

# Get data class
#' @param amData Data name to evaluate
#' @param sepClass Class separator
#' @return Class name for given data
#' @export
amGetClass<-function(amData=NULL,sepClass=config$sepClass){
  if(amNoDataCheck(amData))return()
  as.character(strsplit(unlist(amData),paste0('(\\',sepClass,').*')))
}



#' Get type of data for given layer
#' @param amData Data name to evaluate
#' @param config Accessmod configuration list 
#' @return Type of data (vector, raster, table..)
#' @export
amGetType <- function(amData=NULL){
  if(amNoDataCheck(amData))return()
  if(isTRUE(amData==config$newFacilitiesShort))return('vector')
  class <- amGetClass(amData,config$sepClass)
  type <- config$dataClass[config$dataClass$class==class,'type']
  return(type)
}




# amGetClass(dList$raster)
# return : 
# [1] "land_cover_table" "land_cover"       "population"
#

# get tag of data
amGetTag<-function(amData,type="ui"){
  if(type=="ui"){
    if(is.list(amData))amData<-names(amData)
    tmp<-gsub(".+(?=\\[)|(\\[)|(\\])","",amData,perl=T)
    tmp<-gsub("\\_"," ",tmp)
    tmp
  }else{
    tag<-unlist(strsplit(amData,paste0("\\",config$sepClass)))[[2]]
    tag<-unlist(strsplit(tag,config$sepTagFile))
  }
}
# amGetTag(dList$rast)
# return :
# [1] "super super" "super super" "super new" 





# create data.frame version of dataList
# amDataList = vector of dataSet
# sepClass = class separator (double dash)
# type = type attribute in resulting data.frame
amDataListToDf<-function(amDataList,sepClass,type='raster'){
  if(is.null(amDataList)||length(amDataList)<1)return(NULL)
  cla=amGetClass(amDataList,sep=sepClass)
  tag=amGetTag(amDataList)
  name=amNoMapset(amDataList)
  display=names(amDataList)
  displayCla=amClassListInfo(cla)
  data.frame(
    class=cla,
    tags=tag,
    type=type,
    searchCol=paste(type,displayCla,cla,tag),
    origName=name,
    displayName=display,
    displayClass=displayCla
    )
}




# Create a subset of the data frame.
amDataSubset<-function(pattern='',type=NULL,amDataFrame){
  if(nchar(pattern)>0){    
    pattern=amSubPunct(pattern,'|')
    tbl<-amDataFrame[grep(pattern,amDataFrame$searchCol),]
  }else{
    tbl<-amDataFrame
  }  
  if(!is.null(type))tbl<-tbl[tbl$type %in% type,]
  tbl
}


#amGetTag<-function(amData){
#  if(is.list(amData))amData<-names(amData)
#  tmp<-gsub(".+(?=\\[)|(\\[)|(\\])","",amData,perl=T)
#  tmp<-gsub("\\_"," ",tmp)
#  tmp
#}


#' amSubQuote
#' 
#' Remove simple and double quote and newlines. This can be usefull in message
#' send to javascript functions, popup message, etc.. For complete removal of
#' non ascii character, use amSubPunct
#'
#'@param txt character vector
#'@export
amSubQuote<-function(txt){
  txt<-gsub("\""," ",txt)
  txt<-gsub("\'"," ",txt)
  txt<-gsub("\n"," ",txt)
  return(txt)
}

#' amSubPunct
#' remove all unwanted characters, remplace by sep of choice 
#' @param vect character vector
#' @param rmTrailingSep remove unwanted trailing replacement separator
#' @param rmLeadingSep remove unwanted leanding replacement  separator
#' @param rmDuplicateSep remove duplicated replacement separator
#' @export
amSubPunct<-function(vect,sep='_',rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T,debug=F){
  vect<-gsub("'",'',iconv(vect, to='ASCII//TRANSLIT'))
  res<-gsub("[[:punct:]]+|[[:blank:]]+",sep,vect)#replace punctuation by sep
  res<-gsub("\n","",res)
  if(rmDuplicateSep){
    if(nchar(sep)>0){
      res<-gsub(paste0("(\\",sep,")+"),sep,res)# avoid duplicate
    }
  }
  if(rmLeadingSep){
    if(nchar(sep)>0){
      res<-gsub(paste0("^",sep),"",res)# remove trailing sep.
    }
  }
  if(rmTrailingSep){
    if(nchar(sep)>0){
      res<-gsub(paste0(sep,"$"),"",res)# remove trailing sep.
    }
  }
  res
}
# example :
# amSubPunct('hérétique:crasy#namer*ßuss','_')
# [1] "heretique_crasy_namer_ssuss"


#' amUpdateDataListName
#'
#' Update GRASS raster/vector name and SQLITE table name,
#' based on modified tags field in data list. This function expect
#' a working GRASS environment and an accessmod config list.
#' 
#' @param dataListOrig table with columns: "type displayClass tags origName class" .
#' @param dataListUpdate table with columns: "ttype displayClass tags origName class". If it contains modified tags value, origName is set as old name, new name is formed based on class and tags.
#' @param dbCon: path to sqlite db
#'
# @export
amUpdateDataListName<-function(dataListOrig,dataListUpdate,dbCon,config){
  if(!is.null(dataListOrig) && !is.null(dataListUpdate)){
    tblO <- dataListOrig
    tblU <- dataListUpdate
    tblO[] <- lapply(tblO,as.character)
    tblU[] <- lapply(tblU,as.character)
    # test for empty or incorrect table
    if(any(sapply(tblU,function(x)isTRUE( amNoDataCheck(x) || x=='-' )))){
      stop('Rename data : there is NA, missing char or "-" in update table')
    }else{
      # search for new tags
      tblM <- anti_join(tblU,tblO)
      if( !isTRUE(nrow(tblM)>0) ) return(FALSE)
      #  rename and get a list of changes
      msgs <- apply(tblM,1,function(x){
        # if not class DEM
        if(!x['class'] == amGetClass(config$mapDem)){
          amRenameData(
            type  = x['type'],
            new   = amNewName(x['class'],x['tags']),
            old   = x['origName'],
            dbCon = dbCon
            )
        }
    })

      #send a msg to ui
      uiMsg <- tags$div(style="max-height:300px;overflow-y:scroll;",
        tags$ul(
          HTML(paste("<li>",msgs,"</li>"))
          )
        )
      amMsg(type="ui",title="Rename",subtitle="Result",text=uiMsg)
      return(TRUE)
    }
  }
  return(FALSE)
}

#' amRenameData
#'
#' Function to handle data renaming in GRASS and SQLite database, if data exists.
#'
#' @param type raster,vector or table
#' @param old old name
#' @param new new name
#' @param dbCon RSQLite database connection
#' @param session Shiny session
#'
#' @export
amRenameData<-function(type,old="",new="",dbCon=NULL,session=getDefaultReactiveDomain()){
  if(!type %in% c('raster','vector','table','shape','list') || old==""||new=="")return()
  msgRename=""
  renameOk=FALSE

  switch(type,
    'raster'={
      rL<-execGRASS('g.list',type='raster',intern=T)
      if(!tolower(new) %in% tolower(rL) && old %in% rL){
        execGRASS('g.rename',raster=paste(old,new,sep=','))
        renameOk=TRUE
      }else{
        renameOk=FALSE
      }
    },
    'vector'={
      vL<-execGRASS('g.list',type='vector',intern=T)
      if(!tolower(new) %in% tolower(vL) && old %in% vL) {
        execGRASS('g.rename',vector=paste(old,new,sep=','))
        renameOk=TRUE
      }else{ 
        renameOk=FALSE
      }
    },
    'table'={
      if(is.null(dbCon))return()
      tL<-dbListTables(dbCon)
      if(!tolower(new) %in% tolower(tL) && old %in% tL){
        dbGetQuery(dbCon,paste("ALTER TABLE",old,"RENAME TO",new))
        renameOk=TRUE
      }else{ 
        renameOk=FALSE
      }
    },
    'shape'={
      sL<-amGetShapesList()
        pathShapes <- system(sprintf("echo %s", config$pathShapes),intern=T)
      if(!tolower(new) %in% tolower(names(sL)) && old %in% names(sL)){
        # sL did not return all related files to this layer : get these.
        allShpFiles <- amGetShapesList(pattern=sprintf('^%s\\.',old))
        # sorry for this.
        for( s in allShpFiles){
          sExt <- file_ext(s)
          newPath <- file.path(pathShapes,paste0(new,'.',sExt))
          file.rename(s,newPath) 
        }
        renameOk=TRUE
      }else{
        renameOk=FALSE
      }
    },
    'list'={
      lL <- amGetListsList()
      jsonPath <- system(sprintf("echo %s", config$pathList),intern=T)
      if(!tolower(new) %in% tolower(names(lL)) && old %in% names(lL)){
          newName <- file.path(jsonPath,sprintf("%s.json",new))
          oldName <- lL[[old]]
          file.rename(oldName,newName) 
        renameOk<-TRUE
      }else{
        renameOk=FALSE
      }
    }
    )

  if(renameOk){
    msg <- paste("Renamed",old,"to",new,".")
  }else{
    msg <- paste("Rename",old,"to",new," not necessary. Duplicated tags, empty tags or already existing name.")
  }

  return(msg)


}

########### SECTION GIS MODULES
amRastQueryByLatLong<-function(coord,rasterName,projOrig,projDest){
  coord<-SpatialPoints(data.frame(coord['x'],coord['y']))
  #proj4string(coord)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
  proj4string(coord)<-projDest
  #coord<-spTransform(coord,CRS(getLocationProj()))@bbox[,'max']
  coord<-spTransform(coord,CRS(projOrig))@bbox[,'max']
  suppressWarnings({
    val<-execGRASS('r.what',map=rasterName,coordinates=c(coord[1],coord[2]),flags=c('c','quiet','f'),intern=T) 
  })
  val<-read.table(text=val,sep="|",stringsAsFactors=F)
  val[is.na(val)]<-'-'
  names(val)<-c('long','lat','lab','value','cat label')
  val$value<-val$value
  val$lab<-NULL
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
amGrassLatLongPreview<-function(
  mapToPreview=NULL, # map name to preview. ex. land_cover
  bbxSpLatLongLeaf, # bbx sp object with current region in lat/long (bbxLeafToSp(input$<map>_bounds))
  bbxSpLatLongOrig, # bbx sp object with current region in projected format
  mapCacheDir, # relative path to cache directory eg. ../data/cache. Must exists
  resGrassEW, # grass resolution for east-west. NOTE: could be extracted from "g.region -m | grep EW"
  resMax, # maximum resolution of final file.
  projOrig,
  projDest
  ){
  toc<-function(...){
    if(exists('toc')){
      start=tic
      time=Sys.time()
      diff<-time-start
      amDebugMsg(paste(as.character(...),diff))
      diff
    }
  }
  tic<-Sys.time()
  # var naming convention for bounding boxes. NOTE: put everything in a list instead?
  # bbx<class><projection><label>
  # class : sp, vector, matrix
  # projection : Latitude Longitude, projected
  # label : leaflet, intersection, original
  if(!is.null(bbxSpLatLongLeaf) && !is.null(bbxSpLatLongOrig)){

    amDebugMsg('retrieve map from grass to create png file in lat long ')
    # define bounding box intersection.
    #get intersection betweed leaflet extent and project extent
    bbxSpLatLongInter<-gIntersection(bbxSpLatLongOrig,bbxSpLatLongLeaf)
    if(is.null(bbxSpLatLongInter))return(NULL)
    bbxMatLatLongInter<-bbxSpLatLongInter@bbox
    # to avoid to much cache files, round bbx values.
    # NOTE: if rendering time is short, skip this process ?
    bbxMatLatLongInterRound<-round(bbxMatLatLongInter,10)
    # file names
    cacheMap <- file.path(mapCacheDir,paste0(mapToPreview,"__",paste0(bbxMatLatLongInterRound,collapse="_"),'.png'))
    cacheLegend <- file.path(mapCacheDir,paste0("legend_",mapToPreview,'.png'))
    # don't evaluate if map is already in cache.
    if(!file.exists(cacheMap)){
      rmRastIfExists('MASK*')
      rmRastIfExists('tmp_*')
      rmVectIfExists('tmp_*')

      #create sp object with computed intersection extent and transform to grass orig projection
      bbxSpProjInter<-spTransform(bbxSpLatLongInter,CRS(projOrig))
      #get resulting bbx
      bbxMatProjInter<-bbxSpProjInter@bbox
      #settting resolution. 
      resOverlay<-diff(bbxMatProjInter['x',])/resMax # max x resolution. Leaflet map is 800px, so..
      #resGrassNS<-metaOrig$summary$North
      res=ifelse(resOverlay>resGrassEW,resOverlay,resGrassEW)
      #toc('start g.region')
      execGRASS('g.region',
        e=paste(bbxMatProjInter['x','max']),
        w=paste(bbxMatProjInter['x','min']),
        n=paste(bbxMatProjInter['y','max']),
        s=paste(bbxMatProjInter['y','min']),
        res=paste(resOverlay) 
        )
      #toc('end g.region, start create mask from region')
      execGRASS('v.in.region',output='tmp_mask')
      execGRASS('r.mask',vector='tmp_mask')
      # compute preview map
      #toc('start resampling at the new resolution')
      #toc('end mapcalc, start r.out.png')
      # export in png with transparency and remove mask
      execGRASS('r.out.png',input=mapToPreview, output=cacheMap,flags=c('overwrite','w','t')) # with world file

      #
      # !!! NOTE: d.mon is not available on the grass version in the VM
      #
      #  if(!file.exists(cacheLegend)){
      #suppressWarnings({
      #execGRASS('d.mon',
      #start='png',
      #output=cacheLegend,
      #height=300,
      #width=300
      #)
      #execGRASS('d.legend',raster=mapToPreview,at=c(0,100,0,10),flags=c("f","d"))
      #execGRASS('d.mon',
      #stop='png'
      #)
      #})
      #   }
      # set back the grass resgion to dem values.
      #toc('end r.out.png, start g.region')
      execGRASS('g.region', raster=config$mapDem)
      #toc('stop g.region, cleaning temp map')
      rmRastIfExists('MASK*')
      rmRastIfExists('tmp_*')
      rmVectIfExists('tmp_*')
    }
    amDebugMsg('retrieving done. in ',format(toc(),units='s'))
    return(list(
        pngMap=cacheMap,
        pngLegend=cacheLegend,
        bbx=bbxMatLatLongInter
        ))   
  }
}

####### SECTION accessiblity analysis

# function extract field summary from SQLite table :
# - numeric fields,
# - character fields,
# - index candidate : (unique values & only character and integer & n=nRow )
# - uniques values by fields
amGetFieldsSummary<-function( table, dbCon, getUniqueVal=T ){

  stopifnot(table %in% dbListTables(dbCon))

  # get full table
  tblSample<-dbGetQuery(
    dbCon,
    sprintf("SELECT * FROM %1$s %2$s",
      table,
      ifelse(getUniqueVal,"","LIMIT 1000")
      )
    )
  # number of row
  nR<-nrow(tblSample)

  # get possible index columns
  idxCandidate <- sapply(tblSample,function(x){
    isTRUE(length(unique(x))==nR)
    })

  # Extract unique value
  if(getUniqueVal){
    uniqueVal <- lapply(tblSample,function(x){
      x=unique(x)
      sort(x)
    })
  }else{
    uniqueVal<-NULL
  }

  # get index column name
  idxFields<-names(idxCandidate)[idxCandidate]

  # get numeric field 
  numFields<-sapply(tblSample,function(x){
    isNum<-is.numeric(x) && !is.logical(x)
    if(isNum){
      !any( sapply(x,amNoDataCheck) )
    }else{
      FALSE
    }}) %>% 
  names(tblSample)[.]

#  get integer fields
intFields <- sapply(tblSample,function(x){
  isInt<-is.integer(x) && !is.logical(x)
  if(isInt){
    !any( sapply(x,amNoDataCheck) )
  }else{
    FALSE
    }}) %>% 
names(tblSample)[.]

 #  get character fields
  charFields<-sapply(tblSample,function(x){
    isChar <- is.character(x) && !is.logical(x)
    if(isChar){
      !any( sapply(x,amNoDataCheck) )
    }else{
      FALSE
    }}) %>% 
names(tblSample)[.]

  # return summary
 list(
   int = intFields,
   num = numFields,
   char = charFields,
   idx = idxFields,
   val = uniqueVal
   )
}


amCreateHfTable<-function(mapHf,mapMerged,mapPop,dbCon){
  # mapHf : vector map of facilities
  # map merged : raster landcover merged map
  # mapPop : raster map of population
  # Return value :
  # Facilitie attribute table with additional columns :
  # amOnBarrier : check if facilities is located on barrier (no landcover value)
  # amCatLandCover : get value of merged land cover for each facilities.
  # amPopCell : count population in cells where facilities are located.
  if(!is.null(mapHf) && !is.null(mapMerged)){
    # check if HF are located on barrier by querying merged land cover values.
    tbl<-read.table(
      text=execGRASS("v.what.rast",map=mapHf,raster=mapMerged,flags='p',intern=T),
      sep="|",stringsAsFactors=F)
    names(tbl)<-c('cat','val')
    tbl$amCatLandCover<-ifelse(tbl$val=='*',NA,tbl$val)
    tbl$amOnBarrier<-ifelse(tbl$val=='*',TRUE,FALSE)
    tbl$val<-NULL
    # count population on facilities sites
    if(!is.null(mapPop)){
      pop<-read.table(
        text=execGRASS('v.what.rast',map=mapHf,raster=mapPop,flags='p',intern=T),
        sep="|",stringsAsFactors=F)  
      names(pop)<-c('cat','amPopCell')
      pop[pop$amPopCell=='*','amPopCell']<-0 
      pop$amPopCell<-as.numeric(pop$amPopCell)
      tbl<-merge(tbl,pop,by='cat')
    }
    # copy hf attribute table from SQLite db.
    tblAttribute<-dbGetQuery(dbCon,paste('select * from',mapHf))
    # merge accessmod table with attribute table
    tbl<-merge(tbl,tblAttribute,by='cat')

    return(tbl)
  }else{
    return(NULL)
  }

}


#' amMapPopOnBarrier
#' 
#' Ask grass if there is population located on barrier (null cells)
#' @param inputPop population layer name
#' @param inputMerged merged landcover layer name
#' @param outputMap output layer name containing cells on barrier
#' @export
amMapPopOnBarrier<-function(inputPop,inputMerged,outputMap){
  expr<-sprintf("%s = if(!isnull(%s) && isnull(%s),%s,null())",outputMap,inputPop,inputMerged,inputPop)
  execGRASS('r.mapcalc',expression=expr,flags='overwrite')
}


#' clean travel time map
#' @param map Raster travel time map
#' @param maxCost Number. Maximum cost/travel time in minutes
#' @param minCost Number. Minium cost/travel time in minutes
#' @param convertToMinutes Boolean. Convert the cleaned map to minutes
amCleanTravelTime<-function(map,maxCost,minCost=NULL,convertToMinutes=TRUE){
  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.
  

  stopifnot(!amNoDataCheck(maxCost))

  maxCost <- maxCost * 60

  if(amNoDataCheck(minCost)){
    minCost <- 0 
  }else{
    minCost <- minCost * 60
  }

  if( maxCost > 0 ){

    cleanMaxCost <- sprintf(
      " %1$s = if(%1$s <= %2$s, %1$s, null() ) ",
      map,
      maxCost
      ) 

    execGRASS('r.mapcalc',expression=cleanMaxCost,flags=c('overwrite'))
  }
  if( minCost < maxCost || maxCost==0 ){
    cleanMinCost <- sprintf(
      " %1$s = if(%1$s >= %2$s, %1$s, null() )",
      map,
      minCost
      ) 
    execGRASS('r.mapcalc',expression=cleanMinCost,flags=c('overwrite'))
  }
  if(convertToMinutes){
    exp = sprintf(
      "%1$s = %1$s/60",
      map
      )
    execGRASS('r.mapcalc',expression=exp,flags=c('overwrite'))
  }
}

#'amCreateSpeedMap
#'
#' @export
amCreateSpeedMap<-function(tbl,mapMerged,mapSpeed){
  # creation of new classes for speed map (class+km/h), used in r.walk.accessmod
  # Exemples of rules: 
  # oldClasses = newClasses \t newlabels
  # 1 2 3 = 1002 \t WALKING:2
  # 4 =  2020 \t BICYCLING:20
  # 1002 = 3080 \t MOTORIZED:80
  tbl[,'newClass']<-integer()
  # for each row of the model table...
  for(i in 1:nrow(tbl)){
    #... get the mode
    mod<-tbl[i,'mode']
    #... corrsponding to the predefined value listTranspMod + given speed
    tbl[i,'newClass']<-(as.integer(config$listTranspMod[[mod]]$rastVal)+tbl[i,'speed'])*1000
  }
  # unique new class
  uniqueNewClass<-unique(tbl$newClass)
  reclassRules<-character()
  for(u in uniqueNewClass){
    oldClasses<-tbl[tbl$newClass==u,'class']
    modeSpeedLabel<-paste(tbl[tbl$newClass==u,c('mode','speed')][1,],collapse=':')
    classRule<-paste(paste(oldClasses,collapse=' '),'=',u,'\t',modeSpeedLabel)
    reclassRules<-c(reclassRules,classRule)
  }
  tmpFile<-tempfile()
  write(reclassRules,tmpFile)
  execGRASS('r.reclass',
    input=mapMerged,
    output=mapSpeed,
    #output=mapSpeed,
    rules=tmpFile,
    flags='overwrite')

  #  exp=paste(mapSpeed,'=float(tmp__speed)/1000')
  #  execGRASS('r.mapcalc',
  #    expression=exp,
  #    flags='overwrite')
  #
  #

}

#'amCreateFrictionMap
#'
#'@export
amCreateFrictionMap<-function(tbl,mapMerged,mapFriction,mapResol){
  amDebugMsg('amCreateFrictionMap')

  # creaction of new classes for cost map (seconds) used in r.cost. 
  tbl[,'newClass']<-numeric()
  tbl[,'mode']<-'isotropic'
  # for each row of the model table...
  for(i in 1:nrow(tbl)){
    # km/h to s/m 
    # the time to cover one unit of distance * actual distance (map resolution) == cost to cross a given cell. 
    tbl[i,'newClass']<- (1/(tbl[i,'speed']/3.6))*mapResol
  }

  # unique new class
  uniqueNewClass<-unique(tbl$newClass)
  reclassRules<-character()
  categoryRules<-character()


  for(u in uniqueNewClass){
    oldClasses<-tbl[tbl$newClass==u,'class']
    reclassRule<-paste0(oldClasses,':',oldClasses,':',u,':',u)
    reclassRules<-c(reclassRules,reclassRule)
    catLabel<-paste(
      paste(tbl[tbl$newClass==u,]$label,collapse='/'),
      u,'[s]/',mapResol,'[m]')
    categoryRule<-paste0(u,':',catLabel)
    categoryRules<-c(categoryRules,categoryRule)
  }

  tmpFile<-tempfile()
  write(reclassRules,tmpFile)
  execGRASS('r.recode',
    input=mapMerged,
    #output='tmp__speed',
    output=mapFriction,
    rules=tmpFile,
    flags='overwrite')

  write(categoryRules,tmpFile)
  execGRASS('r.category',
    map=mapFriction,
    separator=':',
    rules=tmpFile
    )
}



#' Evalutate memory available. This is experimental
#' @return Available memory in MB
sysEvalFreeMbMem <- function(){
  sys <- Sys.info()['sysname']
  free = 300

  switch(sys,
    'Darwin'={
      memTot = as.numeric(system("sysctl hw.memsize | awk '{ print $2 / (2^10)^2}'",intern=T))
      memActive = as.numeric(system("vm_stat | awk '/^Pages active/ { print ($3 * 4096) / (2^10)^2}'",intern=T))
      memFree = as.numeric(system("vm_stat | awk '/^Pages free/ { print ($3 * 4096) / (2^10)^2}'",intern=T))
      memPurgeable = as.numeric(system("vm_stat | awk '/^Pages purgeable/ { print ($3 * 4096) / (2^10)^2}'",intern=T))

      free = memTot - memActive
    },
    "Linux"={
      memTot = as.numeric(system("cat /proc/meminfo | awk '/^MemTotal:/ {print $2/ (2^10)}'",intern=T))
      memActive = as.numeric(system("cat /proc/meminfo | awk '/^Active:/ {print $2/ (2^10)}'",intern=T))
      memFree = as.numeric(system("cat /proc/meminfo | awk '/^MemFree:/ {print $2/ (2^10)}'",intern=T))
      memCached = as.numeric(system("cat /proc/meminfo | awk '/^Cached:/ {print $2/(2^10)}'",intern=T))

      free = memTot - memActive
    } 
    )

  return(as.integer(free))
  }


#'amIsotropicTraveTime
#'@export
amIsotropicTravelTime<-function(
  inputFriction,
  inputHf,
  inputStop=NULL,
  outputDir=NULL,
  outputCumulative,
  maxCost,
  minCost=NULL
  ){

  vInfo = amParseOptions(execGRASS("v.info",flags=c("t"),map=inputHf,intern=T))
  vHasLines = as.numeric(vInfo$lines) > 0
  tmpStart = NULL
  if(vHasLines){
    tmpStart =  amRandomName("tmp__raster_start")
    suppressWarnings({
      execGRASS("v.to.rast",input=inputHf,output=tmpStart,use="val",value=1)
    })
    inputRaster=tmpStart
    inputHf=NULL
  }else{
    inputRaster=NULL
  }
 
  # default memory allocation
  free = 300

  # dynamic memory allocation
  tryCatch({
    free = sysEvalFreeMbMem()
  },error=function(cond){
    amMsg(
      type="log",
      text=cond$message
      )
  })

  amMsg(
    type="log",
    text=sprintf("Memory available for r.cost = %s",free)
    )

  amParam=list(
    input=inputFriction,
    output=outputCumulative,
    start_points=inputHf,
    start_raster=inputRaster,
    stop_points=inputStop,
    outdir=outputDir,
    max_cost=maxCost * 60,
    memory = free
    )

  amParam<-amParam[!sapply(amParam,is.null)]

  execGRASS('r.cost',
    parameters=amParam,
    flags='overwrite'
    )

  amCleanTravelTime(outputCumulative,maxCost,minCost) 
  rmRastIfExists(tmpStart)
}








#'amAnisotropicTravelTime 
#' @param maxCost maximum cost in minute
#'@export
amAnisotropicTravelTime<-function(
  inputSpeed,
  inputHf,
  inputStop=NULL,
  outputDir=NULL,
  outputCumulative,
  returnPath,
  maxCost,
  minCost=NULL){


  #  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''),ifelse(keepNull,'n',''))
  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
  flags<-flags[!flags %in% character(1)]
 
  # default memory allocation
  free = 300

  # dynamic memory allocation
  tryCatch({
    free = sysEvalFreeMbMem()
  },error=function(cond){
    amMsg(
      type="log",
      text=cond$message
      )
  })
 
  amMsg(
    type="log",
    text=sprintf("Memory available for r.walk.accessmod = %s",free)
    )



  #
  # Convert vector line starting point to raster
  #
  vInfo = amParseOptions(execGRASS("v.info",flags=c("t"),map=inputHf,intern=T))

  vHasLines = as.numeric(vInfo$lines) > 0

  tmpStart = NULL

  if(vHasLines){
    tmpStart =  amRandomName("tmp__raster_start")
    suppressWarnings({
      execGRASS("v.to.rast",input=inputHf,output=tmpStart,use="val",value=1)
    })
    inputRaster = tmpStart
    inputHf = NULL
  }else{
    inputRaster = NULL
  }

  #
  # set
  #

  amParam=list(
    elevation = config$mapDem,
    friction = inputSpeed,
    output = outputCumulative,
    start_points = inputHf,
    start_raster = inputRaster,
    stop_points = inputStop,
    outdir = outputDir,
    memory = free,
    max_cost = maxCost * 60 # max cost in seconds.
    )

  amParam <- amParam[!sapply(amParam,is.null)]

  execGRASS('r.walk.accessmod',
    parameters=amParam,
    flags=flags
    ) 

  amCleanTravelTime(
    map = outputCumulative,
    maxCost = maxCost,
    minCost = minCost,
    convertToMinutes = TRUE
    )

  rmRastIfExists(tmpStart)

}

#'amCircularTravelDistance
#'@export
amCircularTravelDistance<-function(inputHf,outputBuffer,radius){
  suppressWarnings({
    execGRASS('v.to.rast',input=inputHf,output='tmp_buffer',use='val',value=1,flags='overwrite')
  })
  execGRASS('r.buffer',input='tmp_buffer',output=outputBuffer,distances=radius, flags='overwrite')
  # create one unique zone.
  expr=paste(outputBuffer,'=if(!isnull(',outputBuffer,'),1,null())')
  execGRASS('r.mapcalc',expression=expr,flags='overwrite')
}


#
##' amGetRasterSum
##' 
##' Extract cells sum using r.univar
##' 
##' @param rasterMap grass raster map name
##' @return cells sum
##' @export
#amGetRasterSum<-function(rasterMap){
#  s<-unlist(
#    strsplit(
#      x=grep('sum=', execGRASS('r.univar',map=rasterMap,flags='g',intern=T),value=T),
#      split='='
#      ) 
#    )[[2]] 
#  as.numeric(s)
#}
#

#' Parse scaling up coefficient options
#' @param opt String of option with paired argument separated by sepAssign, separated by given sepItem
#' @param sepAssign Character. Separator of assignement. Default is "="
#' @param sepItem Character. Separarator of items. Default is ";"
amParseOptions <- function(opt,sepItem=";",sepAssign="="){
  optList = list()
  if(!is.null(opt)){
    opt = unlist(strsplit(opt,sepItem))
    if(length(opt)>0){
      opt = strsplit(opt,sepAssign)
      for(o in opt){
        l = length(o)
        optList[o[l-1]]<-o[l]
      }
    }
  }
  return(optList)
}


#' amGetRasterStat
#' 
#' Extract cells stat using r.univar
#' 
#' @param rasterMap grass raster map name
#' @param stats Stat to compute. Should be in c('n','cells','max','mean','stdev','coeff_var','null_cells','min','range','mean_of_abs','variance','sum','percentile') 
#' @param quantile Percentiles to extract
#' @return cells stat
#' @export
amGetRasterStat <- function(rasterMap,metric=c('n','cells','max','mean','stddev','coeff_var','null_cells','min','range','mean_of_abs','variance','sum','percentile'),percentile=99){ 
  # validation
  if(!amRastExists(rasterMap))return()
  stopifnot(length(metric)==1)
  # set options
  metric=match.arg(metric)
  # if quantiles use r.quantile
  if(isTRUE("percentile" %in% metric)){
    val = amParseOptions(execGRASS("r.quantile",input=rasterMap,percentiles=percentile,intern=T),sepAssign=":")
  }else{
    val = amParseOptions(execGRASS("r.univar",map=rasterMap,flags="g",intern=T))[[metric]]
  }
  val <- as.numeric(val)

  if(length(val)==0) val <- 0L

  return(val)
}


#' Get the percentage from two raster
#' @param numerator Numerator
#' @param denominator Denominator
#' @return Percentage of one raster to another
#' @export
amGetRasterPercent <- function(numerator,denominator){
  if(numerator==denominator)return(0)
  denSum <- amGetRasterStat(denominator,"sum")
  numSum <- amGetRasterStat(numerator,"sum")
  return((denSum - numSum) / denSum*100)
}



#' Compose random char name
#' @param prefix Prefix of the resulting string
#' @param suffix Suffix of the resultiing string
#' @param n Number of random letters
#' @return String with random letters
#' @export
amRandomName <- function(prefix=NULL,suffix=NULL,n=20,cleanString=FALSE){
  if(cleanString){
    prefix = amSubPunct(prefix,'_')
    suffix = amSubPunct(suffix,'_')
  }
  rStr = paste(letters[round(runif(n)*24)],collapse="")
  str = c(prefix,rStr,suffix)
  paste(str,collapse="_")
}

#' Check for no data
#' @param val Vector to check 
#' @export
amNoDataCheck <- function( val = NULL ){
  isTRUE(
    is.null(val)
    ) ||
  isTRUE(
    isTRUE( is.data.frame(val) &&  nrow(val) == 0 ) ||
    isTRUE( is.list(val) && ( length(val) == 0 ) ) ||
    isTRUE( !is.list(val) && is.vector(val) && ( 
        length(val) == 0 || 
          val[[1]] %in% config$noDataCheck || 
          is.na(val[[1]]) || 
          nchar(val[[1]],allowNA=TRUE) == 0 )
      )
    )
}


#' function to extract display class info
#' @param class Class identifier
#' @param ls list id and class
#' @param dc dataClass table
#' @export
amClassInfo <- function(class=NULL,ls=FALSE,dc=config$dataClass){
  lang = config$language
  dc = config$dataClass
  if(ls){ 
    dc[,c('class',lang,'type')]
  }else{
    dc[dc$class==class,c('class',lang,'type')][1,]
  }
}


#' Get data class info
#' @param class Data class
#' @param value Value to retrieve, by default, language specific class
#' @export
amClassListInfo <- function(class=NULL,value=NULL){
  vals <- c("type","colors","allowNew","internal")
  lang <- config$language
  res <- character(0)
  if(!is.null(class)){ 
    for(i in class){
      if(is.null(value)){
        res <- c(res,config$dataClassList[[i]][[lang]])
      }else{
        if(!value %in% vals){
          amDebugMsg(paste("value must be in ",paste(vals,collapse=";")))
          return()
        }
        res<- c(res,config$dataClassList[[i]][[value]])   
      }
    }
    return(res)
  }
}





#'Create data list for ui
#'@param class AccessMod class to look for
#'@param dl Config data list to retrieve match
#'@export
amListData <- function(class=NULL,dl=dataList,shortType=TRUE){
  datAll <- character(0)
  for(i in class){
    d=amClassInfo(class=i)
    dType <- d[,'type']
    dat <- grep(paste0('^',d[,'class'],'__'),dl[[dType]],value=T)
    if(!isTRUE(is.null(dat) || length(dat)==0)){
      if(shortType){
        dType <- substr(dType,0,1)
      }
      names(dat) <- paste0("(",dType,") ",names(dat))
      datAll <- c(dat,datAll)
    }
  }
  return(datAll)
}


#' Update select input after validation
#' @param session Shiny session
#' @param idData AccessMod data identifier 
#' @param idSelect Shiny select input to update
#' @param dataList AccessMod reactive dataList
#' @param addChoices Additional choices (will also be used as select item name)
#' @export
amUpdateSelectChoice<-function(session=shiny::getDefaultReactiveDomain(),idData=NULL,idSelect=NULL,dataList=NULL,addChoices=NULL){
  if(is.null(idData) | is.null(idSelect) | is.null(dataList))return()
  dat<-amListData(idData,dataList)
  if(!is.null(addChoices)){

    dat  <- c(addChoices,dat)

  }
  if(length(dat)==0) dat = config$defaultNoData 

  selectNew <- dat[1]

  for(s in idSelect){

    selectOld <- session$input[[s]]

    if( selectOld %in% dat ) selectNew <- selectOld

    updateSelectInput(session,s,choices=dat,selected=selectNew)
  }
}


#' Create list of name for ui, file, file with mapset and html (with validation)
#' @param classes Base classes to which append tags
#' @param tag Character vector containing some tags
#' @param dataList List of existing data name
#' @export
amCreateNames <- function(classes,tag,dataList){

  resFile <- character(0)
  resFileMapset <- character(0)
  resUi <- character(0)
  resHtml <- character(0)

  # keep unique tags
  tag  <- amGetUniqueTags(tag) 
  # add tag function 
  addTags <- function(x,f=TRUE,m=TRUE){

    sepT <- config$sepTagRepl
    sepF <- config$sepTagFile
    sepC <- config$sepClass
    if(f){ 
      if(m){
        mapset <- paste0(config$sepMapset,execGRASS("g.mapset",flags="p",intern=T))
        paste0(paste(c(x,paste(tag,collapse=sepF)),collapse=sepC),mapset)
      }else{
        paste(c(x,paste(tag,collapse=sepF)),collapse=sepC)
      }
    }else{
      paste0(x," [",paste(tag,collapse=sepT),"]")
    }
  }


  for(i in classes){
    resFile[i] <- addTags(i,T,F)
    resFileMapset[i] <- addTags(i,T,T)
    resUi[i] <- addTags(amClassListInfo(i),F,F)
    type <- amClassListInfo(i,"type")
    if( isTRUE(length(dataList)>0) && isTRUE(resFileMapset[i] %in% dataList[[type]]))
    {
      resHtml[i] <- sprintf(" %s <b style=\"color:#FF9900\"> (overwrite warning)</b> ",resUi[i])
    }else{
      resHtml[i] <- sprintf("%s <b style=\"color:#00CC00\"> (ok)</b>",resUi[i])
    }
  }

  list(
    ui = resUi,
    file = resFile,
    fileMapset = resFileMapset,
    html = resHtml
    )
}



#' Save named list of value into cookie
#'
#' Note : don't use this for storing sensitive data, unless you have a trusted network.
#'
#' @param session Shiny session object. By default: default reactive domain.
#' @param cookie Named list holding paired cookie value.
#' @param nDaysExpires Integer of days for the cookie expiration
#' @return NULL
#' @export
amSetCookie <- function(session=getDefaultReactiveDomain(),cookies=list(),nDaysExpires=10L,deleteAll=FALSE){


  stopifnot(is.integer(nDaysExpires))
  stopifnot(is.list(cookies))

  if(is.null(cookies) && !isTRUE(deleteAll)) return()

  res <- list()
  res$deleteAll <- deleteAll
  res$cookies <- cookies
  res$expires <- nDaysExpires

  session$sendCustomMessage(
    type="amSetCookie",
    res
    )
}



#' Create a double linked selectable input
#' @param idInput Id of input
#' @param list1 List for the left part
#' @param list2 list for the right part
#' @export
amDoubleSortableInput <- function(idInput,list1=list(),list2=list(),title1="",title2="",class1="",class2=""){
  id1 <- sprintf("%s_1",idInput)
  id2 <- sprintf("%s_2",idInput)
  linkClass <- sprintf("%s_link",idInput) 

  l1 <- tags$div(class="col-md-6 col-xs-12",
    h3(title1),
    tags$div(
      id=id1,
      class=paste(linkClass,"list-group am_dbl_srt_input am_dbl_srt_box",class1,sep=" "),
      amListToSortableLi(list1)
      )
    )
  l2 <- tags$div(class="col-md-6 col-xs-12",
    h3(title2),
    tags$div(
      id=id2,
      class=paste(linkClass,"list-group am_dbl_srt_input am_dbl_srt_box",class2,sep=" "),
      amListToSortableLi(list2)
      )
    )

  #
  # output
  #
  
  #TODO: initialise the list from the client side
  tagList(
    tags$div(class="row",

      l1,
      l2
      ),
    singleton(
      tagList(
        tags$head(
          tags$script(src="src/jquery_custom/jquery-ui.min.js")
          ),
        tags$script(
          sprintf("
            sortableShinyFeedback = function(evt,ui){
              var el = $(evt.target);
              el.trigger('change');
            }
            $( '#%1$s' ).sortable({
              tolerance: 'pointer',
              forcePlaceholderSize: true,
              helper: function(event, ui){
                var $clone =  $(ui).clone();
                $clone .css('position','absolute');
                return $clone.get(0);
              },
              start: function (e, ui) {
                ui.placeholder.height(ui.helper.outerHeight());
              },
              placeholder: 'am_dbl_srt_placeholder',
              connectWith:'.%3$s',
              receive: sortableShinyFeedback,
              remove: sortableShinyFeedback,
              create: sortableShinyFeedback,
              stop: sortableShinyFeedback,
              update: sortableShinyFeedback

            });
            $( '#%2$s' ).sortable({
              helper: function(event, ui){
                var $clone =  $(ui).clone();
                $clone .css('position','absolute');
                return $clone.get(0);
              },
              start: function (e, ui) {  
                ui.placeholder.height(ui.helper.outerHeight());
              },
              tolerance: 'pointer',
              forceHelperSize: true,
              placeholder: 'am_dbl_srt_placeholder',
              connectWith: '.%3$s',
              receive: sortableShinyFeedback,
              remove: sortableShinyFeedback,
              create: sortableShinyFeedback,
              stop: sortableShinyFeedback,
              update: sortableShinyFeedback
            });
            ",
            id1,
            id2,
            linkClass
            )
          )
        )
      )
    )
}


#' Create a sortable list from stack items names for the landcover merge double sortable input
#' @param x Raster stack members list
#' @export
amListToSortableLi <- function(x){
  n<- names(x)
  if(is.null(n)) n <- x
  tagList(lapply(setNames(n,n),function(i){  
      val <- x[[i]]
      # get the type (line,area,grid,point) from stack item
      # e.g. getp 'line' from rStackRoad__Main_road_test_line@malawi_90_m
      type <- gsub(".*_([a-z]*?)@.*","\\1",val)
      class <- ""
      switch(type,
        "line"={class="icon-grid_line"},
        "area"={class="icon-grid_area"},
        "grid"={class="icon-grid_full"},
        "point"={class="icon-grid_point"}
        )
      class <- paste("list-group-item am_dbl_srt_item",class,sep=" ")
      tags$div(class=class,`data-input`=x[[i]],i)

      }))
}

#' Update double sortable input
#' @param idInput id of the double sortable input
#' @param list1 left list
#' @param list2 rigth list
#' @param session Shiny session
#' @export
amUpdateDoubleSortableInput <- function(idInput,list1=list(),list2=list(),session= shiny::getDefaultReactiveDomain()){

  listItem1 <- amListToSortableLi(list1)
  listItem2 <- amListToSortableLi(list2)

  id1 <- sprintf("%s_1",idInput)
  id2 <- sprintf("%s_2",idInput)


  amUpdateText(id=id1,listItem1)
  amUpdateText(id=id2,listItem2)

  session$sendCustomMessage("updateSortable",id1)
  session$sendCustomMessage("updateSortable",id2)

}

#' Produce a concatenated CamelCase version of a string
#' @param x String to convert
#' @param fromStart Boolean : should the first letter be upercased )
#' @export
amCamelCase <- function(x,fromStart=T){
  template = sprintf(
    "(\\s%s)([a-zA-Z0-9])",
    ifelse(fromStart,"|^","")
    )
  replacement = "\\U\\2\\E"
  gsub(template,replacement,x,perl=T)
}



#' amRasterToShape 
#' 
#' Extract area from raster and create a shapefile or append to it if the files already exist.
#'
#' @param idField Name of the facility id column.
#' @param idPos String id currently processed.
#' @param incPos Numeric increment position.
#' @param inputRaster Raster to export
#' @param outCatch Name of shapefile layer
#' @param listColumnsValue Alternative list of value to put into catchment attributes. Must be a named list.
#' @param dbCon  RSQlite connection to update value of catchment after vectorisation. 
#' @return Shapefile path
#' @export
amRasterToShape <- function(
  pathToCatchment,
  idField,
  idPos,
  incPos,
  inputRaster,
  outputShape="tmp__vect_catch",
  listColumnsValues=list(),
  oneCat=TRUE,
  dbCon){

 
  idField <- ifelse(idField==config$vectorKey,paste0(config$vectorKey,"_join"),idField)

  listColumnsValues[ idField ] <- idPos
  listColumnsValues <- listColumnsValues[!names(listColumnsValues) %in% config$vectorKey ]


  tmpRaster <- amRandomName("tmp__r_to_shape")
  tmpVectDissolve <- amRandomName("tmp__vect_dissolve")

  execGRASS("g.copy",raster=c(inputRaster,tmpRaster))

  if(oneCat){
    expOneCat<-sprintf("%1$s = !isnull(%1$s) ? 1 : null()",tmpRaster)
    execGRASS("r.mapcalc",expression=expOneCat,flags="overwrite")
  }

  #
  # Export input raster to vector
  #
  execGRASS("r.to.vect",
    input  = tmpRaster,
    output = outputShape,
    type   = "area",
    flags  = c("overwrite")
    )

  #
  # Dissolve result to have unique id by feature
  #
  execGRASS("v.dissolve",
    input  = outputShape,
    output = tmpVectDissolve,
    column = "value",
    flags  = c("overwrite")
    )

  #
  # Create a table for catchment
  #

  execGRASS("v.db.addtable",
    map = tmpVectDissolve 
    )

 outPath <- pathToCatchment
   # for the first catchment : overwrite if exists, else append.
  if(incPos==1){
    if(file.exists(outPath)){ 
      file.remove(outPath)
    }
    outFlags=c('overwrite','m','s')
  }else{
    outFlags=c('a','m','s')
  }
  #
  # update attributes 
  #
  dbRec <- dbGetQuery(dbCon,paste('select * from',tmpVectDissolve))

  if(length(listColumnsValues)>0){
    for(n in names(listColumnsValues)){
      dbRec[n] <- listColumnsValues[n]
    }
  }else{
    dbRec[idField] <- idPos
  }
  # rewrite
  dbWriteTable(dbCon,tmpVectDissolve,dbRec,overwrite=T)

  # export to shapefile. Append if incPos > 1
  execGRASS('v.out.ogr',
    input=tmpVectDissolve,
    output=outPath,
    format='ESRI_Shapefile',
    flags=outFlags,
    output_layer=outputShape
  )

  rmVectIfExists(tmpVectDissolve)
  rmVectIfExists(tmpRaster)
  rmVectIfExists(outputShape)


  return(outPath)
}



#' rescale to given range
#' @param inputRast Text raster name to rescale
#' @param outputRast Text output raster name
#' @param reverse Boolean Inverse the scale
#' @export
amRasterRescale <- function(inputMask=NULL,inputRast,outputRast,range=c(0L,10000L),weight=1,reverse=FALSE,  nullHandlerMethod = c("none","min","max")){

  if(amRastExists(inputMask)){ 
    rmRastIfExists("MASK")
    execGRASS("r.mask",raster=inputMask,flags="overwrite")
  }


  inMin <- amGetRasterStat(inputRast,"min") 
  inMax <- amGetRasterStat(inputRast,"max")


  if( nullHandlerMethod %in% c('min','max') ){
    # Input mask (candidate) can occurs were input raster ( map to rescale ) has NULL values.
    # we convert null to highest or lowest values depending on the scaling rescaling mode
    # This will work with travel time, as unreachead area could be seen as high priority,
    val <- ifelse( nullHandlerMethod == 'min', inMin, inMax )
    execGRASS("r.null", map=inputRast, null=val)
  }


  # http://support.esri.com/cn/knowledgebase/techarticles/detail/30961
  if(inMin == inMax){
    exprRescale <- sprintf("%1$s = (%2$s * %3$s) * %4$s",
      outputRast,
      median(range),
      inputMask, #Mask does not seems to be applied there, so add it in the expression.
      weight
      )
  }else{
    if(reverse) {
      expr <- " %1$s = ( %8$s *( %4$s - ((%2$s - %3$s) * (%4$s - %5$s ) / (%6$s - %3$s)) + %5$s)) * %7$s "
    }else{
      expr <- " %1$s = ( %8$s * (((%2$s - %3$s) * (%4$s - %5$s ) / (%6$s - %3$s)) + %5$s)) * %7$s "
    }
    exprRescale <- sprintf(expr,
      outputRast, #1
      inputRast, #2
      inMin,     #3
      max(range),#4
      min(range),#5
      inMax,     #6
      weight,     #7  
      inputMask  #8 mask does not seems to be applied in first case (first expr). Add it here to be sure.
      )
  }
  execGRASS("r.mapcalc",expression=exprRescale,flags="overwrite")
  
  if(!is.null(inputMask)){ 
    rmRastIfExists("MASK")
  } 
  return(outputRast)
}




#' Import temporary shapefile catchment to final directory
#' @param shpFile Full path to temp catchment file . eg. /tmp/super.shp
#' @param outDir Directory path where are stored shapefile. eg. /home/am/data/shapefiles/
#' @param outName Name of the final catchment shapefile, without extension. e.g. catchments_001
#' @return Boolean Done
amMoveShp <- function(shpFile,outDir,outName){
  #
  # Collect all shp related file and copy them to final directory. 
  # NOTE: make sure that: 
  # - pattern of shapefile is unique in its directory

  # in case of variable in path, convert outdir to fullpath
  if(length(shpFile)<1){
    return()
  }
  outDir <- system(sprintf("echo %s",outDir),intern=T)

  fe <- file.exists( shpFile )
  de <- dir.exists( outDir )
  so <- isTRUE( grep( ".*\\.shp$" , shpFile ) >0 )

  if( !fe ) warning( 
    sprintf("amMoveShp: %s input file does not exists",shpFile)
    )
  if( !de ) warning( 
    sprintf("amMoveShp: %s output directory does not exists",outDir)
    )
  if( !so ) warning( 
    sprintf("amMoveShp: %s input file does not have .shp extension",shpFile)
    )

  ok<-c(fe,de,so)

  if(all(ok)){
    # base name file for pattern.
    baseShape <- gsub('.shp','',basename(shpFile))
    # list files (we can also use )
    allShpFiles <- list.files(dirname(shpFile),pattern=paste0('^',baseShape),full.names=TRUE)
    # Copy each files in final catchment directory.
    for( s in allShpFiles){
      sExt <- file_ext(s)
      newPath <- file.path(outDir,paste0(outName,'.',sExt))
      file.copy(s,newPath,overwrite=T) 
    } 
  }
  return(all(ok))
}

#' Read or set cateogries from grass raster source
#' @param raster {string} Name of the raster layer to query
#' @param cateogies {vector} List of category to set
#' @export 
amGetRasterCategory = function(raster = NULL){

  if(amNoDataCheck(raster)) stop("No raster map name provided")

  tbl <- data.frame(integer(0),character(0))

  tblText = execGRASS("r.category",
    map = raster,
    intern =T
    )

  if(!amNoDataCheck(tblText)){

    tbl <- read.csv(
      text = tblText,
      sep = "\t",
      header = F,
      stringsAsFactors = F
      )
    if(ncol(tbl) == 2){ 
      tbl[,1] <- as.integer(tbl[,1])
    }

  }
  names(tbl) <- c("class","label")
  return(tbl)
}

