#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# additional custom reusable helper functions


#require(compiler)
#enableJIT(3)

# wrapper around Sys.sleep. Sleep in milisecond 
amSleep<-function(t=100){
  Sys.sleep(t/1000)
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

#function (x, env = parent.frame(), quoted = FALSE, label = NULL,
#      suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(),
#          autoDestroy = TRUE)
#{
#      fun <- exprToFunction(x, env, quoted)
#    if (is.null(label))
#              label <- sprintf("observe(%s)", paste(deparse(body(fun)),
#                              collapse = "\n"))
#        o <- Observer$new(fun, label = label, suspended = suspended,
#                  priority = priority, domain = domain, autoDestroy = autoDestroy)
#            registerDebugHook(".func", o, "Observer")
#            invisible(o)
#}


#

amGetArchiveList<-function(archivesPath,baseName){
  # archiveGrass need grass environment variables, as defined in config.R
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

amGetShapesList<-function(shapePath){
  # path need grass environment variables, as defined in config.R
  shapePath<-system(paste('echo',shapePath),intern=TRUE) 
  # if  directory doesn't exist, create it.
  dir.create(shapePath,showWarnings = FALSE)
  shapePath<-normalizePath(shapePath) 
  # add ressource for shiny 
  # return archive list
  shapeList<-list.files(shapePath,pattern='.shp$',full.names=T)
  if(length(shapeList)>0){
    #nameCatch<-gsub('.shp',paste0('@',location),catchList)
    nameShape<-gsub('.shp','',basename(shapeList))
    names(shapeList) <- nameShape
    as.list(shapeList)
  }else{
    list()
  }
}


amDataManager<-function(config,dataList,grassSession){
  gisLock=grassSession$gisLock
  dbCon=grassSession$dbCon
  mapset=grassSession$mapset
  if(!is.null(gisLock) && !is.null(dbCon) && !is.null(mapset)){

    rmVectIfExists('^tmp_*')
    rmRastIfExists('^tmp_*')
    archives<-amGetArchiveList(config$pathArchiveGrass,config$archiveBaseName)
    archivesSelect<-archives[order(archives,decreasing=T)]
    
    # extract data of type table only from sqlite
    sqlTables <- dbListTables(dbCon) 
    tblClasses <- config$dataClass$class[config$dataClass$type=='table']
    tables <- unlist(sapply(tblClasses,function(x)grep(paste0(x,config$sepClass),sqlTables,value=T)))


    if(length(tables)>0){
      # create selectize input. E.g table_model__p003 >>
      # named list element :  $`table_model [p003]`
      # value [1] "table_model__p003@p_500_m"
      tablesSelect<-amCreateSelectList(
        dName=tables,
        sepTag=config$sepTagFile,
        sepClass=config$sepClass,
        mapset=mapset)
    }else{
      tablesSelect=NULL
    }
    vectorsSelect<-amCreateSelectList(
      dName=execGRASS('g.list',type='vector',intern=TRUE),
      sepTag=config$sepTagFile,
      sepClass=config$sepClass,
      mapset=mapset
      )
    shapesSelect<-amCreateSelectList(
      dName=names(amGetShapesList(config$pathShapes)),
      sepTag=config$sepTagFile,
      sepClass=config$sepClass,
      mapset=mapset
      )

    rastersSelect<-amCreateSelectList(
      dName=execGRASS('g.list',type='raster',intern=TRUE),
      sepTag=config$sepTagFile,
      sepClass=config$sepClass,
      mapset=mapset
      )

    # if amCreateSelectList found NA in name (wrong data name)
    # remove from GRASS db
    if(T){
      if(!is.null(rastersSelect)){
        rastToRemove<-rastersSelect[is.na(names(rastersSelect))]
        if(isTRUE(length(rastToRemove)>0)){
          sapply(rastToRemove,function(x){
            x<-unlist(strsplit(x,config$sepMapset))[1]
            message(paste("removing unnamed file", x))
            rmRastIfExists(x)}
            )
        }
      }
      if(!is.null(vectorsSelect)){
        vectToRemove<-vectorsSelect[is.na(names(vectorsSelect))]

        if(isTRUE(length(vectToRemove))>0){
          sapply(vectToRemove,function(x){
            x<-unlist(strsplit(x,config$sepMapset))[1]
            message(paste("removing unnamed file", x))
            rmVectIfExists(x)}
            )
        }
      }
      if(!is.null(tablesSelect)){
        tableToRemove<-tablesSelect[is.na(names(tablesSelect))]
        if(isTRUE(length(tableToRemove)>0)){
          sapply(tableToRemove,function(x){
            x<-unlist(strsplit(x,config$sepMapset))[1]
            message(paste("removing unnamed file", x))
            sql<-paste("DROP TABLE IF EXISTS",x)
            dbGetQuery(dbCon,sql)}
            )
        }
      }
    }


    dataList$raster<-rastersSelect
    dataList$vector<-vectorsSelect
    dataList$shape<-shapesSelect
    dataList$table<-tablesSelect
    dataList$archive<-archivesSelect


    dataList$df<-rbind(
      amDataListToDf(rastersSelect,config$sepClass,'raster'),
      amDataListToDf(vectorsSelect,config$sepClass,'vector'),
      amDataListToDf(shapesSelect,config$sepClass,'shape'),
      amDataListToDf(tablesSelect,config$sepClass,'table')
      )

    #dataList$tags <-unique(unlist(strsplit(paste(dataList$df$tag),' ')))
    dataList$tags <- amGetUniqueTags(dataList$df$tag)

  }else{
    amDebugMsg('DataList: no gisLock, mapset or dbCon ')
  }
}
#
#amDataManager<-function(config,dataList,gisLock,dbCon,archivePath,mapset){
#  if(!is.null(gisLock)){
#
#    grassSession$archives<-amGetArchiveList(config$pathArchiveGrass,config$archiveBaseName)
#    rmVectIfExists('^tmp_*')
#    rmRastIfExists('^tmp_*')
#    archives<-list.files(archivePath)
#    archivesSelect<-archives[order(archives,decreasing=T)]
#    sqlTables<-"select name from sqlite_master where type='table' AND name like 'table_%' "
#    tables<-dbGetQuery(dbCon,sqlTables)$name
#    if(length(tables)>0){
#      # create selectize input. E.g table_model__p003 >>
#      # named list element :  $`table_model [p003]`
#      # value [1] "table_model__p003@p_500_m"
#      tablesSelect<-amCreateSelectList(
#        dName=tables,
#        sepTag=config$sepTagFile,
#        sepClass=config$sepClass,
#        mapset=mapset)
#    }else{
#      tablesSelect=NULL
#    }
#    vectorsSelect<-amCreateSelectList(
#      dName=execGRASS('g.list',type='vector',intern=TRUE),
#      sepTag=config$sepTagFile,
#      sepClass=config$sepClass,
#      mapset=mapset
#      )
#
#    rastersSelect<-amCreateSelectList(
#      dName=execGRASS('g.list',type='raster',intern=TRUE),
#      sepTag=config$sepTagFile,
#      sepClass=config$sepClass,
#      mapset=mapset
#      )
#
#    # if amCreateSelectList found NA in name (wrong data name)
#    # remove from GRASS db
#    if(T){
#      if(!is.null(rastersSelect)){
#        rastToRemove<-rastersSelect[is.na(names(rastersSelect))]
#        if(isTRUE(length(rastToRemove)>0)){
#          sapply(rastToRemove,function(x){
#            x<-unlist(strsplit(x,config$sepMapset))[1]
#            message(paste("removing unnamed file", x))
#            rmRastIfExists(x)}
#            )
#        }
#      }
#      if(!is.null(vectorsSelect)){
#        vectToRemove<-vectorsSelect[is.na(names(vectorsSelect))]
#
#        if(isTRUE(length(vectToRemove))>0){
#          sapply(vectToRemove,function(x){
#            x<-unlist(strsplit(x,config$sepMapset))[1]
#            message(paste("removing unnamed file", x))
#            rmVectIfExists(x)}
#            )
#        }
#      }
#      if(!is.null(tablesSelect)){
#        tableToRemove<-tablesSelect[is.na(names(tablesSelect))]
#        if(isTRUE(length(tableToRemove)>0)){
#          sapply(tableToRemove,function(x){
#            x<-unlist(strsplit(x,config$sepMapset))[1]
#            message(paste("removing unnamed file", x))
#            sql<-paste("DROP TABLE IF EXISTS",x)
#            dbGetQuery(dbCon,sql)}
#            )
#        }
#      }
#    }
#
#
#    dataList$raster<-rastersSelect
#    dataList$vector<-vectorsSelect
#    dataList$table<-tablesSelect
#    dataList$archive<-archivesSelect
#
#    dataList$df<-rbind(
#      amDataListToDf(rastersSelect,config$sepClass,'raster'),
#      amDataListToDf(vectorsSelect,config$sepClass,'vector'),
#      amDataListToDf(tablesSelect,config$sepClass,'table')
#      )
#
#  }else{
#    amDebugMsg('DataList: no gisLock. ')
#  }
#}
#
#
#

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

#http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
amCleanHtml <- function(htmlString) {
    return(gsub("<.*?>", "", paste(htmlString)))
}



amMsg<-function(session=shiny:::getDefaultReactiveDomain(),type=c('error','warning','message','log','ui'),text,title=NULL,logFile=config$pathLog){
  type<-match.arg(type)

  if(is.null(title))title=type
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

  amSweetAlert(session, text,title,img="logo/icons/logo128x128.png",timer=2000)
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
  mN<-basename(mapNames) # list of map names to be validated.
  mT<-mapType # vect or rast
  fE<-file_ext(mN) # list of file extension in map list
  # vector files
  if(mT=='vect'){
    # rule 1 : if it's a shapefile, it must have minimal set of  file extensions.
    if('shp' %in% fE){
      valid<-all(amSubPunct(config$fileShpExtMin,'') %in% fE)
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


# functions to handle update
amUpdateApp<-function(){
  system('git merge FETCH_HEAD')
  packrat::restore(prompt=FALSE)
}

amGetVersionLocal<-function(){
  system('git rev-list HEAD --count',intern=T)
}

amGetCurrentBranch<-function(){
  system("git branch | grep '*' |awk '{ print $2}'",intern=T)
}

amGetVersionRemote<-function(){
  netok<-isTRUE(ping('github.io',count=1)<1000) # 1 sec should be enough
  if(netok){
    system(paste('git fetch origin',amGetCurrentBranch()))
    msgVers<-system('git rev-list FETCH_HEAD --count',intern=T)
    if(isTRUE(nchar(msgVers)<0))msgVers='No new revision found.'
  }else{
    msgVers="Repository not available, try again later or report this issue."
  }
  msgVers
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


amExportData<-function(dataName,exportDir,type,vectFormat='shp',rastFormat='hfa',tableFormat='csv',dbCon=NULL,pathShapes=NULL){
  reportName<-paste0(dataName,'_report.txt')
  reportPath<-file.path(exportDir,reportName)
  infoName<-paste0(dataName,'_info.txt')
  infoPath<-file.path(exportDir,infoName)

  # default export function for grass.
  # If other formats are requested, add other preformated command here.
  switch(type,
    'shape'={
      allShpFiles<-list.files(pathShapes,pattern=paste0('^',dataName,'\\.'),full.names=TRUE)
        # sorry for this.
        for(shpP in allShpFiles){
          sExt <- file_ext(shpP)
          newPath <- file.path(exportDir,paste0(dataName,'.',sExt))
          file.copy(shpP,newPath) 
        }
    },
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
            #output=filePath,
            output=exportDir,
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
        'tiff'={
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
          # note : with force flags, Integer could lead to data loss !
        },
        'hfa' = {
          # hfa
          fileName<-paste0(dataName,'.img')
          reportPath<-paste0(dataName,'_report.txt')
          infoPath<-paste0(dataName,'_info.txt')
          filePath<-file.path(exportDir,fileName)
          execGRASS('r.out.gdal',
            flags =c('overwrite','f'),
            input=dataName,
            output=filePath,
            format="HFA",
            #nodata=65535,
            createopt='TFW=YES'
            #type='UInt16') ## preserve cell type. Not a good idea for non-discrete values.
            )

        }
        
        ) 

      return(c(fileName,infoName,reportName))
    },
    'table'={
      fileName<-paste0(dataName,'.xlsx')
      filePath<-file.path(exportDir,fileName)
      q<-paste('SELECT * FROM',dataName,';')
      tbl<-dbGetQuery(dbCon,q)
      rio::export(tbl,filePath)
      #write.csv(tbl,filePath)
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



amRestart<-function(session=shiny:::getDefaultReactiveDomain()){
  session$sendCustomMessage(
    type="jsCode",
    list(code='location.reload();')
    )
}

# update text by id
amUpdateText<-function(session=shiny:::getDefaultReactiveDomain(),id,text){
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


amSweetAlert<-function(session=shiny:::getDefaultReactiveDomain(), text,title=NULL,imgUrl=NULL,timer=NULL){
  #TODO: check how to handle quoted string. Tried to escape everything
  # without success.
  # idea 1: htmltools:::htmlEscape
  # idea 2: convert to binary then base64 and back? 
  #require sweetAlert.js and sweetAlert.css
  items<-list()

  if('html' %in% class(text) || 'shiny.tag.list' %in% class(text)){
    text<-paste(text)
    text<-gsub('\\n'," ",text)
    text<-gsub("\""," ",text)
    text<-gsub("\'"," ",text)
    items$html<-paste("html:'",text,"'")
  }else{
    text<-gsub('\\n'," ",text)
    text<-gsub("\""," ",text)
    text<-gsub("\'"," ",text)
    items$text<-paste0("text:\"",text,"\"")
  }


  if(!is.null(title))items$title<-paste0("title:'",title,"'")
  if(!is.null(img))items$img<-paste0("imageUrl:'",imgUrl,"'")
  if(!is.null(timer) && is.integer(timer))items$timer<-paste0("timer:'",timer,"'")
  items$animation<-paste0("animation:false")
  val<-paste("swal({",paste0(items,collapse=','),"})")

  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )

}

# link selected archive to a new window location. The browser should as to download.
#TODO: as it's rendered in the same window, it could break shiny application, or reset it. Make sure that's not a problem with standard browser. Works with webkit browser.
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
amActionButtonToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
  addDefault<-paste0("$('#",id,"').addClass('btn-default').removeClass('btn-danger').attr('disabled',false);")
  addDanger<-paste0("$('#",id,"').addClass('btn-danger').removeClass('btn-default').attr('disabled',true);")

  val<-ifelse(disable,addDanger,addDefault)
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
    )
}


amActionLinkToggle <- function(id,session=shiny:::getDefaultReactiveDomain(),disable=TRUE) {
  
  addDefault<-paste0("$('#",id,"').css('color','').attr('disabled',false).removeClass('btn btn-txt-left');")
  addDanger<-paste0("$('#",id,"').css({'color':'red','display':'inline'}).addClass('btn btn-txt-left').attr('disabled',true);")

  val<-ifelse(disable,addDanger,addDefault)
  session$sendCustomMessage(
    type="jsCode",
    list(code=val)
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







# amUpdateProgressBar : update amPgoressBar
# session = shiny session
# idBar = id set with amProgressBar()
# amout = value from 0 to 100
# hide = hide progress bar
amUpdateProgressBar<-function(session=shiny:::getDefaultReactiveDomain(),idBar,amount=0,final=F){
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
    #amDebugMsg('accessmod is busy')
    js="amAddBusy()"
  }else{
    #amDebugMsg('accessmod is not busy')
    js="amRemoveBusy()"
  }
  session$sendCustomMessage(type='jsCode',list(code=js))
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
amUploadTable<-function(config,dataName,dataFile,dataClass,dbCon){
  message("Start processing table",dataName)
  tbl<- na.omit(import(dataFile))
  if(!exists('tbl')){
    stop(paste('AccessMod could not read the provided file. Try another compatible format:',config$filesAccept$table))
    }
  aNames<-config$tableColNames[[dataClass]]
  if(is.null(aNames)) stop(paste('No entry found in config for class:',dataClass))
  tNames<-tolower(names(tbl))
  if(!all(aNames %in% tNames)){
    stop(paste('Importation of ',basename(dataFile),': dataset of class ',dataClass,' should contain columns named ',paste(aNames,collapse=';'),'. The provided file contains non-conform column names :',paste(tNames,collapse=';'),'.'))
  }
  names(tbl)<-tNames
  tbl<-tbl[,aNames] # keep only needed columns
  dbWriteTable(dbCon,dataName,tbl,overwrite=TRUE)
  message("Table",dataName," written in DB")
}




amErrHandler<-function(session=shiny:::getDefaultReactiveDomain(),errMsgTable,conditionMsg,title=NULL,type='warning'){
  # in all case, return message as log.
  amMsg(
    session,
    type='log',
    text=conditionMsg,
    title=title
    )
  # try to find a registered simplified message to display in UI
  errorsFound<-sapply(errMsgTable$cond,
    function(x,cond=conditionMsg){
      found<-grep(x,cond)
      ifelse(length(found)==0,FALSE,TRUE)
    })
  errorsMsg<-errMsgTable[errorsFound,]
  # if one or more msg are found in registered msg, 
  # replace original message
  if(nrow(errorsMsg)>0){
    for(i in 1:nrow(errorsMsg)){ 
      amMsg(
        session,
        type=tolower(errorsMsg[i,'type']),
        text=errorsMsg[i,'text'],
        title=title
        )
    }
    # if no match found in msg table, return 
    # original text and type found by amErrorAction
  }else{
    amMsg(
      session,
      type=type,
      text=conditionMsg,
      title=title
      ) 
  }
}





amErrorAction <- function(expr,errMsgTable=config$msgTableError,quotedActionError=NULL,quotedActionWarning=NULL,quotedActionMessage=NULL, quotedActionFinally=NULL, title,session=shiny:::getDefaultReactiveDomain()){
  amBusyManage(session, TRUE)
  withCallingHandlers({
    tryCatch({
      expr
    },
    # error : stop process, eval error quoted function, return condition to amErrHandler
    error = function(cond){
          if(!is.null(quotedActionError))eval(quotedActionError)
      amErrHandler(session,errMsgTable,paste(cond),title=title,type='error')
  })},
    # warning, don't stop process, but return condition to amErrHandler
    warning= function(cond){
      cond<-amSubQuote(cond)
      if(!is.null(quotedActionWarning))eval(quotedActionWarning)
      amErrHandler(session,errMsgTable,paste(cond),title=title,type='warning')
    },
    # simple message : don't stop, write in log
    message= function(cond){
      cond<-amSubQuote(cond)
      if(is.null(quotedActionMessage))eval(quotedActionMessage)
      amMsg(session,text=paste(cond),title=title,type='log')  
    },
    finally={
    if(!is.null(quotedActionFinally))eval(quotedActionFinally)
    amBusyManage(session,FALSE)
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
amUploadRaster<-function(config,dataInput,dataName,dataFiles,dataClass){
  #dataInput=unique files or folder to give to gdal
  #dataName = name of output data
  #dataFile = actual list of files.
  amDebugMsg('Start processing raster ',dataName)
  # retrieve default color table by class
  colorsTable<-config$dataClass[config$dataClass$class==dataClass,'colors']
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
  amValidateFileExt(dataFiles,'rast')
  # temp geotiff
  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.img'))
  gdalwarp(dataInput,
    dstfile=tmpDataPath,
    t_srs=if(tryReproj){amGetLocationProj()},
    of="HFA",
    dstnodata="-9999",
    output_Raster=FALSE,
    overwrite=TRUE)

  amDebugMsg('GDAL finished cleaning.')
  if(file.exists(tmpDataPath)){

    if(dataClass=="dem"){
      currentMapset = execGRASS('g.mapset',flags='p',intern=TRUE)
      execGRASS('g.mapset',mapset="PERMANENT")
    }

    execGRASS('r.in.gdal',
      input=tmpDataPath,
      output=dataName,
      flags=c('overwrite','quiet'),
      title=dataName)



    if(!is.null(colorsTable)){
      message(paste('Set color table to',colConf$color,'with flag=',colConf$flag))
      execGRASS('r.colors',map=dataName,flags=colConf$flag,color=colConf$color)
    }

   if(dataClass=="dem"){
      execGRASS('g.mapset',mapset=currentMapset)
    }

    

    message(paste("Manage data:",dataName,'loaded in accessmod.'))
  }else{
    stop('Manage data: process aborded, due to unresolved CRS or not recognized input files. Please check files metadata and extent. Importation cancelled.')
  }
  # create a rasterlayer to get projection info from the file
  # (raster not loaded in memory)
  r<-raster(tmpDataPath)
  givenProj<-proj4string(r)
  if(!givenProj==amGetLocationProj()){
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
        amGetLocationProj()))
  }
  file.remove(c(dataFiles, tmpDataPath))
  return(NULL)
}

amUploadNewProject<-function(newDem,newProjectName){ 
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
  gHome<-file.path(tempdir(),newProjectName)
  dir.create(gHome,showWarnings=F)
  initGRASS(gisBase = config$pathGrassBase70, # binary files (grass 7.0)
    #home            = config$pathGrassHome, # where store lock file
    home            = gHome, # where store lock file
    gisDbase        = config$pathGrassDataBase, # local grass database
    location        = newProjectName, # rsession
    mapset          = 'PERMANENT', # PERMANENT for dem.
    SG              = sg, #spatial grid as templte for extent and res
    override        = TRUE)
  execGRASS('g.proj',flags='c',proj4=destProj)
  execGRASS('db.connect',driver='sqlite',database=config$pathSqliteDB)
  # set as default region
  message('Set grass environment and import DEM')
  execGRASS('g.gisenv',flags='s')
  amDebugMsg('tmpMapPath exists:',file.exists(tmpMapPath))
  execGRASS('r.in.gdal',
    input=tmpMapPath,
    output=config$mapDem,
    flags=c('overwrite','quiet'),
    title=paste(newProjectName,'DEM')
    )
  execGRASS('r.colors',map=config$mapDem,color='elevation')
  message('Set default region based on DEM and set null values as zeros to enable accessibility calculation in sea region. ')
  execGRASS('g.region', raster=config$mapDem)
  unset.GIS_LOCK()
  unlink_.gislock()
  message('Removing temp files.')
  file.remove(tmpMapPath)
}


#
# Upload vectors
#
#
amUploadVector<-function(dataInput, dataName, dataFiles){
  tryReproj=TRUE
  # helper function to validate file based on extension
  amValidateFileExt(dataFiles,'vect')
  tmpDataPath<-file.path(tempdir(),paste0(dataName,'.shp'))
  ogr2ogr(
    src_datasource_name=dataInput,
    dst_datasource_name=tmpDataPath,
    #where=input$dataSql,
    f="ESRI Shapefile",
    t_srs=if(tryReproj){amGetLocationProj()},
    overwrite=TRUE,
    verbose=TRUE)
  message('GDAL finished cleaning. Importation in GRASS.')
  execGRASS("v.in.ogr",
    flags=c("overwrite","w","2"), # overwrite, lowercase, 2d only,
    parameters=list(input=tmpDataPath, output=dataName, snap=0.0001)
    )

  message(paste(dataName,'loaded in accessmod.'))
  unlink(dataFiles)
  return(NULL)
}
#
#  # validate rules of input file.
#  #fE<-file_ext(dataNew$name)
#  # helper function to validate file based on extension
#  amValidateFileExt(dataInput,'vect')
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
  amDebugMsg('update data list')
  listen$dataListUpdate<-runif(1)
}

#amUpdateProjectList<-function(listen){
#  amDebugMsg('update project')
#  listen$projectListUpdate<-runif(1)
#}
#
#
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

#
#
#amGetGrassMeta<-function(crsOut=c('orig','latlong')){
#  crsOut<-match.arg(crsOut)
#
#
#  locationExt<-as(extent(gmeta2grd()),'SpatialPolygons')
#  proj4orig<-amGetLocationProj(ignore.stderr=T)
#  proj4dest<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
#  proj4string(locationExt)<-proj4orig
#  if(crsOut=='latlong')
#    locationExt<- spTransform(locationExt,CRS(proj4dest))
#  bbx<-as.data.frame(locationExt@bbox)
#  bbxCenter<-c((bbx['y','max']+bbx['y','min'])/2,(bbx['x','max']+bbx['x','min'])/2)
#  bbxBound<-list(list(bbx['y','min'],bbx['x','min']),list(bbx['y','max'],bbx['x','max']))
#
#  if(crsOut=='latlong'){
#    # set a geojson bounding box with a inner ring.
#    extStyle<-list(
#      fillColor = "black",
#      fillOpacity = 0.5,
#      opacity=0.1,
#      weight = 1,
#      color = "#000000"
#      )
#
#    ext<-fromJSON(geojson_json(locationExt)[[1]])
#    worldCoord<-list(c(-180,-90),c(-180,90),c(180,90),c(180,-90),c(-180,-90))
#    extCoord<-ext$features[[1]]$geometry$coordinates[[1]]
#    ext$features[[1]]$geometry$coordinates<-list(worldCoord,extCoord)
#    ext$style<-extStyle
#  }else{
#    ext=NULL
#  }
#
#  bbxList<-as.list(locationExt@bbox)
#  names(bbxList)<-c('xmin','ymin','xmax','ymax')
#
#  gL<-gmeta()
#  metaList<-list(
#    "North-south resolution:"               = gL$nsres,
#    "East-west reolution"                   = gL$ewres,
#    "Bounding box (xmin, xmax, ymin, ymax)" = bbxList,
#    "Number of cells"                        = gL$cells,
#    "Number of rows"                        = gL$rows,
#    "Number of columns"                     = gL$cols
#    )
#  metaHtml<-listToHtml(metaList,h=6) 
#
#  return(list(
#      "bbxGeoJson"=ext,
#        "bbxSp"=locationExt,
#    "bbxDf"=bbx,
#    "bbxLeaflet"=bbxBound,
#    "bbxCenter"=bbxCenter,
#    "summary"=metaList,
#    "summaryHtml"=metaHtml,
#    "projOrig"=getLocationProj(ignore.stderr=T)
#    )
#    )
#
#
#}



amMapMeta<-function(){
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


amAddOverlay<-function(session=shiny:::getDefaultReactiveDomain(),mapId,imgBounds,imgUrl){
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
  if(!is.na(nBridges) || isTRUE(nBridges>0)){
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
  cla=unlist(strsplit(fileN,paste0("\\",sepClass)))[[1]]
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
          #cla=unlist(strsplit(dat,paste0("\\",sepClass)))[[1]]
          #tag=unlist(strsplit(dat,paste0("\\",sepClass)))[[2]]
          #tag=paste0("[",gsub(sepTag,' ',tag),"]")
          #lN<-c(lN,paste(cla,tag))
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
  if(is.null(x) || is.na(x))return()
  if(length(x)>1)x=unlist(x)
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

# get class of data
amGetClass<-function(amData,sepClass){
  as.character(strsplit(unlist(amData),paste0('(\\',sepClass,').*')))
}

amGetType <- function(amData,config){
  if(amData==config$newFacilitiesShort)return('vector')
  class <- amGetClass(amData,config$sepClass)
  type <- config$dataClass[config$dataClass$class==class,'type']
  return(type)
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
# amDataList = vector of dataSet
# sepClass = class separator (double dash)
# type = type attribute in resulting data.frame
amDataListToDf<-function(amDataList,sepClass,type='raster'){
  if(is.null(amDataList)||length(amDataList)<1)return(NULL)
  cla=amGetClass(amDataList,sep=sepClass)
  tag=amGetTag(amDataList)
  name=amNoMapset(amDataList)
  display=names(amDataList)
  data.frame(class=cla,
    tags=tag,
    type=type,
    searchCol=paste(type,cla,tag),
    origName=name,
    displayName=display
    )
}






# Example
# > amDataListToDf(dList$raster,sepClass='__',type='raster')
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
#' @param dataListOrig table with columns: "type,class,tags,origName" .
#' @param dataListUpdate table with columns: "type,class,tags,origName". If it contains modified tags value, origName is set as old name, new name is formed based on class and tags.
#' @param dbCon: path to sqlite db
#'
# @export
amUpdateDataListName<-function(dataListOrig,dataListUpdate,dbCon,pathShapes,config){
  library(dplyr)
  if(!is.null(dataListOrig) && !is.null(dataListUpdate)){
    tblO <- dataListOrig
    tblU <- dataListUpdate
    tblO[] <- lapply(tblO,as.character)
    tblU[] <- lapply(tblU,as.character)
    # test for empty or incorrect table
    if(any(sapply(tblU,function(x)isTRUE(nchar(x)<1 | is.na(x) | x=='-')))){
      message('Rename data : there is NA, missing char or "-" in update table')
    }else{
      # search for new tags
      tblM <- anti_join(tblU,tblO)
      hasChanged<-isTRUE(nrow(tblM)>0)
      if(hasChanged){
        apply(tblM,1,function(x){
          if(x['class']!='dem'){
          amRenameData(
            type=x['type'],
            new=paste(x['class'], paste(amGetUniqueTags(x['tags']),collapse=config$sepTagFile),sep=config$sepClass),
            old=x['origName'],
            dbCon=dbCon,
            pathShapes=pathShapes
            )
          }
    })
        return(TRUE)
      }
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
#'
#' @export
amRenameData<-function(type,old="",new="",dbCon=NULL,pathShapes=NULL){
  if(!type %in% c('raster','vector','table','shape') || old==""||new=="")return()
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
      if(is.null(pathShapes))return()
      sL<-amGetShapesList(pathShapes)

      if(!tolower(new) %in% tolower(names(sL)) && old %in% names(sL)){
        # sL did not return all related files to this layer : get these.
        allShpFiles<-list.files(pathShapes,pattern=paste0('^',old,'\\.'),full.names=TRUE)
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

    }
    )

    if(renameOk){
      message(paste("Renamed",old,"to",new,"."))
    }else{
      warning(paste("Rename",old,"to",new," not necessary: new name already exists or the old one didn't exists"))
    }
}

########### SECTION GIS MODULES
amRastQueryByLatLong<-function(coord,rasterName,projOrig,projDest){
  coord<-SpatialPoints(data.frame(coord['x'],coord['y']))
  #proj4string(coord)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '
  proj4string(coord)<-projDest
  #coord<-spTransform(coord,CRS(getLocationProj()))@bbox[,'max']
  coord<-spTransform(coord,CRS(projOrig))@bbox[,'max']
  val<-execGRASS('r.what',map=rasterName,coordinates=c(coord[1],coord[2]),flags='f',intern=T) 
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
      message(paste(as.character(...),diff))
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

    message('retrieve map from grass to create png file in lat long ')
    # define bounding box intersection.
    #get intersection betweed leaflet extent and project extent
    bbxSpLatLongInter<-gIntersection(bbxSpLatLongOrig,bbxSpLatLongLeaf)
    if(is.null(bbxSpLatLongInter))return(NULL)
    bbxMatLatLongInter<-bbxSpLatLongInter@bbox
    # to avoid to much cache files, round bbx values.
    # NOTE: if rendering time is short, skip this process ?
    bbxMatLatLongInterRound<-round(bbxMatLatLongInter,10)
    # file names
    cacheMap<-file.path(mapCacheDir,paste0(mapToPreview,"__",paste0(bbxMatLatLongInterRound,collapse="_"),'.png'))
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
      toc('start g.region')
      execGRASS('g.region',
        e=paste(bbxMatProjInter['x','max']),
        w=paste(bbxMatProjInter['x','min']),
        n=paste(bbxMatProjInter['y','max']),
        s=paste(bbxMatProjInter['y','min']),
        res=paste(resOverlay) 
        )
      toc('end g.region, start create mask from region')
      execGRASS('v.in.region',output='tmp_mask')
      execGRASS('r.mask',vector='tmp_mask')
      # compute preview map
      toc('start resampling at the new resolution')
      toc('end mapcalc, start r.out.png')
      # export in png with transparency and remove mask
      execGRASS('r.out.png',input=mapToPreview, output=cacheMap,flags=c('overwrite','w','t')) # with world file
      # NOTE: uncomment those lines if reprojection is needed. For a map preview, this should be ok...
      #  gdalwarp(tempMapPng,
      #    dstfile=tempMapTiff,
      #    #s_srs=metaOrig$projOrig,
      #    t_srs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs',
      #    output_Raster=FALSE,
      #    overwrite=TRUE)
      #  # as gdal can't warp directly in png (why?), translate it.
      #  gdal_translate(tempMapTiff,
      #    dst_dataset=cacheMap,
      #    ot='byte',
      #    of='PNG'
      #    )
      # set back the grass resgion to dem values.
      toc('end r.out.png, start g.region')
      execGRASS('g.region', raster=config$mapDem)
      toc('stop g.region, cleaning temp map')
      rmRastIfExists('MASK*')
      rmRastIfExists('tmp_*')
      rmVectIfExists('tmp_*')
    }
    message('retrieving done. in ',format(toc(),units='s'))
    return(list(
        pngFile=cacheMap,
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
# NOTE: instead of reading the whole table : loop on fields and use DISTINCT ?
amGetFieldsSummary<-function(table,dbCon,getUniqueVal=T){
  stopifnot(table %in% dbListTables(dbCon))
  tblSample<-dbGetQuery(dbCon,paste("SELECT * FROM",table,ifelse(getUniqueVal,"","LIMIT 100")))
  nR<-nrow(tblSample)
  idxCandidate<-sapply(tblSample,function(x){
    isTRUE(length(unique(x))==nR)
})
  if(getUniqueVal){
    uniqueVal<-sapply(tblSample,function(x){
      x=unique(x)
      sort(x)
})
  }else{
    uniqueVal=NULL
  }
  idxFields<-names(idxCandidate)[idxCandidate]
  
  numFields<-sapply(tblSample,function(x){
    isNum<-is.numeric(x) && !is.logical(x)
    if(isNum){
      !any(is.na(x) | "" %in% x)
    }else{
      FALSE
    }}) %>% 
  names(tblSample)[.]

 intFields<-sapply(tblSample,function(x){
    isInt<-is.integer(x) && !is.logical(x)
    if(isInt){
      !any(is.na(x) | "" %in% x)
    }else{
      FALSE
    }}) %>% 
  names(tblSample)[.]


 charFields<-sapply(tblSample,function(x){
    isChar<-is.character(x) && !is.logical(x)
    if(isChar){
      !any(is.na(x) | "" %in% x)
    }else{
      FALSE
    }}) %>% 
 names(tblSample)[.]

 list(
   int=intFields,
    num=numFields,
    char=charFields,
    idx=idxFields,
    val=uniqueVal
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

amCleanTravelTime<-function(map,maxCost,minCost=NULL,convertToMinutes=TRUE){
  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.
  if(maxCost>0){
    expr=paste(map,"=if(",map,"<=",maxCost,",",map,",null())")
    amDebugMsg(paste("amCleanTravelTime. maxCost > 0; remove overpassed values. CMD r.mapcalc==",expr))
    execGRASS('r.mapcalc',expression=expr,flags=c('overwrite'))
  }
  if(!is.null(minCost) && (minCost<maxCost || maxCost==0)){
    amDebugMsg("amCleanTravelTime. minCost > 0; remove overpassed values.")
    expr=paste(map,"=if(",map,">=",minCost,",",map,",null())")
    execGRASS('r.mapcalc',expression=expr,flags=c('overwrite'))
  }

  if(convertToMinutes){
  expr = paste(map," = ",paste0(map,"/60"))
  amDebugMsg("amCleanTravelTime. Convert to minutes.")
  execGRASS('r.mapcalc',expression=expr,flags=c('overwrite'))
  }

  rmRastIfExists('tmp__map')
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


#'amIsotropicTraveTime
#'@export
amIsotropicTravelTime<-function(inputFriction,inputHf,inputStop=NULL,outputDir=NULL,outputCumulative,maxCost,minCost=NULL){
  amDebugMsg('amIsotropicTravelTime')
  amParam=list(
    input=inputFriction,
    output=outputCumulative,
    start_points=inputHf,
    stop_points=inputStop,
    outdir=outputDir,
    max_cost=maxCost 
    )
  amParam<-amParam[!sapply(amParam,is.null)]
  execGRASS('r.cost',
    parameters=amParam,
    flags='overwrite'
    )
  amCleanTravelTime(outputCumulative,maxCost,minCost) 
}

#'amAnisotropicTravelTime 
#'@export
amAnisotropicTravelTime<-function(inputSpeed,inputHf,inputStop=NULL,outputDir=NULL,outputCumulative, returnPath,maxCost,minCost=NULL){
  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
  flags<-flags[!flags %in% character(1)]
  
  amParam=list(
    elevation=config$mapDem,
    friction=inputSpeed,
    output=outputCumulative,
    start_points=inputHf,
    stop_points=inputStop,
    outdir=outputDir,
    memory=100,
    max_cost=maxCost # max cost in seconds.
    )
 

  write(jsonlite::toJSON(amParam),config$pathLogTest)

  amParam<-amParam[!sapply(amParam,is.null)]
  
  execGRASS('r.walk.accessmod',
    parameters=amParam,
    flags=flags
    ) 
  amCleanTravelTime(outputCumulative,maxCost,minCost) 
  message(amParam)
}

#'amCircularTravelDistance
#'@export
amCircularTravelDistance<-function(inputHf,outputBuffer,radius){
  execGRASS('v.to.rast',input=inputHf,output='tmp_buffer',use='val',value=1,flags='overwrite')
  execGRASS('r.buffer',input='tmp_buffer',output=outputBuffer,distances=radius, flags='overwrite')
  # create one unique zone.
  expr=paste(outputBuffer,'=if(!isnull(',outputBuffer,'),1,null())')
  execGRASS('r.mapcalc',expression=expr,flags='overwrite')
}


#'amReferralTable
#'@export
amReferralTable<-function(session=shiny:::getDefaultReactiveDomain(),inputSpeed,inputFriction,inputHf,inputHfTo,inputTableHf,inputTableHfTo,idField,idFieldTo,labelField,labelFieldTo,typeAnalysis,resol,dbCon, unitCost=c('s','m','h'),unitDist=c('m','km'),outReferral,outNearestDist,outNearestTime){

  #TODO: describe input and what is returned.

  # check the clock
  timeCheckAll<-system.time({
    # set increment for the progress bar.
    incN=0
    inc=90/nrow(inputTableHf)
    ## subset value for table formating.
    #labelFrom <- inputTableHf[[labelField]]
    #labelTo <- inputTableHfTo[[labelFieldTo]]
    #indexFrom <- inputTableHf[[idField]]
    #indexTo <- inputTableHfTo[[idFieldTo]]

    # set output table header label
    hIdField <- paste0('from','__',amSubPunct(idField)) # amSubPunt to avoid unwanted char (accent, ponctuation..)
    hLabelField <- paste0('from','__',amSubPunct(labelField))
    hIdFieldTo <- paste0('to','__',amSubPunct(idFieldTo))
    hLabelFieldTo <- paste0('to','__',amSubPunct(labelFieldTo))
    hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idFieldTo))
    hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelFieldTo))
    hDistUnit <-paste0('distance','_',unitDist)
    hTimeUnit <- paste0('time','_',unitCost)

    # Create destination HF subset (To). 
    # NOTE: this has already be done outside for other functions.. but for coherence with origin HF (From) map, which need to be subseted in the loop, we also subset destination HF here.
    qSqlTo<-paste("cat IN (",paste0(inputTableHfTo$cat,collapse=','),")")
    execGRASS("v.extract",flags=c('overwrite'),input=inputHfTo,where=qSqlTo,output='tmp_ref_to')
  # cost and dist from one to all selected in table 'to'
  for(i in inputTableHf$cat){  
    timeCheck<-system.time({
      incN=incN+1
      qSqlFrom<-paste("cat==",i)
      # create temporary origine facility map (from) 
      execGRASS("v.extract",flags=c('overwrite'),input=inputHf,where=qSqlFrom,output='tmp__ref_from')
      # NOTE: only extract coordinate instead ? No.. we need points in network. 
      # create cumulative cost map for each hf : iso or aniso
      switch(typeAnalysis,
        'anisotropic'=amAnisotropicTravelTime(
          inputSpeed=inputSpeed,
          inputHf='tmp__ref_from',
          inputStop='tmp_ref_to',
          outputCumulative='tmp__cost', 
          outputDir='tmp__ref_dir',
          returnPath=FALSE,
          maxCost=0
          ),
        'isotropic'=amIsotropicTravelTime(
          inputFriction=inputFriction,
          inputHf='tmp__ref_from',
          inputStop='tmp_ref_to',
          outputCumulative='tmp__cost',
          outputDir='tmp__ref_dir',
          maxCost=0
          )
        )
      # extract time cost V1 = hf cat dest; V2 = time to reach hf
      refTime=execGRASS(
        'v.what.rast',
        map='tmp_ref_to',
        raster='tmp__cost',
        flags='p',
        intern=T
        )%>%
      gsub('\\*',NA,.) %>%
      na.omit %>%
      read.table(text=.,sep='|')
      # rename grass output
      names(refTime)<-c('tcat',hTimeUnit)
      #unit transformation 
      if(!unitCost =='s'){
        div<-switch(unitCost,
          'm'=60,
          'h'=3600,
          'd'=86400
          )
        refTime[hTimeUnit]<-refTime[hTimeUnit]/div
      }
      refTime$cat=i
      # extract network to compute distance
      execGRASS('r.drain',
        input='tmp__cost',
        direction='tmp__ref_dir',
        output='tmp__drain',
        drain='tmp__drain',
        flags=c('overwrite','c','d'),
        start_points='tmp_ref_to'
        )
      # create new layer with start point as node
      execGRASS('v.net',
        input='tmp__drain',
        points='tmp__ref_from',
        output='tmp__net_from',
        node_layer='2',
        operation='connect',
        threshold=resol-1,
        flags='overwrite'
        )
      # create new layer with stop points as node
      execGRASS('v.net',
        input='tmp__net_from',
        points='tmp_ref_to',
        output='tmp__net_all',
        node_layer='3',
        operation='connect',
        threshold=resol-1,
        flags='overwrite'
        )
      # extrad distance for each end node.
      execGRASS('v.net.distance',
        input='tmp__net_all',
        output='tmp__net_dist',
        from_layer='3', # calc distance from all node in 3 to layer 2 (start point)     
        to_layer='2',
        intern=T,
        flags='overwrite'
        )
      # read attribute table of distance network.

      refDist<-dbReadTable(dbCon,'tmp__net_dist')
      # rename grass output
      names(refDist)<-c('tcat','cat',hDistUnit)
      # distance conversion
      if(!unitDist=='m'){
        div<-switch(unitDist,
          'km'=1000
          )
        refDist[hDistUnit]<-refDist[hDistUnit]/div
      }

      # using data.table. TODO: convert previouse data.frame to data.table.
      refTime<-as.data.table(refTime)
      setkey(refTime,cat,tcat)
      refDist<-as.data.table(refDist)
      setkey(refDist,cat,tcat)
      refTimeDist <- refDist[refTime]

      #create or update table
      if(incN==1){
        ref=refTimeDist
      }else{
        ref<-rbind(ref,refTimeDist)
      }
      # remove tmp map
      rmRastIfExists('tmp__*')
      rmVectIfExists('tmp__*')
    })
    amUpdateProgressBar(session,'cumulative-progress',inc*incN)
    print(timeCheck)
  }

# set key to ref
  setkey(ref,cat,tcat)

  # Remove tmp map
  rmVectIfExists('tmp_*')

  # mergin from hf subset table and renaming.
  valFrom<-inputTableHf[inputTableHf$cat %in% ref$cat, c('cat',idField,labelField)]
  names(valFrom)<-c('cat',hIdField,hLabelField)
  valFrom<-as.data.table(valFrom)
  setkey(valFrom,cat)

  valTo<-inputTableHfTo[inputTableHfTo$cat %in% ref$tcat,c('cat',idFieldTo,labelFieldTo)]
  names(valTo)<-c('tcat',hIdFieldTo,hLabelFieldTo)
  valTo<-as.data.table(valTo)
  setkey(valTo,'tcat')

  setkey(ref,cat)
  ref<-ref[valFrom]
  setkey(ref,tcat)
  ref<-ref[valTo]
  # set column subset and order
  #refOut<-ref[,c(hIdField,hIdFieldTo,hDistUnit,hTimeUnit,hLabelField,hLabelFieldTo),with=F]
  refOut<-ref[,c(
    hIdField,
    hLabelField,
    hIdFieldTo,
    hLabelFieldTo,
    hDistUnit,
    hTimeUnit
    ),with=F]

  # set expression to evaluate nested query by group
  expD<-parse(text=paste0(".SD[which.min(",hDistUnit,")]"))
  expT<-parse(text=paste0(".SD[which.min(",hTimeUnit,")]"))

  # Extract nearest feature by time and distance.
  refNearestDist<-refOut[,eval(expD),by=hIdField]
  refNearestTime<-refOut[,eval(expT),by=hIdField]

  })
 # Return meta data
  meta<-list(
    'Function'='amReferralTable',
    'AccessMod revision'=amGetVersionLocal(),
    'Date'=amSysTime(),
    'Timing'=as.list(timeCheckAll)$elapsed,
    'Iterations'=nrow(inputTableHf),
    'Arguments'=list(
      'input'=list(
        'map'=list(
          'cost'=list(
            'speed'=inputSpeed,
            'friction'=inputFriction
            ),
          'facilities'=list(
            'from'=inputHf,
            'to'=inputHfTo
            )
          ),
        'table'=list(
          'cat'=list(
            'from'=inputTableHf$cat,
            'to'=inputTableHfTo$cat
            ),
          'names'=list(
            'from'=names(inputTableHf),
            'to'=names(inputTableHfTo)
            )
          )
        ),
      'analysis'=typeAnalysis,
      'unit'=list(
        'distance'=unitDist,
        'cost'=unitCost
        ),
      'resol'=resol
      ),
    'Output'=list(
      outReferral,
      outNearestDist,
      outNearestTime
      ) 
    )

  dbWriteTable(dbCon,outReferral,refOut,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestDist,refNearestDist,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestTime,refNearestTime,overwrite=T,row.names=F)

 
}

#' amGetRasterSum
#' 
#' Extract cells sum using r.univar
#' 
#' @param rasterMap grass raster map name
#' @return cells sum
#' @export
amGetRasterSum<-function(rasterMap){
  s<-unlist(
    strsplit(
      x=grep('sum=', execGRASS('r.univar',map=rasterMap,flags='g',intern=T),value=T),
      split='='
      ) 
    )[[2]] 
  as.numeric(s)
}

#' amGetRasterStat
#' 
#' Extract cells stat using r.univar
#' 
#' @param rasterMap grass raster map name
#' @param stat Stat to compute. Should be in c('n','cells','max','mean','stdev','coeff_var','null_cells','min','range','mean_of_abs','variance','sum') 
#' @return cells stat
#' @export
amGetRasterStat<-function(rasterMap,stat=c('n','cells','max','mean','stddev','coeff_var','null_cells','min','range','mean_of_abs','variance','sum')){
  stat=match.arg(stat)
  s<-unlist(
    strsplit(
      x=grep(paste0(stat,'='), execGRASS('r.univar',map=rasterMap,flags='g',intern=T),value=T),
      split='='
      ) 
    )[[2]] 
  as.numeric(s)
}



#    #' Rescale friction  map
#    #' @param inputMask Set a mask to limit computation
#    #' @param inputFriction AccessMod frction map (time to go cross a cell on flat surface)
#    #' @return name of the raster map computed
#    #' @export
#    amScalingCoef_Friction <- function(inputMask,inputFriction){
#      tmpName=paste0("tmp_coef_friction")
#      if(!is.null(inputMask)) execGRASS('r.mask',raster=inputMask,flags='overwrite')
#      execGRASS('r.rescale.eq',flags='overwrite',input=inputFriction,output=tmpName,to=c(0L,100L))
#      exp=paste0(tmpName,"=100-",tmpName)
#      execGRASS('r.mapcalc',expression=exp,flags='overwrite')
#      if(!is.null(inputMask)) execGRASS('r.mask',flags='r')
#      return(tmpName) 
#    }
#




#' Update candidates layer by soustracting exclusion layer 
#' 
#' @param inputCandidates Layer of candidates on which exclude cells
#' @param inputMap Layer of exclusion
#' @param inputMapType Type of data for the layer of exclusion: vector or raster
#' @param distance Distance of the buffer. If zero, use vector as exclusion mask
#' @param keep Selection strategy : keep value 'inside' or 'outside' the buffer
#' @param newLayer Boolean : reate new candidates or update
#' @return Name of the resulting map
amScalingUp_candidatesExclusion <- function(
  inputCandidates,
  inputMap,
  inputMapType=c('vector','raster'),
  distance=1000,
  keep=c('inside','outside'),
  newLayer=FALSE,
  verbose=TRUE
  ){
  stopifnot(is.numeric(distance))
  keep <- match.arg(keep)
  inputMapType <- match.arg(inputMapType)

  tmpExcl <- amRandomName('tmp_exclusion')
  tmpExclBuffer <- amRandomName('tmp_exclusion','buffer')
  outputCandidates <- ifelse(newLayer,amRandomName('candidates'),inputCandidates)

  # Convert raster map
  if(inputMapType=='vector'){
    execGRASS('v.to.rast',input=inputMap,output=tmpExcl,use='val',value=1,flags='overwrite')
  }else{
    execGRASS('g.copy',raster=c(inputMap,tmpExcl)) 
  }

  # Create buffer if necessary
  if(distance>0){
    execGRASS('r.buffer',input=tmpExcl,output=tmpExclBuffer,distances=c(distance),flags='overwrite')
  }else{
    execGRASS('g.rename',raster=c(tmpExcl,tmpExclBuffer),flags='overwrite')
  }

  # If this is a new layer, create a new outputCandidate based on tmpExclBuffer or
  # update existing one.
  if(newLayer){
    if(keep == 'inside'){
      exp = sprintf("%s = if(!isnull(%s),1,null())",outputCandidates,tmpExclBuffer)
    }else{
      exp = sprintf("%s = if(isnull(%s),1,null())", outputCandidates, tmpExclBuffer)
    }
  }else{
    if(keep == "inside"){
      exp = sprintf("%s = if(!isnull(%s),%s,null())",outputCandidates, tmpExclBuffer, outputCandidates)
    }else{
      exp = sprintf("%s = if(isnull(%s),%s,null())",outputCandidates, tmpExclBuffer, outputCandidates)
    } 
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
  }
  # Remove temporary raster
  rmRastIfExists(c(tmpExcl,tmpExclBuffer))  
  if(verbose) amDebugMsg('amScalingUp_candidatesExclusion done. Output candidates:',outputCandidates)

  return(outputCandidates)
}




#' Update candidates layer by soustracting exclusion layer
#' @param tableExclusion Data.frame with *layer* name, *type* (vector or raster), optional *buffer*, and *method* (keep *outside* or *inside* area)
#' @param candidatesLayer Raster layer of candidates
#' @return List with candidates count: candidates avaiable before soustraction 'cBefore' and after soustraction 'cAfter'.
amScalingUp_candidatesExclusionFromTable <- function(tableExclusion,candidatesLayer,verbose=TRUE){
  noMoreCandidates = FALSE
  tbl = tableExclusion
  cBeforeFirst = NULL
  cAfter = NULL
  for(i in 1:nrow(tbl)){

    l = tbl[i,'layer']
    t = tbl[i,'type'] 
    d = tbl[i,'buffer']*1000
    m = tbl[i,'method']

    if(noMoreCandidates){
      if(verbose){
        msg <- sprintf("No more candidates, skipping layer %s (method=%s;buffer=%s;type=%s)",l,m,d,t) 
        amDebugMsg(msg)
      } 
    }else{
      cBefore <- amGetRasterStat(candidatesLayer,'n')
      if(i==1){
        cBeforeFirst=cBefore
      }
      amScalingUp_candidatesExclusion(
        inputCandidates=candidatesLayer,
        inputMap=l,
        inputMapType=t,
        distance=d,
        keep=m,
        newLayer=FALSE,
        verbose=verbose)
      cAfter <- amGetRasterStat(candidatesLayer,'n')
      msg <- sprintf('Keep candidates %s a buffer of %s meters using geometry of %s. %s candidates removed. %s candidates left.',m,d,l,cBefore-cAfter,cAfter)
      if(verbose){
        amDebugMsg(msg)
      } 
      if(isTRUE(cAfter < 1 || length(cAfter) == 0)){
        noMoreCandidates <- TRUE
        cAfter <- 0
      }
    }
  }
  count <- list(cBefore=cBeforeFirst, cAfter=cAfter)
  return(count)
}








#' Compose random char name
#' @param prefix Prefix of the resulting string
#' @param suffix Suffix of the resultiing string
#' @param n Number of random letters
#' @return String with random letters
#' @export
amRandomName <- function(prefix=NULL,suffix=NULL,n=20){
  prefix = amSubPunct(prefix,'_')
  suffix = amSubPunct(suffix,'_')
  rStr = paste(letters[round(runif(n)*24)],collapse="")
  str = c(prefix,rStr,suffix)
  paste(str,collapse="_")
}



    #' Calc travel time on existing vector, create a rescaled map
    #' @param inputMask Set a mask to limit computation
    #' @param inputVector Existing vector from where start analysis
    #' @param inputSpeed Speed and transport mod map in accessmod format
    #' @param inputFriction AccessMod friction map
    #' @param typeAnalysis Type of analysis : anisotropic or isotropic
    #' @param inverse Inverse the scale 0 - 100 > 100 -0
    #' @param position Position of the layer
    #' @return name of the rescaled raster map
    #' @export
    amScalingUpCoef_TravelTime <- function(inputMask=NULL,inputVector,inputSpeed,inputFriction,typeAnalysis,inverse=FALSE,position=1){
      if(!is.null(inputMask)) execGRASS('r.mask',raster=inputMask,flags='overwrite')

     
      tmpOut <- amRandomName("tmp_coef_travel_time",inputVector)
      tmpA <- amRandomName('tmp_')
      tmpB <- amRandomName('tmp_')


      # create a cumulative cost map on the whole region, including new hf sets at the end of this loop.
      switch(typeAnalysis,
        'anisotropic'= amAnisotropicTravelTime(
          inputSpeed       = inputSpeed,
          inputHf          = inputHf,
          outputCumulative = tmpA,
          returnPath       = TRUE,
          maxCost          = 0
          ),
        'isotropic'= amIsotropicTravelTime(
          inputFriction    = mapFriction,
          inputHf          = inputHf,
          outputCumulative = tmpA,
          maxCost          = 0
          )
        )
      execGRASS('r.rescale.eq',flags='overwrite',input=tmpA,output=tmpB,to=c(0L,100L))
      
      if(inverse){
      exp =  sprintf("%s = 100 - %s",tmpOut,tmpB)
      }else{
      exp = sprintf("%s = %s",tmpOut,tmpB)
      }
      execGRASS('r.mapcalc',expression=exp,flags="overwrite")

      rmRastIfExists(c(tmpB,tmpA))
      if(!is.null(inputMask)) execGRASS('r.mask',flags='r')
      return(tmpOut)
    }


    #' Create a rescaled cumulative cost map
    #' @param inputMask Set a mask to limit computation
    #' @param inputPop Population map
    #' @param radiusKm Radius of the analysis
    #' @param mapResolution Map resolution in meter
    #' @param inverse Inverse the scale 0 - 100 > 100 -0
    #' @param position Position of the layer
    #' @return name of the rescaled raster map
    #' @export
amScalingUpCoef_pop<-function(inputMask=NULL,inputPop,radiusKm,mapResolution,inverse=FALSE,position=1){
  tmpOut <- amRandomName('tmp_coef_pop_density',inputPop)
  tmpA <- amRandomName('tmp_')
  tmpB <- amRandomName('tmp_')

  neighbourSize <- round((abs(radiusKm)*1000)/mapResolution)
  useMovingWindow <- isTRUE(neighbourSize != 0)
  # r.neighbors  need odd number
  if(isTRUE(useMovingWindow && neighbourSize %% 2 ==0)){
    message('Scaling up. Neighbour size is not odd (',neighbourSize,')., Added one cell to, as required by moving window algorithm.')
    neighbourSize <- neighbourSize +1
  }

  if(!is.null(inputMask)) execGRASS('r.mask',raster=inputMask,flags='overwrite')
  if(useMovingWindow){
    # create a density map using a  moving window sum of population on a radius
    execGRASS('r.neighbors',flags=c('c','overwrite'),input=inputPop,output=tmpA,method='sum',size=neighbourSize)
  }else{
    exp = sprintf("%s = %s",tmpA,inputPop)
    execGRASS('r.mapcalc',expression=exp)
  }
  execGRASS('r.rescale.eq',flags='overwrite',input=tmpA,output=tmpB,to=c(0L,100L))

  if(inverse){
    exp =  sprintf("%s = 100 - %s",tmpOut,tmpB)
  }else{
    exp = sprintf("%s = %s",tmpOutm,tmpB)
  }
  execGRASS('r.mapcalc',expression=exp,flags="overwrite")

  rmRastIfExists(c(tmpB,tmpA))

  if(!is.null(inputMask)) execGRASS('r.mask',flags='r')
}


#' Create a rescaled distance map
#' @param inputMask Set a mask to limit computation
#' @param inputMap A raster or vector  map from which compute euclidean distance. Vector map will be rasterized.
#' @param inputMapType Set if the input map is a vector or a raster
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @param position Position of the layer
#' @return name of the rescaled raster map
#' @export
amScalingUpCoef_dist<-function(inputMask=NULL,inputMap,inputMapType=c('vector','raster'),inverse=FALSE,position=1){

  inputMapType <- match.arg(inputMapType)
  tmpOut <- amRandomName("tmp_coef_dist",inputMap)
  tmpA <- amRandomName('tmp_')
  tmpB <- amRandomName('tmp_')
  tmpC <- amRandomName('tmp_')


  if(!is.null(inputMask)) execGRASS('r.mask',raster=inputMask,flags='overwrite')

  if(inputMapType=='vector'){
    execGRASS('v.to.rast',input=inputMap,output=tmpA,use='val',value=0,flags='overwrite')
  }else{
    expr <- sprintf("%s=if(isnull(%s,null(),0))",tmpA,inputMap) 
    execGRASS('r.mapcalc',expression=expr) 
  }

  execGRASS('r.grow.distance',input=tmpA,distance=tmpB,metric="euclidean",flags="overwrite") 
  execGRASS('r.rescale.eq',flags='overwrite',input=tmbB,output=tmpC,to=c(0L,100L))

  if(inverse){
    exp =  sprintf("%s = 100 - %s",tmpOut,tmpC)
  }else{
    exp = sprintf("%s = %s",tmpOutm,tmpC)
  }
  execGRASS('r.mapcalc',expression=exp,flags="overwrite")


rmRastIfExists(c(tmpB,tmpA,tmpC))

  if(!is.null(inputMask)) execGRASS('r.mask',flags='r')
  return(tmpOut)
}


#' Create a rescaled version of generic suitability map
#' @param inputMask Set a mask to limit the computation
#' @param inputMap The raster map to convert
#' @param position Thw position of the layer
#' @param inverse Inverse the scale 0 - 100 > 100 -0
#' @return name of the rescaled map
#' @export
amScalingUpCoef_generic <- function(inputMask=NULL,inputMap,inverse=FALSE,position=1){

  tmpOut <- amRandomName("tmp_coef_generic",inputMap)
  tmpA  <- amRandomName("tmp_")

  if(!is.null(inputMask)) execGRASS('r.mask',raster=inputMask,flags='overwrite') 
  execGRASS('r.rescale.eq',flags='overwrite',input=inputMap,output=tmpA,to=c(0L,100L))

 if(inverse){
    exp =  sprintf("%s = 100 - %s",tmpOut,tmpA)
  }else{
    exp = sprintf("%s = %s",tmpOut,tmpA)
  }
  execGRASS('r.mapcalc',expression=exp,flags="overwrite")


  # r.rescale.eq produce a reclass map of input to preserve disc space.
  if(!is.null(inputMask)) execGRASS('r.mask',flags='r')
  return(tmpOut)
}



#' Create composite index based on input coef
#' @param coefLayer Components of the composite index
amScalingUpCoef_composite <- function(coefLayers){
  nLayer = length(coefLayers)



}


#' Create temporary candidate cells based on non-null raster value
#' @param raster Raster map to evaluate
#' @return Name of the candidates layer
#' @export
amScalingUp_candidatesCreateFromRaster <- function(raster=NULL){
  candidates = amRandomName('tmp','candidates')
  execGRASS('g.copy',raster=c(raster,candidates),flags='overwrite')
  exp = paste(candidates,"= if(!isnull(",candidates,"),1,null())")
  execGRASS('r.mapcalc',expression=exp,flags="overwrite") 
  message(sprintf("Candidates cells '%s' created",candidates))
  return(candidates)
}


#' amScalingUp
#' @export
amScalingUp<-function(
  session=shiny:::getDefaultReactiveDomain(),
  inputSpeed, # name of speed map based on scenario
  inputFriction, # name of friction map based on scenario
  inputPop, # name of input population or population residual
  inputLandCover, # name of landcover layer. NOTE: not usefull anymore ?
  inputHf, # name of input facilities layer.
  inputTableHf, # table of facilities
  inputTableCap, # table of capacities
  inputTableExclusion, # table of exclusion layers
  inputTableSuitability, # table of suitability index items
  useExistingFacilities,
  typeAnalysis, # type of analysis : iso or anisotropic
  nFacilities, # max number of facilities
  removePop, # toggle population removing
  maxProcessingTime, # maximum processing time
  outputFacilities, # name of the output facilities layer
  outputTable, # output table # NOTE: this table could be appened to hf layer ? l
  dbCon
  ){


  #
  # Initialisation
  #

  # init increment for progress bar
  inc <- 90/nFacilities 
  incN <- 0 
  # Reevaluate exclusion at each iteration ?
  doDynExclusion <- outputFacilities %in% inputTableExclusion$layer 
  # Reevaluate coefficient at each iteration ?
  doDynCoef <- outputFacilities %in% inputTableSuitability$layer
  # Reset useExistingFacilities using also nrow table hf > 0
  useExistingFacilities <- isTRUE(nrow(inputTableHf) > 0 && useExistingFacilities)
  # Set candidates cells, based on friction or speed map.
  candidates <- amScalingUp_candidatesCreateFromRaster(
    raster=ifelse(typeAnalysis=="anisotropic",inputSpeed,inputFriction)
    )
  # Compute first exclusion map
  if(!useExistingFacilities){ 
    # Remove hf layer from exclusion procedure: it does't contain any HF. yet.
    tbl <- inputTableExclusion[! inputTableExclusion$layer %in% outputFacilities,]
  }else{
    tbl <- inputTableExclusion 
  }

candidatesCount <-  amScalingUp_candidatesExclusionFromTable(
    tableExclusion=tbl,
    candidatesLayer=candidates
    )

if(candidatesCount$cAfter < 1){
 stop('No candidate left after initial exclusion. Please check the exclusion table for inconsistencies.')
}



browser()


return()
  # to set as optional input
  # set progressbar info

  progTot=nTry*nFacilities
  progInc=90/progTot
  progNum=0
  nsRes=gmeta()$nsres





  # If necessary, prepare temporary existing facility based on selected hf
  # after the first iteration, useExistingFacilities will always be TRUE
  useExistingFacilities <- isTRUE(!is.null(inputHf) && nrow(inputTableHf) > 0)
  if(useExistingFacilities){
    execGRASS('v.extract',input=inputHf,output='tmp_hf_all',cats=paste(inputTableHf$cat,collapse=','))
  }

  
  # create temp version of population
  execGRASS('g.copy',raster=c(inputPop,'tmp_pop'),flags='overwrite')

  # filter lcv to create initial sampling grid. Set


  #lcvClassToIgnore=c(1,3,4,5)


  if(length(lcvClassToIgnore)>0){
    exp = paste0('tmp_candidate_grid=if(',paste0(paste0(inputLandCover,"=="),lcvClassToIgnore,collapse='|'),',null(),1)')
    execGRASS('r.mapcalc',expression=exp,flags='overwrite') 
  }else{
    exp = paste0('tmp_candidate_grid=if(!isnull(%s),i)') 
  }



  if(length(lcvClassToIgnore)>0){
    tmpFile<-tempfile()
    lcvCat<-read.table(text=execGRASS('r.category',map=inputLandCover,intern=T,separator='comma'),sep=',')$V1
    lcvCat=lcvCat[! lcvCat %in% lcvClassToIgnore] 
    val=paste(paste(lcvCat,collapse=' '),'= 1')
    write(val,tmpFile)
    execGRASS('r.reclass',input=inputLandCover,output='tmp_candidates_grid',rules=tmpFile,flags='overwrite')
  }else{
    # if no filter set, just copy the raster under a tmp name.
    execGRASS('g.copy',raster=c(inputLandCover,'tmp_candidates_grid'),flags='overwrite')
  }


  # for each new HF optimum location
timing<-system.time({
  for(i in 1:nFacilities){
    rmRastIfExists('MASK')




    if(!useExistingFacilities && i==1){
      fTmp<-amScalingCoef_Friction(
        inputMask='tmp_candidate_grid',
        inputFriction=inputFriction)

      pTmp<-amScalingCoef_Pop(
        inputMask=fTmp,
        inputPop='tmp_pop',
        radiusKm=10,
        mapResolution=nsRes)

    }








 
    # filter candidates grid: if non-null value exists in tmp cumulative map or in tmp pop, keep original candidates cell value, else null.
    exp <- paste("tmp_candidates_grid_residual=if(isnull(tmp_pop)|isnull(tmp_cumul),null(),tmp_candidates_grid)")
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    # limit next comuptation to residual candidates grid.
    execGRASS('r.mask',raster='tmp_candidates_grid_residual',flags='overwrite')
    
    if(useMovingWindow){
      # create a density map using a  moving window sum of population on a radius
      execGRASS('r.neighbors',flags=c('c','overwrite'),input='tmp_pop',output='tmp_pop_density',method='sum',size=neighbourSize)
    }else{
      execGRASS('g.copy',raster=c('tmp_pop','tmp_pop_density'))
    }
    # create rescaled base map for priority index
    execGRASS('r.rescale.eq',flags='overwrite',input="tmp_pop_density",output="tmp_pop_density_rescale",to=c(0L,100L))
    execGRASS('r.rescale.eq',flags='overwrite',input=inputFriction,output="tmp_friction_rescale",to=c(0L,100L))
    execGRASS('r.rescale.eq',flags='overwrite',input="tmp_cumul",output="tmp_cumul_rescale",to=c(0L,100L))
    # set weight. NOTE: Could be an option?
    weightFriction=0.4
    weightPopDensity=1
    weightTravelTime=0.01
    weightSum=weightFriction+weightTravelTime+weightPopDensity
    # set the candidate base map 
    exp=paste(
      "tmp_candidates_base=(",
      "(100-tmp_friction_rescale)*",weightFriction,
      "+tmp_pop_density_rescale*",weightPopDensity,
      "+tmp_cumul_rescale*",weightTravelTime,
      ")/",weightSum)
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    # get 99th percentil
    candidates99Percentile<-as.numeric(
      unlist(
        strsplit(grep("percentile_99",
            execGRASS('r.univar',map='tmp_candidates_base',flags=c('e','g'),percentile=99,intern=T)
            ,value=T),
          '=')
        )[[2]])
    # filter 99th percentile
    exp=paste("tmp_candidates_pool=if(tmp_candidates_base>",candidates99Percentile,",tmp_candidates_base,null())")
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    # export as vector points
    execGRASS('r.to.vect',flags='overwrite',type='point',input='tmp_candidates_pool',output='tmp_candidates_pool',column="amPriorityIndex")
    # get max 10 highest priority hf
    candidatesTable<-dbGetQuery(dbCon,'SELECT * FROM tmp_candidates_pool ORDER BY amPriorityIndex DESC LIMIT 10')
    rmRastIfExists('MASK')
    # loop on HF to find the best candidate
    candidatesEval<-data.frame()
    for(j in candidatesTable$cat ){
      progNum=progNum+1
      hfTest<-paste0('tmp_hf_test_',j)
      hfTestCumul<-paste0('tmp_hf_test_catch_',j)
      
      execGRASS('v.extract',input='tmp_candidates_pool',output=hfTest,cats=paste(j),type='point',flags='overwrite')

      # For this candidate, analyse cumulative cost map without mask
      switch(typeAnalysis,
        'anisotropic'= amAnisotropicTravelTime(
          inputSpeed       = inputSpeed,
          inputHf          = hfTest,
          outputCumulative = hfTestCumul,
          returnPath       = TRUE,
          maxCost          = maxCost,
          minCost          = NULL),
        'isotropic'= amIsotropicTravelTime(
          inputFriction    = mapFriction,
          inputHf          = hfTest,
          outputCumulative = hfTestCumul,
          maxCost          = maxCost,
          minCost          = NULL
          )
        )
      # compute integer version of cumulative cost map to use with r.univar, by minutes
      expr=paste(hfTestCumul,'=int(',hfTestCumul,')')
      execGRASS('r.mapcalc',expression=expr,flags='overwrite')
      # compute zonal statistic : time isoline as zone
      tblPopByZone<-read.table(
        text=execGRASS(
          'r.univar',
          flags  = c('g','t','overwrite'),
          map    = 'tmp_pop',
          zones  = hfTestCumul,
          intern = T
          ),sep='|',header=T)
      # calculate cumulated sum of pop at each zone
      tblPopByZone$cumSum<-cumsum(tblPopByZone$sum)
      tblPopByZone<-tblPopByZone[c('zone','sum','cumSum')]
      # After cumulated sum, order was not changed, we can use tail/head to extract min max
      totalPop<-tail(tblPopByZone,n=1)$cumSum
      # Set capacity using capacity table
      resCap<-inputTableCap[totalPop<=as.integer(inputTableCap$max) & totalPop>as.integer(inputTableCap$min), ]
      # If nothing match, stop the process.
      if(!nrow(resCap)==1) stop(paste('amScalingUp did not found a suitable capacity value for a new facility in the provided capacity table. Please make sure that one (and only one) interval min/max can handle a total population potential coverage of',totalPop))
      # find the zone that overpass or equal capacity
      #timeLimitCap<-tblPopByZone[as.integer(resCap$capacity) <= tblPopByZone$cumSum,'zone'][1]
      # check time vs pop correlation : negative value = covered pop decrease with dist; positive value = covered pop increase with dist
      corPopTime <- cor(tblPopByZone$zone,tblPopByZone$sum)
      # bind current summary to previous
      candidatesEval<-rbind(
        candidatesEval,
        data.frame(
          amProcessingOrder    =  i,
          amRasterCumul        =  hfTestCumul,
          amVectorPoint        =  hfTest,
          amCorrPopTime        =  corPopTime,
          amTimeMax            =  maxCost,
          amPopTimeMax         =  totalPop,
          amCapacity           =  resCap$capacity,
          #amTimeLimitCapacity  =  timeLimitCap,
          amLabel              =  resCap$label
          )
        )
    amUpdateProgressBar(session,"cumulative-progress",round(progInc*progNum))
    }
   
    # select best facility
    # criteria: more people covered under time max
    # other criterias could be :
    #     min time to reach a capacity value. Ex. reached capacity of 4000 in 3 minutes: keep only 180 seconds from cumulativecost
    #     most negative correlation between population covered and time traveled. Ex. corr=-0.4, the farest we go, the less we cover population.
    #     ... A lot of choices. What is the best one ??
    candidatBest<-candidatesEval[order(candidatesEval$amPopTimeMax),][1,]
    # get the name of choosen facility vector point
    vectBest<-paste(candidatBest$amVectorPoint)
    # population covered
     if(removePop){
       #TODO: set original population as input, and calc percentage covered at each iteration
       # update population residual: remove potentialy covered population.
       #exp<-paste("tmp_mask_pop=if(",candidatBest$amRasterCumul,">=",candidatBest$amTimeLimitCapacity,",tmp_pop,null())")
       exp<-paste("tmp_pop=if(isnull(",candidatBest$amRasterCumul,"),tmp_pop,null())")
       execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    }
    # update column for select hf
    sql=paste("SELECT * FROM",vectBest)
    candidatTable<-dbGetQuery(dbCon,sql)
    tblNewHf<-cbind(candidatTable,candidatBest[,c('amProcessingOrder','amCorrPopTime','amTimeMax','amPopTimeMax','amLabel','amCapacity')])
    dbWriteTable(dbCon,vectBest,tblNewHf,overwrite=TRUE)
    if(i==1){
      rmVectIfExists(outputFacilities)
      execGRASS('g.copy',vector=c(vectBest,outputFacilities))
    }else{
      execGRASS('v.patch',flags=c('e','a','overwrite'),input=vectBest,output=outputFacilities)
    }
    # Add new HF to existing one for cumulative filtering
    execGRASS('v.patch',input=vectBest,output='tmp_hf_all',flags=c('overwrite','a'))
  }
  # store only cat in final vector, and attribute table as separate file. Why? shapefile limited column naming length.  
  outTableAttr<-dbGetQuery(dbCon,paste('SELECT * FROM',outputFacilities))
  dbWriteTable(dbCon,outputFacilities,outTableAttr['cat'],overwrite=TRUE)
  dbWriteTable(dbCon,outputTable,outTableAttr,overwrite=TRUE)
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')
  amUpdateProgressBar(session,"cumulative-progress",round(progInc*progNum)+10)
})



}

#
#    # get max value (floored to for the comparison in mapcalc. grass does not found back the original max value in map)
#    maxVal<-floor(amGetRasterStat('tmp_pop_density','max')*1000)/1000
#    # exctact point on the max pop density 
#    exp=paste('tmp_sample_point = if(tmp_pop_density>',maxVal,',tmp_pop_density,null())')
#    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
#    if(!amGetRasterStat('tmp_sample_point')==1){
#      # NOTE: if the module found 0 location, reduce minimum distance?
#      stop('AccessMod found more than one (',amGetRasterStat('tmp_sample_point'),'points) candidate for the facility location.')
#    }
#    execGRASS('r.to.vect',type='point',input='tmp_sample_point',column='amPopSumMovingWindow',output='tmp_sample_point',flags='overwrite')
#
#

#'amCapacityAnalysis
#'@export
amCapacityAnalysis<-function(
  session=shiny:::getDefaultReactiveDomain(),
  inputSpeed,
  inputFriction,
  inputPop,
  inputHf,
  inputTableHf,
  inputZoneAdmin=NULL,
  outputPopResidual,
  outputTableHf,
  outputHfCatchment,
  catchPath=NULL,
  removeCapted=FALSE,
  vectCatch=FALSE,
  typeAnalysis,
  returnPath,
  maxCost,
  maxCostOrder=NULL,
  radius,
  hfIdx,
  capField,
  orderField=NULL,
  zonalCoverage=FALSE,
  zoneFieldId=NULL,
  zoneFieldLabel=NULL,
  hfOrder=NULL,
  hfOrderSorting=NULL,
  dbCon=NULL
  ){


# if cat is set as index, change to cat_orig
  if(hfIdx=='cat'){
    hfIdxNew='cat_orig'
  }else{
    hfIdxNew=hfIdx
  }


  #
  # Compute hf processing order
  #

    hfOrderDecreasing<-ifelse(hfOrderSorting=='hfOrderDesc',TRUE,FALSE)
  # nested call if requested order is not given by input hf table
  # hfOrder could be 'tableOrder','travelTime' or 'circlBuffer'
  # If hfOrder is not 'tableOrder' or 'circBuffer', an isotropic or anisotropic will be done.
  # In this case, typeAnalysis will be set from parent function call.

  if(!hfOrder == 'tableOrder' && ! is.null(hfOrder)){
    popWithinDist<-amCapacityAnalysis(
      inputSpeed        = inputSpeed,
      inputFriction     = inputFriction,
      inputPop          = inputPop,
      inputHf           = inputHf,
      inputTableHf        = inputTableHf,
      outputPopResidual = 'tmp_nested_p',
      outputTableHf       = "tmp_nested_hf",
      outputHfCatchment = "tmp_nested_catch",
      typeAnalysis      = ifelse(hfOrder=='circBuffer','circular',typeAnalysis),
      returnPath        = returnPath,
      radius            = radius,
      maxCost           = maxCostOrder,
      hfIdx             = hfIdx,
      capField          = capField,
      )[['capacityTable']][c(hfIdxNew,'amPopTimeMax')]
    orderId<-popWithinDist[order(
      popWithinDist$amPopTimeMax,decreasing=hfOrderDecreasing
      ),hfIdxNew]
    amMsg(session,'log',text=paste('Order process for',inputHf,'(',hfIdxNew,') will be',paste(orderId,collapse=',')))
  }else{
    orderId=unique(inputTableHf[order(inputTableHf[orderField],decreasing=hfOrderDecreasing),hfIdx])
  }

  #
  # clean and initialize object outside loop
  #
  
  
  # temp. maps
  tmpHf             <- 'tmp__h' # vector hf tmp
  tmpCost           <- 'tmp__c' # cumulative cost tmp
  tmpPop            <- 'tmp__p' # population catchment to substract
  tblOut            <- data.frame() # empty data frame for storing capacity summary
  amTtInner         <- 0 # init inner ring
  amTtOuter         <- 0 # init outer ring
  popSum            <- amGetRasterSum(inputPop) # initial population sum
  popCoveredPercent <- NA # init percent of covered population
  inc               <- 90/length(orderId) # init increment for progress bar
  incN              <- 0 # init counter for progress bar

  # create residual population 
  execGRASS('g.copy',raster=c(inputPop,outputPopResidual),flags='overwrite')

  #
  # Start loop on facilities according to defined order
  #
  for(i in orderId){
    incN=incN+1
    # extract temporary facility point
    qSql<-paste(hfIdx,"IN (",paste0("'",i,"'",collapse=','),")")
    execGRASS("v.extract",flags='overwrite',input=inputHf,where=qSql,output=tmpHf)
    # compute cumulative cost map
    switch(typeAnalysis,
      'anisotropic' = amAnisotropicTravelTime(
        inputSpeed       = inputSpeed,
        inputHf          = tmpHf,
        outputCumulative = tmpCost,
        returnPath       = returnPath,
        maxCost          = maxCost
        ),
      'isotropic' = amIsotropicTravelTime(
        inputFriction    = inputFriction,
        inputHf          = tmpHf,
        outputCumulative = tmpCost,
        maxCost          = maxCost
        ),
      'circular' = amCircularTravelDistance(
        inputHf          = tmpHf,
        outputBuffer     = tmpCost,
        radius           = radius
        )
      )
    # compute integer version of cumulative cost map to use with r.univar
    expr <- paste(tmpCost,'=int(',tmpCost,')')
    execGRASS('r.mapcalc',expression=expr,flags='overwrite')
    # compute zonal statistic : time isoline as zone
    tblPopByZone <- read.table(
      text=execGRASS(
        'r.univar',
        flags  = c('g','t','overwrite'),
        map    = outputPopResidual,
        zones  = tmpCost,
        intern = T
        ),sep='|',header=T)
    # calculate cumulated sum of pop at each zone
    tblPopByZone$cumSum <- cumsum(tblPopByZone$sum)
    tblPopByZone <- tblPopByZone[c('zone','sum','cumSum')]
    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    totalPop <- tail(tblPopByZone,n=1)$cumSum
    # check time vs pop correlation : negative value = covered pop decrease with dist; positive value = covered pop increase with dist
    corPopTime <- cor(tblPopByZone[,c('zone','sum')]) 
    # extract hf total capacity. Sum in case of hf group
    hfCap <- sum(inputTableHf[inputTableHf[hfIdx]==i,capField])
    # population in first cell
    firstCellPop <- head(tblPopByZone,n=1)$cumSum
    # get the travel time before the limit
    # first zone where pop <= hf capacity
    # if NA -> hf capacity is already overpassed before the first cumulated cost zone. 
    # E.g. In the cell where the facility is located, the population outnumber the capacity.
    zInner <- tblPopByZone[tblPopByZone$cumSum<=hfCap,c('zone','cumSum')]
    # get the travel time that overpass capacity
    # if NA -> travel time zone is too low to over pass hf capacity
    # all zones where pop > hf capacity
    zOuter <- tblPopByZone[tblPopByZone$cumSum>hfCap,c('zone','sum')]
    hfCapResidual= NA # remaining capacity in HF.
    zMaxInner = NULL # last zone where population cumulated sum is lower or egal to hf capacity
    zMaxOuter = NULL # first zone wher population cumulated sum (in outer ring) is greater (or egal) to hf capacity residual
    propToRemove = NULL # proportion of pop to remove in outer ring
    #
    # Inner ring calculation : where population cumulative sum is lower or equal facility capacity 
    #
    if(!any(is.na(zInner))&&!length(zInner$zone)==0){
      # last zone where population cumulated sum is lower or egal to hf capacity
      zMaxInner<-max(zInner$zone)
      # create temporary population inner ring mask
      expr=paste(
          tmpPop,'=if(',tmpCost,'<=',max(zInner$zone),',',incN,',null())'
          )
      execGRASS('r.mapcalc',expression=expr,flags='overwrite')
      # create population subset for the inner ring mask by removing tmp pop coverage.
      if(removeCapted){
        execGRASS('r.mask',raster=tmpPop,flags='i')
        expr=paste(
            outputPopResidual,"=",outputPopResidual
            )
        execGRASS('r.mapcalc',expression=expr,flags='overwrite')
        execGRASS('r.mask',flags='r')
      }
      # Calculate population residual
      # If hfCapResidual==0, HF can provide services exactly for the pop within this zone
      hfCapResidual=hfCap-max(zInner$cumSum)
      # If there is no residual and save catchment as vector is true,
      # extract pop catchment from raster (tmpPop) and save as final vector polygon
      if(vectCatch && hfCapResidual==0){
        tmpVectCatchOut<-amCatchPopToVect(
          idField = hfIdxNew,
          idPos   = i,
          incPos  = incN,
          tmpPop  = tmpPop,
          dbCon   = dbCon
          )
      }
    }
    #
    # reset residual :  if no inner ring has been computed, set hfCap as the value to be removed from current or next zone.
    #
    if(is.na(hfCapResidual))hfCapResidual=hfCap
    #
    # Outer ring calculation : where capacity was not full and there is population left
    #
    if(!any(is.na(zOuter)) &&  hfCapResidual>0 && nrow(zOuter)>0){
      #calculate cumulative pop count for outer ring.
      zOuter$cumSum<-cumsum(zOuter$sum)
      # if whithin outer ring, there isn't enough pop to fill hf capacity, remove all population.
      if(max(zOuter$cumSum)<=hfCapResidual){
        propToRemove=1
        hfCapResidual=hfCapResidual-max(zOuter$cumSum)
        maxZone=max(zOuter$zone)
      }else{
        # take the first ring where pop outnumber hfCapResidual #NOTE: it there a case where equality could be observed ?
        zOuter<-zOuter[zOuter$cumSum>=hfCapResidual,][1,]
        zMaxOuter=zOuter$zone
        propToRemove<-hfCapResidual/zOuter$cumSum
        hfCapResidual=0 
        maxZone=zMaxOuter
      }
      # temp pop catchment where hf's cumulative cost map is lower (take inner cell) or equal to maxZone 
      expr=paste(tmpPop,'=if(',tmpCost,'<=',maxZone,',1,null())')
      execGRASS('r.mapcalc',
        expression=expr,
        flags='overwrite')
      # calc cell with new lowered values.
      if(removeCapted){  
        expr=paste(
            'tmp__pop_residual',"=",outputPopResidual,'-',outputPopResidual,'*',tmpPop,'*',propToRemove
            )
        execGRASS('r.mapcalc',
          expression=expr,
          flags="overwrite")
        # patch them with pop residual map : priority to tmp__pop (will replace value of corresponding cell(s) in outputPopResidual)
        execGRASS('r.patch',
          input=c('tmp__pop_residual',outputPopResidual),
          output=outputPopResidual,
          flags='overwrite')
      }

      if(vectCatch){
        tmpVectCatchOut<-amCatchPopToVect(
          idField=hfIdxNew,
          idPos=i,
          incPos=incN,
          tmpPop=tmpPop,
          dbCon=dbCon 
          )
      }
    }

    #
    # population coverage analysis.
    #
    if(removeCapted){
      popCoveredPercent<-(popSum-amGetRasterSum(outputPopResidual))/popSum*100
    }

    #
    # manage length= 0 : e.g. when no pop available in cell or in travel time extent
    #
  if(length(zMaxInner)==0)     zMaxInner=NA
  if(length(zMaxOuter)==0)     zMaxOuter=NA
  if(length(propToRemove)==0)  propToRemove=NA
  if(length(hfCapResidual)==0) hfCapResidual=NA
  if(length(totalPop)==0)      totalPop=0
  if(length(firstCellPop)==0)  firstCellPop=0
  if(length(corPopTime)==0)    corPopTime=NA
 
  #
  # Output capacity table
  #
  capDf=data.frame(
    i, # id of hf / group of hf
    hfCap, # capacity from hf table
    incN, # processing order position
    corPopTime[2], # corrrelation (pearson) between time (zone) and population (sum)
    hfCapResidual, # capacity not filled
    hfCap-hfCapResidual,# capacity realised
    maxCost/60, # max allowed travel time (time)
    totalPop, # total population within max time (minutes)
    firstCellPop, # population under start cell
    popCoveredPercent, # if covered pop removed, percent of total.
    zMaxInner/60, # maximum travel time for the inner ring. below this, we have covered all patient
    zMaxOuter/60, # maximum travel time for outer ring. below this, we have covered a fraction of patient,
    propToRemove
    )
  # naming table
  names(capDf)<-c(
    hfIdxNew,
    capField,
    'amProcessingRank',
    'amCorrPopTime',
    'amCapacityResidual',
    'amCapacityRealised',
    'amTimeMax',
    'amPopTimeMax',
    'amPopFirstCell',
    'amPopCoveredPercent',
    'amTimeLimitInnerRing',
    'amTimeLimitOuterRing',
    'amPopPropRemovedOuterRing')
  # append to tblOut
  tblOut<-rbind(tblOut,capDf)
  # clean and set progress bar
  progValue<-inc*incN+10
  amUpdateProgressBar(session,"cumulative-progress",round(inc*incN)+10)
  rmRastIfExists('tmp__*')
  rmVectIfExists('tmp__*')
  tblPopByZone=NULL
  }

  #
  # optional zonal coverage using admin zone polygon
  #

  if(zonalCoverage){
    execGRASS('v.to.rast',
      input            = inputZoneAdmin,
      output           = 'tmp_zone_admin',
      type             = 'area',
      use              = 'attr',
      attribute_column = zoneFieldId,
      label_column     = zoneFieldLabel,
      flags            = c('overwrite'))

    tblAllPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags  = c('g','t','overwrite'),
        map    = inputPop,
        zones  = 'tmp_zone_admin', #
        intern = T
        ),sep='|',header=T)[,c('zone','label','sum')]

    tblResidualPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags  = c('g','t','overwrite'),
        map    = outputPopResidual,
        zones  = 'tmp_zone_admin', #
        intern = T
        ),sep='|',header=T)[,c('zone','label','sum')]

    tblPopByZone         <- merge(tblResidualPopByZone,tblAllPopByZone,by=c('zone','label'))
    tblPopByZone$covered <- tblPopByZone$sum.y - tblPopByZone$sum.x
    tblPopByZone$percent <- (tblPopByZone$covered / tblPopByZone$sum.y) *100
    tblPopByZone$sum.x=NULL
    names(tblPopByZone)<-c(zoneFieldId,zoneFieldLabel,'amPopSum','amPopCovered','amPopCoveredPercent')
  }
  if(vectCatch){
    if(!file.exists(tmpVectCatchOut))stop('Error : the output catchment area was requested but not created. Please report this bug and provide the dataset.')
    baseCatch<-gsub('.shp','',basename(tmpVectCatchOut))
    allShpFiles<-list.files(dirname(tmpVectCatchOut),pattern=paste0('^',baseCatch),full.names=TRUE)
    # sorry for this.
    for( s in allShpFiles){
      sExt <- file_ext(s)
      newPathGrass <- file.path(catchPath,paste0(outputHfCatchment,'.',sExt))
      newPath <- system(paste('echo',newPathGrass),intern=T)
      file.copy(s,newPath,overwrite=T) 
    }



    # get catchment shapefile back and clean columns
#    execGRASS('v.in.ogr',
#      input   = tmpVectCatchOut,
#      output  = outputHfCatchment,
#      flags   = c('overwrite'),
#      type    = 'boundary',
#      columns = 'cat'
#      )
# v.dissolve : sometimes (demo location) the attribute table is lost. dont use this until
# understand why this is happen
   # execGRASS( 
   #   'v.dissolve',
   #   input='tmp_catch_final',
   #   output=outputHfCatchment,
   #   column=hfIdxNew,
   #   flags='overwrite'
   #   )
#    execGRASS(
#      'v.db.dropcolumn',
#      map     = outputHfCatchment,
#      columns = c('cat_')
#      ) 
  }

  if(!removeCapted)rmRastIfExists(outputPopResidual)

  # remove remaining tmp file (1 dash)
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')

  return(
    list(
      capacityTable=tblOut,
      zonalTable=tblPopByZone
      )
    )
}


#' amCatchPopToVect
#' handle population catchment area
#' @export
amCatchPopToVect<-function(idField,idPos,incPos,tmpPop,dbCon){
  # idField : HF id column name
  # idPos : which id is currently processed
  # incPos : numeric increment position.
  # tmpPop : population catchment
  # dbCon : RSQlite connection  
  # NOTE: output catchment as vector, merged and easily readable by other GIS.
  # None of those methods worked at the time this script was written :
  # v.overlay :  geometry / topology ok, seems the way to go ! But... how to handle hundred of overlays ? 
  #              And grass doesn't like to work with non topological 'stacked' data. 
  # v.patch : produced empty area and topologic errors, even without topology building (b flag)
  # v.to.3d with groupId as height and v.patch after. V.patch transform back to 2d... with area errors.
  # r.to.rast3 :groupId as Z. doesn't produce anything + 3d interface really bugged. 
  # So, export and append to shapefile, reimport back after the loop. eerk.
  tDir<-tempdir()
  outCatch='tmp__vect_catch'
  tmpVectCatchOut=file.path(tDir,paste0(outCatch,'.shp'))
  execGRASS('r.to.vect',
    input=tmpPop,
    output=outCatch,
    type='area',
    flags=c('overwrite','v'),
    column=idField)
     # for the first catchment : overwrite if exists, else append.
  if(incPos==1){
    if(file.exists(tmpVectCatchOut)){ 
      file.remove(tmpVectCatchOut)
    }
    outFlags=c('overwrite')
  }else{
    outFlags=c('a')
  }
  # update attribute table with actual ID.
  dbRec<-dbGetQuery(dbCon,paste('select * from',outCatch))
  #dbRec[,idField]<-as.integer(idPos) id is not necessarily integer !
  dbRec[,idField]<-idPos
  dbRec[,'label']<-NULL
  dbWriteTable(dbCon,outCatch,dbRec,overwrite=T)
  # export to shapefile. Append if incPos > 1
  execGRASS('v.out.ogr',
    input=outCatch,
    output=tmpVectCatchOut,
    format='ESRI_Shapefile',
    flags=outFlags,
    lco="SHPT=POLYGONZ",
    output_layer=outCatch)
  return(tmpVectCatchOut)
}


#' Check for no data
#' @param val Vector to check 
#' @export
amNoDataCheck<-function(val){
  if(!is.vector(val)) return(TRUE)
  any(c(isTRUE(is.null(val)),isTRUE(is.na(val)),isTRUE(nchar(val)==0)))
}




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

#'Create data list for ui
#'@param id AccessMod class config id to look for
#'@param dl Config data list to retrieve match
#'@export
amListData <- function(id=NULL,dl=dataList,shortType=TRUE){
  datAll <- character(0)
  for(i in id){
    d=amClassInfo(id=i)
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
    names(addChoices) <- addChoices
    dat  <- c(addChoices,dat)
  }
  if(length(dat)==0)dat=character(1)
  for(s in idSelect){
  updateSelectInput(session,s,choices=dat,selected=dat[1])
  }
}






#
#
#  # use a mask to sample point.
#  execGRASS('r.mask',raster='tmp_candidates_grid_residual',flags='overwrite')
#
#  # apply subsampling or random sampling
#  if(FALSE){
#    reg<-execGRASS('g.region',flags=c('g','p'),intern=T)
#    tbl<-read.table(text=reg,sep='=')
#    res<-round(tbl[tbl$V1=='ewres',]$V2*subSamplingFactor)
#    execGRASS('g.region',res=paste(res))
#    execGRASS('r.to.vect',input='tmp_candidates_grid_residual',type='point',output='tmp_sample',flags='overwrite') 
#    execGRASS('g.region',raster='dem__dem')
#  }else{
#    execGRASS('r.random',input='tmp_candidates_grid_residual',npoints=paste(nSampleMax),flags='overwrite',vector='tmp_sample')
#  }
#
#  #execGRASS('v.db.addcolumn',map='tmp_sample',columns='populationCovered INT,correlationTimePop DOUBLE PRECISION')
#
#  tbl<-dbGetQuery(dbCon,'SELECT * FROM tmp_sample;')
#  
#
#  # for loop timing : 500 sample, res 1k, for loop, data.frame
#  #    user  system elapsed
#  # 458.008 154.924 680.471
#  #
##
## tt<-data.frame()
##
## amTiming<-function(eval=T,id,exp){
##   if(eval){
##     eval.parent(n=1,{tt<-rbind(tt,data.frame(id=id,as.list(system.time(exp))))})
##   }else{
##   eval.parent(n=1,exp)
##   }
## }
## eval(envir=env(amTiming),expr={tt=data.frame()})
#
# # t2<-system.time(
#    execGRASS('r.mask',raster=inputSpeed,flags=c('quiet','overwrite'))
# #     ))
#
#
#
#library(foreach)
#timeTest<-system.time({
#  res=foreach(i=1:nrow(tbl),.combine=rbind) %do% {
#  s=tbl[i,'cat'] 
#  
#    # extract temporary facility point
#    qSql<-paste("cat =",s)
#  t1<-system.time(execGRASS("v.extract",flags='overwrite',input='tmp_sample',where=qSql,output='tmp_sample_item'))
#    # compute cumulative cost map
#  t3<-  system.time(switch(typeAnalysis,
#      'anisotropic'=amAnisotropicTravelTime(
#        inputSpeed=inputSpeed,
#        inputHf='tmp_sample_item', 
#        outputCumulative="tmp_cost", 
#        returnPath=TRUE,
#        maxCost=maxCost
#        ),
#      'isotropic'=amIsotropicTravelTime(
#        inputFriction=inputFriction,
#        inputHf='tmp_sample_item',
#        outputCumulative='tmp_cost',
#        maxCost=maxCost
#        ),
#      'circular'=amCircularTravelDistance(
#        inputHf='tmp_sample_item',
#        outputBuffer='tmp_cost',
#        radius=radius
#        )
#      )
#      )
#  # set a mask on resulting cumulative cost map to do a zonal stat analysis with population
#    #t4<-system.time(execGRASS('r.mask',raster='tmp_cost',flags=c("quiet","overwrite")))
#    # compute integer version of cumulative cost map to use with r.univar
#    expr=paste("tmp_cost=int(tmp_cost/60)")
#    t5<-system.time(execGRASS('r.mapcalc',expression=expr,flags='overwrite'))
#    # compute zonal statistic : time isoline as zone
#   t6<-system.time( tblPopByZone<-read.table(
#      text=execGRASS(
#        'r.univar',
#        flags=c('g','t','overwrite'),
#        map=inputPop, 
#        zones='tmp_cost',
#        intern=T
#        ),sep='|',header=T))
#    # calculate cumulated sum of pop at each zone
#    t7<-system.time(tblPopByZone$cumSum<-cumsum(tblPopByZone$sum))
#   t8<-system.time( tblPopByZone<-tblPopByZone[c('zone','sum','cumSum')])
#    # After cumulated sum, order was not changed, we can use tail/head to extract min max
#   t9<-system.time( totalPop<-tail(tblPopByZone,n=1)$cumSum)
#    #totalPop<-max(tblPopByZone$cumSum)
#    # check time vs pop correlation : negative value = covered pop decrease with dist; positive value = covered pop increase with dist
#   t10<-system.time( corPopTime <- cor(tblPopByZone$zone,tblPopByZone$sum))
#    # update db
#    #sql=paste("UPDATE tmp_sample SET populationCovered=",round(totalPop),",correlationTimePop=",corPopTime,"WHERE cat=",s)
#    #dbGetQuery(dbCon,sql)
#   #rTest<-data.frame(rbind(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10))
#   rTest<-data.frame(rbind(t1,t3,t5,t6,t7,t8,t9,t10))
#   rTest$id<-row.names(rTest)
#   rTest
#  # print(tTot)
#  #  c(pop=round(totalPop),cor=corPopTime)
#  }
#})
#resDt<-as.data.table(res)
#resDt[,median(elapsed),by=id]
#
#
#
#
#tbl$correlationPopTime=res$cor
#tbl$totalPop=res$pop
#tbl$value=NULL
#dbWriteTable(dbCon,'tmp_sample',tbl,overwrite=TRUE)
#
#
#
#    execGRASS('r.mask',raster='tmp_cost')
#
#
#    # extract maximum value for pop density, existing facilities cumulative travel time, and maximum friction
#
#    sdPop<-amGetRasterStat('tmp_pop_density','stddev')
#    meanPop<-amGetRasterStat('tmp_pop_density','mean')
#
#    sdCumul<-amGetRasterStat('tmp_cumul','stddev')
#    meanCumul<-amGetRasterStat('tmp_cumul','mean')
#
#    sdFriction<-amGetRasterStat(inputFriction, 'max')
#    meanFriction<-amGetRasterStat(inputFriction,'mean')
#
##    maxPop<-amGetRasterStat('tmp_pop_density','max')
##    maxCumul<-amGetRasterStat('tmp_cumul','max')
##    maxFriction<-amGetRasterStat(inputFriction,'max')
#
#    # find cells that minimize friction, maximize pop density and distance to existing facilities
#    weightFriction=1
#    weightPopDensity=1
#    weightTravelTime=1
# #   exp <- paste("tmp_candidates_base=((1-(",inputFriction,"/",maxFriction,"))*",weightFriction,"+(tmp_pop_density/",maxPop,")*",weightPopDensity,"+(tmp_cumul/",maxCumul,")*",weightTravelTime,")")
#    
#    scFriction<-paste("1-((",inputFriction,"-",meanFriction,")/",sdFriction,")*",weightFriction)
#    scPop<-paste("((tmp_pop_density-",meanPop,")/",sdPop,")*",weightPopDensity)
#    scTravelTime<-paste("((tmp_cumul-",meanCumul,")/",sdCumul,")*",weightTravelTime)
#
#    exp <-paste("tmp_candidates_base=",scFriction,"+",scPop,"+",scTravelTime)
#
#
#
#    exp <- paste("tmp_test_friction=",scFriction)
#    exp <- paste("tmp_test_pop=",scPop)
#    exp <- paste("tmp_test_travel=",scTravelTime)
#   # exp <- paste("tmp_candidates_base=((1-(",inputFriction,"/",maxFriction,"))*",weightFriction,"+(tmp_pop_density/",maxPop,")*",weightPopDensity,"+(tmp_cumul/",maxCumul,")*",weightTravelTime,")")
#    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
#    # get 99 percentile
#    candidates99Percentile<-as.numeric(
#      unlist(
#        strsplit(grep("percentile_95",
#            execGRASS('r.univar',map='tmp_candidates_base',flags=c('e','g'),percentile=95,intern=T)
#            ,value=T),
#          '=')
#        )[[2]])
