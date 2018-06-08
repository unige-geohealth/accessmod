#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/

#' Search and organise data into dataList reactive function
#' @param config Accessmod list
#' @param dataList Reactive values object that will hold available data list
#' @param grassSession Reactive values object containing grass related info
#' @return populate dataList
#' @export
amDataManager<-function(config,dataList,grassSession){
  gisLock = grassSession$gisLock
  dbCon = grassSession$dbCon
  mapset = grassSession$mapset
  if(!is.null(gisLock) && !is.null(dbCon) && !is.null(mapset)){

    rmVectIfExists('^tmp_*')
    rmRastIfExists('^tmp_*')
    archives<-amGetArchiveList(config$pathArchiveGrass,config$archiveBaseName)
    archivesSelect<-archives[order(archives,decreasing=T)]
    
    # extract data of type table only from sqlite
    sqlTables <- dbListTables(dbCon) 
    # get all class of type table
    tblClasses <- config$dataClass[config$dataClass$type=='table','class']
    # filter only tables with valid table classes
    tables <- unlist(sapply(tblClasses,function(x){
        v <-grep(paste0(x,config$sepClass),sqlTables,value=T)
}
        )
      )

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
    rastersSelect<-amCreateSelectList(
      dName=execGRASS('g.list',type='raster',intern=TRUE),
      sepTag=config$sepTagFile,
      sepClass=config$sepClass,
      mapset=mapset
      )
    shapesSelect<-amCreateSelectList(
      dName=names(amGetShapesList()),
      sepTag=config$sepTagFile,
      sepClass=config$sepClass,
      mapset=mapset
      )
    listsSelect <- amCreateSelectList(
      dName=names(amGetListsList()),
      sepTag=config$sepTagFile,
      sepClass=config$sepClass,
      mapset=mapset
      )

    # if amCreateSelectList found NA in name (wrong data name / class not in config)
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
        vectToRemove <- vectorsSelect[is.na(names(vectorsSelect))]
        if(isTRUE(length(vectToRemove))>0){
          sapply(vectToRemove,function(x){
            x <- unlist(strsplit(x,config$sepMapset))[1]
            message(paste("removing unnamed file", x))
            rmVectIfExists(x)}
            )
        }
      }
      if(!is.null(tablesSelect)){
        tableToRemove<-tablesSelect[is.na(names(tablesSelect))]
        if(isTRUE(length(tableToRemove)>0)){
          sapply(tableToRemove,function(x){
            x <- unlist(strsplit(x,config$sepMapset))[1]
            message(paste("removing unnamed file", x))
            sql <- paste("DROP TABLE IF EXISTS",x)
            dbGetQuery(dbCon,sql)}
            )
        }
      }
  
      if(!is.null(shapesSelect)){
        # if the name is empty or begin with NA (no data class found...)
        shapesToRemove<-unique(c(
            shapesSelect[is.na(names(shapesSelect))],
            shapesSelect[grep(paste0("^NA",config$sepClass),shapesSelect)]
            ))
        if(isTRUE(length(shapesToRemove)>0)){

          sToRm <- amNoMapset(shapesToRemove)
          p <- system(paste("echo",config$pathShapes),intern=T)
          allShapes <- list.files(p)
          toRm <- sapply(sToRm,function(x){file.path(p,allShapes[grep(paste0('^',x),allShapes)])},simplify=T)
          toRm <- toRm[file.exists(toRm)]
          if(length(toRm)>0){
            message(paste("removing file", paste(toRm,sep=",")))
            file.remove(toRm)
          }
        }
      }
      if(!is.null(listsSelect)){
        # if the name is empty or begin with NA (no data class found...)
        listsToRemove <- unique(c(
            listsSelect[is.na(names(listsSelect))],
            listsSelect[grep(paste0("^NA",config$sepClass),listsSelect)]
            ))
        if(isTRUE(length(listsToRemove)>0)){
          rToRm <- amNoMapset(listsToRemove)
          p <- system(paste("echo",config$pathLists),intern=T)
          allRepports <- list.files(p)
          toRm <- sapply(rToRm,function(x){file.path(p,allRepports[grep(paste0('^',x),allRepports)])},simplify=T)
          toRm <- toRm[file.exists(toRm)]
          if(length(toRm)>0){
            message(paste("removing file", paste(toRm,sep=",")))
            file.remove(toRm)
          }
        }
      }

    }

    dataList$raster<-rastersSelect
    dataList$vector<-vectorsSelect
    dataList$shape<-shapesSelect
    dataList$report<-listsSelect
    dataList$table<-tablesSelect
    dataList$archive<-archivesSelect

    dataList$df<-rbind(
      amDataListToDf(tablesSelect,config$sepClass,'table'),
      amDataListToDf(vectorsSelect,config$sepClass,'vector'),
      amDataListToDf(rastersSelect,config$sepClass,'raster'),
      amDataListToDf(shapesSelect,config$sepClass,'shape'),
      amDataListToDf(listsSelect,config$sepClass,'list')
      )

    dataList$tags <- amGetUniqueTags(dataList$df$tag)

  }else{
    amDebugMsg('DataList: no gisLock, mapset or dbCon ')
  }
}
