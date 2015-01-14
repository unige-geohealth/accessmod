#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module manage_map :
# -upload new maps
# -browse exisiting map
# -preview maps (not yet) 
# -delete maps


# import map: ui.
output$modManageData<-renderUI({
  if(!is.null(listen$gisLock)){
    sidebarLayout(
      sidebarPanel(
        busyIndicator("Calculation In progress",wait = 0),
        formMapNew, # set new map and tag
        formMapUpload, # generate name and upload logic
        hr(),
        formMapManage, # manage existing maps
        width=dimsbw
        ),
      mainPanel(  
        dataTableOutput("mapListTable") # table of selected maps
        )
      )
  }else{
    panel('warning','No project selected',p(msgNoLocation))
  }
})


# form new map
formMapNew<-renderUI({
  mapClassChoices<-names(mapClassList)
  tagList(
    h4('Add new map'),
    selectInput('mapClass','Select map class:',choices=mapClassChoices,selected=mapClassChoices[1],width=dimselw),
    textInput('mapTag','Add short tags',value='')
    )
})

# form upload button and map name
formMapUpload<-renderUI({
  tagList(
    p('Output map name:'),div(class="well well-sm",mapMetaList$mapNameGrass),
    if(!is.null(mapMetaList$exists)&&mapMetaList$exists){
      p('Map already exists and will be overwrited')
      }else{
      p('')
    },
    formUploadBtn 
    )
})

# btn upload style
formUploadBtn<-renderUI({
  mapNameGrass<-mapMetaList$mapNameGrass
  mapType<-isolate(mapMetaList$mapType)

  if(!is.null(mapNameGrass)){
    if(mapType=='rast'){
   aF<- acceptRaster
    }else{
    aF<-  acceptVector
    }  
    print(aF)
    amFileInput('btnMapNew',
      label='Add projected map',
      style='success',
      disable=FALSE,
      accept=aF,
      multiple=T
      ) 
  }else{
    amFileInput('btnMapNew',
      label='Add projected map',
      style='danger',
      disable=TRUE
      ) 
  }
})

# validate tag
observe({  
  mapTag<-input$mapTag
  if(!is.null(mapTag)&&!mapTag==""){ 
    updateTextInput(session,'mapTag',value=autoSubPunct(mapTag,charTag))
  }
})


# feed mapMetaList
observe({
  mapClass<-input$mapClass
  mapTag<- input$mapTag
  if(!is.null(mapClass) && !mapClass=="" && !is.null(mapTag) && !mapTag==""){
    mapTag<-getUniqueTagString(mapTag,sepIn=charTag,sepOut='_')
    mapType<-mapClassList[[mapClass]]$type
    mapName<-paste0(c(mapClass,mapTag),collapse=charTagGrass)
    # set metaList
    mapMetaList$mapNameGrass<-mapName   
    mapMetaList$mapType<-mapType
    # test if exist.
    mL<-mapList()[[mapType]]
    mapMetaList$exists<-ifelse(mapName %in% mL,TRUE,FALSE)
  }else{
    mapMetaList$type=NULL
    mapMetaList$mapNameGrass<-NULL
    mapMetaList$exists<-NULL
  }
})

# if valid mapName is given, update style. 
observe({
  mapNameGrass<-mapMetaList$mapNameGrass
  updateStyle(
    id='mapTag',
    type=ifelse(!is.null(mapNameGrass)&&!mapNameGrass=="",'o','e'),
    element='border'
    )
})







# manage map panel
formMapManage<-renderUI({
  list(
    h4('Manage available maps'),
    radioButtons('typeChoice','Type of map',
      c("Vector" = "vect",
        "Raster"="rast",
        "Both"="both"),
      selected="both"
      ),
    txt(inputId = 'filtMap','filter maps names','',sty=stytxt),  
    addUIDep(
      selectizeInput("filtMapTag", 
        "filter by tags",
        choices="",
        multiple=TRUE, 
        options = list(plugins = list("drag_drop", "remove_button")),
        width='100%')
      ),
    checkboxInput('showDelOption','Show removing option for selected maps.'),
    conditionalPanel(
      condition = "input.showDelOption == true",
      list(
        hr(),
        btn('delMapSelect','Delete permanently',sty=stybtn),
        hr()
        )
      ),
    downloadButton('downloadSelection', 'Download selection')
    )
})


#----------------------------------------{ reactivity
# if there is a request to upload a map
observe({
  mapNew<-input$btnMapNew # take reactivity on btnMapNew only.
  mapType<-isolate(mapMetaList$mapType)
  mapNameGrass<-isolate(mapMetaList$mapNameGrass)
  tryReproj<-TRUE # auto reprojection  ?
  # If this observer is trigged, therw should be no null in static list. 
  # to be sure:
  if(!is.null(mapNew) && !is.null(mapNameGrass)){

    # remove tag
    updateTextInput(session,'mapTag',value='')
    # get the temp dir
    mapDir<-dirname(mapNew$datapath[1])
    # rename file. Instead of fileinput default, set original name :
    # e.g. road.shp instead of "3"
    mapNew$newPath<-file.path(mapDir,mapNew$name)
    file.rename(mapNew$datapath,mapNew$newPath)

    # Set the real name for grass.
    #mapNameGrass<-paste(c(mapClass,paste(mapTags,collapse='_')),collapse=charTagGrass)

    # if multiple map, set the parent directory as data source
    if(nrow(mapNew)==1){
      mapInput<-mapNew$newPath
      lF<-mapInput
    }else{
      mapInput<-mapDir
      lF<-list.files(mapInput,full.names=T)
    }
    msg(paste('New map (',mapNameGrass,') added to temp dir. Waiting for GDAL to clean data.'))
    tryCatch({
      if(mapType=='rast'){

        #r<-as(raster(tmpMapPath),'SpatialGridDataFrame')
        tryCatch({

          tmpMapPath<-file.path(tempdir(),paste0(mapNameGrass,'.tiff'))
          gdalwarp(mapInput,
            dstfile=tmpMapPath,
            t_srs=if(tryReproj){getLocationProj()},
            dstnodata="-9999",  
            output_Raster=FALSE,
            overwrite=TRUE,
            verbose=TRUE)
          msg('GDAL finished cleaning.')

          if(file.exists(tmpMapPath)){
            execGRASS('r.in.gdal',
              input=tmpMapPath,
              output=mapNameGrass,
              flags=c('overwrite','quiet'),
              title=mapNameGrass)
            msg(paste(mapNameGrass,'Imported in GRASS.'))
          }else{
            stop('Manage maps: process aborded, due to unresolved CRS or not recognized input files. Check files meta data and extent. Importation cancelled.')
          } 
           r<-raster(tmpMapPath)# not loaded in memory
          
                    givenProj<-proj4string(r)
                    if(!givenProj==getLocationProj()){
                      msg(paste(
                          "Manage map warning. CRS of",
                          mapNameGrass,
                          " did not match exactly the current CRS. Raster's proj4string:",
                          givenProj,
                          " Accessmod current proj4string : \n",
                          getLocationProj()))
                    }

          listen$uploadMap<-sample(100,1)
        },error=function(cond){
          unlink(lF)
          hintBadProjection<-'Projection of dataset does not appear to match current location.'
          cndMsg <- conditionMessage(cond)
          if(length(grep(hintBadProjection,cndMsg))>0){
            msg('ERROR: The map projection is wrong or absent. Please match it with the base map (DEM)',
              verbose=FALSE
              )
          }else{
            msg(cond)
          }
        }
          )
        unlink(lF)
        mapMetaList<-reactiveValues(type=NA,class=NA,tags=NA)
      }else{

        # validate rules of input file.
        fE<-file_ext(mapNew$name)

        # helper function to validate file based on extension
        validateFileExt(fE,'vect')

        tmpMapPath<-file.path(tempdir(),paste0(mapNameGrass,'.shp'))

        ogr2ogr( 
          src_datasource_name=mapInput,
          dst_datasource_name=tmpMapPath,
          where=input$mapSql,
          f="ESRI Shapefile",
          t_srs=if(tryReproj){getLocationProj()},
          overwrite=TRUE,
          verbose=TRUE)
        msg('GDAL finished cleaning. Importation in GRASS.')
        tryCatch({
          execGRASS("v.in.ogr", 
            flags=c("overwrite","w","r","2"), # overwrite, lowercase, current region, 2d only,
            parameters=list(dsn=tmpMapPath, output=mapNameGrass, snap=0.0001)
            )
          unlink(lF)
          msg(paste('Module import:',mapNameGrass,'Imported in GRASS.'))
          listen$uploadMap<-sample(100,1)
        },
        error=function(cond){
          file.remove(lF)
          hintBadProjection<-'Projection of dataset does not appear to match current location.'
          cndMsg <- conditionMessage(cond)
          badProjection<-if(length(grep(hintBadProjection,cndMsg))>0){
            msg('ERROR: The map projection is wrong or absent. Please match it with the base map (DEM)')
          }else{
            msg(cond)
          }
        }
        )
        file.remove(lF)
        mapMetaList<-reactiveValues(type=NA,class=NA,tags=NA)
      }
    },
    # handle errors. Message disable because of grass is too much verbose.
    error = function(cond){
      unlink(lF,recursive=TRUE)
      # TODO: write messages in db / list 
      hintBadRasterOrig<-'file does not exist'
      hintBadRasterConvert<-'Error : file not recognized, make sure you have uploaded a supported raster files, with all its dependencies.'
      browser()
      cndMsg <- conditionMessage(cond)
      if(length(grep(hintBadRasterOrig,cndMsg))>0){
        cond<-hintBadRasterConvert
      } 
      msg(paste('Importation failed:',cond))
    }
    #warning = function(c) msg(paste(mapNameGrass,'importation warning',c))
    # message = function(c) msg(paste('Dem importation msg',c))
    )
  }


})



# delete button raster
observe({
  delMapSelect<-input$delMapSelect
  if(!is.null(delMapSelect) && delMapSelect >0){
    tbl<-isolate(tableMap())
    rastName<-as.character(tbl[tbl$type=='rast','name'])
    rastName<-rastName[!rastName %in% 'dem']
    vectName<-as.character(tbl[tbl$type=='vect','name'])
    if(!is.null(rastName) && length(rastName)>0){
      msg(paste('Module manage : removing raster maps. Selected=',paste(rastName, collapse='; ')))
      execGRASS('g.remove',rast=rastName)
    }
    if(!is.null(vectName) && length(vectName)>0){
      msg(paste('Module manage : removing vectors maps. Selected=',paste(vectName, collapse='; ')))
      execGRASS('g.remove',vect=vectName)
    }
    updateTextInput(session,'filtMap',value = '')
    updateSelectizeInput(session,'filtMapTag',selected = '')
    listen$deleteMap<-sample(100,1)
  }  
})

# Dynamic filter by existing tag for raster
tableMap<-reactive({
  filtMap<-input$filtMap
  filtMapTag<-input$filtMapTag
  filtMapType<-input$typeChoice
  t<-input$delMapSelect
  tryCatch({
    # get the value from reactive function mapList
    #mList <- mapList()[c('rast','vect')]

    # test for null and populate table
    okVect<- !is.null(mapList()$vect) && length(mapList()$vect)>0 
    okRast<- !is.null(mapList()$rast) && length(mapList()$rast)>0
    if(okVect && okRast){
      tbl<-rbind(
        data.frame(name=mapList()$rast,type='rast'),
        data.frame(name=mapList()$vect,type='vect')   
        )
    }else if(okVect){
      tbl<- data.frame(name=mapList()$vect,type='vect') 
    }else if(okRast){ 
      tbl<-data.frame(name=mapList()$rast,type='rast')
    }else{
      tbl<-data.frame(name="",type="")
    }


    # check if at least one filter is active
    if(!is.null(filtMapType) || 
      !is.null(filtMap) && !filtMap=="" ||
      !is.null(filtMapTag) && !filtMapTag==""){
      # filter map type
      mType<-switch(filtMapType,
        vect=c('vect'),
        rast=c('rast'),
        both=c('vect','rast') 
        ) 
      tbl<-tbl[tbl$type %in% mType,]

      # filter text entry and tag input
      filtAll<-c(autoSubPunct(filtMap,' '),filtMapTag)
      grepExpr<-paste0('(?=.*',filtAll,')',collapse='')
      tbl<-tbl[grep(grepExpr,tbl$name,perl=T),]

      if(nrow(tbl)<1){
        tbl<-data.frame(name='',type='')
      }
      tblTag<-getTagsBack(tbl$name,uniqueTags = T,includeBase=T)
      updateSelectInput(session,'filtMapTag',choices=tblTag,selected=filtMapTag)
      return(tbl)
    }else{
      tblTag<-getTagsBack(tbl$name,uniqueTags = T,includeBase=T)
      updateSelectInput(session,'filtMapTag',choices=tblTag,selected=filtMapTag)
      return(tbl)
    }
  },error=function(c)message(c)
  )
})


# render dataTable
output$mapListTable<-renderDataTable({
  tableMap()
},options=list(
  searching = FALSE,
  pageLength = 100,
  searchable=FALSE, 
  paging=FALSE
  ))


output$downloadSelection <- downloadHandler(
  filename = function() {
    paste0('accessModMaps_',Sys.Date(),'.zip')
  },
  content = function(file) {
    tryCatch({
      tMap<-isolate(tableMap())
      tMap[]<-lapply(tMap, as.character)
      tmpDir <- tempdir()
      listFiles<-c()
      wdOrig<-getwd()
      for(i in 1:nrow(tMap)){
        if(tMap[i,'type']=='vect'){
          m=tMap[i,'name']
          fN<-exportGrass(m,tmpDir,type='vect')
          listFiles<-c(listFiles,fN)
        }else{
          m=tMap[i,'name']
          fN<-exportGrass(m,tmpDir,type='rast')   
          listFiles<-c(listFiles,fN)
        }
      }
      if(is.null(listFiles))stop('Download handler : no maps found')
      setwd(tmpDir)
      zip(file,files = listFiles)
      setwd(wdOrig)
      if (file.exists(paste0(file, ".zip")))
        file.rename(paste0(file, ".zip"), file)
      file
    },
    error=function(c)msg(paste('Error:',c))
    )
  },
  contentType = "application/zip"
  )











