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


#----------------------------------------{ General 
# import map: ui.
output$modManageMap<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  sidebarLayout(
    sidebarPanel(
      formMapClass, # new map class
      formMapTag, # new map tag
      busyIndicator("Calculation In progress",wait = 0),
      formMapUpload, # new map upload
      hr(),
      formManageMap,
      width=dimsbw
      ),
    mainPanel(  
      dataTableOutput("mapListTable")
      )
    )
})


# select map class
formMapClass<-renderUI({
  mapClassChoices<-names(mapClassList)
  list(
    h4('Add new map'),
    selectInput('mapClass','Select map class:',choices=mapClassChoices,selected=mapClassChoices[1],width=dimselw),
    checkboxInput('tryReproj',label = "Auto-reprojection in case of conflict.",value = TRUE)
    )
})


# select tags for this map
formMapTag<-renderUI({
  mapClass<-input$mapClass
  if(!is.null(mapClass) && !mapClass==''){
    mapMetaList$class<-mapClass # land cover, population, road...
    mapMetaList$type<-mapClassList[[mapClass]]$type
    txt('mapTag','Add tags (Minimum one. e.g. base, smith, 2014)',value='',sty=stytxt)
  }
})


# set condition to show the upload form.
formMapUpload<-renderUI({
  mapClass<-input$mapClass
  mapTag<-unlist(input$mapTag)
  mapType<-mapMetaList$type
  if(!is.null(mapTag)){
    # tag auto update. Should take care of the order?
    updateTextInput(session,'mapTag',value=autoSubPunct(mapTag,charTag))
    tagSplit<-unlist(strsplit(mapTag,charTag,fixed=T))
    if(length(tagSplit>0)){
      mapMetaList$tags<-tagSplit
      if(mapType=='rast'){
        list(
          upload(
            'mapNew', 
            'Add projected raster map:',
            multiple = TRUE, 
            accept = acceptRaster,
            sty=stybtn))
      }else{
        list(
          txt('mapSql',"Optional sql query:"),
          upload(
            'mapNew', 
            'Add projected vector map',
            multiple = TRUE, 
            accept = acceptVector,
            sty=stybtn)
          )
      }
    }else{
      p('')
    }
  }else{
    p('')
  }
})


# manage map panel
formManageMap<-renderUI({
  list(
    h4('Manage available maps'),
    radioButtons('typeChoice','Type of map',
      c("Vector" = "vect",
        "Raster"="rast",
        "Both"="both")
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
    downloadButton('downloadSelection', 'Download selection'),
    checkboxInput('showDelOption','Show removing option'),
    conditionalPanel(
      condition = "input.showDelOption == true",
      btn('delMapSelect','Delete permanently',sty=stybtn)
      )
    )
})


#----------------------------------------{ reactivity
# if there is a request to upload a map
observe({
  mapNew<-input$mapNew # take reactivity on mapNew only.
  mapType<-isolate(mapMetaList$type)
  mapClass<-isolate(mapMetaList$class)
  mapTags<-isolate(mapMetaList$tags)

  # If this observer is trigged, therw should be no null in static list. 
  # to be sure:
  if(!is.null(mapType) && !is.null(mapClass) && !is.null(mapTags) && !is.null(mapNew)){

    # remove tag
    updateTextInput(session,'mapTag',value='')
    # get the temp dir
    mapDir<-dirname(mapNew$datapath[1])
    # rename file. Instead of fileinput default, set original name :
    # e.g. road.shp instead of "3"
    mapNew$newPath<-file.path(mapDir,mapNew$name)
    file.rename(mapNew$datapath,mapNew$newPath)

    # Set the real name for grass.
    mapNameGrass<-paste(c(mapClass,paste(mapTags,collapse='_')),collapse=charTagGrass)

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
        tmpMapPath<-file.path(tempdir(),paste0(mapNameGrass,'.tiff'))
        gdalwarp(mapInput,
          dstfile=tmpMapPath,
          t_srs=if(input$tryReproj)getLocationProj(),
          dstnodata="-9999",  
          output_Raster=TRUE,
          overwrite=TRUE,
          verbose=TRUE)
        msg('GDAL finished cleaning.')

        r<-as(raster(tmpMapPath),'SpatialGridDataFrame')
        tryCatch({
          writeRAST6(r, vname=mapNameGrass, overwrite=TRUE)
          msg(paste(mapNameGrass,'Imported in GRASS.'))
        },error=function(cond){
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
          t_srs=if(input$tryReproj)getLocationProj(),
          overwrite=TRUE,
          verbose=TRUE)
        msg('GDAL finished cleaning. Importation in GRASS.')
        tryCatch({
          execGRASS("v.in.ogr", 
            flags=c("overwrite","w"), 
            parameters=list(dsn=tmpMapPath, output=mapNameGrass, snap=0.0001)
            )
          msg(paste('Module import:',mapNameGrass,'Imported in GRASS.'))
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
    error = function(c){
      file.remove(lF)
      msg(paste(mapNameGrass,' importation failed:',c))
    }
    #warning = function(c) msg(paste(mapNameGrass,'importation warning',c))
    # message = function(c) msg(paste('Dem importation msg',c))
    )
  }


})



# delete button raster
observe({
  delRast<-input$delRast
  if(!is.null(delRast) && delRast >0){
    mapL<-paste0(hot.to.df(isolate(input$mapListRast))$name,collapse=',')
    msg(paste('Module manage : removing raster maps. Selected=',mapL))
    execGRASS('g.remove',rast=mapL)
    updateTextInput(session,'filtRast',value = '')
    updateSelectizeInput(session,'filtTagRast',selected = '')
    updateCheckboxInput(session,'showDelRast',value=FALSE)
  }  
})

# delete button raster.
observe({
  delVect<-input$delVect
  if(!is.null(delVect) && delVect >0){
    mapL<-paste0(hot.to.df(isolate(input$mapListVect))$name,collapse=',')
    msg(paste('Module manage : removing vector maps. Selected=',mapL))
    execGRASS('g.remove',vect=mapL)
    updateTextInput(session,'filtVect',value = "")
    updateCheckboxInput(session,'showDelVect',value=FALSE)
  }  
})

# Dynamic filter by existing tag for raster
tableMap<-reactive({
  filtMap<-input$filtMap
  filtMapTag<-input$filtMapTag
  filtMapType<-input$typeChoice     
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











