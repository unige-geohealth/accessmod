

# import map: ui.
output$modManageMap<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  sidebarLayout(
    sidebarPanel(
      formMapClass,
      formMapTag,
      formMapUpload
    ),
    mainPanel(
      manageMap
    )
  )
})


# manage map panel
manageMap<-renderUI({
  # output panel
  tabsetPanel(
    tabPanel('Raster',
             sidebarLayout(
               sidebarPanel(  
                 txt(inputId = 'filtRast','filter maps names','',sty=stytxt),
                 
                 addUIDep(
                   selectizeInput("filtTagRast", 
                                  "filter by tags",
                                  choices="",
                                  multiple=TRUE, 
                                  options = list(plugins = list("drag_drop", "remove_button")),
                                  width='100%')
                 ),
                
                 downloadButton('downloadRaster', 'Download'),
                 checkboxInput('showDelRast','Show removing option'),
                 conditionalPanel(
                   condition = "input.showDelRast == true",
                   btn('delRast','Delete permanently',sty=stybtn)
                   )
               ),
               mainPanel(
                 hotable("mapListRast")
               )
             )),
    tabPanel('Vector',  
             sidebarLayout(
               sidebarPanel( 
                 txt(inputId = 'filtVect','filter map names','',sty=stytxt),
                 btn('downVect','Download',sty=stybtn),
                 checkboxInput('showDelVect','Show removing option'),
                 conditionalPanel(
                   condition="input.showDelVect == true",
                   btn('delVect','Delete  permanently',sty=stybtn)
                   ) 
               ),
               mainPanel(
                 hotable("mapListVect")
               )
             ))
  )
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


observe({
  filtRast<-input$filtRast
  filtRastTag<-input$filtTagRast 
  rastList<-mapList()$rast
  if(!is.null(filtRast) && !filtRast=="" || !is.null(filtRastTag) && !filtRastTag==""){
    tbl<-hot.to.df(isolate(input$mapListRast))
    filtAll<-c(autoSubPunct(filtRast,' '),filtRastTag)    
    grepExpr<-paste0('(?=.*',filtAll,')',collapse='')
    tryCatch({
      tbl<-tbl[grep(grepExpr,tbl$name,perl=T),]
    },error=function(c)message(c))
    output$mapListRast<-renderHotable({tbl})
    rastTag<-getTagsBack(tbl$name,uniqueTags = T,includeBase=T)  
    updateSelectInput(session,'filtTagRast',choices=rastTag,selected=filtRastTag)
  }else{
    rastTag<-getTagsBack(rastList)
    tbl=data.frame(name=rastList,tags=rastTag)
    rastTag<-getTagsBack(tbl$name,uniqueTags = T,includeBase=T)  
    updateSelectInput(session,'filtTagRast',choices=rastTag,selected=filtRastTag)
    output$mapListRast<-renderHotable({tbl}) 
  }
})


observe({
  filtVect<-input$filtVect
  mV<-mapList()$vect
  if(!is.null(filtVect) && !filtVect==""){
    tbl<-hot.to.df(isolate(input$mapListVect))
    filtVect<-autoSubPunct(filtVect,' ')
    tryCatch({
      output$mapListVect<-renderHotable({tbl[grep(filtVect,tbl$name),]})
    },
    error=function(c)msg(c)
    )
  }else{
    output$mapListVect<-renderHotable({
      tags<-getTagsBack(mV)
      if(length(mV)==0){
        tags='-'
        mV='-'
      }
      data.frame(name=mV,tags=tags)
    }) 
  }
})


#-------------------- download handler

grassDownload<-function(mapsRast){
  tmpDir<-tempdir()
  zipFile<-file.path(tmpDir,'accessModRaster.zip')
  for(m in mapsRast){
    fileName<-file.path(tmpDir,paste0(m,'.tiff'))
    execGRASS('r.out.gdal',input=m,output=fileName,format="GTiff")
  }
  zip(zipFile,files = list.files(tmpDir))
  return(zipFile)
}





output$downloadRaster <- downloadHandler(
  filename = function() {
    'accessModRaster.zip'
  },
  content = function(file) {
    mapsRast<-hot.to.df(input$mapListRast)$name
    tmpDir <- tempdir()
    
    listFiles<-c()
    wdOrig<-getwd()
    setwd(tmpDir)
    for(m in mapsRast){
      fileName<-paste0(m,'.tiff')
#      fileName<-file.path(tmpDir,paste0(m,'.tiff'))
      listFiles<-c(listFiles,fileName)
      execGRASS('r.out.gdal',flags =c('c','overwrite'),input=m,output=fileName,format="GTiff")
    }
    
    zip(file,files = listFiles)
    setwd(wdOrig)
    if (file.exists(paste0(file, ".zip")))
      file.rename(paste0(file, ".zip"), file)
    file
  },
  contentType = "application/zip"
)




#------------------



# select map class
formMapClass<-renderUI({
  mapClassChoices<-names(mapClassList)
  list(
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
    txt('mapTag','Add tags (minimum 1)',value='',sty=stytxt)
  }
  #if(mapMetaList$type=='rast'){
  # txt('mapNoValue','What value is attribuited to noData',value='',sty=stytxt)
  #}
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
      #mapType<-mapMetaList$type
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
          txt('mapSql',"Otional sql query:"),
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
          
          msg(paste(mapNameGrass,'Imported in GRASS.'))
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









