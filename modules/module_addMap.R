

# import map.

output$modAddMap<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  list(
    sidebarPanel(
      formMapClass,
      formMapTag,
      formMapUpload
    ),
    mainPanel(
      grassMapList
    )
  )
})


# reactive function to get info about available maps
getGrassMapList<-reactive({
  mL<-mapList()
  # output panel
  tabsetPanel(
    tabPanel('Map list',
             h5('List of available vector maps'),
             renderTable({data.frame(map=mL$vect)}),
             h5('List of available raster maps'),
             renderTable({data.frame(map=mL$rast)})
    )
  )
  
})



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
          #checkboxInput('mapWhere','Include SQL where querry?',value=FALSE),
          #if(!is.null(input$mapWhere) && input$mapWhere ){ 
          txt('mapSql',"Enter an optional SQL WHERE query. e.g. : ROAD_TYPE='Community Road' "),
          # },
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
    
    mapDir<-dirname(mapNew$datapath[1])
    # Renaming all maps with original names. 
    mapNew$newPath<-file.path(mapDir,mapNew$name)
    file.rename(mapNew$datapath,mapNew$newPath)
    
    # Set the real name for grass.
    mapNameGrass<-paste(c(mapClass,paste(mapTags,collapse='_')),collapse=charTagGrass)
    
    # if multiple map, set the parent directory as data source
    if(nrow(mapNew)==1){
      mapInput<-mapNew$newPath
    }else{
      mapInput<-mapDir 
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
          hintBadProjection<-'Projection of dataset does not appear to match current location.'
          cndMsg <- conditionMessage(cond)
          badProjection<-if(length(grep(hintBadProjection,cndMsg))>0){
            msg('ERROR: The map projection is wrong or absent. Please match it with the base map (DEM)')
          }else{
            msg(cond)
          }
        }
        )
        mapMetaList<-reactiveValues(type=NA,class=NA,tags=NA)
      }else{
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
        #v<-shapefile(tmpMapPath)
        tryCatch({
        execGRASS("v.in.ogr", 
                  flags=c("o", "overwrite","w"), 
                  parameters=list(dsn=tmpMapPath, output=mapNameGrass, snap=0.0001)
                  )
        
          msg(paste(mapNameGrass,'Imported in GRASS.'))
        },
        error=function(cond){
          hintBadProjection<-'Projection of dataset does not appear to match current location.'
          cndMsg <- conditionMessage(cond)
          badProjection<-if(length(grep(hintBadProjection,cndMsg))>0){
            msg('ERROR: The map projection is wrong or absent. Please match it with the base map (DEM)')
          }else{
            msg(cond)
          }
        }
        )
        
        mapMetaList<-reactiveValues(type=NA,class=NA,tags=NA)
      }
    },
    # handle errors. Message disable because of grass is too much verbose.
    error = function(c) msg(paste(mapNameGrass,' importation failed:',c))
    #warning = function(c) msg(paste(mapNameGrass,'importation warning',c))
    # message = function(c) msg(paste('Dem importation msg',c))
    )
  }
  
  grassMapList<-renderUI({
    getGrassMapList() 
  })

})

# get a table with all available map
grassMapList<-renderUI({
  getGrassMapList() 
})








#
#
#
#
#
#uiProvision<-function(title,tagBase){
#  inputTag=paste0(tagBase,'tag')
#  inputFile=paste0(tagBase,'file')
#  ui<-renderUI({
#    h4(title)
#    txt(inputTag,'Tags:',value='',sty=stytxt)
#  })
#  return(ui)
#}
#
#observe({
#  tags<-as.character(input[[tagRoad]])
#  if(!length(tags)==0L && !tags==''){
#    updateTextInput(session,tagRoad,value=autoSubPunct(tags,charTag)) 
#  }
#})
#
#
#


# provision data: 
# one big function that handle UI and function or multiple small functions and separate ui ??
# type : rast or vect
# if type = rast create rast UI, after upload (filename=tagBase__tag) reproject 
# return : shiny ui.
# hell of scoping logic..
#uiImport<-function(type,infoMsg,tagBase,title){
#  if(!type %in% c('rast','vect') stop("Type not 'rast or vect'"))
#  tagGrass<-paste0(tagBase,'__')
#  inputTag<-paste0(tagBase,'_tag')
#  inputFile<-paste0(tagBase,'_file')
#  tagList<-execGRASS('g.mlist',type=type,pattern=paste(tagBase,'*')) ## e.g. road__secondary_big
#  destProj<-getLocationProj(ignore.stderr = FALSE)
##observer: modify tag 
#observe()
#
# if(type='rast'){
#   renderUI({
#   h4(title)
#   p(infoMsg)
#   txt(inputTag,'tags:',value='',sty=stytxt)
#   if(!input[[inputTag]] %in% )
#   upload(tagBase, '', multiple = TRUE, accept = acceptRaster,sty=stybtn)
#   
#   
#   })
# 
# }else{
# 
# } 
#
#
#
#
#
#}





