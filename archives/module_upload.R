

# import map.
# naming convention in GRASS:
# Lcv : lcv_tag
# population : pop_tag
# Roads : road_tag
# Barriers : barriers_tag
# HealthFacilities : hf_tag


mapClassList<-list(
  land_cover=list(type='rast'),
  population=list(type='rast'),
  barrier=list(type='vect'),
  road=list(type='vect'),
  health_facilities=list(type='vect')
  ) 


charTag='+'

output$modProvision<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  sidebarPanel(
    formMapClass,
    formMapTag,
    formMapUpload
   # h4('Roads'),
   # roadForm,
   # h4('Barriers'),
   # barrierForm,
   # h4('Population'),
   # populationForm,
   # h4('Health facilities'),
   # hospitalForm,
   # width=dimsbw
    )
})




formMapClass<-renderUI({
  mapClassChoices<-names(mapClassList)
selectInput('mapClass','Select map class:',choices=mapClassChoices,selected=mapClassChoices[1],width=dimselw)
})

formMapTag<-renderUI({
  mapClass<-input$mapClass
  str(mapClass)
if(!is.null(mapClass) && !mapClass==''){
 txt('mapTag','Add tags (minimum 1)',value='',sty=stytxt)
}
})


# set condition to show the upload form.
formMapUpload<-renderUI({
  mapTag<-unlist(input$mapTag)
  if(!is.null(mapTag)){
    updateTextInput(session,'mapTag',value=autoSubPunct(mapTag,charTag))
    tagSplit<-unlist(strsplit(mapTag,charTag,fixed=T))
    if(length(tagSplit>0)){
      mapType<-mapClassList[[input$mapClass]]$type
      print(mapType)
      if(mapType=='rast'){
        upload(
          input$mapClass, 
          'Upload projected raster map',
          multiple = TRUE, 
          accept = acceptRaster,
          sty=stybtn)
      }else{
        upload(
          input$mapClass, 
          'Upload projected vector map',
          multiple = TRUE, 
          accept = acceptVector,
          sty=stybtn)
      }
    }else{
      p('')
    }
  }else{
  p('')
  }
})

observe({

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





