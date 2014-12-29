# module : create base for velocity map
# TODO : in case of multiple masks, patch the old on with the new one
# TODO : find a clean way to handle the optional buffer.



output$mod1<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  list(
    h3('Stack of map to merge into a new land cover'),
    fluidRow(stackModule),
    hr(),
    h3('Add to stack'),
    fluidRow(landCoverMod1),
    hr(),
    fluidRow(roadMod1),
    hr(),
    fluidRow(barrierMod1)
  )
})


#--------------------------------------{ LANDCOVER

# main ui layout
landCoverMod1<-renderUI({
  mL<-mapList()
  mapLandCover<-mL$lcv
  list(sidebarPanel(
    h4('Land cover'),
    if(length(mapLandCover)>0){
      list(
        selectInput('landCoverSelect','Select land cover map:',choices=mapLandCover,width=dimselw),
        actionButton('btnAddStackLcv','Add to stack')
      )
      
    }else{
      p('No land cover map found')
    } 
    ,width=dimsbw),
    mainPanel(
      h4('Table of Land cover categories'),
      if(length(mapLandCover)>0){
        list(
          p("Edit the column 'Label' to change raster category or copy and paste from spreadsheet."), 
          hotable("landCoverCatTable")
        )
      }else{
        p('No land cover map found')
      }
    )
  )
})




# Reactivity handling:

# reactive land cover cat table.
landCoverCatTable<-reactive({
  sel<-input$landCoverSelect
  tblCat<-read.csv(
    text=execGRASS('r.category',
                   map=sel,
                   intern=T),
    sep='\t',
    header=F,
    stringsAsFactors=F
  )
  names(tblCat)<-c('Class','Label')
  tblCat[is.na(tblCat)]<-'-'
  return(tblCat)
})

# save change in the lcv map.
landCoverCatSave<-reactive({
  sel<-input$landCoverSelect
  tbl<-hot.to.df(isolate(input$landCoverCatTable))
  if(!is.null(tbl)){
    msg(paste('Add to stack requested for: ',sel))
    tblOut<-tempfile()
    stackName<-paste0('stack__',sel)
    msg(paste('Add to stack requested for: ',sel,'. Stack name is',stackName))
    write.table(tbl,file=tblOut,row.names=F,col.names=F,sep='\t',quote=F)
    execGRASS('r.category', map=sel, rules=tblOut)
    execGRASS('g.copy',rast=paste0(sel,',',stackName),flags='overwrite')
    
  }
})


# If new map is selected, import new data 
observe({
  sel<-input$landCoverSelect
  if(!is.null(sel)){
    tbl<-landCoverCatTable()
    if(!is.null(tbl)){
      output$landCoverCatTable<- renderHotable({tbl}, readOnly = FALSE)
    }
  }
})

# if stack btn is pressed, save in GRASS.
observe({
  btn<-input$btnAddStackLcv
  if(!is.null(btn) && btn>0){
    landCoverCatSave()
    tbl<-landCoverCatTable()
    output$landCoverCatTable<- renderHotable({tbl}, readOnly = FALSE)
  }  
})




#--------------------------------------{ ROADS


# main ui layout
roadMod1<-renderUI({
  mL<-mapList()
  mapRoad<-mL$road
  list(
    sidebarPanel(list(
      h4('Roads'),
      if(length(mapRoad)>0){
        list(
          
          
          selectInput('roadSelect','Select road map:',choices=mapRoad,width=dimselw),
          roadClass,
          roadLabel,
          actionButton('btnAddStackRoad','Add to stack')
        )
      }else{
        p('No road map found')
      }
      
    ),width=dimsbw),
    mainPanel(
      fluidRow(
        h4('Table of road categories.'),
        
        if(length(mapRoad)>0){
          list(
            p(paste('Preview the distinct combination from selected column (max.',maxRowPreview,'rows.)')),
            hotable('roadPreviewTable')
          )
          
        }else{        
          p('No road map found')
        }
      )  
    )
  )
})


roadPreview<-reactive({
  sel<-input$roadSelect
  cla<-input$roadSelectClass
  lab<-input$roadSelectLabel
  if(!is.null(sel) && !is.null(cla) && !is.null(lab)){
    q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',maxRowPreview)
    tbl<-read.table(text=execGRASS('db.select',sql=q,intern=T),
                    sep='|',
                    header=T,
                    stringsAsFactors=F)
    names(tbl)<-c('Class','Label')
    tbl
  }
})

roadClass<-renderUI({
  selTable<-input$roadSelect
  if(!is.null(selTable)){
    cols<-grassDbColType(selTable,'INTEGER')
    cols<-cols[!cols %in% c('cat')]
    selectInput('roadSelectClass','Select road class column (integer) :',choices=cols,width=dimselw)
  }else{
    tags$p()
  }
})

roadLabel<-renderUI({
  selTable<-input$roadSelect
  if(!is.null(selTable)){
    cols<-grassDbColType(selTable,'CHARACTER')
    selectInput('roadSelectLabel','Select road label column (text) :',choices=cols,width=dimselw)
  }else{
    tags$p()
  }
})


output$roadPreviewTable<-renderHotable({
  sel<-input$roadSelect
  cla<-input$roadSelectClass
  lab<-input$roadSelectLabel
  if(!is.null(sel) && !is.null(cla) && !is.null(lab)){
    tbl<-roadPreview()
    if(!is.null(tbl)){
      tbl
    }else{
      data.frame('NA')
    }}
})




# create new name for stack (raster) version
observe({
  btn<-input$btnAddStackRoad
  sel<-isolate(input$roadSelect)
  cla<-isolate(input$roadSelectClass)
  lab<-isolate(input$roadSelectLabel)
  if(!is.null(sel) && !is.null(cla) && !is.null(lab) && btn>0){
    tbl<-roadPreview()
    msg(paste('Spliting',sel,'in',nrow(tbl),'new stack map.'))
    tryCatch({
      for(i in 1:nrow(tbl)){
        class <- tbl[i,'Class']
        label <- tolower(autoSubPunct(vect = tbl[i,'Label'],sep='_'))
        labelRule <- autoSubPunct(vect = tbl[i,'Label'],sep=' ')
        tmpFile<-tempfile()
        write(paste0(class,'\t',labelRule),tmpFile)
        outNameTmp<-paste0('tmp__',sel)
        outNameStack<-paste0('stack__',sel,'_',label)
        msg(paste('Vector add to stack : extract class',class,' from ',sel))
        execGRASS('v.extract',
                  input=sel,
                  output=outNameTmp,
                  where=paste0(cla,"=",class),
                  flags='overwrite'
                  )
        msg(paste('Vector add to stack : Vector to raster, class',class,' from',outNameTmp))
        execGRASS('v.to.rast',
                  use='val',
                  input=outNameTmp,
                  output=outNameStack,
                  value=class,
                  flags=c('d','overwrite')
                  )
        msg(paste('Vector add to stack : setting categories. class',class,' for',outNameStack))
        execGRASS('r.category',
                  map=outNameStack,
                  rules=tmpFile
                  )
        execGRASS('g.remove',
                  vect=outNameTmp)}
    },error=function(c)msg(c))
    msg('Vector add to stack finished.')
  }else{
    
  }
})


#--------------------------------------{ BARRIERS

# main ui layout
barrierMod1<-renderUI({
  mL<-mapList()
  mapBarrier<-mL$barrier
  list(
    sidebarPanel(
      list(
        h4('Barriers'),
        if(length(mapBarrier)>0){ 
          list(
            selectInput('barrierSelect',
                        'Select barrier map:',
                        choices=mapBarrier,
                        selected=mapBarrier,
                        width=dimselw,
                        multiple=F),
            barrierForm
          )
          
        }else{
          p('No barrier map found.')
        }
      ), 
      width=dimsbw)
    # mainPanel(h4('Table barrier'))
  )
})

barrierForm<-renderUI({
  sel<-input$barrierSelect
  if(!is.null(sel) && !sel==''){
    actionButton('btnAddStackBarrier','Add to stack')
  }
})


observe({
  btn<-input$btnAddStackBarrier
  sel<-isolate(input$barrierSelect)
  if(!is.null(sel) && !sel=='' && !is.null(btn) && btn>0){
    cl=999
    la='barrier'
    tmpFile<-tempfile()
    write(paste0(cl,'\t',la),tmpFile)
    for(s in sel){
      tryCatch({
        outNameStack<-paste0('stack__',s)
        msg(paste('Barrier add to stack : Vector to raster, class',cl,' from',outNameStack))
        execGRASS('v.to.rast',use='val',input=s,output=outNameStack,value=cl,flags=c('d','overwrite'))
        execGRASS('r.category',map=outNameStack,rules=tmpFile)
      },error=function(c)msg(c))
    }
    
  }
})


#--------------------------------------{ Stack module


stackModule<-renderUI({  

  input$btnAddStackLcv
  input$btnAddStackRoad
  input$btnAddStackBarrier
  mL<-mapList()
  mapStack<-mL$stack
  sidebarPanel(
    h4('Choose stack order for merging maps.'),
    if(length(mapStack)>0){
      list(
        addUIDep(
          selectizeInput("mapStack", 
                         "",
                         choices=mapStack,
                         selected=mapStack, 
                         TRUE, 
                         options = list(plugins = list("drag_drop", "remove_button")),
                         width='100%')
        ),
        actionButton('btnRmMerge',"Hide all stack items"),
        actionButton('btnAddMerge',"Show all stack items"),
        hr(),
        h4('Merge selected maps.'),
        txt('stackTag','Add tags (minimum 1)',value='',sty=stytxt),
        stackForm
      )
    }else{
      p("No stack found. Please add one or more maps to the stack.")
    }
  )
})







stackForm<-renderUI({
  stackTag<-input$stackTag
  if(!is.null(stackTag) && ! stackTag==''){
    stackTag<-unlist(stackTag)
    updateTextInput(session,'stackTag',value=autoSubPunct(stackTag,charTag))  
    list(
    checkboxInput('checkBuffer',label = 'Add a buffer around barriers?',value = FALSE),
    actionButton('btnMerge',"Merge new land cover")
    )
  }else{
    p('')
  }
})

# function to remove raster based on pattern
rmRastIfExists<-function(pattern=''){
  rastList <- execGRASS('g.mlist',type='rast',pattern=pattern,intern=TRUE)
  if(length(rastList)>0){
    execGRASS('g.mremove',flags=c('b','f'),type='rast',pattern=pattern)
  }
}


# button to hide stack items
observe({
  rmMerge<-input$btnRmMerge
  if(!is.null(rmMerge)&& rmMerge>0){
    updateSelectizeInput(session = session, inputId = "mapStack",selected='')
  }
  
})

# button to show stack items
observe({
  addMerge<-input$btnAddMerge   
  if(!is.null(addMerge)&& addMerge>0){
    mL<-mapList()
    mapStack<-mL$stack
    updateSelectizeInput(session = session, inputId = "mapStack",selected=mapStack)
  }
  
})




observe({
  btnMerge<-input$btnMerge
 
  sel<-isolate(input$mapStack)
  buff<-isolate(input$checkBuffer)
  stackTag<-isolate(input$stackTag)
  
  if(!is.null(btnMerge) && btnMerge > 0){
    msg('Merging landcover map requested.')
    tagSplit<-unlist(strsplit(stackTag,charTag,fixed=T)) #from tag+test+v1#
    mergedName<-paste(c('merged',paste(tagSplit,collapse='_')),collapse=charTagGrass)
    maskCount<-0
    tempBase<-'stack__tmp__'
    tempMapBase=paste0(tempBase,'map')
    tempMapBuffer=paste0(tempBase,'buffer')
    tempMapIn=tempMapBase
    tempMapOut=tempMapBase
    reg<-execGRASS('g.region',flags='p',intern=T)
    res<-reg[grep('nsres',reg)]
    res<-ceiling(as.numeric(gsub("[:]+|[[:space:]]+|[[:alpha:]]",'',res)))
    isFirstMap=TRUE
    rmRastIfExists('MASK')
    rmRastIfExists(paste0(tempBase,'*'))
    
    msg(paste('stack will be merged in this order:',paste(sel,collapse=',')))
    for(s in sel){

      msg(paste('Map merge. Map=',s))
      if(length(grep('stack__barrier__', s))>0){
        # if the map is a barrier map, create a new mask
        # with an optional buffer
        maskCount=maskCount+1
        dist<-if(buff){res}else{0.1}
        execGRASS('r.buffer',input=s,output=tempMapBuffer,distances=dist,flags=c('overwrite'))
        rmRastIfExists('MASK')
        execGRASS('r.mask',raster=tempMapBuffer,flags=c('i')) 
        tempMapOut<-paste0(tempMapBase,'_',maskCount)
      }else{
        if(isFirstMap){
          msg(paste('Map merge:',s,'is first item of stack'))
          execGRASS('r.mapcalc',expression=paste(tempMapOut,'=',s),flags='overwrite')
          tempMapIn=tempMapOut
          isFirstMap=FALSE
        }else{
          msg(paste('Map merge:',s,'will be merged in with',tempMapIn))
          execGRASS('r.patch', input=paste0(tempMapIn,',',s),output=tempMapOut,flags=c('overwrite'))
          tempMapIn=tempMapOut
        }
      }
    }

    tempMapList<-execGRASS('g.mlist',type='rast',pattern=paste0(tempMapBase,'*'),intern=TRUE)
    rmRastIfExists('MASK')
    message('tempMapList=',tempMapList)
    if(length(tempMapList)>1){  
      execGRASS('r.patch',input=paste(tempMapList,collapse=','),output=mergedName,flags=c('overwrite'))
    }else{
      execGRASS('g.copy',rast=paste0(tempMapList,',',mergedName),flags='overwrite')
    }
    rmRastIfExists('MASK*')
    rmRastIfExists(paste0(tempBase,'*'))
    msg(paste('Map merge:',mergedName,'created'))
  }  
})












