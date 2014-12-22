# module : create base for velocity map
# TODO : in case of multiple maks, patch the old on with the new one
# TODO : find a clean way to handle the optional buffer.



output$mod1<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  list(
    h3('Stack of map to merge'),
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
  tbl<-hot.to.df(input$landCoverCatTable)
  if(!is.null(tbl)){
    msg(paste('Add to stack requested for: ',sel))
    tblOut<-tempfile()
    stackName<-paste0('s_',sel)
    msg(paste('Add to stack requested for: ',sel,'. Stack name is',stackName))
    write.table(tbl,file=tblOut,row.names=F,col.names=F,sep='\t',quote=F)
    execGRASS('r.category', map=sel, rules=tblOut)
    execGRASS('g.copy',rast=paste0(sel,',',stackName))
    
  }
})


# If new map is selected, import new data 
observe({
  sel<-input$landCoverSelect
  if(!is.null(sel)){
    tbl<-landCoverCatTable()
    if(!is.null(tbl)){
      #cachedTbl <<- tbl
      output$landCoverCatTable<- renderHotable({tbl}, readOnly = FALSE)
      #<-renderHtable({tbl})
    }
  }
})

# if stack btn is pressed, save in GRASS.
observe({
  btn<-input$btnAddStackLcv
  if(!is.null(btn) && btn>0){
    landCoverCatSave()
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


# roadCols<-reactive({
#   sel<-input$roadSelect
#   execGRASS('db.columns',table=sel,intern=T)
#   
#   })

roadPreview<-reactive({
  sel<-input$roadSelect
  cla<-input$roadSelectClass
  lab<-input$roadSelectLabel
  if(!is.null(sel) && !is.null(cla) && !is.null(lab)){
    q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',maxRowPreview)
    tbl<-read.table(text=execGRASS('db.select',sql=q,intern=T),sep='|',header=T,stringsAsFactors=F)
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



# observe if button "add to stack" is activated, then
# get the road Preview as base for distinct values. Smart or not ?
# create new name for stack version
# for each combinaison of value, create a stack layer version with class values
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
        cl <- tbl[i,'Class']
        la <- tolower(autoSubPunct(vect = tbl[i,'Label'],sep='_'))
        tmpFile<-tempfile()
        write(paste0(cl,'\t',la),tmpFile)
        outNameTmp<-paste0('tmp__',sel)
        outNameStack<-paste0('s_',sel,'_',la)
        msg(paste('Vector add to stack : extract class',cl,' from ',sel))
        execGRASS('v.extract',input=sel,output=outNameTmp,where=paste0(cla,"=",cl),flags='overwrite')
        msg(paste('Vector add to stack : Vector to raster, class',cl,' from',outNameTmp))
        execGRASS('v.to.rast',use='val',input=outNameTmp,output=outNameStack,value=cl,flags='overwrite')
        msg(paste('Vector add to stack : setting categories. class',cl,' for',outNameStack))
        execGRASS('r.category',map=outNameStack,rules=tmpFile)
        execGRASS('g.remove',vect=outNameTmp)}
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
                        width=dimselw,
                        multiple=T),
            actionButton('btnAddStackBarrier','Add to stack')
          )
          
        }else{
          p('No barrier map found.')
        }
      ), 
      width=dimsbw)
    # mainPanel(h4('Table barrier'))
  )
})


observe({
  btn<-input$btnAddStackBarrier
  sel<-isolate(input$barrierSelect)
  if(!is.null(sel)){
    cl=999
    la='barrier'
    tmpFile<-tempfile()
    write(paste0(cl,'\t',la),tmpFile)
    for(s in sel){
      tryCatch({
        outNameStack<-paste0('s_',s)
        msg(paste('Barrier add to stack : Vector to raster, class',cl,' from',outNameStack))
        execGRASS('v.to.rast',use='val',input=s,output=outNameStack,value=cl,flags='overwrite')
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
        checkboxInput('checkBuffer',label = 'Add a buffer around barriers?',value = TRUE),
        actionButton('btnMerge',"Merge maps"),
        actionButton('btnRmMerge',"Remove all")
      )
    }else{
      p("No stack found. Please add one or more maps to the stack.")
    }
  )
})


rmRastIfExists<-function(pattern=''){
  rastList <- execGRASS('g.mlist',type='rast',pattern=pattern,intern=TRUE)
  if(length(rastList)>0){
    execGRASS('g.mremove',flags=c('b','f'),type='rast',pattern=pattern)
  }
}


observe({
  rmMerge<-input$btnRmMerge
#   mL<-mapList()
#   mapStack<-mL$stack
  if(!is.null(rmMerge)&& rmMerge>0){
    updateSelectizeInput(session = session, inputId = "mapStack",selected='')
  }
  
})


# hideItemStack<-reactive({
#   stackSub<-input$mapStack
#   stackMod<-stackSub[! stackSub == currentMap]
#   updateSelectizeInput(session = session,inputId = "mapStack", selected=stackMod)
#   })
# 
# 


observe({
  btnMerge<-input$btnMerge
  sel<-isolate(input$mapStack)
  buff<-isolate(input$checkBuffer)
  
  if(!is.null(btnMerge) && btnMerge > 0){
    mergedMap<-paste0('merged__','test') # TODO: selectText input + control existing maps.
    maskCount<-0
    tempBase<-'s_tmp__'
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
      if(length(grep('s_barrier__', s))>0){
        # if the map is a barrier map, create a new mask
        # with an optional buffer
        maskCount=maskCount+1
        dist<-if(buff){res}else{0}
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
    
    #browser()

    tempMapList<-execGRASS('g.mlist',type='rast',pattern=paste0(tempMapBase,'*'),intern=TRUE)
    #execGRASS('r.mask',flags='r')
    rmRastIfExists('MASK')
    message('tempMapList=',tempMapList)
    if(length(tempMapList)>1){  
      execGRASS('r.patch',input=paste(tempMapList,collapse=','),output=mergedMap,flags=c('overwrite'))
    }else{
      execGRASS('g.copy',rast=paste0(tempMapList,',',mergedMap),flags='overwrite')
    }
    rmRastIfExists('MASK*')
    rmRastIfExists(paste0(tempBase,'*'))
    msg(paste('Map merge:',mergedMap created))
#     rmRastIfExists()
#     tempMapList<-execGRASS('g.mlist',type='rast',pattern=paste0(tempBase,'*'),intern=TRUE)
#     execGRASS('g.remove',rast=tempMapList)
  }  
})












