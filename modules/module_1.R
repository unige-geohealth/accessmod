#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing landcover in a given order.
#
# input : road, barrier, landcover
# output : merged landcover

#TODO: clean script and convert to function.

#----------------------------------------{ UI : validation
output$module1<-renderUI({
 # conditional module display
  validInput<-c(
    'g'=length(listen$gisLock)>0
    )
  msgList<-tagList()
  if(!all(validInput)){
 msgList$g<-ifelse(!validInput['g'],msgNoLocation,'')
  }else{
  tagList(
    #busyIndicator("Stack calculation",wait=2000),
    h3('Stack of map to merge into a new land cover'),
    stackModule,
    hr(),
    h3('Add to stack'),
    landCoverStack,
    hr(),
    roadStack,
    hr(),
    barrierStack
    )
  }
})

#----------------------------------------{ Landcover
#------------------------------{ UI
landCoverStack<-renderUI({
  # get dataset list
  lcvTable<-dataList$table
  lcvTable<-lcvTable[grep('table_land_cover',lcvTable)]
  lcvMap<-dataList$lcv
  # send ui chages
  fluidRow(
    sidebarPanel(width=3,
      h4('Land cover'),
      amProgressBar('lcvStackProgress'),
      selectInput('landCoverSelect','Select land cover map:',choices=lcvMap),
      selectInput('landCoverSelectTable','Select optional land cover table:',choices=lcvTable),
      p('Save raster value and add land cover raster on stack:'),
      actionButton('btnAddStackLcv','Add to stack')
      ),
    mainPanel(width=9,
      h4('Table of land cover categories'),
      fluidRow(
        amPanel(width=6,
          h5('Categories from raster'),
          p("Edit the column 'Label' to change raster table or copy and paste from spreadsheet."), 
          actionButton('mergeLcvUndo',icon=icon('undo'),'reset'),
          hotable("landCoverRasterTable")
          ),
        amPanel(width=6,
          h5('Categories from table'),
          p('Value from imported land cover table. Click on arrow to merge by class.'),
          actionButton('mergeLcv',icon=icon('long-arrow-left'),'merge'),
          hotable("landCoverSqliteTable")
          )
        )
      )
    )
})
#------------------------------{ Reactivity

# toggle buttons to merge lcv table and add to stack

observe({
  lS<-input$landCoverSelect
  lT<-input$landCoverSelectTable
  lab<-hot.to.df(input$landCoverRasterTable)$label
  disableMerge=any(is.null(lS),lS=='',is.null(lT),lT=="")
  disableStack=any(is.null(lS),lS=='',is.null(lab),"" %in% lab,NA %in% lab)
  amActionButtonToggle(id='btnAddStackLcv',session,disable=disableStack)
  #amActionButtonToggle(id='mergeLcvUndo',session,disable=!allow)
  amActionButtonToggle(id='mergeLcv',session,disable=disableMerge)
},label='observeBtnsLcv')



observe({
  tblUpdated<-hot.to.df(input$landCoverRasterTable)
  tblOriginal<-isolate(landCoverRasterTable())
  testNrow<-nrow(tblUpdated)==nrow(tblOriginal)
  # rule 1 : if nrow doesnt match, return original
  if(!is.null(tblUpdated) && !is.null(tblOriginal) && testNrow){
    # rule 2: do not allow changing class
    tblValidated<-data.frame(c(tblOriginal[,c('class')],tblUpdated[,c('label')]))
  }else{
    tbleValidated<-landCoverRasterTable()
  }
  output$mergedMapCatTable<- renderHotable({tblValidated}, readOnly = FALSE, fixed=1, stretched='last')
})


# populate lcv selectize
#observe({
#  amDebugMsg(' update lcv list, module 1, gislock=',isolate(listen$gisLock))
#  dLcv<-dataList$lcv
#  dLcvT<-dataList$table
#  dLcvT<-dLcvT[grep('table_land_cover',dLcvT)] 
#  if(length(dLcv)==0)dLcv=""
#  if(length(dLcvT)==0)dLcvT=""
#  updateSelectizeInput(session,'landCoverSelect',choices=dLcv, selected=dLcv[1])
#  updateSelectizeInput(session,'landCoverSelectTable',choices=dLcvT, selected=dLcvT[1])
#})
#


# Get reactive land cover cat table from raster.
landCoverRasterTable<-reactive({
  sel<-input$landCoverSelect
  #dataList$stack
  #if(!is.null(btn) && btn>0 || !is.null(sel) && !sel==''){
  if(!is.null(sel) && !sel==''){
    tblCat<-read.csv(
      text=execGRASS('r.category',
        map=sel,
        intern=T),
      sep='\t',
      header=F,
      stringsAsFactors=F
      )
    names(tblCat)<-acceptColNames[['table_land_cover']]
    tblCat[is.na(tblCat)]<-'' #na is understood as logical and displayed as checkbox
    return(tblCat)
  }else{
    tblCat<-data.frame(as.integer(NA),as.character(NA))
    names(tblCat)<-acceptColNames[['table_land_cover']]
    return(tblCat) 
  }
})

landCoverSqliteTable<-reactive({
  sel<-input$landCoverSelectTable
  if(!is.null(sel) && !sel==''){
    return(dbGetQuery(isolate(listen$dbCon),paste('select * from',sel)))
  }else{
    tblCat<-data.frame(as.integer(NA),as.character(NA))
    names(tblCat)<-acceptColNames[['table_land_cover']]
    return(tblCat)
  }
})
# Save change in the lcv map.
landCoverRasterSave<-function(selLcv,tblLcv){
  if(!is.null(selLcv) && !is.null(tblLcv)){
    tblOut<-tempfile()
    stackName<-paste0('stack_',selLcv)
    amMsg(session,type="log",text=paste('Add to stack requested for: ',selLcv,'. Stack name is',stackName))
    write.table(tblLcv,file=tblOut,row.names=F,col.names=F,sep='\t',quote=F)
    execGRASS('r.category', map=selLcv, rules=tblOut)
    execGRASS('g.copy',raster=paste0(selLcv,',',stackName),flags='overwrite')
    colorSetting<-unlist(strsplit(dataClass[dataClass$class=='stack_land_cover','colors'],'&'))

    execGRASS('r.colors',map=stackName,color=colorSetting[1])
  }
}
# if select lcv map change or undo btn is pressed, update hotable with value from raster.
observe({
  input$mergeLcvUndo # re evaluate if undo is pressed
  tblSqlite<-landCoverSqliteTable()
  tblRaster<-landCoverRasterTable()
  output$landCoverRasterTable<-renderHotable(tblRaster,readOnly=F,fixedCols=1,stretched='last')
  output$landCoverSqliteTable<-renderHotable(tblSqlite,readOnly=T,fixedCols=1,stretched='last')
})

# if merge button is pressed, merge external and raster table
observe({
  btn<-input$mergeLcv
  if(!is.null(btn) && btn > 0){
    tblOrig<-hot.to.df(isolate(input$landCoverRasterTable))
    tblOrig$label<-NULL
    tblExt<-hot.to.df(isolate(input$landCoverSqliteTable))
    tblMerge<-merge(tblOrig,tblExt,by='class')
    output$landCoverRasterTable<- renderHotable({tblMerge}, readOnly = FALSE, fixedCols=1, stretched='last')
  }
})


# if stack btn is pressed, save in GRASS.
observe({ 
  btn<-input$btnAddStackLcv
  sel<-isolate(input$landCoverSelect)
  tbl<-hot.to.df(isolate(input$landCoverRasterTable))
  if(!is.null(btn) && btn>0){
    amUpdateProgressBar(session,"lcvStackProgress",1)
    landCoverRasterSave(sel,tbl) 
    amUpdateDataList(listen)
    amUpdateProgressBar(session,"lcvStackProgress",100)
  }  
})


#----------------------------------------{ Roads


#------------------------------{ UI
roadStack<-renderUI({
  roadMap<-dataList$road
  roadMapCols<-listen$roadMapCols
  amDebugMsg('road stack ui update')
  fluidRow(
    sidebarPanel(width=3,
      h4('Roads'),
      amProgressBar('roadStackProgress'),
      selectInput('roadSelect','Select road map:',choices=roadMap),
      selectInput('roadSelectClass','Select road class column (integer) :',choices=roadMapCols$cla),
      selectInput('roadSelectLabel','Select road label column (text) :',choices=roadMapCols$lab),
      actionButton('btnAddStackRoad','Add to stack')
      ),
    mainPanel(width=9,
      amPanel(width=6,
        h4('Table of road categories.'),
        p(paste('Preview the distinct combination from selected column (max.',maxRowPreview,'rows.). No missing value allowed')),
        hotable('roadPreviewTable')
        ),
      mainPanel(width=6)
      )
    )
})




observe({
  rP<-roadPreview()
  output$roadPreviewTable<-renderHotable({rP},readOnly=T,stretched='all',fixedCols=2)
  amActionButtonToggle(session=session,id='btnAddStackRoad',disable=any(NA %in% rP$label, "" %in% rP$label))
})


# to be sure that colons will show in roadStack renderui, put them in the listener.
# updateSelectizeInput is weak: If the input entry dosn't seems to be changed,it do nothing.
# if values of selectize are initialised with NA or "", when a dataList update occurs, 
# the the renderui change, but not updateSelectize input (without putting a dependence link on dataList)

observe({
  listen$gisLock
  selTable<-input$roadSelect
    amDebugMsg('in grass road col type, location is ',isolate(input$selectProject))
  if(!is.null(selTable) && !selTable=="" && selTable %in% dataList$road){
    cla<-grassDbColType(selTable,'INTEGER')
    cla<-cla[!cla %in% c('cat')]
    lab<-grassDbColType(selTable,'CHARACTER')
    listen$roadMapCols<-reactiveValues(
      cla=cla,
      lab=lab
      )
  }else{
    listen$roadMapCols<-NULL
  }
})

#------------------------------{ reactivity

roadPreview<-reactive({
  sel<-input$roadSelect
  cla<-input$roadSelectClass
  lab<-input$roadSelectLabel
  if(!is.null(sel) && !sel=="" && !is.null(cla) && !cla=="" && !is.null(lab) && !lab==""){
    amErrorAction(title='Module 1: road preview',{
      q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',maxRowPreview)
      tbl<-dbGetQuery(isolate(listen$dbCon),q)
      names(tbl)<-acceptColNames[['table_stack_road']]
      tbl
      })
  }else{
    tbl<-data.frame(as.integer(NA),as.character(NA))
    names(tbl)<-acceptColNames[['table_stack_road']]
    tbl
  }
})


# create new name for road stack (raster) version
observe({
  btn<-input$btnAddStackRoad
  sel<-isolate(input$roadSelect)
  cla<-isolate(input$roadSelectClass)
  lab<-isolate(input$roadSelectLabel)
  if(!is.null(sel) && !is.null(cla) && !is.null(lab) && btn>0){ 
    amErrorAction(title='Module 1: add stack road',{
      tbl<-isolate(roadPreview())
      message('Module 1: Spliting',sel)
      tblN <- nrow(tbl)
      amUpdateProgressBar(session,'roadStackProgress',1)
      #increment
      inc <- 1/tblN*100
      for(i in 1:tblN){
        class <- tbl[i,'class']
        label <- tolower(autoSubPunct(vect = tbl[i,'label'],sep='_'))
        #labelRule <- autoSubPunct(vect = tbl[i,'label'],sep=' ')
        labelRule <- autoSubPunct(label,sep=' ')
        tmpFile<-tempfile()
        tmpRules<-paste0(class,'\t',labelRule)
        write(tmpRules,file=tmpFile)
        outNameTmp<-paste0('tmp__',sel)
        outNameStack<-paste0('stack_',sel,'_',label)
        message(paste('Vector add to stack : extract class',class,' from ',sel))
        execGRASS('v.extract',
          input=sel,
          output=outNameTmp,
          where=paste0(cla,"=",class),
          flags='overwrite'
          )
        message(paste('Vector add to stack : Vector to raster, class',class,' from',outNameTmp))
        execGRASS('v.to.rast',
          use='val',
          type='line',
          input=outNameTmp,
          output=outNameStack,
          value=class,
          flags=c('d','overwrite')
          )
        colorSetting<-unlist(strsplit(dataClass[dataClass$class=='stack_road','colors'],'&'))
        execGRASS('r.colors',map=outNameStack,color=colorSetting[1])
        message(paste('Vector add to stack : setting categories. class',class,' for',outNameStack))
        execGRASS('r.category',
          map=outNameStack,
          rules=tmpFile
          )
        rmVectIfExists(outNameTmp)

        amUpdateProgressBar(session,'roadStackProgress',i*inc)
      }
      amUpdateDataList(listen)

      })
  }
})

#----------------------------------------{ Barriers

#------------------------------{ UI 
# main ui layout
barrierStack<-renderUI({
  barrierMap<-dataList$barrier
  fluidRow(
    sidebarPanel(width=3,
      h4('Barriers'),
      amProgressBar('barrierProgress'),
      selectInput('barrierSelect','Select barrier map:',choices=barrierMap,multiple=F),
      radioButtons("barrierType", "Barrier type:",
        c("Areas" = "area",
          "Lines" = "line",
          "Point" = "point"),selected='', inline=TRUE),
      actionButton('btnAddStackBarrier','Add to stack')
      ),
    mainPanel(width=9,
      amPanel(width=6,
        h4('Barrier info'),
        hotable("barrierPreviewTable")
        ),
      mainPanel(width=6)
      )
    )
})


observe({
  bT<-input$barrierType
  bS<-input$barrierSelect
  disableBtn<-any(is.null(bT), bT=="", is.null(bS) , bS=="")
  amActionButtonToggle(id="btnAddStackBarrier",session,disable=disableBtn)
})



barrierPreview<-reactive({
  sel<-input$barrierSelect
  amErrorAction(title='Module 1: barrier preview',{
    if(!is.null(sel) && !sel==""){
      tbl<-read.table(text = execGRASS('v.info',map=sel,flags='t',intern=T),sep="=")
      names(tbl)<-c('features','count')
      tbl<-tbl[tbl$features %in% c('areas','lines','points'),]
      return(tbl)
    }else{
      tbl<-data.frame(as.character(NA),as.integer(NA))
      names(tbl)<-c('features','count')
      return(tbl)
    }
    })
})

observe({
  tbl<-barrierPreview()
  if(!any(is.na(tbl))){
    output$barrierPreviewTable<-renderHotable({tbl},readOnly=T,fixedCols=2,stretched='all') 
    updateRadioButtons(session,'barrierType',selected=gsub('s$','',tbl[which.max(tbl$count),'features']))

  }
})


#------------------------------{ reactivity

observe({
  btn<-input$btnAddStackBarrier
  sel<-isolate(input$barrierSelect)
  type<-isolate(input$barrierType)
  if(!is.null(sel) && !sel=='' && !is.null(btn) && btn>0){
    cl=1
    la='barrier'
    tmpFile<-tempfile()
    write(paste0(cl,'\t',la),tmpFile)
    inc=1/length(sel)*100
    amErrorAction(title='Module 1: add stack barrier',{
      amUpdateProgressBar(session,'barrierProgress',1)
      for(i in 1:length(sel)){
        s<-sel[i]
        outNameStack<-paste0('stack_',s)
        message('Barrier add to stack : Vector to raster, class',cl,' from',outNameStack)
        execGRASS('v.to.rast',use='val',
          input=s,
          output=outNameStack,
          type=type,
          value=cl,
          flags=c('overwrite',if(type=='line')'d')# bug densified lines with area: not working.
          ) 
        execGRASS('r.category',map=outNameStack,rules=tmpFile)
        rmVectIfExists('tmp__')
        amUpdateProgressBar(session,'barrierProgress',1*inc)
      }
      amUpdateDataList(listen)
    })
  }
})

#----------------------------------------{ Stack module


#------------------------------{ UI
stackModule<-renderUI({
  mergedMap<-dataList$stack
  fluidRow(
    sidebarPanel(width=3,
      h4('Merge stack'),
      amProgressBar('stackProgress'),
      p(tags$b('Display or hide stack items')),
      actionButton('btnRmMerge',"Hide all stack items"),
      actionButton('btnAddMerge',"Show all stack items"),
      textInput('stackTag','Add tags (minimum 1)',value=''),
      actionButton('btnMerge',"Merge stack")
      ),
    amPanel(width=9,
      h4('Stack order'),
      p('Select the order to merge maps'),
      addUIDep(
        selectizeInput("mapStack","",choices=mergedMap,selected=mergedMap, 
          multiple=TRUE, options = list(plugins = list("drag_drop", "remove_button")
            )
          )
        )
      )
    )
})



#------------------------------{ reactivity

# btn merge activation
observe({
  mS<-input$mapStack
  sT<-input$stackTag
  disableBtn=any(is.null(mS), mS=='', is.null(sT), nchar(sT)<1)
  amActionButtonToggle(id='btnMerge',session,disable=disableBtn)
})

# tag validation
observe({
  stackTag<-input$stackTag
  if(!is.null(stackTag) && ! stackTag==''){
    stackTag<-unlist(stackTag)
    updateTextInput(session,'stackTag',value=autoSubPunct(stackTag,sepTagUi))
  }
})
# button to hide stack items
observe({
  rmMerge<-input$btnRmMerge
  if(!is.null(rmMerge)&& rmMerge>0){
    updateSelectizeInput(session = session, inputId = "mapStack",selected='')
  }
})
# button to show stack items
#observe({
#  addMerge<-input$btnAddMerge   
#  if(!is.null(addMerge)&& addMerge>0){
#    mL<-dataList()
#    mapStack<-mL$stack
#  }
#})

#observe if new stack maps are available
#observe({
#  aM<-input$btnAddMerge
#  sL<-dataList$stack
#  if(length(sL)==0)sL=""
#  updateSelectizeInput(session = session, inputId = "mapStack",choices=sL)
#})
#

#  merge action
observe({
  btnMerge<-input$btnMerge
  if(!is.null(btnMerge) && btnMerge > 0){
    amErrorAction(title='Module 1: merge process',{
      sel<-isolate(input$mapStack)
      selL<-length(sel)
      inc<-1/(selL+1)*100
      #buff<-isolate(input$checkBuffer)
      stackTag<-isolate(input$stackTag)
      message('Merging landcover map requested.')
      stackTag<-autoSubPunct(stackTag,sepTagFile)
      tagSplit<-unlist(strsplit(stackTag,sepTagFile,fixed=T)) #from tag+test+v1#
      merged<-paste(c('merged',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
      bridges<-paste(c('merged_bridge',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
      #maskCount<-0
      mapPosition=1
      tempBase<-'tmp__'
      #tempMask<-'tmp_mask__'
      
      isFirstMap=TRUE
      rmRastIfExists('tmp_*')
      if(amRastExists('MASK'))execGRASS('r.mask',flags='r')

      message(paste('stack will be merged in this order:',paste(sel,collapse=', ')))
      amUpdateProgressBar(session,"stackProgress",1)

      for(i in 1:length(sel)){
        map<-sel[i]
        message(paste('Proceding map',map,'MASK is',amRastExists('MASK')))
        if(length(grep('stack_barrier__', map))>0){
          if(amRastExists('MASK')){
            execGRASS('r.mapcalc',expression=paste("MASK=isnull(",map,")?MASK:null()"),flags="overwrite")
          }else{
            execGRASS('r.mask',raster=map,flags=c('i'))
          }
        }else{
          tempMap=paste0(tempBase,mapPosition,'_',map)
          execGRASS('r.mapcalc',expression=paste(tempMap,"=",map),flags='overwrite')
        }
        mapPosition=mapPosition+1
        amUpdateProgressBar(session,'stackProgress',i*inc)
      }
      #removing temp mask and active mask
      rmRastIfExists('tmp_mask__*')
      if(amRastExists('MASK'))execGRASS('r.mask',flags='r')
      # get list of tmp__stack... maps.
      tempMapList<-execGRASS('g.list',type='raster',pattern=paste0(tempBase,'*'),intern=TRUE)
      if(length(tempMapList)>1){  
        execGRASS('r.patch',input=paste(tempMapList,collapse=','),output=merged,flags=c('overwrite'))
      }else{
        execGRASS('g.copy',raster=paste0(tempMapList,',',merged),flags='overwrite')
      }

      removeErrorBridge=T
      # In accessmod accessibility analysis, a null cell is a barrier, e.g. a river, mountain, militarized zone.
      # When we patch road maps to landcover maps, small overlaps can appear on top of predefined barrier.
      # Those overlaps act as briges when used in travel time analyis, thus, create shortcuts and wrong calculation.
      # If we used densified lines during rasterization process of roads, we can safely set the "one cell diagonal
      # bridge" as barrier without breaking road continuity. 
      # 
      # Example:
      # X=non-null cell in <road_map>; N=null in <merged_map>; A=non-null cell in <merged_map> 
      # X will be set as null in fallowing cases:
      #
      # X N
      # N A
      #
      # N X
      # A N
      #browser()
      if(removeErrorBridge){
        fromRoad<-sel[grep('stack_road',sel)]
        amBridgeFinder(fromRoad,merged,bridges)
        amBridgeRemover(bridges,removeFromMap=merged)  
      }

      execGRASS('r.colors',map=merged,color='random')
      rmRastIfExists(paste0(tempBase,'*'))
      message(paste(merged,'created'))
      amUpdateProgressBar(session,'stackProgress',100)
      amUpdateDataList(listen)
    }) 
  }
})




#coord=NULL
#for(j in 1:1){
#  for(i in c(j,-j)){
#    for(d in list(c(i,0),c(0,i),c(i,i),c(i,-i))){
#  coord=c(paste0('[',paste(d,collapse=','),']'),coord)
#    }
#  }
#}
#







