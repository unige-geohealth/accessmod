


output$mod2<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  list(
    h3('Compute accessibility to health facilities'),
    p('Create an anistropic cummulative cost map.'),
    fluidRow(formCreateTimeCostMap),
    hr()
  )
})



formCreateTimeCostMap<-renderUI({
  mL<-mapList()
  mapMerged<-mL$merged
  mapHf<-mL$hf
  if(length(mapMerged)>0 && length(mapHf)>0){  
    list(
      sidebarPanel(
        h4('Compute map of cost'),
        selectInput('mergedSelect','Select merged land cover map:',choices=mapMerged,width=dimselw),
        selectInput('hfSelect','Select health facilities map:',choices=mapHf,width=dimselw),
        selectInput('tropicSelect','Select type of analysis:',choices=names(analysisChoicesList),width=dimselw),
        sliderInput('maxHourSlider',
                    label ='Maximum transportation time (h)' ,
                    value = 2,
                    min=0,
                    max=10,
                    step=0.25,
                    width=dimselw),
        checkboxInput('knight',
                      label = "Use 16 directions (knight's move)? Slower but more accurate",
                      value=TRUE),
        checkboxInput('returnPath',
                      label = "Compute travel cost to facilities instead of from facilities ?",
                      value=FALSE),
        txt('costTag','Add tags (minimum 1)',
            value='',
            sty=stytxt),
        formCostBtn  
      ),
      mainPanel(
        list(
          h4('TimeCost model table'),    
          p('Edit this table or copy and paste cells from a spreadsheet'),
          p("Accessmod doesn't store this table. Please select and copy your changes on a spreadsheet"),
          hr(),
          hotable("mergedMapCatTable"),
          hr(),
          p(list(strong('Class:'),'merged land cover class')),
          p(list(strong('Label:'),'description of class')),
          p(list(strong('Speed:'), 'speed estimate in [km/h] on flat surface')),
          p(list(strong('Mode'), 'mode of transportation :',names(transpModList)))
          
        )
      ))
  }else{
    sidebarPanel(
      if(length(mapMerged)==0){
        p('No merged land cover map found. Please import one or compute one with module 1')
      }else if(length(mapHf)==0){
        p('No health facilities map found. Please import one for this module.')  
      }   
    )
  }
})



# validate tags and return button UI
formCostBtn<-renderUI({
  costTag<-input$costTag
  if(!is.null(costTag) && ! costTag==''){
    costTag<-unlist(costTag)
    updateTextInput(session,'costTag',value=autoSubPunct(costTag,charTag))
    actionButton('btnCreateTimeCostMap','Create cumulative cost map')
  }else{
    p('')
  }
})





# reactive expression to create model table from the categories of land cover merged map
mergedMapCatTable<-reactive({
  sel<-input$mergedSelect
  
  if(!is.null(sel) && !sel==''){
    tblCat<-read.csv(
      text=execGRASS('r.category',
                     map=sel,
                     intern=T),
      sep='\t',
      header=F,
      stringsAsFactors=F
    )
    names(tblCat)<-c('Class','Label')
    tblCat[,'Speed']<-as.integer(0)
    tblCat[,'Mode']<-as.character('NONE')
    #tblCat[is.na(tblCat)]<-''
    return(tblCat)
  }else{
    return(NULL)
  }
})

# If new map is selected, update hotable
observe({
  sel<-input$mergedSelect
  if(!is.null(sel)){
    tbl<-mergedMapCatTable()
    if(!is.null(tbl)){
      output$mergedMapCatTable<- renderHotable({tbl}, readOnly = FALSE)
    }
  }
})


#validate if table is updated
observe({
  tblUpdated<-hot.to.df(input$mergedMapCatTable)
  tblOriginal<-mergedMapCatTable()
  if(!is.null(tblUpdated) && !is.null(tblOriginal)){
    # rule 1: do not allow changing class and label
    tblValidated<-data.frame(c(tblOriginal[,c('Class','Label')],tblUpdated[,c('Speed','Mode')]))
    # rule 2: if Speed is not integer, set to 0
    s<-tblUpdated$Speed
    s<-as.integer(s)
    s[is.na(s)]<-0
    # rule 3: if mode is not in allowedModTransp choices, set to NONE
    m<-toupper(tblUpdated$Mode)
    mTest<- m %in% names(transpModList)
    m[!mTest]<-'NONE'
    # update with validated values
    tblValidated$Mode<-m
    tblValidated$Speed<-s
    output$mergedMapCatTable<- renderHotable({tblValidated}, readOnly = FALSE)
  }
})





observe({
  btn<-input$btnCreateTimeCostMap
  tbl<-isolate(hot.to.df(input$mergedMapCatTable))
  costTag<-isolate(input$costTag)
  mergedSelect<-isolate(input$mergedSelect)
  hfSelect<-isolate(input$hfSelect)
  tropicSelect<-isolate(input$tropicSelect)
  directionSelect<-isolate(input$directionSelect)
  maxHourSlider<-isolate(input$maxHourSlider)
  knight<-isolate(input$knight)
  returnPath<-isolate(input$returnPath)

  
  if(!is.null(btn) && btn>0){ 
    msg(paste('Module 2: r.walk.accessmod for ',mergedSelect,'requested'))
    tagSplit<-unlist(strsplit(costTag,charTag,fixed=T))
    speedName<-paste(c('speed',paste(tagSplit,collapse='_')),collapse=charTagGrass)
    costName<-paste(c('cumulative_cost',paste(tagSplit,collapse='_')),collapse=charTagGrass)
    tbl[,'NewClass']<-integer()
    for(i in 1:nrow(tbl)){
      mod<-tbl[i,'Mode']
      tbl[i,'NewClass']<-transpModList[[mod]]$rastVal+tbl[i,'Speed']
    }
      
    uniqueNewClass<-unique(tbl$NewClass)
    reclassRules<-character()
    
    
    for(u in uniqueNewClass){
      oldClasses<-tbl[tbl$NewClass==u,'Class']
      modeSpeedLabel<-paste(tbl[tbl$NewClass==u,c('Mode','Speed')][1,],collapse=':')
      classRule<-paste(paste(oldClasses,collapse=' '),'=',u,'\t',modeSpeedLabel)
      reclassRules<-c(reclassRules,classRule)
    }
    ## Rules will be like this
    # 1 2 3 = 1002 \t WALKING:2
    # 4 =  2020 \t BICYCLING:20
    # 1002 = 3080 \t NONE:80
    
    tmpFile<-tempfile()
    write(reclassRules,tmpFile)
    
    execGRASS('r.reclass',
              input=mergedSelect,
              output=speedName,
              rules=tmpFile,
              flags='overwrite')
    msg(paste('Module 2:',mergedSelect,'reclassed'))
    
    flags=c(c('overwrite','s'),ifelse(knight,'k',''),ifelse(returnPath,'t',''))
    flags<-flags[!flags %in% ""]
    msg(paste('Module 2 : flags used:',paste(flags,collapse=',')))
    
    execGRASS('r.walk.accessmod',
              elevation='dem',
              friction=speedName,
              output=costName,
              start_points=hfSelect,
              max_cost=maxHourSlider*3600,
              flags=flags
              )
    msg(paste('Module 2: r.walk.accessmod for ',mergedSelect,'done. Output map:',costName))
  }
  
})











