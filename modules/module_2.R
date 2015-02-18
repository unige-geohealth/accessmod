#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 2: calc accessibility to health facility.
# Uses r.walk.accessmod, a custom module made for GRASS GIS
#

#----------------------------------------{ UI : validation
output$module2<-renderUI({
  # conditional module display
  mapMerged<-dataList$merged
  mapHf<-dataList$hf
  validInput<-c(
    'm'=length(mapMerged)>0,
    'h'=length(mapHf)>0,
    'g'=length(listen$gisLock)>0
    )
  msgList<-tagList()
  if(!all(validInput)){
    # if not all input are valid, create corresponding message
    msgList$g<-ifelse(!validInput['g'],msgNoLocation,'')
    msgList$h<-ifelse(!validInput['h'],'No health facilities map found. ','')
    msgList$m<-ifelse(!validInput['m'],'No merged land cover map found. ','') 
    amPanel(width=12,tagList(icon('exclamation-triangle'),msgList))
    # display ui if everything is ok.
  }else{
    fluidRow(
      sidebarPanel(width=3,
        formCreateTimeCostMap
        ),
      mainPanel(width=9,
        formTablePanel
        )
      )
  }
})


#----------------------------------------{ UI : create cumulative cost map
formCreateTimeCostMap<-renderUI({
  tableSqlite<-dataList$table
  tableSqlite<-tableSqlite[grep('table_model',tableSqlite)]
  tagList(
    h4('Compute accessibility to health facilities test'),
    amProgressBar('cumulative-progress'),
    p('Create an anistropic cummulative cost map.'),
    selectInput('mergedSelect','Select merged land cover map:',choices=dataList$merged),
    selectInput('hfSelect','Select health facilities map:',choices=dataList$hf),
    selectInput('modelSelect','Select optional model table:',choices=tableSqlite),
    radioButtons('typeAnalysis','Type of analalysis',
      c('Isotropic'='isotropic',
        'Anisotropic'='anistropic'
        ),
      selected='anistropic',
      inline=TRUE
      ),
    conditionalPanel(
      condition="input.typeAnalysis=='anistropic'",
      radioButtons('dirAnalysis','Direction of analysis',
        c("From facilities"="fromHf",
          "Towards facilities"="toHf"),
        selected='toHf',
        inline=TRUE
        )
      ),
    radioButtons("colTable","Use a color table for cummulative map",
      c( "White-blue-black" = "blue",
        "Yellow-green-blue-red-black" = "slope",
        "none" = "none"
        ),selected="slope"),
    numericInput('maxTimeWalk',
      label='Maximum transportation time [minutes]',
      value=120,
      min=0,
      max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
      step=1
      ),
    textInput('costTag','Add tags (minimum 1)',value=''),
    actionButton('btnCreateTimeCostMap','Compute speed map and cumulative cost map')
    )
})


#----------------------------------------{ UI : display model table
formTablePanel<-renderUI({
  tabsetPanel(
    tabPanel("Speed model",
      h4('Table of speed by category and mode of transportation.'),
      fluidRow(
        amPanel(width=6,
          h5('Categories from raster'),
          p("Edit the columns 'speed' and 'mode' or copy and paste from spreadsheet."), 
          actionButton('speedTableUndo',icon=icon('undo'),'reset'),
          hotable("speedRasterTable")
          ),
        amPanel(width=6,
          h5('Categories from table'),
          p('Value from imported from model table. Click on arrow to merge by class.'),
          actionButton('speedTableMerge',icon=icon('long-arrow-left'),'merge'),
          hotable("speedSqliteTable")
          )
        ),
      p(list(strong('Class:'),'merged land cover class')),
      p(list(strong('Label:'),'description of class')),
      p(list(strong('Speed:'), 'speed estimate in [km/h] on flat surface')),
      p(list(strong('Mode'), 'mode of transportation :',names(transpModList)))
      ),
    tabPanel("Health facilities",
      h4('Health facilities'),
      hotable('hfTableModule2')
      )
    )
})


  #----------------------------------------{ Reactivity
  # name validation
  observe({
    costTag<-input$costTag
    if(!is.null(costTag) && nchar(costTag)>0){
      amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=F)
      costTag<-unlist(costTag)
      cumulativeName<-paste(c('cumulative_cost',paste(costTag,collapse=sepTagFile)),collapse=sepTagPrefix )
      if(cumulativeName %in% isolate(dataList$raster)) amMsg(session,type="log",text=paste('Warning: map',cumulativeName,'already exists and will be overwritten. Select other tags to avoid overwriting.'))
      updateTextInput(session,'costTag',value=autoSubPunct(costTag,sepTagUi))
    }else{ 
      amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=T)
    }
  })

  # reactive table with data from raster
  speedRasterTable<-reactive({
    #reactive dependencies
    amDebugMsg('speedRasterTable updated')
    listen$gisLock
    sel<-input$mergedSelect
    if(!is.null(sel) && !sel==''){
      tbl<-read.csv(
        text=execGRASS('r.category',
          map=sel,
          intern=T),
        sep='\t',
        header=F,
        stringsAsFactors=F
        )
      names(tbl)<-c('class','label')
      tbl[,'speed']<-as.integer(0)
      tbl[,'mode']<-as.character('MOTORIZED')
      #tblCat[is.na(tblCat)]<-''
    }else{
      tbl<-data.frame('class'=integer(),'label'=character(),'speed'=integer(),'mode'=character())
    }
    return(tbl)
  })


  # reactive table for speed / module value. Empty if none.
  speedSqliteTable<-reactive({
    amDebugMsg('speedSqliteTable updated')
    listen$gisLock
    sel<-input$modelSelect
    if(!is.null(sel) && !sel==''){
      tbl<-dbGetQuery(listen$dbCon,paste('select * from',sel))
    }else{
      tbl<-data.frame(as.integer(NA),as.character(NA),as.integer(NA),as.character(NA))
      names(tbl)<-acceptColNames[['table_model']] 
    }
    return(tbl)
  })

  # reactive table for speed / module value. Empty if none.
  hfSqliteTableMod2<-reactive({
    listen$gisLock
    selHf<-input$hfSelect
    selMerged<-input$mergedSelect
    if(!is.null(selHf) && !selHf=='' && selHf %in% dataList$vector
      && !is.null(selMerged) && !selMerged=='' &&selMerged %in% dataList$raster){
      onBarrier<-read.table(
        text=execGRASS("v.what.rast",map=selHf,raster=selMerged,flags='p',intern=T)
        ,sep="|",stringsAsFactors=F)
      names(onBarrier)<-c('cat','val')
      onBarrier$amOnBarrier<-ifelse(onBarrier$val=='*',TRUE,FALSE)
      onBarrier$amCatLandCover<-ifelse(onBarrier$val=='*',NA,onBarrier$val)
      onBarrier$val<-NULL
      onBarrier$amSelect<-!sapply(onBarrier$amOnBarrier,isTRUE)
      tbl<-dbGetQuery(listen$dbCon,paste('select * from',selHf))
      tbl<-merge(onBarrier,tbl,by='cat')
    }else{
      data.frame(NULL)
    }
    return(tbl)
  })

  # If new map is selected, update hotable
  output$speedRasterTable<- renderHotable({
    amDebugMsg('output$speedRasterTable updated')
    sel<-input$mergedSelect
    undo<-input$speedTableUndo
    if((!is.null(sel) && !sel=="" || !is.null(undo) && undo>0) && sel %in% dataList$raster){
      speedRasterTable()
    }
  }, readOnly = FALSE, fixed=2, stretch='last')



  # 
  output$hfTableModule2<-renderHotable({
    sel<-input$hfSelect
    if(!is.null(sel) && !sel=="" && sel %in% dataList$vector){
      tbl<-hfSqliteTableMod2()
      nTbl<-names(tbl)[!names(tbl)=='cat'] # remove cat column
      tbl$amOnBarrier<-ifelse(tbl$amOnBarrier==TRUE,'yes','no')
      colOrder<-unique(c('cat','amSelect','amOnBarrier',names(tbl))) 
      tbl[,colOrder] 
    }
  },readOnly=TRUE,fixed=3,stretch='last')

  # render handson table after table model selection OR undo button

  output$speedSqliteTable<-renderHotable({
    amDebugMsg('output$speedSqliteTable updated')
    sel<-input$modelSelect
    undo<-input$speedTableUndo
    if(!is.null(sel) && !sel=="" && sel %in% dataList$table){
      speedSqliteTable()
    }
  }, readOnly = TRUE, fixed=2, stretch='last')



  # table merge process.
  observe({
    btn<-input$speedTableMerge
    tblOrig<-hot.to.df(isolate(input$speedRasterTable))
    tblExt<-hot.to.df(isolate(input$speedSqliteTable))
    if(!is.null(btn) && btn > 0 && length(tblOrig)>0 &&length(tblExt)>0){
      origKeep<-c('class','label')
      tblOrig<-tblOrig[,origKeep]
      tblExt$label<-NULL
      tblMerge<-merge(tblOrig,tblExt,by='class',all.x=TRUE)
      output$speedRasterTable<- renderHotable({tblMerge}, readOnly = FALSE, fixed=2, stretch='last')
    }
  })

  #validate if table is updated
  observe({
    tblUpdated<-hot.to.df(input$speedRasterTable)
    tblOriginal<-isolate(speedRasterTable())
    testNrow<-nrow(tblUpdated)==nrow(tblOriginal)
    if(!is.null(tblUpdated) && !is.null(tblOriginal) && testNrow){
      # rule 1: do not allow changing class and label
      tblValidated<-data.frame(c(tblOriginal[,c('class','label')],tblUpdated[,c('speed','mode')]))
      # rule 2: if Speed is not integer, set to 0
      s<-as.integer(tblUpdated$speed)
      s[is.na(s)]<-as.integer(0)
      # rule 3: if mode is not in allowedModTransp choices, set to NONE
      m<-toupper(tblUpdated$mode)
      mTest<- m %in% names(transpModList)
      m[!mTest]<-'MOTORIZED'
      # update with validated values
      tblValidated$mode<-m
      tblValidated$speed<-s
      output$speedRasterTable<- renderHotable({tblValidated}, readOnly = FALSE, fixed=2, stretch='last')
    }
  })

  # main function to launch grass r.walk.accessmod
  observe({
    btn<-input$btnCreateTimeCostMap # only reactive dependencie on create travel time button.
    tbl<-isolate(hot.to.df(input$speedRasterTable))
    costTag<-isolate(input$costTag)
    mergedSelect<-isolate(input$mergedSelect)
    hfSelect<-isolate(input$hfSelect)
    maxTimeWalk<-isolate(input$maxTimeWalk)
    dirAnalysis<-isolate(input$dirAnalysis)
    typeAnalysis<-isolate(input$typeAnalysis)
    colorTable<-isolate(input$colTable)

    if(!is.null(btn) && btn>0){
      amErrorAction(title='Module 2, cumulative cost',{ 
        # set rules 
        # 0 kmh speed could lead to infinite values when calculate cost in sec (division by 0)
        if(any(tbl$speed==0))stop(
          'Speed of zero km/h is not allowed. Please remove category from the merged map or set a speed value.'
          )
        amUpdateProgressBar(session,"cumulative-progress",5)
        # return path = towards facilities.
        returnPath<-ifelse(dirAnalysis=='toHf',TRUE,FALSE)
        # max cost from minutes to seconds
        maxCost<-maxTimeWalk*60
        # set a max value for null values. 
        #maxValNull<-(ceiling(maxCost/1e4)*1e4)-1
        message(paste(typeAnalysis,'analysis for ',mergedSelect,'requested'))
        tagSplit<-unlist(strsplit(costTag,sepTagUi,fixed=T))
        # maps names
        speedName<-paste(c('speed',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
        costName<-paste(c('friction',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
        cumulativeName<-paste(c('cumulative_cost',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
        modelTable<-paste(c('table_model',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)

        dbWriteTable(listen$dbCon,modelTable,tbl,overwrite=TRUE)

        # set analysis type
        if(typeAnalysis=='anistropic'){
          # ANISOTROPIC using r.walk.accessmod.
          # creation of new classes for speed map (class+km/h), used in r.walk.accessmod
          # Exemples of rules: 
          # oldClasses = newClasses \t newlabels
          # 1 2 3 = 1002 \t WALKING:2
          # 4 =  2020 \t BICYCLING:20
          # 1002 = 3080 \t MOTORIZED:80
          tbl[,'newClass']<-integer()
          # for each row of the model table...
          for(i in 1:nrow(tbl)){
            #... get the mode
            mod<-tbl[i,'mode']
            #... corrsponding to the predefined value from transpModList + given speed
            tbl[i,'newClass']<-as.integer(transpModList[[mod]]$rastVal)+as.integer(tbl[i,'speed'])
          }
          # unique new class
          uniqueNewClass<-unique(tbl$newClass)
          reclassRules<-character()
          for(u in uniqueNewClass){
            oldClasses<-tbl[tbl$newClass==u,'class']
            modeSpeedLabel<-paste(tbl[tbl$newClass==u,c('mode','speed')][1,],collapse=':')
            classRule<-paste(paste(oldClasses,collapse=' '),'=',u,'\t',modeSpeedLabel)
            reclassRules<-c(reclassRules,classRule)
          }
          tmpFile<-tempfile()
          write(reclassRules,tmpFile)
          execGRASS('r.reclass',
            input=mergedSelect,
            #output='tmp__speed',
            output=speedName,
            rules=tmpFile,
            flags='overwrite')
          message(paste(mergedSelect,'reclassed and saved to new map (',speedName,')'))
          amUpdateProgressBar(session,"cumulative-progress",10)

          flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
          flags<-flags[!flags %in% ""]
          message(paste('Module 2 : flags used:',paste(flags,collapse=',')))

          execGRASS('r.walk.accessmod',
            elevation='dem',
            friction=speedName,
            output=cumulativeName,
            start_points=hfSelect,
            max_cost=maxCost, # max cost in seconds.
            flags=flags
            )
          message(paste('Module 2: r.walk.accessmod for ',mergedSelect,'done. Output map:',cumulativeName))
          amUpdateProgressBar(session,"cumulative-progress",80)
        }else{
          # ISOTROPIC using r.cost.
          # creaction of new classes for cost map (seconds) used in r.cost. 
          tbl[,'newClass']<-numeric()
          tbl[,'mode']<-'isotropic'
          # for each row of the model table...
          for(i in 1:nrow(tbl)){
            # km/h to s/m * 1000 (r.reclass works only in integer)
            tbl[i,'newClass']<- (1/(tbl[i,'speed']/3.6))*1000*gmeta6()$nsres
          }
          # unique new class
          uniqueNewClass<-unique(tbl$newClass)
          reclassRules<-character()
          for(u in uniqueNewClass){
            oldClasses<-tbl[tbl$newClass==u,'class']
            modeSpeedLabel<-paste(tbl[tbl$newClass==u,c('mode','speed')][1,],collapse=':')
            classRule<-paste(paste(oldClasses,collapse=' '),'=',u,'\t',modeSpeedLabel)
            reclassRules<-c(reclassRules,classRule)
          }
          tmpFile<-tempfile()
          write(reclassRules,tmpFile)
          execGRASS('r.reclass',
            input=mergedSelect,
            #output='tmp__speed',
            output=costName,
            rules=tmpFile,
            flags='overwrite')
          message(paste(mergedSelect,'reclassed and saved to new map (',speedName,')'))
          amUpdateProgressBar(session,"cumulative-progress",10)
          #flags=c(c('overwrite','s'),ifelse(knight,'k',''),ifelse(returnPath,'t',''))
          flags=c('overwrite')
          message(paste('r.cost flags used:',paste(flags,collapse=',')))
          execGRASS('r.cost',
            input=costName,
            output=cumulativeName,
            start_points=hfSelect,
            max_cost=maxCost*1000,
            flags=flags
            )
          execGRASS('r.mapcalc',expression=paste(cumulativeName,'=',cumulativeName,'/1000'),flags='overwrite')
          message(paste('r.cost for ',mergedSelect,'done. Output map:',cumulativeName))
          amUpdateProgressBar(session,"cumulative-progress",80)
        }
        # remove over passed values :
        # r.walk check for over passed value after last cumulative cost :
        # so if a new cost is added and the new mincost is one step further tan
        # the thresold, grass will keep it and stop algorithm from there.
        if(TRUE){
          execGRASS('r.mapcalc',expression=paste(
              "tmp__map=if(",cumulativeName,"<=",maxCost,",",cumulativeName,",null())"
              ),flags=c('overwrite')
            )
          execGRASS('r.mapcalc',expression=paste(
              cumulativeName,"=tmp__map"
              ),flags=c('overwrite')
            )
          rmRastIfExists('tmp__map')
        }

        message(paste('Cells values superior to ', maxCost,' set to null for ', cumulativeName))
        amUpdateProgressBar(session,"cumulative-progress",95)
        switch(colorTable,
          none=message('No color table selected'),
          blue={
            message('Blue table color table ')
            tempF<-tempfile()
            execGRASS('r.null',map=cumulativeName, null=65535)
            createColorTable(maxCost,paletteFun=paletteBlue,filePath=tempF)
            execGRASS('r.colors',map=cumulativeName,rules=tempF)
          },
          slope={
            message('Slope color table selected ')
            execGRASS('r.colors',map=cumulativeName,color='slope',flags ="e")
          }
          )
        message('color table for ',cumulativeName, 'set to',colorTable)
        amUpdateProgressBar(session,"cumulative-progress",100)
        amUpdateDataList(listen)
        })
    }
  })




