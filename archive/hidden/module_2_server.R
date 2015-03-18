

output$transpModList<-renderText({names(transpModList)})


# populate select input

observe({
  mergedList<-dataList$raster[grep('^merged__',dataList$raster)]
  if(length(mergedList)==0)mergedList=""
  updateSelectInput(session,'mergedSelect',choices=mergedList,selected=mergedList[1])
})

observe({
  hfList<-dataList$vector[grep('^health_facilities__',dataList$vector)]
  if(length(hfList)==0)hfList=""
  updateSelectInput(session,'hfSelect',choices=hfList,selected=hfList[1])

})


observe({
  modelList<-dataList$table[grep('^table_model__*',dataList$table)]
  if(length(modelList)==0)modelList=""
  updateSelectInput(session,'modelSelect',choices=modelList,selected=modelList[1])
})




# name validation
observe({
  costTag<-input$costTag
  if(!is.null(costTag) && nchar(costTag)>0){
    amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=F)
    costTag<-unlist(costTag)
    cumulativeName<-paste(c('cumulative_cost',paste(costTag,collapse=sepTagFile)),collapse=sepClass )
    if(cumulativeName %in% isolate(dataList$raster)) amMsg(session,type="log",text=paste('Warning: map',cumulativeName,'already exists and will be overwritten. Select other tags to avoid overwriting.'))
    updateTextInput(session,'costTag',value=amSubPunct(costTag,sepTagUi))
  }else{ 
    amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=T)
  }
})




speedRasterTable<-reactive({
  sel<-amNameCheck(input$mergedSelect,'raster')
  undo<-input$speedTableUndo
  isolate({
    if(length(sel)>0 || (length(sel)>0 && !is.null(undo) && undo>0)){
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
      #tbl<-data.frame('class'="",'label'="",'speed'="",'mode'="")
      tbl<-data.frame(as.integer(NA),as.character(NA),as.integer(NA),as.character(NA)) 
      names(tbl)<-acceptColNames[['table_model']] 
    }
  })
  tbl
})

# display handson table of speed table from raster.
observe({ 
  tbl<-speedRasterTable()
  output$speedRasterTable <- renderHotable({tbl}, readOnly = FALSE, fixed=2, stretch='last')
})


# render handson table from sqlite lcv table
observe({
  # reactive table for speed / module value. Empty if none.
  sel<-amNameCheck(input$modelSelect,'table')
  isolate({
    if(!is.null(sel)){
      tbl<-dbGetQuery(listen$dbCon,paste('select * from',sel))
    }else{
      tbl<-data.frame(as.integer(NA),as.character(NA),as.integer(NA),as.character(NA))
      names(tbl)<-acceptColNames[['table_model']] 
    }
    output$speedSqliteTable<-renderHotable({
      tbl
    },readOnly=TRUE,fixed=2,stretch='last')
  })
})

# render handson table for HF
observe({
  selHf<-amNameCheck(input$hfSelect,'vector')
  selMerged<-amNameCheck(input$mergedSelect,'raster')
  isolate({
    if(!is.null(selHf) && !is.null(selMerged)){
      # check if HF are located on barrier
      tbl<-read.table(
        text=execGRASS("v.what.rast",map=selHf,raster=selMerged,flags='p',intern=T)
        ,sep="|",stringsAsFactors=F)
      names(tbl)<-c('cat','val')
      tbl$amOnBarrier<-ifelse(tbl$val=='*',TRUE,FALSE)
      tbl$amCatLandCover<-ifelse(tbl$val=='*',NA,tbl$val)
      tbl$val<-NULL
      tbl$amSelect<-!sapply(tbl$amOnBarrier,isTRUE)
      # copy hf attribute table
      tblAttribute<-dbGetQuery(isolate(listen$dbCon),paste('select * from',selHf))
      # merge with first table
      tbl<-merge(tbl,tblAttribute,by='cat')
      nTbl<-names(tbl)[!names(tbl)=='cat'] # remove cat column
      tbl$amOnBarrier<-ifelse(tbl$amOnBarrier==TRUE,'yes','no')# avoid handsontable checkbox.
      colOrder<-unique(c('cat','amSelect','amOnBarrier',names(tbl))) 
      tbl<-tbl[,colOrder] 
    }else{
      tbl=data.frame(cat=as.integer(NA),amSelect=as.integer(NA),amOnBarrier=as.integer(NA))
    }
    output$hfTableModule2<-renderHotable({
      tbl
    },readOnly=TRUE,fixed=4,stretch='last')
  })
})

## when tbl and hf select are set, render htable
#observe({
#  sel<-amNameCheck(input$hfSelect,'vector')
#  tbl<-htTableModule2()
#  if(!is.null(sel) && !is.null(tbl)){
#    if(length(tbl)>1 && nrow(tbl)>1){
#    }else{
#      tbl=data.frame(cat=as.integer(NA))
#    }
#    output$hfTableModule2<-renderHotable({
#      tbl
#    },readOnly=TRUE,fixed=3,stretch='last')
#  }
#})
#

## render handson table after table model selection OR undo button
#observe({
#  sel<-amNameCheck(input$modelSelect,'table')
#  undo<-input$speedTableUndo
#  tbl<-listen$speedSqliteTable
#  if((!is.null(undo)&& length(undo) > 1) || !is.null(sel) && !is.null(tbl)){
#      }
#})
#


# table merge process.
observe({
  btn<-input$speedTableMerge
  isolate({
    tblOrig<-hot.to.df(input$speedRasterTable)
    tblExt<-hot.to.df(input$speedSqliteTable)
    if(!is.null(btn) && btn > 0 && length(tblOrig)>0 &&length(tblExt)>0){ 
      origKeep<-c('class','label')
      tblOrig<-tblOrig[,origKeep]
      tblExt$label<-NULL
      tblMerge<-merge(tblOrig,tblExt,by='class',all.x=TRUE)
      output$speedRasterTable<- renderHotable({tblMerge}, readOnly = FALSE, fixed=2, stretch='last')
    }
  })
})

#validate if table is updated
observe({
  tblUpdated<-hot.to.df(input$speedRasterTable)
  isolate({
    if(!is.null(tblUpdated)){
      tblOriginal<-speedRasterTable()
      testNrow<-nrow(tblUpdated)==nrow(tblOriginal)
      #testValidClass<-!any(tblOriginal=="")||!any(tblUpdated=="")
      testValidClass<-!anyNA(tblOriginal)||!anyNA(tblUpdated)
      if(!is.null(tblOriginal) && isTRUE(testNrow) &&isTRUE(testValidClass)){
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
      }else{
        tblValidated=tblOriginal
      }
      output$speedRasterTable<- renderHotable({tblValidated}, readOnly = FALSE, fixed=2, stretch='last')
    }
  })
})

# main function to launch grass r.walk.accessmod
observe({
  btn<-input$btnCreateTimeCostMap # only reactive dependencie on create travel time button.
  isolate({
    tbl<-hot.to.df(input$speedRasterTable)
    costTag<-input$costTag
    mergedSelect<-amNameCheck(input$mergedSelect,'raster')
    hfSelect<-amNameCheck(input$hfSelect,'vector')
    maxTimeWalk<-input$maxTimeWalk
    dirAnalysis<-input$dirAnalysis
    typeAnalysis<-input$typeAnalysis
    #colorTable<-isolate(input$colTable)

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
        speedName<-paste(c('speed',paste(tagSplit,collapse='_')),collapse=sepClass)
        costName<-paste(c('friction',paste(tagSplit,collapse='_')),collapse=sepClass)
        cumulativeName<-paste(c('cumulative_cost',paste(tagSplit,collapse='_')),collapse=sepClass)
        modelTable<-paste(c('table_model',paste(tagSplit,collapse='_')),collapse=sepClass)

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
            elevation=configDem,
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
        # switch(colorTable,
        #   none=message('No color table selected'),
        #   blue={
        #     message('Blue table color table ')
        #     tempF<-tempfile()
        #     execGRASS('r.null',map=cumulativeName, null=65535)
        #     createColorTable(maxCost,paletteFun=paletteBlue,filePath=tempF)
        #     execGRASS('r.colors',map=cumulativeName,rules=tempF)
        #   },
        #   slope={
        #     message('Slope color table selected ')
        execGRASS('r.colors',map=cumulativeName,color='slope',flags ="e")
        # }
        # )
        #message('color table for ',cumulativeName, 'set to',colorTable)
        amUpdateProgressBar(session,"cumulative-progress",100)
        amUpdateDataList(listen)
        })
    }
  })
})



