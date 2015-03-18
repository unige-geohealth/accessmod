#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 3: calc accessibility to health facility.
# 
# server


output$transpModList<-renderText({names(transpModList)})

# populate select input

observe({
  mergedList<-dataList$raster[grep('^merged__',dataList$raster)]
  if(length(mergedList)==0)mergedList=character(1)
  updateSelectInput(session,'mergedSelect',choices=mergedList,selected=mergedList[1])
})

observe({
  hfList<-dataList$vector[grep('^health_facilities__',dataList$vector)]
  if(length(hfList)==0)hfList=character(1)
  updateSelectInput(session,'hfSelect',choices=hfList,selected=hfList[1])
})

observe({
  zoneAdminList<-dataList$vector[grep('^zone_admin__',dataList$vector)]
  if(length(zoneAdminList)==0)zoneAdminList=character(1)
  updateSelectInput(session,'zoneAdminSelect',choices=zoneAdminList,selected=zoneAdminList[1])
})

observe({
  modelList<-dataList$table[grep('^table_model__*',dataList$table)]
  if(length(modelList)==0)modelList=character(1)
  updateSelectInput(session,'modelSelect',choices=modelList,selected=modelList[1])
})


observe({
  popList<-dataList$raster[grep('^population__*',dataList$raster)]
  if(length(popList)==0)modelList=character(1)
  updateSelectInput(session,'popSelect',choices=popList,selected=popList[1])
})


hfFields<-reactive({
  hfSel<-amNameCheck(input$hfSelect,'vector')
  if(length(hfSel)>0){
    tblSample<-dbGetQuery(listen$dbCon,paste("SELECT * FROM",amNameCheck(input$hfSelect),"LIMIT 1"))
    all<-sapply(tblSample,typeof)
    num<-all[sapply(tblSample,is.numeric)]
    list(
      num=num
      )
  }else{
    list()
  }
})

observe({
  hfFields<-names(hfFields()$num)
  if(length(hfFields)>0){
    hfFields<-hfFields[!hfFields =='cat']
    capPos<-grep('[cC]apac',hfFields)
    if(length(capPos)>0){sel=hfFields[capPos][1]}else{sel=hfFields[1]}
    updateSelectInput(session,'hfCapacityField',choices=hfFields,selected=sel)
  }
})

observe({
  hfCapacity<-input$hfCapacityField
  hfFields<-names(hfFields()$num)
  if(length(hfCapacity)>0){
    hfFields<-hfFields[!hfFields %in% hfCapacity]
  }else{ 
    hfFields=""
  }
  updateSelectInput(session,'hfGroupField',choices=hfFields, selected='cat')
})


# tag format
observe({
  costTag<-input$costTag
  if(isTRUE(nchar(costTag)>0)){
    updateTextInput(session,'costTag',value=amSubPunct(costTag,sepTagUi))
  }
})




# preventive field validation
observe({
  amErrorAction(title='Module 3: validation',{
    #empty msg container
    err = character(0)
    info = character(0)
    # map validation.
    merged<-isTRUE(!is.null(amNameCheck(input$mergedSelect,'raster')))
    hf<-isTRUE(!is.null(amNameCheck(input$hfSelect,'vector')))
    pop<-isTRUE(!is.null(amNameCheck(input$popSelect,'raster'))) 
    # table validation
    #tblHf<-any(hot.to.df(input$hfTable)$select) ## too slow !
    tblModel<-!any(hot.to.df(input$speedRasterTable)$speed <1)
    # parameter validation
    costTag<-input$costTag
    tag<-isTRUE(nchar(costTag)>0)
    maxTT<-isTRUE(input$maxTimeWalk>0)
    #cap<-input$capacityAnalysis == 'eachHf'
    cap<-isTRUE(input$moduleSelector == 'module_3')
    groupField<-isTRUE(length(input$hfGroupField)>0)
    capField<-isTRUE(length(input$hfCapacityField)>0)
    hfBuffer<-isTRUE(input$hfOrder == 'circBuffer')
    popBuffer<-isTRUE(input$popBufferRadius > listen$mapMeta$grid$`North`)
    # map overwrite warning.
    costTag<-unlist(costTag)
    cumulativeName<-paste(c('cumulative_cost',paste(costTag,collapse=sepTagFile)),collapse=sepClass )
    mapExists <-isTRUE(cumulativeName %in% amNameCheck(isolate(dataList$raster),'raster'))

    # set message(s)
    # errors
    if(!tag) err = c(err,'no tags entered')
    if(!merged) err = c(err,'merged land cover missing')
    if(!hf) err = c(err,'health facilities map missing')
    if(!maxTT) err = c(err,'max travel time to small')
    #if(hf)if(!tblHf) err = c(err,'at least one facilities must be selected') ## too slow
    if(merged)if(!tblModel) err = c(err,'speed of 0 km/h not allowed')
    if(cap){
      if(!pop) err = c(err,'population map missing')
      if(!groupField) err = c(err,'no group/id field set for hf')
      if(!capField) err = c(err,'no capacity field set for hf')
      if(hfBuffer)if(!popBuffer) err = c(err,'circular buffer must be superior to project resolution')
    }
    # informations
    if(mapExists) info = c(info,paste('map',cumulativeName,'exists and will be overwritten, along with corresponding new facilities map, speed map or friction map. However, if changes has been made to the homonymic model (speed table), a new table will be stored with a distinctive tag.'))

    # create HTML for
    if(length(err)>0){
      err<-tags$ul(
        HTML(paste("<li>",icon('exclamation-triangle'),err,"</li>",collapse=""))
        )
      disBtn=TRUE
    }else{
      disBtn=FALSE
    }
    if(length(info)>0) info<- tags$ul(HTML(paste("<li>",icon('info-circle'),info,"</li>",collapse="")))

    # send result to ui
    if(length(err)>0 || length(info)>0){
      msgList<-tagList(tags$b('Validation'),err,info)
    }else{
      msgList=tagList(tags$b('Ok to compute map'))
    }

    output$msgModule3 <-renderUI({msgList})
    amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=disBtn)
      })
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
      #tbl<-data.frame('class'=character(1),'label'=character(1),'speed'=character(1),'mode'=character(1))
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
# system time : 197 h 197 hf 
#  user  system elapsed
#  0.041   0.030   0.079
observe({
  selHf<-amNameCheck(input$hfSelect,'vector')
  selMerged<-amNameCheck(input$mergedSelect,'raster')
  selPop<-amNameCheck(input$popSelect,'raster')
  isolate({
    if(!is.null(selHf) && !is.null(selMerged)){
      # check if HF are located on barrier
      tbl<-read.table(
        text=execGRASS("v.what.rast",map=selHf,raster=selMerged,flags='p',intern=T),
        sep="|",stringsAsFactors=F)
      names(tbl)<-c('cat','val')
      tbl$amOnBarrier<-ifelse(tbl$val=='*',TRUE,FALSE)
      tbl$amCatLandCover<-ifelse(tbl$val=='*',NA,tbl$val)
      tbl$val<-NULL

      if(!is.null(selPop)){
        pop<-read.table(
          text=execGRASS('v.what.rast',map=selHf,raster=selPop,flags='p',intern=T),
          sep="|",stringsAsFactors=F)  
        names(pop)<-c('cat','amPopCell')
        pop[pop$amPopCell=='*','amPopCell']<-0 
        pop$amPopCell<-as.numeric(pop$amPopCell)
        tbl<-merge(tbl,pop,by='cat')
      }
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
    output$hfTable<-renderHotable({
      tbl
    },readOnly=TRUE,fixed=5,stretch='last')
  })
})


observe({
  btnNoHf<-input$btnSelecteNoHf
  if(!is.null(btnNoHf) && btnNoHf>0){
    isolate({
      tbl<-hot.to.df(input$hfTable)
      tbl$amSelect=FALSE
      output$hfTable<-renderHotable({
        tbl
      },readOnly=TRUE,fixed=4,stretch='last')
    })
  }
})
observe({
  btnAllHf<-input$btnSelectAllHf
  if(!is.null(btnAllHf) && btnAllHf>0){
    isolate({
      tbl<-hot.to.df(input$hfTable)
      tbl$amSelect=TRUE
      output$hfTable<-renderHotable({
        tbl
      },readOnly=TRUE,fixed=4,stretch='last')
    })
  }
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
      #testValidClass<-!any(tblOriginal==character(1))||!any(tblUpdated==character(1))
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



observe({
  btn<-input$btnCreateTimeCostMap
  if(!is.null(btn)&&btn>0){
    amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=TRUE)
  }
})


# main function to launch grass r.walk.accessmod
observe({
  timeCheck<-system.time({
    btn<-input$btnCreateTimeCostMap # only reactive dependencie on create travel time button.
    if(!is.null(btn) && btn>0){
      isolate({
        # tables
        tbl<-hot.to.df(input$speedRasterTable)
        tblHf<-hot.to.df(input$hfTable)
        # tags
        costTag<-input$costTag 
        # maps
        mapMerged<-amNameCheck(input$mergedSelect,'raster')
        mapHf<-amNameCheck(input$hfSelect,'vector')
        mapPop<-amNameCheck(input$popSelect,'raster')
        # parameters
        maxTimeWalk<-input$maxTimeWalk
        dirAnalysis<-input$dirAnalysis
        typeAnalysis<-input$typeAnalysis 
        capAnalysis<-input$moduleSelector # module_3 or module_2
        groupField<-input$hfGroupField
        capField<-input$hfCapacityField
        hfOrder<-input$hfOrder
        hfOrderSorting<-input$hfOrderSorting
        popBuffer<-input$popBufferRadius
        modParam<-input$mod3param
        # local parameter
        # return path = towards facilities.
        returnPath<-ifelse(dirAnalysis=='toHf',TRUE,FALSE)
        # max cost from minutes to seconds
        maxCost<-maxTimeWalk*60
        # map name formating
        tags<-unlist(strsplit(costTag,sepTagUi,fixed=T))
        mapSpeed<-paste(c('speed',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        mapFriction<-paste(c('friction',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        mapCumulative<-paste(c('cumulative_cost',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        mapPopResidual<-paste(c('population_residual',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        hfCatchment<-paste(c('health_facilities_catchment',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        tableModel<-paste(c('table_model',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        tableHfOut<-paste(c('table_hf',paste(tags,collapse=sepTagFile)),collapse=sepClass)
        # temporary map, will be deleted.
        mapTmpHf<-paste('tmp_hf')
        # start process
        message(paste(typeAnalysis,'analysis for ',mapMerged,'requested'))
        amUpdateProgressBar(session,"cumulative-progress",5)

        amErrorAction(title=paste(input$moduleSelector,' cumulative cost'),{ 

          # test if a table with same name exists. 
          # a.If their content are identical, do nothing. 
          # b.If their content differ add a short time stamp to the name
          # c.if the table doesnt exists, save it.

          if(tableModel %in% amNameCheck(dataList$table,'table')){
            tblStored<-dbGetQuery(listen$dbCon,paste("SELECT * FROM",tableModel))
            if(!identical(tblStored,tbl)){
              tableMode=paste0(tableModel,'_',amSysTime('short'))
              dbWriteTable(listen$dbCon,tableModel,tbl,overwrite=TRUE)
            }
          }else{ 
            dbWriteTable(listen$dbCon,tableModel,tbl,overwrite=TRUE)
          }
          # create HF subset
          tblHfSubset<-tblHf[tblHf$amSelect==TRUE,]
          qSql<-paste("cat IN (",paste0("'",tblHfSubset$cat,"'",collapse=','),")")
          execGRASS("v.extract",flags='overwrite',input=mapHf,where=qSql,output=mapTmpHf)

          # create base map for travel time computation.
          if(typeAnalysis == 'anisotropic'){
            amCreateSpeedMap(tbl,mapMerged,mapSpeed)
          }else{      
            amCreateFrictionMap(tbl,mapMerged,mapFriction,mapResol=listen$mapMeta$grid$North)
          }

          amUpdateProgressBar(session,"cumulative-progress",10)

          #   amMsg(session,'ui',title='Module 2',
          #     paste("Accessibility computed for '",mapMerged,
          #       "'. Speed map='",mapSpeed,
          #       "'. Model table='",tableModel,
          #       "'. Output='",mapCumulative,"'."))
          # set analysis 
          switch(capAnalysis,
            'module_2'={
              switch(typeAnalysis,
                'anisotropic'= amAnisotropicTravelTime(
                  inputSpeed=mapSpeed,
                  inputHf=mapTmpHf,
                  outputCumulative=mapCumulative,
                  returnPath=returnPath,
                  maxCost=maxCost),
                'isotropic'= amIsotropicTravelTime(
                  inputFriction=mapFriction,
                  inputHf=mapTmpHf,
                  outputCumulative=mapCumulative,
                  maxCost=maxCost
                  ),
                error(paste(typeAnalysis,'analysis not implemented'))
                )
              amUpdateProgressBar(session,"cumulative-progress",100)
            },
            'module_3'={
              tblOut<-amAnisotropicCapacityAnalysis(
                inputSpeed=mapSpeed,
                inputFriction=mapFriction,
                inputPop=mapPop,
                inputHf=mapHf,
                inputTblHf=tblHfSubset,
                outputPopResidual=mapPopResidual,
                outputTblHf=tableHfOut,
                outputHfCatchment=hfCatchment,
                removeCapted='rmPop' %in% modParam,
                vectCatch='vectCatch' %in% modParam,
                typeAnalysis=typeAnalysis,
                returnPath=returnPath,
                radius=popBuffer,
                maxCost=maxCost,
                groupField=groupField,
                capField=capField,
                hfOrder=hfOrder,
                hfOrderSorting=hfOrderSorting
                )
              dbWriteTable(listen$dbCon,tableHfOut,tblOut,overwrite=T)
            }
            )
          amUpdateDataList(listen)
          }) # close isolate
      }) # close error handling 
    }
  })
  print(timeCheck)
})


amRmOverPassedTravelTime<-function(map,maxCost){
  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.

  execGRASS('r.mapcalc',expression=paste(
      "tmp__map=if(",map,"<=",maxCost,",",map,",null())"
      ),flags=c('overwrite')
    )
  execGRASS('r.mapcalc',expression=paste(
      map,"=tmp__map"
      ),flags=c('overwrite')
    )
  rmRastIfExists('tmp__map')
}

amCreateSpeedMap<-function(tbl,mapMerged,mapSpeed){
  amDebugMsg('AmCreateSpeedMap')
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
    input=mapMerged,
    #output='tmp__speed',
    output=mapSpeed,
    rules=tmpFile,
    flags='overwrite')
}

amCreateFrictionMap<-function(tbl,mapMerged,mapFriction,mapResol){
  amDebugMsg('amCreateFrictionMap')

  # creaction of new classes for cost map (seconds) used in r.cost. 
  tbl[,'newClass']<-numeric()
  tbl[,'mode']<-'isotropic'
  # for each row of the model table...
  for(i in 1:nrow(tbl)){
    # km/h to s/m 
    # the time to cover one unit of distance * actual distance (map resolution) == cost to cross a given cell. 
    tbl[i,'newClass']<- (1/(tbl[i,'speed']/3.6))*mapResol
  }

  # unique new class
  uniqueNewClass<-unique(tbl$newClass)
  reclassRules<-character()
  categoryRules<-character()


  for(u in uniqueNewClass){
    oldClasses<-tbl[tbl$newClass==u,'class']
    reclassRule<-paste0(oldClasses,':',oldClasses,':',u,':',u)
    reclassRules<-c(reclassRules,reclassRule)
    catLabel<-paste(
      paste(tbl[tbl$newClass==u,]$label,collapse='/'),
      u,'[s]/',mapResol,'[m]')
    categoryRule<-paste0(u,':',catLabel)
    categoryRules<-c(categoryRules,categoryRule)
  }

  tmpFile<-tempfile()
  write(reclassRules,tmpFile)
  execGRASS('r.recode',
    input=mapMerged,
    #output='tmp__speed',
    output=mapFriction,
    rules=tmpFile,
    flags='overwrite')

  write(categoryRules,tmpFile)
  execGRASS('r.category',
    map=mapFriction,
    separator=':',
    rules=tmpFile
    )
}

amIsotropicTravelTime<-function(inputFriction,inputHf,outputCumulative,maxCost){
  amDebugMsg('amIsotropicTravelTime')
  flags=c('overwrite')
  message(paste('r.cost flags used:',paste(flags,collapse=',')))
  execGRASS('r.cost',
    input=inputFriction,
    output=outputCumulative,
    start_points=inputHf,
    # cost in friction map was set a factor 1000 to compensate conversion from km/h to s/m.
    # see amCreateFrictionMap
    max_cost=maxCost,
    flags=flags
    )
  amRmOverPassedTravelTime(outputCumulative,maxCost) 
  execGRASS('r.mapcalc',expression=paste(outputCumulative,'=',outputCumulative,'/1000'),flags='overwrite')
}

amAnisotropicTravelTime<-function(inputSpeed,inputHf, outputCumulative, returnPath,maxCost){
  amDebugMsg('amAnisotropicTravelTime')
  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
  flags<-flags[!flags %in% character(1)]
  execGRASS('r.walk.accessmod',
    elevation=configDem,
    friction=inputSpeed,
    output=outputCumulative,
    start_points=inputHf,
    max_cost=maxCost, # max cost in seconds.
    flags=flags
    ) 
  amRmOverPassedTravelTime(outputCumulative,maxCost) 
}

amCircularTravelDistance<-function(inputHf,outputBuffer,radius){
  execGRASS('v.to.rast',input=inputHf,output='tmp_buffer',use='val',value=1,flags='overwrite')
  execGRASS('r.buffer',input='tmp_buffer',output=outputBuffer,distances=radius, flags='overwrite')
  execGRASS('r.mapcalc',expression=paste(outputBuffer,'=if(!isnull(',outputBuffer,'),1,null())'))
}




amAnisotropicCapacityAnalysis<-function(inputSpeed,inputFriction,inputPop,inputHf,inputTblHf,outputPopResidual,outputTblHf,outputHfCatchment,removeCapted,vectCatch,typeAnalysis,returnPath,maxCost,radius,groupField,capField,hfOrder=NULL,hfOrderSorting=NULL){
  # nested call if requested order is not given by input hf table
  # hfOrder could be 'tableOrder','travelTime' or 'circlBuffer'
  # If hfOrder is not 'tableOrder' or 'circBuffer', an isotropic or anisotropic will be done.
  # In this case, typeAnalysis will be set from parent function call.
  if(!hfOrder == 'tableOrder' && ! is.null(hfOrder)){
    popWithinDist<-amAnisotropicCapacityAnalysis(
      inputSpeed=inputSpeed,
      inputFriction=inputFriction,
      inputPop=inputPop,
      inputHf=inputHf,
      inputTblHf=inputTblHf,
      outputPopResidual='tmp_nested_p',
      outputTblHf="tmp_nested_hf",
      outputHfCatchment="tmp_nested_catch",
      removeCapted=FALSE,
      vectCatch=FALSE,
      typeAnalysis=ifelse(hfOrder == 'circBuffer','circular',typeAnalysis),
      returnPath=returnPath,
      radius=radius,
      maxCost=maxCost,
      groupField=groupField,
      capField=capField,
      hfOrder=NULL,
      hfOrderSorting=NULL
      )[c(groupField,'amPopWithinMaxDist')]
    hfOrderDecreasing<-ifelse(hfOrderSorting=='hfOrderDesc',TRUE,FALSE)
    orderId<-popWithinDist[order(
      popWithinDist$amPopWithinMaxDist,decreasing=hfOrderDecreasing
      ),groupField]
  }else{
    orderId=unique(inputTblHf[,groupField])
  }
  # temporary maps
  tmpHf='tmp_h' # vector hf tmp
  tmpCost='tmp_c' # cumulative cost tmp
  tmpPop='tmp_p' # population catchment to substract
 
  # empty data frame for storing capacity summary
  tblOut<-data.frame()
  # set travel time inner ring and outer ring to zero
  amTtInner=0
  amTtOuter=0
  # copy population map to create residual version
  execGRASS('g.copy',raster=c(inputPop,outputPopResidual),flags='overwrite') 
  # set increment for counter and progressbar
  inc=90/length(orderId)
  incN=0
  for(i in orderId){
    incN=incN+1
    # extract subset of facilities by group id
    qSql<-paste(groupField,"IN (",paste0("'",i,"'",collapse=','),")")
    execGRASS("v.extract",flags='overwrite',input=inputHf,where=qSql,output=tmpHf)
    # compute cost map or distance map
    switch(typeAnalysis,
      'anisotropic'=amAnisotropicTravelTime(
        inputSpeed=inputSpeed,
        inputHf=tmpHf, 
        outputCumulative=tmpCost, 
        returnPath=returnPath,
        maxCost=maxCost
        ),
      'isotropic'=amIsotropicTravelTime(
        inputFriction=inputFriction,
        inputHf=tmpHf,
        outputCumulative=tmpCost,
        maxCost=maxCost
        ),
      'circular'=amCircularTravelDistance(
        inputHf=tmpHf,
        outputBuffer=tmpCost,
        radius=radius
        )
      )
    # calculate integer version of cumulated cost map for zonal statistics
    execGRASS('r.mapcalc',expression=paste(tmpCost,'=int(',tmpCost,')'),flags='overwrite')
    # zonal stat
    tblPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags=c('g','t','overwrite'),
        map=outputPopResidual, 
        zones=tmpCost, # zone == travel time
        intern=T
        ),sep='|',header=T)
    # calculate cumulated sum of pop at each zone
    tblPopByZone$cumSum<-cumsum(tblPopByZone$sum)
    tblPopByZone<-tblPopByZone[c('zone','sum','cumSum')]
    # After cumulated sum, order was not changed, we can use tail/head to extract min max
    totalPop<-tail(tblPopByZone,n=1)$cumSum
    firstCellPop<-head(tblPopByZone,n=1)$cumSum
    # sum in case of multiple hf (group of id).
    hfCap<-sum(inputTblHf[inputTblHf[groupField]==i,capField])
    # get the travel time before the limit
    # first zone where pop <= hf capacity
    # if NA -> hf capacity is already overpassed before the first cumulated cost zone. 
    # E.g. In the cell where the facility is located, the population outnumber the capacity.
    zInner<-tblPopByZone[tblPopByZone$cumSum<=hfCap,c('zone','cumSum')]
    # get the travel time that overpass capacity
    # if NA -> travel time zone is too low to over pass hf capacity
    #first zone where pop > hf capacity
    zOuter<-tblPopByZone[tblPopByZone$cumSum>hfCap,c('zone','sum')]
    # get remove the  population catchment fom population
    # hfCapResidual pop residual to removed from the next zone in pop residual
    hfCapResidual= NA  #remaining capacity in HF.
    zMaxInner = NULL
    zMaxOuter = NULL
    # cat is used a key field in vector maps : set another name
    if(groupField=='cat'){
      groupFieldNew='catOrig'
    }else{
      groupFieldNew=groupField
    }
    # Inner ring calculation
    if(!any(is.na(zInner))&&!length(zInner$zone)==0){
      # last zone where population cumulated sum is lower or egal to hf capacity
      zMaxInner<-max(zInner$zone)
      # create temporary population inner ring mask
      execGRASS('r.mapcalc',expression=paste(
          tmpPop,'=if(',tmpCost,'<=',max(zInner$zone),',',i,',null())'
          ),flags='overwrite')
      # create population subset for the inner ring mask.
      if(removeCapted){
        execGRASS('r.mask',raster=tmpPop,flags='i')
        execGRASS('r.mapcalc',expression=paste(
            outputPopResidual,"=",outputPopResidual
            ),flags='overwrite')
        execGRASS('r.mask',flags='r')
      }
      # if zero, HF can provide services exactly for the pop within this zone
      hfCapResidual=hfCap-max(zInner$cumSum)
      # Vector version of HF catchment
      if(vectCatch){ 
        # output catchment as vector. SHOULD BE EASY TO DOWNLOAD BY USER.
        # None of those methods worked at the time this script was written :
        # v.patch : produced empty area and topologic errors, even without topology building (b flag)
        # v.to.3d with groupId as height and v.patch after : transform back to 2d with area errors.
        # r.to.rast3 : doesn't produce anything + 3d interface really bugged.
        # v.overlay : great geometry / topology, but not compatible with hundred of overlays :
        #           produce many layers OR many columns for each pairs of overlays
        # So, export and append to shapefile, reimport back after the loop.
        tDir<-tempdir()      
        tmpVectCatchOut=file.path(tDir,paste0('tmp_vect_catch','.shp'))
        execGRASS('r.to.vect',input=tmpPop,output='tmp_vect_catch',type='area',flags=c('overwrite'),column=groupFieldNew)
        # for the first hf/group, remove old shapefile
        if(incN==1){
          file.remove(tmpVectCatchOut)
          outFlags=c('overwrite')
        }else{
          outFlags=c('a')
        }
        # export in shapefile, sorry.
        execGRASS('v.out.ogr',input='tmp_vect_catch',output=tmpVectCatchOut,format='ESRI_Shapefile',
          flags=outFlags,output_layer='tmp_vect_catch')
      }
    }

    # if no inner ring has been computed, set hfCap as the value to be removed from current or next zone.
    if(is.na(hfCapResidual))hfCapResidual=hfCap
    # outer ring calculation to fill remaining place in HF, if available
    if(!any(is.na(zOuter)) &&  hfCapResidual>0 && nrow(zOuter)>0){
      #calculate cumulative pop count for outer ring.
      zOuter$cumSum<-cumsum(zOuter$sum)
      # if whithin outer ring, there isn't enough pop to fill hf capacity, remove all population.
      if(max(zOuter$cumSum)<=hfCapResidual){
        propToRemove=1
        hfCapResidual=hfCapResidual-max(zOuter$cumSum)
        maxZone=max(zOuter$zone)
      }else{
        # take the first ring where pop outnumber hfCapResidual
        zOuter<-zOuter[zOuter$cumSum>=hfCapResidual,][1,]
        zMaxOuter=zOuter$zone
        propToRemove<-hfCapResidual/zOuter$cumSum
        hfCapResidual=0
        maxZone=zMaxOuter
      }
    }

    # handle length == 0, for special case :
    # e.g. when no pop available in cell or in travel time extent
    if(length(zMaxInner)==0)zMaxInner=NA
    if(length(zMaxOuter)==0)zMaxOuter=NA
    if(length(hfCapResidual)==0)hfCapResidual=NA
    if(length(totalPop)==0)totalPop=0
    if(length(firstCellPop)==0)firstCellPop=0
    # Output capacity table
    catDf=data.frame(
      as.integer(i), # id of hf / group of hf
      hfCap, # capacity from hf table
      hfCapResidual, # capacity not filled
      totalPop, # total population within max distance
      firstCellPop, # population under start cell
      zMaxInner, # maximum travel time for the inner ring. below this, we have capted all patient
      zMaxOuter # maximum travel time for outer ring. below this, we have capted a fraction of patient
      )
    names(catDf)<-c(groupFieldNew,capField,'amCapacityResidual','amPopWithinMaxDist','amPopInFirstCell','amInnerRingTimeLimit','amOuterRingTimeLimit')
    if(nrow(catDf)==0)browser()
    tblOut<-rbind(tblOut,catDf)
    progValue<-inc*incN+10
    amDebugMsg('Progress=',progValue,'inc=',inc,'incN=',incN)
    amUpdateProgressBar(session,"cumulative-progress",round(inc*incN)+10)
    browser()
  }
  if(vectCatch){
  # get catchment shapefile back and clean columns
  execGRASS('v.in.ogr',input=tmpVectCatchOut,output=outputHfCatchment,flags='overwrite',snap=1,columns='cat')
  execGRASS('v.db.dropcolumn',map=outputHfCatchment,columns=c('label','cat_'))
  # remove temp files
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')
  }

  if(!removeCapted)rmRastIfExists(outputPopResidual)
  return(tblOut)

}


