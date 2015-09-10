#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Every modules that implements accessibility are processed here.
# 



observe({
  # Avoid registering reactive function before the first time the module is shown.
  amModEnabled<-listen$tabControl_module_selector
  if(isTRUE(!is.null(amModEnabled) && amModEnabled)){
    #
    # Populate or update selectInput
    #
    observe({
      amUpdateSelectChoice(
        idData=c('amLcvM'),
        idSelect="mergedSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c('amHf'),
        idSelect=c("hfSelect","hfSelectTo"),
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c('amModTable'),
        idSelect="modelSelect",
        dataList=dataList
        )
    })
    observe({
      idData <- "amPop"
      if(input$moduleSelector=="module_6") idData <-c(idData,"amPopRes")
      amUpdateSelectChoice(
        idData=idData,
        idSelect=c('popSelect'),
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c('amCumCost'),
        idSelect="cumulativeCostMapSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c('amZone'),
        idSelect="zoneSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c('amScalCapTable'),
        idSelect="capTableSelect",
        dataList=dataList
        )
    })
  observe({
      amUpdateSelectChoice(
        idData=c('amScalExclTable'),
        idSelect="exclusionTableSelect",
        dataList=dataList
        )
    })
  observe({
      amUpdateSelectChoice(
        idData=c('amScalSuitTable'),
        idSelect="suitabilityTableSelect",
        dataList=dataList
        )
    })
    #
    #  Scaling up suitability factor layer 
    #
    observe({ 
      switch(input$selFactor,
        "popsum"=amUpdateSelectChoice(
          idData=c('amPop','amPopRes'),
          idSelect="selFactorLayer",
          dataList=dataList
          ),   
         "dist"=amUpdateSelectChoice(
          idData=c('amRoad','amScalProxi','amBar','amHf'),
          idSelect='selFactorLayer',
          addChoices=config$newFacilitiesShort,
          dataList=dataList
          ),
        "traveltime"=amUpdateSelectChoice(
          idData=c('amRoad','amScalProxi','amBar'),
          idSelect='selFactorLayer',
          addChoices=config$newFacilitiesShort,
          dataList=dataList
          ),
        "priority"=amUpdateSelectChoice(
          idData=c('amScalPriority'),
          idSelect='selFactorLayer',
          dataList=dataList)
        )
    })

    #
    #  set layer avilable for exclusion 
    #

    observe({
      amUpdateSelectChoice(
        idData=c('amScalExcluR','amScalExcluV'),
        addChoices=config$newFacilitiesShort,
        idSelect='selExclusion',
        dataList=dataList
        ) 
    })

    #
    #  Capacity table  
    # 
    #extract capacity table and render in handson table
    observe({
      amErrorAction(title='Set new capacity table',{
        capNewTable<-amNameCheck(dataList,input$capTableSelect,'table',dbCon=grassSession$dbCon) 
        selProject <- listen$selProject
        isolate({
          if(is.null(capNewTable)||nchar(capNewTable)==0){
            tbl=data.frame(min=as.numeric(NA),max=as.numeric(NA),label=as.character(NA),capacity=as.numeric(NA))
          }else{
            tbl <- dbGetQuery(grassSession$dbCon,paste("SELECT * FROM",capNewTable))
            # NOTE: if types are set in config, why did we get wrong type here ? Check in importation.
            tbl <- as.data.frame(lapply(tbl,function(x){if(is.integer(x)){x <- as.numeric(x)};x}))
            tbl
          }
          output$capacityTable<-renderHotable({tbl},readOnly = FALSE, fixed=3, stretch='last') 
        })
})
    })
    # add a row
    observeEvent(input$btnAddRowCapacity,{
      tbl<-hot.to.df(input$capacityTable)
      row=data.frame(min=as.numeric(NA),max=as.numeric(NA),label=as.character(NA),capacity=as.numeric(NA))
      tbl$min<-as.numeric(tbl$min)
      tbl$max<-as.numeric(tbl$max)
      tbl$label<-as.character(tbl$label)
      tbl$capacity<-as.numeric(tbl$capacity)
      tbl<-rbind(tbl,row)
      output$capacityTable<-renderHotable({tbl},readOnly = FALSE, fixed=3, stretch='last') 
    })
    # remove a row
    observeEvent(input$btnRmRowCapacity,{
      tbl<-hot.to.df(input$capacityTable)
      nrTable<-nrow(tbl)
      if(nrTable==1)return()
      tbl$min<-as.numeric(tbl$min)
      tbl$max<-as.numeric(tbl$max)
      tbl$label<-as.character(tbl$label)
      tbl$capacity<-as.numeric(tbl$capacity)

      output$capacityTable<-renderHotable({tbl[1:(nrTable-1),]},readOnly = FALSE, fixed=3, stretch='last') 
    })


    #
    # Zonal stat :fields from zonal vector map
    #

    # get fields summary reactive list
    zoneFields<-reactive({
      zoneSel<-amNameCheck(dataList,input$zoneSelect,'vector')
      # get field summary 
      isolate({
        if(length(zoneSel)>0){
          zoneFieldsSummary<-amGetFieldsSummary(dbCon=grassSession$dbCon,zoneSel,getUniqueVal=F)
        }else{
          zoneFieldsSummary=list()
        }
        return(zoneFieldsSummary)
      })
    })
    # get zone attribute table fields summary (num,char,idx candidate,val unique)
    observe({
      zoneFieldIdx<-zoneFields()$int
      #zoneFieldIdx<-zoneFieldIdx[zoneFieldIdx %in% zoneFields()$int] 
      # NOTE: We have to convert vector of zone to raster to use r.univar. In this case, only integer column are allowed.
      # NOTE: v.rast.stat could be a better choice, but it does not return a table: new prefixed column are created in original vector.
      zoneFieldLabel<-zoneFields()$char
      if(length(zoneFieldIdx)>0 && length(zoneFieldLabel)>0){
        # search for common id and label/name field position using grep
        idPos<-grep('[iI][dD]',zoneFieldIdx)
        labelPos<-grep('[nN][aA][mM][eE]',zoneFieldLabel)
        # set id selection 
        if(length(idPos)>0){
          zoneIdSel=zoneFieldIdx[idPos][1]
        }else{
          zoneIdSel=zoneFieldIdx[1]}
        # set label selection
        if(length(labelPos)>0){
          zoneLabelSel=zoneFieldLabel[labelPos][1]
        }else{
          zoneLabelSel=zoneFieldLabel[1]
        }
      }else{
        zoneFieldIdx=""
        zoneIdSel=""
        zoneFieldLabel=""
        zoneLabelSel=""
      }
      updateSelectInput(session,'zoneId',choices=zoneFieldIdx,selected=zoneIdSel)
      updateSelectInput(session,'zoneLabel',choices=zoneFieldLabel,selected=zoneLabelSel)
    })
    #
    # Hf fields summary (FROM/TO)
    #
    # get hf (from) attribute table fields summary (num,char,idx candidate,val unique)
    hfFields<-reactive({
      selHfFrom<-amNameCheck(dataList,input$hfSelect,'vector')
      # get field summary 
      isolate({
        if(length(selHfFrom)>0){
          hfFrom<-amGetFieldsSummary(dbCon=grassSession$dbCon,selHfFrom)
        }else{
          hfFrom=list()
        }
        return(hfFrom)
      })
    })
    # get hf (to) attribute table fields summary (num,char,idx candidate,val unique)
    hfFieldsTo<-reactive({
      isModReferral<-isTRUE(input$moduleSelector=='module_4')
      selHfTo<-amNameCheck(dataList,input$hfSelectTo, 'vector')
      selHfFrom<-amNameCheck(dataList,input$hfSelect,'vector')
      if(!is.null(selHfTo)){
        if(selHfFrom==selHfTo)return(hfFields())
        # get field summary 
        isolate({
          if(length(selHfTo) &&isModReferral)return(amGetFieldsSummary(dbCon=grassSession$dbCon,selHfTo))
      })}
      list()
    })

    # update hf field selection
    observe({
      hfTo<-isTRUE(input$selHfFromTo=='To' && input$moduleSelector=='module_4')
      if(isTRUE(input$hfDisplayRules)){
        isolate({
          if(hfTo){
            hfVal<-hfFieldsTo()$val
          }else{
            hfVal<-hfFields()$val
          }
          if(!is.null(hfVal)){
            nHfField<-names(hfVal)
          }else{
            nHfField=''
          }
          updateSelectInput(session,'hfFilterField',choices=nHfField,selected=nHfField[1])
        })
      }
    })

    # update operator and values according to hf field
    observe({
      hfField<-input$hfFilterField
      hfTo<-isTRUE(input$selHfFromTo=='To' && input$moduleSelector=='module_4')
      isolate({
        if(hfTo){
          hfVal<-hfFieldsTo()$val
        }else{
          hfVal<-hfFields()$val
        }
        if(is.null(hfField) || isTRUE(nchar(hfField)==0)){hfField='cat'}
        classNum<-is.numeric(hfVal[[hfField]])
        if(classNum){
          oper=list('is'='=','is not'='!=','greater than'='>','lower than'='<')
        }else{
          oper=list('is'='=','is not'='!=')
        }
        if(!is.null(hfVal)){
          hfValSubset<-hfVal[[hfField]]
          updateSelectInput(session,'hfFilterOperator',choices=oper,selected=oper[1])
          updateSelectInput(session,'hfFilterVal',choices=hfValSubset,selected=hfValSubset[1])
        }
      })
    })


    # create hf filter rules 
    observe({
      btnAddHfRule<-input$btnAddHfRule
      isolate({
        amErrorAction(title='Hf table create',{
          if(!is.null(btnAddHfRule) && btnAddHfRule>0){
            oldRules<-hot.to.df(input$hfTableRules)
            if(!is.null(oldRules)){
              oldRules<-na.omit(oldRules)
            }
            newRules<-data.frame(
              id=0,
              enable=TRUE,
              field=input$hfFilterField,
              operator=input$hfFilterOperator,
              value=paste(input$hfFilterVal,collapse='; ')
              )
            if(!is.null(newRules))
              tbl=rbind(oldRules,newRules)
          }else{
            tbl=data.frame(
              id=as.integer(NA),
              enable=as.logical(NA),
              field=as.character(NA),
              operator=as.character(NA),
              value=as.character(NA)
              )
          }
})
      })
      #render in hansdontable
      tbl$id<-1:nrow(tbl)
      output$hfTableRules<-renderHotable({tbl},
        readOnly = TRUE, fixed=2, stretch='last'
        )
    })


    # disable hf table rule when user unselect 'enable'
    observe({
      tbl<-hot.to.df(input$hfTableRules)
      amErrorAction(title='Hf table rules',{
        #if(!is.null(tbl) && isTRUE(nrow(tbl)>0) && isTRUE(any(!tbl$enable))){
        if(!is.null(tbl) && isTRUE(nrow(tbl)>0) &&isTRUE(any(!tbl$enable))){
          tbl<-tbl[tbl$enable==TRUE,]
          tbl$id<-as.integer(tbl$id)
          if(nrow(tbl)==0){
            tbl=data.frame(
              id=as.integer(NA),
              enable=as.logical(NA),
              field=as.character(NA),
              operator=as.character(NA),
              value=as.character(NA)
              )
          }
          output$hfTableRules<-renderHotable({
            tbl
          },
          readOnly = TRUE, fixed=2, stretch='last'
          )
        }
        })
    })

    # update select HF capacity fields
    observe({
      hfFields<-hfFields()$num
      if(length(hfFields)>0){
        hfFields<-hfFields[!hfFields =='cat']
        capField<-grep('[cC]apac',hfFields,value=T)
        if(length(capField)>0){sel=capField[1]}else{sel=hfFields[1]}
      }else{
        hfFields=""
        sel=""
      }
      updateSelectInput(session,'hfCapacityField',choices=hfFields,selected=sel)
    })
   
    # update select order field
    observe({
      hfFields<-hfFields()$num
      if(length(hfFields)>0){
        hfFields<-hfFields[!hfFields =='cat']
        capField<-grep('[cC]apac',hfFields,value=T)
        if(length(capField)>0){sel=capField[1]}else{sel=hfFields[1]}
      }else{
        hfFields=""
        sel=""
      }
      updateSelectInput(session,'hfOrderColumn',choices=hfFields,selected=sel) 
    })

    # update idx fields FROM
    observe({
      hfCapacity<-input$hfCapacityField
      hfFields<-hfFields()$idx
      if(isTRUE(nchar(hfCapacity)>0) && length(hfFields)>0){
        hfFields<-hfFields[!hfFields %in% hfCapacity]
        sel='cat'
      }else{ 
        hfFields=""
        sel=""
      }
      updateSelectInput(session,'hfIdxField',choices=hfFields, selected=sel)
    })

    # update idx fields TO
    observe({
      hfFields<-hfFieldsTo()$idx
      if(length(hfFields)>0){
        sel='cat'
      }else{
        sel=''
        hfFields=""
      }
      updateSelectInput(session,'hfIdxFieldTo',choices=hfFields, selected='cat')
    })


    # update label fields
    observe({
      hfIdx<-input$hfIdxField
      hfCapacity<-input$hfCapacityField
      hfFields<-c(hfFields()$char,hfFields()$num)
      if(isTRUE(nchar(hfIdx)>0 && length(hfFields)>0)){
        hfFields<-hfFields[!hfFields %in% hfIdx]
        hfFields<-hfFields[!hfFields %in% hfCapacity]
        nameField<-grep('[nN]ame',hfFields,value=T)
      }else{ 
        hfFields=""
        nameField=""
      }
      if(length(nameField)>0){sel=nameField[1]}else{sel=hfFields[1]}
      updateSelectInput(session,'hfNameField',choices=hfFields, selected=sel)
    })

    # update label fields to
    observe({
      hfIdx<-input$hfIdxFieldTo
      hfFields<-c(hfFieldsTo()$char,hfFieldsTo()$num)
      if(isTRUE(nchar(hfIdx)>0) && length(hfFields)>0){
        hfFields<-hfFields[!hfFields %in% hfIdx]
        nameField<-grep('[nN]ame',hfFields,value=T)
      }else{ 
        hfFields=""
        nameField=""
      }
      if(length(nameField)>0){sel=nameField[1]}else{sel=hfFields[1]}
      updateSelectInput(session,'hfNameFieldTo',choices=hfFields, selected=sel)
    })


    #
    # Population on barriervalidation
    #


    # popOnBarrier stat
    popOnBarrierStat<-reactive({
     #if(input$moduleSelector=='module_3'){
        pop<-amNameCheck(dataList,input$popSelect,'raster')
        merged<-amNameCheck(dataList,input$mergedSelect,'raster')
        if(!is.null(pop) & !is.null(merged)){
          tmpMapPop<-'tmp__test_pop_on_barrier'
          execGRASS('r.mask',flags='i',raster=merged)
          execGRASS('r.mapcalc',flags='overwrite',
            expression=paste(tmpMapPop," = ",pop,"")
            )
          execGRASS('r.mask',flags='r')
          sumPop<-read.table(text=
            execGRASS('r.univar',map=tmpMapPop,flags=c('g','t'),intern=T),
            sep='|',header=T
            )[c('non_null_cells','sum')]
          origPop<-read.table(text=
            execGRASS('r.univar',map=pop,flags=c('g','t'),intern=T),
            sep='|',header=T
            )[c('sum')]

          return(
            list(
              sum=round(sumPop$sum,2),
              cells=sumPop$non_null_cells,
              percent=round(100*(sumPop$sum/origPop$sum),2)
              )
            )
        }
   #   }
      return(list())
    })




    #
    # Scalling up validation options
    # 


    observe({
      selFactor <- input$selFactorLayer 
      if(isTRUE(!is.null(selFactor) && nchar(selFactor) > 0 )){
        disBtn=FALSE
      }else{
        disBtn=TRUE
      }
       
      amActionButtonToggle(session=session,'btnAddFactor',disable=disBtn)

    })


    # initial exclusion table
    observe({
      amErrorAction(title='Initial exclusion table',{
        selProject <- listen$selProject
      #  reInit <- listen$initExclusionTable
        excluTable<-amNameCheck(dataList,input$exclusionTableSelect,'table',dbCon=grassSession$dbCon)
        btnReset <- input$btnResetExcluTable
        isolate({
          if(is.null(excluTable)||nchar(excluTable)==0){
            tbl=data.frame(select=as.logical(NA),layer=as.character(NA),buffer=as.numeric(NA),method=as.character(NA))
          }else{
            tbl=dbGetQuery(grassSession$dbCon,paste("SELECT * FROM",excluTable))
            tbl$select=TRUE
          }
          output$exclusionTable <- renderHotable({tbl},readOnly = c(2,3,4) , fixed=1, stretch='last') 
        })
})
    })



    observeEvent(input$btnAddExclusion,{
      amErrorAction(title="Button add exclusion",{ 

        tbl<- na.omit(hot.to.df(input$exclusionTable))
        layer <- input$selExclusion
        buffer <-input$exclusionBuffer
        method <- input$exclusionMethod

        tbl <- rbind(tbl,data.frame(select=TRUE,layer=layer,buffer=buffer,method=method))
        output$exclusionTable <- renderHotable({tbl},readOnly = c(2,3,4) , fixed=1, stretch='last') 

})

    })


    observeEvent(input$btnRmExcluUnselected,{
      amErrorAction(title="Button remove unselected exclusion row",{ 
        tbl<- na.omit(hot.to.df(input$exclusionTable))
        if(!isTRUE(nrow(tbl)>0 && length(tbl$select)>0))return()

        tbl <- tbl[tbl$select,]
        if(nrow(tbl)<1){
            tbl=data.frame(select=as.logical(NA),layer=as.character(NA),buffer=as.numeric(NA),method=as.character(NA))
         # listen$initExclusionTable <- runif(1)
        }

        output$exclusionTable <- renderHotable({tbl},readOnly = c(2,3,4) , fixed=1, stretch='last') 
})

    })






    # initial suitability table

  # get table info from db
    observe({
      amErrorAction(title='Initial suitability table',{
        selProject <- listen$selProject
        #reInit <- listen$initSuitTable
        suitTable<-amNameCheck(dataList,input$suitabilityTableSelect,'table',dbCon=grassSession$dbCon)
        btnReset <- input$btnResetSuitTable 
        isolate({
          if(is.null(suitTable)||nchar(suitTable)==0){
            tbl=data.frame(select=as.logical(NA),factor=as.character(NA),layer=as.character(NA),weight=as.numeric(NA),options=as.character(NA))
          }else{
            tbl=dbGetQuery(grassSession$dbCon,paste("SELECT * FROM",suitTable))
            tbl$select=TRUE
          }
          output$suitabilityTable <- renderHotable({tbl},readOnly = c(2,3,4,5) , fixed=1, stretch='last') 
        })
})
    })

    observeEvent(input$btnAddFactor,{
      amErrorAction(title="Button add factor",{ 
        # init variables
        sep <- ";"
        opt <- character(0)
        # import input
        tbl <- na.omit(hot.to.df(input$suitabilityTable))
        layer <- input$selFactorLayer
        fact <- input$selFactor 
        weight <- input$factorWeight
        # set options for population sum and traveltime
        switch(fact,
          "popsum"={
            opt <- paste0('r=',input$factorPopSumRadius)
          },
          "traveltime"={
            type <- input$factorTypeAnalysis
            if(type=="aniso") opt <- paste0('d=', input$factorTravelDirection)
            opt <- paste(c(opt,paste0('t=',input$factorTypeAnalysis)),collapse=sep )
          }
          )
        # set options
        opt <-paste(c(opt,paste0('p=',input$factorDirection)),collapse=sep)
        # add factor to existing table
        tbl=rbind(tbl,data.frame(select=TRUE,factor=fact,layer=layer,weight=weight,options=opt))
        # render table
        output$suitabilityTable <- renderHotable({tbl},readOnly = c(2,3,4,5), fixed=1, stretch='last') 
})
    })



    observeEvent(input$btnRmSuitTableUnselected,{
      amErrorAction(title="Button remove selecte suit table row",{ 
        tbl<- na.omit(hot.to.df(input$suitabilityTable))
        if(!isTRUE(nrow(tbl)>0 && length(tbl$select)>0))return()
        tbl <- tbl[tbl$select,]
        if(nrow(tbl)<1){
            tbl=data.frame(select=as.logical(NA),factor=as.character(NA),layer=as.character(NA),weight=as.numeric(NA),options=as.character(NA))
        }
        output$suitabilityTable <- renderHotable({tbl},readOnly = c(2,3,4,5), fixed=1, stretch='last') 
})

    })



    #
    # General validation and error message
    #

    # preventive field validation
    # TODO: this validation step was written for one module:
    # With almost all modules depending on it, this should be rewritten.
    observe({
      amErrorAction(title='Module 2,3,4,6: validation',{
timeCheck<-system.time({
        #
        # init messages
        #

        err = character(0)
        info = character(0)
        out  = character(0)
        msgList = character(0)

        #
        # store current module
        #

        module2    <- isTRUE(input$moduleSelector == 'module_2')
        module3    <- isTRUE(input$moduleSelector == 'module_3')
        module4    <- isTRUE(input$moduleSelector == 'module_4')
        module5    <- isTRUE(input$moduleSelector == 'module_5')
        module6    <- isTRUE(input$moduleSelector == 'module_6')

        #
        # Clean tags
        #

        tagsClean  <- amGetUniqueTags(input$costTag) 
      
        #
        # Control maps and values
        #

        # general input validation
        merged     <- isTRUE(!is.null(amNameCheck(dataList,input$mergedSelect,'raster')))
        hf         <- isTRUE(!is.null(amNameCheck(dataList,input$hfSelect,'vector')))
        pop        <- isTRUE(!is.null(amNameCheck(dataList,input$popSelect,'raster')))
        #popRes     <- isTRUE(!is.null(amNameCheck(dataList,input$popResSelect,'raster')))


        # table validation
        hfOnBarrier<-isTRUE(
          any(tblHfSubset()$amOnBarrier=='yes') ||
          any(tblHfSubsetTo()$amOnBarrier=='yes') 
          )
        # check if there is at least one facility selectected.
        hfNoSelected            <- isTRUE(!any(tblHfSubset()$amSelect))
        hfNoSelectedTo          <- isTRUE(!any(tblHfSubsetTo()$amSelect))
        # check for speed of  0 kmh
        tblModel          <- isTRUE(!any(hot.to.df(input$speedRasterTable)$speed <1))
        # parameter validation
        unlimitedTT       <- isTRUE(input$maxTravelTime == 0)
        # population on barrier
        popBarrierSum     <- popOnBarrierStat()$sum
        popBarrierCells   <- popOnBarrierStat()$cells
        popBarrierPercent <- popOnBarrierStat()$percent

        #
        # Parameters control.
        #

        if(module3){
          # simple character control (user cannot put custom value)
          hfIdx           <- isTRUE(nchar(input$hfIdxField)>0)
          capField        <- isTRUE(nchar(input$hfCapacityField)>0)
          hfBuffer        <- isTRUE(input$hfOrder == 'circBuffer')
          popBuffer       <- isTRUE(input$popBufferRadius > listen$mapMeta$grid$`North`)
          popBarrierFound <- isTRUE(popBarrierSum>0)
          zonalPop        <- isTRUE('zonalPop' %in% input$mod3param)

          if(zonalPop){
            zonalSelect <- isTRUE(!is.null(amNameCheck(dataList,input$zoneSelect,'vector')))
            zoneId      <- isTRUE(length(input$zoneId)>0)
            zoneLabel   <- isTRUE(length(input$zoneLabel)>0)
          }

          hfOrderInconsistency       <- isTRUE(input$hfOrder!='tableOrder' && !'rmPop' %in% input$mod3param)
          zonalCoverageInconsistency <- isTRUE(zonalPop && !'rmPop' %in% input$mod3param)
          # data overwrite warning module 3 : validate each output !
          # TODO: inform user of all provided output. Warning if risk of overwrite.
        }

        if(module6){
          capNewTable <- hot.to.df(input$capacityTable)
          suitTable <- hot.to.df(input$suitabilityTable)
          withoutFacility <- isTRUE(input$initialFacilityLayer == "empty")
          #excluTable <- hot.to.df(input$exclusionTable)

          tblCapTypeOk              <- TRUE
          tblCapMissingOk           <- TRUE
          tblCapOverlapOK           <- TRUE
          tblCapInRangeOk           <- TRUE
          tblCapGreaterThanPrevOk   <- TRUE
          tblCapWithoutButHfSelect  <- FALSE
          tblCapMinMaxOk            <- TRUE
          tblCapLabelOk             <- TRUE
          tblSuitOk                 <- TRUE
          popSelect                 <- TRUE


          if(withoutFacility) {
            if(!hfNoSelected && hf){
              tblCapWithoutButHfSelect <- TRUE 
            }
            # manually validate hf layer and hf on barrier.
            hfNoSelected <- FALSE
            hfOnBarrier <- FALSE
            hf <- TRUE
          }

          if(!is.null(suitTable)){
          tblSuitOk <- nrow(na.omit(suitTable))>0 
          }
          #  validate null
          if(!is.null(capNewTable)){
            #  validate missing value
            tblCapMissingOk <-isTRUE(all(
                sapply(capNewTable,function(x){a=all(str_length(x)>0)})
                ))
            # validate type
            if(tblCapMissingOk)(
              tblCapTypeOk <- all(
                is.numeric(capNewTable$min),
                is.numeric(capNewTable$max),
                is.numeric(capNewTable$capacity), 
                is.character(capNewTable$label)
                )
              )
            # validate overlap min max and capacity in range.
            if(tblCapMissingOk){
              # max greater than min
              tblCapMinMaxOk<-all(capNewTable$min<capNewTable$max)

              # checking previous row values
              nR<-nrow(capNewTable)
              if(nR>1){
                for(i in 2:nR){
                  # Capacity is greater than previous capacity 
                  tblCapGreaterThanPrevOk <- all(tblCapGreaterThanPrevOk,isTRUE(capNewTable[i,'capacity']>capNewTable[i-1,'capacity'])) 
                  # min max+1 overlap
                  tblCapOverlapOK<-all(tblCapOverlapOK,isTRUE(capNewTable[i,'min'] > capNewTable[i-1,'max'])) 
                }
              }
              # capacity in min max range
              tblCapInRangeOk <- isTRUE(
                all(capNewTable$capacity <= capNewTable$max & capNewTable$capacity >= capNewTable$min)
                )
              # unique labels
              tblCapLabelOk<-isTRUE(length(unique(capNewTable$label))==length(capNewTable$label))

            }
          }
        }


        #
        # Collect messages in err and info 
        #



        if(!hf) err = c(err,'Health facilities map missing.') 
        if(hfOnBarrier) err = c(err, 'There are facilities located on barrier, unselect them to proceed.')
        if(!merged) err = c(err,'Merged land cover missing.')
        if(unlimitedTT) info = c(info,'Unlimited travel time')
        #if(hf)if(!tblHf) err = c(err,'at least one facilities must be selected') ## too slow
        if(merged)if(!tblModel) err = c(err,'Please correct the final scenario table (0 km/h is not allowed for travel speed).')

        if(module2 | module6){
          if(hfNoSelected) err = c(err, 'Please select at least one facility.')
        }
        if(module3 | module6){ 
          if(!pop) err = c(err,'Population map missing.')
        }
        if(module3){
          if(!hfIdx) err = c(err,'No group/id field set for hf.')
          if(hfNoSelected) err = c(err, 'Select at least one facility.')
          if(!capField) err = c(err,'No capacity field set for hf.')
          if(hfBuffer)if(!popBuffer) err = c(err,'Circular buffer must be higher to project resolution.')
          #if(!popBarrier) info = c(info,'Map of population on barrier will NOT be computed.')
          if(popBarrierFound) info = c(info,paste('Population encoutered on barrier in',popBarrierCells,' cells for a total of ',popBarrierSum,'individuals. (',popBarrierPercent,'% of total population)'))
          if(hfOrderInconsistency) info=c(info,"If covered population is not removed at each iteration, facilities processing order should be set to 'Order from health facilities table.'")
          if(zonalPop){
            if(!zonalSelect) err=c(err,'Zonal map missing.')
            if(!zoneId) err =c(err,'Zonal id column missing.')
            if(!zoneLabel) err =c(err,'Zonal label column missing.')

          }
          if(zonalCoverageInconsistency) err = c(err,'If covered population is not removed at each iteration, zonal analysis could not be performed.')
        }
        if(module4){
          if(hfNoSelected) err = c(err, "Select at least one facility in table 'FROM'.")
          if(hfNoSelectedTo) err = c(err,"Select at least one facility in table 'TO'. ")
        }
        if(module6){
          #if(hfNoSelected && !pop) err = c(err,'Scaling up : if no facility is selected, you must choose a population map.')
          #if(!hfNoSelected && popRes) err = c(err,'Scaling up : if .')
          if(!tblSuitOk) err = c(err, "Table of suitability factors : missing value")
          if(!tblCapMissingOk) err = c(err,'Table of scaling up capacity: missing value')
          if(!tblCapTypeOk) err = c(err,'Table of scaling up capacity: type error.')
          if(!tblCapMinMaxOk) err =c(err,"Table of scaling up capacity:  min greater than or equal to max.")
          if(!tblCapGreaterThanPrevOk) err = c(err,"Table of scaling up capacity: capacity is not incremental")
          if(!tblCapInRangeOk) info =c(info,"Table of scaling up capacity: there is capacity value(s) not in range [min,max].")
          if(!tblCapOverlapOK) err =c(err,"Table of scaling up capacity: min value can't be equal or less than previous max value.")
          if(tblCapWithoutButHfSelect) err = c(err, "Scaling up : initial new facility layer paramater is set to 'empty' : please unselect facilities.")

          if(!tblCapLabelOk) err =c(err,"Table scaling up capacity: duplicate labels.")
          #if(hfNoSelected) err = c(err, "Select at least one facility.") 
        }

  # output name text. 
        if(!isTRUE(length(tagsClean)>0)){
          err <- c(err,'Please enter at least one tag.')
        }

        #
        # create HTML for validation message list.
        #

        if(length(err)>0){
          err <- HTML(paste("<div>",icon('exclamation-triangle'),err,'</div>',collapse=""))
          disBtn <- TRUE
        }else{
          disBtn <- FALSE
        }

        if(length(info)>0) {
          info <- HTML(paste("<div>",icon('info-circle'),info,'</div>',collapse=""))
        }

        # send result to ui
        if(length(err)>0 || length(info)>0){
          msgList <- tagList(tags$b('Validation issues:'),err,info)
        }else{
          msgList <- ""# tagList(tags$b('Ready to compute.'))
        }

        #
        # If no errors, naming datasets that will be produced. 
        # 
        
        if(length(err)==0){

          # naming output (use local scoping)
          addTag <- function(base,tag=tagsClean,sepT=config$sepTagFile,sepC=config$sepClass){
            tag <- amGetUniqueTags(tag)
            base <- amClassInfo(base)$class
            paste(c(base,paste(tag,collapse=config$sepTagFile)),collapse=config$sepClass)
          }
          # all
          tableModel               <- addTag(tag=c(tagsClean,'processed'),'amModTable')     # m2, m3, m4, m6
          mapSpeed                 <- addTag('amSpeed')          # m2, m3, m4, m6
          mapFriction              <- addTag('amFric')           # m2, m3, m4, m6
          # accessibility (map travel time)
          mapCumulative            <- addTag('amCumCost')        # m2
          # geographic coverage
          tableCapacityOut         <- addTag('amCapTable')         # m3
          tableZonalOut            <- addTag('amZoneCovTable')     # m3
          hfCatchment              <- addTag('amHfCatch')        # m3
          mapPopResidual           <- addTag('amPopRes')         # m3
          mapPopOnBarrier          <- addTag('amPopBar')         # m3
          # referral
          tableReferral            <- addTag('amRefTable')         # m4
          tableReferralNearestDist <- addTag('amRefTableDist')     # m4
          tableReferralNearestTime <- addTag('amRefTableTime')     # m4
          # scaling up
          #mapPotentialCoverage     <- addTag('amPotCov')         # m6 NOTE: 
          mapNewHf                 <- addTag('amScalHfNew')      # m6
          tableScalingUp           <- addTag('amScalHfNewTable')   # m6
          tableCapacityNew         <- addTag('amScalCapTable')     # m6




          # existing dataset (use local scoping)
          outTxt <- function(dataName,condition=TRUE){
            if(isTRUE(condition)){
              dataExists <- dataName %in% dataList$df$origName
              dataNameDisplay <- amTagsFileToDisplay(dataName)
              #y <- paste(amGetClass(x,config$sepClass),'[',paste(tagsClean,collapse=" "),']')
              if(dataExists){
                return(sprintf("<b style=\"color:#FF9900\"> (overwrite)</b> %s",dataNameDisplay))
              }else{
                return(sprintf("<b style=\"color:#00CC00\">(ok)</b> %s",dataNameDisplay))
              }
            }else{
              NULL
            }
          }

          # set data output name string
          if(module2 | module3 | module4){
            out <- c(
              out, 
              outTxt(tableModel),
              outTxt(mapSpeed),
              outTxt(mapFriction)
              )
          }
          if(module2){
            out <- c(
              out,
              outTxt(mapCumulative)
              )
          }
          if(module3){
            out <- c(
              out, 
              outTxt(tableCapacityOut),
              outTxt(tableZonalOut,'zonalPop' %in% input$mod3param),
              outTxt(hfCatchment, 'vectCatch' %in% input$mod3param),
              outTxt(mapPopOnBarrier),
              outTxt(mapPopResidual, 'rmPop' %in% input$mod3param)
              )
          }
          if(module4){
            out <- c(
              out,
              outTxt(tableReferral),
              outTxt(tableReferralNearestDist),
              outTxt(tableReferralNearestTime)
              )
          } 
        }

        #
        # Set final message 
        #

        if(length(out)>0) {
          msgList <- tagList(
            msgList,
            tags$b('Output dataset:'), 
            HTML(paste("<div>",icon('sign-out'),out,"<div/>",collapse=""))
            )
        }


        amActionButtonToggle(session=session,'btnComputeAccessibility',disable=disBtn)
        output$msgModule3 <-renderUI({msgList})
        

})
            })
    })


    # extract category from merged landcover raster and add new column.
    speedRasterTable<-reactive({
      sel<-amNameCheck(dataList,input$mergedSelect,'raster')
      isolate({
        if(length(sel)>0){
          lcvMergedCat<-execGRASS('r.category', map=sel,intern=T)
          if(length(lcvMergedCat)>0){
            tbl<-read.csv(
              text=lcvMergedCat,
              sep='\t',
              header=F,
              stringsAsFactors=F
              )
            names(tbl)<-c('class','label')
            noLabel<-is.na(tbl$label) | is.null(tbl$label)
            tbl[noLabel,'label']<-paste0('no_label_',as.character(tbl[noLabel,'class']))
            #tbl[,'speed']<-as.integer(0)
            tbl[,'speed']<- 0
            tbl[,'mode']<-as.character('MOTORIZED')
            return(tbl)
          }else{
            amMsg(session,type='warning',title='speedRasterTableReactive',text=paste('no category found in',sel))
          }
        }
        tbl<-data.frame(as.integer(NA),as.character(NA),as.integer(NA),as.character(NA)) 
        names(tbl)<-config$tableColNames[['table_scenario']] 
        return(tbl)
      })
    })

    # display handson table of speed table from raster.
    observe({
      amErrorAction(title='Observe speed raster table',{
      tbl<-speedRasterTable()
      undo<-input$speedTableUndo
      if(isTRUE(nrow(tbl)>0) || (isTRUE(!is.null(undo)) && isTRUE(undo)>0)){
        # create raster table with orignal value
        output$speedRasterTable <- renderHotable({tbl}, readOnly = FALSE, fixed=2, stretch='last')
        # update selector lcv class to exclude 
        updateSelectInput(session,'excludeLandCoverClass',choices=tbl$class,selected="")
      }
      })
    })



    # render handson table from sqlite lcv table
    observe({
      # reactive table for speed / module value. Empty if none.
      sel<-amNameCheck(dataList,input$modelSelect,'table',dbCon=isolate(grassSession$dbCon))
      isolate({
        if(!is.null(sel)){
          tbl<-dbGetQuery(grassSession$dbCon,paste('select * from',sel))
          tbl$class <-as.integer(tbl$class)
        }else{
          tbl<-data.frame(class=as.integer(NA),label=as.character(NA),speed=as.integer(NA),mode=as.character(NA))
        }
        output$speedSqliteTable<-renderHotable({
          tbl
        },readOnly=TRUE,fixed=2,stretch='last')
      })
    })




    # create facilitie table with additional aaccesdmod column
    tblHfOrig<-reactive({
      selHf<-amNameCheck(dataList,input$hfSelect,'vector')
      selMerged<-amNameCheck(dataList,input$mergedSelect,'raster')
      selPop<-amNameCheck(dataList,input$popSelect,'raster')
      isolate({
        amCreateHfTable(
          mapHf=selHf,
          mapMerged=selMerged,
          mapPop=selPop,
          dbCon=grassSession$dbCon
          )
      })
    })

    #create facilitie table for second table. 
    tblHfOrigTo<-reactive({
      selHf<-amNameCheck(dataList,input$hfSelect,'vector')
      selHfTo<-amNameCheck(dataList,input$hfSelectTo,'vector')
      selMerged<-amNameCheck(dataList,input$mergedSelect,'raster')
      selPop<-amNameCheck(dataList,input$popSelect,'raster')
      if(input$moduleSelector=='module_4'){
        if(selHf==selHfTo && isTRUE(nrow(tblHfOrig())>0))return(tblHfOrig())
        isolate({
          amCreateHfTable(
            mapHf=selHfTo,
            mapMerged=selMerged,
            mapPop=selPop,
            dbCon=grassSession$dbCon
            )
        })
      }else{
        NULL
      }
    })

    # render facilities table.
    observe({
      tbl<-tblHfOrig()
      if(!is.null(tbl)){
        tbl$amSelect<-FALSE 
        # renderHotable convert logical to HTML checkbox and checkbox are always writable. 
        # To avoid write on this logical vector, use plain text :
        tbl$amOnBarrier<-ifelse(tbl$amOnBarrier==TRUE,'yes','no')
        # choose which columns display first.
        colOrder<-unique(c('cat','amSelect','amOnBarrier',names(tbl))) 
        tbl<-tbl[order(tbl$amOnBarrier,decreasing=T),colOrder] 
      }else{
        # display at least a data frame with named column.
        tbl<-data.frame(cat=as.integer(NA),amSelect=as.integer(NA),amOnBarrier=as.integer(NA))
      }
      output$hfTable<-renderHotable({
        tbl
      },readOnly=TRUE,fixed=5,stretch='last')
    })

    # render facilities table to.
    observe({
      amErrorAction(title='tblHfOrigTo to hot',{
        tbl<-tblHfOrigTo()
        if(!is.null(tbl)){
          tbl$amSelect<-FALSE 
          # renderHotable convert logical to HTML checkbox and checkbox are always writable. 
          # To avoid write on this logical vector, use plain text :
          tbl$amOnBarrier<-ifelse(tbl$amOnBarrier==TRUE,'yes','no')
          # choose which columns display first.
          colOrder<-unique(c('cat','amSelect','amOnBarrier',names(tbl))) 
          tbl<-tbl[order(tbl$amOnBarrier,decreasing=T),colOrder] 
        }else{
          # display at least a data frame with named column.
          tbl<-data.frame(cat=as.integer(NA),amSelect=as.integer(NA),amOnBarrier=as.integer(NA))
        }
        output$hfTableTo<-renderHotable({
          tbl
        },readOnly=TRUE,fixed=5,stretch='last')
      })
    })


    # hf subset (from) used in other functions
    tblHfSubset<-reactive({
      tbl<-hot.to.df(input$hfTable)
      if(!is.null(tbl)){
        tbl$cat<-as.integer(tbl$cat)
        tbl[tbl$amSelect==TRUE,]
      }
    })
    # hf subset (to) used in other functions
    tblHfSubsetTo<-reactive({
      tbl<-hot.to.df(input$hfTableTo)
      if(!is.null(tbl)){
        tbl$cat<-as.integer(tbl$cat)
        tbl[tbl$amSelect==TRUE,]
      }
    })

    # buttons select hf with rules
    observe({ 
      btnHfRule<-input$btnSelectHfFromRule
      if(!is.null(btnHfRule) && btnHfRule>0){
        isolate({
          tblRule<-hot.to.df(input$hfTableRules)
          selHfTo<-input$selHfFromTo=='To'
          isModReferral<-input$moduleSelector=='module_4'
          tblHf<-hot.to.df(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
          if(!is.null(tblRule)&&!is.null(tblHf)){
            tblRule<-na.omit(tblRule)
            tblRule<-tblRule[tblRule$enable==TRUE,]
            if(nrow(tblRule)>0){
              tblHf$amSelect=FALSE
              for(i in 1:nrow(tblRule)){
                fi=tblRule[i,'field']
                op=tblRule[i,'operator']
                va=unlist(strsplit(tblRule[i,'value'],';\\s'))
                if(fi %in% names(tblHf)){
                  if(is.numeric(tblHf[,fi]))va<-as.numeric(va)
                  switch(op,
                    '='={
                      tblHf$amSelect<- tblHf[,fi] %in% va | sapply(tblHf$amSelect,isTRUE)
                    },
                    '!='={
                      tblHf$amSelect<- !tblHf[,fi] %in% va | sapply(tblHf$amSelect,isTRUE)
                    },
                    '<'={
                      tblHf$amSelect<-tblHf[,fi] < min(va) | sapply(tblHf$amSelect,isTRUE)
                    },
                    '>'={
                      tblHf$amSelect<-tblHf[,fi] > max(va) | sapply(tblHf$amSelect,isTRUE)
                    }
                    )
                }
              }
              output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
                tblHf$cat<-as.integer(tblHf$cat)
                tblHf
              },readOnly=TRUE,fixed=5,stretch='last')
            }
          }
        })
      }
    })


    # unselect HF (to/from)
    observe({
      btnNoHf<-input$btnSelecteNoHf
      if(!is.null(btnNoHf) && btnNoHf>0){
        isolate({
          selHfTo<-input$selHfFromTo=='To'
          isModReferral<-input$moduleSelector=='module_4'
          tbl<-hot.to.df(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
          tbl$amSelect=FALSE
          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
            tbl$cat<-as.integer(tbl$cat)
            tbl
          },readOnly=TRUE,fixed=5,stretch='last')
        })
      }
    })
    # select all Hf (to/from)
    observe({
      btnAllHf<-input$btnSelectAllHf
      if(!is.null(btnAllHf) && btnAllHf>0){
        isolate({
          selHfTo<-input$selHfFromTo=='To'
          isModReferral<-input$moduleSelector=='module_4'
          tbl<-hot.to.df(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
          tbl$amSelect=TRUE
          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
            tbl$cat<-as.integer(tbl$cat)
            tbl
          },readOnly=TRUE,fixed=5,stretch='last')
        })
      }
    })


    # Select random Hf
    observe({
      btnRandomHf<-input$btnSelectRandomHf
      if(!is.null(btnRandomHf) && btnRandomHf>0){
        isolate({
          selHfTo<-input$selHfFromTo=='To'
          isModReferral<-input$moduleSelector=='module_4'
          tbl<-hot.to.df(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
          nR<-nrow(tbl)
          sR=floor(nR/10)
          dR<-nR-sR
          sel<-sample(c(rep(TRUE,sR),rep(FALSE,dR))) 
          tbl$amSelect=sel
          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
            tbl$cat<-as.integer(tbl$cat)
            tbl
          },readOnly=TRUE,fixed=5,stretch='last')
        })
      }
    })



    # speed table merge button enabling

    observe({
      amErrorAction(title="Autocomplete scenario table validation",{
        selP <- listen$selProject
        tblOrig <-hot.to.df(input$speedRasterTable)
        tblExt <-hot.to.df(input$speedSqliteTable)

        if(TRUE){
          noDataCheck <- any(sapply(unlist(tblExt),amNoDataCheck))
          
          validMode <-  isTRUE(all(
                tolower(tblExt$mode) %in% tolower(names(config$listTranspMod))
                ))
            labelMatch <- isTRUE(all(tblExt$label %in% tblOrig$label))
            classMatch <- isTRUE(all(as.integer(tblExt$class) %in% as.integer(tblOrig$class)))

            # validation message

            err=character(0)
            info=character(0)
            disableBtn <- TRUE
            warningButton <- TRUE

            if(noDataCheck) err <- c(err,"Empty field found")
            if(!noDataCheck){
              if(!validMode) info <- c(info,paste("Some modes of transportation do not match currently allowed ones:",paste(names(config$listTranspMod),collapse=','),". Unknown mode(s) will be changed to default value."))
              if(!labelMatch) info <- c(info, "Some labels do not match those stored in the travel scenario to be processed and will overwrite them.")
              if(!classMatch) info <- c(info, "Some classes do not match those stored in merged land cover and will not be imported.")
            }
            if(length(info)>0) {
              info <- HTML(paste("<div>",icon('info-circle'),info,'</div>',collapse=""))
            }

            if(length(err)>0){
              disBtn <- TRUE
            }else{
               disBtn <- FALSE
            }

            # send result to ui
            if(length(err)>0 || length(info)>0){
              msgList <- tagList(tags$b('Information:'),err,info)

            }else{
              msgList <- ""# tagList(tags$b('Ready to compute.'))
            }

          amActionLinkToggle(session=session,'speedTableMerge',disable=disBtn)
          output$speedTableMergeValidation <- renderUI(msgList)
        }
        })
        })




    # table merge process.
    observeEvent(input$speedTableMerge,{
      amErrorAction(title='Autocomplete scenario table',{
        tblOrig<-hot.to.df(input$speedRasterTable)
        tblExt<-hot.to.df(input$speedSqliteTable)
        if(length(tblOrig)>0 &&length(tblExt)>0){ 
          classOrig<-as.integer(tblOrig[,'class'])
          tblExt$class<-as.integer(tblExt$class)
          tblMergeOk <- tblExt[tblExt$class %in% classOrig,]
          tblMergeNo <- tblOrig[!classOrig %in% tblExt$class,]
          tblMerge<-rbind(tblMergeOk,tblMergeNo)
          tblMerge <- tblMerge[order(tblMerge$class,decreasing=F),]
          output$speedRasterTable<- renderHotable({tblMerge}, readOnly = 1, fixed=2, stretch='last')
        }
        })
    })

    #validate if table is updated
    observe({
      tblUpdated<-na.omit(hot.to.df(input$speedRasterTable))
      isolate({
        if(!is.null(tblUpdated)){
          tblOriginal<-speedRasterTable()
          testNrow<-nrow(tblUpdated)==nrow(tblOriginal)
          #testValidClass<-!any(tblOriginal==character(1))||!any(tblUpdated==character(1))
          testValidClass<-!anyNA(tblOriginal)||!anyNA(tblUpdated)
          if(!is.null(tblOriginal) && isTRUE(testNrow) &&isTRUE(testValidClass)){
            # rule 1: do not allow changing class and label
            #tblValidated<-data.frame(c(tblOriginal[,c('class','label')],tblUpdated[,c('speed','mode')]))
            # rule 1, keep class. NOTE: with modified version of handson table (read only vector) no need for this
            tblValidated<-data.frame(class=tblOriginal[,c('class')],tblUpdated[,c('label','speed','mode')])
            # rule 2: if Speed is not integer, set to 0
            #s<-as.integer(tblUpdated$speed)
            s<-as.numeric(tblUpdated$speed)
            #s[is.na(s)]<-as.integer(0)
            s[is.na(s)]<- 0
            # rule 3: if mode is not in allowedModTransp choices, set to NONE
            m<-toupper(tblUpdated$mode)
            mTest<- m %in% names(config$listTranspMod)
            m[!mTest]<-'MOTORIZED'
            # update with validated values
            tblValidated$mode<-m
            tblValidated$speed<-s
          }else{
            tblValidated=tblOriginal
          }
          output$speedRasterTable<- renderHotable({tblValidated}, readOnly = 1, fixed=2, stretch='last')
        }
      })
    })


    # disable button 'createTimeCostMap'  each time it's activated
    observe({
      btn<-input$btnComputeAccessibility
      if(!is.null(btn)&&btn>0){
        amActionButtonToggle(session=session,'btnComputeAccessibility',disable=TRUE)
      }
    })


    # main function 
    observe({
      timeCheck<-system.time({
        btn<-input$btnComputeAccessibility # only reactive dependencie on create travel time button.
        if(!is.null(btn) && btn>0){
          isolate({
            # tags
            costTag            <- amSubPunct(input$costTag,config$sepTagFile,rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T)

            # input table
            tbl                <- hot.to.df(input$speedRasterTable)
            tblHfSubset        <- tblHfSubset()
            
          
            if(input$moduleSelector=='module_4'){ 
              tblHfSubsetTo    <- tblHfSubsetTo()

            }


            # input maps
            mapMerged          <- amNameCheck(dataList,input$mergedSelect,'raster')
            mapHf              <- amNameCheck(dataList,input$hfSelect,'vector')
            mapHfTo            <- amNameCheck(dataList,input$hfSelectTo,'vector')
            mapPop             <- amNameCheck(dataList,input$popSelect,'raster')
            mapZoneAdmin       <- amNameCheck(dataList,input$zoneSelect,'vector')
            mapCumulativeCost  <- amNameCheck(dataList,input$cumulativeCostMapSelect,'raster')

            # catch. path
            catchPath          <- grassSession$pathShapes

            # field selection
            hfIdx              <- input$hfIdxField
            hfLab              <- input$hfNameFieldTo
            hfIdxTo            <- input$hfIdxFieldTo
            hfLabTo            <- input$hfNameFieldTo
            zoneFieldLabel     <- input$zoneLabel
            zoneFieldId        <- input$zoneId
            capField           <- input$hfCapacityField
            orderField         <- input$hfOrderColumn

            # parameters
            maxTravelTime      <- input$maxTravelTime*60
            maxTravelTimeOrder <- input$maxTravelTimeProcOrder*60
            dirAnalysis        <- input$dirAnalysis
            typeAnalysis       <- input$typeAnalysis
            selectedAnalysis   <- input$moduleSelector
            hfOrder            <- input$hfOrder
            hfOrderSorting     <- input$hfOrderSorting
            popBuffer          <- input$popBufferRadius
            modParam           <- input$mod3param

 
            # scaling up only additional tables
            if(input$moduleSelector == 'module_6'){
              tblCapacity        <- hot.to.df(input$capacityTable)
              tblExclusion       <- hot.to.df(input$exclusionTable)
              tblSuitability     <- hot.to.df(input$suitabilityTable)
              initHf             <- input$initialFacilityLayer # empty or existing
              nNewHf             <- input$newHfNumber
              maxProcessingTime  <- input$maxProcessingTime
              rmPotentialPop     <- input$rmPopPotential
            }




            # logic
            #zonalCoverage      <- 'zonalCoverage' %in% input$zonalPopOption
            returnPath         <- ifelse(dirAnalysis=='toHf',TRUE,FALSE) # return path = towards facilities.

            # tags format
            tags               <- unlist(strsplit(costTag,config$sepTagFile,fixed=T))
            
            # tags function
            addTag <- function(base,tag=tags,sepT=config$sepTagFile,sepC=config$sepClass){
              tag <- amGetUniqueTags(tag)
              base <- amClassInfo(base)$class
              paste(c(base,paste(tag,collapse=config$sepTagFile)),collapse=config$sepClass)
            }
          
            
            # set names WARNING : THIS IS ALREADY DONE IN VALIDATION ! TODO: Find a way to get those values.
            mapSpeed                 <- addTag('amSpeed')
            mapFriction              <- addTag('amFric')
            mapCumulative            <- addTag('amCumCost')
            mapPopResidual           <- addTag('amPopRes')
            hfCatchment              <- addTag('amHfCatch')
            mapPopOnBarrier          <- addTag('amPopBar')
            tableModel               <- addTag(tag=c(tags,'processed'),'amModTable')
            tableCapacityOut         <- addTag('amCapTable')
            tableZonalOut            <- addTag('amZoneCovTable')
            tableReferral            <- addTag('amRefTable')
            tableReferralNearestDist <- addTag('amRefTableDist')
            tableReferralNearestTime <- addTag('amRefTableTime')
            mapPotentialCoverage     <- addTag('amPotCov')
            mapNewHf                 <- addTag('amScalHfNew')
            tableScalingUp           <- addTag('amScalHfNewTable')
            tableCapacityOut         <- addTag('amScalCapTable')
            tableExclOut             <- addTag('amScalExclTable')
            tableSuitOut             <- addTag('amScalSuitTable')


         

            #
            # Start processing data
            #
            message(paste(typeAnalysis,'analysis in ',input$moduleSelector,'requested'))
            amUpdateProgressBar(session,"cumulative-progress",5)
            # keep record of error, redict or set priority according to config$msgListError in config. 
            amErrorAction(title=paste(input$moduleSelector,'Accessibility analysis'),{ 
              #
              # table save in DB
              #
              if(TRUE){ 
                dbCon=grassSession$dbCon
                dbWriteTable(grassSession$dbCon,tableModel,tbl,overwrite=TRUE)
                if(selectedAnalysis=='module_6'){
                  dbWriteTable(grassSession$dbCon,tableCapacityOut,tblCapacity,overwrite=TRUE)
                  dbWriteTable(grassSession$dbCon,tableSuitOut,tblSuitability,overwrite=TRUE)
                  dbWriteTable(grassSession$dbCon,tableExclOut,tblExclusion,overwrite=TRUE)
                }
              }
              #
              # get type of data for module 6 tables
              #
              if(selectedAnalysis=="module_6"){
                tblExclusion$type <- sapply(tblExclusion$layer,function(x){amGetType(x,config)})
                tblSuitability$type <- sapply(tblSuitability$layer,function(x){amGetType(x,config)})
                # get the temporary name of new facilities
                newFac <- config$newFacilitiesShort
                # replace temp new facility name by actual new layer
                tblExclusion$layer <- as.character(tblExclusion$layer)
                tblExclusion$layer[tblExclusion$layer==newFac] <- mapNewHf
                # replace temp new facility name by actual new layer
                tblSuitability$layer <- as.character(tblSuitability$layer)
                tblSuitability$layer[tblSuitability$layer==newFac] <- mapNewHf
              }


              # create HF vector map subset

              # create HF destination vector map subset 
              #if(input$moduleSelector=='module_4'){
              #  # create HF vector map subset
              #  qSql<-paste("cat IN (",paste0("'",tblHfSubsetTo$cat,"'",collapse=','),")")
              #  execGRASS("v.extract",flags='overwrite',input=mapHfTo,where=qSql,output='tmp_hf_to')
              #}
              # create base map for travel time computation.
              # NOTE: compute both for external analysis.
              amCreateSpeedMap(tbl,mapMerged,mapSpeed)
              amCreateFrictionMap(tbl,mapMerged,mapFriction,mapResol=listen$mapMeta$grid$North)
              # set initial progress bar. 
              amUpdateProgressBar(session,"cumulative-progress",10)
              #
              # Start analysis 
              #
              switch(selectedAnalysis,
                'module_2'={
                  qSql<-paste("cat IN (",paste0("'",tblHfSubset$cat,"'",collapse=','),")")
                  execGRASS("v.extract",flags='overwrite',input=mapHf,where=qSql,output='tmp_hf')
                  switch(typeAnalysis,
                    'anisotropic'= amAnisotropicTravelTime(
                      inputSpeed       = mapSpeed,
                      inputHf          = 'tmp_hf',
                      outputCumulative = mapCumulative,
                      returnPath       = returnPath,
                      maxCost          = maxTravelTime
                      ),
                    'isotropic'= amIsotropicTravelTime(
                      inputFriction    = mapFriction,
                      inputHf          = 'tmp_hf',
                      outputCumulative = mapCumulative,
                      maxCost          = maxTravelTime
                      ),
                    error(paste(typeAnalysis,'analysis not implemented'))
                    )
                  amUpdateProgressBar(session,"cumulative-progress",100)
                },
                'module_3'={
                  if(TRUE){ 
                    amMapPopOnBarrier(
                      inputPop=mapPop,
                      inputMerged=mapMerged,
                      outputMap=mapPopOnBarrier
                      )
                  }
                  tblOut<-amCapacityAnalysis(
                    inputSpeed        = mapSpeed,
                    inputFriction     = mapFriction,
                    inputPop          = mapPop,
                    inputHf           = mapHf,
                    inputTableHf        = tblHfSubset,
                    inputZoneAdmin    = mapZoneAdmin,
                    outputPopResidual = mapPopResidual,
                    outputTableHf       = tableHfOut,
                    outputHfCatchment = hfCatchment,
                    catchPath         = catchPath,
                    removeCapted      = 'rmPop' %in% modParam,
                    vectCatch         = 'vectCatch' %in% modParam,
                    typeAnalysis      = typeAnalysis,
                    returnPath        = returnPath,
                    radius            = popBuffer,
                    maxCost           = maxTravelTime,
                    maxCostOrder      = maxTravelTimeOrder,
                    hfIdx             = hfIdx,
                    capField          = capField,
                    orderField       = orderField,
                    #zonalCoverage     = zonalCoverage,
                    zonalCoverage     = 'zonalPop' %in% modParam,
                    zoneFieldId       = zoneFieldId,
                    zoneFieldLabel    = zoneFieldLabel,
                    hfOrder           = hfOrder,
                    hfOrderSorting    = hfOrderSorting,
                    dbCon             = isolate(grassSession$dbCon)
                    )
                  # write result in sqlite 
                  dbWriteTable(grassSession$dbCon,tableCapacityOut,tblOut[['capacityTable']],overwrite=T)
                  if(!is.null(tblOut$zonalTable)){
                    dbWriteTable(grassSession$dbCon,tableZonalOut,tblOut[['zonalTable']],overwrite=T)
                  }
                },
                'module_4'={
                  listTableReferral<-amReferralTable(
                    inputSpeed     = mapSpeed,
                    inputFriction  = mapFriction,
                    inputHf        = mapHf,
                    inputHfTo      = mapHfTo,
                    inputTableHf     = tblHfSubset,
                    inputTableHfTo   = tblHfSubsetTo,
                    idField        = hfIdx,
                    labelField     = hfLab,
                    idFieldTo      = hfIdxTo,
                    labelFieldTo   = hfLabTo,
                    typeAnalysis   = typeAnalysis,
                    resol          = listen$mapMeta$grid$No,
                    dbCon          = grassSession$dbCon,
                    unitCost       = 'm',
                    unitDist       = 'km',
                    outReferral    = tableReferral,
                    outNearestDist = tableReferralNearestDist,
                    outNearestTime = tableReferralNearestTime
                    )

                  amUpdateProgressBar(session,"cumulative-progress",100)
                },
                'module_6'={
                  amScalingUp(
                    inputSpeed            = mapSpeed,
                    inputFriction         = mapFriction,
                    inputPop              = mapPop,
                    inputLandCover        = mapMerged,
                    inputHf               = mapHf,
                    inputTableHf          = tblHfSubset,
                    inputTableCap         = tblCapacity,
                    inputTableExclusion   = tblExclusion,
                    inputTableSuitability = tblSuitability,
                    useExistingFacilities = initHf == "existing",
                    typeAnalysis          = typeAnalysis,
                    nFacilities           = nNewHf,
                    removePop             = rmPotentialPop,
                    maxProcessingTime     = maxProcessingTime,
                    outputFacilities      = mapNewHf,
                    outputTable           = tableScalingUp,
                    dbCon                 = grassSession$dbCon
                    )
                } 

                )
              amUpdateDataList(listen)
              amUpdateText(session,'costTag',"")
              }) # close isolate
          }) # close error handling 
        }
      })
      print(timeCheck)
    })


    ## module 5

    # update slider input 
    observe({
      cumCostSelect<-amNameCheck(dataList,input$cumulativeCostMapSelect,'raster')
      isolate({
        if(length(cumCostSelect)>0){
          cumCostStat<-read.table(
            text=execGRASS('r.univar',map=cumCostSelect,intern=T,flags='g'),
            sep='='
            )
          updateSliderInput(session,'sliderTimeAnalysis',
            max=ceiling(cumCostStat[cumCostStat$V1=='max',]$V2),
            min=floor(cumCostStat[cumCostStat$V1=='min',]$V2),
            step=1
            )
        }
      })
    })



    # prepare zonal map for later use : select zone where we have at least one HF.
    # TODO: 
    # 1. validate zone map field
    # 2. Check if this potentially costly operation should be protected ('create temp map button') 
    # 3. Check of previously map could match our need.
    # 4. create an entry in listen object to inform dependent function that this map has been updated
    # NOTE:
    # Warning. This is a risky assumption.
    # If subset of HF or different HT map has been used to compute cumulative cost map,
    # this will be mislanding : unrelated zone could be selected, and vice versa.
    observe({
         })




    observeEvent(input$btnZoneTravelTime,{
     mapZone<-amNameCheck(dataList,input$zoneSelect,'vector')
      mapHf<-amNameCheck(dataList,input$hfSelect,'vector')
      fieldZoneLabel<-input$zoneLabel
      fieldZoneId<-input$zoneId
      if(!is.null(mapZone) && 
        !is.null(mapHf) &&
        isTRUE(nchar(fieldZoneId)>0) &&
        isTRUE(nchar(fieldZoneLabel)>0) &&
        isTRUE(input$moduleSelector=='module_5')){
      
          # Create raster version of admin zone. 
          execGRASS('v.to.rast',
            input=mapZone,
            #cats=paste(useCat,collapse=','),
            output='tmp__map_zone',
            type='area',
            use='attr',
            label_column=fieldZoneLabel,
            attribute_column=fieldZoneId,
            flags='overwrite'
            )
      }


      #
      # Render table, map and chart
      #


      #
      # TODO: check why this is not isolated 
      #


    output$zoneCoverageTable<-renderHotable({
isolate({
      timeCumCost<-input$sliderTimeAnalysis
      zoneSelect<-amNameCheck(dataList,input$zoneSelect,'vector')
        if(timeCumCost>0 && !is.null(zoneSelect)){
          tmpZoneExists<-isTRUE('tmp__map_zone' == execGRASS('g.list',type='raster',pattern='tmp__map_zone',intern=T))
          if(tmpZoneExists){
            mapCumCost<-input$cumulativeCostMapSelect
            mapZone<-input$zoneSelect
            mapPop<-input$popSelect
            fieldZoneLabel<-input$zoneLabel
            fieldZoneId<-input$zoneId

            # reclass travel time to keep only the slider value
            tmpRulesFile<-tempfile()
            tmpRules<-paste(
              "0 thru", timeCumCost, "=" ,timeCumCost, "travelTime"
              )
            write(tmpRules,tmpRulesFile)
            execGRASS('r.reclass',
              input=mapCumCost,
              output='tmp__cum_cost',
              rules=tmpRulesFile,
              flags='overwrite'
              )
            # extract population under coverage area.
            execGRASS('r.mapcalc',expression=paste(
                'tmp__pop_under_travel_time=if(tmp__cum_cost==',timeCumCost,',',mapPop,',null())'
                ),flags='overwrite')

            # produce map
            if(TRUE){
              tmpMapTt<-file.path(tempdir(),'mapTravelTime.tiff')
              res<-isolate({listen$mapMeta$grid$Nor})
              nCols<-isolate({listen$mapMeta$grid$`Number of columns`})
              execGRASS('g.region',res=paste(res*nCols/500))
              exp=paste('tmp__cum_cost_preview=if(',mapCumCost,'<',timeCumCost,',',mapCumCost,',null())')
              execGRASS('r.mapcalc',expression=exp,flags='overwrite')
              execGRASS('r.out.gdal',
                flags =c('overwrite','f'),
                input='tmp__cum_cost_preview',
                output=tmpMapTt,
                format="GTiff",
                createopt='TFW=YES'
                )
              execGRASS('g.region',raster=config$mapDem)
              rTt<-raster(tmpMapTt)

              ## labels
              labelat = c(timeCumCost)
              labeltext = paste("Travel time",timeCumCost,"[min]")

              ## plot
              output$previewTravelTime<-renderPlot({
                plot(rTt,col=heat.colors(timeCumCost),main=paste("Cumulated travel time at",timeCumCost,"minutes."))

              })
            }
            statZonePopTravelTime<-read.table(text=
              execGRASS('r.univar',
                map='tmp__pop_under_travel_time',
                zones='tmp__map_zone',
                flags=c('g','t'),
                intern=T
                ),sep='|',header=T
              )[,c('zone','label','sum')]

            statZonePopTotal<-read.table(text=
              execGRASS('r.univar',
                map=mapPop,
                zones='tmp__map_zone',
                flags=c('g','t'),
                intern=T
                ),sep='|',header=T
              )[,c('zone','label','sum')]

            statZoneMerge<-merge(statZonePopTotal,statZonePopTravelTime,by=c('zone','label'),all.x=TRUE)
            names(statZoneMerge)<-c(fieldZoneId,fieldZoneLabel,'popTotal','popTravelTime')
            statZoneMerge$popCoveredPercent<-(statZoneMerge$popTravelTime/statZoneMerge$popTotal)*100
            statZoneMerge[is.na(statZoneMerge)]<-0
            return( statZoneMerge[order(statZoneMerge$popCoveredPercent),])
          }
        }

        ## default
        output$previewTravelTime<-renderPlot({
          plot(0,main='Travel time area')
          text(1,0.2,'Please enter the required information')
          text(1,-0.2,'and set a travel time greater than 0.')
        })

        return(data.frame(id='-',label='-',popTotal='-',popTravelTime='-',popCoveredPercent='-'))

})
    }, readOnly = FALSE, fixed=1)

    
    
    
    })




  }
})


#   # NOTE: output catchment as vector, merged and easily readable by other GIS.
#        # None of those methods worked at the time this script was written :
#        # v.overlay :  geometry / topology ok, seems the way to go ! But... how to handle hundred of overlays ? 
#        #              And grass doesn't like to work with non topological 'stacked' data. 
#        # v.patch : produced empty area and topologic errors, even without topology building (b flag)
#        # v.to.3d with groupId as height and v.patch after. V.patch transform back to 2d... with area errors.
#        # r.to.rast3 :groupId as Z. doesn't produce anything + 3d interface really bugged. 
#        # So, export and append to shapefile, reimport back after the loop. eerk.
#        tDir<-tempdir()      
#        tmpVectCatchOut=file.path(tDir,paste0('tmp__vect_catch','.shp'))
#        execGRASS('r.to.vect',
#          input=tmpPop,
#          output='tmp__vect_catch',
#          type='area',
#          flags=c('overwrite','v'),
#          column=hfIdxNew)
#        # for the first catchment : overwrite if exists, else append.
#        if(incN==1){
#          if(file.exists(tmpVectCatchOut)){ 
#            file.remove(tmpVectCatchOut)
#          }
#          outFlags=c('overwrite')
#        }else{
#          outFlags=c('a')
#        }
#        # update attribute table with actual ID.
#        dbRec<-dbGetQuery(grassSession$dbCon,'select * from tmp__vect_catch')
#        dbRec[,hfIdxNew]<-as.integer(i)
#        dbRec[,'label']<-NULL
#        dbWriteTable(grassSession$dbCon,'tmp__vect_catch',dbRec,overwrite=T)
#        # export to shapefile. Append if incC >1
#        execGRASS('v.out.ogr',input='tmp__vect_catch',output=tmpVectCatchOut,format='ESRI_Shapefile',
#          flags=outFlags,output_layer='tmp__vect_catch')
#      }
#
# render handson table for HF
# system time : 197 h 197 hf 
#  user  system elapsed
#  0.041   0.030   0.079
#observe({
#  selHf<-amNameCheck(dataList,input$hfSelect,'vector')
#  selMerged<-amNameCheck(dataList,input$mergedSelect,'raster')
#  selPop<-amNameCheck(dataList,input$popSelect,'raster')
#  isolate({
#    if(!is.null(selHf) && !is.null(selMerged)){
#      # check if HF are located on barrier
#      tbl<-read.table(
#        text=execGRASS("v.what.rast",map=selHf,raster=selMerged,flags='p',intern=T),
#        sep="|",stringsAsFactors=F)
#      names(tbl)<-c('cat','val')
#      tbl$amOnBarrier<-ifelse(tbl$val=='*',TRUE,FALSE)
#      tbl$amCatLandCover<-ifelse(tbl$val=='*',NA,tbl$val)
#      tbl$val<-NULL
#
#      if(!is.null(selPop)){
#        pop<-read.table(
#          text=execGRASS('v.what.rast',map=selHf,raster=selPop,flags='p',intern=T),
#          sep="|",stringsAsFactors=F)  
#        names(pop)<-c('cat','amPopCell')
#        pop[pop$amPopCell=='*','amPopCell']<-0 
#        pop$amPopCell<-as.numeric(pop$amPopCell)
#        tbl<-merge(tbl,pop,by='cat')
#      }
#
#
#      tbl$amSelect<-!sapply(tbl$amOnBarrier,isTRUE)
#
#
#      # copy hf attribute table from SQLite db.
#      tblAttribute<-dbGetQuery(grassSession$dbCon,paste('select * from',selHf))
#      # merge with first table
#      tbl<-merge(tbl,tblAttribute,by='cat')
#      nTable<-names(tbl)[!names(tbl)=='cat'] # remove cat column
#      tbl$amOnBarrier<-ifelse(tbl$amOnBarrier==TRUE,'yes','no')# avoid handsontable checkbox:use char
#      colOrder<-unique(c('cat','amSelect','amOnBarrier',names(tbl))) 
#      tbl<-tbl[,colOrder] 
#    }else{
#      tbl=data.frame(cat=as.integer(NA),amSelect=as.integer(NA),amOnBarrier=as.integer(NA))
#    }
#    output$hfTable<-renderHotable({
#      tbl
#    },readOnly=TRUE,fixed=5,stretch='last')
#  })
#})

