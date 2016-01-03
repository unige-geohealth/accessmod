#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Every modules that implements accessibility are processed here.
# Dependencies :
#   amServer_validation.R
#   amHelp.R



#

# populate input
observe({
  # Avoid registering reactive function before the first time the module is shown.
  amModEnabled<-listen$tabControl_module_selector
  if(isTRUE(!is.null(amModEnabled) && amModEnabled)){
  # load module's dependencies
  source("modules/amAnalysisAccessibility/amServer_validation.R",local=T)
 
    #
    # Populate or update selectInput
    #
    observe({
      amUpdateSelectChoice(
        idData=c("rLandCoverMerged"),
        idSelect="mergedSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c("vFacilityNew","vFacility"),
        idSelect=c("hfSelect","hfSelectTo"),
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c("tScenarioOut","tScenarioNew"),
        idSelect="modelSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData="rPopulation",
        idSelect=c("popSelect"),
        dataList=dataList
        )
    })
    observe({
      if(input$moduleSelector=="module_6"){
        amUpdateSelectChoice(
          idData=c("rPopulationResidual","rPopulation"),
          idSelect=c("popResidualSelect"),
          dataList=dataList
          )
      }
    })
    observe({
      amUpdateSelectChoice(
        idData=c("rTravelTime"),
        idSelect="travelTimeSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c("vZone"),
        idSelect="zoneSelect",
        dataList=dataList
        )
    })
    observe({
      amUpdateSelectChoice(
        idData=c("tCapacity","tCapacityOut"),
        idSelect="capTableSelect",
        dataList=dataList
        )
    })
  observe({
      amUpdateSelectChoice(
        idData=c("tExclusion","tExclusionOut"),
        idSelect="exclusionTableSelect",
        dataList=dataList
        )
    })
  observe({
      amUpdateSelectChoice(
        idData=c("tSuitability","tSuitabilityOut"),
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
          idData=c("rPopulation","rPopulationResidual"),
          idSelect="selFactorLayer",
          addChoices=config$dynamicPopulation,
          dataList=dataList
          ),   
         "dist"=amUpdateSelectChoice(
          idData=c("vRoad","vBarrier","vFacility"),
          idSelect="selFactorLayer",
          addChoices=config$dynamicFacilities,
          dataList=dataList
          ),
        "traveltime"=amUpdateSelectChoice(
          idData=c("vRoad","vBarrier","vFacility"),
          idSelect="selFactorLayer",
          addChoices=config$dynamicFacilities,
          dataList=dataList
          ),
        "priority"=amUpdateSelectChoice(
          idData=c("rPriority"),
          idSelect="selFactorLayer",
          dataList=dataList)
        )
    })

    #
    #  set layer avilable for exclusion 
    #

    observe({
      amUpdateSelectChoice(
        idData=c("rExclusion","vExclusion"),
        addChoices=config$dynamicFacilities,
        idSelect="selExclusion",
        dataList=dataList
        ) 
    })

    #
    #  Capacity table  
    # 
    #extract capacity table and render in handson table
    observe({
      amErrorAction(title="Set new capacity table",{
        capNewTable<-amNameCheck(dataList,input$capTableSelect,"table",dbCon=grassSession$dbCon) 
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
          output$capacityTable<-renderHotable({tbl},readOnly = FALSE, fixed=3, stretch="last") 
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
      output$capacityTable<-renderHotable({tbl},readOnly = FALSE, fixed=3, stretch="last") 
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

      output$capacityTable<-renderHotable({tbl[1:(nrTable-1),]},readOnly = FALSE, fixed=3, stretch="last") 
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
  

    # update capacity label field
 observe({

      hfFields<-hfFields()$char
      hfIdx<-input$hfIdxField
      hfName<-input$hfNameField
      
      if(isTRUE(nchar(hfIdx)>0 && length(hfFields)>0)){
        hfFields<-hfFields[!hfFields %in% hfIdx]
        hfFields<-hfFields[!hfFields %in% hfName]
        nameField<-grep('[tT]ype|[lL]abel',hfFields,value=T)
      }else{ 
        hfFields=""
        nameField=""
      }
      if(length(nameField)>0){sel=nameField[1]}else{sel=hfFields[1]}
      updateSelectInput(session,'hfCapacityLabelField',choices=hfFields, selected=sel)
    })




    # update select order field
    observe({
      hfFields<-hfFields()$num
      if(length(hfFields)>0){
        hfFields<-hfFields[!hfFields =='cat']
        capField<-grep('[oO]rder|[cC]apac',hfFields,value=T)
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


    # update name fields
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
        excluTable<-amNameCheck(dataList,input$exclusionTableSelect,'table',dbCon=grassSession$dbCon)
        btnReset <- input$btnResetExcluTable
        isolate({
          if(is.null(excluTable)||nchar(excluTable)==0){
            tbl=data.frame(select=as.logical(NA),layer=as.character(NA),buffer=as.numeric(NA),method=as.character(NA))
          }else{
            tbl <- dbGetQuery(grassSession$dbCon,paste("SELECT * FROM",excluTable))
            if(nrow(tbl)<1) tbl[1,] <- NA
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
            tbl[,'mode']<-as.character(config$defaultTranspMode)
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




    # create facilitie table with additional accessMod column
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
#
#
#    # unselect HF (to/from)
#    observe({
#      btnNoHf<-input$btnSelecteNoHf
#      if(!is.null(btnNoHf) && btnNoHf>0){
#        isolate({
#          selHfTo<-input$selHfFromTo=='To'
#          isModReferral<-input$moduleSelector=='module_4'
#          tbl<-hot.to.df(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
#          tbl$amSelect=FALSE
#          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
#            tbl$cat<-as.integer(tbl$cat)
#            tbl
#          },readOnly=TRUE,fixed=5,stretch='last')
#        })
#      }
#    })
#    # select all Hf (to/from)
#    observe({
#      btnAllHf<-input$btnSelectAllHf
#      if(!is.null(btnAllHf) && btnAllHf>0){
#        isolate({
#          selHfTo<-input$selHfFromTo=='To'
#          isModReferral<-input$moduleSelector=='module_4'
#          tbl<-hot.to.df(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
#          tbl$amSelect=TRUE
#          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
#            tbl$cat<-as.integer(tbl$cat)
#            tbl
#          },readOnly=TRUE,fixed=5,stretch='last')
#        })
#      }
#    })
#

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
              if(!labelMatch) info <- c(info, "Some labels in the selected scenario table do not match those stored in the travel scenario to be processed. The later will be overwriten.")
              if(!classMatch) info <- c(info, "Some classes in the selected scenario table do not match those stored in the travel scenario to be processed. The corressponding information will not be imported.")
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
            m[!mTest]<- config$defaultTranspMode
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
    observeEvent(input$btnComputeAccessibility,{
    amErrorAction(title="Accessibility analysis (m2,m3,m4,m6)",pBarFinalRm=TRUE,{    
        # check time
        start <- Sys.time()
        # change this value to show or not the final message
        finished <- FALSE
        # invalidate data list 
        amUpdateDataList(listen)
        # update text
        amUpdateText(session,'costTag',"")
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
        mapPopResidual     <- amNameCheck(dataList,input$popResidualSelect,'raster')
        mapZoneAdmin       <- amNameCheck(dataList,input$zoneSelect,'vector')
        mapCumulativeCost  <- amNameCheck(dataList,input$cumulativeCostMapSelect,'raster')

        # catch. path
        catchPath          <- grassSession$pathShapes

        # field selection
        hfIdx              <- input$hfIdxField
        hfLab              <- input$hfNameField
        hfIdxTo            <- input$hfIdxFieldTo
        hfLabTo            <- input$hfNameFieldTo
        zoneFieldLabel     <- input$zoneLabel
        zoneFieldId        <- input$zoneId
        capField           <- input$hfCapacityField
        capLabelField      <- input$hfCapacityLabelField
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
          tblCapacity        <- na.omit(hot.to.df(input$capacityTable))
          tblExclusion       <- na.omit(hot.to.df(input$exclusionTable))
          tblSuitability     <- na.omit(hot.to.df(input$suitabilityTable))
          useExistingHf      <- input$useExistingHf == "TRUE" #radio button character value.. 
          maxScUpNewHf       <- input$maxScUpNewHf
          maxScUpPopGoal     <- input$maxScUpPopGoal
          maxScUpTime        <- input$maxScUpTime

          maxProcessingTime  <- input$maxProcessingTime
          #              rmPotentialPop     <- input$rmPopPotential
          rmPotentialPop     <- TRUE
        }

        # logic
        #zonalCoverage      <- 'zonalCoverage' %in% input$zonalPopOption
        returnPath         <- ifelse(dirAnalysis=='toHf',TRUE,FALSE) # return path = towards facilities.
        #
        #            # tags format
        #            tags               <- unlist(strsplit(costTag,config$sepTagFile,fixed=T))
        #            
        #            # tags function
        #            addTag <- function(base,tag=tags,sepT=config$sepTagFile,sepC=config$sepClass){
        #              tag <- amGetUniqueTags(tag)
        #              base <- amClassInfo(base)$class
        #              paste(c(base,paste(tag,collapse=config$sepTagFile)),collapse=config$sepClass)
        #            }
        #          

        # set output names.
        out <-  listen$outputNames$file
        mapSpeed                 <- out['rSpeed']
        mapFriction              <- out['rFriction']
        mapCumulative            <- out['rTravelTime']
        mapPopResidualOut        <- out['rPopulationResidual']
        hfCatchment              <- out['vCatchment']
        hfCatchmentNew           <- out['vCatchmentNew']
        mapPopOnBarrier          <- out['rPopulationOnBarrier']
        tableModel               <- out['tScenarioOut']
        tableCapacityOut         <- out['tCapacityOut']
        tableCapacityStat        <- out['tCapacityStat']
        tableCapacityStatNew     <- out['tCapacityStatNew']
        tableZonalStat           <- out['tZonalStat']
        tableReferral            <- out['tReferral']
        tableReferralNearestDist <- out['tReferralDist']
        tableReferralNearestTime <- out['tReferralTime']
        mapNewHf                 <- out['vFacilityNew']
        tableExclOut             <- out['tExclusionOut']
        tableSuitOut             <- out['tSuitabilityOut']





        #
        # Start processing data
        #
        message(paste(typeAnalysis,'analysis in ',input$moduleSelector,'requested'))
        amUpdateProgressBar(session,"cumulative-progress",5)
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
          # set table and value for module_6 
          #
          if(selectedAnalysis=="module_6"){
            #
            # Exclusion table
            #
            tblExclusion$layer <- as.character(tblExclusion$layer)
            # Replace dynamic facility name by given layer name
            tblExclusion$layer[tblExclusion$layer==config$dynamicFacilities] <- mapNewHf

            #

            # Get type (raster or vector) for each layer.
            if(nrow(tblExclusion)>0){

              exclType = sapply(tblExclusion$layer,function(x){amGetType(x,config)})
            }else{
              exclType = character(0)
            }
            tblExclusion$type <- exclType


            #
            # Suitability table
            #

            tblSuitability$layer <- as.character(tblSuitability$layer)
            # replace temp new facility name by actual new layer
            tblSuitability$layer[tblSuitability$layer==config$dynamicFacilities] <- mapNewHf
            # replace temp pop name by actual new layer name
            tblSuitability$layer[tblSuitability$layer==config$dynamicPopulation] <- mapPopResidualOut 
            # Get type (raster or vector) for each layer.
            tblSuitability$type <- sapply(tblSuitability$layer,function(x){amGetType(x,config)})

          }

          #
          # create speed and friction map for travel time computation.
          #
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
                  )
                )
              amUpdateProgressBar(session,"cumulative-progress",100)
            },
            'module_3'={
              #  if(TRUE){ 
              #    amMapPopOnBarrier(
              #      inputPop=mapPop,
              #      inputMerged=mapMerged,
              #      outputMap=mapPopOnBarrier
              #      )
              #  }
              tblOut <- amCapacityAnalysis(
                inputSpeed        = mapSpeed,
                inputFriction     = mapFriction,
                inputPop          = mapPop,
                inputHf           = mapHf,
                inputTableHf      = tblHfSubset,
                inputZoneAdmin    = mapZoneAdmin,
                outputPopResidual = mapPopResidualOut,
                outputTableHf     = tableHfOut,
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
                nameField         = hfLab,
                capField          = capField,
                capLabelField     = capLabelField,
                orderField        = orderField,
                zonalCoverage     = 'zonalPop' %in% modParam,
                zoneFieldId       = zoneFieldId,
                zoneFieldLabel    = zoneFieldLabel,
                hfOrder           = hfOrder,
                hfOrderSorting    = hfOrderSorting,
                dbCon             = isolate(grassSession$dbCon)
                )
              # write result in sqlite 
              dbWriteTable(grassSession$dbCon,tableCapacityStat,tblOut[['capacityTable']],overwrite=T)
              if(!is.null(tblOut$zonalTable)){
                dbWriteTable(grassSession$dbCon,tableZonalStat,tblOut[['zonalTable']],overwrite=T)
              }
              finished = TRUE
            },
            'module_4'={
              listTableReferral<-amReferralTable(
                inputSpeed     = mapSpeed,
                inputFriction  = mapFriction,
                inputHf        = mapHf,
                inputHfTo      = mapHfTo,
                inputTableHf   = tblHfSubset,
                inputTableHfTo = tblHfSubsetTo,
                outReferral    = tableReferral,
                outNearestDist = tableReferralNearestDist,
                outNearestTime = tableReferralNearestTime,
                idField        = hfIdx,
                labelField     = hfLab,
                idFieldTo      = hfIdxTo,
                labelFieldTo   = hfLabTo,
                typeAnalysis   = typeAnalysis,
                resol          = listen$mapMeta$grid$No,
                dbCon          = grassSession$dbCon,
                unitCost       = 'm',
                unitDist       = 'km'
                )

              amUpdateProgressBar(session,"cumulative-progress",100)

              finished = TRUE
            },
            'module_6'={

    amErrorAction(title="Accessibility analysis (m6)",pBarFinalRm=TRUE,{    
              titleAnalysis <- paste(input$moduleSelector,": Scaling up analysis")
                amScalingUp(
                  inputSpeed              = mapSpeed,
                  inputFriction           = mapFriction,
                  inputPop                = mapPop,
                  inputPopResidual        = mapPopResidual,
                  inputFacility           = mapHf,
                  inputTableFacility      = tblHfSubset,
                  inputTableCapacity      = tblCapacity,
                  inputTableExclusion     = tblExclusion,
                  inputTableSuitability   = tblSuitability,
                  outputFacility          = mapNewHf,
                  outputPopResidual       = mapPopResidualOut,
                  outputCatchment         = hfCatchmentNew,
                  outputCapacityAnalysis  = tableCapacityStatNew,
                  maxCost                 = maxTravelTime,
                  facilityIndexField      = hfIdx,
                  facilityCapacityField   = capField,
                  facilityNameField       = hfLab,
                  useExistingFacilities   = useExistingHf,
                  typeAnalysis            = typeAnalysis,
                  limitFacilitiesNumber   = maxScUpNewHf,
                  limitProcessingTime     = maxScUpTime,
                  limitPopCoveragePercent = maxScUpPopGoal,
                  pBarTitle               = "Scaling up analysis",
                  dbCon                   = grassSession$dbCon
                  )

              finished <- TRUE
                })
            } 
            )

          if(finished){
            #
            # Send tags and filename
            #
            # send unique tags
            listen$lastComputedTags <- amGetUniqueTags(input$costTag)
            # send real file names to avoid other files with same name
            listen$outFiles <- listen$outputNames$file
            #
            # Remove old tags
            #
            updateTextInput(session,"costTag",value="")
            # 
            # Create ui output message.
            #
            outNames <-  listen$outputNames$ui
            outputDatasets <- tags$ul(
              HTML(paste("<li>",outNames,"</li>"))
              )
            timing <- round(difftime(Sys.time(),start,units="m"),3)
            msg <- sprintf("Process finished in %s minutes. Output data names:",timing)
            msg2 <- sprintf("Items selected in data manager.")
            msg <- tagList(
              p(msg),
              outputDatasets,
              p(msg2)
              )
            amMsg(session,type='message',title='Process finished',text=msg)
          }
      })

    })


    ## module 5

    # update slider input 
    observe({
      travelTimeSelect <- amNameCheck(dataList,input$travelTimeSelect,'raster')
      isolate({
        if(!is.null(travelTimeSelect)){
          updateSliderInput(session,'sliderTimeAnalysis',
            max = ceiling(amGetRasterStat(travelTimeSelect,'max')),
            min = floor(amGetRasterStat(travelTimeSelect,'min')),
            step=1
            )
        }
      })
    })



    # prepare zonal map for later use : select zone where we have at least one HF.
    observeEvent(input$btnZoneTravelTime,{
        
      mapZone<-amNameCheck(dataList,input$zoneSelect,'vector')
        mapPop <- amNameCheck(dataList,input$popSelect,'raster')
        mapTravelTime <- amNameCheck(dataList,input$travelTimeSelect,'raster')
        #mapHf<-amNameCheck(dataList,input$hfSelect,'vector')

        fieldZoneLabel<-input$zoneLabel
        fieldZoneId<-input$zoneId

        tmpMapZoneRaster <- "tmp__map_zone"

        if(!is.null(mapZone) && 
          isTRUE(nchar(fieldZoneId)>0) &&
          isTRUE(nchar(fieldZoneLabel)>0)
          ){
          # Create raster version of admin zone. 
          execGRASS('v.to.rast',
            input=mapZone,
            output=tmpMapZoneRaster,
            type='area',
            use='attr',
            label_column=fieldZoneLabel,
            attribute_column=fieldZoneId,
            flags='overwrite'
            )
        }

        #
        # Generate table and plot
        #


        res <- amZonalAnalysis(
          inputTravelTime = mapTravelTime ,
          inputPop = mapPop,
          inputZone = mapZone,
          inputZoneTemp = tmpMapZoneRaster,
          timeCumCost = input$sliderTimeAnalysis ,
          zoneIdField = fieldZoneId,
          zoneLabelField = fieldZoneLabel,
          resolution = listen$mapMeta$grid$Nor,
          nColumns = listen$mapMeta$grid$`Number of columns`,
          mapDem = config$mapDem
          )


        listen$zonalStatTable <- res$table
        listen$zonalStatPlot <- res$plot


    })


    #
    # Zonal stat table
    #

    output$zoneCoverageTable<-renderHotable({
      zonalTable <- listen$zonalStatTable
      if(is.null(zonalTable)){
        data.frame(id='-',label='-',popTotal='-',popTravelTime='-',popCoveredPercent='-')
      }else{
        zonalTable
      }
    }, readOnly = FALSE, fixed=1)



    #
    # Zonal stat plot
    #
    output$previewTravelTime<-renderPlot({

      zonalTime <- listen$zonalStatPlot
      maxTime <- isolate(input$sliderTimeAnalysis)

      if(is.null(zonalTime)){
        plot(0,main='Travel time area')
        text(1,0.2,'Please enter the required information')
        text(1,-0.2,'and set a travel time greater than 0.')
      }else{
        plot(zonalTime,
          col=heat.colors(maxTime),
          main=paste("Cumulated travel time at",maxTime,"minutes.")
          )


      }


    })


  }
})

