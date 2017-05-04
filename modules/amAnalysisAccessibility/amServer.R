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
        idData=c("tScenario","tScenarioOut"),
        idSelect="modelSelect",
        dataList=dataList
        )
    })
    observe({
      if(input$moduleSelector %in% c("module_3","module_5","module_6")){
        amUpdateSelectChoice(
          idData=c("rPopulationResidual","rPopulation"),
          idSelect=c("popSelect"),
          dataList=dataList
          )
      }else{
        amUpdateSelectChoice(
          idData="rPopulation",
          idSelect=c("popSelect"),
          dataList=dataList
          )
      }
    })
    observe({
      if(input$moduleSelector=="module_6"){
        amUpdateSelectChoice(
          idData=c("rPopulation","rPopulationResidual"),
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
          output$capacityTable<-renderHotable({
            tbl
          }
            ,readOnly = FALSE
            , fixed=3
            , stretch="last"
            ) 
        })
})
    })
    # add a row
    observeEvent(input$btnAddRowCapacity,{
      tbl<-hotToDf(input$capacityTable)
      row=data.frame(min=as.numeric(NA),max=as.numeric(NA),label=as.character(NA),capacity=as.numeric(NA))
      tbl$min<-as.numeric(tbl$min)
      tbl$max<-as.numeric(tbl$max)
      tbl$label<-as.character(tbl$label)
      tbl$capacity<-as.numeric(tbl$capacity)
      tbl<-rbind(tbl,row)
      output$capacityTable<-renderHotable({
        tbl
      }
        ,readOnly = FALSE
        , fixed=3
        , stretch="last"
        ) 
    })
    # remove a row
    observeEvent(input$btnRmRowCapacity,{
      tbl<-hotToDf(input$capacityTable)
      nrTable<-nrow(tbl)
      if(nrTable==1)return()
      tbl$min<-as.numeric(tbl$min)
      tbl$max<-as.numeric(tbl$max)
      tbl$label<-as.character(tbl$label)
      tbl$capacity<-as.numeric(tbl$capacity)

      output$capacityTable <- renderHotable({
        tbl[1:(nrTable-1),]
      }
        , readOnly = FALSE,
        , fixed=3
        , stretch="last"
        ) 
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
          zoneFieldsSummary <- amGetFieldsSummary(
            dbCon = grassSession$dbCon,
            table = zoneSel,
            getUniqueVal = F
            )
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
          if(length(selHfTo) &&isModReferral) return(
            amGetFieldsSummary(dbCon=grassSession$dbCon,selHfTo)
            )
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
            nHfField <- names(hfVal)
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
        amErrorAction(title='Hf fields filter val update',{
        if(hfTo){
          hfVal<-hfFieldsTo()$val
        }else{
          hfVal<-hfFields()$val
        }
        if(is.null(hfField) || isTRUE(nchar(hfField)==0)){hfField=config$vectorKey}
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
    })






    output$hfTableRules <- renderHotable({
      update <- input$hfSelect
      update <- input$hfSelectTo
      return(data.frame(
        id=integer(0),
        enable=logical(0),
        field=character(0),
        operator=character(0),
        value=character(0)
        ))
    })


    observeEvent(input$btnAddHfRule,{
  
        # get old values
        oldRules<-hotToDf(input$hfTableRules)
        # add new rule
        newRules<-data.frame(
          id=0,
          enable=TRUE,
          field=input$hfFilterField,
          operator=input$hfFilterOperator,
          value=paste(input$hfFilterVal,collapse='; ')
          )

        # if there is no old view, use new only
        if(!is.null(oldRules)){
          oldRules<-na.omit(oldRules)
          tbl <- rbind(oldRules,newRules)
        }else{
          tbl <- newRules
        } 
        tbl$id = as.integer(1:nrow(tbl))

      output$hfTableRules <- renderHotable({
        tbl
      })
    })

## handle remove rule
    observe({
      tbl <-hotToDf(input$hfTableRules)
      if(!amNoDataCheck(tbl)){
        output$hfTableRules <- renderHotable({
          tbl[tbl$enable,]
        })
      }
    })


  
    # update select order field
    observe({
      hfFields<-hfFields()$num
      if(length(hfFields)>0){
        hfFields<-hfFields[!hfFields ==config$vectorKey]
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
      hfFields<-hfFields()$idx
      if(length(hfFields)>0){
        sel=config$vectorKey
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
        sel=config$vectorKey
      }else{
        sel=''
        hfFields=""
      }
      updateSelectInput(session,'hfIdxFieldTo',choices=hfFields, selected=config$vectorKey)
    })

    # update select HF capacity fields
    observe({
      hfFields<-hfFields()$num
      hfIdx <- input$hfIdxField
      if(isTRUE(nchar(hfIdx)>0) && length(hfFields)>0){
        hfFields<-hfFields[!hfFields == config$vectorKey]
        hfFields<-hfFields[!hfFields == hfIdx ]
        capField<-grep('[cC]apac',hfFields,value=T)
        if(length(capField)>0){sel=capField[1]}else{sel=hfFields[1]}
      }else{
        hfFields=""
        sel=""
      }
      updateSelectInput(session,'hfCapacityField',choices=hfFields,selected=sel)
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


    observeEvent(input$selFactorLayer,{
      selFactor <- input$selFactorLayer 
      if(isTRUE(!is.null(selFactor) && nchar(selFactor) > 0 )){
        disBtn=FALSE
      }else{
        disBtn=TRUE
      } 
      amActionButtonToggle(session=session,'btnAddFactor',disable=disBtn)
    })

    #
    # indication of the number of cells processed for popsum distance
    #

    observeEvent(input$factorPopSumRadius,{

      radius <- input$factorPopSumRadius*1000
      ncellsTxt <- 0
      valid <- is.numeric(radius) && !is.na(radius) && length(radius)>0
      if(valid){
        grid <- listen$mapMeta$grid
        area <- pi*radius^2
        frac <- area/grid$nsres^2

        if(isTRUE(frac<1)){
          ncells = grid$cells
        }else{ 
          ncells <- frac*grid$cells
        }
        ncellsTxt <- format(ncells,digits="4",scientific=T)
        if(isTRUE(ncells>1e6)){
          ncellsTxt <- paste("warning",ncellsTxt,sep=": ") 
        }
      }

      amActionButtonToggle(session=session,'btnAddFactor',disable=!valid)
      amUpdateText(id="popSumNumCells",text=ncellsTxt)    
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
          output$exclusionTable <- renderHotable({
            tbl
          }
            ,readOnly = c(2,3,4)
            , fixed=1
            , stretch='last'
            ) 
        })
})
    })



    observeEvent(input$btnAddExclusion,{
      amErrorAction(title="Button add exclusion",{ 

        tbl<- na.omit(hotToDf(input$exclusionTable))
        layer <- input$selExclusion
        buffer <-input$exclusionBuffer
        method <- input$exclusionMethod

        tbl <- rbind(tbl,data.frame(select=TRUE,layer=layer,buffer=buffer,method=method))
        output$exclusionTable <- renderHotable({
          tbl
        },readOnly = c(2,3,4)
        , fixed=1
        , stretch='last'
        ) 

})

    })


    observeEvent(input$btnRmExcluUnselected,{
      amErrorAction(title="Button remove unselected exclusion row",{ 
        tbl<- na.omit(hotToDf(input$exclusionTable))
        if(!isTRUE(nrow(tbl)>0 && length(tbl$select)>0))return()

        tbl <- tbl[tbl$select,]
        if(nrow(tbl)<1){
            tbl=data.frame(select=as.logical(NA),layer=as.character(NA),buffer=as.numeric(NA),method=as.character(NA))
         # listen$initExclusionTable <- runif(1)
        }

        output$exclusionTable <- renderHotable({
          tbl
        },
        readOnly = c(2,3,4),
        fixed=1,
        stretch='last'
        ) 
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
          output$suitabilityTable <- renderHotable({
            tbl
          }
            , readOnly = c(2,3,4,5)
            , fixed=1
            , stretch='last'
            ) 
        })
})
    })

    observeEvent(input$btnAddFactor,{
      amErrorAction(title="Button add factor",{ 
        # init variables
        sep <- ";"
        opt <- character(0)
        # import input
        tbl <- na.omit(hotToDf(input$suitabilityTable))
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
        output$suitabilityTable <- renderHotable({
          tbl
        }
          , readOnly = c(2,3,4,5)
          , fixed=1
          , stretch='last'
          ) 
})
    })



    observeEvent(input$btnRmSuitTableUnselected,{
      amErrorAction(title="Button remove selecte suit table row",{ 
        tbl<- na.omit(hotToDf(input$suitabilityTable))
        if(!isTRUE(nrow(tbl)>0 && length(tbl$select)>0))return()
        tbl <- tbl[tbl$select,]
        if(nrow(tbl)<1){
            tbl=data.frame(select=as.logical(NA),factor=as.character(NA),layer=as.character(NA),weight=as.numeric(NA),options=as.character(NA))
        }
        output$suitabilityTable <- renderHotable({
          tbl
        }
          , readOnly = c(2,3,4,5)
          , fixed=1
          , stretch='last'
          ) 
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
            noLabel <- is.na(tbl$label) | is.null(tbl$label)
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
        output$speedRasterTable <- renderHotable({
          tbl
        }
          , readOnly = FALSE
          , fixed=2
          , stretch='all'
          )
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
        }
          , readOnly = TRUE
          , fixed = 2 
          , stretch = 'all'
          )
      })
    })




    # create facilitie table with additional accessMod column
    tblHfOrig<-reactive({
      selHf<-amNameCheck(dataList,input$hfSelect,'vector')
      selMerged<-amNameCheck(dataList,input$mergedSelect,'raster')
      selPop<-amNameCheck(dataList,input$popSelect,'raster')
      isolate({
       return( amCreateHfTable(
          mapHf=selHf,
          mapMerged=selMerged,
          mapPop=selPop,
          dbCon=grassSession$dbCon
          ))
      })
    })

    #create facilitie table for second table. 
    tblHfOrigTo<-reactive({
      selHf<-amNameCheck(dataList,input$hfSelect,'vector')
      selHfTo<-amNameCheck(dataList,input$hfSelectTo,'vector')
      selMerged<-amNameCheck(dataList,input$mergedSelect,'raster')
      selPop<-amNameCheck(dataList,input$popSelect,'raster')

      isolate({
        #if(input$moduleSelector=='module_4'){
        if(selHf==selHfTo && isTRUE(nrow(tblHfOrig())>0)){
          return(tblHfOrig())
        }else{
          return( amCreateHfTable(
              mapHf=selHfTo,
              mapMerged=selMerged,
              mapPop=selPop,
              dbCon=grassSession$dbCon
              ))
        }
      })
    })

    # render facilities table.
    observe({
      tbl<-tblHfOrig()
      if(!is.null(tbl) && nrow(tbl) > 0 ){

        tbl$amSelect <- TRUE

        # choose which columns display first.
        colOrder<-unique(c(config$vectorKey,'amSelect','amOnBarrier',names(tbl))) 
        tbl<-tbl[order(tbl$amOnBarrier,decreasing=T),colOrder] 
        # renderHotable convert logical to HTML checkbox and checkbox are always writable. 
        # To avoid write on this logical vector, use plain text :
        tbl$amOnBarrier <- ifelse(sapply(tbl$amOnBarrier,isTRUE),"yes","no")
      }else{
        # display at least a data frame with named column.
        tbl<-data.frame(cat=as.integer(NA),amSelect=as.integer(NA),amOnBarrier=as.integer(NA))
      }

      output$hfTable<-renderHotable({
        tbl
      }
        , readOnly =  !names(tbl) == "amSelect",
        , fixed = 5
        , stretch = 'all'
        )
    })

    # render facilities table to.
    observe({
      amErrorAction(title='tblHfOrigTo to hot',{
        tbl<-tblHfOrigTo()
        if(!is.null(tbl) && nrow(tbl) > 0){
          tbl$amSelect<-TRUE 
          # renderHotable convert logical to HTML checkbox and checkbox are always writable. 
          # To avoid write on this logical vector, use plain text :
          tbl$amOnBarrier<-ifelse(tbl$amOnBarrier==TRUE,'yes','no')
          # choose which columns display first.
          colOrder<-unique(c(config$vectorKey,'amSelect','amOnBarrier',names(tbl))) 
          tbl<-tbl[order(tbl$amOnBarrier,decreasing=T),colOrder] 
        }else{
          # display at least a data frame with named column.
          tbl<-data.frame(cat=as.integer(NA),amSelect=as.integer(NA),amOnBarrier=as.integer(NA))
        }
        output$hfTableTo<-renderHotable({
          tbl
        }
          , readOnly=!names(tbl) == "amSelect"
          , fixed=5
          , stretch='all'
          )
        })
    })


    # hf subset (from) used in other functions
    tblHfSubset<-reactive({
      tbl<-hotToDf(input$hfTable)
      if(!is.null(tbl)){
        tbl[[config$vectorKey]]<-as.integer(tbl[[config$vectorKey]])
        tbl <- tbl[sapply(tbl$amSelect,isTRUE),]
      }else{
        tbl <- data.frame()
      }
      return(tbl)
    })
    # hf subset (to) used in other functions
    tblHfSubsetTo<-reactive({
      tbl<-hotToDf(input$hfTableTo)
      if(!is.null(tbl)){
        tbl[[config$vectorKey]]<-as.integer(tbl[[config$vectorKey]])
        tbl <- tbl[sapply(tbl$amSelect,isTRUE),]
      }else{
        tbl <- data.frame()
      }
      return(tbl)
    })

    # buttons select hf with rules
    observe({ 
      btnHfRule<-input$btnSelectHfFromRule
      if(!is.null(btnHfRule) && btnHfRule>0){
        isolate({
          tblRule<-hotToDf(input$hfTableRules)
          selHfTo<-input$selHfFromTo=='To'
          isModReferral<-input$moduleSelector=='module_4'
          tblHf<-hotToDf(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
          if(!is.null(tblRule)&&!is.null(tblHf)){
            tblRule<-na.omit(tblRule)
            tblRule<-tblRule[tblRule$enable==TRUE,]
            if(nrow(tblRule)>0){
              tblHf$amSelect=FALSE
              for(i in 1:nrow(tblRule)){
                fi=tblRule[i,'field']
                op=as.character(tblRule[i,'operator'])
                vals = as.character(tblRule[i,'value'])
                if( grepl(";",vals)){
                va = unlist(strsplit(vals,';\\s'))
                }else{
                va = vals
                }
                
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
                tblHf[[config$vectorKey]]<-as.integer(tblHf[[config$vectorKey]])
                tblHf
              }
                ,readOnly=TRUE
                ,fixed=5
                ,stretch='last'
                )
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
#          tbl<-hotToDf(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
#          tbl$amSelect=FALSE
#          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
#            tbl[[config$vectorKey]]<-as.integer(tbl[[config$vectorKey]])
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
#          tbl<-hotToDf(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
#          tbl$amSelect=TRUE
#          output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
#            tbl[[config$vectorKey]]<-as.integer(tbl[[config$vectorKey]])
#            tbl
#          },readOnly=TRUE,fixed=5,stretch='last')
#        })
#      }
#    })
#

    ## Select random Hf
    #observe({
      #btnRandomHf<-input$btnSelectRandomHf
      #if(!is.null(btnRandomHf) && btnRandomHf>0){
        #isolate({
          #selHfTo<-input$selHfFromTo=='To'
          #isModReferral<-input$moduleSelector=='module_4'
          #tbl<-hotToDf(input[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]])
          #nR<-nrow(tbl)
          #sR=floor(nR/10)
          #dR<-nR-sR
          #sel<-sample(c(rep(TRUE,sR),rep(FALSE,dR))) 
          #tbl$amSelect=sel
          #output[[ifelse(selHfTo && isModReferral ,'hfTableTo','hfTable')]]<-renderHotable({
            #tbl[[config$vectorKey]]<-as.integer(tbl[[config$vectorKey]])
            #tbl
          #},readOnly=TRUE,fixed=5,stretch='last')
        #})
      #}
    #})



    # speed table merge button enabling

    observe({
      amErrorAction(title="Autocomplete scenario table validation",{
        selP <- listen$selProject
        tblOrig <-hotToDf(input$speedRasterTable)
        tblExt <-hotToDf(input$speedSqliteTable)

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
              if(!validMode) info <- c(info,paste("Some modes of transportation do not match currently allowed ones:",paste(names(config$listTranspMod),collapse=','),". Unknown mode(s) will be changed to the ",config$defaultTranspMode," mode."))
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
        tblOrig<-hotToDf(input$speedRasterTable)
        tblExt<-hotToDf(input$speedSqliteTable)
        if(length(tblOrig)>0 &&length(tblExt)>0){ 
          classOrig<-as.integer(tblOrig[,'class'])
          tblExt$class<-as.integer(tblExt$class)
          tblMergeOk <- tblExt[tblExt$class %in% classOrig,]
          tblMergeNo <- tblOrig[!classOrig %in% tblExt$class,]
          tblMerge<-rbind(tblMergeOk,tblMergeNo)
          tblMerge <- tblMerge[order(tblMerge$class,decreasing=F),]
          output$speedRasterTable<- renderHotable({
            tblMerge
          }
            , readOnly = 1
            , fixed=2
            , stretch='all'
            )
        }
        })
    })

    #validate if table is updated
    observeEvent(input$speedRasterTable,{
      tblUpdated <- na.omit(hotToDf(input$speedRasterTable))
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
            tblValidated<-data.frame(
              class=tblOriginal[,c('class')],
              tblUpdated[,c('label','speed','mode')]
              )
            # rule 2: if Speed is not integer, set to 0
            s<-as.numeric(tblUpdated$speed)
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
          output$speedRasterTable<- renderHotable({
            tblValidated
          }
          , readOnly = 1
          , fixed=2
          , stretch='all'
          )
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
        amUpdateText('costTag',"")
        # input table
        tbl                <- hotToDf(input$speedRasterTable)
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
        orderField         <- input$hfOrderColumn

        # parameters
        maxTravelTime      <- input$maxTravelTime
        maxTravelTimeOrder <- input$maxTravelTimeProcOrder
        dirAnalysis        <- input$dirAnalysis
        typeAnalysis       <- input$typeAnalysis
        limitClosest       <- input$checkReferralLimitClosest
        selectedAnalysis   <- input$moduleSelector
        hfOrder            <- input$hfOrder
        hfOrderSorting     <- input$hfOrderSorting
        popBuffer          <- input$popBufferRadius
        modParam           <- input$mod3param


        # scaling up only additional tables
        if(input$moduleSelector == 'module_6'){
          tblCapacity        <- na.omit(hotToDf(input$capacityTable))
          tblExclusion       <- na.omit(hotToDf(input$exclusionTable))
          tblSuitability     <- na.omit(hotToDf(input$suitabilityTable))
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
          #
          # table save in DB
          #
          if(TRUE){ 
            dbCon=grassSession$dbCon
            dbWriteTable(grassSession$dbCon,tableModel,tbl,overwrite=TRUE)

            if(selectedAnalysis=='module_6'){

              dbWriteTable(
                grassSession$dbCon,
                tableCapacityOut,
                tblCapacity,
                overwrite=TRUE
                )
              dbWriteTable(
                grassSession$dbCon,
                tableSuitOut,
                tblSuitability,
                overwrite=TRUE
                )
              dbWriteTable(
                grassSession$dbCon,
                tableExclOut,
                tblExclusion,
                overwrite=TRUE
                )
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

              exclType = sapply(tblExclusion$layer,function(x){amGetType(x)})
            }else{
              exclType = character(0)
            }
            tblExclusion$type <- exclType


            #
            # Suitability table
            #

            tblSuitability$layer <- as.character(tblSuitability$layer)
            # replace temp new facility name by actual new layer
            tblSuitability$layer[
              tblSuitability$layer == config$dynamicFacilities
              ] <- mapNewHf
            # replace temp pop name by actual new layer name
            tblSuitability$layer[
              tblSuitability$layer==config$dynamicPopulation
              ] <- mapPopResidualOut 
            # Get type (raster or vector) for each layer.
            tblSuitability$type <- sapply(
              tblSuitability$layer,
              function(x){
                amGetType(x)
              }
              )

          }

          #
          # create speed and friction map for travel time computation.
          #
          amCreateSpeedMap(tbl,mapMerged,mapSpeed)
          amCreateFrictionMap(tbl,mapMerged,mapFriction,mapResol=listen$mapMeta$grid$nsres)
          # set initial progress bar. 
          #
          # Start analysis 
          #
          switch(selectedAnalysis,
            'module_2'={

              pBarTitle = "Accessibility analysis"

              msg <- sprintf("Processing %s facilit%s in one step: this could be a very long process, please wait.",
                nrow(tblHfSubset),
                ifelse(nrow(tblHfSubset)>1,"ies","y")
                )
            
              pbc(
                visible=TRUE,
                percent=1,
                title=pBarTitle,
                text=msg,
                timeOut=3
                )

              qSql <- sprintf(" %1$s IN ( %2$s )",
                config$vectorKey,
                paste0("'",tblHfSubset[[config$vectorKey]],"'",collapse=',')
                )

              execGRASS(
                "v.extract",
                flags='overwrite',
                input=mapHf,
                where=qSql,
                output='tmp_hf'
                )

              switch(typeAnalysis,
                'anisotropic'= 
                  amAnisotropicTravelTime(
                  inputSpeed       = mapSpeed,
                  inputHf          = 'tmp_hf',
                  outputCumulative = mapCumulative,
                  returnPath       = returnPath,
                  maxCost          = maxTravelTime
                  )
                ,
                'isotropic'= amIsotropicTravelTime(
                  inputFriction    = mapFriction,
                  inputHf          = 'tmp_hf',
                  outputCumulative = mapCumulative,
                  maxCost          = maxTravelTime
                  )
                )

              pbc(
                visible=TRUE,
                percent=100,
                title=pBarTitle,
                text="Finished.",
                timeOut=2
                )

              pbc(
                visible=FALSE,
                )
              # 
              # Fnished without error
              #
              finished = TRUE
            },
            'module_3'={
              amMapPopOnBarrier(
                inputPop=mapPop,
                inputMerged=mapMerged,
                outputMap=mapPopOnBarrier
                )

              amErrorAction(title="Geographic coverage analysis",pBarFinalRm=TRUE,{    
                tblOut <- amCapacityAnalysis(
                  inputSpeed        = mapSpeed,
                  inputFriction     = mapFriction,
                  inputPop          = mapPop,
                  inputHf           = mapHf,
                  inputTableHf      = tblHfSubset,
                  inputZoneAdmin    = mapZoneAdmin,
                  outputPopResidual = mapPopResidualOut,
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
                  orderField        = orderField,
                  zonalCoverage     = 'zonalPop' %in% modParam,
                  zoneFieldId       = zoneFieldId,
                  zoneFieldLabel    = zoneFieldLabel,
                  hfOrder           = hfOrder,
                  hfOrderSorting    = hfOrderSorting,
                  pBarTitle         = "Geographic Coverage analysis",
                  dbCon             = grassSession$dbCon
                  )
                #
                # Write summary table in db
                # 

                dbWriteTable(
                  grassSession$dbCon,
                  tableCapacityStat,
                  tblOut[['capacityTable']],
                  overwrite=T
                  )
                #
                # Write zonal stat table if exists
                #
                if(!is.null(tblOut$zonalTable) && nrow(tblOut$zonalTable) > 0){
                  dbWriteTable(
                    grassSession$dbCon,
                    tableZonalStat,
                    tblOut[['zonalTable']],
                    overwrite=T)
                }
                # 
                # Fnished without error
                #

              finished = TRUE
                })
            },
            'module_4'={
              listTableReferral <- amAnalysisReferral(
                inputSpeed     = mapSpeed,
                inputFriction  = mapFriction,
                inputHf        = mapHf,
                inputHfTo      = mapHfTo,
                inputTableHf   = tblHfSubset,
                inputTableHfTo = tblHfSubsetTo,
                outReferral    = tableReferral,
                outNearestDist = tableReferralNearestDist,
                outNearestTime = tableReferralNearestTime,
                maxCost        = maxTravelTime,
                idField        = hfIdx,
                labelField     = hfLab,
                idFieldTo      = hfIdxTo,
                labelFieldTo   = hfLabTo,
                typeAnalysis   = typeAnalysis,
                limitClosest   = limitClosest,
                resol          = listen$mapMeta$grid$nsres,
                dbCon          = grassSession$dbCon,
                pBarTitle      = "Referral analysis",
                unitCost       = 'm',
                unitDist       = 'km'
                )
              # 
              # Fnished without error
              #
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
          #updateSliderInput(session,'sliderTimeAnalysis',
          updateNumericInput(session,'sliderTimeAnalysis',
            max = ceiling(amGetRasterStat(travelTimeSelect,'max')),
            min = floor(amGetRasterStat(travelTimeSelect,'min')),
            step=1
            )
        }
      })
    })



    # prepare zonal map for later use : select zone where we have at least one HF.



    observeEvent(input$btnZonalStat,{

      amErrorAction(title='Zonal stat',{
      # result list
      res <- list()

      mapZone<-amNameCheck(dataList,input$zoneSelect,'vector')
      mapPop <- amNameCheck(dataList,input$popSelect,'raster')
      mapTravelTime <- amNameCheck(dataList,input$travelTimeSelect,'raster')
      fieldZoneLabel<-input$zoneLabel
      fieldZoneId<-input$zoneId

      tmpMapZoneRaster <- sprintf(
        "tmp_zones_%s",digest::digest(c(mapZone,fieldZoneLabel,fieldZoneId))
        )

      if(!is.null(mapZone) && 
         !is.null(mapTravelTime) &&
        isTRUE(nchar(fieldZoneId)>0) &&
        isTRUE(nchar(fieldZoneLabel)>0 &&
        isTRUE(input$moduleSelector=='module_5')
          )
        ){


        selTime <- input$sliderTimeAnalysis
        #
        # Update numeric input
        #
        #updateNumericInput(session,"numZonal",value=selTime)
        #
        # Create raster version of admin zone. 
        #
        if(!isTRUE(amRastExists(tmpMapZoneRaster))){
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
          resolution = listen$mapMeta$grid$nsres,
          nColumns = listen$mapMeta$grid$cols,
          mapDem = config$mapDem
          )

        #
        # Zonal stat table
        #

        output$zoneCoverageTable<-renderHotable({
          #zonalTable <- listen$zonalStatTable
          zonalTable <- res$table
          if(is.null(zonalTable)){
            data.frame(id='-',label='-',popTotal='-',popTravelTime='-',popCoveredPercent='-')
          }else{
            zonalTable
          }
        }, readOnly = FALSE, fixed=1)


      }

      })
    })

  }
})

