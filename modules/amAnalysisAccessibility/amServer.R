#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 3: calc accessibility to health facility.
# 
# server

# TODO: avoid multiple hot.to.df with HF table. Use reactive table instead.


observe({
  amModEnabled<-listen$tabControl_module_selector
  if(!is.null(amModEnabled) && amModEnabled){
    #amErrorAction(title='Module Accessibility',{
    #
    # Populate or update selectInput
    #
    observe({
      mergedList<-amListData('amLcvM',dataList)
      if(length(mergedList)==0)mergedList=character(1)
      updateSelectInput(session,'mergedSelect',choices=mergedList,selected=mergedList[1])
    })
    observe({
      hfList<-amListData('amHf',dataList)
      if(length(hfList)==0)hfList=character(1)
      updateSelectInput(session,'hfSelect',choices=hfList,selected=hfList[1])
      updateSelectInput(session,'hfSelectTo',choices=hfList,selected=hfList[1])
    })
    observe({
      modelList<-amListData('amModTbl',dataList)
      if(length(modelList)==0)modelList=character(1)
      updateSelectInput(session,'modelSelect',choices=modelList,selected=modelList[1])
    })
    observe({
      popList<-amListData('amPop',dataList)
      if(length(popList)==0)popList=character(1)
      updateSelectInput(session,'popSelect',choices=popList,selected=popList[1])
    })
    observe({
      popResList<-amListData('amPopRes',dataList)
      if(length(popResList)==0)popResList=character(1)
      updateSelectInput(session,'popResSelect',choices=popResList,selected=popResList[1])
    })
    observe({
      cumCostList<-amListData('amCumCost',dataList)
      if(length(cumCostList)==0)cumCostList=character(1)
      updateSelectInput(session,'cumulativeCostMapSelect',choices=cumCostList,selected=cumCostList[1])
    })
    observe({
      zoneList<-amListData('amZone',dataList)
      if(length(zoneList)==0)zoneList=character(1)
      updateSelectInput(session,'zoneSelect',choices=zoneList,selected=zoneList[1])
    })
    observe({
      capTbl<-amListData('amNewCapTbl',dataList)
      if(length(capTbl)==0)capTbl=character(1)
      updateSelectInput(session,'capTblSelect',choices=capTbl,selected=capTbl[1])
    })




    observe({
      amErrorAction(title='Set new capacity table',{
        capNewTbl<-amNameCheck(dataList,input$capTblSelect,'table',dbCon=grassSession$dbCon)
        isolate({
          if(is.null(capNewTbl)||nchar(capNewTbl)==0){
            #tbl<-data.frame(min_pop=as.integer(NA),max_pop=as.integer(NA),hf_type=as.character(NA),capacity=as.integer(NA))
           # tbl=data.frame(
           #   min=as.integer(c(0,4125,55042,353877,2102663)),
           #   max=as.integer(c(4124,55041,353876,2012662,999999999)),
           #   label=c("Other Health Facility","Health Centre","Hospital Level 1","Hospital Level 2","Hospital Level 3"),
           #   capacity=as.integer(c(4124,5893,78632,505539,2875233))
           #   )
            tbl=data.frame(min="-",max="-",label="-",capacity="-")
          }else{
            tbl=dbGetQuery(grassSession$dbCon,paste("SELECT * FROM",capNewTbl))
          }
          output$capacityTable<-renderHotable({tbl},readOnly = FALSE, fixed=3, stretch='last') 
        })
})
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


    # NOTE: check if this table correctly handle change of HF To/From !
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
    # Tags
    #

    # tag format
  #  observe({
  #    costTag<-input$costTag 
  #    if(isTRUE(nchar(costTag)>0)){
  #      updateTextInput(session,'costTag',value=amSubPunct(costTag,config$sepTagUi))
  #    }
  #  })

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
    # General validation and error message
    #

    # preventive field validation
    # TODO: this validation step was written for one module:
    # With almost all modules depending on it, this should be rewritten.
    observe({
      amErrorAction(title='Module 2,3,4,6: validation',{
        #empty msg container
        err = character(0)
        info = character(0)
        # check current module (TODO:clean this)
        module2 <- isTRUE(input$moduleSelector =='module_2')
        module3 <- isTRUE(input$moduleSelector == 'module_3')
        module4 <- isTRUE(input$moduleSelector =='module_4')
        module6 <- isTRUE(input$moduleSelector =='module_6')
        # map validation for all modules
        merged <- isTRUE(!is.null(amNameCheck(dataList,input$mergedSelect,'raster')))
        hf     <- isTRUE(!is.null(amNameCheck(dataList,input$hfSelect,'vector')))
        pop    <- isTRUE(!is.null(amNameCheck(dataList,input$popSelect,'raster')))
        # table validation
        #tblHf<-any(hot.to.df(input$hfTable)$select) ## if many columns or rows, to slow!
        hfOnBarrier<-isTRUE(
          any(tblHfSubset()$amOnBarrier=='yes') ||
          any(tblHfSubsetTo()$amOnBarrier=='yes') 
          )
        # check if there is at least one hospital selectect.
        hfNoHf            <- isTRUE(!any(tblHfSubset()$amSelect))
        hfNoHfTo          <- isTRUE(!any(tblHfSubsetTo()$amSelect))
        # check for speed of  0 kmh
        tblModel          <- isTRUE(!any(hot.to.df(input$speedRasterTable)$speed <1))
        # parameter validation
        costTag           <- input$costTag
        tag               <- isTRUE(nchar(costTag)>0)
        maxTT             <- isTRUE(input$maxTimeWalk == 0)
        # population on barrier
        popBarrierSum     <- popOnBarrierStat()$sum
        popBarrierCells   <- popOnBarrierStat()$cells
        popBarrierPercent <- popOnBarrierStat()$percent

        if(module2){
          # map overwrite warning module 2
          costTag<-unlist(costTag)
          cumulativeName<-paste(c('cumulative_cost',paste(costTag,collapse=config$sepTagFile)),collapse=config$sepClass )
          cumulativeCostExists <-isTRUE(cumulativeName %in% amNameCheck(dataList,isolate(dataList$raster),'raster'))
        }
        if(module3){
          hfIdx<-isTRUE(length(input$hfIdxField)>0)
          capField<-isTRUE(length(input$hfCapacityField)>0)
          hfBuffer<-isTRUE(input$hfOrder == 'circBuffer')
          popBuffer<-isTRUE(input$popBufferRadius > listen$mapMeta$grid$`North`)
          #popBarrier<-isTRUE('popBarrier' %in% input$mod3param)
          popBarrierFound<-isTRUE(popBarrierSum>0)
          zonalPop<-isTRUE('zonalPop' %in% input$mod3param)

          if(zonalPop){
            zonalSelect<-isTRUE(!is.null(amNameCheck(dataList,input$zoneSelect,'vector')))
            zoneId<-isTRUE(length(input$zoneId)>0)
            zoneLabel<-isTRUE(length(input$zoneLabel)>0)
          }

          zonalCoverage<-isTRUE(zonalPop && 'zonalCoverage' %in% input$zonalPopOption)
          # Selection inconsistency
          hfOrderInconsistency<-isTRUE(input$hfOrder!='tableOrder' && !'rmPop' %in% input$mod3param)
          zonalCoverageInconsistency <- isTRUE(zonalCoverage && !'rmPop' %in% input$mod3param)
          # data overwrite warning module 3 : validate each output !
          # TODO: inform user of all provided output. Warning if risk of overwrite.
        }
        if(module6){
          capNewTbl <- hot.to.df(input$capacityTable)
          if(!is.null(capNewTbl))capNewTbl<-na.omit(capNewTbl)
          tblCapacityOk <- isTRUE(nrow(capNewTbl)>0)
        }


        # register messages
        if(!tag) err = c(err,'No tags entered.')
        if(!merged) err = c(err,'Merged land cover missing.')
        if(!hf) err = c(err,'Health facilities map missing.')
        if(hfOnBarrier) err = c(err, 'There are facilities located on barrier, unselect them to proceed.')
        if(maxTT) info = c(info,'Unlimited travel time')
        #if(hf)if(!tblHf) err = c(err,'at least one facilities must be selected') ## too slow
        if(merged)if(!tblModel) err = c(err,'Speed of 0 km/h not allowed.')

        if(module2){
          if(hfNoHf) err = c(err, 'Select at least one facility.')
          if(cumulativeCostExists) info = c(info,paste('Map',cumulativeName,'exists and will be overwritten.'))
        }
        if(module3){
          if(!pop) err = c(err,'Population map missing.')
          if(!hfIdx) err = c(err,'No group/id field set for hf.')
          if(hfNoHf) err = c(err, 'Select at least one facility.')
          if(!capField) err = c(err,'No capacity field set for hf.')
          if(hfBuffer)if(!popBuffer) err = c(err,'Circular buffer must be higher to project resolution.')
          #if(!popBarrier) info = c(info,'Map of population on barrier will NOT be computed.')
          if(popBarrierFound) info = c(info,paste('Population encoutered on barrier in',popBarrierCells,' cells for a total of ',popBarrierSum,'individuals.(',popBarrierPercent,'% of total)'))
          if(hfOrderInconsistency) info=c(info,"If covered population is not removed at each iteration, facilities processing order should be set to 'Order from health facilities table.'")
          if(zonalCoverage){
            if(!zonalSelect) err=c(err,'Zonal map missing.')
            if(!zoneId) err =c(err,'Zonal id column missing.')
            if(!zoneLabel) err =c(err,'Zonal label column missing.')

          }
          if(zonalCoverageInconsistency) err = c(err,'If covered population is not removed at each iteration, zonal analysis could not be performed.')
        }
        if(module4){
          if(hfNoHf) err = c(err, "Select at least one facility in table 'FROM'.")
          if(hfNoHfTo) err = c(err,"Select at least one facility in table 'TO'. ")
        }
        if(module6){
          if(!tblCapacityOk) err = c(err,'Set at least one complete row for capacity table.')
          if(hfNoHf) err = c(err, "Select at least one facility.") 
        }


        # create HTML for validation message list.
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
        amActionButtonToggle(session=session,'btnComputeAccessibility',disable=disBtn)
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
        names(tbl)<-config$tableColNames[['table_model']] 
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
        }else{
          tbl<-data.frame(as.integer(NA),as.character(NA),as.integer(NA),as.character(NA))
          names(tbl)<-config$tableColNames[['table_model']] 
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


    # table merge process.
    observe({
      btn<-input$speedTableMerge
      isolate({
        tblOrig<-hot.to.df(input$speedRasterTable)
        tblExt<-hot.to.df(input$speedSqliteTable)
        if(!is.null(btn) && btn > 0 && length(tblOrig)>0 &&length(tblExt)>0){ 
          classOrig<-as.integer(tblOrig[,'class'])
          tblExt$class<-as.integer(tblExt$class)
          tblMerge<-tblExt[tblExt$class==classOrig,]
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
            costTag           <- amSubPunct(input$costTag,config$sepTagFile,rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T)

            # handson tables and reactive table subset.
            tbl<-hot.to.df(input$speedRasterTable)
            tblHfSubset<-tblHfSubset()
            if(input$moduleSelector=='module_4'){ 
              tblHfSubsetTo<-tblHfSubsetTo()
            }
            tblCap<-hot.to.df(input$capacityTable)


            # maps
            mapMerged         <- amNameCheck(dataList,input$mergedSelect,'raster')
            mapHf             <- amNameCheck(dataList,input$hfSelect,'vector')
            mapHfTo           <- amNameCheck(dataList,input$hfSelectTo,'vector')
            mapPop            <- amNameCheck(dataList,input$popSelect,'raster')
            mapPopRes         <- amNameCheck(dataList,input$popResSelect,'raster')
            mapZoneAdmin      <- amNameCheck(dataList,input$zoneSelect,'vector')
            mapCumulativeCost <- amNameCheck(dataList,input$cumulativeCostMapSelect,'raster')

            # field selection
            hfIdx             <- input$hfIdxField
            hfLab             <- input$hfNameFieldTo
            hfIdxTo           <- input$hfIdxFieldTo
            hfLabTo           <- input$hfNameFieldTo
            zoneFieldLabel    <- input$zoneLabel
            zoneFieldId       <- input$zoneId
            capField          <- input$hfCapacityField

            # parameters
            maxTimeWalk       <- input$maxTimeWalk
            dirAnalysis       <- input$dirAnalysis
            typeAnalysis      <- input$typeAnalysis
            selectedAnalysis  <- input$moduleSelector 
            hfOrder           <- input$hfOrder
            hfOrderSorting    <- input$hfOrderSorting
            popBuffer         <- input$popBufferRadius
            modParam          <- input$mod3param
            # scaling up
            minTravelTime     <- input$minTravelTime
            nNewHf            <- input$newHfNumber
            lcvIgnoreClass    <- input$excludeLandCoverClass
            maxProcessingTime <- input$maxProcessingTime
            rmPotentialPop    <- input$rmPopPotential


            # clean and transform.
            zonalCoverage     <- 'zonalCoverage' %in% input$zonalPopOption
            # return path = towards facilities.
            returnPath        <- ifelse(dirAnalysis=='toHf',TRUE,FALSE)
            # max cost from minutes to seconds
            maxCost           <- maxTimeWalk*60
            minPrecedingCost  <- minTravelTime*60

            # map name formating
            tags              <- unlist(strsplit(costTag,config$sepTagFile,fixed=T))
            # function to add and format tags for output dataset
            addTag <- function(base,tag=tags,sepT=config$sepTagFile,sepC=config$sepClass){
              base <- amClassInfo(base)$class
              paste(c(base,paste(tag,collapse=config$sepTagFile)),collapse=config$sepClass)
            }
            # set names
            mapSpeed                 <- addTag('amSpeed')
            mapFriction              <- addTag('amFric')
            mapCumulative            <- addTag('amCumCost')
            mapPopResidual           <- addTag('amPopRes')
            hfCatchment              <- addTag('amHfCatch')
            mapPopOnBarrier          <- addTag('amPopBar')
            tableModel               <- addTag('amModTbl')
            tableCapacityOut         <- addTag('amCapTbl')
            tableZonalOut            <- addTag('amZoneCovTbl')
            tableReferral            <- addTag('amRefTbl')
            tableReferralNearestDist <- addTag('amRefTblDist')
            tableReferralNearestTime <- addTag('amRefTblTime')
            mapPotentialCoverage     <- addTag('amPotCov')
            mapNewHf                 <- addTag('amHfNew')
            tableScalingUp          <- addTag('amHfNewTbl')
            tableCapacityNew         <- addTag('amNewCapTbl')
            #
            # Start processing data
            #
            message(paste(typeAnalysis,'analysis in ',input$moduleSelector,'requested'))
            amUpdateProgressBar(session,"cumulative-progress",5)
            # keep record of error, redict or set priority according to config$msgListError in config. 
            amErrorAction(title=paste(input$moduleSelector,'Accessibility analysis'),{ 
              # Test if a table with same name exists. 
              # a.If their content are identical, do nothing. 
              # b.If their content differ, add a short time stamp to the name
              # c.if the table doesnt exists, save it.
              # TODO: find a way to avoid enormous amount of duplicate with different names?
              dbCon=grassSession$dbCon
             # if(tableModel %in% amNameCheck(dataList,name=dataList$table,'table',dbCon=dbCon)){
             #   tblStored<-dbGetQuery(dbCon,paste("SELECT * FROM",tableModel))
             #   tblStored$speed<-as.integer(tblStored$speed)
             #   if(!identical(tblStored,tbl)){
             #     tableModel=paste0(tableModel,'_',amSysTime('short'))
             #     dbWriteTable(dbCon,tableModel,tbl,overwrite=TRUE)
             #   }
             # }else{ 
                dbWriteTable(grassSession$dbCon,tableModel,tbl,overwrite=TRUE)
            #  }


              if(selectedAnalysis=='module_6'){
              # same for capacity template
            #  if(tableCapacityNew %in% amNameCheck(dataList,name=dataList$table,'table',dbCon=dbCon)){
            #    tblStored<-dbGetQuery(dbCon,paste("SELECT * FROM",tableCapacityNew))
            #    if(!identical(tblStored,tblCap)){
            #      tableCapacityNew=paste0(tableCapacityNew,'_',amSysTime('short'))
            #      dbWriteTable(dbCon,tableCapacityNew,tblCap,overwrite=TRUE)
            #    }
            #  }else{ 
                dbWriteTable(grassSession$dbCon,tableCapacityNew,tblCap,overwrite=TRUE)
            #  }
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
                      maxCost          = maxCost),
                    'isotropic'= amIsotropicTravelTime(
                      inputFriction    = mapFriction,
                      inputHf          = 'tmp_hf',
                      outputCumulative = mapCumulative,
                      maxCost          = maxCost
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
                    inputTblHf        = tblHfSubset,
                    inputZoneAdmin    = mapZoneAdmin,
                    outputPopResidual = mapPopResidual,
                    outputTblHf       = tableHfOut,
                    outputHfCatchment = hfCatchment,
                    removeCapted      = 'rmPop' %in% modParam,
                    vectCatch         = 'vectCatch' %in% modParam,
                    typeAnalysis      = typeAnalysis,
                    returnPath        = returnPath,
                    radius            = popBuffer,
                    maxCost           = maxCost,
                    hfIdx             = hfIdx,
                    capField          = capField,
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
                    inputTblHf     = tblHfSubset,
                    inputTblHfTo   = tblHfSubsetTo,
                    idField        = hfIdx,
                    labelField     = hfLab,
                    idFieldTo      = hfIdxTo,
                    labelFieldTo   = hfLabTo,
                    typeAnalysis   = typeAnalysis,
                    resol          = listen$mapMeta$grid$No,
                    dbCon          = grassSession$dbCon,
                    unitCost       = 'h',
                    unitDist       = 'km',
                    outReferral    = tableReferral,
                    outNearestDist = tableReferralNearestDist,
                    outNearestTime = tableReferralNearestTime
                    )

                  amUpdateProgressBar(session,"cumulative-progress",100)
                },
                'module_6'={
                  amScalingUp(
                    inputSpeed       = mapSpeed,
                    inputFriction    = mapFriction,
                    inputPop         = mapPopRes,
                    inputLandCover   = mapMerged,
                    inputHf          = mapHf,
                    inputTblHf       = tblHfSubset,
                    inputTblCap      = tblCap,
                    lcvClassToIgnore = lcvIgnoreClass,
                    maxCost          = maxCost,
                    minPrecedingCost = minPrecedingCost,
                    nFacilities      = nNewHf,
                    removePop        = rmPotentialPop,
                    maxProcessingTime= maxProcessingTime,
                    outputFacilities = mapNewHf,
                    outputTable      = tableScalingUp,
                    dbCon            = grassSession$dbCon
                    )
                } 

                )
              amUpdateDataList(listen)
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
            max=ceiling(cumCostStat[cumCostStat$V1=='max',]$V2/60),
            min=floor(cumCostStat[cumCostStat$V1=='min',]$V2/60),
            step=10
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
      mapZone<-amNameCheck(dataList,input$zoneSelect,'vector')
      mapHf<-amNameCheck(dataList,input$hfSelect,'vector')
      fieldZoneLabel<-input$zoneLabel
      fieldZoneId<-input$zoneId
      if(!is.null(mapZone) && 
        !is.null(mapHf) &&
        isTRUE(nchar(fieldZoneId)>0) &&
        isTRUE(nchar(fieldZoneLabel)>0) &&
        isTRUE(input$moduleSelector=='module_5')){
        isolate({
          # search admin zone category where all HF are located. 
  #        useCat<-unique(read.table(text=execGRASS('v.distance',
  #              from=mapHf,
  #              to=mapZone,
  #              dmax=listen$mapMeta$grid$No,
  #              upload='cat',
  #              flags='p',
  #              intern=T
  #              ),
  #            sep="|",
  #            header=T
  #            )[,2])

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
        })

      }
    })

    output$zoneCoverageTable<-renderHotable({
      btnZtt<-input$btnZoneTravelTime
      isolate({ 
      timeCumCost<-input$sliderTimeAnalysis*60
      zoneSelect<-amNameCheck(dataList,input$zoneSelect,'vector')
        if(timeCumCost>0 && !is.null(zoneSelect)){
          tmpZoneExists<-isTRUE('tmp__map_zone' == execGRASS('g.list',type='raster',pattern='tmp__map_zone',intern=T))
          if(tmpZoneExists){
            mapCumCost<-input$cumulativeCostMapSelect
            mapZone<-input$zoneSelect
            mapPop<-input$popSelect
            #mapHf<-input$hfSelect
            #catHf<-tblHfSubset()$cat
            #fieldCapacity<-input$hfCapacityField
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
              exp=paste('tmp__cum_cost_preview=if(',mapCumCost,'<',timeCumCost,',',mapCumCost,'/60,null())')
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
              labelat = c(timeCumCost/60)
              labeltext = paste("Travel time",timeCumCost/60,"[min]")

              ## plot
              output$previewTravelTime<-renderPlot({
                plot(rTt,col=heat.colors(timeCumCost/60),main=paste("Cumulated travel time at",timeCumCost/60,"minutes."))
              #  spplot(rTt,
              #    #c("random"),
              #    #col.regions = '#333333',
              #    col.regions = heat.colors(timeCumCost/60),
              #    scales=list(draw = TRUE),
              #    colorkey = list(
              #      space='left',
              #      width=1,
              #      height=0.1,
              #      labels=list(
              #       # at = labelat,
              #        labels = labeltext
              #        )
              #      ),
              #    par.settings = list(
              #      panel.background=list(col="grey")
              #      )
              #   )
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

            statZoneMerge<-merge(statZonePopTotal,statZonePopTravelTime,by=c('zone','label'))
            names(statZoneMerge)<-c(fieldZoneId,fieldZoneLabel,'popTotal','popTravelTime')

            statZoneMerge$popCoveredPercent<-(statZoneMerge$popTravelTime/statZoneMerge$popTotal)*100
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
#      nTbl<-names(tbl)[!names(tbl)=='cat'] # remove cat column
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

