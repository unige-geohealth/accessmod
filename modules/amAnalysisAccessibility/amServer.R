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

    # populate select input
    observe({
      mergedList<-grep('^merged__',dataList$raster,value=T)
      if(length(mergedList)==0)mergedList=character(1)
      updateSelectInput(session,'mergedSelect',choices=mergedList,selected=mergedList[1])
    })
    observe({
      hfList<-grep('^health_facilities__',dataList$vector,value=T)
      if(length(hfList)==0)hfList=character(1)
      updateSelectInput(session,'hfSelect',choices=hfList,selected=hfList[1])
      updateSelectInput(session,'hfSelectTo',choices=hfList,selected=hfList[1])
    })
    observe({
      modelList<-grep('^table_model__*',dataList$table,value=T)
      if(length(modelList)==0)modelList=character(1)
      updateSelectInput(session,'modelSelect',choices=modelList,selected=modelList[1])
    })
    observe({
      popList<-grep('^population__*',dataList$raster,value=T)
      if(length(popList)==0)popList=character(1)
      updateSelectInput(session,'popSelect',choices=popList,selected=popList[1])
    })
    observe({
      cumCostList<-grep('^cumulative_cost__*',dataList$raster,value=T)
      if(length(cumCostList)==0)cumCostList=character(1)
      updateSelectInput(session,'cumulativeCostMapSelect',choices=cumCostList,selected=cumCostList[1])
    })
    observe({
      zoneList<-grep('^zone_admin__*',dataList$vector,value=T)
      if(length(zoneList)==0)zoneList=character(1)
      updateSelectInput(session,'zoneSelect',choices=zoneList,selected=zoneList[1])
    })




    #
    # Zonal stat :fields from zonal vector map
    #

    # get fields summary reactive list
    zoneFields<-reactive({
      zoneSel<-amNameCheck(input$zoneSelect,'vector')
      # get field summary 
      isolate({
        if(length(zoneSel)>0){
          zoneFieldsSummary<-amGetFieldsSummary(dbCon=listen$dbCon,zoneSel,getUniqueVal=F)
        }else{
          zoneFieldsSummary=list()
        }
        return(zoneFieldsSummary)
      })
    })


    # get zone attribute table fields summary (num,char,idx candidate,val unique)
    observe({
      zoneFieldIdx<-zoneFields()$idx
      zoneFieldIdx<-zoneFieldIdx[zoneFieldIdx %in% zoneFields()$num]
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
      selHfFrom<-amNameCheck(input$hfSelect,'vector')
      # get field summary 
      isolate({
        if(length(selHfFrom)>0){
          hfFrom<-amGetFieldsSummary(dbCon=listen$dbCon,selHfFrom)
        }else{
          hfFrom=list()
        }
        return(hfFrom)
      })
    })
    # get hf (to) attribute table fields summary (num,char,idx candidate,val unique)
    hfFieldsTo<-reactive({
      isModReferral<-isTRUE(input$moduleSelector=='module_4')
      selHfTo<-amNameCheck(input$hfSelectTo, 'vector')
      selHfFrom<-amNameCheck(input$hfSelect,'vector')
      if(!is.null(selHfTo)){
        if(selHfFrom==selHfTo)return(hfFields())
        # get field summary 
        isolate({
          if(length(selHfTo) &&isModReferral)return(amGetFieldsSummary(dbCon=listen$dbCon,selHfto))
      })}
      list()
    })

    # update hf field selection
    observe({
      hfTo<-isTRUE(input$selHfFromTo=='To' && input$moduleSelector=='module_4')
      if(isTRUE(input$hfDisplaySelect)){
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
      if(input$moduleSelector=='module_3'){
        pop<-amNameCheck(input$popSelect,'raster')
        merged<-amNameCheck(input$mergedSelect,'raster')
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
      }
      return(list())
    })


    #
    # General validation and error message
    #

    # preventive field validation
    observe({
      amErrorAction(title='Module 2,3,4: validation',{
        #empty msg container
        err = character(0)
        info = character(0)
        # check current module
        module2<-isTRUE(input$moduleSelector =='module_2')
        module3<-isTRUE(input$moduleSelector == 'module_3')
        module4<-isTRUE(input$moduleSelector =='module_4')
        # map validation for all modules
        merged<-isTRUE(!is.null(amNameCheck(input$mergedSelect,'raster')))
        hf<-isTRUE(!is.null(amNameCheck(input$hfSelect,'vector')))
        pop<-isTRUE(!is.null(amNameCheck(input$popSelect,'raster'))) 
        # table validation
        #tblHf<-any(hot.to.df(input$hfTable)$select) ## if many columns or rows, to slow!
        hfOnBarrier<-isTRUE(
          any(tblHfSubset()$amOnBarrier=='yes') ||
          any(tblHfSubsetTo()$amOnBarrier=='yes') 
          )
        # check if there is at least one hospital selectect.
        hfNoHf<-isTRUE(!any(tblHfSubset()$amSelect))
        hfNoHfTo<-isTRUE(!any(tblHfSubsetTo()$amSelect))
        # check for speed of  0 kmh 
        tblModel<-isTRUE(!any(hot.to.df(input$speedRasterTable)$speed <1))
        # parameter validation
        costTag<-input$costTag
        tag<-isTRUE(nchar(costTag)>0)
        maxTT<-isTRUE(input$maxTimeWalk == 0)
        # population on barrier
        popBarrierSum<-popOnBarrierStat()$sum
        popBarrierCells<-popOnBarrierStat()$cells
        popBarrierPercent<-popOnBarrierStat()$percent

        if(module2){
          # map overwrite warning module 2
          costTag<-unlist(costTag)
          cumulativeName<-paste(c('cumulative_cost',paste(costTag,collapse=config$sepTagFile)),collapse=config$sepClass )
          cumulativeCostExists <-isTRUE(cumulativeName %in% amNameCheck(isolate(dataList$raster),'raster'))
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
            zonalSelect<-isTRUE(!is.null(amNameCheck(input$zoneSelect,'vector')))
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
          if(cumulativeCostExists) info = c(info,paste('Map',cumulativeName,'exists and will be overwritten, along with corresponding new facilities map, speed map or friction map. However, if changes has been made to the homonymic model (speed table), a new table will be stored with a distinctive tag.'))
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
        amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=disBtn)
            })
    })


    # extract category from merged landcover raster and add new column.
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
          names(tbl)<-config$tableColNames[['table_model']] 
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
          names(tbl)<-config$tableColNames[['table_model']] 
        }
        output$speedSqliteTable<-renderHotable({
          tbl
        },readOnly=TRUE,fixed=2,stretch='last')
      })
    })




    # create facilitie table with additional aaccesdmod column
    tblHfOrig<-reactive({
      selHf<-amNameCheck(input$hfSelect,'vector')
      selMerged<-amNameCheck(input$mergedSelect,'raster')
      selPop<-amNameCheck(input$popSelect,'raster')
      isolate({
        amCreateHfTable(
          mapHf=selHf,
          mapMerged=selMerged,
          mapPop=selPop,
          dbCon=listen$dbCon
          )
      })
    })

    #create facilitie table for second table. 
    tblHfOrigTo<-reactive({
      selHf<-amNameCheck(input$hfSelect,'vector')
      selHfTo<-amNameCheck(input$hfSelectTo,'vector')
      selMerged<-amNameCheck(input$mergedSelect,'raster')
      selPop<-amNameCheck(input$popSelect,'raster')
      if(input$moduleSelector=='module_4'){
        if(selHf==selHfTo && isTRUE(nrow(tblHfOrig())>0))return(tblHfOrig())
        isolate({
          amCreateHfTable(
            mapHf=selHfTo,
            mapMerged=selMerged,
            mapPop=selPop,
            dbCon=listen$dbCon
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
              },readOnly=TRUE,fixed=4,stretch='last')
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
          },readOnly=TRUE,fixed=4,stretch='last')
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
          },readOnly=TRUE,fixed=4,stretch='last')
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
          },readOnly=TRUE,fixed=4,stretch='last')
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
            mTest<- m %in% names(config$listTranspMod)
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


    # disable button 'createTimeCostMap'  each time it's activated
    observe({
      btn<-input$btnCreateTimeCostMap
      if(!is.null(btn)&&btn>0){
        amActionButtonToggle(session=session,'btnCreateTimeCostMap',disable=TRUE)
      }
    })


    # main function 
    observe({
      timeCheck<-system.time({
        btn<-input$btnCreateTimeCostMap # only reactive dependencie on create travel time button.
        if(!is.null(btn) && btn>0){
          isolate({
            # tables and reactive subset.
            tbl<-hot.to.df(input$speedRasterTable)
            tblHfSubset<-tblHfSubset()
            if(input$moduleSelector=='module_4'){ 
              tblHfSubsetTo<-tblHfSubsetTo()
            }

            # tags
            costTag<-input$costTag 

            # maps
            mapMerged<-amNameCheck(input$mergedSelect,'raster')
            mapHf<-amNameCheck(input$hfSelect,'vector')
            mapHfTo<-amNameCheck(input$hfSelectTo,'vector')
            mapPop<-amNameCheck(input$popSelect,'raster')
            mapZoneAdmin=amNameCheck(input$zoneSelect,'vector')

            # field selection
            hfIdx<-input$hfIdxField
            hfLab<-input$hfNameFieldTo
            hfIdxTo<-input$hfIdxFieldTo
            hfLabTo<-input$hfNameFieldTo
            zoneFieldLabel=input$zoneLabel
            zoneFieldId=input$zoneId
            capField<-input$hfCapacityField

            # parameters
            maxTimeWalk<-input$maxTimeWalk
            dirAnalysis<-input$dirAnalysis
            typeAnalysis<-input$typeAnalysis 
            capAnalysis<-input$moduleSelector # module_3 or module_2
            hfOrder<-input$hfOrder
            hfOrderSorting<-input$hfOrderSorting
            popBuffer<-input$popBufferRadius
            modParam<-input$mod3param

            # clean and transform.
            zonalCoverage='zonalCoverage' %in% input$zonalPopOption
            # return path = towards facilities.
            returnPath<-ifelse(dirAnalysis=='toHf',TRUE,FALSE)
            # max cost from minutes to seconds
            maxCost<-maxTimeWalk*60

            # map name formating
            tags<-unlist(strsplit(costTag,config$sepTagUi,fixed=T))
            # function to add and format tags for output dataset
            addTag<-function(base,tag=tags,sepT=config$sepTagFile,sepC=config$sepClass){
              paste(c(base,paste(tag,collapse=config$sepTagFile)),collapse=config$sepClass)
            }
            # set names
            mapSpeed<-addTag('speed')
            mapFriction<-addTag('friction')
            mapCumulative<-addTag('cumulative_cost')
            mapPopResidual<-addTag('population_residual')
            hfCatchment<-addTag('health_facilities_catchment')
            mapPopOnBarrier<-addTag('population_on_barrier')
            tableModel<-addTag('table_model')
            mapPopOnBarrier<-addTag('population_on_barrier')
            tableCapacityOut<-addTag('table_capacity')
            tableZonalOut<-addTag('table_zonal_coverage')
            tableReferral <- addTag('table_referral')
            tableReferralNearestDist <-addTag('table_referral_nearest_by_dist')
            tableReferralNearestTime <-addTag('table_referral_nearest_by_time')




            # start process
            message(paste(typeAnalysis,'analysis in ',input$moduleSelector,'requested'))
            amUpdateProgressBar(session,"cumulative-progress",5)
            # keep record of error, redict or set priority according to config$msgListError in config. 
            amErrorAction(title=paste(input$moduleSelector,' cumulative cost'),{ 

              # Test if a table with same name exists. 
              # a.If their content are identical, do nothing. 
              # b.If their content differ add a short time stamp to the name
              # c.if the table doesnt exists, save it.
              # TODO: find a way to avoid enormous amount of duplicate with different names?
              if(tableModel %in% amNameCheck(dataList$table,'table')){
                tblStored<-dbGetQuery(listen$dbCon,paste("SELECT * FROM",tableModel))
                if(!identical(tblStored,tbl)){
                  tableModel=paste0(tableModel,'_',amSysTime('short'))
                  dbWriteTable(listen$dbCon,tableModel,tbl,overwrite=TRUE)
                }
              }else{ 
                dbWriteTable(listen$dbCon,tableModel,tbl,overwrite=TRUE)
              }
              # TODO: check if this is not duplicated inside function!
              # create HF vector map  subset
              qSql<-paste("cat IN (",paste0("'",tblHfSubset$cat,"'",collapse=','),")")
              execGRASS("v.extract",flags='overwrite',input=mapHf,where=qSql,output='tmp_hf')

              if(input$moduleSelector=='module_4'){
                # create HF vector map subset
                qSql<-paste("cat IN (",paste0("'",tblHfSubsetTo$cat,"'",collapse=','),")")
                execGRASS("v.extract",flags='overwrite',input=mapHfTo,where=qSql,output='tmp_hf_to')
              }

              # create base map for travel time computation.
              # NOTE: compute both to allow user to run external analysis ?
              amCreateSpeedMap(tbl,mapMerged,mapSpeed)
              amCreateFrictionMap(tbl,mapMerged,mapFriction,mapResol=listen$mapMeta$grid$North)
              # if(typeAnalysis == 'anisotropic'){
              # }else{      
              # }

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
                      inputHf='tmp_hf',
                      outputCumulative=mapCumulative,
                      returnPath=returnPath,
                      maxCost=maxCost),
                    'isotropic'= amIsotropicTravelTime(
                      inputFriction=mapFriction,
                      inputHf='tmp_hf',
                      outputCumulative=mapCumulative,
                      maxCost=maxCost
                      ),
                    error(paste(typeAnalysis,'analysis not implemented'))
                    )
                  amUpdateProgressBar(session,"cumulative-progress",100)
                },
                'module_3'={
                  #if('popBarrier' %in% modParam){
                  # steeve recomendation : export pop on barrier by default.
                  if(TRUE){ 
                    amMapPopOnBarrier(
                      inputPop=mapPop,
                      inputMerged=mapMerged,
                      outputMap=mapPopOnBarrier
                      )
                  }
                  tblOut<-amCapacityAnalysis(
                    inputSpeed=mapSpeed,
                    inputFriction=mapFriction,
                    inputPop=mapPop,
                    inputHf=mapHf,
                    inputTblHf=tblHfSubset,
                    inputZoneAdmin=mapZoneAdmin,
                    outputPopResidual=mapPopResidual,
                    outputTblHf=tableHfOut,
                    outputHfCatchment=hfCatchment,
                    removeCapted='rmPop' %in% modParam,
                    vectCatch='vectCatch' %in% modParam,
                    typeAnalysis=typeAnalysis,
                    returnPath=returnPath,
                    radius=popBuffer,
                    maxCost=maxCost,
                    hfIdx=hfIdx,
                    capField=capField,
                    zonalCoverage=zonalCoverage,
                    zoneFieldId=zoneFieldId,
                    zoneFieldLabel=zoneFieldLabel,
                    hfOrder=hfOrder,
                    hfOrderSorting=hfOrderSorting
                    )
                  # write result in sqlite 
                  dbWriteTable(listen$dbCon,tableCapacityOut,tblOut[['capacityTable']],overwrite=T)
                  if(!is.null(tblOut$zonalTable)){
                    dbWriteTable(listen$dbCon,tableZonalOut,tblOut[['zonalTable']],overwrite=T)
                  }
                },
                'module_4'={
                  listTableReferral<-amReferralTable(
                    inputSpeed=mapSpeed,
                    inputFriction=mapFriction,
                    inputHf=mapHf,
                    inputHfTo=mapHfTo,
                    inputTblHf=tblHfSubset,
                    inputTblHfTo=tblHfSubsetTo,
                    idField=hfIdx,
                    labelField=hfLab,
                    idFieldTo=hfIdxTo,
                    labelFieldTo=hfLabTo,
                    typeAnalysis=typeAnalysis,
                    resol=listen$mapMeta$grid$No,
                    dbCon=listen$dbCon,
                    unitCost='h',
                    unitDist='km',
                    outReferral=tableReferral,
                    outNearestDist=tableReferralNearestDist,
                    outNearestTime=tableReferralNearestTime
                    )

                  amUpdateProgressBar(session,"cumulative-progress",100)
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
      cumCostSelect<-amNameCheck(input$cumulativeCostMapSelect,'raster')
      isolate({
        if(length(cumCostSelect)>0){
          cumCostStat<-read.table(
            text=execGRASS('r.univar',map=cumCostSelect,intern=T,flags='g'),
            sep='='
            )
          updateSliderInput(session,'sliderTimeAnalysis',
            max=ceiling(cumCostStat[cumCostStat$V1=='max',]$V2),
            min=floor(cumCostStat[cumCostStat$V1=='min',]$V2),
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
      mapZone<-amNameCheck(input$zoneSelect,'vector')
      mapHf<-amNameCheck(input$hfSelect,'vector')
      fieldZoneLabel<-input$zoneLabel
      fieldZoneId<-input$zoneId
      if(!is.null(mapZone) && 
        !is.null(mapHf) &&
        nchar(fieldZoneId)>0 &&
        nchar(fieldZoneLabel)>0 &&
        input$moduleSelector=='module_5' ){
        isolate({
          # search admin zone category where all HF are located. 
          useCat<-unique(read.table(text=execGRASS('v.distance',
                from=mapHf,
                to=mapZone,
                dmax=listen$mapMeta$grid$No,
                upload='cat',
                flags='p',
                intern=T
                ),
              sep="|",
              header=T
              )[,2])

          # Create raster version of admin zone. 
          execGRASS('v.to.rast',
            input=mapZone,
            cats=paste(useCat,collapse=','),
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
      timeCumCost<-input$sliderTimeAnalysis
      zoneSelect<-amNameCheck(input$zoneSelect,'vector')
      isolate({ 
        if(timeCumCost>0 && !is.null(zoneSelect)){
          tmpZoneExists<-'tmp__map_zone' == execGRASS('g.list',type='raster',pattern='tmp__map_zone',intern=T)
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
              execGRASS('g.region',res=paste(res*nCols/300))
              execGRASS('r.out.gdal',
                flags =c('overwrite','f'),
                input='tmp__cum_cost',
                output=tmpMapTt,
                format="GTiff",
                createopt='TFW=YES'
                )
              execGRASS('g.region',raster=config$mapDem)
              rTt<-raster(tmpMapTt)

              ## labels
              labelat = c(timeCumCost)
              labeltext = paste("Travel time",timeCumCost,"[s]")

              ## plot
              output$previewTravelTime<-renderPlot({
                spplot(rTt,
                  #c("random"),
                  col.regions = '#333333',
                  scales=list(draw = TRUE),
                  colorkey = list(
                    space='bottom',
                    width=0.3,
                    height=0.1,
                    labels=list(
                      at = labelat,
                      labels = labeltext
                      )
                    )
                  )})
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
        return(data.frame(zone='noData'))
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
#        dbRec<-dbGetQuery(listen$dbCon,'select * from tmp__vect_catch')
#        dbRec[,hfIdxNew]<-as.integer(i)
#        dbRec[,'label']<-NULL
#        dbWriteTable(listen$dbCon,'tmp__vect_catch',dbRec,overwrite=T)
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
#  selHf<-amNameCheck(input$hfSelect,'vector')
#  selMerged<-amNameCheck(input$mergedSelect,'raster')
#  selPop<-amNameCheck(input$popSelect,'raster')
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
#      tblAttribute<-dbGetQuery(listen$dbCon,paste('select * from',selHf))
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

