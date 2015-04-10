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


# function extract field summary from SQLite table :
# - numeric fields,
# - character fields,
# - index candidate : (unique values & only character and integer & n=nRow )
# - uniques values by fields
# NOTE: instead of reading the whole table : loop on fields and use DISTINCT ?
amGetFieldsSummary<-function(table,dbCon,getUniqueVal=T){
  stopifnot(table %in% dbListTables(dbCon))
  tblSample<-dbGetQuery(dbCon,paste("SELECT * FROM",table,ifelse(getUniqueVal,"","LIMIT 1")))
  nR<-nrow(tblSample)
  idxCandidate<-sapply(tblSample,function(x){
    isTRUE(length(unique(x))==nR)
})
  if(getUniqueVal){
    uniqueVal<-sapply(tblSample,function(x){
      x=unique(x)
      sort(x)
})
  }else{
    uniqueVal=NULL
  }
  idxFields<-names(idxCandidate)[idxCandidate]
  
  numFields<-sapply(tblSample,function(x){
    isNum<-is.numeric(x) && !is.logical(x)
    if(isNum){
      !any(is.na(x) | "" %in% x)
    }else{
      FALSE
    }}) %>% 
  names(tblSample)[.]

 charFields<-sapply(tblSample,function(x){
    isChar<-is.character(x) && !is.logical(x)
    if(isChar){
      !any(is.na(x) | "" %in% x)
    }else{
      FALSE
    }}) %>% 
 names(tblSample)[.]

 list(
    num=numFields,
    char=charFields,
    idx=idxFields,
    val=uniqueVal
    )
}

system.time(any(c(rep(c("",'test','b'),10e6)) == ""))
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
observe({
  costTag<-input$costTag 
  if(isTRUE(nchar(costTag)>0)){
    updateTextInput(session,'costTag',value=amSubPunct(costTag,config$sepTagUi))
  }
})

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




amCreateHfTable<-function(mapHf,mapMerged,mapPop,dbCon){
  # mapHf : vector map of facilities
  # map merged : raster landcover merged map
  # mapPop : raster map of population
  # Return value :
  # Facilitie attribute table with additional columns :
  # amOnBarrier : check if facilities is located on barrier (no landcover value)
  # amCatLandCover : get value of merged land cover for each facilities.
  # amPopCell : count population in cells where facilities are located.
  if(!is.null(mapHf) && !is.null(mapMerged)){
    # check if HF are located on barrier by querying merged land cover values.
    tbl<-read.table(
      text=execGRASS("v.what.rast",map=mapHf,raster=mapMerged,flags='p',intern=T),
      sep="|",stringsAsFactors=F)
    names(tbl)<-c('cat','val')
    tbl$amCatLandCover<-ifelse(tbl$val=='*',NA,tbl$val)
    tbl$amOnBarrier<-ifelse(tbl$val=='*',TRUE,FALSE)
    tbl$val<-NULL
    # count population on facilities sites
    if(!is.null(mapPop)){
      pop<-read.table(
        text=execGRASS('v.what.rast',map=mapHf,raster=mapPop,flags='p',intern=T),
        sep="|",stringsAsFactors=F)  
      names(pop)<-c('cat','amPopCell')
      pop[pop$amPopCell=='*','amPopCell']<-0 
      pop$amPopCell<-as.numeric(pop$amPopCell)
      tbl<-merge(tbl,pop,by='cat')
    }
    # copy hf attribute table from SQLite db.
    tblAttribute<-dbGetQuery(dbCon,paste('select * from',mapHf))
    # merge accessmod table with attribute table
    tbl<-merge(tbl,tblAttribute,by='cat')
    return(tbl)
  }else{
    return(NULL)
  }

}
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





amMapPopOnBarrier<-function(inputPop,inputMerged,outputMap){
  expr<-sprintf("%s = if(!isnull(%s) && isnull(%s),%s,null())",outputMap,inputPop,inputMerged,inputPop)
  execGRASS('r.mapcalc',expression=expr,flags='overwrite')
}

amRmOverPassedTravelTime<-function(map,maxCost){
  # remove over passed values :
  # r.walk check for over passed value after last cumulative cost :
  # so if a new cost is added and the new mincost is one step further tan
  # the thresold, grass will keep it and stop algorithm from there.
  if(maxCost>0){
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
    #... corrsponding to the predefined value listTranspMod + given speed
    tbl[i,'newClass']<-as.integer(config$listTranspMod[[mod]]$rastVal)+as.integer(tbl[i,'speed'])
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

amIsotropicTravelTime<-function(inputFriction,inputHf,inputStop=NULL,outputDir=NULL,outputCumulative,maxCost){
  amDebugMsg('amIsotropicTravelTime')
  amParam=list(
    input=inputFriction,
    output=outputCumulative,
    start_points=inputHf,
    stop_points=inputStop,
    outdir=outputDir,
    max_cost=maxCost 
    )
  amParam<-amParam[!sapply(amParam,is.null)]
  execGRASS('r.cost',
    parameters=amParam,
    flags='overwrite'
    )
  amRmOverPassedTravelTime(outputCumulative,maxCost) 
}

amAnisotropicTravelTime<-function(inputSpeed,inputHf,inputStop=NULL,outputDir=NULL,outputCumulative, returnPath,maxCost){
  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
  flags<-flags[!flags %in% character(1)]
  amParam=list(
    elevation=config$mapDem,
    friction=inputSpeed,
    output=outputCumulative,
    start_points=inputHf,
    stop_points=inputStop,
    outdir=outputDir,
    memory=100,
    max_cost=maxCost # max cost in seconds.
    )
  amParam<-amParam[!sapply(amParam,is.null)]
  execGRASS('r.walk.accessmod',
    parameters=amParam,
    flags=flags
    ) 
  amRmOverPassedTravelTime(outputCumulative,maxCost) 
}

amCircularTravelDistance<-function(inputHf,outputBuffer,radius){
  execGRASS('v.to.rast',input=inputHf,output='tmp_buffer',use='val',value=1,flags='overwrite')
  execGRASS('r.buffer',input='tmp_buffer',output=outputBuffer,distances=radius, flags='overwrite')
  # create one unique zone.
  execGRASS('r.mapcalc',expression=paste(outputBuffer,'=if(!isnull(',outputBuffer,'),1,null())'),flags='overwrite')
}


amReferralTable<-function(inputSpeed,inputFriction,inputHf,inputHfTo,inputTblHf,inputTblHfTo,idField,idFieldTo,labelField,labelFieldTo,typeAnalysis,resol,dbCon, unitCost=c('s','m','h'),unitDist=c('m','km'),outReferral,outNearestDist,outNearestTime){

  #TODO: describe input and what is returned.

  # check the clock
  timeCheckAll<-system.time({
    # set increment for the progress bar.
    incN=0
    inc=90/nrow(inputTblHf)
    ## subset value for table formating.
    #labelFrom <- inputTblHf[[labelField]]
    #labelTo <- inputTblHfTo[[labelFieldTo]]
    #indexFrom <- inputTblHf[[idField]]
    #indexTo <- inputTblHfTo[[idFieldTo]]

    # set output table header label
    hIdField <- paste0('from','__',amSubPunct(idField)) # amSubPunt to avoid unwanted char (accent, ponctuation..)
    hLabelField <- paste0('from','__',amSubPunct(labelField))
    hIdFieldTo <- paste0('to','__',amSubPunct(idFieldTo))
    hLabelFieldTo <- paste0('to','__',amSubPunct(labelFieldTo))
    hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idFieldTo))
    hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelFieldTo))
    hDistUnit <-paste0('distance','_',unitDist)
    hTimeUnit <- paste0('time','_',unitCost)

    # Create destination HF subset (To). 
    # NOTE: this has already be done outside for other functions.. but for coherence with origin HF (From) map, which need to be subseted in the loop, we also subset destination HF here.
    qSqlTo<-paste("cat IN (",paste0(inputTblHfTo$cat,collapse=','),")")
    execGRASS("v.extract",flags=c('overwrite'),input=inputHfTo,where=qSqlTo,output='tmp_ref_to')
  # cost and dist from one to all selected in table 'to'
  for(i in inputTblHf$cat){  
    timeCheck<-system.time({
      incN=incN+1
      qSqlFrom<-paste("cat==",i)
      # create temporary origine facility map (from) 
      execGRASS("v.extract",flags=c('overwrite'),input=inputHf,where=qSqlFrom,output='tmp__ref_from')
      # NOTE: only extract coordinate instead ? No.. we need points in network. 
      # create cumulative cost map for each hf : iso or aniso
      switch(typeAnalysis,
        'anisotropic'=amAnisotropicTravelTime(
          inputSpeed=inputSpeed,
          inputHf='tmp__ref_from',
          inputStop='tmp_ref_to',
          outputCumulative='tmp__cost', 
          outputDir='tmp__ref_dir',
          returnPath=FALSE,
          maxCost=0
          ),
        'isotropic'=amIsotropicTravelTime(
          inputFriction=inputFriction,
          inputHf='tmp__ref_from',
          inputStop='tmp_ref_to',
          outputCumulative='tmp__cost',
          outputDir='tmp__ref_dir',
          maxCost=0
          )
        )
      # extract time cost V1 = hf cat dest; V2 = time to reach hf
      refTime=execGRASS(
        'v.what.rast',
        map='tmp_ref_to',
        raster='tmp__cost',
        flags='p',
        intern=T
        )%>%
      gsub('\\*',NA,.) %>%
      na.omit %>%
      read.table(text=.,sep='|')
      # rename grass output
      names(refTime)<-c('tcat',hTimeUnit)
      #unit transformation 
      if(!unitCost =='s'){
        div<-switch(unitCost,
          'm'=60,
          'h'=3600,
          'd'=86400
          )
        refTime[hTimeUnit]<-refTime[hTimeUnit]/div
      }
      refTime$cat=i
      # extract network to compute distance
      execGRASS('r.drain',
        input='tmp__cost',
        direction='tmp__ref_dir',
        output='tmp__drain',
        drain='tmp__drain',
        flags=c('overwrite','c','d'),
        start_points='tmp_ref_to'
        )
      # create new layer with start point as node
      execGRASS('v.net',
        input='tmp__drain',
        points='tmp__ref_from',
        output='tmp__net_from',
        node_layer='2',
        operation='connect',
        threshold=resol-1,
        flags='overwrite'
        )
      # create new layer with stop points as node
      execGRASS('v.net',
        input='tmp__net_from',
        points='tmp_ref_to',
        output='tmp__net_all',
        node_layer='3',
        operation='connect',
        threshold=resol-1,
        flags='overwrite'
        )
      # extrad distance for each end node.
      execGRASS('v.net.distance',
        input='tmp__net_all',
        output='tmp__net_dist',
        from_layer='3', # calc distance from all node in 3 to layer 2 (start point)     
        to_layer='2',
        intern=T,
        flags='overwrite'
        )
      # read attribute table of distance network.
      refDist<-dbReadTable(dbCon,'tmp__net_dist')
      # rename grass output
      names(refDist)<-c('tcat','cat',hDistUnit)
      # distance conversion
      if(!unitDist=='m'){
        div<-switch(unitDist,
          'km'=1000
          )
        refDist[hDistUnit]<-refDist[hDistUnit]/div
      }

      # using data.table. TODO: convert previouse data.frame to data.table.
      refTime<-as.data.table(refTime)
      setkey(refTime,cat,tcat)
      refDist<-as.data.table(refDist)
      setkey(refDist,cat,tcat)
      refTimeDist <- refDist[refTime]

      #create or update table
      if(incN==1){
        ref=refTimeDist
      }else{
        ref<-rbind(ref,refTimeDist)
      }
      # remove tmp map
      rmRastIfExists('tmp__*')
      rmVectIfExists('tmp__*')
    })
    amUpdateProgressBar(session,'cumulative-progress',inc*incN)
    print(timeCheck)
  }

# set key to ref
  setkey(ref,cat,tcat)

  # Remove tmp map
  rmVectIfExists('tmp_*')

  # mergin from hf subset table and renaming.
  valFrom<-inputTblHf[inputTblHf$cat %in% ref$cat, c('cat',idField,labelField)]
  names(valFrom)<-c('cat',hIdField,hLabelField)
  valFrom<-as.data.table(valFrom)
  setkey(valFrom,cat)

  valTo<-inputTblHfTo[inputTblHfTo$cat %in% ref$tcat,c('cat',idFieldTo,labelFieldTo)]
  names(valTo)<-c('tcat',hIdFieldTo,hLabelFieldTo)
  valTo<-as.data.table(valTo)
  setkey(valTo,'tcat')

  setkey(ref,cat)
  ref<-ref[valFrom]
  setkey(ref,tcat)
  ref<-ref[valTo]
  # set column subset and order
  refOut<-ref[,c(hIdField,hIdFieldTo,hDistUnit,hTimeUnit,hLabelField,hLabelFieldTo),with=F]

  # set expression to evaluate nested query by group
  expD<-parse(text=paste0(".SD[which.min(",hDistUnit,")]"))
  expT<-parse(text=paste0(".SD[which.min(",hTimeUnit,")]"))

  # Extract nearest feature by time and distance.
  refNearestDist<-refOut[,eval(expD),by=hIdField]
  refNearestTime<-refOut[,eval(expT),by=hIdField]

  })
 # Return meta data
  meta<-list(
    'Function'='amReferralTable',
    'AccessMod revision'=amGetVersionLocal(),
    'Date'=amSysTime(),
    'Timing'=as.list(timeCheckAll)$elapsed,
    'Iterations'=nrow(inputTblHf),
    'Arguments'=list(
      'input'=list(
        'map'=list(
          'cost'=list(
            'speed'=inputSpeed,
            'friction'=inputFriction
            ),
          'facilities'=list(
            'from'=inputHf,
            'to'=inputHfTo
            )
          ),
        'table'=list(
          'cat'=list(
            'from'=inputTblHf$cat,
            'to'=inputTblHfTo$cat
            ),
          'names'=list(
            'from'=names(inputTblHf),
            'to'=names(inputTblHfTo)
            )
          )
        ),
      'analysis'=typeAnalysis,
      'unit'=list(
        'distance'=unitDist,
        'cost'=unitCost
        ),
      'resol'=resol
      ),
    'Output'=list(
      outReferral,
      outNearestDist,
      outNearestTime
      ) 
    )

  dbWriteTable(dbCon,outReferral,refOut,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestDist,refNearestDist,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestTime,refNearestTime,overwrite=T,row.names=F)

 
}



amCapacityAnalysis<-function(inputSpeed,inputFriction,inputPop,inputHf,inputTblHf,inputZoneAdmin=NULL,outputPopResidual,outputTblHf,outputHfCatchment,removeCapted=FALSE,vectCatch=FALSE,typeAnalysis,returnPath,maxCost,radius,hfIdx,capField,zonalCoverage=FALSE,zoneFieldId=NULL,zoneFieldLabel=NULL,hfOrder=NULL,hfOrderSorting=NULL){
  # cat is used a key field in vector maps : set another name
  if(hfIdx=='cat'){
    hfIdxNew='cat_orig'
  }else{
    hfIdxNew=hfIdx
  }
  # nested call if requested order is not given by input hf table
  # hfOrder could be 'tableOrder','travelTime' or 'circlBuffer'
  # If hfOrder is not 'tableOrder' or 'circBuffer', an isotropic or anisotropic will be done.
  # In this case, typeAnalysis will be set from parent function call.
  if(!hfOrder == 'tableOrder' && ! is.null(hfOrder)){
    popWithinDist<-amCapacityAnalysis(
      inputSpeed=inputSpeed,
      inputFriction=inputFriction,
      inputPop=inputPop,
      inputHf=inputHf,
      inputTblHf=inputTblHf,
      outputPopResidual='tmp_nested_p',
      outputTblHf="tmp_nested_hf",
      outputHfCatchment="tmp_nested_catch",
      typeAnalysis=ifelse(hfOrder == 'circBuffer','circular',typeAnalysis),
      returnPath=returnPath,
      radius=radius,
      maxCost=maxCost,
      hfIdx=hfIdx,
      capField=capField,
      )[['capacityTable']][c(hfIdxNew,'amPopTimeMax')]
    hfOrderDecreasing<-ifelse(hfOrderSorting=='hfOrderDesc',TRUE,FALSE)
    orderId<-popWithinDist[order(
      popWithinDist$amPopTimeMax,decreasing=hfOrderDecreasing
      ),hfIdxNew]
    amMsg(session,'log',text=paste('Order process for',inputHf,'(',hfIdxNew,') will be',paste(orderId,collapse=',')))
  }else{
    orderId=unique(inputTblHf[,hfIdx])
  }
  # temporary maps
  tmpHf='tmp__h' # vector hf tmp
  tmpCost='tmp__c' # cumulative cost tmp
  tmpPop='tmp__p' # population catchment to substract

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
    qSql<-paste(hfIdx,"IN (",paste0("'",i,"'",collapse=','),")")
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
    hfCap<-sum(inputTblHf[inputTblHf[hfIdx]==i,capField])
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
    propToRemove = NULL

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
      # Calculate population residual
      # If hfCapResidual==0, HF can provide services exactly for the pop within this zone
      hfCapResidual=hfCap-max(zInner$cumSum)
      # inner ring vector of HF catchment if no hfCapResidual only. 
      if(vectCatch && hfCapResidual==0){
        tmpVectCatchOut<-amCatchPopToVect(
          idField=hfIdxNew,
          idPos=i,
          incPos=incN,
          tmpPop=tmpPop,
          dbCon=listen$dbCon 
          )
      }
    } # end inner ring

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
      # temp pop catchment where hf's cumulative cost map is lower (take inner cell) or equal to maxZone 
      execGRASS('r.mapcalc',
        expression=paste(tmpPop,'=if(',tmpCost,'<=',maxZone,',1,null())'),
        flags='overwrite')

      if(removeCapted){  
        # calc cell with new lowered values.
        execGRASS('r.mapcalc',
          expression=paste(
            'tmp__pop_residual',"=",outputPopResidual,'-',outputPopResidual,'*',tmpPop,'*',propToRemove
            ),
          flags="overwrite")
        # patch them with pop residual map
        execGRASS('r.patch',
          input=c('tmp__pop_residual',outputPopResidual),
          output=outputPopResidual,
          flags='overwrite')
      }

      if(vectCatch){
        tmpVectCatchOut<-amCatchPopToVect(
          idField=hfIdxNew,
          idPos=i,
          incPos=incN,
          tmpPop=tmpPop,
          dbCon=listen$dbCon 
          )
      }
    }# end outer ring
  # handle length == 0, for special case :
  # e.g. when no pop available in cell or in travel time extent
  if(length(zMaxInner)==0)zMaxInner=NA
  if(length(zMaxOuter)==0)zMaxOuter=NA
  if(length(propToRemove)==0)propToRemove=NA
  if(length(hfCapResidual)==0)hfCapResidual=NA
  if(length(totalPop)==0)totalPop=0
  if(length(firstCellPop)==0)firstCellPop=0
  # Output capacity table
  catDf=data.frame(
    as.integer(i), # id of hf / group of hf
    hfCap, # capacity from hf table
    hfCapResidual, # capacity not filled
    maxCost=maxCost,
    totalPop, # total population within max distance
    firstCellPop, # population under start cell
    zMaxInner, # maximum travel time for the inner ring. below this, we have covered all patient
    zMaxOuter, # maximum travel time for outer ring. below this, we have covered a fraction of patient,
    propToRemove
    )
  names(catDf)<-c(
    hfIdxNew,
    capField,
    'amCapacityResidual',
    'amTimeMax',
    'amPopTimeMax',
    'amPopFirstCell',
    'amTimeLimitInnerRing',
    'amTimeLimitOuterRing',
    'amPopPropRemovedOuterRing')

  if(nrow(catDf)==0)browser()
  tblOut<-rbind(tblOut,catDf)
  progValue<-inc*incN+10
  amDebugMsg('Progress=',progValue,'inc=',inc,'incN=',incN)
  amUpdateProgressBar(session,"cumulative-progress",round(inc*incN)+10)
  rmRastIfExists('tmp__*')
  rmVectIfExists('tmp__*')

  }# end of hf loop


  tblPopByZone=NULL
  if(zonalCoverage){
    execGRASS('v.to.rast',
      input=inputZoneAdmin,
      output='tmp_zone_admin',
      type='area',
      use='attr',
      attribute_column=zoneFieldId,
      label_column=zoneFieldLabel,
      flags=c('overwrite'))


    tblAllPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags=c('g','t','overwrite'),
        map=inputPop, 
        zones='tmp_zone_admin', #
        intern=T
        ),sep='|',header=T)[,c('zone','label','sum')]

    tblResidualPopByZone<-read.table(
      text=execGRASS(
        'r.univar',
        flags=c('g','t','overwrite'),
        map=outputPopResidual, 
        zones='tmp_zone_admin', # 
        intern=T
        ),sep='|',header=T)[,c('zone','label','sum')]

    tblPopByZone<-merge(tblResidualPopByZone,tblAllPopByZone,by=c('zone','label'))

    tblPopByZone$covered<-tblPopByZone$sum.y - tblPopByZone$sum.x
    tblPopByZone$percent<- (tblPopByZone$covered / tblPopByZone$sum.y) *100
    tblPopByZone$sum.x=NULL
    names(tblPopByZone)<-c(zoneFieldId,zoneFieldLabel,'amPopSum','amPopCovered','amPopCoveredPercent')

  }
  if(vectCatch){
    # get catchment shapefile back and clean columns
    execGRASS('v.in.ogr',
      input=tmpVectCatchOut,
      output=outputHfCatchment,
      flags=c('overwrite','c'),
      snap=1,
      columns='cat'
      )
    execGRASS(
      'v.db.dropcolumn',
      map=outputHfCatchment,
      columns=c('cat_')
      )
  }

  if(!removeCapted)rmRastIfExists(outputPopResidual)

  # remove remaining tmp file (1 dash)
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')

  return(
    list(
      capacityTable=tblOut,
      zonalTable=tblPopByZone
      )
    )
}



# function to handle vector output of catchment
amCatchPopToVect<-function(idField,idPos,incPos,tmpPop,dbCon){
  # idField : HF id name used in for loop
  # idPos : which id element is currently processed
  # incPos : numeric increment position.
  # tmpPop : population catchment
  # dbCon : RSQlite connection  
  # NOTE: output catchment as vector, merged and easily readable by other GIS.
  # None of those methods worked at the time this script was written :
  # v.overlay :  geometry / topology ok, seems the way to go ! But... how to handle hundred of overlays ? 
  #              And grass doesn't like to work with non topological 'stacked' data. 
  # v.patch : produced empty area and topologic errors, even without topology building (b flag)
  # v.to.3d with groupId as height and v.patch after. V.patch transform back to 2d... with area errors.
  # r.to.rast3 :groupId as Z. doesn't produce anything + 3d interface really bugged. 
  # So, export and append to shapefile, reimport back after the loop. eerk.
  tDir<-tempdir()
  outCatch='tmp__vect_catch'
  tmpVectCatchOut=file.path(tDir,paste0(outCatch,'.shp'))
  execGRASS('r.to.vect',
    input=tmpPop,
    output=outCatch,
    type='area',
    flags=c('overwrite','v'),
    column=idField)
  # for the first catchment : overwrite if exists, else append.
  if(incPos==1){
    if(file.exists(tmpVectCatchOut)){ 
      file.remove(tmpVectCatchOut)
    }
    outFlags=c('overwrite')
  }else{
    outFlags=c('a')
  }
  # update attribute table with actual ID.
  dbRec<-dbGetQuery(dbCon,paste('select * from',outCatch))
  dbRec[,idField]<-as.integer(idPos)
  dbRec[,'label']<-NULL
  dbWriteTable(dbCon,outCatch,dbRec,overwrite=T)
  # export to shapefile. Append if incPos > 1
  execGRASS('v.out.ogr',
    input=outCatch,
    output=tmpVectCatchOut,
    format='ESRI_Shapefile',
    flags=outFlags,
    output_layer=outCatch)
  return(tmpVectCatchOut)
}



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
