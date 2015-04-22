#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing landcover in a given order.
#
# input : road, barrier, landcover
# output : merged landcover



observe({
  amModEnabled<-listen$tabControl_module_selector
  if(!is.null(amModEnabled) && amModEnabled){
    #------------------------------------------------------------------------------#

    # MERGING NEW LANDCOVER

    #------------------------------------------------------------------------------#

    # populate selectize input

    observe({
      mapStack<-dataList$raster[grep('^stack_',dataList$raster)]
      updateSelectInput(session,'mapStack',choices=mapStack,selected=mapStack)
    })


    # btn merge toggle
    observe({
      mS<-amNameCheck(input$mapStack,'raster')
      sT<-input$stackTag
      disableBtn=any(is.null(mS), mS=='', is.null(sT), nchar(sT)<1)
      amActionButtonToggle(id='btnMerge',session,disable=disableBtn)
    })

    # tag validation
#    observe({
#      stackTag<-input$stackTag
#      if(!is.null(stackTag) && ! stackTag==''){
#        stackTag<-unlist(stackTag)
#        updateTextInput(session,'stackTag',value=amSubPunct(stackTag,config$sepTagUi))
#      }
#    })
    
    observe({
      stackTag<-input$stackTag
      rmArtefact<-input$cleanArtefact
      if(!is.null(rmArtefact) && !is.null(stackTag) && nchar(stackTag)>0){
        stackTag<-amSubPunct(stackTag,config$sepTagFile,rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T)
        merged<-paste('merged',stackTag,sep=config$sepClass)
        bridges<-paste('merged_bridge',stackTag,sep=config$sepClass)    
        outMap<-tagList(
          p(icon('info-circle'),tags$b(paste('Merged map:',merged))),
          p(icon('info-circle'),tags$b(paste('Artefacts/Bridges:',bridges)))
          )
        if(!rmArtefact)outMap=outMap[1]
        output$stackNameInfo<-renderUI(outMap)
      }
    })

    # button to hide stack items
    observe({
      rmMerge<-input$btnRmMerge
      if(!is.null(rmMerge)&& rmMerge>0){
        updateSelectInput(session = session, inputId = "mapStack",selected='')
      }
    })

    observe({
      addMerge<-input$btnAddMerge   
      if(!is.null(addMerge)&& addMerge>0){
        mapStack<-isolate(dataList$stack)
        updateSelectInput(session = session, inputId = "mapStack",selected=mapStack)
      }
    })


    stackConflictTable<-reactive({
      amErrorAction(title='stack conflict table',{
        sel<-amNameCheck(dataList$raster[grep('^stack*',dataList$raster)],'raster')
        if(!is.null(sel)){
          sel<-sel[-grep('^stack_barrier*',sel)]
          tbl<-data.frame(map=as.character(NA),class=as.integer(NA),label=as.character(NA))
          for(m in sel){
            t<-read.table(text=execGRASS('r.category',map=m,intern=T),
              sep="\t",
              stringsAsFactors=F
              )
            t$map=m
            names(t)<-c('class','label','map')
            tbl=rbind(tbl,t)
          }
          tbl[duplicated(tbl$class) | duplicated(tbl$class,fromLast=T),]
        }
})
    })

    observe({
      tbl<-stackConflictTable()
      amErrorAction(title='stack conflict read',{
        if(!is.null(tbl)){
          isolate({
            nRowConflict <- nrow(tbl)
            # test if nrow >0 and send a message to UI in case of conflict
            if(nRowConflict>0){
              msgConflict<-p(
                icon('exclamation-triangle'),
                paste(nRowConflict,'conflicts of class found. See in stack conflict table.')
                )
            }else{
              msgConflict=""    
              tbl<-data.frame(map=as.character(NA),class=as.integer(NA),label=as.character(NA))
            }

            output$stackWarning<-renderUI({msgConflict})
            # render hotable with a possibly empty table
            output$stackConflict<-renderHotable({tbl},
              stretched='last',readOnly=T
              )

          })
        }
})
    })




    #  merge action
    observe({
      btnMerge<-input$btnMerge
      timeCheck<-system.time({
        isolate({

          stackTag<-input$stackTag
          sel<-amNameCheck(input$mapStack,'raster')
          if(!is.null(btnMerge) && btnMerge > 0 && !is.null(sel) && isTRUE(nchar(stackTag)>0)){
            amErrorAction(title='Module 1: merge process',{        
              amActionButtonToggle(id='btnMerge',session,disable=TRUE)
              updateTextInput(session,'stackTag',value="")
              selL<-length(sel)
              cleanBridge<-input$cleanArtefact
              inc<-1/(selL+1)*100
              message('Merging landcover map requested.')
              stackTag<-amSubPunct(stackTag,config$sepTagFile,rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T)
              merged<-paste('merged',stackTag,sep=config$sepClass)
              bridges<-paste('merged_bridge',stackTag,sep=config$sepClass)  
              
              mapPosition=1
              tempBase<-'tmp__' 
              isFirstMap=TRUE
              rmRastIfExists('tmp_*')
              if(amRastExists('MASK'))execGRASS('r.mask',flags='r')

              message(paste('stack will be merged in this order:',paste(sel,collapse=', ')))
              amUpdateProgressBar(session,"stackProgress",1)

              for(i in 1:length(sel)){
                map<-sel[i]
                message(paste('Proceding map',map,'MASK is',amRastExists('MASK')))
                if(length(grep('stack_barrier__', map))>0){
                  if(amRastExists('MASK')){
                    execGRASS('r.mapcalc',expression=paste("MASK=isnull(",map,")?MASK:null()"),flags="overwrite")
                  }else{
                    execGRASS('r.mask',raster=map,flags=c('i'))
                  }
                }else{
                  tempMap=paste0(tempBase,mapPosition,'_',map)
                  execGRASS('r.mapcalc',expression=paste(tempMap,"=",map),flags='overwrite')
                }
                mapPosition=mapPosition+1
                amUpdateProgressBar(session,'stackProgress',i*inc)
              }
              #removing temp mask and active mask
              rmRastIfExists('tmp_mask__*')
              if(amRastExists('MASK'))execGRASS('r.mask',flags='r')
              # get list of tmp__stack... maps.
              tempMapList<-execGRASS('g.list',type='raster',pattern=paste0(tempBase,'*'),intern=TRUE)
              if(length(tempMapList)>1){  
                execGRASS('r.patch',input=paste(tempMapList,collapse=','),output=merged,flags=c('overwrite'))
              }else{
                execGRASS('g.copy',raster=paste0(tempMapList,',',merged),flags='overwrite')
              }

              # In accessmod accessibility analysis, a null cell is a barrier, e.g. a river, mountain, militarized zone.
              # When we patch road maps to landcover maps, small overlaps can appear on top of predefined barrier.
              # Those overlaps act as briges when used in travel time analyis, thus, create shortcuts and wrong calculation.
              # If we used densified lines during rasterization process of roads, we can safely set the "one cell diagonal
              # bridge" as barrier without breaking road continuity. 
              # 
              # Example:
              # X=non-null cell in <road_map>; N=null in <merged_map>; A=non-null cell in <merged_map> 
              # X will be set as null in fallowing cases:
              #
              # X N
              # N A
              #
              # N X
              # A N
              #browser()
              if(cleanBridge){
                message('Cleaning artefact/bridges of one sel')
                fromRoad<-sel[grep('stack_road',sel)]
                amBridgeFinder(fromRoad,merged,bridges)
                amBridgeRemover(bridges,removeFromMap=merged)  
              }

              execGRASS('r.colors',map=merged,color='random')
              rmRastIfExists(paste0(tempBase,'*'))
              message(paste(merged,'created'))
              amUpdateProgressBar(session,'stackProgress',100)
              amUpdateDataList(listen)
              }) 
          }
        })
      })
      print(timeCheck)
    })


    #------------------------------------------------------------------------------#

    # ADD TO STACK LANDCOVER 

    #------------------------------------------------------------------------------#

    observe({
      lcv<-dataList$raster[grep('^land_cover__',dataList$raster)]
      lcvTable<-dataList$table[grep('^table_land_cover__',dataList$table)]
      if(length(lcv)<1)lcv=""
      if(length(lcvTable)<1)lcvTable=""
      updateSelectInput(session,'landCoverSelect',choices=lcv)
      updateSelectInput(session,'landCoverSelectTable',choices=lcvTable)
    })



    # toggle buttons to merge lcv table and add to stack
    observe({
      lS<-amNameCheck(input$landCoverSelect,'raster')
      lT<-amNameCheck(input$landCoverSelectTable,'table')
      lab<-hot.to.df(input$landCoverRasterTable)$label
      disableMerge=any(is.null(lS),lS=='',is.null(lT),lT=="")
      disableStack=any(is.null(lS),lS=='',is.null(lab),"" %in% lab,NA %in% lab)
      amActionButtonToggle(id='btnAddStackLcv',session,disable=disableStack)
      #amActionButtonToggle(id='mergeLcvUndo',session,disable=!allow)
      amActionButtonToggle(id='mergeLcv',session,disable=disableMerge)
    },label='observeBtnsLcv')



    observe({
      tblUpdated<-hot.to.df(input$landCoverRasterTable)
      isolate({
        tblOriginal<-isolate(landCoverRasterTable())
        testNrow<-nrow(tblUpdated)==nrow(tblOriginal)
        # rule 1 : if nrow doesnt match, return original
        if(!is.null(tblUpdated) && !is.null(tblOriginal) && testNrow){
          # rule 2: do not allow changing class
          tblValidated<-data.frame(class=tblOriginal$class,label=amSubPunct(tblUpdated$label,'_'))
        }else{
          tblValidated<-tblOriginal
        }
        output$landCoverRasterTable<- renderHotable({tblValidated}, readOnly = FALSE, fixed=1, stretched='last') 
      })
    })


    # Get reactive land cover cat table from raster.
    landCoverRasterTable<-reactive({
      sel<-amNameCheck(input$landCoverSelect,'raster')
      if(!is.null(sel)){
        tbl<-read.csv(
          text=execGRASS('r.category',
            map=sel,
            intern=T),
          sep='\t',
          header=F,
          stringsAsFactors=F
          )
        names(tbl)<-config$tableColNames[['table_land_cover']]
        tbl[,1]<-as.integer(tbl[,1])
        tbl[,2]<-as.character(amSubPunct(tbl[,2],'_'))
      }else{
        tbl<-data.frame(as.integer(NA),as.character(NA))
        names(tbl)<-config$tableColNames[['table_land_cover']]
      }
      tbl
    })

    landCoverSqliteTable<-reactive({
      sel<-amNameCheck(input$landCoverSelectTable,'table')
      if(!is.null(sel)){
        tbl<-dbGetQuery(isolate(listen$dbCon),paste('select * from',sel))
        tbl[,1]<-as.integer(tbl[,1])
        tbl[,2]<-amSubPunct(tbl[,2],'_')
      }else{
        tbl<-data.frame(as.integer(NA),as.character(NA))
        names(tbl)<-config$tableColNames[['table_land_cover']]
      }
      tbl
    })


    # Save change in the lcv map.
    landCoverRasterSave<-function(selLcv,tblLcv){
      if(!is.null(selLcv) && !is.null(tblLcv)){
        tblOut<-tempfile()
        stackName<-paste0('stack_',selLcv)
        amMsg(session,type="log",text=paste('Add to stack requested for: ',selLcv,'. Stack name is',stackName))
        write.table(tblLcv,file=tblOut,row.names=F,col.names=F,sep='\t',quote=F)
        execGRASS('r.category', map=selLcv, rules=tblOut)
        execGRASS('g.copy',raster=paste0(selLcv,',',stackName),flags='overwrite')
        colorSetting<-unlist(strsplit(config$dataClass[config$dataClass$class=='stack_land_cover','colors'],'&'))
        execGRASS('r.colors',map=stackName,color=colorSetting[1])
      }
    }

    # if select lcv map change or undo btn is pressed, update hotable with value from raster.
    observe({
      input$mergeLcvUndo # re evaluate if undo is pressed
      tblSqlite<-landCoverSqliteTable()
      tblRaster<-landCoverRasterTable()
      output$landCoverRasterTable<-renderHotable(tblRaster,readOnly=F,fixedCols=1,stretched='last')
      output$landCoverSqliteTable<-renderHotable(tblSqlite,readOnly=T,fixedCols=1,stretched='last')
    })

    # if merge button is pressed, merge external and raster table
    observe({
      btn<-input$mergeLcv
      isolate({
        if(!is.null(btn) && btn > 0){
          tbl<-hot.to.df(isolate(input$landCoverRasterTable))
          tbl[tbl==""]<-NA
          #tblOrig$label<-NULL
          tblExt<-hot.to.df(isolate(input$landCoverSqliteTable))
          tblExt[tblExt==""]<-NA
          # merging. we have to preserve Y classes and manual edits !
          # so, replacing only NA with corresponding value from ext table.
          # It doesn't seems to be doable with merge(y,x,all.x=T,by='class')
          # so, do it manually. 
          naClass<-tbl[is.na(tbl$label),]$class # class of missing label
          tblExtSub<-na.omit(tblExt[tblExt$class %in% naClass,]) # find corresponding value in ext
          tbl[tbl$class %in% tblExtSub$class,]$label<-tblExtSub$label #replace value with non-na remplacement
          tbl[,1]<-as.integer(tbl[,1])
          tbl[,2]<-amSubPunct(tbl[,2],'_')
          output$landCoverRasterTable<- renderHotable({tbl}, readOnly = FALSE, fixedCols=1, stretched='last')
        }
      })
    })


    # if stack btn is pressed, save in GRASS.
    observe({ 
      btn<-input$btnAddStackLcv
      amErrorAction(title='Add to stack: lcv',{
        isolate({
          sel<-amNameCheck(input$landCoverSelect,'raster')
          tbl<-hot.to.df(input$landCoverRasterTable)
          if(!is.null(btn) && btn>0 && !is.null(sel)){
            amUpdateProgressBar(session,"lcvStackProgress",1)
            landCoverRasterSave(sel,tbl) 
            amUpdateDataList(listen)
            amUpdateProgressBar(session,"lcvStackProgress",100)
          }  
        })
      })
    })

    #------------------------------------------------------------------------------#

    # ADD TO STACK ROAD 

    #------------------------------------------------------------------------------#


    # populate selectize input
    observe({
      roadList<-dataList$vector[grep('road__',dataList$vector)]
      if(length(roadList)<1) roadList=""
      amDebugMsg('Road 1. update input. roadList=',roadList)
      updateSelectInput(session,'roadSelect',choices=roadList,selected=roadList[1])
    })


    # get road table columns
    observe({
      sel<-amNameCheck(input$roadSelect,'vector')
      amErrorAction(title='get road table columns',{
        if(!is.null(sel)){
          cla<-grassDbColType(sel,'INTEGER')
          cla<-cla[!cla %in% c('cat')]
          lab<-grassDbColType(sel,'CHARACTER')
        }else{
          cla=""
          lab=""
        }
        updateSelectInput(session,'roadSelectClass',choices=cla,selected=cla[1])
        updateSelectInput(session,'roadSelectLabel',choices=lab,selected=lab[1])
      })
    })

    # create raod preview table
    observe({
      cla<-input$roadSelectClass
      lab<-input$roadSelectLabel
      amErrorAction(title='create road preview table',{
        isolate({
          sel<-amNameCheck(input$roadSelect,'vector')
          amDebugMsg('raod 3. Sel=',sel,"lab=",lab,"cla=",cla)
          if(!is.null(sel)  && !is.null(cla) && !cla=="" && !is.null(lab) && !lab==""){
            amErrorAction(title='Module 1: road preview',{
              q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',config$maxRowPreview)
              tbl<-dbGetQuery(listen$dbCon,q)
              names(tbl)<-config$tableColNames[['table_stack_road']]
              tbl[,2]<-amSubPunct(tbl[,2],'_')
              })
          }else{
            tbl<-data.frame(as.integer(NA),as.character(NA))
            names(tbl)<-config$tableColNames[['table_stack_road']]
            tbl
          }
          output$roadPreviewTable<-renderHotable({tbl},readOnly=T,stretched='all',fixedCols=2)
          amActionButtonToggle(session=session,id='btnAddStackRoad',disable=any(NA %in% tbl$label, "" %in% tbl$label))
        })
      })
    })

    # Add vector road to raster road stack
    observe({
      btn<-input$btnAddStackRoad
      amErrorAction(title='Module 1: add stack road',{
        isolate({
          sel<-amNameCheck(input$roadSelect,'vector')
          cla<-input$roadSelectClass
          lab<-input$roadSelectLabel
          if(!is.null(sel) && !is.null(cla) && !is.null(lab) && !is.null(btn) && btn>0){ 
            tbl<-hot.to.df(input$roadPreviewTable)
            message('Module 1: Spliting',sel)
            tblN <- nrow(tbl)
            amUpdateProgressBar(session,'roadStackProgress',1)
            #increment
            inc <- 1/tblN*100
            for(i in 1:tblN){
              class <- tbl[i,'class']
              label <- tolower(amSubPunct(vect = tbl[i,'label'],sep='_'))
              #labelRule <- amSubPunct(vect = tbl[i,'label'],sep=' ')
              labelRule <- amSubPunct(label,sep=' ')
              tmpFile<-tempfile()
              tmpRules<-paste0(class,'\t',labelRule)
              write(tmpRules,file=tmpFile)
              outNameTmp<-paste0('tmp__',sel)
              outNameStack<-paste0('stack_',sel,'_',label)
              message(paste('Vector add to stack : extract class',class,' from ',sel))
              execGRASS('v.extract',
                input=sel,
                output=outNameTmp,
                where=paste0(cla,"=",class),
                flags='overwrite'
                )
              message(paste('Vector add to stack : Vector to raster, class',class,' from',outNameTmp))
              execGRASS('v.to.rast',
                use='val',
                type='line',
                input=outNameTmp,
                output=outNameStack,
                value=class,
                flags=c('d','overwrite')
                )
              colorSetting<-unlist(strsplit(config$dataClass[config$dataClass$class=='stack_road','colors'],'&'))
              execGRASS('r.colors',map=outNameStack,color=colorSetting[1])
              message(paste('Vector add to stack : setting categories. class',class,' for',outNameStack))
              execGRASS('r.category',
                map=outNameStack,
                rules=tmpFile
                )
              rmVectIfExists(outNameTmp)
              amUpdateProgressBar(session,'roadStackProgress',i*inc)
            }
            amUpdateDataList(listen)
          }
        })
      })
    })



    #------------------------------------------------------------------------------#

    #  ADD TO STACK BARRIER

    #------------------------------------------------------------------------------#


    # populate select input
    observe({
      barrierList<-dataList$vector[grep('barrier__',dataList$vector)]
      if(length(barrierList)<1)barrierList=""
      updateSelectInput(session,'barrierSelect',choices=barrierList,selected=barrierList[1])
    })

    # toggle add to stack barrier btn
    observe({
      bT<-input$barrierType
      bS<-amNameCheck(input$barrierSelect,'vector')
      disableBtn<-any(is.null(bT), bT=="", is.null(bS) , bS=="")
      amActionButtonToggle(id="btnAddStackBarrier",session,disable=disableBtn)
    })



    # preview table of barrier features
    barrierPreview<-reactive({
      sel<-amNameCheck(input$barrierSelect,'vector')
      amErrorAction(title='Module 1: barrier preview',{
        if(length(sel)>0 && !sel==""){
          tbl<-read.table(text = execGRASS('v.info',map=sel,flags='t',intern=T),sep="=")
          names(tbl)<-c('features','count')
          tbl<-tbl[tbl$features %in% c('areas','lines','points'),]
          return(tbl)
        }else{
          tbl<-data.frame(as.character(NA),as.integer(NA))
          names(tbl)<-c('features','count')
          return(tbl)
        }
      })
    })


    #  pre select feature based on max feature count
    observe({
      tbl<-barrierPreview()
      output$barrierPreviewTable<-renderHotable({tbl},readOnly=T,fixedCols=2,stretched='all') 
    })

    observe({
      tbl<-barrierPreview()
      if(!any(is.na(tbl))){
        updateRadioButtons(session,'barrierType',selected=gsub('s$','',tbl[which.max(tbl$count),'features']))
      }
    })


    # add to stack process
    observe({
      btn<-input$btnAddStackBarrier
      amErrorAction(title='Add to stack : barrier',{
        isolate({
          sel<-amNameCheck(input$barrierSelect,'vector')
          type<-input$barrierType
          if(!is.null(sel) && !sel=='' && !is.null(btn) && btn>0){
            cl=1
            la='barrier'
            tmpFile<-tempfile()
            write(paste0(cl,'\t',la),tmpFile)
            inc=1/length(sel)*100
            amErrorAction(title='Module 1: add stack barrier',{
              amUpdateProgressBar(session,'barrierProgress',1)
              for(i in 1:length(sel)){
                s<-sel[i]
                outNameStack<-paste0('stack_',s)
                message('Barrier add to stack : Vector to raster, class',cl,' from',outNameStack)
                execGRASS('v.to.rast',use='val',
                  input=s,
                  output=outNameStack,
                  type=type,
                  value=cl,
                  flags=c('overwrite',if(type=='line')'d')# bug densified lines with area: not working.
                  ) 
                execGRASS('r.category',map=outNameStack,rules=tmpFile)
                rmVectIfExists('tmp__')
                amUpdateProgressBar(session,'barrierProgress',1*inc)
              }
              amUpdateDataList(listen)
                })
          }
        })
      })
    })

  }
})
