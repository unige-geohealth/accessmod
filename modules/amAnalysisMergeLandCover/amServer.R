#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing land cover in a given order.
#
# input : road, barrier, land cover
# output : merged land cover



observe({
  amModEnabled<-listen$tabControl_module_selector
  if(isTRUE(!is.null(amModEnabled) && amModEnabled)){
    
    #------------------------------------------------------------------------------#

    # MERGING NEW LANDCOVER

    #------------------------------------------------------------------------------#

    # populate selectize input

    observe({
      mapStack<-dataList$raster[grep('^stack_',dataList$raster)]
      updateSelectInput(session,'mapStack',choices=mapStack,selected=mapStack)
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
        mapStack<-dataList$raster[grep('^stack_',dataList$raster)]
        updateSelectInput(session = session, inputId = "mapStack",choices=mapStack,selected=mapStack)
      }
    })


    observeEvent(input$btnDeleteStack,{
      sel<-amNameCheck(dataList,input$mapStack,'raster')
      if(length(sel)>0){
        for(m in sel){ 
          rmRastIfExists(m)
        } 
         amUpdateDataList(listen)
      }
    })




    stackConflictTable<-reactive({
      #TODO: use only stack present in mapStack
      amErrorAction(title='stack conflict table',{
        #sel<-amNameCheck(dataList,dataList$raster[grep('^stack*',dataList$raster)],'raster')
        sel<-amNameCheck(dataList,input$mapStack,'raster')
        btnStack<-input$btnAddStackRoad
        btnStack<-input$btnAddStackLcv
        listen$updatedConflictTable  
        tbl<-data.frame(map="-",class="-",label="-")
        if(!is.null(sel)){
          tbl=data.frame()
          sel <- sel[! sel %in% grep('^stack_barrier*',sel,value=T)]
          for(m in sel){
            t<-read.table(text=execGRASS('r.category',map=m,intern=T),
              sep="\t",
              stringsAsFactors=F
              )
            t$map=m
            names(t)<-c('class','label','map')
          tbl=rbind(t,tbl)
          }
          dupClass <- tbl$class[duplicated(tbl$class)]
          tbl <- tbl[tbl$class %in% dupClass,]
          tbl <- tbl[order(tbl$class),]
        }
        return(tbl)
})
    })


    # validation
    observe({
      tbl<-stackConflictTable()
      stackList <- amNameCheck(dataList,input$mapStack,'raster')
      stackTags <- input$stackTag
      rmArtefact<-input$cleanArtefact
      amErrorAction(title='stack merge validation',{


        # conflict table update
        if(!is.null(tbl)){
          isolate({
            nRowConflict <- nrow(tbl)
            # test if nrow >0 and send a message to UI in case of conflict
            if(nRowConflict>1){
              tbl$newClass=tbl$class
            }else{
              tbl<-data.frame(map=as.character(NA),class=as.integer(NA),label=as.character(NA),newClass=as.integer(NA))
            }
            # render hotable with a possibly empty table
            output$stackConflict<-renderHotable({tbl},
              stretched='last',readOnly=c(1,2,3)
              )

          })
        }

        # validation process
        if(TRUE){
          err = character(0)
          stackItemMissing <- isTRUE(any(sapply(stackList,is.null)))
          hasConflict <- isTRUE(!is.null(tbl) && nrow(tbl) > 1)
          hasTag <- isTRUE(!any(stackTags=='', is.null(stackTags), nchar(stackTags)<1))
          stackLcvName <- config$dataClass[config$dataClass$id=="amStackLcv","class"]
          stackNotOneLcv <- !isTRUE(length(grep(stackLcvName,stackList))==1)
          if(stackItemMissing){
            err <- c(err,"Stack listed not found, relaunch the application.")
          }else{
            if(hasConflict){
              nConf <- duplicated(tbl$class)
              nConf <- length(nConf[nConf])
              confPlur <- ifelse(nConf>1,"conflicts","conflict")
              err <- c(err,paste(nConf,confPlur,"of class found. See under \"Conflicting classes among items in the stack\""))
            }else{
              if(stackNotOneLcv){
                err <- c(err,"At least one land cover stack item is required to proceed.")
              }else{
                if(!hasTag){
                  err <- c(err,"Please enter a least one tag")
                }
              }
            }
          }
          if(length(err)>0){
            err <- HTML(paste("<div>",icon('exclamation-triangle'),err,'</div>',collapse=""))
            msgList <- tagList(tags$b('Validation issues:'),err)
            disBtn <- TRUE
          }else{
            msgList <- tagList(p(''))
            disBtn <- FALSE
          }
        }


        # set outputname if no validation problem
        if(!is.null(rmArtefact) && hasTag && !disBtn){
          stackTag  <- amGetUniqueTags(stackTags) 
          addTag<-function(base,tag=stackTag,sepT=config$sepTagFile,sepC=config$sepClass){
            paste(c(base,paste(tag,collapse=sepT)),collapse=sepC)
          }
          # existing dataset (use local scoping)
          outTxt <- function(x,condition=TRUE){
            if(isTRUE(condition)){
              e <- x %in% dataList$df$origName
              y <- paste(amGetClass(x,config$sepClass),'[',paste(stackTag,collapse=" "),']')
              if(e){
                return(sprintf(" %s  <b style=\"color:#FF9900\"> (overwrite warning)</b> ",y))
              }else{
                return(sprintf("%s <b style=\"color:#00CC00\">(ok)</b>",y))
              }
            }else{
              NULL
            }
          }
          # set names
          merged  <- addTag(amClassInfo('amLcvM')$class)
          bridges <- addTag(amClassInfo('amLcvMB')$class)

          # output lines
          out <- c(outTxt(merged),outTxt(bridges))
          # take ony merged name if not rm artefect 
          if(!rmArtefact)out=out[1]

          outMap <- tagList(       
            tags$b('Output dataset:'), 
            HTML(paste("<div>",icon('sign-out'),out,"<div/>",collapse=""))
            ) 
        }else{
        outMap=""
        }


        output$stackNameInfo<-renderUI(outMap)
        output$stackWarning<-renderUI({msgList})
        amActionButtonToggle(id='btnMerge',session,disable=disBtn)



})
    })



    # if btn correct stack is pressed
    # reclassify raster. 
    # NOTE: we can't do a simple update using r.mapcalc or r.category : we need to keep cat label.
    observeEvent(input$btnCorrectStack,{
      # get input table with modified column
      cTable<-hot.to.df(input$stackConflict)
      nCtbl<-nrow(cTable)
      # if number of row is greater than one
      if(nCtbl>1){
        # for each map in table
        for(m in cTable$map){
          # get tables orig and new classes
          oClass = cTable[cTable$map==m,'class']
          nClass = cTable[cTable$map==m,'newClass']
          # if texts in classes are different
          if(!identical(paste(oClass),paste(nClass))){ 
            # read table from raster category
            tbl<-read.csv(
              text=execGRASS('r.category',
                map=m,
                intern=T),
              sep='\t',
              header=F,
              stringsAsFactors=F
              )
            tbl[is.na(tbl$V2),'V2']<-"no label"
            rulesFile<-tempfile()
            # extract all old classes
            clOrig=tbl$V1
            clNew=tbl$V1
            clNew[clNew==oClass]<-nClass

            # compose a new rules file and 
            rules=paste(clOrig,"=",clNew," ",tbl$V2,collapse="\n")
            write(x=rules,file=rulesFile)
            execGRASS('g.copy',raster=c(m,'tmp_reclass'),flags='overwrite')
            execGRASS('r.reclass', input='tmp_reclass',output='tmp_reclass_2', rules=rulesFile,flags='overwrite')
            execGRASS('r.resample',input='tmp_reclass_2',output=m,flags='overwrite')
            # signal change to reactive stack conflict table using a listener.
            listen$updatedConflictTable<-runif(1)
          }
        } 
      }
    })



    #  merge action
    observeEvent(input$btnMerge,{
      timeCheck<-system.time({
        amActionButtonToggle(session=session,id='btnMerge',disable=TRUE)
          stackTag<-input$stackTag
          sel<-amNameCheck(dataList,input$mapStack,'raster')
          if(!is.null(sel) && isTRUE(nchar(stackTag)>0)){
            amErrorAction(title='Module 1: merge process',{        
              updateTextInput(session,'stackTag',value="")
              selL<-length(sel)
              cleanBridge<-input$cleanArtefact
              inc<-1/(selL+1)*100
              message('Merging land cover map requested.')

              stackTag<-amSubPunct(stackTag,config$sepTagFile,rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T)
              addTag<-function(base,tag=stackTag,sepT=config$sepTagFile,sepC=config$sepClass){
                paste(c(base,paste(tag,collapse=sepT)),collapse=sepC)
              }
              # set names
              merged<-addTag(amClassInfo('amLcvM')$class)
              bridges<-addTag(amClassInfo('amLcvMB')$class)

              mapPosition=1
              tempBase<-'tmp__' 
              isFirstMap=TRUE
              rmRastIfExists('tmp_*')
              if(amRastExists('MASK'))execGRASS('r.mask',flags='r')

              message(paste('stack will be merged in this order:',paste(sel,collapse=', ')))
              amUpdateProgressBar(session,"stackProgress",1)

              # Use barrier as mask for each stack element 
              # keep order in tempMap name. eg. tmp__12_stack_road_test
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
              # When we patch road maps to land cover maps, small overlaps can appear on top of predefined barrier.
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

        amActionButtonToggle(session=session,id='btnMerge',disable=FALSE)
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
      lS<-amNameCheck(dataList,input$landCoverSelect,'raster')
      # lT<-amNameCheck(dataList,input$landCoverSelectTable,'table',dbCon=isolate(grassSession$dbCon))
      tbl <- hot.to.df(input$landCoverRasterTable)
      if(TRUE){
        err = character(0)
        uTable = tolower(gsub("\\s","",unlist(tbl)))
        hasEmptyCells <- isTRUE("-" %in% uTable || "" %in% uTable || NA %in% uTable)
        hasDuplicate <- isTRUE(any(duplicated(uTable)))
        lcvNotFound <- isTRUE(is.null(lS))
        if(lcvNotFound){
          err <- c(err,"Land cover layer not found")
        }else{
          if(hasEmptyCells){
            err <- c(err,"The table has empty values")
          }else{ 
            if(hasDuplicate) err <- c(err,"The table has duplicated values")
          }
        }
        if(length(err)>0){
          err <- HTML(paste("<div>",icon('exclamation-triangle'),err,'</div>',collapse=""))
          disBtn <- TRUE
        }else{
          disBtn <- FALSE
        }
      }


      # send result to ui
      if(length(err)>0){
        msgList <- tagList(tags$b('Validation issues:'),err)
      }else{
        msgList <- tagList(p('Save labels and add land cover data to the stack:'))
      }
      output$stackLandcoverValidation <- renderUI(msgList) 

      #      lab<-hot.to.df(input$landCoverRasterTable)$label
      #     disableMerge=any(is.null(lS),lS=='',is.null(lT),lT=="")
      #    disableStack=any(is.null(lS),lS=='',is.null(lab),"" %in% lab,NA %in% lab)
      amActionButtonToggle(id='btnAddStackLcv',session,disable=disBtn)
      #amActionButtonToggle(id='mergeLcvUndo',session,disable=!allow)
      #     amActionButtonToggle(id='mergeLcv',session,disable=disableMerge)
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
      sel<-amNameCheck(dataList,input$landCoverSelect,'raster')
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
      sel<-amNameCheck(dataList,input$landCoverSelectTable,'table',dbCon=isolate(grassSession$dbCon))
      if(!is.null(sel)){
        tbl<-dbGetQuery(isolate(grassSession$dbCon),paste('select * from',sel))
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
    observeEvent(input$mergeLcv,{
      print('btn merge lcv pressed')
        #if(!is.null(btn) && btn > 0){
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
        #}
     # })
    })


    # if stack btn is pressed, save in GRASS.
    observe({ 
      btn<-input$btnAddStackLcv
      amErrorAction(title='Add to stack: lcv',{
        isolate({
          sel<-amNameCheck(dataList,input$landCoverSelect,'raster')
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
    roadList<-amListData('amRoad',dataList)
      if(length(roadList)==0)hfList=character(1)
      amDebugMsg('Road 1. update input. roadList=',roadList)
      updateSelectInput(session,'roadSelect',choices=roadList,selected=roadList[1])
    })



    # get road table columns
    observe({
      sel<-amNameCheck(dataList,input$roadSelect,'vector')
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
        sel<-amNameCheck(dataList,input$roadSelect,'vector')
        if(!is.null(sel)  && !is.null(cla) && !cla=="" && !is.null(lab) && !lab==""){
          q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',config$maxRowPreview)
          tbl<-dbGetQuery(grassSession$dbCon,q)
          names(tbl)<-config$tableColNames[['table_stack_road']]
          tbl[,2]<-amSubPunct(tbl[,2],'_')
        }else{
          tbl<-data.frame("-","-")
          names(tbl)<-config$tableColNames[['table_stack_road']]
          tbl
        }
        output$roadPreviewTable<-renderHotable({tbl},readOnly=T,stretched='all',fixedCols=2)


        if(TRUE){
          err = character(0)
          uTable = tolower(gsub("\\s","",unlist(tbl)))
          hasEmptyCells <- isTRUE("-" %in% uTable || "" %in% uTable || NA %in% uTable)
          hasDuplicate <- isTRUE(any(duplicated(uTable)))
          roadLayerNotFound <- isTRUE(is.null(sel))
          if(roadLayerNotFound){
            err <- c(err,"Road layer not found")
          }else{ 
            if(hasEmptyCells){ 
              err <- c(err,"The table has empty values") 
            }else{
              if(hasDuplicate) err <- c(err,"The table has duplicated values")
            }
          }
          if(length(err)>0){
            err <- HTML(paste("<div>",icon('exclamation-triangle'),err,'</div>',collapse=""))
            disBtn <- TRUE
          }else{
            disBtn <- FALSE
          }
        }


        # send result to ui
        if(length(err)>0){
          msgList <- tagList(tags$b('Validation issues:'),err)
        }else{
          msgList <- ""# tagList(tags$b('Ready to compute.'))
        }
        output$stackRoadValidation <- renderUI(msgList) 




        amActionButtonToggle(session=session,id='btnAddStackRoad',disable=disBtn)
      })
      })
  })

    # Add vector road to raster road stack
    observeEvent(input$btnAddStackRoad,{
      amErrorAction(title='Module 1: add stack road',{
          amActionButtonToggle(session=session,id='btnAddStackRoad',disable=TRUE)
          sel<-amNameCheck(dataList,input$roadSelect,'vector')
          cla<-input$roadSelectClass
          lab<-input$roadSelectLabel
          if(!is.null(sel) && !is.null(cla) && !is.null(lab)){ 
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
          amActionButtonToggle(session=session,id='btnAddStackRoad',disable=FALSE)
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
      bS<-amNameCheck(dataList,input$barrierSelect,'vector')
      disableBtn<-any(is.null(bT), bT=="", is.null(bS) , bS=="")
      amActionButtonToggle(id="btnAddStackBarrier",session,disable=disableBtn)
    })



    # preview table of barrier features
    barrierPreview<-reactive({
      sel<-amNameCheck(dataList,input$barrierSelect,'vector')
      amErrorAction(title='Module 1: barrier preview',{
        if(length(sel)>0 && !sel==""){
          tbl<-read.table(text = execGRASS('v.info',map=sel,flags='t',intern=T),sep="=")
          names(tbl)<-c('type','count')
          tbl$type <- as.character(tbl$type)
          tbl<-tbl[tbl$type %in% c('areas','lines','points'),]
          tbl[tbl$type == "areas","type"] <- "polygons"
          return(tbl)
        }else{
          tbl<-data.frame(as.character(NA),as.integer(NA))
          names(tbl)<-c('type','count')
          return(tbl)
        }
      })
    })

    # render table
    observe({
      tbl<-barrierPreview()
      output$barrierPreviewTable <- renderHotable({tbl},readOnly=T,fixedCols=2,stretched='all') 
    })

    #  pre select feature based on max count by type
    observe({
      tbl<-barrierPreview()
      if(isTRUE(!any(is.na(tbl)))){
        sel <- tbl[which.max(tbl$count),'type'] 
        if(sel=="polygons"){
         sel == "areas"
        }
        updateRadioButtons(session,'barrierType',selected=gsub('s$','',sel))
      }
    })


    # add to stack process
    observeEvent(input$btnAddStackBarrier,{
      amErrorAction(title='Add to stack : barrier',{
        amActionButtonToggle(session=session,id='btnAddStackBarrier',disable=TRUE)
        sel<-amNameCheck(dataList,input$barrierSelect,'vector')
        type <- input$barrierType
        if(!is.null(sel) && !sel==''){
          cl=1
          la='barrier'
          tmpFile<-tempfile()
          write(paste0(cl,'\t',la),tmpFile)
          inc=1/length(sel)*100
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
        }
        amActionButtonToggle(session=session,id='btnAddStackBarrier',disable=FALSE)
      })
    })

  }
})
