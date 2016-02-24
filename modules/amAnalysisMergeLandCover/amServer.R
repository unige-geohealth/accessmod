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

    # Update selectize when data list change

    #------------------------------------------------------------------------------#

    # Land cover
    observe({

      amUpdateSelectChoice(
        idData=c('rLandCover'),
        idSelect=c("landCoverSelect"),
        dataList=dataList
        )
      amUpdateSelectChoice(
        idData=c('tLandCover'),
        idSelect=c("landCoverSelectTable"),
        dataList=dataList
        )
    })

    # Road
    observe({
      amUpdateSelectChoice(
        idData=c('vRoad'),
        idSelect=c("roadSelect"),
        dataList=dataList
        )
    })

    # Barrier 
    observe({
      amUpdateSelectChoice(
        idData=c('vBarrier'),
        idSelect=c("barrierSelect"),
        dataList=dataList
        )
    })



    #------------------------------------------------------------------------------#

    # Handle list of stack items

    #------------------------------------------------------------------------------#

    #
    # Update stack contents
    #

    # observe change in rasters, update stack listener
    observeEvent(dataList$raster,{
      listen$stackAll <- dataList$raster[grep('^rStack*',dataList$raster)] 
    })

    # observe stack listenner, update ui
    observeEvent(listen$stackAll,{
      #
      # get values
      #
      oldIn <- input$stackMapList_1
      oldOut <- input$stackMapList_2
      stackAll <- listen$stackAll
      #
      # new stack item
      #
      newStack <- stackAll[!stackAll %in% c(oldIn,oldOut)]
      newIn  <- na.exclude(c(newStack,stackAll[match(oldIn,stackAll)]))
      newOut <- na.exclude(stackAll[match(stackAll,oldOut)])
      
      newIn <- newIn[!sapply(newIn,is.null)]
      newOut <- newOut[!sapply(newOut,is.null)]

      #
      # update sortable input
      #
      amUpdateDoubleSortableInput("stackMapList",newIn,newOut) 
    })


    #
    # handle action buttons
    #

    observeEvent(input$btnStackAllProcess,{ 
      #
      # Move all item in the list to process
      #
      amUpdateDoubleSortableInput("stackMapList",list1=listen$stackAll) 
    })

    observeEvent(input$btnStackAllSkip,{ 
      #
      # Move all item to skip list
      #
      amUpdateDoubleSortableInput("stackMapList",list2=listen$stackAll) 
    })


    #------------------------------------------------------------------------------#

    # MERGING NEW LANDCOVER

    #------------------------------------------------------------------------------#



    # populate selectize input


    # button to hide stack items
   # observeEvent(input$btnRmMerge,{
   #   updateSelectInput(session = session, inputId = "mapStack",selected='')
   # })

    #observeEvent(input$btnAddMerge,{
    #  mapStack<-dataList$raster[grep('^rStack',dataList$raster)]
    #  updateSelectInput(session = session, inputId = "mapStack",choices=mapStack,selected=mapStack)
   # })

    observeEvent(input$btnDeleteStack,{
      amErrorAction(title="Module merge landcover : stack deletion confirmation",{

        dList <- dataList$raster[grep('^rStack',dataList$raster)]
        dSel<- input$stackMapList_2
        dList <-  dList[which(dList %in% dSel)]

        if(length(dList>0)){
          if(length(dList)>1){
            txtHead<-tags$span("Those items will be deleted")
          }else{ 
            txtHead<-tags$span("This item will be deleted")
          }
          content  <- tagList(
            txtHead,
            tags$ul(
              HTML(
                paste("<li>",names(dList),"</li>")
                )
              )
            )
          aBtns = list(
            actionButton('btnDeleteStackConfirm',"Delete")
            )
          addCancel=TRUE
        }else{
          content <- tags$span("No item selected") 
          aBtns <- NULL
          addCancel=FALSE
        }

        amUpdateModal(
          panelId="amModal",
          title="Confirmation",
          html=content,
          listActionButton=aBtns,
          addCancelButton=addCancel)
        })
    })



    observeEvent(input$btnDeleteStackConfirm,{
      amUpdateModal("amModal",close=TRUE) 
      sel<-amNameCheck(dataList,input$stackMapList_2,'raster')
      if(length(sel)>0){
        for(m in sel){ 
          rmRastIfExists(m)
        } 
        amUpdateDataList(listen)
      }
    })




    stackConflictTable<-reactive({
      tbl<-data.frame(map="-",class="-",label="-")
      amErrorAction(title='stack conflict table',{
        #sel<-amNameCheck(dataList,dataList$raster[grep('^stack*',dataList$raster)],'raster')
        #sel<-amNameCheck(dataList,input$mapStack,'raster')
        sel <- amNameCheck(dataList,input$stackMapList_1)
        btnStack<-input$btnAddStackRoad
        btnStack<-input$btnAddStackLcv
        listen$updatedConflictTable  
        if(!is.null(sel)){
          sel <- sel[! sel %in% grep('^rStackBarrier',sel,value=T)]
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
        })
      return(tbl)
    })


    # validation
    observe({
      tbl<-stackConflictTable()
      #stackList <- amNameCheck(dataList,input$mapStack,'raster')
      stackList <- amNameCheck(dataList,input$stackMapList_1,'raster')
      stackTags <- input$stackTag
      rmArtefact <- input$cleanArtefact
      amErrorAction(title='Stack merge validation',{
        # conflict table update
        if(!is.null(tbl)){
          isolate({
            nRowConflict <- nrow(tbl)
            # test if nrow >0 and send a message to UI in case of conflict
            if(isTRUE(nRowConflict>1)){
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
          #stackLcvName <- config$dataClass[config$dataClass$class=="rStackLandCover",config$language]
          stackLcvName <- "rStackLandCover" 
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
          # existing dataset (use local scoping)

          if(!rmArtefact){
            classes <- c("rLandCoverMerged") 
          }else{
            classes <- c("rLandCoverMerged","rLandCoverBridge") 
          }

          vNames <- amCreateNames(classes,stackTags,dataList)

          outMap <- tagList(       
            tags$b('Output dataset:'), 
            HTML(paste("<div>",icon('sign-out'),vNames$html,"<div/>",collapse=""))
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

      amErrorAction(title="Stack correction",{
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
    })



    #  merge action
    observeEvent(input$btnMerge,{
      timeCheck<-system.time({
        amActionButtonToggle(session=session,id='btnMerge',disable=TRUE)
        stackTag<-input$stackTag
        #sel<-amNameCheck(dataList,input$mapStack,'raster')
        sel <- amNameCheck(dataList,input$stackMapList_1,'raster')
        if(!is.null(sel) && isTRUE(nchar(stackTag)>0)){
          amErrorAction(title='Module 1: merge process',{        
            updateTextInput(session,'stackTag',value="")
            cleanBridge<-input$cleanArtefact

            pBarTitle <- "Stack merging"
            selL<-length(sel)
            inc <- 100/(selL+1)
            incN <- 0

            message('Merging land cover map requested.')

            stackTag<-amSubPunct(stackTag,config$sepTagFile,rmTrailingSep=T,rmLeadingSep=T,rmDuplicateSep=T)
            addTag<-function(base,tag=stackTag,sepT=config$sepTagFile,sepC=config$sepClass){
              paste(c(base,paste(tag,collapse=sepT)),collapse=sepC)
            }
            # set names
            merged<-addTag(amClassInfo('rLandCoverMerged')$class)
            bridges<-addTag(amClassInfo('rLandCoverBridge')$class)

            mapPosition=1
            tempBase<-'tmp__' 
            isFirstMap=TRUE
            rmRastIfExists('tmp_*')
            if(amRastExists('MASK'))execGRASS('r.mask',flags='r')

            # Use barrier as mask for each stack element 
            # keep order in tempMap name. eg. tmp__12_stack_road_test
            for(i in 1:selL){

              pbc(
                visible = TRUE,
                percent = incN*inc,
                title   = pBarTitle,
                text    = sprintf("Stack item %s/%s",i,selL)
                )
              incN <- incN +1




              map<-sel[i]
              message(paste('Proceding map',map,'MASK is',amRastExists('MASK')))
              if(length(grep('rStackBarrier', map))>0){
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


              pbc(
                visible = TRUE,
                percent = incN*inc,
                title   = pBarTitle,
                text    = sprintf("Stack item %s/%s",i,selL)
                )

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
              pbc(
                visible = TRUE,
                percent = 100,
                title   = pBarTitle,
                text    = "Cleaning artefacts/bridges requested. Please wait"
                )
              message('Cleaning artefact/bridges of one sel')
              fromRoad<-sel[grep('rStackRoad',sel)]
              amBridgeFinder(fromRoad,merged,bridges)
              amBridgeRemover(bridges,removeFromMap=merged)  
            }

            execGRASS('r.colors',map=merged,color='random')
            rmRastIfExists(paste0(tempBase,'*'))
            message(paste(merged,'created'))

            pbc(
              visible = TRUE,
              percent = 100,
              title   = pBarTitle,
              text    = "Process finished.",
              timeOut = 2
              )
            pbc(
              visible=FALSE
              )

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




    #
    #    observe({
    #      lcv<-dataList$raster[grep('^land_cover__',dataList$raster)]
    #      lcvTable<-dataList$table[grep('^table_land_cover__',dataList$table)]
    #      if(length(lcv)<1)lcv=""
    #      if(length(lcvTable)<1)lcvTable=""
    #      updateSelectInput(session,'landCoverSelect',choices=lcv)
    #      updateSelectInput(session,'landCoverSelectTable',choices=lcvTable)
    #    })
    #


    # toggle buttons to merge lcv table and add to stack
    observe({ 
      amErrorAction(title="Land cover table validation ui handling",{
        lS<-amNameCheck(dataList,input$landCoverSelect,'raster')
        # lT<-amNameCheck(dataList,input$landCoverSelectTable,'table',dbCon=isolate(grassSession$dbCon))
        tbl <- hot.to.df(input$landCoverRasterTable)
        if(TRUE){
          err = character(0)
          uTable = tolower(gsub("\\s","",unlist(tbl)))
          hasEmptyCells <- isTRUE("-" %in% uTable || "" %in% uTable || NA %in% uTable || 0 %in% uTable)
          hasDuplicate <- isTRUE(any(duplicated(uTable)))
          lcvNotFound <- isTRUE(is.null(lS))
          if(lcvNotFound){
            err <- c(err,"Land cover layer not found")
          }else{
            if(hasEmptyCells){
              err <- c(err,"The selected class column contains missing or '0' values. Please choose another column or modify the data layer.")
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

        amActionButtonToggle(id='btnAddStackLcv',session,disable=disBtn)
      })
    },label='observeBtnsLcv')



    observe({
      tblUpdated<-hot.to.df(input$landCoverRasterTable)
      isolate({
        amErrorAction(title="Land cover table validation correction",{
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
    })


    # Get reactive land cover cat table from raster.
    landCoverRasterTable<-reactive({
      amErrorAction(title="Land cover table raster",{
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
          names(tbl)<-config$tableColNames[['tLandCover']]
          tbl[,1]<-as.integer(tbl[,1])
          tbl[,2]<-as.character(amSubPunct(tbl[,2],'_'))
        }else{
          tbl<-data.frame(as.integer(NA),as.character(NA))
          names(tbl)<-config$tableColNames[['tLandCover']]
        }
        tbl
      })
    })

    landCoverSqliteTable<-reactive({
      amErrorAction(title="Land cover table sqlite",{
        sel<-amNameCheck(dataList,input$landCoverSelectTable,'table',dbCon=isolate(grassSession$dbCon))
        if(!is.null(sel)){
          tbl<-dbGetQuery(isolate(grassSession$dbCon),paste('select * from',sel))
          tbl[,1]<-as.integer(tbl[,1])
          tbl[,2]<-amSubPunct(tbl[,2],'_')
        }else{
          tbl<-data.frame(as.integer(NA),as.character(NA))
          names(tbl)<-config$tableColNames[['tLandCover']]
        }
        tbl
      })
    })


    # Save change in the lcv map.
    landCoverRasterSave<-function(selLcv,tblLcv){
      if(!is.null(selLcv) && !is.null(tblLcv)){
        # save table in temp file
        tblOut<-tempfile()
        cla = "rStackLandCover"
        stackName <- amNewName(cla,c(amGetTag(selLcv,type="file"),'grid'))
        write.table(tblLcv,file=tblOut,row.names=F,col.names=F,sep='\t',quote=F)
        execGRASS('r.category', map=selLcv, rules=tblOut)
        execGRASS('g.copy',raster=paste0(selLcv,',',stackName),flags='overwrite')
        colorSetting <- amClassListInfo(cla,"colors")
        execGRASS('r.colors',map=stackName,color=colorSetting[1])
        amMsg(session,type="log",text=paste('Add to stack requested for: ',selLcv,'. Stack name is',stackName))
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
          
          pBarTitle = "Add landcover to stack"
          sel<-amNameCheck(dataList,input$landCoverSelect,'raster')
          tbl<-hot.to.df(input$landCoverRasterTable)
          if(!is.null(btn) && btn>0 && !is.null(sel)){


          pbc(
              visible = TRUE,
              percent = 0,
              title   = pBarTitle,
              text    = ""
              )

            landCoverRasterSave(sel,tbl) 
            amUpdateDataList(listen)

            pbc(
              visible = TRUE,
              percent = 100,
              title   = pBarTitle,
              text    = "Process finished",
              timeOut = 2
              )

            pbc(
              visible= FALSE
              )
          }  
        })
      })
    })

    #------------------------------------------------------------------------------#

    # ADD TO STACK ROAD 

    #------------------------------------------------------------------------------#


    # populate selectize input




    #
    #
    #
    #  observe({
    #    roadList<-amListData('vRoad',dataList)
    #      if(length(roadList)==0)hfList=character(1)
    #      amDebugMsg('Road 1. update input. roadList=',roadList)
    #      updateSelectInput(session,'roadSelect',choices=roadList,selected=roadList[1])
    #    })



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
      noClass1000<-input$checkDontAdd1000
      amErrorAction(title='create road preview table',{
        isolate({
          sel<-amNameCheck(dataList,input$roadSelect,'vector')
          if(!is.null(sel)  && !is.null(cla) && !cla=="" && !is.null(lab) && !lab==""){
            q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',config$maxRowPreview)
            tbl<-dbGetQuery(grassSession$dbCon,q)
            names(tbl)<-config$tableColNames[['tStackRoad']]
            tbl[,2]<-amSubPunct(tbl[,2],'_')
          }else{
            tbl<-as.data.frame(list(cla="-",lab="-"),stringsAsFactors=FALSE)
            names(tbl)<-config$tableColNames[['tStackRoad']]
            tbl
          }
          output$roadPreviewTable<-renderHotable({tbl},readOnly=T,stretched='all',fixedCols=2)


          if(TRUE){

            msgList <- character(0) 
            err = character(0)
            info = character(0)
            uTable = tolower(gsub("\\s","",unlist(tbl)))
            hasEmptyCells <- isTRUE("-" %in% uTable || "" %in% uTable || NA %in% uTable)
            hasDuplicate <- isTRUE(any(duplicated(uTable)))
            roadLayerNotFound <- isTRUE(is.null(sel))
            autoAdd1000 <- isTRUE(any(tbl$class<1000) && !noClass1000)


            if(roadLayerNotFound){
              err <- c(err,"Road layer not found")
            }else{ 
              if(hasEmptyCells){ 
                err <- c(err,"The table has empty values") 
              }else{
                if(hasDuplicate) err <- c(err,"The table has duplicated values")
              }
            }


            if(autoAdd1000){
              info <- c(info,"Some classes values are less than 1000. AccesssMod will automatically convert them.")
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
            if(length(info)>0){

              info <- HTML(paste("<div>",icon('info'),info,"<div>",collapse=""))
              msgList <- tagList(msgList,tags$b("Information"),info)
            }

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
        stackClass <- "rStackRoad"
        pBarTitle <- "Add roads to stack"

        tbl<-hot.to.df(input$roadPreviewTable)
        sel<-amNameCheck(dataList,input$roadSelect,'vector')
        cla<-input$roadSelectClass
        lab<-input$roadSelectLabel


        if(!is.null(sel) && !is.null(cla) && !is.null(lab)){ 


          tblN <- nrow(tbl)
          inc <- 100/tblN
          incN <- 0



          message('Module 1: Spliting',sel)
          #increment
          for(i in 1:tblN){

            pbc(
              visible = TRUE,
              percent = incN*inc,
              title   = pBarTitle,
              text    = sprintf("Stack item %s/%s",i,tblN)
              )
            incN <- incN +1


            #
            # Init
            #
            class <- tbl[i,'class']
            label <- tbl[i,'label']
            outNameTmp<-paste0('tmp__',sel)
            outNameStack <- amNewName(stackClass,c(label,amGetTag(sel,type="file"),'line'))
            colorSetting <- amClassListInfo(stackClass,"colors")
           


            #
            # Extract road by class
            #

            execGRASS('v.extract',
              input=sel,
              output=outNameTmp,
              where=paste0(cla,"=",class),
              flags='overwrite'
              )

            #
            # Class and label cleaning 
            #

            if(isTRUE(class<1000 && !input$checkDontAdd1000)){
              class=1000+class
            }

            label <- tolower(amSubPunct(label, sep='_'))
            labelRule <- amSubPunct(label,sep=' ')
            tmpFile<-tempfile()
            tmpRules<-paste0(class,'\t',labelRule)


            #
            # Write temp rules
            #

            write(tmpRules,file=tmpFile)

            #
            # Raster creation
            #


            execGRASS('v.to.rast',
              use='val',
              type='line',
              input=outNameTmp,
              output=outNameStack,
              value=class,
              flags=c('d','overwrite')
              )

            execGRASS('r.colors',map=outNameStack,color=colorSetting[1])

            execGRASS('r.category',
              map=outNameStack,
              rules=tmpFile
              )

            rmVectIfExists(outNameTmp)

            pbc(
              visible = TRUE,
              percent = incN*inc,
              title   = pBarTitle,
              text    = sprintf("Stack item %s/%s",i,tblN)
              )

          }
          amUpdateDataList(listen)
          pbc(
            visible = TRUE,
            percent = 100,
            title   = pBarTitle,
            text    = "Process finished.",
            timeOut = 2
            )
          pbc(
            visible=FALSE
            )


        }
        amActionButtonToggle(session=session,id='btnAddStackRoad',disable=FALSE)
      })
    })



    #------------------------------------------------------------------------------#

    #  ADD TO STACK BARRIER

    #------------------------------------------------------------------------------#




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
          sel <- "areas"
        }
        updateRadioButtons(session,'barrierType',selected=gsub('s$','',sel))
      }
    })


    # add to stack process
    observeEvent(input$btnAddStackBarrier,{
      amErrorAction(title='Add to stack : barrier',{
        stackClass <- "rStackBarrier"
        pBarTitle <- "Add barriers to stack"
        amActionButtonToggle(session=session,id='btnAddStackBarrier',disable=TRUE)
        sel<-amNameCheck(dataList,input$barrierSelect,'vector')
        type <- input$barrierType
        if(!is.null(sel) && !sel==''){
          cl=1
          la='barrier'
          tmpFile<-tempfile()
          write(paste0(cl,'\t',la),tmpFile)
          nSel <- length(sel)
          inc <- 100/nSel
          incN <- 0


          for(i in 1:nSel){

            pbc(
              visible = TRUE,
              percent = incN*inc,
              title   = pBarTitle,
              text    = sprintf("Stack item %s/%s",i,nSel)
              )
            incN <- incN + 1



            s<-sel[i]
            outNameStack <- amNewName(stackClass,c(amGetTag(s,type="file"),type))
            #outNameStack<-paste0('stack_',s)
            message('Barrier add to stack : Vector to raster, class',cl,' from',outNameStack)
            execGRASS('v.to.rast',use='val',
              input=s,
              output=outNameStack,
              type=type,
              value=cl,
              flags=c('overwrite',if(type=='line')'d')# bug densified lines with area: not working.
              ) 
            execGRASS('r.category',map=outNameStack,rules=tmpFile)

            pbc(
              visible = TRUE,
              percent = incN*inc,
              title   = pBarTitle,
              text    = sprintf("Stack item %s/%s",i,nSel)
              )

          }


          amUpdateDataList(listen)
          pbc(
            visible = TRUE,
            percent = 100,
            title   = pBarTitle,
            text    = "Process finished.",
            timeOut = 2
            )

          pbc(
            visible=FALSE
            )
        }
        amActionButtonToggle(session=session,id='btnAddStackBarrier',disable=FALSE)
      })
    })

  }
})
