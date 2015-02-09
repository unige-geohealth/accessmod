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

#TODO: clean script and convert to function.

#----------------------------------------{ UI : validation
output$module1<-renderUI({
  print(paste(amSysTime(),'renderUI module 1'))
  # conditional module display
  validInput<-c(
    'g'=!is.null(listen$gisLock)
    )
  msgList<-tagList()
  if(!all(validInput)){
    msgList$g<-ifelse(!validInput['g'],msgNoLocation,'')
    box(status='danger',tagList(icon('exclamation-triangle'),msgList))
  }else{
    tagList(
      #busyIndicator("Stack calculation",wait=2000),
      h3('Stack of map to merge into a new land cover'),
      fluidRow(stackModule),
      hr(),
      h3('Add to stack'),
      fluidRow(landCoverMod1),
      hr(),
      fluidRow(roadMod1),
      hr(),
      fluidRow(barrierMod1)
      )
  }
})


#----------------------------------------{ Landcover
#------------------------------{ UI
landCoverMod1<-renderUI({
  tagList(
    sidebarPanel(
      h4('Land cover'),
      selectInput('landCoverSelect','Select land cover map:',choices=""),
      selectInput('landCoverSelectTable','Select optional land cover table:',choices=""),
      p('Save raster value and add land cover raster on stack:'),
      actionButton('btnAddStackLcv','add to stack')
      ),
    mainPanel(
      h4('Table of land cover categories'),
      list(
        #fluidRow(  
        tags$div(class='div-table',  
          # column(4,
          tags$div(class='div-table-vert-center',
            h5('Clategories from raster'),
            p("Edit the column 'Label' to change raster table or copy and paste from spreadsheet."), 
            #hotable("landCoverCatTable")
            tableOutput("landCoverCatTable")
            ),
          conditionalPanel(condition="input.landCoverSelectTable.length>0",
            tags$div(class='div-table-vert-center',
              actionButton('mergeLcv',c(icon('long-arrow-left'),'merge')),
              actionButton('mergeLcvUndo',c(icon('undo'),'undo'))
              ),
            #column(4, 
            tags$div(class='div-table-vert-center',
              h5('Categories from table'),
              p('Value from imported land cover table. Click on arrow to merge by classe.'),
              tableOutput("landCoverSqliteTable")
              )
            )
          )
        )
      )
    )
})

observe({
gLock<-listen$gisLock
print(paste(amSysTime(),'observe gislock landcover'))
updateSelectizeInput(session,'landCoverSelect',choices=dataList()$lcv,selected='')
updateSelectizeInput(session,'landCoverSelectTable',choices=dataList()$table[grep('table_land_cover',dataList()$table)],selected='')

})


#------------------------------{ Reactivity

# Get reactive land cover cat table.
landCoverCatTable<-reactive({

print(paste(amSysTime(),'landcovercattable'))
  btn<-input$btnAddStackLcv
  sel<-input$landCoverSelect
  if(!is.null(btn) && btn>0 || !is.null(sel) && !sel==''){
    tblCat<-read.csv(
      text=execGRASS('r.category',
        map=sel,
        intern=T),
      sep='\t',
      header=F,
      stringsAsFactors=F
      )
    names(tblCat)<-acceptColNames[['table_land_cover']]
    tblCat[is.na(tblCat)]<-'-'
    return(tblCat)
  }
})

# Save change in the lcv map.
landCoverCatSave<-function(selLcv,tblLcv){
  if(!is.null(selLcv) && !is.null(tblLcv)){
    tblOut<-tempfile()
    stackName<-paste0('stack_',selLcv)
    amMsg(session,type="log",text=paste('Add to stack requested for: ',selLcv,'. Stack name is',stackName))
    write.table(tblLcv,file=tblOut,row.names=F,col.names=F,sep='\t',quote=F)
    execGRASS('r.category', map=selLcv, rules=tblOut)
    execGRASS('g.copy',rast=paste0(selLcv,',',stackName),flags='overwrite')
  }
}



# If new map is selected, import new data 
observe({
print(paste(amSysTime(),'observe output landcover cat table'))
  sel<-input$landCoverSelect
  mergeLcvUndo<-input$mergeLcvUndo
  selTable<-input$landCoverSelectTable
  if(!is.null(sel) && !sel=='' ||
    !is.null(mergeLcvUndo) && mergeLcvUndo >0
    ){
    browser()
    tblRaster<-landCoverCatTable()
    if(!is.null(selTable) && !selTable==''){
      tblSqlite<-dbGetQuery(listen$dbCon,paste('select * from',selTable))
    }else{
      tblSqlite<-NULL
    }
    if(!is.null(tblRaster)){
      output$landCoverCatTable<- renderHotable({tblRaster}, readOnly = FALSE)
    }
    if(!is.null(tblSqlite)){
      output$landCoverSqliteTable<-renderHotable({tblSqlite}, readOnly=TRUE)
    }
  }

})

# if merge button is pressed, merge external and raster table
observe({

print(paste(amSysTime(),'observe merge button'))
  btn<-input$mergeLcv
  if(!is.null(btn) && btn > 0){
    tblOrig<-hot.to.df(isolate(input$landCoverCatTable))
    tblOrig$label<-NULL
    tblExt<-hot.to.df(isolate(input$landCoverSqliteTable))
    tblMerge<-merge(tblOrig,tblExt,by='class')
    output$landCoverCatTable<- renderHotable({tblMerge}, readOnly = FALSE)
  }

})



# if stack btn is pressed, save in GRASS.
observe({
  
print(paste(amSysTime(),'observe btn add to stack'))
  btn<-input$btnAddStackLcv
  sel<-isolate(input$landCoverSelect)
  tbl<-hot.to.df(isolate(input$landCoverCatTable))
  if(!is.null(btn) && btn>0){
    landCoverCatSave(sel,tbl) 
  }  
})


#----------------------------------------{ Roads

#------------------------------{ UI
roadMod1<-renderUI({
  tagList(
    sidebarPanel(
      tagList(
        h4('Roads'),
        selectInput('roadSelect','Select road map:',choices="",selected=""),
        selectInput('roadSelectClass','Select road class column (integer) :',choices="",selected=""),
        selectInput('roadSelectLabel','Select road label column (text) :',choices="",selected=""),
        actionButton('btnAddStackRoad','Add to stack')
        )
      ),
    mainPanel(
      fluidRow(
        h4('Table of road categories.'),
        conditionalPanel(condition="input.roadSelect.length>0",
          list(
            p(paste('Preview the distinct combination from selected column (max.',maxRowPreview,'rows.)')),
            hotable('roadPreviewTable')
            )
          )
        )  
      )
    )
})


observe({
listen$gisLock
updateSelectizeInput(session,'roadSelect',choices=dataList()$road,selected="")
})


observe({
  selTable<-input$roadSelect
  if(!is.null(selTable) && !selTable==""){
    cla<-grassDbColType(selTable,'INTEGER')
    cla<-cla[!cla %in% c('cat')]
    lab<-grassDbColType(selTable,'CHARACTER')
    updateSelectizeInput(session,'roadSelectClass',choices=cla, selected="")
    updateSelectizeInput(session,'roadSelectLabel',choices=lab, selected="")
  }
})


output$roadPreviewTable<-renderHotable({
  sel<-input$roadSelect
  cla<-input$roadSelectClass
  lab<-input$roadSelectLabel
  if(!is.null(sel) && !sel=="" && !is.null(cla) && !cla=="" && !is.null(lab) && !lab==""){
    tbl<-roadPreview()
    if(!is.null(tbl)){
      tbl
    }else{
      data.frame('NA')
  }}
})

#------------------------------{ reactivity

roadPreview<-reactive({
  sel<-input$roadSelect
  cla<-input$roadSelectClass
  lab<-input$roadSelectLabel
  if(!is.null(sel) && !sel=="" && !is.null(cla) && !cla=="" && !is.null(lab) && !lab==""){
    withCallingHandlers({
      tryCatch({
        q=paste('SELECT DISTINCT',cla,',',lab,' FROM',sel,'LIMIT',maxRowPreview)
        tbl<-dbGetQuery(listen$dbCon,q)
        names(tbl)<-acceptColNames[['table_stack_road']]
        tbl
      },
      error = function(cond){
        amErrHandler(errMsgList,conditionMessage(cond),title="Module 1")
    })},
      warning= function(cond){
        amErrHandler(errMsgList,conditionMessage(cond),title="Module 1")
      },
      message= function(cond){
        amMsg(session,'log',conditionMessage(cond),title="Module 1")  
      }
      )
  }
})


# create new name for stack (raster) version
observe({
  btn<-input$btnAddStackRoad
  sel<-isolate(input$roadSelect)
  cla<-isolate(input$roadSelectClass)
  lab<-isolate(input$roadSelectLabel)
  if(!is.null(sel) && !is.null(cla) && !is.null(lab) && btn>0){ 
    withCallingHandlers({
      tryCatch({
        tbl<-roadPreview()
        msgRoadStack<-paste('Module 1: Spliting',sel)
        message(msgRoadStack)
        tblN <- nrow(tbl)
        updateProgressBar(session, inputId = "progMod2Road", value = 1,visible=TRUE)
        #increment
        inc <- 1/tblN*100
        for(i in 1:tblN){
          class <- tbl[i,'class']
          label <- tolower(autoSubPunct(vect = tbl[i,'label'],sep='_'))
          #labelRule <- autoSubPunct(vect = tbl[i,'label'],sep=' ')
          labelRule <- autoSubPunct(label,sep=' ')
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
            input=outNameTmp,
            output=outNameStack,
            value=class,
            flags=c('d','overwrite')
            )
          message(paste('Vector add to stack : setting categories. class',class,' for',outNameStack))
          execGRASS('r.category',
            map=outNameStack,
            rules=tmpFile
            )
          execGRASS('g.remove',
            vect=outNameTmp)

          updateProgressBar(session, inputId = "progMod2Road", value = i*inc)
        }

      },
      error = function(cond){
        updateProgressBar(session, inputId = "progMod2Road", value =0, visible=FALSE)
        amErrHandler(errMsgList,conditionMessage(cond),title="Module 1")
    })},
      warning= function(cond){
        amErrHandler(errMsgList,conditionMessage(cond),title="Module 1")
      },
      message= function(cond){
        amMsg(session,'log',conditionMessage(cond),title="Module 1")  
      }
      )
  }
})

#----------------------------------------{ Barriers

#------------------------------{ UI 
# main ui layout
barrierMod1<-renderUI({
  tagList(
    sidebarPanel(
      tagList(
        h4('Barriers'),
        selectInput('barrierSelect','Select barrier map:',choices="",selected="",multiple=F),
        checkboxGroupInput("barrierType", "Barrier type:",
          c("Areas" = "area",
            "Lines" = "line",
            "Point" = "point"),selected=c('area','line','point'), inline=TRUE),
        conditionalPanel(
          condition = "input.barrierSelect.length > 0 && input.barrierType.length >0",
          actionButton('btnAddStackBarrier','Add to stack')
          )
        )
      ),
    mainPanel(
      h4('Barrier info'),
      hotable("barrierPreviewTable")
      )
    )
})

observe({
listen$gislock
updateSelectizeInput(session,'barrierSelect',choices=dataList()$barrier,selected="")

})



output$barrierPreviewTable<-renderHotable({
  listen$gisLock
  sel<-input$barrierSelect
  if(!is.null(sel) && !sel==""){
    tbl<-read.table(text = execGRASS('v.info',map=sel,flags='t',intern=T),sep="=")
    names(tbl)<-c('features','count')
    tbl<-tbl[tbl$features %in% c('areas','lines','points'),]
    return(tbl)
  }
})







#------------------------------{ reactivity

observe({
  btn<-input$btnAddStackBarrier
  sel<-isolate(input$barrierSelect)
  type<-isolate(input$barrierType)
  if(!is.null(sel) && !sel=='' && !is.null(btn) && btn>0){
    cl=999
    la='barrier'
    tmpFile<-tempfile()
    write(paste0(cl,'\t',la),tmpFile)
    for(s in sel){
      withCallingHandlers({
        tryCatch({
          outNameStack<-paste0('stack_',s)
          message('Barrier add to stack : Vector to raster, class',cl,' from',outNameStack)
          execGRASS('v.to.rast',use='val',
            input=sel,
            output=outNameStack,
            type=type,
            value=cl,
            flags=c('overwrite','d'))
          execGRASS('r.category',map=outNameStack,rules=tmpFile)
          rmVectIfExists('tmp__')
        },
        error = function(cond){
          amErrHandler(errMsgList,conditionMessage(cond),title='Module 1')
      })},
        warning= function(cond){
          amErrHandler(errMsgList,conditionMessage(cond),title='Module 1')
        },
        message= function(cond){
          amMsg(session,'log',conditionMessage(cond),title='Module 1')  
        }
        )
    }
  }

})

#----------------------------------------{ Stack module


#------------------------------{ UI
stackModule<-renderUI({  
  mapStack<-dataList()$stack
  sidebarPanel(
    h4('Choose stack order for merging maps.'),
    bsProgressBar("progMod2Stack", value = 0, visible=FALSE),
    if(length(mapStack)>0){
      list(
        addUIDep(
          selectizeInput("mapStack", 
            "",
            choices=mapStack,
            selected=mapStack, 
            TRUE, 
            options = list(plugins = list("drag_drop", "remove_button")))
          ),
        actionButton('btnRmMerge',"Hide all stack items"),
        actionButton('btnAddMerge',"Show all stack items"),
        hr(),
        h4('Merge selected maps.'),
        textInput('stackTag','Add tags (minimum 1)',value=''),
        conditionalPanel(
          condition = "input.stackTag.length > 0",
          #list(
          #checkboxInput('checkBuffer',label = 'Add a buffer (one cell) around barriers? recommended for 16 directions analysis',value = TRUE),
          actionButton('btnMerge',"Merge new land cover")
          # )
          )
        )
    }else{
      p("No stack maps was found. Please add one or more to the stack.")
    }
    ,width=8)
})



#------------------------------{ reactivity
observe({
  stackTag<-input$stackTag
  if(!is.null(stackTag) && ! stackTag==''){
    stackTag<-unlist(stackTag)
    updateTextInput(session,'stackTag',value=autoSubPunct(stackTag,sepTagUi))
  }
})


# button to hide stack items
observe({
  rmMerge<-input$btnRmMerge
  if(!is.null(rmMerge)&& rmMerge>0){
    updateSelectizeInput(session = session, inputId = "mapStack",selected='')
  }

})

# button to show stack items
observe({
  addMerge<-input$btnAddMerge   
  if(!is.null(addMerge)&& addMerge>0){
    mL<-dataList()
    mapStack<-mL$stack
    updateSelectizeInput(session = session, inputId = "mapStack",selected=mapStack)
  }

})

# button merge
observe({
  btnMerge<-input$btnMerge
  if(!is.null(btnMerge) && btnMerge > 0){
    withCallingHandlers({
      tryCatch({
        sel<-isolate(input$mapStack)
        selL<-length(sel)
        inc<-1/selL*100
        #buff<-isolate(input$checkBuffer)
        stackTag<-isolate(input$stackTag)
        message('Merging landcover map requested.')
        stackTag<-autoSubPunct(stackTag,sepTagFile)
        tagSplit<-unlist(strsplit(stackTag,sepTagFile,fixed=T)) #from tag+test+v1#
        mergedName<-paste(c('merged',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
        maskCount<-0
        tempBase<-'stack_tmp__'
        tempMapBase=paste0(tempBase,'map')
        tempMapBuffer=paste0(tempBase,'buffer')
        tempMapIn=tempMapBase
        tempMapOut=tempMapBase
        reg<-execGRASS('g.region',flags='p',intern=T)
        res<-reg[grep('nsres',reg)]
        res<-ceiling(as.numeric(gsub("[:]+|[[:space:]]+|[[:alpha:]]",'',res)))
        isFirstMap=TRUE
        rmRastIfExists('MASK')
        rmRastIfExists(paste0(tempBase,'*'))

        i=0
        message(paste('stack will be merged in this order:',paste(sel,collapse=', ')))
        updateProgressBar(session, inputId = "progMod2Stack", value =1, visible=TRUE)
        for(s in sel){
          message(paste('Map=',s))
          if(length(grep('stack_barrier__', s))>0){
            # if the map is a barrier map, create a new mask
            # with an optional buffer.
            maskCount=maskCount+1
            #dist<-if(buff){res}else{0.01}
            dist<-0.01
            execGRASS('r.buffer',input=s,output=tempMapBuffer,distances=dist,flags=c('overwrite'))
            rmRastIfExists('MASK')
            execGRASS('r.mask',raster=tempMapBuffer,flags=c('i')) 
            tempMapOut<-paste0(tempMapBase,'_',maskCount)
          }else{
            if(isFirstMap){
              message(paste(s,'is first item of stack'))
              execGRASS('r.mapcalc',expression=paste(tempMapOut,'=',s),flags='overwrite')
              tempMapIn=tempMapOut
              isFirstMap=FALSE
            }else{
              message(paste(s,'will be merged in with',tempMapIn))
              execGRASS('r.patch', input=paste0(tempMapIn,',',s),output=tempMapOut,flags=c('overwrite'))
              tempMapIn=tempMapOut
            }
          }
          i=i+1
          updateProgressBar(session, inputId = "progMod2Stack", value = i*inc)
        }

        tempMapList<-execGRASS('g.mlist',type='rast',pattern=paste0(tempMapBase,'*'),intern=TRUE)
        rmRastIfExists('MASK')
        message('tempMapList=',tempMapList)
        if(length(tempMapList)>1){  
          execGRASS('r.patch',input=paste(tempMapList,collapse=','),output=mergedName,flags=c('overwrite'))
        }else{
          execGRASS('g.copy',rast=paste0(tempMapList,',',mergedName),flags='overwrite')
        }
        rmRastIfExists('MASK*')
        rmRastIfExists(paste0(tempBase,'*'))
        message(paste(mergedName,'created'))
        updateProgressBar(session, inputId = "progMod2Stack", value =0, visible=FALSE)
      },
      error = function(cond){
        amErrHandler(errMsgList,conditionMessage(cond),title='Module 1')
    })},
      warning= function(cond){
        amErrHandler(errMsgList,conditionMessage(cond),title='Module 1')
      },
      message= function(cond){
        amMsg(session,'log',conditionMessage(cond),title='Module 1')  
      }

      )
  }
})












