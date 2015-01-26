#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 2: calc accessibility to health facility.
# Uses r.walk.accessmod, a custom module made for GRASS GIS
#

#----------------------------------------{ UI
output$mod2<-renderUI({
  if(!is.null(listen$gisLock)){
    list(
      h3('Compute accessibility to health facilities'),
      busyIndicator("Please wait, long calculation",wait = 0),
      p('Create an anistropic cummulative cost map.'),
      fluidRow(formCreateTimeCostMap),
      hr()
      )
  }else{
    p(msgNoLocation)
  }
})



# first form element : create speed map and derived cumulative_cost map
formCreateTimeCostMap<-renderUI({
  mL<-dataList()
  mapMerged<-mL$merged
  mapHf<-mL$hf
  if(length(mapMerged)>0 && length(mapHf)>0){  
    list(
      sidebarPanel(
        h4('Compute map of cost'),
        bsProgressBar('progMod2',visible=FALSE),
        selectInput('mergedSelect','Select merged land cover map:',choices=mapMerged,selectize=F),
        selectInput('hfSelect','Select health facilities map:',choices=mapHf,selectize=F),
              radioButtons('typeAnalysis','Type of analalysis',
          c('Isotropic'='isotropic',
            'Anisotropic'='anistropic'
            ),
          selected='isotropic',
          inline=TRUE
          ),
        conditionalPanel(
          condition="input.typeAnalysis=='anistropic'",
          radioButtons('dirAnalysis','Direction of analysis',
            c("From facilities"="fromHf",
              "Towards facilities"="toHf"),
            selected='toHf',
            inline=TRUE
            )
          ),
           radioButtons("colTable","Use a color table for cummulative map",
          c(
            "White-blue-black" = "blue",
            "Yellow-green-blue-red-black" = "slope",
            "none" = "none"
            ),selected="slope"),
        numericInput('maxTimeWalk',
          label='Maximum transportation time [minutes]',
          value=120,
          min=0,
          max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
          step=1
          ),
        textInput('costTag','Add tags (minimum 1)',value=''),
        conditionalPanel(
          condition = "input.costTag.length > 0",
          actionButton('btnCreateTimeCostMap','Create cumulative cost map')
          )
        ),
      mainPanel(
        list(
          h4('TimeCost model table'),    
          p('Edit this table or copy and paste cells from a spreadsheet'),
          p("Accessmod doesn't store this table (yet). Please save your modifications in a spreadsheet."),
          hr(),
          hotable("mergedMapCatTable"),
          hr(),
          p(list(strong('Class:'),'merged land cover class')),
          p(list(strong('Label:'),'description of class')),
          p(list(strong('Speed:'), 'speed estimate in [km/h] on flat surface')),
          p(list(strong('Mode'), 'mode of transportation :',names(transpModList)))

          )
        ))
  }else{
    sidebarPanel(
      if(length(mapMerged)==0){
        p('No merged land cover map found. Please import one or compute one with module 1')
      }else if(length(mapHf)==0){
        p('No health facilities map found. Please import one for this module.')  
      }   
      )
  }
})


#----------------------------------------{ Reactivity

# selectize input populate


observe({
updateSelectizeInput(session,'mergedSelect',choices=dataList()$merged)
})




# name validation
observe({
  costTag<-input$costTag
  if(!is.null(costTag) && ! costTag==''){
    costTag<-unlist(costTag)
    cumulativeName<-paste(c('cumulative_cost',paste(costTag,collapse=sepTagFile)),collapse=sepTagPrefix )
    if(cumulativeName %in% isolate(dataList()$rast)) msg(paste('Warning: map',cumulativeName,'already exists and will be overwritten. Select other tags to avoid overwriting.'))
    updateTextInput(session,'costTag',value=autoSubPunct(costTag,sepTagUi))
  }
})





# reactive expression to create model table from the categories of land cover merged map
mergedMapCatTable<-reactive({
  #reactive dependencies
  listen$gisLock
  dataList()
  sel<-input$mergedSelect
  if(!is.null(sel) && !sel==''){
    tblCat<-read.csv(
      text=execGRASS('r.category',
        map=sel,
        intern=T),
      sep='\t',
      header=F,
      stringsAsFactors=F
      )
    names(tblCat)<-c('Class','Label')
    tblCat[,'Speed']<-as.integer(0)
    tblCat[,'Mode']<-as.character('NONE')
    #tblCat[is.na(tblCat)]<-''
    return(tblCat)
  }else{
    return(NULL)
  }
})

# If new map is selected, update hotable
observe({
  sel<-input$mergedSelect
  if(!is.null(sel)){
    tbl<-mergedMapCatTable()
    if(!is.null(tbl)){
      output$mergedMapCatTable<- renderHotable({tbl}, readOnly = FALSE)
    }
  }
})


#validate if table is updated
observe({
  tblUpdated<-hot.to.df(input$mergedMapCatTable)
  tblOriginal<-mergedMapCatTable()
  testNrow<-nrow(tblUpdated)==nrow(tblOriginal)
  if(!is.null(tblUpdated) && !is.null(tblOriginal) && testNrow){
    # rule 1: do not allow changing class and label
    tblValidated<-data.frame(c(tblOriginal[,c('Class','Label')],tblUpdated[,c('Speed','Mode')]))
    # rule 2: if Speed is not integer, set to 0
    s<-as.integer(tblUpdated$Speed)
    s[is.na(s)]<-as.integer(0)
    # rule 3: if mode is not in allowedModTransp choices, set to NONE
    m<-toupper(tblUpdated$Mode)
    mTest<- m %in% names(transpModList)
    m[!mTest]<-'NONE'
    # update with validated values
    tblValidated$Mode<-m
    tblValidated$Speed<-s
    output$mergedMapCatTable<- renderHotable({tblValidated}, readOnly = FALSE)
  }
})

# main function to launch grass r.walk.accessmod
observe({
  btn<-input$btnCreateTimeCostMap
  tbl<-isolate(hot.to.df(input$mergedMapCatTable))
  costTag<-isolate(input$costTag)
  mergedSelect<-isolate(input$mergedSelect)
  hfSelect<-isolate(input$hfSelect)

  #tropicSelect<-isolate(input$tropicSelect)
  #directionSelect<-isolate(input$directionSelect)

  maxTimeWalk<-isolate(input$maxTimeWalk)
  #knight<-isolate(input$knight)
  dirAnalysis<-isolate(input$dirAnalysis)
  typeAnalysis<-isolate(input$typeAnalysis)
  colorTable<-isolate(input$colTable)

  if(!is.null(btn) && btn>0){
    tryCatch({
      # set rules 
      # 0 kmh speed could lead to infinite values when calculate cost in sec (division by 0)
      if(any(tbl$Speed==0))stop(
        'Speed of zero km/h is not allowed. Please remove category from the merged map or set a speed value.'
        )
      updateProgressBar(session,'progMod2',5, visible=TRUE)
      # return path = towards facilities.
      returnPath<-ifelse(dirAnalysis=='toHf',TRUE,FALSE)

      # max cost from minutes to seconds
      maxCost<-maxTimeWalk*60

      # set a max value for null values. 
      #maxValNull<-(ceiling(maxCost/1e4)*1e4)-1

      msg(paste('Module 2:',typeAnalysis,'analysis requested for ',mergedSelect,'requested'))
      tagSplit<-unlist(strsplit(costTag,sepTagUi,fixed=T))

      # maps names
      speedName<-paste(c('speed',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
      costName<-paste(c('friction',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)
      cumulativeName<-paste(c('cumulative_cost',paste(tagSplit,collapse='_')),collapse=sepTagPrefix)



      if(typeAnalysis=='anistropic'){
      # ANISOTROPIC using r.walk.accessmod.
        # creation of new classes for speed map (class+km/h), used in r.walk.accessmod
        # Exemples of rules: 
        # oldClasses = newClasses \t newlabels
        # 1 2 3 = 1002 \t WALKING:2
        # 4 =  2020 \t BICYCLING:20
        # 1002 = 3080 \t NONE:80
        tbl[,'NewClass']<-integer()
        # for each row of the model table...
        for(i in 1:nrow(tbl)){
          #... get the mode
          mod<-tbl[i,'Mode']
          #... corrsponding to the predefined value from transpModList + given speed
          tbl[i,'NewClass']<-transpModList[[mod]]$rastVal+tbl[i,'Speed']
        }
        # unique new class
        uniqueNewClass<-unique(tbl$NewClass)
        reclassRules<-character()
        for(u in uniqueNewClass){
          oldClasses<-tbl[tbl$NewClass==u,'Class']
          modeSpeedLabel<-paste(tbl[tbl$NewClass==u,c('Mode','Speed')][1,],collapse=':')
          classRule<-paste(paste(oldClasses,collapse=' '),'=',u,'\t',modeSpeedLabel)
          reclassRules<-c(reclassRules,classRule)
        }
        tmpFile<-tempfile()
        write(reclassRules,tmpFile)
        execGRASS('r.reclass',
          input=mergedSelect,
          #output='tmp__speed',
          output=speedName,
          rules=tmpFile,
          flags='overwrite')
        msg(paste('Module 2:',mergedSelect,'reclassed and saved to new map (',speedName,')'))
        updateProgressBar(session,'progMod2',10)

        #flags=c(c('overwrite','s'),ifelse(knight,'k',''),ifelse(returnPath,'t',''))
        flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
        flags<-flags[!flags %in% ""]
        msg(paste('Module 2 : flags used:',paste(flags,collapse=',')))

        execGRASS('r.walk.accessmod',
          elevation='dem',
          friction=speedName,
          output=cumulativeName,
          start_points=hfSelect,
          max_cost=maxCost, # max cost in seconds.
          flags=flags
          )
        msg(paste('Module 2: r.walk.accessmod for ',mergedSelect,'done. Output map:',cumulativeName))
        updateProgressBar(session,'progMod2',80)


      }else{
        # ISOTROPIC using r.cost.
        # creaction of new classes for cost map (seconds) used in r.cost. 
        tbl[,'NewClass']<-numeric()
        tbl[,'Mode']<-'isotropic'
        # for each row of the model table...
        for(i in 1:nrow(tbl)){
          # km/h to s/m * 1000 (r.reclass works only in integer)
          tbl[i,'NewClass']<- (1/(tbl[i,'Speed']/3.6))*1000*gmeta6()$nsres
        }
        # unique new class
        uniqueNewClass<-unique(tbl$NewClass)
        reclassRules<-character()
        for(u in uniqueNewClass){
          oldClasses<-tbl[tbl$NewClass==u,'Class']
          modeSpeedLabel<-paste(tbl[tbl$NewClass==u,c('Mode','Speed')][1,],collapse=':')
          classRule<-paste(paste(oldClasses,collapse=' '),'=',u,'\t',modeSpeedLabel)
          reclassRules<-c(reclassRules,classRule)
        }
        tmpFile<-tempfile()
        write(reclassRules,tmpFile)
        execGRASS('r.reclass',
          input=mergedSelect,
          #output='tmp__speed',
          output=costName,
          rules=tmpFile,
          flags='overwrite')
        msg(paste('Module 2:',mergedSelect,'reclassed and saved to new map (',speedName,')'))
        updateProgressBar(session,'progMod2',10)
        #flags=c(c('overwrite','s'),ifelse(knight,'k',''),ifelse(returnPath,'t',''))
        flags=c('overwrite')
        msg(paste('Module 2 : flags used:',paste(flags,collapse=',')))
        execGRASS('r.cost',
          input=costName,
          output=cumulativeName,
          start_points=hfSelect,
          max_cost=maxCost*1000,
          flags=flags
          )
        execGRASS('r.mapcalc',expression=paste(cumulativeName,'=',cumulativeName,'/1000'),flags='overwrite')
        msg(paste('Module 2: r.cost for ',mergedSelect,'done. Output map:',cumulativeName))
        updateProgressBar(session,'progMod2',80)


      }
      # remove over passed values :
      # r.walk check for over passed value after last cumulative cost :
      # so if a new cost is added and the new mincost is one step further tan
      # the thresold, grass will keep it and stop algorithm from there.
      if(TRUE){
        execGRASS('r.mapcalc',expression=paste(
            "tmp__map=if(",cumulativeName,"<=",maxCost,",",cumulativeName,",null())"
            ),flags=c('overwrite')
          )
        execGRASS('r.mapcalc',expression=paste(
            cumulativeName,"=tmp__map"
            ),flags=c('overwrite')
          )
        execGRASS('g.remove',rast='tmp__map')
      }

        msg(paste('Module 2: cells values superior to ', maxCost,' set to null for ', cumulativeName))
        updateProgressBar(session,'progMod2',95)
      switch(colorTable,
        none=msg('Module 2 : no table selected'),
        blue={
          msg('Module 2 : blue table color table ')
          tempF<-tempfile()
          execGRASS('r.null',map=cumulativeName, null=65535)
          createColorTable(maxCost,paletteFun=paletteBlue,filePath=tempF)
          execGRASS('r.colors',map=cumulativeName,rules=tempF)
        },
        slope={
          execGRASS('r.colors',map=cumulativeName,color='slope',flags ="e")
        },
        )
      msg(paste('Module 2: color table set for ',cumulativeName, 'set to',colorTable))
updateProgressBar(session,'progMod2',100)
    },
    error=function(c){ 
      msg(c)
    },
      finally=updateProgressBar(session,'progMod2',0, visible=FALSE)
      )
  }

})




