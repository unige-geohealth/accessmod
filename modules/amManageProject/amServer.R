#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
## Module Project :
# Project creation and selection
# Meta data visualisation


#update ui
observe({
  updateSelectInput(
    session,
    inputId="selectProject",
    choices=projectList$loc, 
    selected=isolate(listen$mapset))
})

# rendering
# plot map of the project extent.
output$locationMap<-renderPlot({
  mapMeta<-listen$mapMeta
  if(!is.null(mapMeta)){
    bx<-mapMeta$latlong$bbx$ext
    map("world",
        ylim=c(bx$y$min-30,bx$y$max+30),
        xlim=(c(bx$x$min-110,bx$x$max+110)),
        fill=TRUE, col=rgb(0.0,0.0,0.0)
    )
    title('Extent of the project in lat/long')
    abline(v=bx$x$min,col='blue',lty=3)
    abline(v=bx$x$max,col='blue',lty=3)
    abline(h=bx$y$min,col='blue',lty=3)
    abline(h=bx$y$max,col='blue',lty=3)
    map.axes()
    plot(amBboxSp(mapMeta,proj='latlong'),add=TRUE,col='blue')
   
  }
},bg='transparent')

# project meta : proj 4 string info text
output$infoProj4String<-renderUI({
  mapMeta<-listen$mapMeta
  amDebugMsg('update infoProj4String')
  if(!is.null(mapMeta)){
    tags$pre(mapMeta[['orig']]$proj)
  }
})

# project meta : grid information 
output$infoGrid<-renderUI({
  mapMeta<-listen$mapMeta
  if(!is.null(mapMeta)){
    tags$pre(HTML(listToHtml(mapMeta$grid,h=5)))
  }
})

# project meta : original projected extent
output$infoExtentProj<-renderUI({
  mapMeta<-listen$mapMeta
  if(!is.null(mapMeta)){
    tags$pre(HTML(listToHtml(mapMeta$orig$bbx$ext,h=5)))
  }
})

# project meta : lat/long extent
output$infoExtentLatLong<-renderUI({
  mapMeta<-listen$mapMeta
  if(!is.null(mapMeta)){
    tags$pre(HTML(listToHtml(mapMeta$latlong$bbx$ext,h=5)))
  }
})


# New project name validation
observe({
  #btn<-input$btnNewProject
  pN<-input$txtNewProjectName
  if(!is.null(pN)){
    pN<-amSubPunct(pN,config$sepTagUi)
    pN<-amSubPunct(input$txtNewProjectName,config$sepTagUi)
    pA<-!pN %in% isolate(projectList$loc)
    pL<-length(pN)>0
    pC<-nchar(pN)
    moreChar<-NULL
    notAvailable<-NULL
    msgUpload<-NULL
    if(pL){
      updateTextInput(session,'txtNewProjectName',value=pN)
      if(pA && pC>3){ 
        listen$newProjectName<-pN 
        msgUpload<-'Please choose a raster DEM.'
      }else{
        if(pC<4)moreChar<-paste('Enter',4-pC,'more characters.')
        if(!pA)notAvailable<-paste('Project',pN,'is not available.')
        listen$newProjectName<-NULL
      }
    }
    amUpdateText(session,'hint-new-dem',paste(icon('info-circle'),moreChar,notAvailable,msgUpload))
  }
})


# if valid project name is provided, set conditional style of upload DEM button.
observe({
  disable<-is.null(listen$newProjectName) 
  amActionButtonToggle('fileNewDem',session,disable=disable)
  if(!disable){
    amFileInputUpdate('fileNewDem',session,multiple=TRUE,accepts=config$filesAccept[['raster']])
  }
})

observe({
  # after upload process finished, shiny return a data frame with file info.
  # DF (newDem) names : "name"     "size"     "type"     "datapath"
  # this part will handle uploaded files, and set new grass region.
  newDem<-input$fileNewDem
  newProjectName<-amSubPunct(isolate(listen$newProjectName),'_')
  amErrorAction(title='Module project: upload new project',{
    if(length(newDem)>0 && length(newProjectName)>0){
      updateTextInput(session,"txtNewProjectName",value="")
      # main function.
      listen$gisLock<-NULL
      amUploadNewProject(newDem,newProjectName)
      amUpdateProjectList(listen)
      updateSelectInput(session,"selectProject",choices=isolate(projectList$loc),selected=newProjectName)
    }
  })
  
})



# if a project is selected, init a grass session and set gislock value in listen.
observe({
  selProject<-input$selectProject
  amErrorAction(title="Module project: init grass session",{
    if(!is.null(selProject) && !selProject==""){
      amTimeStamp(selProject)
      listen$gisLock=NULL
      unset.GIS_LOCK()
      unlink_.gislock()
      initGRASS(
        gisBase = config$pathGrassBase70,
        home=config$pathGrassHome,
        gisDbase = config$pathGrassDataBase,
        location = selProject, 
        mapset= selProject, 
        override=TRUE)
      message('GIS process ',get.GIS_LOCK(),' started.')
      execGRASS('db.connect',driver='sqlite',database=config$pathSqliteDB)
      listen$dbCon<-dbConnect(SQLite(),system(paste("echo",config$pathSqliteDB),intern=T))
      listen$mapset=selProject
      execGRASS('g.region', raster=config$mapDem) 
      if(amRastExists('MASK'))execGRASS('r.mask',flags='r')
      listen$mapMeta<-amMapMeta()
    }else{
      dbCon<-isolate(listen$dbCon)
      if(!is.null(dbCon))dbDisconnect(dbCon)
      message(paste('GIS process',get.GIS_LOCK(),' closed.'))
      listen$gisLock<-NULL
      listen$mapMeta<-NULL
      unset.GIS_LOCK()
      unlink_.gislock()
    } 
  })
})



observe({
  sP<-input$selectProject
  if(length(sP)>0 && !sP==""){
    amDebugMsg('new get gisLock=',get.GIS_LOCK())
    listen$gisLock<-get.GIS_LOCK()
  }else{
    listen$gisLock=NULL
  }
})




#update project name
observe({
  gL<-listen$gisLock
  if(!is.null(gL)){
    amUpdateText(session, 'proj-name',isolate(input$selectProject))
  }
})



