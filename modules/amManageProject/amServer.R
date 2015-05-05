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
  amDebugMsg('projectList has changed, update select project input')
  updateSelectInput(
    session,
    inputId="selectProject",
    choices=projectList$loc
    )
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
    abline(v=bx$x$min,col='red',lty=3)
    abline(v=bx$x$max,col='red',lty=3)
    abline(h=bx$y$min,col='red',lty=3)
    abline(h=bx$y$max,col='red',lty=3)
    map.axes()
    plot(amBboxSp(mapMeta,proj='latlong'),add=TRUE,col='red')
   
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
  amErrorAction(title='Set new project',{
    if(isTRUE(!is.null(pN)) && isTRUE(nchar(pN)>0) ){
      #pNameUi<-amSubPunct(pN,config$sepTagUi)
      pNameFile<-amSubPunct(pN,config$sepTagFile,rmLeadingSep=T,rmTrailingSep=T,rmDuplicateSep=T)
      pNameAvailable<-isTRUE(!pNameFile %in% isolate(projectList$loc))
      pNameLength<-length(pNameFile)>0
      pChar<-nchar(pNameFile)
      moreChar<-NULL
      notAvailable<-NULL
      msgUpload<-NULL
      if(isTRUE(pNameLength)){
        #updateTextInput(session,'txtNewProjectName',value=pNameUi)
        if(pNameAvailable && pChar>3){ 
          newProjectName<-pNameFile
          msgUpload<-paste(pNameFile,'is available. Please choose a raster DEM.')
        }else{
          if(pChar<4) moreChar<-paste('Enter',4-pChar,'more characters.')
          if(!pNameAvailable) notAvailable<-paste('Project',pNameFile,'is not available.')
          newProjectName<-NULL
        }
      }
      amUpdateText(session,'hint-new-dem',paste(icon('info-circle'),moreChar,notAvailable,msgUpload))
    }else{
      newProjectName=NULL
    }
    listen$newProjectName<-newProjectName
    })
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
      amActionButtonToggle('fileNewDem',session,disable=TRUE)
      # remove gislock
      listen$gisLock<-NULL
      # upload function
      amUploadNewProject(newDem,newProjectName) 
      # extract all project list (should list the new one also)
      allGrassLoc<-grassListLoc(config$pathGrassDataBase)
      # test if new project realy exist
      locCreated<-newProjectName %in% allGrassLoc
      m=character(0)
      if(locCreated){
        # update project list reactive value
        projectList$loc<-allGrassLoc
        listen$newProjectUploaded<-runif(1)
        # update selected project
        m<-tagList(
            tags$b("Project '",newProjectName,"' created and available for analysis."),
            p("Check extent, projection and grid resolution used."),
            p(" Check also DEM layer in preview tab.")
            )
      }else{
        m<-tagList(
            tags$b("Project '",newProjectName,"' is not avaialbe."),
            p(" Something went wrong. Check logs tab for more information."),
            p(" Please report any issue to:",config$repository)
            )
      }
      amMsg(session,'message',title="AccessMod project settings",text=m)
    }
  }) 
})


observeEvent(listen$newProjectUploaded,{

  amUpdateText(session,'hint-new-dem',paste(icon('info-circle'),'Add another project name to unlock DEM upload.'))
  amDebugMsg('new project uploaded, change selected project and remove text in new name')
      updateSelectInput(session,"selectProject",selected=isolate(listen$newProjectName))
      updateTextInput(session,"txtNewProjectName",value="")
})


# if a project is selected, init a grass session and set gislock value in listen.
observe({
  selProject<-input$selectProject
  amErrorAction(title="Module project: set project selection in listener",{
    if(!is.null(selProject) && !selProject==""){
      listen$selProject=selProject
  }else{
    listen$selProject=NULL
    } 
  })
})


observe({
  project<-listen$selProject
  amErrorAction(title="Module project: init grass session",{
    if(!is.null(project)){
      gHome<-file.path(tempdir(),project)
      dir.create(gHome,showWarnings=F)
      amTimeStamp(project)
      listen$gisLock=NULL
      unset.GIS_LOCK()
      unlink_.gislock()
      initGRASS(
        gisBase = config$pathGrassBase70,
        home=gHome,
        gisDbase = config$pathGrassDataBase,
        location = project, 
        mapset= project, 
        override=TRUE)
      message('GIS process ',get.GIS_LOCK(),' started.')
      execGRASS('db.connect',driver='sqlite',database=config$pathSqliteDB)
      listen$dbCon<-dbConnect(SQLite(),system(paste("echo",config$pathSqliteDB),intern=T))
      execGRASS('g.region', raster=config$mapDem) 
      listen$mapset=project
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
  currentMapset<-listen$mapset
  selectedMapset<-isolate(input$selectProject)
  if(!is.null(currentMapset) && !identical(currentMapset,selectedMapset)){
    m<-paste("Current project (",currentMapset,") doesn't match selected project (",selectedMapset,"). AccessMod does not allow multiple sessions for one user and will revert to current project")
  amMsg(type='warning',text=m)
  }
})


observe({
  #sP<-input$selectProject
  sP<-listen$mapset
  gLock<-get.GIS_LOCK()
  if(length(sP)>0 && !sP==""){
    amDebugMsg('new get gisLock=',gLock)
    listen$gisLock<-gLock
  }else{
    listen$gisLock=NULL
  }
})


#update project name in title
observe({
  gL<-listen$gisLock
  if(!is.null(gL)){
    amUpdateText(session, 'proj-name',isolate(input$selectProject))
  }
})



