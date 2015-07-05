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
    choices=grassSession$locations
    )
})


observe({
  loc = grassSession$locations
  loc = loc[! loc %in% grassSession$mapset]
 updateSelectInput(
   session,
   inputId="selectProjectToDelete",
   choices=loc
   )
})


observeEvent(input$btnDelProject,{
  amErrorAction(title="Module project: project deletion",{
    deleteConfirmation <- tagList( 
      div(style="
        opacity:0.3; 
        background: #000; 
        width:100%;
        height:100%; 
        z-index:9999;
        top:0; 
        left: 0; 
        position:fixed;"),
        div(class='panel',style="
          width:500px;
          height:400px;
          overflow-y:scroll;
          z-index:10000;
          top:30%; 
          left:50%; 
          position:fixed;
          padding:50px;
          ",
          tagList(
            h4(paste("Project",input$selectProjectToDelete),"will be deleted with every dataset, settings and archives. This can't be undone."),
            p("Advice : select all your data in the data manager, create an archive and export it before proceeding."),
            div(style="left:50%;margin-top:20%",
            p(tags$b("Confirm project deletion:")),
            actionButton('btnConfirmDelProject',"Delete",style="width:100px"),
            actionButton('btnCancelDelProject',"Cancel",style="width:100px")
            )
            )
          )
      )
    output$amModalConfirmation <- renderUI(deleteConfirmation) 
   })
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
    title(mapMeta$location)
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
    ext<-mapMeta$orig$bbx$ext
    names(ext)<-c('Easting','Northing')
    tags$pre(HTML(listToHtml(ext,h=5)))
  }
})

# project meta : lat/long extent
output$infoExtentLatLong<-renderUI({
  mapMeta<-listen$mapMeta
  if(!is.null(mapMeta)){
    ext<-mapMeta$latlong$bbx$ext
    names(ext)<-c('Longitude','Latitude')
    tags$pre(HTML(listToHtml(ext,h=5)))
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
      pNameAvailable<-isTRUE(!pNameFile %in% isolate(grassSession$locations))
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
      grassSession$gisLock<-NULL
      # upload function
      amUploadNewProject(newDem,newProjectName) 
      # extract all project list (should list the new one also)
      allGrassLoc<-amGetGrassListLoc(config$pathGrassDataBase)
      # test if new project realy exist
      locCreated<-newProjectName %in% allGrassLoc
      m=character(0)
      if(locCreated){
        # update project list reactive value
        grassSession$locations<-allGrassLoc
        listen$newProjectUploaded<-runif(1)
        # update selected project
        m<-tagList(
            tags$b("Project '",newProjectName,"' created and available for analysis."),
            p("Please check extent, projection and grid resolution used."),
            p("Please check also DEM layer in preview tab.")
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
  project <- listen$selProject
  amErrorAction(title="Module project: init grass session",{
    if(!is.null(project)){
      gHome <- file.path(tempdir(),project)
      dir.create(gHome,showWarnings=F)
      amTimeStamp(project)
      grassSession$gisLock <- NULL
      unset.GIS_LOCK()
      unlink_.gislock()
      initGRASS(
        gisBase  = config$pathGrassBase70,
        gisDbase = config$pathGrassDataBase,
        home     = gHome,
        location = project,
        mapset   = project,
        override = TRUE)
      message('GIS process ',get.GIS_LOCK(),' started.')
      dbSqlitePath<-system(paste("echo",config$pathSqliteDB),intern=T)
      grassSession$dbCon <- dbConnect(SQLite(),dbSqlitePath)
      execGRASS('db.connect',driver='sqlite',database=dbSqlitePath)
      execGRASS('g.region', raster=config$mapDem) 
      grassSession$mapset <- project
      grassSession$gisLock <- get.GIS_LOCK()
      if(amRastExists('MASK')) execGRASS('r.mask',flags='r')
      listen$mapMeta <- amMapMeta()
      amUpdateDataList(listen)
    }else{
      dbCon <- isolate(grassSession$dbCon)
      if(!is.null(dbCon)) dbDisconnect(dbCon)
      message(paste('GIS process',get.GIS_LOCK(),' closed.'))
      grassSession$gisLock <- NULL
      listen$mapMeta <- NULL
      unset.GIS_LOCK()
      unlink_.gislock()
      amUpdateDataList(listen)
    } 
  })
})


observe({
  currentMapset<-grassSession$mapset
  selectedMapset<-isolate(input$selectProject)
  if(isTRUE(nchar(selectedMapset)>0)){
    if(!is.null(currentMapset) && !identical(currentMapset,selectedMapset)){
      m<-paste("Someone has set current project to '",currentMapset,"' while your session was set to '",selectedMapset,"'. AccessMod is limited to one project at a time by user. Selected project has been synchronized with actual project.")
      amMsg(type='log',text=m)
      updateSelectInput(session,'selectProject',selected=currentMapset)
    }
  }
})


observe({
  #sP<-input$selectProject
  sP<-grassSession$mapset
  gLock<-get.GIS_LOCK()
  if(length(sP)>0 && !sP==""){
    amDebugMsg('new get gisLock=',gLock)
    grassSession$gisLock<-gLock
  }else{
    grassSession$gisLock=NULL
  }
})


#update project name in title
observe({
  gL<-grassSession$gisLock
  if(!is.null(gL)){
    amUpdateText(session, 'proj-name',isolate(input$selectProject))
  }
})



