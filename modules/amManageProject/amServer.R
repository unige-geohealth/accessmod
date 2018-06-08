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

  # select default loc
  oldLoc <- isolate({ input$amCookies$am5_location })
  loc <- grassSession$locations
  locSel <- loc[[1]]
  if( !is.null(oldLoc) && isTRUE( oldLoc %in% loc )){
    locSel <- oldLoc
  }
  if(amNoDataCheck(locSel)) locSel <- config$defaultNoData

  # set input
  updateSelectInput(
    session,
    inputId="selectProject",
    choices=loc,
    selected=locSel
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
  amErrorAction(
    title="Module project: project deletion confirmation",{
    content  <- tagList(
      p(paste("Project",input$selectProjectToDelete),"will be deleted with every dataset, settings and archives. This can't be undone.")
      )
    aBtns = list(
      actionButton('btnConfirmDelProject',"Delete")
      )
    amUpdateModal(panelId="amModal",title="Confirmation",html=content,listActionButton=aBtns,addCancelButton=TRUE)
   })
})

  # in case of project deletion, unlink project files (delete) and update data list
  observeEvent(input$btnConfirmDelProject,{
    amErrorAction(title='Module project : project deletion',{
     amUpdateModal("amModal",close=TRUE) 
      project <- input$selectProjectToDelete
      projectList <- grassSession$locations
      if(project %in% grassSession$locations){
        projPath <- file.path(config$pathGrassDataBase, project)
        if(file.exists(projPath)){
          unlink(projPath, recursive = TRUE, force = TRUE)
          grassSession$locations <- amGetGrassListLoc(grassDataBase=config$pathGrassDataBase)
        }else{
          stop(paste("Error:",project,"files (",projPath,") are not found. Please report this bug."))
        }
      }else{
        stop(paste("Error: selected proejct (",project,") is not present in project list (",projectList,"). Please report this bug."))
      }
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
      amUpdateText(id='hint-new-dem',paste(icon('info-circle'),moreChar,notAvailable,msgUpload))
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

observeEvent(input$fileNewDem,{
  # after upload process finished, shiny return a data frame with file info.
  # DF (newDem) names : "name"     "size"     "type"     "datapath"
  # this part will handle uploaded files, and set new grass region.
  newDem<-input$fileNewDem
  newProjectName<-amSubPunct(listen$newProjectName,'_')
  amErrorAction(title='Module project: upload new project',{
    if(length(newDem)>0 && length(newProjectName)>0){

      pBarTitle = "Upload new project file"

      pbc(
          visible=TRUE,
          percent=1,
          title=pBarTitle,
          text="Start project importation"
          )



      amActionButtonToggle('fileNewDem',session,disable=TRUE)
      # remove gislock
      grassSession$gisLock<-NULL
      # upload function
      amUploadNewProject(newDem,newProjectName,pBarTitle) 
      # extract all project list (should list the new one also)
      allGrassLoc <- amGetGrassListLoc(config$pathGrassDataBase)
      # test if new project realy exist
      locCreated<-newProjectName %in% allGrassLoc
      m=character(0)
      if(locCreated){
        # update project list reactive value
        grassSession$locations<-allGrassLoc
        listen$newProjectUploaded<-runif(1)
        # update selected project

       pbc(
          visible=TRUE,
          percent=100,
          title=pBarTitle,
          text="Project created and ready to use. Please verify the project's extent and resolution !",
          timeOut=4
          )

       pbc(
          visible=FALSE,
          percent=0,
          title=pBarTitle,
          text="",
          timeOut=0
          )


      }else{

        pbc(
          visible=TRUE,
          percent=100,
          title=pBarTitle,
          text="Project missing, something went wrong...",
          timeOut=4
          )
       
        pbc(
          visible=FALSE,
          percent=0,
          title=pBarTitle,
          text="",
          timeOut=0
          )


        m<-tagList(
            p(" Something went wrong, the project is absent from the database."),
            p(" Plase check logs tab for more information."),
            p(" Please report any issue to:",config$repository)
            )
      amMsg(session,'message',title="AccessMod project settings",text=m)
      

      }
    }
  }) 
})


observeEvent(listen$newProjectUploaded,{
  amUpdateText('hint-new-dem',paste(icon('info-circle'),'Add another project name to unlock DEM upload.'))
  amDebugMsg('new project uploaded, change selected project and remove text in new name')
      updateSelectInput(session,"selectProject",selected=isolate(listen$newProjectName))
      updateTextInput(session,"txtNewProjectName",value="")
})


# if a project is selected, init a grass session and set gislock value in listen.
observe({
  selProject<-input$selectProject
  amErrorAction(title="Module project: set project selection in listener",{
    if(!amNoDataCheck(selProject)){
      amSetCookie(
        cookies=list("am5_location"=selProject))  
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
      grassSession$dbCon <- dbConnect(RSQLite::SQLite(),dbSqlitePath)
      execGRASS('db.connect',driver='sqlite',database=dbSqlitePath)
      execGRASS('g.region', raster=config$mapDem) 
      grassSession$mapset <- project
      grassSession$gisLock <- get.GIS_LOCK()
      if(amRastExists('MASK')) execGRASS('r.mask',flags='r')
      listen$mapMeta <- amMapMeta()
      listen$outFiles<- NULL
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
    amUpdateText('projName',isolate(input$selectProject))
  }
})



