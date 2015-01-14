#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module project : chose existing project or set a new one.


# ui skeleton
output$modProject<-renderUI({
  list(
    sidebarPanel(
      controlPanel
      ),
    mainPanel(
      infoPanel
      )
    )
})

# left control panel
controlPanel<-renderUI({
  tagList(
    busyIndicator("Please wait..",wait=0),
    tags$h4(icon('crosshairs'),'Project'),
    selectizeInput("selectProject","Select a project",isolate(projectList()),selected="",width='100%'),
    bsToggleButton("btnNewProject", label = "New project",style='success'),
    hr(),
    formNewProject,
    formNewProjectHint,
    formDemInput
    )
})

# if selected project is valide put in listen list.
observe({
  # validate input select location. not very usefull : mostly duplicate input$selectProject
  selectProject<-input$selectProject
  if(!is.null(selectProject) && length(selectProject)>0){
    listen$selectProject<-selectProject
  }else{
    listen$selectProject<-NULL 
  }
})

# if a new project btn toggle is TRUE, show text input 
formNewProject<-renderUI({
  btn<-input$btnNewProject
  if(!is.null(btn)&&btn){
    textInput('txtNewProjectName','Enter a new available project name (min 4 characters)',value='')
  }
})

# project name validation : no punctuation, unique project name, min 4 chars
observe({
  pN<-autoSubPunct(input$txtNewProjectName)
  pA<-!pN %in% projectList()
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
      if(!pA)notAvailable<-paste('Project',pN,'not available.')
      listen$newProjectName<-NULL
    }
  }
  listen$newProjectHint=paste(moreChar,notAvailable,msgUpload)
})

# render project naming hint.
formNewProjectHint<-renderUI({
  btn<-input$btnNewProject
  if(!is.null(btn)&&btn){
    p(listen$newProjectHint)
  }
})


# if valid project name is provided, update style 
observe({
  updateStyle(
    id='txtNewProjectName',
    type=ifelse(!is.null(listen$newProjectName),'o','e'),
    element='border'
    )  
})

txtAddNewDem<-"Upload the DEM raster map to automatically set the project's resolution, extent and projection metadata. Accessmod expects a map in metric system with an equal area projection. ESRI grids (multiples files) or GeoTIFF (one file) are supported."


# if valid project name is provided, set conditional style of upload DEM button.
formDemInput<-renderUI({
  btn<-input$btnNewProject
  pN<-!is.null(listen$newProjectName)
  message('t')
  if(btn){
    if(pN){
      amFileInput('fileNewDem','Upload DEM',multiple=TRUE,accept=acceptRaster,style='success')
    }else{ 
      amFileInput('fileNewDem','Upload DEM',multiple=TRUE,accept=acceptRaster,style='danger',disable=TRUE)
    }
  }
})

observe({
  # after upload process finished, shiny return a data frame with file info.
  # DF (newDem) names : "name"     "size"     "type"     "datapath"
  newDem<-input$fileNewDem
  newProjectName<-isolate(listen$newProjectName)
  if(!is.null(newDem) && !newDem=='' && !is.null(newProjectName) && !newProjectName==''){
    # capture all error from now, from potentially error prone steps.
    tryCatch({
      msg(paste('DEM uploaded for location',newProjectName))

      # TODO:How to know wich file to select if multiple file are uploaded ?
      # It can vary with data format. 
      # here, we guess that's the heaviest file that contains data.
      # Other dependencies such as proj.adf, prj files should be recognized by gdal 
      newDem<-newDem[with(newDem, order(-size)),]
      tmpDir<-dirname(newDem[1,'datapath'])
      newDem$newPath<-file.path(tmpDir,newDem$name)
      file.rename(newDem$datapath,newDem$newPath)

      # raster validation.
      validateFileExt(newDem$name,'rast')

      # take the first raster (heavier) as the base map
      tmpMapPath<-newDem[1,'newPath']

      # test for projection issues 
      r<-raster(tmpMapPath)
      destProj<-proj4string(r) 
      if(is.na(destProj))stop(msgNoProj)
      if(!length(grep('+to_meter|+units=m',destProj))>0)stop(msgNotMetric)

        # get proj4string
        msg(paste('Projection detected:',destProj));
        msg('Importation in grass')
        # empty grid for the default WIND object
        msg('Raster as SpatialGrid...',verbose=verbMod)
        #sg<-as(r,'SpatialGridDataFrame')
        sg<-as(r,'SpatialGrid')
        # grass initialisation.
        msg('Init new GRASS session...',verbose=verbMod)
        initGRASS(gisBase = grassBase70, # binary files (grass 7.0)
          home=grassHome,
          gisDbase = grassDataBase, # local grass database
          location = newProjectName, # rsession
          mapset= 'PERMANENT', # PERMANENT for dem.
          SG=sg,
          override=TRUE)
        listen$gisLock<-get.GIS_LOCK()
        execGRASS('g.proj',flags='c',proj4=destProj)
        execGRASS('db.connect',driver='sqlite',database='$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db')

        # assign('gisLock',get.GIS_LOCK(),envir=globalenv())
        #gisLock=get.GIS_LOCK()
        # set as default region
        execGRASS('g.gisenv',flags='s')

        msg('Grass initialised, writing DEM as base map.')
        execGRASS('r.in.gdal',
          input=tmpMapPath,
          output='dem',
          flags=c('overwrite','quiet'),
          title=paste(newProjectName,'DEM')
          )

        # set default region on DEM and set zeros instead of null.
        execGRASS('g.region', rast='dem')
        msg('g.region set to dem map')
        execGRASS('r.null',map='dem',null=0)# usefull when null are set for sea level.
        msg('Updated null value in base map')
        # clean gislock and unlink (remove) gislock files
        listen$gisLock<-NULL
        listen$newProjectName<-NULL
        unset.GIS_LOCK()
        unlink_.gislock()
        # remove temp dir
        unlink(tmpDir, recursive = TRUE)
        # toggle button new project. TODO : doesn't work, why ?
        updateButton(
          session=session,
          id='btnNewProject',
          value=FALSE
          )
        # projectList update
        updateSelectizeInput(
          session=session,
          inputId='selectProject',
          choices=projectList(),
          selected=newProjectName)
    },
    # handle errors. "Message" type disable because grass send too much information.
    error = function(cond){
      unlink(tmpDir,recursive=TRUE)
      # TODO: write messages in db / list 
      hintBadRasterOrig<-'Cannot create a RasterLayer object from this file'
      hintBadRasterConvert<-'Error : file not recognized, make sure you have uploaded a supported raster files, with all its dependencies.'
      cndMsg <- conditionMessage(cond)
      if(length(grep(hintBadRasterOrig,cndMsg))>0){
        cond<-hintBadRasterConvert
      } 
      msg(paste('DEM importation failed:',cond))}
    )
  }
})

# if a project is selected, init a grass session and set gislock value in listen.
observe({
  sP<-listen$selectProject
  sM<-"PERMANENT"
  if(!is.null(sP) && !sP==""){
    message(sP)
    tryCatch({
      unset.GIS_LOCK()
      unlink_.gislock()
      initGRASS(
        gisBase = grassBase70,
        home=grassHome,
        gisDbase = grassDataBase,
        location = sP, 
        mapset= sM, 
        override=TRUE)
      msg(paste('GIS process id: ',get.GIS_LOCK()))
      print(gmeta6(ignore.stderr = T))
      execGRASS('db.connect',driver='sqlite',database='$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db') 
      listen$gisLock<-get.GIS_LOCK()
    },
    error = function(c) msg(paste('GRASS init error:',c)),
    warning = function(c) msg(paste('GRASS init  warning:',c))
    )
  }else{
    listen$gisLock<-NULL
    unset.GIS_LOCK()
    unlink_.gislock()
    msg(paste('GIS process closed.'),verbose=FALSE)
  }
})

# if a gislock is set, show meta data of linked project / location.
infoPanel<-renderUI({
  if(!is.null(listen$gisLock)){
    loc<-isolate(listen$selectProject)
    locationExt<-as(extent(gmeta2grd()),'SpatialPolygons')
    proj4string(locationExt)<-getLocationProj()
    locationExtLongLat<- spTransform(locationExt,CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
    gL<-gmeta6()
    metaList<-list(
      "North-south resolution:"               = gL$nsres,
      "East-west reolution"                   = gL$ewres,
      "Bounding box (xmin, xmax, ymin, ymax)" = locationExt@bbox,
      "Number of cell"                        = gL$cells,
      "Number of rows"                        = gL$rows,
      "Number of columns"                     = gL$cols
      )
    metaHtml<-listToHtml(metaList,h=6)
    panInfoList<-tagList(
      tags$h4(paste('Location info:',loc)),
      tags$h5('Projection used:'),
      tags$pre(getLocationProj(ignore.stderr = T)),
      tags$h5('General map'),
      renderPlot({
        map('world',fill=FALSE)
        plot(locationExtLongLat,add=TRUE,col='red')
      }),
    tags$h5('Grass project:'),
    tags$pre(HTML(metaHtml))
    )
    panel('success','Project informations',panInfoList)
  }else{
    panel('warning','Project informations',p(msgNoLocation)) 
  }
})




# Show project name in title.
output$title<-renderUI({
  if(!is.null(listen$gisLock)){
    locSelect<-isolate(listen$selectProject)
    h5(iconSmall,' ',title,'(',locSelect,')')
  }else{
    h5(iconSmall,' ',title)
  }
})

# if a gislock is set, show modules UI.
output$modAccesmod<-renderUI({
  if(!is.null(listen$gisLock)){
    tabsetPanel(
      tabPanel('Module 1',uiOutput('mod1')), # Module 1
      tabPanel('Module 2',uiOutput('mod2')), # Module 2
      tabPanel('Module 3',uiOutput('mod3')), # Module 3
      tabPanel('Module 4',uiOutput('mod4')), # Module 4
      tabPanel('Module 5',uiOutput('mod5')) # Module 5
      )
  }else{
    panel('warning','No project selected',p(msgNoLocation))
  }
})








