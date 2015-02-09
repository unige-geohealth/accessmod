#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module project : chose existing project or set a new one.

# devel : custom browser() call
observe({
sB<-input$showBrowser
if(!is.null(sB) && sB>0){
browser()
}
})


# ui skeleton
output$moduleProject<-renderUI({
  # conditional panel display
  fluidRow(
      controlPanel,
      infoPanel
    )
})

# left control panel
controlPanel<-renderUI({
  sidebarPanel(width=3,
    tagList(
      tags$h4(icon('play-circle'),'Project'),
      actionButton('showBrowser','Show browser'),
      selectInput("selectProject","Select a project",choices=projectList$loc),
      tags$h4(icon('plus-circle'),'New Project'),
      textInput('txtNewProjectName','Enter a new available project name (min 4 characters)',value=''),
      tags$p(tags$b(id='hint-new-dem',icon('info-circle'),'Enter new name')),
      amFileInput('fileNewDem','Upload DEM')
      )
    )
})

#observe({
#input$selectProject
#listen<-reactiveValues(reset=runif(1))
#})
#


## if selected project is valide put in listen list.
#observe({
#  # validate input select location. not very usefull : mostly duplicate input$selectProject
#  selectProject<-input$selectProject
#  if(!is.null(selectProject) && length(selectProject)>0){
#    input$selectProject<-selectProject
#  }else{
#    input$selectProject<-NULL 
#  }
#})

## if a new project btn toggle is TRUE, show text input 
#formNewProject<-renderUI({
#  btn<-input$btnNewProject
#  if(!is.null(btn)&&btn){
#   
#  }
#})

# project name validation : no punctuation, unique project name, min 4 chars
observe({
  #btn<-input$btnNewProject
  pN<-input$txtNewProjectName
  if(!is.null(pN)){
  pN<-autoSubPunct(pN,sepTagUi)
    pN<-autoSubPunct(input$txtNewProjectName,sepTagUi)
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

# if valid project name is provided, update style 
#observe({
#  updateStyle(
#    id='txtNewProjectName',
#    type=ifelse(!is.null(listen$newProjectName),'o','e'),
#    element='border'
#    )
#})


txtAddNewDem<-"Upload the DEM raster map to automatically set the project's resolution, extent and projection metadata. Accessmod expects a map in metric system with an equal area projection. ESRI grids (multiples files) or GeoTIFF (one file) are supported."


# if valid project name is provided, set conditional style of upload DEM button.
observe({
  disable<-is.null(listen$newProjectName) 
  message(listen$newProjectName)
  amActionButtonToggle('fileNewDem',session,disable=disable)
  if(!disable){
    amFileInputUpdate('fileNewDem',session,multiple=TRUE,accepts=acceptFiles[['rast']])
  }
})

observe({
  # after upload process finished, shiny return a data frame with file info.
  # DF (newDem) names : "name"     "size"     "type"     "datapath"
  # this part will handle uploaded files, and set new grass region.
  newDem<-input$fileNewDem
  newProjectName<-autoSubPunct(isolate(listen$newProjectName),'_')
  if(!is.null(newDem) && !newDem=='' && !is.null(newProjectName) && !newProjectName==''){
    amErrorAction(title='Module project: upload new project',{
      # main function.
      listen$gisLock<-NULL
      amUploadNewProject(newDem,newProjectName)
      amUpdateProjectList(listen)
      updateSelectizeInput(session,"selectProject",selected=newProjectName)
    })
  }
})

# if a project is selected, init a grass session and set gislock value in listen.
observe({
  sP<-input$selectProject
  sM<-"PERMANENT"
  amErrorAction(title="Module project: init grass session",{
    if(!is.null(sP) && !sP==""){
      listen$gisLock=NULL
      unset.GIS_LOCK()
      unlink_.gislock()
      initGRASS(
        gisBase = grassBase70,
        home=grassHome,
        gisDbase = grassDataBase,
        location = sP, 
        mapset= sM, 
        override=TRUE)
      message('GIS process ',get.GIS_LOCK(),' started.')
      print(gmeta6(ignore.stderr = T))
      execGRASS('db.connect',driver='sqlite',database='$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db') 
      listen$dbCon<-dbConnect(SQLite(),system(paste("echo",sqliteDB),intern=T))
      listen$gisLock<-get.GIS_LOCK()
    }else{
      dbCon<-isolate(listen$dbCon)
      if(!is.null(dbCon))dbDisconnect(dbCon)
      message(paste('GIS process',get.GIS_LOCK(),' closed.'))
      listen$gisLock<-NULL
      unset.GIS_LOCK()
      unlink_.gislock()
    } 
    })
})




# if a gislock is set, show meta data of linked project / location.
infoPanel<-renderUI({
  #if(!length(listen$gisLock)>0){
  if(length(projectList$loc)==0 && is.null(listen$gisLock)){
    return( box(status='danger',width=9,tagList(icon('exclamation-triangle'),msgNoLocation)))
  }else{
      message(amSysTime(),paste('leaflet generated.'))
    amPanel(width=9,
      tagList(
        h3('Project summary'),
        tags$h5('Projection used (proj4string):'),
        tags$pre(id='meta-proj','-'),
        tags$h5('General map:'),
        leafletMap(
          "amMap", "100%", 400,
          #initialTileLayer = "//{s}.tiles.mapbox.com/v3/fxi.801dac55/{z}/{x}/{y}.png",
          initialTileLayer="http://a{s}.acetate.geoiq.com/tiles/terrain/{z}/{x}/{y}.png",
          #initialTileLayer="http://{s}.tile.osm.org/{z}/{x}/{y}.png",
          #initialTileLayer="http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
          initialTileLayerAttribution = HTML('tiles:acetate.geoiq.com,data:OSM'),
          options=list(
            center = c(0,0),
            zoom = 2,
            zoomControl=FALSE
            )),
        tags$h5('Grass project:','-'),
        tags$pre(id='meta-grass')
        ) 
      )
  }
})


# generate meta used for mapping
observe({
  loc<-input$selectProject
  if(length(loc)>0 && loc %in% isolate(projectList$loc)){
    listen$mapMetaLatLong<-amGetGrassMeta('latlong') 
    listen$mapMetaOrig<-amGetGrassMeta('orig') 

  }
})



# observe when leaflet is ready. listen$mapReady will not be updated each time bounds change.
# It's a one time operation, by location. 
# See session$onFlushed(once=TRUE, function(){}) if a base map should be rendered once for all.
observe({ 
if(length(input$amMap_bounds)==4)listen$mapReady=TRUE
})



observe({
  listen$mapReady
  m <-listen$mapMetaLatLong

  if(!is.null(m) && !is.null(listen$mapReady) && listen$mapReady){
    # add sleep to be sure that the map is rendered, enven if it's ready.
    Sys.sleep(1)
    # countryMap<-fromJSON(file='www/countries.geojson')
    # countryMap$properties<-countryProp
    #amMap$addGeoJSON(countryMap,'world-map')
    #print(paste(amSysTime()))
    amUpdateText(session,'meta-proj',m$proj)
    amUpdateText(session,'meta-grass',m$summaryHtml)
       #str(ext)
    amMap$addGeoJSON(m$bbxGeoJson,'extent')
    bbx<-unlist(m$bbxLeaflet)
    amMap$fitBounds(bbx[1],bbx[2],bbx[3],bbx[4])
  }
})



# Show project name in title.


observe({
gL<-listen$gisLock
if(!is.null(gL)){
amUpdateText(session, 'proj-name',isolate(input$selectProject))
}

})


#output$title<-renderUI({
#  if(!is.null(listen$gisLock)){
#    locSelect<-isolate(input$selectProject)
#    h5(iconSmall,' ',title,'(',locSelect,')')
#  }else{
#    h5(iconSmall,' ',title)
#  }
#})

## if a gislock is set, show modules UI.
#output$modAccesmod<-renderUI({
#  if(!is.null(listen$gisLock)){
#    tabsetPanel(
#      tabPanel('Module 1',uiOutput('mod1')), # Module 1
#      tabPanel('Module 2',uiOutput('mod2')), # Module 2
#      tabPanel('Module 3',uiOutput('mod3')), # Module 3
#      tabPanel('Module 4',uiOutput('mod4')), # Module 4
#      tabPanel('Module 5',uiOutput('mod5')) # Module 5
#      )
#  }else{
#    panel('warning','No project selected',p(msgNoLocation))
#  }
#})
#







