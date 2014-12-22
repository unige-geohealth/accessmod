


## Final UI construction. See components below.
output$modManage<-renderUI({
  list(
    sidebarPanel(
      formSelectLoc,
      formNewLocation,
      formSelectMapset,
      width=dimsbw),
    mainPanel(
      mainViewManage 
    )
  )
})


# detail :


#-----------------------------{ select existing mapset and location

# form "location" : Select location button.
formSelectLoc<-renderUI({
  list(
    tags$h4('Locations'),
    tags$p('Select a location:'),
    selectInput("location", "",
                selectListMaker(grassListLoc(grassDataBase),default='select'),
                selectize=T,width=dimselw),
    btn('btnNewLoc','New location',sty=stybtn)
  )
})

# form "mapset" : select mapset 
formSelectMapset<-renderUI({
  list(
    renderUiLocationCheck(input,msgNoLocation,ui={
      list(
        tags$h4('Mapsets'),
        tags$p('Select a mapset:'),
        selectInput("mapset", "",choices=c('PERMANENT'),selected='PERMANENT',width=dimselw)
      )
      #selectListMaker(grassListMapset(grassDataBase,input$location),default='select'),
      #selectListMaker("PERMANENT",default='PERMANENT'),
      #selectize=T,width=dimselw),
      #btn('btnNewMapset','New mapset',sty=stybtn))
    })
  )
})

#-----------------------------{ Dynamic UI

# Set title dynamically:


output$title<-renderText({title})


observe({
  locSelect<-input$location
  mapSelect<-input$mapset
  
 
  
  if(!is.null(locSelect) && !is.null(mapSelect)){
    # set location in title
    loc<-paste(c(locSelect,'(',mapSelect,')'),collapse='')
    output$location<-renderUI(p(loc))
  }
})


# show or not module if location and mapset are set.
output$modAccesmod<-renderUiLocMapsetCheck(input,msgNoLocMapset,ui={
  tabsetPanel(
    tabPanel('Module 1',uiOutput('mod1')), # Module 1
    tabPanel('Module 2',uiOutput('mod2')), # Module 2
    tabPanel('Module 3',uiOutput('mod3')), # Module 3
    tabPanel('Module 4',uiOutput('mod4')), # Module 4
    tabPanel('Module 5',uiOutput('mod5')) # Module 5
  )
})


# tabset for the main view.
mainViewManage<-renderUI({
  tabsetPanel(
    tabPanel('Location info',uiOutput('manageLocationInfo'))
  )
})





#-----------------------------{ Reactive observer
# if location and mapset provided, set grass region!
# TODO: avoid two steps for gis unlock


observe({
  
  iL<-input$location
  gL<-grassListLoc(grassDataBase)
  initOK<-FALSE
  if(!is.null(iL) && !iL=='' && iL %in% gL){
    iM<-input$mapset
    gM<-grassListMapset(grassDataBase,iL)
    if(!is.null(iM) && !iM=='' && iM %in% gM){
      tryCatch({
        unset.GIS_LOCK()
        unlink_.gislock()
        #browser()
        initGRASS(
          gisBase = grassBase70,
          #gisBase = grassBase70,
          home=grassHome,
          gisDbase = grassDataBase,
          location = iL, # input location
          mapset= iM, # input mapset
          override=TRUE)
        msg(paste('GIS process id: ',get.GIS_LOCK()))
        print(gmeta6(ignore.stderr = T))
      },
      # handle errors. Message disable: grass is too much verbose.
      error = function(c) msg(paste('GRASS init error:',c)),
      warning = function(c) msg(paste('GRASS init  warning:',c))
      )
      infoPanel()
    }else{
      unset.GIS_LOCK()
      unlink_.gislock()
      msg(paste('GIS process closed.'),verbose=FALSE)
    }
  }else{
    unset.GIS_LOCK()
    unlink_.gislock()
    msg(paste('GIS process closed.'),verbose=FALSE)
  }
})



infoPanel<-reactive({
  loc<-input$location
  output$manageLocationInfo<-renderUiLocationCheck(input,'',ui={
    locationExt<-as(extent(gmeta2grd()),'SpatialPolygons')
    proj4string(locationExt)<-getLocationProj()
    locationExt<- spTransform(locationExt,CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))
    list(
      tags$h4(paste('Location info:',loc)),
      tags$h5('Projection used:'),
      tags$pre(getLocationProj(ignore.stderr = T)),
      tags$h5('General map'),
      renderPlot({
        map('world',fill=FALSE)
        plot(locationExt,add=TRUE,col='red')
      }),
      tags$h5('Grass project:'),
      tags$pre(gmeta6())
    )
  })  
})



#-----------------------------{ new location

# assemble location form
formNewLocation<-renderUI({
  list(
    tags$hr(),
    formNewLocName,
    formNewLocDesc,
    formNewLocDem
  ) 
})


# new location and update location form
formNewLocName<-renderUI({
  if((input$btnNewLoc+1)%%2==0){
    updateSelectInput(session=session, inputId='location',selected='select')
    showNewLoc=showNewLoc+1
    msg('New location requested')
    txt('newLocName','Location name (min. 4 characters)',value='',sty=stytxt)
  }else{
    list(tags$p(),
         msg(''))
  }
})


# Avoid unwanted character, use autoSubPunct function
observe({
  newLoc<-input$newLocName
  updateTextInput(session,'newLocName',value=autoSubPunct(newLoc))
})



# new location description
formNewLocDesc<-renderUI({
  if(!is.null(input$newLocName) && 
       nchar(input$newLocName)>3 && 
       (input$btnNewLoc+1)%%2==0 &&
       ifNewLocAvailable(input$newLocName)
  ){
    txt('newLocDesc','Location description (min. 5 characters)',value='',sty=stytxt)
  }else{
    tags$p()
  }
})


# upload base DEM raster
formNewLocDem<-renderUI({
  if(!is.null(input$newLocDesc) && 
       nchar(input$newLocName)>3 && 
       nchar(input$newLocDesc)>4 && 
       (input$btnNewLoc+1)%%2==0){ 
    upload('newDem', 'Upload projected base map : raster DEM. Multiple files possibles.', multiple = TRUE, accept = acceptRaster,sty=stybtn)
  }else{
    tags$p()
  }
})

# after DEM importation, test if correctly projected and create grass region
observe({
  isolate(newLocName<-input$newLocName)
  newDem<-input$newDem
  if(!is.null(newDem) && !newDem=='' && !is.null(newLocName) && !newLocName==''){
    
    msg(paste('DEM uploaded for location',newLocName))
    # TODO:How to know wich file to select if multiple file are uploaded ?
    # It can vary with data format. 
    # here, we guess that's the heaviest file that contains data.. 
    newDem<-newDem[with(newDem, order(-size)),]
    tmpDir<-dirname(newDem[1,'datapath'])
    newDem$newPath<-file.path(tmpDir,newDem$name)
    file.rename(newDem$datapath,newDem$newPath)
    # capture all error from now, from potentially error prone steps.
    tryCatch({
      # take the first raster as the base map
      r<-raster(newDem[1,'newPath']) 
      destProj<-proj4string(r)
      if(is.na(destProj)){
        m<-'Dataset is not projected'
        msg(m)
        stop(m)
      }else{
        # get proj4string
        msg(paste('Projection detected:',destProj));
        msg('Importation in grass')
        # empty grid for the default WIND object
        msg('Raster as SpatialGrid...',verbose=verbMod)
        sg<-as(r,'SpatialGridDataFrame')
        # grass initialisation. Variable are set in global.R
        msg('Init new GRASS session...',verbose=verbMod)
        initGRASS(gisBase = grassBase70, # binary files (grass 7.0)
                  home=grassHome,
                  gisDbase = grassDataBase, # local grass database
                  location = input$newLocName, # rsession
                  mapset= 'PERMANENT', # PERMANENT for dem.
                  SG=sg,
                  override=TRUE)
        execGRASS('db.connect',driver='sqlite',database='$GISDBASE/$LOCATION_NAME/$MAPSET/sqlite.db')
        execGRASS('g.proj',flags='c',proj4=destProj)
        
        assign('gisLock',get.GIS_LOCK(),envir=globalenv())
        gisLock=get.GIS_LOCK()
        # set as default region
        execGRASS('g.gisenv',flags='s')
        
        msg('Grass initialised, writing DEM as base map.')
        writeRAST6(sg, vname='dem', overwrite=TRUE)
        execGRASS('g.region', rast='dem')
        #unset grass lock
        unset.GIS_LOCK()
        unlink_.gislock()
        
        # Set selection fields and clean.
        updateSelectInput(session=session,'location',choices=
                            grassListLoc(grassDataBase),selected=newLocName)
        updateSelectInput(session=session,'mapset',choices=
                            grassListMapset(grassDataBase,newLocName),selected='PERMANENT')
        updateTextInput(session=session, inputId='newLocName',value='')
        updateTextInput(session=session, inputId='newLocDesc',value='')
        unlink(tmpDir, recursive = TRUE)
        return(NULL);
      }
    },
    # handle errors. Message disable because of grass is too much verbose.
    error = function(c) msg(paste('DEM importation failed:',c)),
    warning = function(c) msg(paste('DEM importation warning',c))
    # message = function(c) msg(paste('Dem importation msg',c))
    )
    
  }
})


#-----------------------------{ Manage : main panel
## manage main view 













