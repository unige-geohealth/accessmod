#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Module Project :
# Project creation and selection
# Meta data visualisation



observeEvent(input$selectLanguage,{
  language <<- input$selectLanguage
  amTranslateSetLanguageClient(language)
})


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
    inputId = "selectProject",
    choices = loc,
    selected = locSel
    )
})




observe({
  loc = grassSession$locations
  loc = loc[! loc %in% grassSession$mapset]
  updateSelectInput(
    session,
    inputId = "selectProjectToDelete",
    choices = loc
    )
})


observeEvent(input$btnDelProject, {
  amErrorAction(
    title = "Module project: project deletion confirmation", {
      content <- tagList(
        p(
          sprintf(
            ams(
              id = "srv_project_delete_project_confirm"
              ),
            input$selectProjectToDelete
            )
          )
        )

      aBtns = list(
        actionButton('btnConfirmDelProject', ams(
            id = "srv_project_delete_confirm_btn"
            ))
        )
      amUpdateModal(panelId = "amModal", 
        title = ams(
          id = "srv_project_confirmation_title"
          ),
        html = content,
        listActionButton = aBtns,
        addCancelButton = TRUE)
    })
})

# in case of project deletion, unlink project files (delete) and update data list
observeEvent(input$btnConfirmDelProject,{
  amErrorAction(title = 'Module project : project deletion',{
    amUpdateModal("amModal",close = TRUE) 
    project <- input$selectProjectToDelete
    projectList <- grassSession$locations
    if(project %in% grassSession$locations){
      projPath <- file.path(config$pathGrassDataBase, project)
      if(file.exists(projPath)){
        unlink(projPath, recursive = TRUE, force = TRUE)
        grassSession$locations <- amGetGrassListLoc(grassDataBase = config$pathGrassDataBase)
      }else{
        stop(sprintf(
            ams(
              id = "srv_project_error_files_not_found"
              ),
            project,
            projPath
            ))
      }
    }else{
      stop(sprintf(
          ams(
            id = "srv_project_error_project_not_present"
            ),
          project,
          projectList
          ))
    }
    })
})

# rendering
# plot map of the project extent.
output$locationMap <- renderPlot({
  mapMeta <- listen$mapMeta
  if(!is.null(mapMeta)){
    bx <- mapMeta$latlong$bbx$ext

    map("world",
      ylim = c(bx$y$min-30,bx$y$max+30),
      xlim = (c(bx$x$min-110,bx$x$max+110)),
      fill = TRUE, col = rgb(0.0,0.0,0.0)
      )
    title(mapMeta$location)
    abline(v = bx$x$min,
      col = 'red',
      lty = 3)
    abline(v = bx$x$max,
      col = 'red',
      lty = 3)
    abline(h = bx$y$min,
      col = 'red',
      lty = 3)
    abline(h = bx$y$max,
      col = 'red',
      lty = 3)
    map.axes()
    plot(amBboxSp(mapMeta,
        proj = 'latlong'),
      add = TRUE,
      col = 'red')

  }
}, bg = 'transparent')

# project meta : proj 4 string info text
output$infoProj4String <- renderUI({
  mapMeta <- listen$mapMeta
  amDebugMsg('update infoProj4String')
  if(!is.null(mapMeta)){
    tags$pre(mapMeta[['orig']]$proj)
  }
})

# project meta : grid information 
output$infoGrid <- renderUI({
  mapMeta <- listen$mapMeta
  if(!is.null(mapMeta)){
    tags$pre(HTML(listToHtml(mapMeta$grid,h = 5)))
  }
})

# project meta : original projected extent
output$infoExtentProj <- renderUI({
  mapMeta <- listen$mapMeta
  if(!is.null(mapMeta)){
    ext <- mapMeta$orig$bbx$ext
    names(ext) <- c('Easting','Northing')
    tags$pre(HTML(listToHtml(ext,h = 5)))
  }
})

# project meta : lat/long extent
output$infoExtentLatLong <- renderUI({
  mapMeta <- listen$mapMeta
  if(!is.null(mapMeta)){
    ext <- mapMeta$latlong$bbx$ext
    names(ext) <- c('Longitude','Latitude')
    tags$pre(HTML(listToHtml(ext,h = 5)))
  }
})


# New project name validation
observeEvent(input$txtNewProjectName,{
  pN <- input$txtNewProjectName
  moreChar <- NULL
  notAvailable <- NULL
  msgUpload <- NULL
  isNumberOnly <- NULL

  if(amNoDataCheck(pN)){
    newProjectName <- character(1)
  }else{
    newProjectName <- amSubPunct(
      pN,
      config$sepTagFile,
      rmLeadingSep = T,
      rmTrailingSep = T,
      rmDuplicateSep = T
      )
  }

  amErrorAction(title = 'Validate project name',{
    #
    # Check rules
    #
    pNameAvailable <- !isTRUE(newProjectName %in% grassSession$locations)
    pChar <- nchar(newProjectName)
    if(amNoDataCheck(pChar)){
     pChar <- 0
    }
    pCharValid <- isTRUE(pChar > 3)
    numberOnly <- isTRUE(grepl('^[0-9]*$',newProjectName))
    #
    # Available and correct length
    #
    if(pNameAvailable && pCharValid && !numberOnly){ 
      msgUpload <- sprintf(
        ams("srv_project_available_name_choose_dem"),
        newProjectName
        )
    }else{

      #
      # Incorrect length
      #
      if( !pCharValid ) {
        moreChar <- sprintf(
          ams("srv_project_enter_more_characters"),
          4 - pChar
          )
      }

      #
      # Not available
      #
      if( !pNameAvailable ){
        notAvailable <- sprintf(
          ams("srv_project_project_not_available"),
          newProjectName
          )
      }

      #
      # Number only
      #
      if( numberOnly ){
        isNumberOnly <- sprintf(
          ams("srv_project_project_number_only"),
          newProjectName
          )
      }
      #
      # Default to NULL
      #
      newProjectName <- NULL
    }

    #
    # Add validation text
    #
    amUpdateText(
      id ='txtHintNewProject',
      text = paste(
        icon('info-circle'),
        moreChar,
        notAvailable,
        msgUpload,
        isNumberOnly
        )
      )

    #
    # Set the validate name available for import and new project
    #
    listen$newProjectName <- newProjectName

    #
    # Update inputs
    #
    disable <- amNoDataCheck(newProjectName) 
    #amActionButtonToggle('fileNewDem',session,disable = disable)
    amActionButtonToggle('btnProjectImport', session,disable = disable)
    if(!disable){
      amFileInputUpdate('btnProjectImport',session,
        multiple = TRUE,
        accept = c(
          config$filesAccept$raster,
          config$filesAccept$project
          )
        )
    }


      })
})
#
# Import a project
#
# DF (newProject) names : "name"     "size"     "type"     "datapath"
observeEvent(input$btnProjectImport,{
  newProject <- input$btnProjectImport
  newProjectName <- listen$newProjectName

  on.exit({
    amActionButtonToggle('btnProjectImport',session,disable = FALSE)
  })

  if(length(newProject)>0 && length(newProjectName)>0){
    amErrorAction(title = 'Module project: upload project or DEM',{

      grassSession$gisLock <- NULL

      amActionButtonToggle('btnProjectImport',session,disable = TRUE)

      isProject <- isTRUE(grepl(config$fileArchiveProjectDb,newProject$name))

      pBarTitle = ams('project_import_progress_title')

      pbc(
        id = "import_project",
        title = pBarTitle,
        text = ams('project_import_progress_text'),
        percent = 10
        )

      if(isProject){
        amProjectImport(
          fileProject = newProject,
          name = newProjectName
          )
      }else{
        # upload function
        amProjectCreateFromDem(
          newDem = newProject,
          newProjectName = newProjectName,
          onProgress = function(text="...",percent=0,timeout=0){
            pbc(
              percent = percent,
              title = pBarTitle,
              text = text,
              timeOut = timeout
              )
          }) 
      }

      pbc(
        id = "import_project",
        percent = 100
        )

      listen$newProjectUploaded <- runif(1)

  })
  }
})

#
# Trigger project change afer new project uploaded
#
observeEvent(listen$newProjectUploaded,{
  amErrorAction(title = "Module project: trigger new project",{
    
    #
    # Check if project exists in locations
    #
    newProjectName = listen$newProjectName
    allGrassLoc <- amGetGrassListLoc(config$pathGrassDataBase)
    grassSession$locations <- allGrassLoc
    isListed <- newProjectName  %in% allGrassLoc

    if(!isListed){
      stop("New project not found")
    }

    amUpdateText('hint-new-dem',
      paste(icon('info-circle'),
        ams(
          id = "srv_project_add_new_project_name"
          )
        )
      )

    #
    # Trigger project change
    #
    updateSelectInput(
      session,
      "selectProject",
      choices = allGrassLoc
      )

    #
    # Reset project name
    #
    updateTextInput(session,
      "txtNewProjectName",
      value = ""
      )

    listen$selProject <- newProjectName

  })
})


#
# Project selection validation
#
observeEvent(input$selectProject,{
  selProject <- input$selectProject
  amErrorAction(title = "Module project: set project selection in listener",{
    if(!amNoDataCheck(selProject)){  
      listen$selProject = selProject
    }else{
      listen$selProject = NULL
    } 
    })
})

#
# Main projection selection
#
observeEvent(listen$selProject,{
  project <- listen$selProject
  amErrorAction(title = "Module project: init grass session",{

    #
    # Test if project is listed
    #
    isListed <- isTRUE(project  %in% grassSession$locations)


    if(isListed){
      #
      # Save cookie
      # 
      amSetCookie(
        cookies = list("am5_location" = project)
        )

      #
      # Set GRASS environment
      #
      amUpdateText('projName',project)
      gHome <- file.path(tempdir(),project)
      dir.create(gHome,showWarnings = F)
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
      Sys.setenv(GRASS_SKIP_MAPSET_OWNER_CHECK = TRUE)
      #Sys.setenv(GRASS_VECTOR_TEMPORARY = 'keep')
      # NOTE : no vector modules works in delete mode. 
      # amCleanGrassTemp do it manually
      amCleanGrassTemp()
      amCleanCacheFiles()
      dbSqlitePath <- system(paste("echo",config$pathSqliteDB),intern=T)
      grassSession$dbCon <- dbConnect(RSQLite::SQLite(),dbSqlitePath)
      execGRASS('db.connect',driver='sqlite',database=dbSqlitePath)
      amRegionReset()
      grassSession$mapset <- project
      grassSession$gisLock <- get.GIS_LOCK()
      if(amRastExists('MASK')) execGRASS('r.mask',flags='r')
      listen$mapMeta <- amMapMeta()
      listen$outFiles <- NULL
      amUpdateDataList(listen)
    }else{
      amUpdateText('projName','')
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
  currentMapset <- grassSession$mapset
  selectedMapset <- isolate(input$selectProject)
  if(isTRUE(nchar(selectedMapset)>0)){
    if(!is.null(currentMapset) && !identical(currentMapset,selectedMapset)){
      m <- sprintf(
        ams(
          id = "srv_project_warning_one_project_per_user"
          ),
        currentMapset,
        selectedMapset
        )
      amMsg(type = 'log',
        text = m
        )
      updateSelectInput(session,
        'selectProject',
        selected = currentMapset
        )
    }
  }
})


observe({
  sP <- grassSession$mapset
  gLock <- get.GIS_LOCK()
  if(length(sP)>0 && !sP==""){
    amDebugMsg('new get gisLock=',gLock)
    grassSession$gisLock <- gLock
  }else{
    grassSession$gisLock = NULL
  }
})


##update project name in title
#observe({
  #gL <- grassSession$gisLock
  #if(!is.null(gL)){
  #}
#})

#
# Export a project
#
observeEvent(input$btnProjectExport,{
  idProject <- input$selectProject
  pbc(
    id = "export_project",
    title = ams('project_export_progress_title'),
    text = ams('project_export_progress_text'),
    percent = 10
    )
  archivePath <- amProjectExport(idProject)
  archiveName <- basename(archivePath)
  archiveUrl <- file.path(config$prefixCache,archiveName)
  amGetData(session, archiveUrl)

  pbc(
    id = "export_project",
    percent = 100
    )
})




