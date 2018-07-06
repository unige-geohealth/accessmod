#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# settings and admin task 


observeEvent(input$btnClearCache,{

  #
  # Force Grass cache removal
  #
  amCleanGrassTemp()

  #
  # clean cached files
  #
  cacheFiles <- list.files(config$pathCacheDir,full.names=T)

  if(length(cacheFiles)>0){
    file.remove(cacheFiles)
    amMsg(
      type="log",
      text=sprintf("Clean cache, removed % files",length(cacheFiles)))
  }

  #
  # clear cookies
  # 
  session$sendCustomMessage(
    type="amSetCookie",
    list(
      deleteAll=TRUE,
      reload=TRUE
      )
    )
})


#
# reset grass region
#
observeEvent(input$grassResetRegion,{
  #
  # set region according to DEM
  #
  grassReloadRegion(config$mapDem)
  #
  # reset map meta
  listen$mapMeta <- amMapMeta()
  #
  # set message
  #
  grassMeta<-HTML(
    "<p>Reloaded project spatial metadata:</p>",
    listToHtml(
      listen$mapMeta$grid,
      h=4
      )
    )
  amMsg(
    session,
    type="warning",
    title="Roload project meta data",
    subtitle='summary',
    text=grassMeta,
    logFile=config$pathLog
    )
})


observe({
  title <- "AccessMod 5"
  version <- amGetAppCurrentTag()
  title <- sprintf("%s ( %s )",title,version)
  amUpdateText("amVersionTitle",title)
})

observe({
  update <- dataList$archive
  update <- dataList$df
  #
  # Set VM size values
  #
  diskFree <- sysEvalFreeMbDisk()/1000
  diskTotal <- sysEvalSizeMbDisk()/1000
  diskUsed <- diskTotal - diskFree
  diskFreePercent <- round(diskFree / diskTotal * 100)
  diskUsedPercent <- round(diskUsed / diskTotal * 100)
  
  uiDiskUsage <- tagList(
    tags$h4("Disk usage"),
      tags$ul(
        tags$li(tags$label("Free :"),sprintf("%s GB ( %s %% )",diskFree,diskFreePercent )),
        tags$li(tags$label("Used :"),sprintf("%s GB ( %s %% )",diskUsed,diskUsedPercent )),
        tags$li(tags$label("Total :"),sprintf("%s GB",diskTotal))
        )
    )
  output$uiDiskUsage <- renderUI(uiDiskUsage)
})
#
# Change update info ui
#
output$amUpdate <- renderUI({
  if(!isTRUE(input$whichTab == "module_settings")) return()

  amErrorAction(title="Settings : Version check",{
    #
    # Default version "0"
    #
    valueOut <- character(0)

    #
    # Enable if there is a diff
    #
    enableUpdate <- ! identical(
      amGetAppVersionLocal(),
      amGetAppVersionFetched()
      )

    #
    # update version text 
    #
    msg <- list(
      `Branch` = amGetAppCurrentBranch(),
      `Revision local` = amGetAppVersionLocal(),
      `Revision fetched`  = amGetAppVersionFetched(),
      `Node name`        = Sys.info()['nodename']
      )

    amUpdateText(id="txtAccessmodVersion",listToHtml(h=6,msg))

    #
    # Update install button
    #
    if(identical(as.character(config$hostname),"accessmod")){
      # test for version match

      if( enableUpdate ){
        valueOut <-tagList(
          p("An update is available."),
          actionButton("btnInstall","Install update")
          )
      }
    }

    return(valueOut)
    })
})


#
# Update application
#

observeEvent(input$btnInstall,{
  amErrorAction(title="Settings : update application",{
    amUpdateApp()
    })
})

#
# Restart application
#

observeEvent(input$btnRestart,{
  amRestart()
})

#
# Update file size limit
#

observeEvent(input$btnSetFileSizeLimit,{
  amErrorAction(title="Set upload limit",{
    maxSize =  as.integer(input$numSetUploadLimit)
    if(isTRUE(maxSize < 10 || maxSize > 1000)){
      stop("File size not accepted. Min = 10 MB; Max = 1000 MB")
    }else{ 
      options(shiny.maxRequestSize= maxSize*1024^2)
    }
    if( ! maxSize == config$maxUploadSize ){

      warn <- "
      Maximum file limit set to 500 MB. This change could lead to unexpected issues, proceed with caution. If applicable, modify your virtual server settings accordingly (see the user manual for more information)" 

    }else{
      warn <- ""
    }

    txt <- sprintf("Data importing limit temporary set to %s MB. %s",
      maxSize,
      warn
      )

    amMsg(session,"warning",title="Updating upload limit",text=txt)  
    })
})



