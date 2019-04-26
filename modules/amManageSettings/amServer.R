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
  cacheFiles <- list.files(config$pathCacheDir,full.names = T)

  if(length(cacheFiles)>0){
    file.remove(cacheFiles)
    amMsg(
      type = "log",
      text = sprintf(
        ams(
          id = "srv_settings_clean_cache_removed_files"
          ),
        length(cacheFiles)
        )
      )
  }

  #
  # clear cookies
  # 
  session$sendCustomMessage(
    type = "amSetCookie",
    list(
      deleteAll = TRUE,
      reload = TRUE
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
  grassMeta <- HTML(
    "<p>Reloaded project spatial metadata:</p>",
    listToHtml(
      listen$mapMeta$grid,
      h = 4
      )
    )
  amMsg(
    session,
    type = "warning",
    title = ams(
      id = "srv_settings_reload_meta_data"
      ),
    subtitle = ams(
      id = "srv_settings_reload_meta_data_summary"
      ),
    text = grassMeta,
    logFile = config$pathLog
    )
})


observe({
  title <- "AccessMod 5"
  version <- amGetAppCurrentTag()
  title <- sprintf("%s ( %s )", title, version)
  amUpdateText("amVersionTitle", title)
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
    tags$h4(
      ams(
        id = "srv_settings_disk_usage_title"
        )
      ),
    tags$ul(
      tags$li(tags$label(
          ams(
            id = "srv_settings_free_disk_text"
            )
          ),
        sprintf("%s GB ( %s %% )",
          diskFree,
          diskFreePercent
          )
        ),
      tags$li(tags$label(
          ams(
            id = "srv_settings_used_disk_text"
            )
          ),
        sprintf("%s GB ( %s %% )",
          diskUsed,
          diskUsedPercent
          )
        ),
      tags$li(tags$label(
          ams(
            id = "srv_settings_total_disk"
            )
          ),
        sprintf("%s GB", diskTotal))
      )
    )
  output$uiDiskUsage <- renderUI(uiDiskUsage)
})
#
# Change update info ui
#
output$amUpdate <- renderUI({
  if(!isTRUE(input$whichTab == "module_settings")) return()

  amErrorAction(title = "Settings: Version check",{
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

    amUpdateText(id = "txtAccessmodVersion", listToHtml(h = 6,msg))

    #
    # Update install button
    #
    if(identical(as.character(config$hostname), "accessmod")){
      # test for version match

      if( enableUpdate ){
        valueOut <-tagList(
          p(ams(
              id = "srv_settings_update_available_notice"
              )),
          actionButton("btnInstall",
            ams(
              id = "srv_settings_install_update_btn"
              )
            )
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
  amErrorAction(title = "Settings: update application",{
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
  amErrorAction(title = "Set upload limit",{
    maxSize =  as.integer(input$numSetUploadLimit)
    if(isTRUE(maxSize < 10 || maxSize > 1000)){
      stop(ams(
          id = "srv_settings_file_size_rejected"
          )
        )
    }else{ 
      options(shiny.maxRequestSize =  maxSize*1024^2)
    }
    if( ! maxSize == config$maxUploadSize ){

      warn <- ams(
        id = "srv_settings_max_file_limit_500mb_warning"
        ) 

    }else{
      warn <- ""
    }

    txt <- sprintf(
      ams(
        id = "srv_settings_importing_limits_warning"
        ),
      maxSize,
      warn
      )

    amMsg(session,
      "warning",
      title = ams(
        id = "srv_settings_update_upload_limit"
        ),
      text = txt
      )  
    })
})

observeEvent(input$btnForceUpdate,{
  amUpdateApp(force=TRUE)
})

