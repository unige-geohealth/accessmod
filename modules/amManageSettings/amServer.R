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

# settings and admin task 

observe({

  language <- listen$language
  version <- listen$newVersion

  uiMenuVersion <- tags$div(
    ams('menu_version_current'),
    actionLink(
      inputId = 'btnShowChangelog_menu',
      label = amGetAppVersionCurrent()
      )
    )

  if( amHasAppUpdate() ){

    uiMenuVersionNew <- tags$div(
      ams('menu_version_new'),
      amGetAppVersionFetched(),'. ',
      actionLink('btnShowUpdateModal_menu',
        label = ams('menu_version_btn_update')
        )
      )
    uiMenuVersion <- tags$div(
      uiMenuVersion,
      uiMenuVersionNew
      )
  }else{
    btnCheckForUpdate <- tags$span(
      amt('menu_version_no_update'),
      actionLink(
        inputId = 'btnCheckForUpdate',
        label = ams('menu_check_for_update')
        )
      )

    uiMenuVersion <- tags$div(
      uiMenuVersion,
      btnCheckForUpdate
      )
  }

  output$uiMenuVersion <- renderUI(uiMenuVersion)

})

observeEvent(input$btnCheckForUpdate,{

  if( amHasAppUpdate() ){
    listen$newVersion <- runif(1)  
  }else{
    amUpdateModal(
      panelId = "amModal",
      title = ams(
        id = "modal_title_app_update"
        ),
      html =  amt("menu_version_no_update")
      )
  }

})


observeEvent(input$btnShowChangelog,{
  listen$showCurrentChangeLog <- runif(1)
})

observeEvent(input$btnShowChangelog_menu,{
  listen$showCurrentChangeLog <- runif(1)
})

observeEvent(listen$showCurrentChangeLog,{

  changes <- amGetAppChangesCurrent()

  amUpdateModal(
    panelId = "amModal",
    title = ams(
      id = "modal_title_app_changelog"
      ),
    html = 
      tags$div(
        tags$p(
          class = paste("markdown","base64"),
          # NOTE:
          # Need to encode, as shiny remove special characters, 
          # Even with HTML() function.

          amEncode(changes)
          ),
        tags$span(
          amt("settings_get_link_source"),
          amUiLinkRepoRelease(
            version =  amGetAppVersionCurrent(),
            text = amGetAppVersionCurrent()
            )
          )
        ),
      addCancelButton = FALSE
      )

})


observeEvent(input$btnShowUpdateModal_menu,{
  listen$showUpdateModal <- runif(1)
})
observeEvent(input$btnShowUpdateModal,{
  listen$showUpdateModal <- runif(1)
})
observeEvent(listen$showUpdateModal,{

  changes <- amGetAppChangesFetched()

  buttons <- actionButton('btnInstall',
    ams('menu_version_btn_update')
    )

  amUpdateModal(
    panelId = "amModal",
    title = ams(
      id = "modal_title_app_update"
      ),
    listActionButton = tagList(buttons),
    html = tags$p(
      class = paste("markdown","base64"),
      # Need to encode, as shiny remove special characters, 
      # Even with HTML() function.
      amEncode(changes)
      ),
    addCancelButton = FALSE
    )
})


observeEvent(input$btnClearCache,{

  #
  # Force Grass cache removal
  #
  amCleanGrassTemp()

  #
  # clean cached files
  #
  amCleanCacheFiles()

  #
  # clear cookies
  # 
  session$sendCustomMessage(
    type = "amSetCookie",
    list(
      deleteAll = TRUE,
      reload = FALSE
      )
    )

  #
  # Restart
  #
  amRestart()
})

observeEvent(input$btnClearArchives,{
  archiveList <- amGetArchiveList()
  nArchives <- length(archiveList)
  ulArchiveList <- tagList()
  buttons <- NULL;

  if(nArchives>0){
    msg <- tags$p(sprintf(ams("settings_clear_archives_message_confirm"),nArchives))
    ulArchiveList <- tags$ul(lapply(archiveList, tags$li))
    buttons <- tagList(
      actionButton('btnClearArchivesConfirm',
        ams('settings_clear_archives_confirm_btn')
      )
    )
  }else{
    msg <- tags$p(sprintf(ams("settings_clear_archives_message_no_archive")))
  }

  #
  # Force archive remove
  #
  amUpdateModal(
    panelId = "amModal",
    title = ams("settings_clear_archives_btn"),
    html = tagList(msg,ulArchiveList),
    listActionButton = buttons,
    addCancelButton = TRUE
  )
})

observeEvent(input$btnClearArchivesConfirm, {
  nClean = amCleanArchivesFiles()
  amUpdateModal(
    panelId = "amModal",
    title = ams("settings_clear_archives_btn"),
    html = tags$p(
      sprintf(ams("settings_clear_archives_confirm_done"), nClean)
    )
  )
  amUpdateDataList(listen)
})
#
# reset grass region
#
observeEvent(input$grassResetRegion,{
  #
  # set region according to DEM
  #
  amRegionReset()

  #
  # reset map meta
  listen$mapMeta <- amMapMeta()

  #
  # Reset DB connection
  #
  amUpdateSqliteDbPath(grassSession$mapset)

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
  version <- amGetAppVersionCurrent()
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
    enableUpdate <- amHasAppUpdate()

    if(enableUpdate){
      listen$newVersion <- runif(1)
    }

    #
    # update version text 
    #
    msg <- list(
      `Node name` = Sys.info()['nodename'],
      `Branch` = amGetAppCurrentBranch(),
      `Version` = sprintf('%s ( %s )',
        amGetAppVersionCurrent(), 
        amGetAppRevisionCurrent()
        )
      )

    if( enableUpdate ){
      msg <- c(
        msg,
        list(
          `New version`  = sprintf('%s ( %s )' ,
            amGetAppVersionFetched(),
            amGetAppRevisionFetched()
            )
          )
        )
    }

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
          actionButton("btnShowUpdateModal",
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
    if(isTRUE(maxSize < 10)){
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

