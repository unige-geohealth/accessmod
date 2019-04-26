#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.



sidebarPanel(
  tagList(
    tags$h4(img(src = "logo/icons/logo32x32.png"
      ),
      span(id = "amVersionTitle"
      )),
    #
    # Advanced options
    #
    checkboxInput("showAdvancedTools", amt(
      id = "settings_advanced_tools"
      )),
    #
    # Admin tools : restart, update.
    #
    checkboxInput('showAdminTools', amt(
      id = "settings_admin_tools"
      )),
    conditionalPanel(condition = "input.showAdminTools==true",
      #
      # display disk usage
      #
      tags$h3(amt(
        id = "settings_system_info_title"
        )),
      uiOutput("uiDiskUsage"
        ),
      #
      # display information update
      #
      tags$h3(amt(
        id = "settings_am_version_title"
        )),
      span(id = "txtAccessmodVersion"
        ),

      #
      # Text and button for update
      #
      uiOutput("amUpdate"),
      #
      # Restart application (do not update)
      #
      actionButton('btnRestart', amt(
        id = "settings_restart_am_btn"
        )),
      actionButton("btnClearCache", amt(
        id = "settings_clear_cache_restart_btn"
        )),
      #
      # Expert tools
      #
      checkboxInput('showDevelTools', amt(
        id = "settings_expert_options"
        )),
      conditionalPanel(condition = 'input.showDevelTools == true',
        p(amt(
          id = "settings_warning_unstable"
          )),
        #
        # Change upload limit.
        #
        numericInput("numSetUploadLimit", amt(
          id = "settings_upload_limit_import"
          ),
          min = 10,
          max = 1000,
          value = config$maxUploadSize,
          step = 1
          ),
        actionButton("btnSetFileSizeLimit", amt(
          id = "settings_upload_limit_btn"
          )),
        #
        # In some case, grass lost spatial settings
        #
        actionButton('grassResetRegion',
          label = amt(
            id = "settings_reload_spatial_btn"
            )
          ),
        actionButton('btnForceUpdate',
          label = amt(
            id = "settings_force_update"
            )
          )
        )
      )
    )
  )


