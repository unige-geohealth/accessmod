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
      id = "settings_advanced_tools",
      str = "Enable advanced options in modules"
      )),
    #
    # Admin tools : restart, update.
    #
    checkboxInput('showAdminTools', amt(
      id = "settings_admin_tools",
      str = 'Show advanced settings'
      )),
    conditionalPanel(condition = "input.showAdminTools==true",
      #
      # display disk usage
      #
      tags$h3(amt(
        id = "settings_system_info_title",
        str = "System information"
        )),
      uiOutput("uiDiskUsage"
        ),
      #
      # display information update
      #
      tags$h3(amt(
        id = "settings_am_version_title",
        str = "Accessmod version"
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
        id = "settings_restart_am_btn",
        str = "Restart Accessmod"
        )),
      actionButton("btnClearCache", amt(
        id = "settings_clear_cache_restart_btn",
        str = "Clear cache and restart"
        )),
      #
      # Expert tools
      #
      checkboxInput('showDevelTools', amt(
        id = "settings_expert_options",
        str = 'Show expert options'
        )),
      conditionalPanel(condition = 'input.showDevelTools == true',
        p(amt(
          id = "settings_warning_unstable",
          str = "Warning: these options could make AccessMod unstable"
          )),
        #
        # Change upload limit.
        #
        numericInput("numSetUploadLimit", amt(
          id = "settings_upload_limit_import",
          str = "Temporary limit for data importing (Megabytes)"
          ),
          min = 10,
          max = 1000,
          value = config$maxUploadSize,
          step = 1
          ),
        actionButton("btnSetFileSizeLimit", amt(
          id = "settings_upload_limit_btn",
          str = "Apply the temporary data importing limit"
          )),
        #
        # In some case, grass lost spatial settings
        #
        actionButton('grassResetRegion',
          label = amt(
            id = "settings_reload_spatial_btn",
            str = 'Reload spatial settings'
            )
          )
        #
        # Show interactive browser
        #
        # actionButton('showBrowser',
        # Interractive debugger (break the application)'
        # )
        )
      )
    )
  )


