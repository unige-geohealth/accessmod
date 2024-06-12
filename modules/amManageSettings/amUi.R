#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

# description of the project

sidebarPanel(
  tagList(
    tags$h4(
      img(src = "logo/icons/logo32x32.png"),
      span(id = "amVersionTitle")
    ),
    #
    # Advanced options
    #
    checkboxInput("showAdvancedTools", amt(
      id = "settings_advanced_tools"
    )),
    #
    # Admin tools : restart, update.
    #
    checkboxInput("showAdminTools", amt(
      id = "settings_admin_tools"
    )),
    conditionalPanel(
      condition = "input.showAdminTools==true",
      #
      # display disk usage
      #
      tags$h3(amt(
        id = "settings_system_info_title"
      )),
      uiOutput("uiDiskUsage"),
      #
      # Restart application
      #
      actionButton("btnRestart", amt(
        id = "settings_restart_am_btn"
      )),
      actionButton("btnClearCache", amt(
        id = "settings_clear_cache_restart_btn"
      )),
      actionButton("btnClearArchives", amt(
        id = "settings_clear_archives_btn"
      )),
      #
      # Expert tools
      #
      checkboxInput("showDevelTools", amt(
        id = "settings_expert_options"
      )),
      conditionalPanel(
        condition = "input.showDevelTools == true",
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
        actionButton("grassResetRegion",
          label = amt(
            id = "settings_reload_spatial_btn"
          )
        )
        # actionButton('btnForceUpdate',
        # label = amt(
        # id = "settings_force_update"
        # )
        # )
      )
    )
  )
)
