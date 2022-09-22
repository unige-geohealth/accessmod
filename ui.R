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

# User interface
dashboardPage(
  title = "AccessMod 5.0",
  skin = "black",
  header = dashboardHeader(
    title = div(
      class = "amCenterTitle",
      tags$a(
        href = "http://www.who.int",
        config$iconWhoSvg
      )
    )
  ),
  sidebar = dashboardSidebar(
    div(
      hr(),
      div(
        class = "amCenterTitle",
        h4("AccessMod 5"),
        tags$span(amt("main_ui_current_project")),
        tags$h4(id = "projName", ""),
        div(class = "shinyCookies", id = "amCookies")
      ),
      hr(),
      sidebarMenu(
        id = "whichTab",
        menuItem(
          text = amt("main_ui_tab_project"),
          tabName = "module_project",
          icon = icon("map-marker")
        ),
        menuItem(
          text = amt("main_ui_tab_data"),
          tabName = "module_data",
          icon = icon("folder-open")
        ),
        menuItem(
          text = amt("main_ui_tab_analysis"),
          tabName = "module_analysis",
          icon = icon("cubes")
        ),
        menuItem(
          text = amt("main_ui_tab_toolbox"),
          tabName = "module_toolbox",
          icon = icon("cogs")
        ),
        menuItem(
          text = amt("main_ui_tab_logs"),
          tabName = "module_logs",
          icon = icon("archive")
        ),
        menuItem(
          text = amt("main_ui_tab_settings"),
          tabName = "module_settings",
          icon = icon("sliders-h")
        ),
        menuItem(
          text = amt("main_ui_tab_about"),
          tabName = "module_about",
          icon = icon("info-circle")
        )
      ),
      hr(),
      tags$div(
        style = "padding:20px;opacity:0.9",
        id = "amVersion"
        # uiOutput('uiMenuVersion')
      )
    )
  ),
  body = tags$div(
    class = "content-wrapper",
    # headers
    tags$head(
      tags$link(rel = "shortcut icon", href = "logo/favicon.ico?"),
      # @fxi/el
      tags$script(src = "modules/el/index.js"),

      # handsontable
      tags$script(src = "modules/handsontable/handsontable.full.min.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "modules/handsontable/handsontable.full.min.css"),
      tags$script(src = "modules/accessmod/extend_handsontable.js"),
      # progress
      tags$script(src = "modules/progress/index.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "modules/progress/style.css"),
      # accessmod
      tags$script(src = "modules/accessmod/index.js"),
      tags$script(src = "modules/accessmod/translate.js"),
      tags$script(src = "modules/accessmod/http.js"),
      tags$script(src = "modules/accessmod/versions.js"),
      tags$script(src = "modules/accessmod/modal.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "modules/accessmod/geom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "modules/accessmod/style.css"),
      # marked
      tags$script(src = "modules/marked/marked.min.js")
    ),
    tags$div(
      class = "panel-container",
      # default modal panel
      uiOutput("amModal"),
      # help modal panel
      uiOutput("amHelpPanel")
    ),
    # tabs
    tabItems(
      tabItem(
        "module_project",
        loadUi("modules/amManageProject/amUi.R")
      ),
      tabItem(
        "module_data",
        loadUi("modules/amManageData/amUi.R")
      ),
      tabItem(
        "module_analysis",
        loadUi("modules/amManageAnalysis/amUi.R")
      ),
      tabItem(
        "module_toolbox",
        loadUi("modules/amManageToolbox/amUi.R")
      ),
      tabItem(
        "module_logs",
        loadUi("modules/amManageLogs/amUi.R")
      ),
      tabItem(
        "module_settings",
        loadUi("modules/amManageSettings/amUi.R")
      ),
      tabItem(
        "module_about",
        loadUi("modules/amAbout/amUi.R")
      )
    )
  )
)
