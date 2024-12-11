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

# Module logs: Display and download logs

tagList(
  sidebarLayout(
    sidebarPanel(
      h4(amt(
        id = "logs_title"
      )),
      sliderInput("nLogsToKeep", amt(
        id = "logs_to_show"
      ),
      min = 1,
      max = config$nLogMax,
      value = config$nLogDefault,
      step = 100
      ),
      amRadioButtons("filterLogs", amt(
        id = "logs_filter"
      ),
      choiceNames = list(
        amt("logs_filter_error"),
        amt("logs_filter_warning"),
        amt("logs_filter_message"),
        amt("logs_filter_log"),
        amt("logs_filter_all")
      ),
      choiceValues = list(
        "error",
        "warning",
        "message",
        "log",
        "all"
      )
      ),
      downloadButton("downloadLogs",
        label = amt(
          id = "logs_download"
        )
      )
    ),
    mainPanel(
      tabulator_output("logsTable")
    )
  )
)
