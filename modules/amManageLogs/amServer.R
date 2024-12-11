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


# Module logs:
# Display and download logs

reactiveLogTable <- reactiveFileReader(
  session = session,
  intervalMillis = 1e3 * 30,
  filePath = config$pathLog,
  readFunc = amReadLogs,
  nToKeep = config$nLogMax
)

output$logsTable <- render_tabulator({
  amErrorAction(title = "Log table", {
    nk <- input$nLogsToKeep
    filterLogs <- input$filterLogs
    logsTable <- reactiveLogTable()

    if (isEmpty(nk)) {
      nk <- config$nLogDefault
    }

    logsTable <- head(
      x = logsTable,
      n = nk
    )

    if (filterLogs != "all") {
      logsTable <- logsTable[grep(filterLogs, logsTable[, "type"]), ]
    }

    tabulator(
      data = logsTable,
      readOnly = TRUE
    )
  })
})

output$downloadLogs <- downloadHandler(
  filename = function() {
    paste("AccessModLogs-", amSysTime(), ".csv", sep = "")
  },
  content = function(file) {
    logs <- reactiveLogTable()
    write.csv(logs, file)
  }
)
