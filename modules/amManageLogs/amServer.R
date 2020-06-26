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

# module logs :
# Display and download logs


reactiveLogTable <- reactive({
  nK <- input$nLogsToKeep
  if(!is.null(nK) && !nK==""){  
    reactiveLogTable <- reactiveFileReader(
	  5000,
	  session,
	  config$pathLog,
	  readFunc = amReadLogs,
	  nToKeep = nK
	  )
    logsTable <- reactiveLogTable()
    names(logsTable) <- c('time','type','msg')
    logsTable
  }else{
    NULL
    }
  })

output$logsTable <- renderHotable({
  filterLogs <- input$filterLogs
    logsTable <- reactiveLogTable()
    if(!is.null(logsTable) && !is.null(filterLogs)){
      logsTable <- logsTable[order(logsTable$time, decreasing = T),]
      if(filterLogs=='all'){
        return(logsTable)
      }else{
        logsTable <- logsTable[grep(filterLogs, logsTable[,'type']),]
        if(isTRUE(nrow(logsTable)==0))logsTable[1,] <- '-'
        return(logsTable)
        }
      }

  })

output$downloadLogs <- downloadHandler(
  filename = function() {
    paste('AccessModLogs-', amSysTime(), '.csv', sep = '')
    },
  content = function(file){
    logs <- reactiveLogTable()
    write.csv(logs, file) 
    } 
  )

