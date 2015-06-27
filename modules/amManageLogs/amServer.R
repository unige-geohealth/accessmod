#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# module logs :
# Display and download logs



reactiveLogTable<-reactive({
  nK<-input$nLogsToKeep
  if(!is.null(nK) && !nK==""){  
    reactiveLogTable<-reactiveFileReader(5000,session,config$pathLog,readFunc = amReadLogs, nToKeep=nK)
    logsTable<-reactiveLogTable()
    names(logsTable)<-c('time','type','msg')
    logsTable
  }else{
    NULL
  }
})

output$logsTable <- renderHotable({
 filterLogs<-input$filterLogs
  logsTable <- reactiveLogTable()
  if(!is.null(logsTable) && !is.null(filterLogs)){
    logsTable <- logsTable[order(logsTable$time,decreasing=T),]
    if(filterLogs=='all'){
      return(logsTable)
    }else{
      logsTable<-logsTable[grep(filterLogs, logsTable[,'type']),]
      if(isTRUE(nrow(logsTable)==0))logsTable[1,]<-'-'
      return(logsTable)
    }
  }

})

#output$logsTable <- renderDataTable({
#  filterLogs<-input$filterLogs
#  logsTable <- reactiveLogTable()
#  if(!is.null(logsTable) && !is.null(filterLogs)){
#    logsTable <- logsTable[order(logsTable$time,decreasing=T),]
#    if(filterLogs=='all'){
#      logsTable
#    }else{
#      logsTable[grep(filterLogs, logsTable[,'type']),]
#    }
#  }
#},
#options=list(searching = FALSE,pageLength = 100, searchable=FALSE, paging=FALSE)
#)
#



output$downloadLogs <- downloadHandler(
  filename = function() {
    paste('AccessModLogs-', amSysTime(), '.csv', sep='')
  },
  content = function(file){
    logs<-reactiveLogTable()
    write.csv(logs,file) 
  } 

  )

