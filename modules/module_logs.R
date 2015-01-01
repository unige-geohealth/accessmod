#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# module logs :
# Display and download logs
#
output$modLogs<-renderUI({
  list(
    sidebarPanel(  
      p('Table of recent logs'),
      sliderInput('nLogsToKeep','Number of logs to show',min=1,max=1000,value=300,step=10),
      checkboxInput('noVerbose','Hide verbose message.',value=F),
      downloadButton('downloadLogs', label = "Download logs")
    ),
    mainPanel(  
      tableOutput('logsTable')
    )
  )
})



reactiveLogTable<-reactive({
  nK<-input$nLogsToKeep
  if(!is.null(nK) && !nK==""){  
    reactiveLogTable<-reactiveFileReader(1000,session,logPath,readFunc = readLogs, nToKeep=nK)
    logsTable<-reactiveLogTable()
    names(logsTable)<-c('date','msg','verbose')
    logsTable
  }else{
    NULL
  }
})



output$logsTable <- renderTable({
  noVerbose <- input$noVerbose
  logsTable <- reactiveLogTable()
  if(!is.null(logsTable) && !is.null(noVerbose)){
    
    if(noVerbose){
      logsTable<-logsTable[logsTable$verbose==F,c('date','msg')]
    }else{
      logsTable<-logsTable[,c('date','msg')]
    }
    logsTable<-logsTable[order(as.integer(row.names(logsTable))),]
    logsTable
  }
  
})




output$downloadLogs <- downloadHandler(
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(file){
    logs<-reactiveLogTable()
    write.csv(logs,file) 
  } 
)

