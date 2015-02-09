#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# module logs :
# Display and download logs
#
output$moduleLogs<-renderUI({
  tagList(
    sidebarLayout(
      sidebarPanel(  
        h4('Logs'),
        sliderInput('nLogsToKeep','Number of last logs to show',min=1,max=1000,value=300,step=10),
        radioButtons('filterLogs','Filter',c('error','warning','message','log','all'),inline=TRUE),
        downloadButton('downloadLogs', label = "Download logs")
        ),
      mainPanel('')
      ),
    div(class='wide-table',
      amPanel(
        dataTableOutput('logsTable')
        )
      )
    ) 
})



reactiveLogTable<-reactive({
  nK<-input$nLogsToKeep
  if(!is.null(nK) && !nK==""){  
    reactiveLogTable<-reactiveFileReader(5000,session,logPath,readFunc = amReadLogs, nToKeep=nK)
    logsTable<-reactiveLogTable()
    names(logsTable)<-c('time','type','msg')
    logsTable
  }else{
    NULL
  }
})



output$logsTable <- renderDataTable({
  filterLogs<-input$filterLogs
  logsTable <- reactiveLogTable()
  if(!is.null(logsTable) && !is.null(filterLogs)){
    logsTable <- logsTable[order(logsTable$time,decreasing=T),]
    if(filterLogs=='all'){
      logsTable
    }else{
      logsTable[grep(filterLogs, logsTable[,'type']),]
    }
  }
},
options=list(searching = FALSE,pageLength = 100, searchable=FALSE, paging=FALSE)
)




output$downloadLogs <- downloadHandler(
  filename = function() {
    paste('AccessModLogs-', amSysTime(), '.csv', sep='')
  },
  content = function(file){
    logs<-reactiveLogTable()
    write.csv(logs,file) 
  } 

  )

