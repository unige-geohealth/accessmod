#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# module logs :
# Display and download logs

tagList(
  sidebarLayout(
    sidebarPanel(  
      h4('Logs'),
      #numericInput('nLogsToKeep',"Number of logs to display",value=200,min=0,max=1000,step=1),
      sliderInput('nLogsToKeep','Number of last logs to show',min=1,max=1000,value=300,step=10),
      radioButtons('filterLogs','Filter',c('error','warning','message','log','all'),inline=TRUE),
      downloadButton('downloadLogs', label = "Download logs")
      ),
    mainPanel(
      hotable('logsTable') 
      )
    )
  ) 




