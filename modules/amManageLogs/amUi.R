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
      h4(amt(
          id = "logs_title"
          )),
      #numericInput('nLogsToKeep',"Number of logs to display",value = 200,min = 0,max = 1000,step = 1),
      sliderInput('nLogsToKeep', amt(
          id = "logs_to_show"
          ),
        min = 1,
        max = 1000,
        value = 300,
        step = 10
        ),
      amRadioButtons('filterLogs', amt(
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
          'error',
          'warning',
          'message', 
          'log', 
          'all'
          )
        ),
      downloadButton('downloadLogs', 
        label = amt(
          id = "logs_download"
          )
        )
      ),
    mainPanel(
      hotable('logsTable'
        ))
    )
  ) 




