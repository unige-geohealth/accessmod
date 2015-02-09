#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.



output$moduleInfo<-renderUI({  
  sidebarPanel(
    iconLarge,
    tags$h3('Accessmod 5, version:', appVersion()),
    p('This is the development version of accessmod.'),
    actionButton('appUpdate',label="update AccessMod",icon="download"),
    actionButton('appRefresh',label='Restart application')
    )
})

observe({
  t<-input$appUpdate
  if(!is.null(t) && t>0){
    amMsg(session,'warning',paste('App update requested. Version: ',appVersion()),title='Module update')
    appUpdate()
    amMsg(session, 'warning',paste('App update finished.Version:',appVersion(),' please restart.'),title='Module update')
  }
})


output$js<-renderUI({
  t<-input$appRefresh
  if(!is.null(t) && t>0){ 
    amRestart(session)
  }
})
