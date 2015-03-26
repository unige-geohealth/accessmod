#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.


observe({
  t<-input$appUpdate
  if(!is.null(t) && t>0){
    amErrorAction(title='Access mod update',{
      amMsg(session,'warning',paste('App update requested. Version: ',amAppVersion()),title='Module update')
      amAppUpdate()
      amMsg(session, 'warning',paste('App update finished.Version:',amAppVersion(),' please restart.'),title='Module update')
})
  }
})


output$js<-renderUI({
  t<-input$appRefresh
  if(!is.null(t) && t>0){ 
    amErrorAction(title='App restart',{
      amRestart(session)
})
  }
})




output$appVersion<-renderUI({ 
  tags$h4( img(src="logo/icons/logo32x32.png"),'Accessmod 5, version:',amAppVersion())
})
