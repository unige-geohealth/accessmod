#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.



amAppVers<-reactive({
  amAppVersion()
})

amRemoteVers<-reactive({
  amRemoteVersion()
})


observe({
  t<-input$appUpdate
  if(!is.null(t) && t>0){
    amErrorAction(title='AccessMod 5 update',{
      if(is.null(amRemoteVers()) || isTRUE(nchar(amRemoteVers()==0))){
        amMsg(session,'warning','No remote version has been found. Are you connected to internet?',title='Module update')
      }else{
        if(amAppVers()<amRemoteVers()){
          amMsg(session,'warning',paste('App update requested. From revision:',amAppVers(),'to',amRemoteVers(),"Auto restart in 3 seconds."),title='Module update')
          Sys.sleep(3)
          amAppUpdate()
          amRestart(session)
        }else{
          amMsg(session,'warning',paste('App update requested, but no new version found.'),title='Module update')
        }
      }
})
  }
})

output$appVersion<-renderUI({ 
  tagList(
  tags$h4(img(src="logo/icons/logo32x32.png"),'Accessmod 5'),
  p('Local version:',amAppVers()),
  p('Remote version:',amRemoteVers())
  )
})
