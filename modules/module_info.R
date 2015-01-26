#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.



output$modInfo<-renderUI({  
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
    msg('App update requested. Version: ',appVersion())
    appUpdate()
    msg('App update finished. Version: ',appVersion())
  }
})

output$js<-renderUI({
  t<-input$appRefresh
  if(!is.null(t) && t>0){ 
  tags$script("location.reload();")
  }
})
