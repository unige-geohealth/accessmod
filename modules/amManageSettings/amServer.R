#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# description of the project 
# TODO : link the wiki here.



# reactive expression
amVersionLocal<-reactive({
  amGetVersionLocal()
})

amVersionRemote<-reactive({
  amGetVersionRemote()
})



# Handle Browser Btn.
observe({
  sB<-input$showBrowser
  if(!is.null(sB) && sB>0){
    browser()
  }
})




observeEvent(input$grassResetRegion,{
   grassReloadRegion(config$mapDem)
  listen$mapMeta<-amMapMeta()
  grassMeta<-HTML(listToHtml(listen$mapMeta$grid,h=4))
  amMsg(session,type='warning',title='Roload project meta data',text=grassMeta,logFile=config$pathLog)
})


# observe action
observe({
  btnFetch<-input$appFetchGit
  amErrorAction(title='Check for update',{
    if(!is.null(btnFetch) && btnFetch>0){
      output$appVersionRemoteText<-renderUI({
        p('Remote version:',tags$b(amVersionRemote()),'.')
      })
    }
})
})

observe({ 
  btnUpdate<-input$appUpdate
  amErrorAction(title='Check for update',{
    output$appVersionLocalText<-renderUI({
      p('Revisin number',tags$b(amVersionLocal()),'( branch:',amGetCurrentBranch(),').')
    })
})
})

observe({
  btnUpdate<-input$appUpdate
  amErrorAction(title='Update AccessMod',{
    if(!is.null(btnUpdate) && btnUpdate>0){
      isolate({
        if(is.null(amVersionRemote()) || isTRUE(nchar(amVersionRemote()==0))){
          amMsg(session,'warning','Please check first for new version',title='No version found.')
        }else{
          amVersionRemote<-as.integer(gsub("[^[:digit:]]","",amVersionRemote()))
          if(amVersionRemote>amVersionLocal()){
            amMsg(session,'warning',paste('App update requested. From revision:',amVersionLocal(),'to',amVersionRemote,"Auto restart in 3 seconds."),title='Module update')
            Sys.sleep(3)
            #update.packages(ask=FALSE, checkBuilt=TRUE)
            amUpdateApp()
            amRestart(session)
          }else{
            amMsg(session,'warning',paste('Local version seems to be up to date.'),title='Module update')
          }
        }
      })
    }

})
})

