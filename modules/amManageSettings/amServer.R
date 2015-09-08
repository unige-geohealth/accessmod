#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# settings and admin task 



# get local revision as reactive expression
amVersionLocal<-reactive({
  amGetVersionLocal()
})

# get remote revision as reactive expression 
amVersionRemote<-reactive({
  amGetVersionRemote()
})

# btn show browser (only works in interactive mode).
observe({
  sB<-input$showBrowser
  if(!is.null(sB) && sB>0){
    browser()
  }
})
# reset grass region
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
       amMsg(session,'warning',paste('Check on the remote server for new revision. Please be patient.'),title='Module update')
      output$appVersionRemoteText<-renderUI({
        p('Remote revision:',tags$b(amVersionRemote()),'.')
      })
    }
})
})

observe({ 
  btnUpdate<-input$appUpdate
  amErrorAction(title='Check for update',{
    output$appVersionLocalText<-renderUI({
      p('Revision number',tags$b(amVersionLocal()),'( branch:',amGetCurrentBranch(),').')
    })
})
})

observe({
  btnUpdate<-input$appUpdate
   amErrorAction(title='Update AccessMod',{
    if(!is.null(btnUpdate) && btnUpdate>0){
      amVersionLocal<-amVersionLocal()
      amVersionRemote<-amVersionRemote()
      isolate({
        if(is.null(amVersionRemote) || isTRUE(nchar(amVersionRemote==0))){
          amMsg(session,'warning','Please check first for new version',title='No version found.')
        }else{
          amVersionRemote<-as.integer(gsub("[^[:digit:]]","",amVersionRemote))
          amVersionLocal<-as.integer(gsub("[^[:digit:]]","",amVersionLocal))
          if(amVersionRemote>amVersionLocal){
            amMsg(session,'warning',paste('App update requested. From revision:',amVersionLocal,'to',amVersionRemote,". Please be patient, this could take a while."),title='Module update')
            amUpdateApp()
            source('config.R')
            amMsg(session,'warning',paste('App update done from revision:',amVersionLocal,'to',amVersionRemote,". Auto restart in 5 seconds."),title='Module update')
            Sys.sleep(5)
            amRestart(session)
          }else{
            amMsg(session,'warning',paste('Local version seems to be up to date.'),title='Module update')
          }
        }
      })
    }

})
})


observeEvent(input$btnSetFileSizeLimit,{
  amErrorAction(title="Set upload limit",{
    maxSize =  as.integer(input$numSetUploadLimit)
    if(maxSize!=200){
      if(isTRUE(maxSize < 10 || maxSize > 1000))stop("File size not accepted. Min = 10 MiB; Max = 1000 MiB")
      shiny.maxRequestSize <-  maxSize*1024^2 
      amMsg(session,'warning',paste("Maximum file limit set to ",maxSize,"MiB. This change could lead to unexpected issues, proceed with caution. If applicable,  modify your virtual server settings accordingly."))  
    }
})
})



