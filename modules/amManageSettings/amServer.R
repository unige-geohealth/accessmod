#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# settings and admin task 


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
  grassMeta<-HTML("<p>Reloaded project spatial metadata:</p>",listToHtml(listen$mapMeta$grid,h=4))
  amMsg(session,type="warning",title="Roload project meta data",subtitle='summary',text=grassMeta,logFile=config$pathLog)
})


observe({
  if(isTRUE(input$whichTab == "module_settings")){
    amErrorAction(title="Settings : Version check",{
      isolate({
        msg <- list(
          `Branch`           = amGetCurrentBranch(),
          `Current revision` = amGetVersionLocal(),
          `Latest revision`  = amGetVersionRemote(),
          `Node name`        = Sys.info()['nodename']
          )





        amUpdateText(id="txtAccessmodVersion",listToHtml(h=6,msg))

        #
        # Update button
        #
        if(identical(as.character(config$hostname),"accessmod")){
          if(!identical(amGetVersionLocal(),amGetVersionRemote())){
            output$amUpdate <- renderUI({
              tagList(
                p("An update is available."),
                actionButton("btnInstall","Install update")
                )
            })
          }
        }
      })
})
  }
})


observeEvent(input$btnInstall,{
  amErrorAction(title="Settings : update application",{
    amUpdateApp()
})
})

observeEvent(input$btnRestart,{
  amRestart()
})

#
#
## get local revision as reactive expression
#amVersionLocal<-reactive({
#  amGetVersionLocal()
#})
#
## get remote revision as reactive expression 
#amVersionRemote<-reactive({
#  amGetVersionRemote()
#})
#
#
## observe fetch 
#observe({
#  btnFetch<-input$appFetchGit
#  amErrorAction(title="Check for update",{
#    if(!is.null(btnFetch) && btnFetch>0){
#       amMsg(session,"warning",paste("The remote revision number will appear below the current one once obtained from the server."),title="Module update",subtitle="Information")
#      output$appVersionRemoteText<-renderUI({
#        p("Remote revision:",tags$b(amVersionRemote()),".")
#      })
#    }
#})
#})
#
#observe({ 
#  btnUpdate<-input$appUpdate
#  amErrorAction(title="Check for update",{
#    output$appVersionLocalText<-renderUI({
#      p("Revision number",tags$b(amVersionLocal()),"( branch:",amGetCurrentBranch(),").")
#    })
#})
#})
#
#observe({
#  btnUpdate<-input$appUpdate
#   amErrorAction(title="Update AccessMod",{
#    if(!is.null(btnUpdate) && btnUpdate>0){
#      tit <- "Version manager"
#      sub <- "Information"
#      amVersionLocal<-amVersionLocal()
#      amVersionRemote<-amVersionRemote()
#      isolate({
#        if(is.null(amVersionRemote) || isTRUE(nchar(amVersionRemote==0))){
#          msg <- "Please check first for new version"
#          sub <- "No version found."
#          amMsg(session,"warning",title=tit,subtitle=sub,text=msg)
#        }else{
#          amVersionRemote<-as.integer(gsub("[^[:digit:]]","",amVersionRemote))
#          amVersionLocal<-as.integer(gsub("[^[:digit:]]","",amVersionLocal))
#
#          if(amVersionRemote>amVersionLocal){
#            # set messages
#            msg <- paste("App update requested. From revision:",amVersionLocal,"to",amVersionRemote,". Please be patient, this could take a while.")
#            sub <- "Please wait..."
#            # send message
#            amMsg(session,"warning",title=tit,subtitle=sub,text=msg)
#            # update application
#            amUpdateApp()
#            # reload config 
#            source("config.R")
#            #set message
#            amMsg(session,"warning",title=tit,subtitle="Restarting...",text="",listActionButton=list())
#            # add touch restart.txt to reload shiny server at next connection
#            system("touch restart.txt")
#            Sys.sleep(5)
#            amRestart(session)
#          }else{
#            amMsg(session,"warning",paste("The local version is up-to-date."),title=tit,subtitle="up-to-date")
#          }
#        }
#      })
#    }
#
#})
#})
#
#

observeEvent(input$btnSetFileSizeLimit,{
  amErrorAction(title="Set upload limit",{
    maxSize =  as.integer(input$numSetUploadLimit)
    if(isTRUE(maxSize < 10 || maxSize > 1000)){
      stop("File size not accepted. Min = 10 MB; Max = 1000 MB")
    }else{ 
      options(shiny.maxRequestSize= maxSize*1024^2)
    }
    if( ! maxSize == config$maxUploadSize ){

      warn <- "This change could lead to unexpected issues, proceed with caution. If applicable, modify your virtual server settings accordingly."
    }else{
      warn <- ""
    }

    txt <- sprintf("Data importing limit temporary set to %s MB. %s",
      maxSize,
      warn
      )

    amMsg(session,"warning",title="Updating upload limit",text=txt)  
})
})



