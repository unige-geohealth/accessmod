

amStoreObs <- function(obs,idModule,idObs){
  observers[[c(idModule,idObs)]] <<- obs
}
#
# Add observers list in global env.
#
observers <- list(
   module_project = list(),
   module_data =  list(),
   module_toolbox = list(),
   module_analysis = list(),
   module_logs = list(),
   module_settings = list(),
   module_about = list()
  )
#
# Source modules
#
modList<-dir(config$pathModule)
for(m in modList){

  amDebugMsg(paste("Init module ",m))

  modulePath <- file.path(config$pathModule,m)
  if(  m %in% c("amManageData","amGisPreview","amManageProject","amAnalysisMergeLandCover","amAnalysisAccessibility","amManageSettings")){
    amServPath<-file.path(modulePath,"amServer.R")
    amHelpPath<-file.path(modulePath,"amHelp.R")
    if(file.exists(amServPath)){
      source(amServPath,local=TRUE)
    }
    if(file.exists(amHelpPath)){
      source(amHelpPath,local=TRUE)
    }
  }
}


#
# Switch on/off observers based on tab
#
observeEvent(input$whichTab,{
  currentTab <- input$whichTab
  for(mod in names(observers)){
    for(obs in observers[[mod]]){
      if( mod == currentTab){
        obs$resume()
      }else{
        obs$suspend()
      }
    }
  }
})


