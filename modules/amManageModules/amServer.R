#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.


#
# NOTE: This script store all observers and suspend or resume them based on 
# Which tab is currently oppened in UI
#

amStoreObs <- function(obs,idModule,idObs){
  observers[[c(idModule,idObs)]] <<- obs
}
#
# Add observers list in global env.
#
observers <- list(
  module_project = list(),
  module_data = list(),
  module_toolbox = list(),
  module_analysis = list(),
  module_logs = list(),
  module_settings = list(),
  module_about = list()
  )

#
# Source modules
#
modList <- dir(config$pathModule)
for(m in modList){

  modulePath <- file.path(config$pathModule,m)
  if( m %in% c(
      "amManageData",
      "amGisPreview",
      "amManageProject",
      "amAnalysisMergeLandCover",
      "amAnalysisAccessibility",
      "amManageSettings",
      "amManageLogs",
      "amPopulationBarrierCorrection"
      )){
    amServPath<-file.path(modulePath, "amServer.R")
    amHelpPath<-file.path(modulePath, "amHelp.R")
    if(file.exists(amServPath)){
      source(amServPath, local=TRUE)
    }
    if(file.exists(amHelpPath)){
      source(amHelpPath, local=TRUE)
    }
  }
}


#
# Resume or suspend observers based on tab
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


