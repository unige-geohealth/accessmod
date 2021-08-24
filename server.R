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

# main server file.
function(input, output, session){

  amErrorAction(title="Shiny server",
    pBarFinalRm=F,{
      #
      # Session reactive values
      #

      # reactive value to hold event and logic 
      listen <- reactiveValues()
      # reactive object to hold variables in module "manage data" NOTE: could be merged with "listen"
      dataMetaList <- reactiveValues()
      # set global grassSession reactive values
      grassSession <- reactiveValues()
      # reactive values to store list of data set
      dataList <- reactiveValues()

      #
      # Set language
      #
      language <- amTranslateGetSavedLanguage()
      listen$language <- language 
      amTranslateDefault()

      updateSelectInput(session,
        inputId = 'selectLanguage',
        selected = amTranslateGetSavedLanguage()
      )

      observeEvent(input$selectLanguage,{
        listen$language <- input$selectLanguage 
        listen$dataListUpdate <- runif(1)
        amTranslateSetSavedLanguage(input$selectLanguage)
        amTranslateSetLanguageClient(amTranslateGetSavedLanguage())
        amTranslateDefault()
      })



      #
      # Grass session
      #

      # check if there is already an active grass session and update value accordingly.
      if(isTRUE(nchar(get.GIS_LOCK())>0)){
        grassSession$mapset <- execGRASS("g.mapset",flags="p",intern=T)
      }
      # initiate gisLock
      grassSession$gisLock<-NULL


      #
      # Initial memoisation 
      # 
      amReMemoizeCostlyFunctions()

      #
      # Data list update
      #
      observeEvent(listen$dataListUpdate,{
        amErrorAction(title="Data list observer",{
          #
          # Reset memoised function 
          #
          amReMemoizeCostlyFunctions()

          #
          # get available grass locations (does not need grass env yet)
          #
          grassSession$locations <- amGetGrassListLoc(config$pathGrassDataBase)

          #
          # Update data manager
          #
          amDataManager(
            config = config,
            dataList = dataList,
            grassSession = grassSession
          )

          #
          # Update 'dataListUpdated' for reseting inputs and such
          #
          listen$dataListUpdated <- runif(1)
})
      })

      #
      # Remove selectize cache (see ./tools/r/amSelectizeHelpers.R
      #
      observeEvent({
        input$selectProject
        input$selectLanguage
      },{
        dataList$cache_selectize <- list()
      })

      #
      # Modules pause / resume based on tabs
      #
      source(config$pathModuleManager,local=TRUE)

    })
}


