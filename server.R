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
function(input, output, session) {
  amErrorAction(
    title = "Shiny server",
    pBarFinalRm = F,
    {
      #
      # GRASS session store
      #
      amGrassNS({
        #
        # Session reactive values
        #

        # set grass reactive values
        grassSession <- reactiveValues()

        # reactive value to hold event and logic
        listen <- reactiveValues()

        # reactive object to hold variables in module "manage data"
        dataMetaList <- reactiveValues()

        # reactive values to store list of data set
        dataList <- reactiveValues()

        #
        # Update settings
        #
        amUpdateClientSettings(
          list(
            settings = list(
              httpPort = config$network$httpPort
            ),
            dictionary = fromJSON(
              config$pathDictMain,
              simplifyDataFram = FALSE
            )
          )
        )

        #
        # Set language
        #
        language <- amTranslateGetSavedLanguage()
        listen$language <- language
        amTranslateDefault()

        updateSelectInput(session,
          inputId = "selectLanguage",
          selected = amTranslateGetSavedLanguage()
        )

        observeEvent(input$selectLanguage, {
          listen$language <- input$selectLanguage
          listen$dataListUpdate <- runif(1)
          amTranslateSetSavedLanguage(input$selectLanguage)
          amTranslateSetLanguageClient(amTranslateGetSavedLanguage())
          amTranslateDefault()
        })

        #
        # Initial memoisation
        #
        amReMemoizeCostlyFunctions()

        #
        # Data list update
        #
        observeEvent(listen$dataListUpdate, {
          amErrorAction(title = "Data list observer", {
            #
            # Reset memoised function
            #
            amReMemoizeCostlyFunctions()

            #
            # get available grass locations (does not need grass env yet)
            #
            grassSession$locations <- amGetGrassListLoc()

            #
            # Update data manager
            #
            amUpdateDataListObject(dataList)

            #
            # Update 'dataListUpdated' for reseting inputs and such
            #
            listen$dataListUpdated <- runif(1)
          })
        })

        #
        # Remove selectize cache (see ./tools/r/amSelectizeHelpers.R
        #
        observeEvent(
          {
            input$selectProject
            input$selectLanguage
          },
          {
            dataList$cache_selectize <- list()
          }
        )

        #
        # Modules pause / resume based on tabs
        #
        source(config$pathModuleManager, local = TRUE)
      })
    }
  )
}
