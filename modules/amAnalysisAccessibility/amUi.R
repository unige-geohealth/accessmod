#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

fluidRow(
  column(
    width = 4,
    #
    # Select input data
    #
    loadUi("modules/amAnalysisAccessibility/amUi_modulesData.R"),
    #
    # Module 6 scaling up table
    #
    conditionalPanel(
      condition = "input.moduleSelector=='module_6'",
      loadUi("modules/amAnalysisAccessibility/amUi_scalingUpTablesSettings.R")
    ),
    loadUi("modules/amAnalysisAccessibility/amUi_modulesSettings.R"),
    loadUi("modules/amAnalysisAccessibility/amUi_modulesValidation.R")
  ),
  #
  # Right panel with table / Graphs
  #
  # column(id = "accessibilityRightPanel",width = 7,
  tags$div(
    class = "col-xs-12 col-md-8 col-lg-7",
    conditionalPanel(
      condition = "input.moduleSelector!='module_5'",
      #
      # Scenario tables
      #
      loadUi("modules/amAnalysisAccessibility/amUi_tableScenario.R"),
      #
      # Scaling up tables
      #
      conditionalPanel(
        condition = "input.moduleSelector=='module_6'",
        loadUi("modules/amAnalysisAccessibility/amUi_tablesScalingUp.R")
      ),
      #
      # Facilities tables
      #
      #      conditionalPanel(condition = "!(input.moduleSelector=='module_6' & input.useExistingHf == 'FALSE')",
      loadUi("modules/amAnalysisAccessibility/amUi_tablesFacilities.R")
      #       )
    ),
    conditionalPanel(
      condition = "input.moduleSelector=='module_5'",
      loadUi("modules/amAnalysisAccessibility/amUi_zonalStat.R")
    )
  )
)
