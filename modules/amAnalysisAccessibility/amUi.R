#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Accessibility modules ui.
# 
# USER INTERFACE
# TODO: break into small parts.

fluidRow(
  column(width=4,
    #
    # Select input data
    #
    loadUi("modules/amAnalysisAccessibility/amUi_modulesData.R"),
    #
    # Module 6 scaling up table
    #
    conditionalPanel(
      condition="input.moduleSelector=='module_6'",
      loadUi("modules/amAnalysisAccessibility/amUi_scalingUpTablesSettings.R")
      ),
    conditionalPanel(
      condition="input.moduleSelector!='module_5'",
      loadUi("modules/amAnalysisAccessibility/amUi_modulesSettings.R")
      ),
    conditionalPanel(
      condition="input.moduleSelector!='module_5'",
      loadUi("modules/amAnalysisAccessibility/amUi_modulesValidation.R")
      )
    ),
  #
  # Right panel with table / Graphs
  #
  # column(id="accessibilityRightPanel",width=7,
  column(width=7,
    conditionalPanel(condition="input.moduleSelector!='module_5'",
      #
      # Scenario tables
      #
      loadUi("modules/amAnalysisAccessibility/amUi_tableScenario.R"),
      #
      # Scaling up tables
      #
      conditionalPanel(condition="input.moduleSelector=='module_6'",
        loadUi("modules/amAnalysisAccessibility/amUi_tablesScalingUp.R")
        ),
      #
      # Facilities tables
      # 
        loadUi("modules/amAnalysisAccessibility/amUi_tablesFacilities.R")
      ),
    conditionalPanel(condition="input.moduleSelector=='module_5'",
      loadUi("modules/amAnalysisAccessibility/amUi_zonalStat.R")
      )
    )
  )


