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
  amCenterTitle(amt(
    id = "analysis_travel_scenario_title"
  ),
  sub = amt(
    id = "analysis_travel_scenario_sub"
  )
  ),
  fluidRow(
    class = "amRowTable",
    h4(amt(
      id = "analysis_travel_scenario_processed"
    )),
    conditionalPanel(
      "isEmpty(input.mergedSelect)",
      tags$p(
        class = "callout callout-info",
        amt(
          id = "analysis_travel_scenario_add_lc"
        )
      )
    ),
    conditionalPanel(
      "isNotEmpty(input.mergedSelect)",
      actionLink("speedTableUndo",
        icon = icon("undo"),
        amt(
          id = "analysis_travel_scenario_reset"
        )
      ),
      "|",
      actionLink("speedTableMerge",
        icon = icon("magic"),
        amt(
          id = "analysis_travel_scenario_import_selected_table"
        )
      ),
      "|",
      actionLink("helpLinkSpeedTable",
        icon = icon("question-circle"),
        ""
      ),
      tabulator_output("speedRasterTable",
        height = "30vh"
      ),
      h4(amt(
        id = "analysis_travel_scenario_selected_table"
      )),
      conditionalPanel(
        "isEmpty(input.modelSelect)",
        tags$p(
          class = "callout callout-info",
          amt(
            id = "analysis_travel_scenario_add_table"
          )
        )
      ),
      conditionalPanel(
        "isNotEmpty(input.modelSelect)",
        uiOutput("speedTableMergeValidation"),
        tabulator_output("speedSqliteTable",
          height = "30vh"
        )
      )
    )
  )
)
