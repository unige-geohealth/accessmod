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
  amCenterTitle(amt("analysis_facility_selection_title"),
    sub = amt("analysis_facility_selection_sub")
  ),
  fluidRow(
    class = "amRowTable",
    h4(amt("analysis_facility_selected_facilities")),
    conditionalPanel(
      "!isNotEmpty(input.hfSelect)",
      tags$p(
        class = "callout callout-info",
        amt("analysis_facility_selection_add_data")
      )
    ),
    conditionalPanel(
      "isNotEmpty(input.hfSelect)",
      #
      #  Actions
      #
      div(
        class = "amTableMargin",
        #
        # Table of facilities (module 4 : origin facilities / FROM )
        #
        conditionalPanel(
          condition = "input.moduleSelector=='module_4'",
          tags$h3(amt("analysis_facility_selection_from"))
        ),
        tabulator_output("hfTable",
          height = "75vh"
        ),
        #
        # Table of facilities (module 4 :  destination facilities / TO)
        #
        conditionalPanel(
          condition = "input.moduleSelector=='module_4'",
          conditionalPanel(
            "!isNotEmpty(input.hfSelectTo)",
            tags$p(
              class = "callout callout-info",
              amt("analysis_facility_selection_add_data")
            )
          ),
          conditionalPanel(
            "isNotEmpty(input.hfSelect)",
            tags$h3(amt("analysis_facility_selection_to")),
            tabulator_output("hfTableTo",
              height = "75vh"
            )
          )
        )
      )
    )
  )
)
