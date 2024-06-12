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
    id = "analysis_table_scaleup_title"
  ),
  sub = amt(
    id = "analysis_table_scaleup_sub"
  )
  ),
  fluidRow(
    class = "amRowTable",
    h4(amt(
      id = "analysis_table_scaleup_new_facility"
    )),
    conditionalPanel(
      "!isNotEmpty(input.mergedSelect) ||
      !isNotEmpty(input.hfSelect) ||
      !isNotEmpty(input.popSelect) ",
      tags$p(
        class = "callout callout-info",
        amt(
          id = "analysis_table_scaleup_add_items"
        )
      )
    ),
    conditionalPanel(
      "isNotEmpty(input.mergedSelect) ||
      isNotEmpty(input.hfSelect) ||
      isNotEmpty(input.popSelect) ",
      div(
        class = "amTableMargin",
        actionLink("btnAddRowCapacity",
          icon = icon("plus-circle"),
          amt(
            id = "analysis_table_scaleup_add_row_btn"
          )
        ),
        actionLink("btnRmRowCapacity",
          icon = icon("minus-circle"),
          amt(
            id = "analysis_table_scaleup_rm_row_btn"
          )
        ),
        hotable("capacityTable",
          height = "20vh"
        )
      ),
      h4(amt(
        id = "analysis_table_scaleup_suitability"
      )),
      div(
        class = "amTableMargin",
        actionLink("btnResetSuitTable",
          icon = icon("undo"),
          amt(
            id = "analysis_table_scaleup_reset_suitability_btn"
          )
        ),
        actionLink("btnRmSuitTableUnselected",
          icon = icon("minus-circle"),
          amt(
            id = "analysis_table_scaleup_rm_unselected_suitability_btn"
          )
        ),
        hotable("suitabilityTable",
          height = "20vh"
        )
      ),
      h4(amt(
        id = "analysis_table_scaleup_exclusion"
      )),
      div(
        class = "amTableMargin",
        actionLink("btnResetExcluTable",
          icon = icon("undo"),
          amt(
            id = "analysis_table_scaleup_reset_exclusion_btn"
          )
        ),
        actionLink("btnRmExcluUnselected",
          icon = icon("minus-circle"),
          amt(
            id = "analysis_table_scaleup_rm_exclusion_btn"
          )
        ),
        hotable("exclusionTable",
          height = "20vh"
        )
      )
    )
  )
)
