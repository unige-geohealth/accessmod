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
  class = "amTableMargin",
  amCenterTitle(amt(
    id = "analysis_zonal_stat_title"
  ),
  sub = amt(
    id = "analysis_zonal_stat_sub"
  )
  ),
  conditionalPanel(
    "isEmpty(input.popSelect) ||
    isEmpty(input.travelTimeSelect) ||
    isEmpty(input.zoneSelect) ",
    tags$p(
      class = "callout callout-info",
      amt(
        id = "analysis_zonal_stat_add"
      )
    )
  ),
  conditionalPanel(
    "isNotEmpty(input.popSelect) &&
    isNotEmpty(input.travelTimeSelect) &&
    isNotEmpty(input.zoneSelect) ",
    actionLink("helpLinkZoneCoverageTable",
      icon = icon("question-circle"),
      ""
    ),
    tabulator_output("zoneCoverageTable",
      height = "80vh"
    )
  )
)
