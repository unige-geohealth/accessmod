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

wellPanel(
  amCenterTitle(div(icon("check-square-o"), amt(
    id = "analysis_validation_title"
  )),
  m = 0,
  h = 3,
  sub = amt(
    id = "analysis_validation_sub"
  )
  ),
  uiOutput("msgModule3"),
  conditionalPanel(
    condition = "(
  input.moduleSelector=='module_5'
  )",
    actionButton("btnZonalStat", amt(
      id = "analysis_validation_update_btn"
    ))
  ),
  conditionalPanel(
    condition = "(
  input.moduleSelector!='module_5'
  )",
    tagList(
      actionButton(
        "btnComputeAccessibility",
        amt("analysis_validation_compute_btn")
      )
    )
  )
)
