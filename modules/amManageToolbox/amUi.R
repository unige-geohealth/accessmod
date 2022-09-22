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

# Module selector / top menu in analysis

toolInlineBtn <- div(
  id = "toolSelector",
  class = "form-group shiny-input-radiogroup shiny-input-container-inline shiny-flow-layout",
  "data-toggle" = "buttons",
  style = "line-height:37px;",
  tags$label("for" = "toolSelector"),
  tags$label(
    class = "btn btn-default btn-inline active",
    tags$input(
      type = "radio",
      id = "tool2",
      name = "toolSelector",
      value = "tool_map_preview",
      checked = "checked"
    ),
    icon("map"), amt(
      id = "tools_map_btn"
    )
  ),
  tags$label(
    class = "btn btn-default btn-inline",
    tags$input(
      type = "radio",
      id = "tool1",
      name = "toolSelector",
      value = "tool_merge_landcover"
    ),
    icon("list"), amt(
      id = "tools_merge_lc_btn"
    )
  ),
  tags$label(
    class = "btn btn-default btn-inline",
    tags$input(
      type = "radio",
      id = "tool3",
      name = "toolSelector",
      value = "tool_pop_correction"
    ),
    icon("users"), amt(
      id = "tools_correct_pop_btn"
    )
  )
)

fluidRow(
  column(
    width = 12,
    HTML(gsub("\n", "", toolInlineBtn))
  ),
  hr(),
  column(
    width = 12,
    conditionalPanel(
      condition = "
      input.toolSelector=='tool_merge_landcover'
      ",
      loadUi("modules/amAnalysisMergeLandCover/amUi.R")
    ),
    conditionalPanel(
      condition = "
      input.toolSelector=='tool_map_preview'
      ",
      loadUi("modules/amGisPreview/amUi.R")
    ),
    conditionalPanel(
      condition = "
      input.toolSelector=='tool_pop_correction'
      ",
      loadUi("modules/amPopulationBarrierCorrection/amUi.R")
    )
  )
)
