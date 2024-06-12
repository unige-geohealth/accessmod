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

# Module selector / top menu in analysis

moduleInlineBtn <- div(
  id = "moduleSelector",
  class = "form-group shiny-input-radiogroup shiny-input-container-inline shiny-flow-layout",
  "data-toggle" = "buttons",
  style = "line-height:37px;",
  tags$label("for" = "moduleSelector"),
  tags$label(
    class = "btn btn-default active btn-inline w6-100",
    tags$input(
      type = "radio",
      id = "ms2",
      name = "moduleSelector",
      value = "module_2",
      checked = "checked"
    ),
    icon("clock"), amt(
      id = "analysis_accessibility_main_tab"
    )
  ),
  tags$label(
    class = "btn btn-default btn-inline w6-100",
    tags$input(
      type = "radio",
      id = "ms3",
      name = "moduleSelector",
      value = "module_3"
    ),
    icon("hospital"), amt(
      id = "analysis_coverage_main_tab"
    )
  ),
  tags$label(
    class = "btn btn-default btn-inline w6-100 ",
    tags$input(
      type = "radio",
      id = "ms4",
      name = "moduleSelector",
      value = "module_4"
    ),
    icon("table"), amt(
      id = "analysis_referral_main_tab"
    )
  ),
  tags$label(
    class = "btn btn-default btn-inline w6-100",
    tags$input(
      type = "radio",
      id = "ms5",
      name = "moduleSelector",
      value = "module_5"
    ),
    icon("chart-bar"), amt(
      id = "analysis_zonal_stats_main_tab"
    )
  ),
  tags$label(
    class = "btn btn-default btn-inline w6-100 ",
    tags$input(
      type = "radio",
      id = "ms6",
      name = "moduleSelector",
      value = "module_6"
    ),
    icon("plus"), amt(
      id = "analysis_scalingup_main_tab"
    )
  )
)


fluidRow(
  column(
    width = 12,
    # HTML(gsub('\n', '', moduleInlineBtn))
    moduleInlineBtn
  ),
  hr(),
  column(
    width = 12,
    conditionalPanel(condition = "
      input.moduleSelector=='module_2' ||
      input.moduleSelector=='module_3' ||
      input.moduleSelector=='module_4' ||
      input.moduleSelector=='module_5' ||
       input.moduleSelector=='module_6'
      ", loadUi("modules/amAnalysisAccessibility/amUi.R"))
  )
)
