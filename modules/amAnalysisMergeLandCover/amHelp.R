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

observeEvent(input$helpLinkLcvTable, {
  content <- tagList(
    tags$p(
      ams(
        id = "help_merge_lc_labels_edition_1"
      )
    ),
    tags$ol(
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_2"
        ),
        icon("magic"),
        ";"
      ),
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_3"
        )
      ),
      tags$li(
        ams(
          id = "help_merge_lc_labels_edition_4"
        )
      )
    ),
    tags$p(
      ams(
        id = "help_merge_lc_resetting_table_content"
      )
    )
  )


  amUpdateModal(
    panelId = "amHelpPanel",
    title = config$helpTitle,
    html = content
  )
})


observeEvent(input$helpLinkRoadTable, {
  content <- tagList(
    tags$p(
      ams(
        id = "help_merge_lc_column_preview"
      )
    ),
    tags$p(
      ams(
        id = "help_merge_lc_empty_labels_ban"
      )
    ),
    tags$p(
      ams(
        id = "help_merge_lc_different_classes_requirement"
      )
    )
  )
  amUpdateModal(
    panelId = "amHelpPanel",
    title = config$helpTitle,
    html = content
  )
})
