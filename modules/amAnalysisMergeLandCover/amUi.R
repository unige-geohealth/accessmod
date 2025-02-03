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

uiAddLandCover <- tags$div(
  class = "row am-tab-content",
  tagList(
    sidebarPanel(
      width = 4,
      amCenterTitle(
        title = amt(
          id = "toolbox_land_cover_title"
        ),
        sub = amt(
          id = "toolbox_land_cover_add_land_cover_sub"
        ),
        h = 3
      ),
      selectInput("landCoverSelect", amt(
        id = "toolbox_land_cover_select_raster"
      ),
      choices = ""
      ),
      selectInput("landCoverSelectTable", amt(
        id = "toolbox_land_cover_table"
      ),
      choices = ""
      ),
      uiOutput("stackLandcoverValidation"),
      actionButton("btnAddStackLcv", amt(
        id = "toolbox_land_cover_add_to_stack_btn"
      ))
    ),
    tags$div(
      class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(
        id = "toolbox_land_cover_labeling_table"
      )),
      conditionalPanel(
        "isEmpty(input.landCoverSelect)",
        tags$p(
          class = "callout callout-info",
          amt(
            id = "toolbox_land_cover_add_data"
          )
        )
      ),
      conditionalPanel(
        "isNotEmpty(input.landCoverSelect)",
        h5(amt(
          id = "toolbox_land_cover_label_sub"
        )),
        tagList(
          actionLink("mergeLcvUndo",
            icon = icon("undo"),
            amt(
              id = "toolbox_land_cover_reset_to_original"
            )
          ),
          "|",
          actionLink("mergeLcv",
            icon = icon("magic"),
            amt(
              id = "toolbox_land_cover_import_labels"
            )
          ),
          "|",
          actionLink("helpLinkLcvTable",
            icon = icon("question-circle"),
            ""
          )
        ),
        tabulator_output("landCoverRasterTable",
          height = "30vh"
        )
      ),
      conditionalPanel(
        "isNotEmpty(input.landCoverSelectTable)",
        h5(amt(
          id = "toolbox_land_cover_label_optional"
        )),
        tabulator_output("landCoverSqliteTable",
          height = "30vh"
        )
      )
    )
  )
)

uiAddRoad <- tags$div(
  class = "row am-tab-content",
  tagList(
    sidebarPanel(
      width = 4,
      amCenterTitle(
        title = amt(
          id = "toolbox_land_cover_roads_title"
        ),
        sub = amt(
          id = "toolbox_land_cover_roads_sub"
        ),
        h = 3
      ),
      selectInput("roadSelect", amt(
        id = "toolbox_land_cover_road_layer_vector"
      ),
      choices = ""
      ),
      selectInput("roadSelectClass", amt(
        id = "toolbox_land_cover_road_column"
      ),
      choices = ""
      ),
      selectInput("roadSelectLabel", amt(
        id = "toolbox_land_cover_road_label"
      ),
      choices = ""
      ),
      uiOutput("stackRoadValidation"),
      conditionalPanel(
        condition = "input.showAdvancedTools==true",
        checkboxInput("checkDontAdd1000", amt(
          id = "toolbox_land_cover_dont_add_1000"
        ),
        value = FALSE
        )
      ),
      actionButton("btnAddStackRoad", amt(
        id = "toolbox_land_cover_road_add_btn"
      ))
    ),
    tags$div(
      class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(
        id = "toolbox_land_cover_road_class_label"
      )),
      conditionalPanel(
        "isEmpty(input.roadSelect)",
        tags$p(
          class = "callout callout-info",
          amt(
            id = "toolbox_land_cover_road_data_add"
          )
        )
      ),
      conditionalPanel(
        "isNotEmpty(input.roadSelect)",
        actionLink("helpLinkRoadTable",
          icon = icon("question-circle"),
          ""
        ),
        tabulator_output("roadPreviewTable",
          height = "80vh"
        )
      )
    )
  )
)

uiAddBarrier <- tags$div(
  class = "row am-tab-content",
  tagList(
    sidebarPanel(
      width = 4,
      amCenterTitle(
        title = amt(
          id = "toolbox_land_cover_barrier_title"
        ),
        sub = amt(
          id = "toolbox_land_cover_barrier_sub"
        ),
        h = 3
      ),
      p(amt(
        id = "toolbox_land_cover_barrier_add_several"
      )),
      selectInput("barrierSelect", amt(
        id = "toolbox_land_cover_barrier_select_vector"
      ),
      choices = "",
      multiple = F
      ),
      amRadioButtons("barrierType", amt(
        id = "toolbox_land_cover_barrier_type"
      ),
      choiceNames = list(
        amt("toolbox_land_cover_barrier_type_polygons"),
        amt("toolbox_land_cover_barrier_type_lines"),
        amt("toolbox_land_cover_barrier_type_points")
      ),
      choiceValues = list("area", "line", "point"),
      selected = "",
      inline = TRUE
      ),
      conditionalPanel(
        condition = "input.barrierType === 'area'",
        checkboxInput("checkBarrierPolyAsSkeleton",
          label = amt("toolbox_land_cover_poly_as_skeleton")
        ),
        conditionalPanel(
          condition = "input.checkBarrierPolyAsSkeleton === true",
          numericInput("numBarrierSkeletonRes",
            label = amt("toolbox_land_cover_poly_skeleton_res"),
            value = 20,
            min = 10,
            max = 1000
          ),
          tags$small(
            class = "text-muted",
            textOutput("txtMessageSkeletonRes")
          ),
          numericInput("numBarrierSkeletonBuffer",
            label = amt("toolbox_land_cover_poly_skeleton_buffer"),
            value = 20,
            min = 10,
            max = 1000
          ),
          tags$small(
            class = "text-muted",
            textOutput("txtMessageSkeletonBuffer")
          )
        )
      ),
      actionButton("btnAddStackBarrier", amt(
        id = "toolbox_land_cover_barrier_add_btn"
      )),
      uiOutput("msgAddStackBarrier")
    ),
    tags$div(
      class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(
        id = "toolbox_land_cover_barrier_selected_layer"
      )),
      conditionalPanel(
        "isEmpty(input.barrierSelect)",
        tags$p(
          class = "callout callout-info",
          amt(
            id = "toolbox_land_cover_barrier_selected_sub"
          )
        )
      ),
      conditionalPanel(
        "isNotEmpty(input.barrierSelect)",
        tabulator_output("barrierPreviewTable",
          height = "80vh"
        )
      )
    )
  )
)

uiMergeLandcover <- tags$div(
  class = "row am-tab-content",
  tagList(
    sidebarPanel(
      width = 4,
      amCenterTitle(
        title = amt(
          id = "toolbox_land_cover_merge_title"
        ),
        sub = amt(
          id = "toolbox_land_cover_merge_sub"
        ),
        h = 3
      ),
      p(tags$b(amt(
        id = "toolbox_land_cover_merge_manage"
      ))),
      actionButton("btnStackAllSkip", amt(
        id = "toolbox_land_cover_merge_skip_btn"
      )),
      actionButton("btnStackAllProcess", amt(
        id = "toolbox_land_cover_merge_all_btn"
      )),
      actionButton("btnDeleteStack", amt(
        id = "toolbox_land_cover_merge_del_btn"
      )),
      p(tags$b(amt(
        id = "toolbox_land_cover_merge_option"
      ))),
      checkboxInput("cleanArtefact",
        label = tags$div(
          amt("toolbox_land_cover_merge_clean_artefact"),
          tags$small(
            class = "text-muted",
            amt("toolbox_land_cover_merge_clean_artefact_desc")
          )
        ),
        value = TRUE
      ),
      uiOutput("stackWarning"),
      textInput("stackTag", amt(
        id = "toolbox_land_cover_merge_tag"
      ),
      value = ""
      ),
      uiOutput("stackNameInfo"),
      actionButton("btnMerge", amt(
        id = "toolbox_land_cover_merge_btn"
      ))
    ),
    tags$div(
      class = "col-xs-12 col-md-8 col-lg-8",
      amAccordionGroup("stackTable",
        show = c(1),
        itemList = list(
          "stack" = list(
            title = amt(
              id = "toolbox_land_cover_merge_order"
            ),
            content = tagList(
              p(amt(
                id = "toolbox_land_cover_merge_reorder_mouse"
              )),
              amDoubleSortableInput(
                "stackMapList",
                title1 = amt(
                  id = "toolbox_land_cover_merge_items_to_use"
                ),
                title2 = amt(
                  id = "toolbox_land_cover_merge_items_to_skip"
                )
              )
            )
          ),
          "stackConflict" = list(
            title = amt(
              id = "toolbox_land_cover_merge_conflict_warning"
            ),
            content = tagList(
              p(amt(
                id = "toolbox_land_cover_merge_conflict_explanation"
              )),
              tabulator_output("stackConflict"),
              uiOutput("uiBtnCorrectStack")
            )
          )
        )
      )
    )
  )
)

fluidRow(
  uiOutput("helpPanelAccessibility"),
  amCenterTitle(
    amt(
      id = "toolbox_land_cover_merge_main"
    ),
    sub = amt(
      id = "toolbox_land_cover_merge_main_sub"
    )
  ),
  column(
    width = 12,
    tabsetPanel(
      tabPanel(
        amt(
          id = "toolbox_land_cover_tab"
        ),
        uiAddLandCover
      ),
      tabPanel(
        amt(
          id = "toolbox_land_cover_roads_tab"
        ),
        uiAddRoad
      ),
      tabPanel(
        amt(
          id = "toolbox_land_cover_barrier_tab"
        ),
        uiAddBarrier
      ),
      tabPanel(
        amt(
          id = "toolbox_land_cover_merge_tab"
        ),
        uiMergeLandcover
      )
    )
  )
)
