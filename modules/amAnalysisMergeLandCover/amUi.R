#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing land cover in a given order.
#
# USER INTERFACE.




uiAddLandCover = tags$div(class = "row am-tab-content",
  tagList(
    sidebarPanel(width = 4,
      amCenterTitle(title = amt(
        id = "toolbox_land_cover_title",
        str = "Land cover"
        ),
        sub = amt(
          id = "toolbox_land_cover_add_land_cover_sub",
          str ="Add land cover to the stack"
          ),
          h = 3),
      selectInput("landCoverSelect", amt(
        id = "toolbox_land_cover_select_raster",
        str = "Select land cover layer (raster)"
        ),
        choices = ""),
      selectInput("landCoverSelectTable", amt(
        id = "toolbox_land_cover_table",
        str = "Select land cover table (optional)"
        ),
        choices = ""),
      uiOutput("stackLandcoverValidation"
      ),
      actionButton("btnAddStackLcv", amt(
        id = "toolbox_land_cover_add_to_stack_btn",
        str = "Add to the stack"
        ))
      ),
    tags$div(class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(
        id = "toolbox_land_cover_labeling_table",
        str = "Labeling of land cover classes"
        )),
      conditionalPanel("!isNotEmpty(input.landCoverSelect)",
        tags$p(amt(
          id = "toolbox_land_cover_add_data",
          str = "Please add land cover data"
          ))
        ),
      conditionalPanel("isNotEmpty(input.landCoverSelect)",
        h5(amt(
          id = "toolbox_land_cover_label_sub",
          str = "Labels in the land cover layer (raster)"
          )),
        tagList(
          actionLink("mergeLcvUndo",
          icon = icon("undo"
          ),
          amt(
            id = "toolbox_land_cover_reset_to_original",
            str = "Reset to original values"
            )),
            "|",
          actionLink("mergeLcv",
            icon = icon("magic"
            ),
            amt(
              id = "toolbox_land_cover_import_labels",
              str = "Import label(s) from the table below"
              )),
              "|",
          actionLink('helpLinkLcvTable',
          icon = icon('question-circle'
          ),
          '')
          ),
        hotable("landCoverRasterTable"
          )
        ),
      conditionalPanel("isNotEmpty(input.landCoverSelectTable)",
        h5(amt(
          id = "toolbox_land_cover_label_optional",
          str = "Labels in the optional land cover table"
          )),
        hotable("landCoverSqliteTable"
        ))
      )
    ) 
  )


uiAddRoad = tags$div(class = "row am-tab-content",
  tagList(
    sidebarPanel(width = 4,
      amCenterTitle(title = amt(
        id = "toolbox_land_cover_roads_title",
        str = "Roads"
        ),
        sub = amt(
          id = "toolbox_land_cover_roads_sub",
          str = "Add roads to the stack"
          ),
        h = 3
        ),
      selectInput("roadSelect", amt(
        id = "toolbox_land_cover_road_layer_vector",
        str = "Select road layer (vector)"
        ),
        choices = ""
        ),
      selectInput("roadSelectClass", amt(
        id = "toolbox_land_cover_road_column",
        str = "Select road class column (integer) "
        ),
        choices = ""
        ),
      selectInput("roadSelectLabel", amt(
        id = "toolbox_land_cover_road_label",
        str = "Select road label column (text) "
        ),
        choices = ""
        ),
      uiOutput("stackRoadValidation"),
      conditionalPanel(condition = "input.showAdvancedTools==true",
        checkboxInput("checkDontAdd1000", amt(
          id = "toolbox_land_cover_dont_add_1000",
          str = "Do not add 1000 to class < 1000"
          ),
          value = FALSE
          )
        ),
      actionButton("btnAddStackRoad", amt(
        id = "toolbox_land_cover_road_add_btn",
        str = "Add to the stack"
        ))
      ),
    tags$div(class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(
        id = "toolbox_land_cover_road_class_label",
        str = "Labeling of road classes"
        )),
      conditionalPanel("!isNotEmpty(input.roadSelect)",
        tags$p(amt(
          id = "toolbox_land_cover_road_data_add",
          str = "Please add road data"
          ))
        ),
      conditionalPanel("isNotEmpty(input.roadSelect)",
        actionLink("helpLinkRoadTable",
        icon = icon("question-circle"
        ),
        ""
        ),
        hotable("roadPreviewTable"
        ))
      )
    ) 
  )

uiAddBarrier = tags$div(class = "row am-tab-content",
  tagList(
    sidebarPanel(width = 4,
      amCenterTitle(title = amt(
        id = "toolbox_land_cover_barrier_title",
        str = "Barriers"
        ),
        sub = amt(
          id = "toolbox_land_cover_barrier_sub",
          str = "Add barriers to the stack"
          ),
        h = 3
        ),
      p(amt(
        id = "toolbox_land_cover_barrier_add_several",
        str = "You can add several barriers to the stack"
        )),
      selectInput("barrierSelect", amt(
        id = "toolbox_land_cover_barrier_select_vector",
        str = "Select barrier layer (vector)"
        ),
        choices = "",
        multiple = F
        ),
      radioButtons("barrierType", amt(
        id = "toolbox_land_cover_barrier_type",
        str = "Select barrier type"
        ),
        c("Polygons" = "area",
          "Lines" = "line",
          "Points" = "point"
          ),
        selected = "",
        inline = TRUE),
      actionButton("btnAddStackBarrier", amt(
        id = "toolbox_land_cover_barrier_add_btn",
        str = "Add to the stack"
        ))
      ),

    tags$div(class = "col-xs-12 col-md-8 col-lg-6",
      h4(amt(
        id = "toolbox_land_cover_barrier_selected_layer",
        str = "Selected barrier layer content"
        )),
      conditionalPanel("!isNotEmpty(input.barrierSelect)",
        tags$p(amt(
          id = "toolbox_land_cover_barrier_selected_sub",
          str = "Please add barrier data"
          ))
        ),
      conditionalPanel("isNotEmpty(input.barrierSelect)",
        hotable("barrierPreviewTable"
        ))
      )
    )
  )

uiMergeLandcover = tags$div(class = "row am-tab-content",
  tagList(
    sidebarPanel(width = 4,
      amCenterTitle(title = amt(
        id = "toolbox_land_cover_merge_title",
        str = "Merge"
        ),
        sub = amt(
          id = "toolbox_land_cover_merge_sub",
          str = "Order and merge the stack"
          ),
        h = 3),
      p(tags$b(amt(
        id = "toolbox_land_cover_merge_manage",
        str = "Manage stack items"
        ))),
      actionButton("btnStackAllSkip", amt(
        id = "toolbox_land_cover_merge_skip_btn",
        str = "Skip all items"
        )),
      actionButton("btnStackAllProcess", amt(
        id = "toolbox_land_cover_merge_all_btn",
        str = "Use all items"
        )),
      actionButton("btnDeleteStack", amt(
        id = "toolbox_land_cover_merge_del_btn",
        str = "Delete skipped items"
        )),
      p(tags$b(amt(
        id = "toolbox_land_cover_merge_option",
        str = "Option"
        ))),
      checkboxInput("cleanArtefact", amt(
        id = "toolbox_land_cover_merge_clean_artefact",
        str = "Clean artefacts (this can take some time)"
        )),
      uiOutput("stackWarning"),
      textInput("stackTag", amt(
        id = "toolbox_land_cover_merge_tag",
        str = "Add short tags"
        ),
        value = ""),
      uiOutput("stackNameInfo"),
      actionButton("btnMerge", amt(
        id = "toolbox_land_cover_merge_btn",
        str = "Merge the items in the stack"
        ))
      ),
    tags$div(class = "col-xs-12 col-md-8 col-lg-6",
      amAccordionGroup("stackTable",
        show = c(1),
        itemList = list(
          "stack" = list(
            title = amt(
              id = "toolbox_land_cover_merge_order",
              str = "Order and merge the stack"
              ),
            content = tagList(
              p(amt(
                id = "toolbox_land_cover_merge_reorder_mouse",
                str = "Reorder and move stack items using the mouse."
                )),
              amDoubleSortableInput(
                "stackMapList",
                title1 = amt(
                  id = "toolbox_land_cover_merge_items_to_use",
                  str = "Stack items to use"
                  ),
                title2 = amt(
                  id = "toolbox_land_cover_merge_items_to_skip",
                  str = "Stack items to skip"
                  )
                ) 
              )
            ),
          "stackConflict" = list(
            title = amt(
              id = "toolbox_land_cover_merge_conflict_warning",
              str = "If any, conflicting classes among items in the stack"
              ),
            content = tagList(
              p(amt(
                id = "toolbox_land_cover_merge_conflict_explanation",
                str = "Conflicting classes between the land cover and road network layers will appear in the table below. The classes in question have to be modified in the original layer."
                )),
              hotable("stackConflict"
              ),
              uiOutput("uiBtnCorrectStack"
              ))
            )
          )
        )
      )
    ) 
  )

fluidRow(
  uiOutput('helpPanelAccessibility'),
  amCenterTitle(amt(
    id = "toolbox_land_cover_merge_main",
    str = 'Merge land cover'
    ),
    sub = amt(
      id = "toolbox_land_cover_merge_main_sub",
      str = "Module for ordering and merging the data listed in the 'stack' into a new land cover layer"
      )),
  column(width = 12,
    tabsetPanel(
      tabPanel(amt(
        id = "toolbox_land_cover_tab",
        str = "Land cover"
        ),
        uiAddLandCover),
      tabPanel(amt(
        id = "toolbox_land_cover_roads_tab",
        str = "Roads"
        ),
        uiAddRoad),
      tabPanel(amt(
        id = "toolbox_land_cover_barrier_tab",
        str = "Barriers"
        ),
        uiAddBarrier),
      tabPanel(amt(
        id = "toolbox_land_cover_merge_tab",
        str = "Merge"
        ),
        uiMergeLandcover)
      )
    )
  )
