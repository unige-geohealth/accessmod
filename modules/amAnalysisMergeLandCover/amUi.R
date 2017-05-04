#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing land cover in a given order.
#
# USER INTERFACE.




uiAddLandCover = tags$div(class="row am-tab-content",
  tagList(
    sidebarPanel(width=4,
      amCenterTitle(title="Land cover",sub="Add land cover to the stack",h=3),
      selectInput("landCoverSelect","Select land cover layer (raster)",choices=""),
      selectInput("landCoverSelectTable","Select land cover table (optional)",choices=""),
      uiOutput("stackLandcoverValidation"),
      actionButton("btnAddStackLcv","Add to the stack")
      ),
    tags$div(class="col-xs-12 col-md-8 col-lg-6",
      h4("Labeling of land cover classes"),
      conditionalPanel("!isNotEmpty(input.landCoverSelect)",
        tags$p("Please add land cover data")
        ),
      conditionalPanel("isNotEmpty(input.landCoverSelect)",
        h5("Labels in the land cover layer (raster)"),
        tagList(
          actionLink("mergeLcvUndo",icon=icon("undo"),"Reset to original values"),"|",
          actionLink("mergeLcv",icon=icon("magic"),"Import label(s) from the table bellow"),"|",
          actionLink('helpLinkLcvTable',icon=icon('question-circle'),'')
          ),
        hotable("landCoverRasterTable")
        ),
      conditionalPanel("isNotEmpty(input.landCoverSelectTable)",
        h5("Labels in the optional land cover table"),
        hotable("landCoverSqliteTable")
        )
      )
    ) 
  )


uiAddRoad = tags$div(class="row am-tab-content",
  tagList(
    sidebarPanel(width=4,
      amCenterTitle(title="Roads",sub="Add roads to the stack",h=3),
      selectInput("roadSelect","Select road layer (vector)",choices=""),
      selectInput("roadSelectClass","Select road class column (integer) ",choices=""),
      selectInput("roadSelectLabel","Select road label column (text) ",choices=""),
      uiOutput("stackRoadValidation"),
      conditionalPanel(condition="input.showAdvancedTools==true",
        checkboxInput("checkDontAdd1000","Do not add 1000 to class < 1000",value=FALSE)
        ),
      actionButton("btnAddStackRoad","Add to the stack")
      ),
    tags$div(class="col-xs-12 col-md-8 col-lg-6",
      h4("Labeling of road classes"),
      conditionalPanel("!isNotEmpty(input.roadSelect)",
        tags$p("Please add road data")
        ),
      conditionalPanel("isNotEmpty(input.roadSelect)",
        actionLink("helpLinkRoadTable",icon=icon("question-circle"),""),
        hotable("roadPreviewTable")
        )
      )
    ) 
  )


uiAddBarrier = tags$div(class="row am-tab-content",
  tagList(
    sidebarPanel(width=4,
      amCenterTitle(title="Barriers",sub="Add barriers to the stack",h=3),
      p("You can add several barriers to the stack"),
      selectInput("barrierSelect","Select barrier layer (vector)",choices="",multiple=F),
      radioButtons("barrierType", "Select barrier type",
        c("Polygons" = "area",
          "Lines" = "line",
          "Points" = "point"),selected="", inline=TRUE),
      actionButton("btnAddStackBarrier","Add to the stack")
      ),

    tags$div(class="col-xs-12 col-md-8 col-lg-6",
      h4("Selected barrier layer content"),
      conditionalPanel("!isNotEmpty(input.barrierSelect)",
        tags$p("Please add barrier data")
        ),
      conditionalPanel("isNotEmpty(input.barrierSelect)",
        hotable("barrierPreviewTable")
        )
      )
    )
  )

uiMergeLandcover = tags$div(class="row am-tab-content",
  tagList(
    sidebarPanel(width=4,
      amCenterTitle(title="Merge",sub="Order and merge the stack",h=3),
      p(tags$b("Manage stack items")),
      actionButton("btnStackAllSkip","Skip all items"),
      actionButton("btnStackAllProcess","Use all items"),
      actionButton("btnDeleteStack","Delete skipped items"),
      p(tags$b("Option")),
      checkboxInput("cleanArtefact","Clean artefacts (this can take some time)"),
      uiOutput("stackWarning"),
      textInput("stackTag","Add short tags",value=""),
      uiOutput("stackNameInfo"),
      actionButton("btnMerge","Merge the items in the stack")
      ),
    tags$div(class="col-xs-12 col-md-8 col-lg-6",
      amAccordionGroup("stackTable",show=c(1),itemList=list(
          "stack"=list(
            title="Order and merge the stack",
            content=tagList(
              p("Reorder and move stack items using the mouse."),
              amDoubleSortableInput(
                "stackMapList",
                title1="Stack items to use",
                title2="Stack items to skip"
                ) 
              )
            ),
          "stackConflict"=list(
            title="If any, conflicting classes among items in the stack",
            content=tagList(
              p("Conflicting classes between the land cover and road network layers will appear in the table below. The classes in question have to be modified in the original layer."),
              hotable("stackConflict"),
              uiOutput("uiBtnCorrectStack")
              )
            )
          )
        )
      )

    ) 
  )

fluidRow(
  uiOutput('helpPanelAccessibility'),
  amCenterTitle('Merge land cover',sub="Module for ordering and merging the data listed in the 'stack' into a new land cover layer"),
  column(width=12,
    tabsetPanel(position='left',
      tabPanel("Land cover", uiAddLandCover),
      tabPanel("Roads", uiAddRoad) ,
      tabPanel("Barriers", uiAddBarrier),
      tabPanel("Merge", uiMergeLandcover)
      )
    )
  )
