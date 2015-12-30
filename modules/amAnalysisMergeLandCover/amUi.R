#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing land cover in a given order.
#
# USER INTERFACE.

fluidRow(
  uiOutput('helpPanelAccessibility'),
  amCenterTitle('Merged land cover',sub="Module for ordering and merging the data listed in the 'stack' into a new land cover layer"),
  column(width=12,
    amAccordionGroup(id="module1",itemList=list(
        "landCover"=list(
          title=div(icon("image"),icon("long-arrow-right"),icon("bars")," Add land cover to the stack"),
          content=tagList(
            sidebarPanel(width=3,
              h4("Land cover"),
              selectInput("landCoverSelect","Select land cover layer (raster):",choices=""),
              selectInput("landCoverSelectTable","Select land cover table (optional):",choices=""),
              uiOutput("stackLandcoverValidation"),
              actionButton("btnAddStackLcv","Add to the stack"),
              amProgressBar("lcvStackProgress")
              ),
            mainPanel(width=9,
              h4("Labeling of land cover classes"),
              fluidRow(
                amPanel(width=6,
                  h5("Labels in the land cover layer (raster)"),
                  tagList(
                    actionLink("mergeLcvUndo",icon=icon("undo"),"Reset to original values"),"|",
                    actionLink("mergeLcv",icon=icon("magic"),"Import label(s) from the right table"),"|",
                    actionLink('helpLinkLcvTable',icon=icon('question-circle'),'')
                    ),
                  hotable("landCoverRasterTable")
                  ),
                amPanel(width=6,
                  h5("Labels in the optional land cover table"),
                  hotable("landCoverSqliteTable")
                  )
                )
              )
            )
          ),
        "roads"=list(
          title=div(icon("road"),icon("long-arrow-right"),icon("bars")," Add roads to the stack"),
          content=tagList(
            sidebarPanel(width=3,
              h4("Roads"),
              selectInput("roadSelect","Select road layer (vector):",choices=""),
              selectInput("roadSelectClass","Select road class column (integer) :",choices=""),
              selectInput("roadSelectLabel","Select road label column (text) :",choices=""),
              uiOutput("stackRoadValidation"),
              actionButton("btnAddStackRoad","Add to the stack"),
              amProgressBar("roadStackProgress")
              ),
            mainPanel(width=9,
              amPanel(width=6,
                h4("Labeling of road classes"),
                actionLink("helpLinkRoadTable",icon=icon("question-circle"),""),
                hotable("roadPreviewTable")
                ),
              mainPanel(width=6)
              )
            )
          ),
        "barriers"=list(
          title=div(icon("ban"),icon("long-arrow-right"),icon("bars")," Add barriers to the stack"),
          content=tagList(
            sidebarPanel(width=3,
              h4("Barriers"),
              p("You can add several barriers to the stack"),
              selectInput("barrierSelect","Select barrier layer (vector):",choices="",multiple=F),
              radioButtons("barrierType", "Select barrier type:",
                c("Polygons" = "area",
                  "Lines" = "line",
                  "Points" = "point"),selected="", inline=TRUE),
              actionButton("btnAddStackBarrier","Add to the stack"),
              amProgressBar("barrierProgress")
              ),
            mainPanel(width=9,
              amPanel(width=6,
                h4("Selected barrier layer content"),
                hotable("barrierPreviewTable")
                ),
              mainPanel(width=6)
              )
            )
          ),
        "mergeLcv"=list(
          title=div(icon("bars"),icon("long-arrow-right"),icon("list-ol"),"Order and merge the stack"),
          content=tagList(
            sidebarPanel(width=3,
              h4("Merge stack"),
              p(tags$b("Display or hide the items in the stack")),
              actionButton("btnRmMerge","Hide all the items in the stack"),
              actionButton("btnAddMerge","Show all the items in the stack"),
              actionButton("btnDeleteStack","Delete all the items in the stack"),
              p(tags$b("Option")),
              checkboxInput("cleanArtefact","Clean artefacts (this can take some time)"),
              uiOutput("stackWarning"),
              textInput("stackTag","Add short tags",value=""),
              uiOutput("stackNameInfo"),
              actionButton("btnMerge","Merge the items in the stack"),
              amProgressBar("stackProgress")
              ),
            column(width=9,
              amAccordionGroup("stackTable",show=c(1),itemList=list(
                  "stack"=list(
                    title="Order and merge the stack",
                    content=tagList(
                      p("Change stack order by holding and paning the name of the item. Remove an item by clicking on the \"x\" on the right. Only the items appearing in this list will get merged."),
                      #div(class="box-body",addUIDep(
                          selectizeInput("mapStack","",choices="",selected="", 
                            multiple=TRUE, 
                            options = list(plugins = list("drag_drop", "remove_button")
                              )
                           # )
                          #)
                        )
                      )
                    ),
                  "stackConflict"=list(
                    title="Conflicting classes among items in the stack",
                    content=tagList(
                      p("Conflicting classes between the land cover and road network layers will appear in the table below. The classes in question have to be modified in the original layer."),
                      hotable("stackConflict"),
                      column(width=5,
                        checkboxInput("checkBoxStackCorrect","Show advanced option"),
                        conditionalPanel("input.checkBoxStackCorrect==true",
                          tags$p("Manually change the 'newClass' values and click on 'Quick correction' to apply. This will not change values from the original data: only the stack items will be updated."),
                          actionButton("btnCorrectStack","Quick correction")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )


