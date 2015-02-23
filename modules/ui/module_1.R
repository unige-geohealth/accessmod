#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 1 : Add road and barrier to an existing landcover in a given order.
#
# USER INTERFACE.
div(class='module1',id='empty',
tagList(
  fluidRow(
    sidebarPanel(width=3,
      h4('Merge stack'),
      amProgressBar('stackProgress'),
      p(tags$b('Display or hide stack items')),
      actionButton('btnRmMerge',"Hide all stack items"),
      actionButton('btnAddMerge',"Show all stack items"),
      p(tags$b('Options')),
      checkboxInput('cleanArtefact','Clean artefacts (can take a while)'),
      uiOutput('stackWarning'),
      textInput('stackTag','Add tags (minimum 1)',value=''),
      actionButton('btnMerge',"Merge stack")
      ),
    amPanel(width=9,
      tabsetPanel(
        tabPanel('Stack order',
          p('Select the order to merge maps'),
          addUIDep(
            selectizeInput("mapStack","",choices="",selected="", 
              multiple=TRUE, options = list(plugins = list("drag_drop", "remove_button")
                )
              )
            )
          ),
        tabPanel('Stack class conflict',
          hotable('stackConflict')
          )
        )
      )
    ),
  fluidRow(
    sidebarPanel(width=3,
      h4('Land cover'),
      amProgressBar('lcvStackProgress'),
      selectInput('landCoverSelect','Select land cover map:',choices=""),
      selectInput('landCoverSelectTable','Select optional land cover table:',choices=""),
      p('Save raster value and add land cover raster on stack:'),
      actionButton('btnAddStackLcv','Add to stack')
      ),
    mainPanel(width=9,
      h4('Table of land cover categories'),
      fluidRow(
        amPanel(width=6,
          h5('Categories from raster'),
          p("Edit the column 'Label' to change raster table or copy and paste from spreadsheet."), 
          actionButton('mergeLcvUndo',icon=icon('undo'),'reset'),
          hotable("landCoverRasterTable")
          ),
        amPanel(width=6,
          h5('Categories from table'),
          p('Value from imported land cover table. Click on arrow to merge by class.'),
          actionButton('mergeLcv',icon=icon('long-arrow-left'),'merge'),
          hotable("landCoverSqliteTable")
          )
        )
      )
    ),
  fluidRow(
    sidebarPanel(width=3,
      h4('Roads'),
      amProgressBar('roadStackProgress'),
      selectInput('roadSelect','Select road map:',choices=""),
      selectInput('roadSelectClass','Select road class column (integer) :',choices=""),
      selectInput('roadSelectLabel','Select road label column (text) :',choices=""),
      actionButton('btnAddStackRoad','Add to stack')
      ),
    mainPanel(width=9,
      amPanel(width=6,
        h4('Table of road categories.'),
        p(paste('Preview the distinct combination from selected column (max.',15,'rows.). No missing value allowed.')), 
        p(icon('exclamation-triangle'),'If a label or category exists in another stack element, the label and category of the uppermost stack element will be kept as the final raster landcover value after merging process.'),
        hotable('roadPreviewTable')
        ),
      mainPanel(width=6)
      )
    ),
  fluidRow(
    sidebarPanel(width=3,
      h4('Barriers'),
      amProgressBar('barrierProgress'),
      selectInput('barrierSelect','Select barrier map:',choices="",multiple=F),
      radioButtons("barrierType", "Barrier type:",
        c("Areas" = "area",
          "Lines" = "line",
          "Point" = "point"),selected='', inline=TRUE),
      actionButton('btnAddStackBarrier','Add to stack')
      ),
    mainPanel(width=9,
      amPanel(width=6,
        h4('Barrier info'),
        hotable("barrierPreviewTable")
        ),
      mainPanel(width=6)
      )
    )

  )
)
