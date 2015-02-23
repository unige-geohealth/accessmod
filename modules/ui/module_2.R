#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 2: calc accessibility to health facility.
# 
# USER INTERFACE


fluidRow(
  sidebarPanel(width=3,
    h4('Compute accessibility to health facilities test'),
    amProgressBar('cumulative-progress'),
    p('Create an anistropic cummulative cost map.'),
    selectInput('mergedSelect','Select merged land cover map:',choices=""),
    selectInput('hfSelect','Select health facilities map:',choices=""),
    selectInput('modelSelect','Select optional model table:',choices=""),
    radioButtons('typeAnalysis','Type of analalysis',
      c('Isotropic'='isotropic',
        'Anisotropic'='anistropic'
        ),
      selected='anistropic',
      inline=TRUE
      ),
    conditionalPanel(
      condition="input.typeAnalysis=='anistropic'",
      radioButtons('dirAnalysis','Direction of analysis',
        c("From facilities"="fromHf",
          "Towards facilities"="toHf"),
        selected='toHf',
        inline=TRUE
        )
      ),
   # radioButtons("colTable","Use a color table for cummulative map",
   #   c( "White-blue-black" = "blue",
   #     "Yellow-green-blue-red-black" = "slope",
   #     "none" = "none"
   #     ),selected="slope"),
    numericInput('maxTimeWalk',
      label='Maximum transportation time [minutes]',
      value=120,
      min=0,
      max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
      step=1
      ),
    textInput('costTag','Add tags (minimum 1)',value=''),
    actionButton('btnCreateTimeCostMap','Compute speed map and cumulative cost map') 
    ),
  mainPanel(width=9,
    tabsetPanel(
      tabPanel("Speed model",
        h4('Table of speed by category and mode of transportation.'),
        fluidRow(
          amPanel(width=6,
            h5('Categories from raster'),
            p("Edit the columns 'speed' and 'mode' or copy and paste from spreadsheet."), 
            actionButton('speedTableUndo',icon=icon('undo'),'reset'),
            hotable("speedRasterTable")
            ),
          amPanel(width=6,
            h5('Categories from table'),
            p('Value from imported from model table. Click on arrow to merge by class.'),
            actionButton('speedTableMerge',icon=icon('long-arrow-left'),'merge'),
            hotable("speedSqliteTable")
            )
          ),
        p(list(strong('Class:'),'merged land cover class')),
        p(list(strong('Label:'),'description of class')),
        p(list(strong('Speed:'), 'speed estimate in [km/h] on flat surface')),
        p(list(strong('Mode'), 'mode of transportation :',textOutput('transpModList')))
        ),
      tabPanel("Health facilities",
        h4('Health facilities'),
        hotable('hfTableModule2')
        )
      )

    )
  )

