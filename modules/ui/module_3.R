#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 3: calc accessibility to health facility.
# 
# USER INTERFACE

fluidRow(
  sidebarPanel(width=3,
    h4('Compute accessibility'),
    #
    # select map and table for accessibility
    #
    conditionalPanel(condition="input.moduleSelector!= 'module_5'",
      selectInput('mergedSelect','Select merged land cover map:',choices=""),
      selectInput('modelSelect','Select optional model table:',choices=""),
      selectInput('hfSelect','Select health facilities map:',choices="")
      ),
    #
    # Select health facilities capacity field  
    #
    conditionalPanel(condition="input.moduleSelector=='module_3'",
      selectInput('hfCapacityField','Select facilities capacity numeric field:',choices=""),
      selectInput('hfGroupField',"Select facilities unique ID",choices="")
      ),
    #
    # Select population map map
    #
    conditionalPanel(condition="(
      input.moduleSelector=='module_3' |
      input.moduleSelector=='module_5'
      )",
    selectInput('popSelect','Select population map:',choices="")
    ),
  #
  # Settings anisotropic
  #
  conditionalPanel(condition="
    input.moduleSelector=='module_2' | 
    input.moduleSelector=='module_3' |
    input.moduleSelector=='module_4' 
    ",
    radioButtons('typeAnalysis','Type of analysis',
      c('Isotropic'='isotropic',
        'Anisotropic'='anisotropic'
        ),
      selected='anisotropic',
      inline=TRUE
      ),
    conditionalPanel(
      condition="
      input.typeAnalysis=='anisotropic' & (
        input.moduleSelector=='module_2' | 
        input.moduleSelector=='module_3'
        ) ",
      radioButtons('dirAnalysis','Direction of analysis',
        c(
          "From facilities"="fromHf",
          "Towards facilities"="toHf"),
        selected='toHf',
        inline=TRUE
        )
      )
    ),


  #
  # Module 4 : selection method
  #

  conditionalPanel(condition="input.moduleSelector=='module_4'",
    radioButtons('referalHfSelMeth','Choose referral method',
      c(
        "All to all"="allToAll",
        "From to"="fromTo"
        ),
      selected='fromTo',inline=TRUE
      )
    ),


  #
  # Module 3: sorting parameters
  #
  conditionalPanel(condition="input.moduleSelector=='module_3'",
    radioButtons('hfOrder','Facilities processing order according to:',
      c(
        'Order from health facilities table'='tableOrder',
        'Population living whithin a given travel time from one or group of facilities'='travelTime',
        'Population living in a circular buffer zone around one or group of facilities'='circBuffer'
        )
      ), 
    conditionalPanel( condition="input.hfOrder!='tableOrder'",
      conditionalPanel( condition="input.hfOrder=='circBuffer'",
        numericInput('popBufferRadius','Buffer radius in meters',value=5000)
        ),
      radioButtons('hfOrderSorting','Sorting:',
        c(
          'Ascending'='hfOrderAsc',
          'Descending'='hfOrderDesc'
          ),
        selected='hfOrderAsc',
        inline=TRUE
        )
      )),

  #
  # Module 3 : options
  #
  conditionalPanel(condition="input.moduleSelector=='module_3'",
    checkboxGroupInput('mod3param','Options:',choices=list(
        'Compute map of facilities coverage (vector).'='vectCatch',
        'Remove covered population at each iteration.'='rmPop',
        'Compute map of population cells on barrier.'='popBarrier',
        'Perform zonal analysis of population coverage.'='zonalPop'
        ),selected=c('rmPop','vectCatch','popBarrier'))
    ),
  #
  # Module 3  zonal stat options
  #
  conditionalPanel(condition="
    (input.moduleSelector=='module_3' & input.mod3param.indexOf('zonalPop') != -1)
    ",
    checkboxGroupInput('zonalPopOption','Select zonal options:',choices=c(
        'Compute table of coverage by zone.'='zonalCoverage'
        ## add others options here.
        )
      )
    ),
  #
  # Module 3 and 5 . Choose zonal map
  #
  conditionalPanel(condition="
    (input.moduleSelector=='module_3' & input.zonalPopOption.indexOf('zonalCoverage') != -1) |
    input.moduleSelector=='module_5' 
    ",
    selectInput('zoneSelect','Select zone admin map',choices=''),
    selectInput('zoneId','Select zone unique id',choices=''),
    selectInput('zoneLabel','Select zone label',choices='')
    ),
  #
  # Set maximum walk time
  #
  conditionalPanel(condition="(
    input.moduleSelector=='module_2' | 
    input.moduleSelector=='module_3'
    )",
  numericInput('maxTimeWalk',
    label='Maximum travel time [minutes]',
    value=120,
    min=0,
    max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
    step=1
    )
  ),
#
# Select cumulative cost map
#
conditionalPanel(condition="input.moduleSelector=='module_5'",
  selectInput('cumulativeCostMapSelect',"Select cumulative cost map",choices=""),
  sliderInput('sliderTimeAnalysis',"Select time value",value='',min=0, max=0)
  ),
#
# Tags, validation message and compute button
#
conditionalPanel(condition="input.moduleSelector!='module_5'",
  textInput('costTag','Add tags (minimum 1)',value=''),
  uiOutput('msgModule3'),
  actionButton('btnCreateTimeCostMap','Compute'), 
  amProgressBar('cumulative-progress')
  )
),

#
# Right panel with table / Graphs
#
mainPanel(width=9,
  conditionalPanel(condition="input.moduleSelector!='module_5'",
    fluidRow(
      amPanel(width=6,
        h4('Speed model to be proceeded'),
        conditionalPanel(condition='!input.hideSpeedRasterTable',
          p('Class and label extracted from merged landcover.'),
          p("Edit the columns 'speed' and 'mode' or import from another table."), 
          actionButton('speedTableUndo',icon=icon('undo'),'reset'),
          hotable("speedRasterTable"),
          p(list(strong('Class:'),'merged land cover class')),
          p(list(strong('Label:'),'description of class')),
          p(list(strong('Speed:'), 'speed estimate in [km/h] on flat surface')),
          p(list(strong('Mode'), 'mode of transportation :',textOutput('transpModList')))
          ),
        checkboxInput('hideSpeedRasterTable','Hide table')
        ),
      amPanel(width=6,
        h4('Speed model template'),
        conditionalPanel(condition="!input.hideSpeedSqliteTable",
          p('Categories from table'),
          p('Value from imported from model table. Click on arrow to merge by class.'),
          actionButton('speedTableMerge',icon=icon('long-arrow-left'),'merge'),
          hotable("speedSqliteTable")
          ),
        p(''),
        checkboxInput('hideSpeedSqliteTable','Hide table')
        )
      )
    ),
  conditionalPanel(condition="input.moduleSelector!='module_5'",
    fluidRow(
      amPanel(width=12,
        h4('Health facilities'),  
        h5('Choice and optional processing order (Click on a column header to sort.)'),
        div(class='btn-group',
          actionButton('btnSelectAllHf','Select all',class='btn-inline'),
          actionButton('btnSelecteNoHf','none',class='btn-inline'),
          actionButton('btnSelectRandomHf','10% random',class='btn-inline')
          ),
        hotable('hfTable'),
        hr(),        
        p(tags$b('Accessmod additional columns:'),'amSelect=which rows will be proceeded; amOnBarrier=Facilities is located on a barrier in merged land cover (isotropic and anisotropic analysis will not work); amCatLandCover= Land cover category where the facilitie is located; amPopCell=Population count in the celle where the facilitie is located.'),
        hr()
        )
      )
    ),
  conditionalPanel(condition="input.moduleSelector=='module_5'",
      amPanel(width=12,
    fluidRow(
        column(width=6,
           h4('Preview travel time area'),
        plotOutput('previewTravelTime')
          ),
        column(width=6,
               h4('Potential population coverage under given cumulative cost map'),
        hotable('zoneCoverageTable')
        )
        
        )
      )
    )
  )
)

