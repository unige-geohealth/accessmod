#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 3: calc accessibility to health facility.
# 
# USER INTERFACE
# TODO: break into small parts.

fluidRow(
  column(width=4,
    wellPanel(
      amCenterTitle(div(icon('sign-in'),'Data input'),sub="Data selection for the current module."),
      conditionalPanel(condition="(
        input.moduleSelector=='module_3' |
        input.moduleSelector=='module_5' |
        input.moduleSelector=='module_6'
        )",
      selectInput('popSelect','Select population layer (raster)',choices="")
      )
    ),
    amAccordionGroup(id='accessibilitySettings',show=1,itemList=list(
        'inputSettings'=list(
          title=div(icon('sign-in'),'Data input'),
          content=tagList(

            #
            # Select population layer
            #
            conditionalPanel(condition="(
              input.moduleSelector=='module_3' |
              input.moduleSelector=='module_5' |
              input.moduleSelector=='module_6'
              )",
            selectInput('popSelect','Select population layer (raster)',choices="")
            ),
          #
          # select merged landcover and model table
          #
          conditionalPanel(condition="
            input.moduleSelector != 'module_5'
            ",
            selectInput("mergedSelect",'Select merged land cover layer (raster)',choices=""),
            selectInput("modelSelect",'Select scenario table (table)',choices=""),
            conditionalPanel(condition="input.moduleSelector== 'module_4'",
              tags$b(icon('play'),"From:")
              ),
            #
            # select facility tmap and columns
            #
            selectInput('hfSelect','Select existing health facilities layer (vector)',choices=""),
            conditionalPanel(condition="
              input.moduleSelector=='module_3' |
              input.moduleSelector=='module_4'
              ",
              div(style='margin-left:10%;',
                selectInput('hfIdxField',"Select facility ID field (unique)",choices=""),
                selectInput('hfNameField',"Select facility name field (text)",choices="") 
                )
              ),
            conditionalPanel(condition="input.moduleSelector=='module_4'",
              tags$b(icon('stop'),"To:"),
              selectInput('hfSelectTo','Select existing health facilities layer (vector)',choices=""), 
              div(style='margin-left:10%;',
                selectInput('hfIdxFieldTo',"Select facility ID field (unique)",choices=""),
                selectInput('hfNameFieldTo',"Select facility name field (text)",choices="") 
                )
              )
            #   )
            ),
          #
          # Select health facilities capacity field  
          #
          conditionalPanel(condition="input.moduleSelector=='module_3'",
            div(style='margin-left:10%;',
              selectInput('hfCapacityField','Select facilities coverage capacity (numeric):',choices="")
              )
            ),

          #
          # Select cumulative cost map
          #
          conditionalPanel(condition="(
            input.moduleSelector=='module_5'
            //input.moduleSelector=='module_6'
            )",
          selectInput('cumulativeCostMapSelect',"Select travel time layer (raster)",choices="")
          ),
        #
        # Module 3 and 5 . Choose zonal map
        #
        conditionalPanel(condition="
          (input.moduleSelector=='module_3' & 
            //input.zonalPopOption.indexOf('zonalCoverage') != -1 &
            input.mod3param.indexOf('zonalPop') != -1
            ) |
          input.moduleSelector=='module_5' 
          ",
          selectInput('zoneSelect','Select zones layer (vector)',choices=''),
          div(style="margin-left:10%;",
            selectInput('zoneId','Select zone unique ID (integer)',choices=''),
            selectInput('zoneLabel','Select zone name (text)',choices='')
            )
          ),
        conditionalPanel(condition="(
          input.moduleSelector=='module_5'
          )",
        sliderInput('sliderTimeAnalysis',"Select maximum travel time [minutes]",value=0,min=0, max=0),
        actionButton('btnZoneTravelTime','Update')
        ),
      conditionalPanel(condition="(
        input.moduleSelector=='module_6'
        )",
      #
      # select external capacity table 
      #
      selectInput('capTblSelect',"Select external capacity table",choices=""),
      selectInput('exclusionTblSelect',"Select external exclusion table",choices=""),
      selectInput('suitabilityTblSelect',"Select external suitability table",choices="") 
      )
      )
    ),
  'ModuleSettings'=list(
    condition="input.moduleSelector!='module_5'",
    title=div(icon('wrench'),'Analysis settings'),
    content=tagList(
      #
      # Module 6 scaling up option
      #
      conditionalPanel(condition="
        input.moduleSelector=='module_6'
        ",
        amAccordionGroup(id='scalingUpSettings',style='margin-left:-5%;margin-right:-5%',show=NULL,itemList=list(
            'Suitability index'=list(
              title=div('Suitability index'),
              content=tagList(
                #
                # Choice of factor for the suitability index
                #
                selectInput('selFactor',"Select factor for the suitability index",choices=c(
                    'Sum of population within a radius'='popsum',
                    'Euclidean distance from features'='dist',
                    'Travel time from/to feature'='traveltime',
                    'Generic priority map'='priority')
                  ),
                
                conditionalPanel(condition="input.selFactor == 'popsum'",
                  numericInput('factorPopSumRadius',
                    label='Set a radius (km)',
                    value=0,
                    min=0,
                    max=5
                    ),
                  p('Note: if the radius is smaller than one map unit, AccessMod will use the original values.')
                  ),
                conditionalPanel(condition="input.selFactor == 'traveltime'",
                  radioButtons('factorTypeAnalysis','Type of analysis',
                    c('Isotropic (ignore DEM)'='iso',
                      'Anisotropic (use DEM)'='aniso'
                      ),
                    selected='iso',
                    inline=FALSE
                    ),
                  conditionalPanel(condition="input.factorTypeAnalysis=='aniso'",
                    radioButtons('factorTravelDirection',
                      label='Direction of travel',
                      choices=c(
                        "From feature"="from",
                        "Towards feature"="to"),
                      selected='to',
                      inline=FALSE
                      )
                    )
                  ),
                radioButtons('factorDirection',
                  label='Direction of prioritization',
                  choices=c(
                    'Higher values are more suitable'='hvms',
                    'Higher values are less suitable'='hlms'
                    ),
                  selected='hvms'
                  ),
                numericInput('factorWeight',
                  label="Select factor weight",
                  min=0,
                  max=10,
                  value=1
                  ),
                selectInput('selFactorLayer','Select available layer',choices=""),
                actionButton('btnAddFactor',icon=icon('plus-circle'),"Add")
                )
              ),
            'exclusionAreas'=list(
              title=div('Exclusion areas'),
              content=tagList(
                #
                #  Choice of exclusion area 
                #
                selectInput('selExclusion','Select exclusion areas (vector or raster)',choices=""),
                numericInput('exclusionBuffer',
                  label='Set a buffer (km)',
                  value=0,
                  min=0,
                  max=99
                  ),
                radioButtons('exclusionMethod',
                  label='Choose exclusion method',
                  c(
                    'Exclude inside'='inside',
                    'Exclude outside'='outside'
                    )
                  ),
                actionButton('btnAddExclusion',icon=icon('plus-circle'),'Add')
                )
              ),
            'computeLimit'=list(
              title="Optional computation limit",
              content=tagList(
                checkboxInput('sUpComputeLimit',"Set computation limit",value=TRUE),
                conditionalPanel(condition='input.sUpComputeLimit == true',
                  div(style='margin-left:10%;',
                    numericInput('newHfNumber',
                      label='Number of new health facilities to locate',
                      value=10,
                      min=1,
                      max=500,
                      step=1
                      ),
                    numericInput('maxProcessingTime',
                      label='Set maximum processing time [minutes]',
                      value=10,
                      min=1,
                      max=400
                      )
                    )
                  )
                )
              )
            )
          ),
        #
        #  Choice of start layer
        #
        radioButtons('initialFacilityLayer',
          label=paste(config$newFacilitiesShort,' initial layer'),
          choices=c('Start with empty layer'='empty','Start using selected facilities'='existing'),
          selected='empty'
          ),
        checkboxInput('rmPopPotential',
          label='Remove potential population coverage at each iteration',
          value=TRUE
          )
        ),
      #
      # Settings anisotropic
      #
      conditionalPanel(condition="
        input.moduleSelector=='module_2' | 
        input.moduleSelector=='module_3' |
        input.moduleSelector=='module_4' |
        input.moduleSelector=='module_6'
        ",
        radioButtons('typeAnalysis','Type of analysis',
          c('Isotropic (ignore DEM)'='isotropic',
            'Anisotropic (use DEM)'='anisotropic'
            ),
          selected='anisotropic',
          inline=FALSE
          ),
        conditionalPanel(
          condition="
          input.typeAnalysis=='anisotropic' & (
            input.moduleSelector=='module_2' | 
            input.moduleSelector=='module_3' |
            input.moduleSelector=='module_6'
            ) ",
          radioButtons('dirAnalysis','Direction of travel',
            c(
              "From facilities"="fromHf",
              "Towards facilities"="toHf"),
            selected='toHf',
            inline=FALSE
            )
          )
        ),
      #
      # Module 3: sorting parameters
      #
      conditionalPanel(condition="input.moduleSelector=='module_3'",
        radioButtons('hfOrder','Facilities processing order according to:',
          c(
            'A facilities column'='tableOrder',
            'The population living whithin a given travel time from the facilities'='travelTime',
            'The population living in a circular buffer zone around the facilities'='circBuffer'
            )
          ), 
        #  conditionalPanel( condition="input.hfOrder!='tableOrder'",
        conditionalPanel(condition="input.hfOrder=='tableOrder'",
          selectInput('hfOrderColumn','Select column',choices="")
          ),
        conditionalPanel( condition="input.hfOrder=='circBuffer'",
          numericInput('popBufferRadius','Buffer radius [meters] ',value=5000)
          ),
        conditionalPanel(condition="input.hfOrder=='travelTime'",
          numericInput('maxTravelTimeProcOrder',
            label='Max travel time [minutes]',
            value=120,
            min=0,
            max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
            step=1
            )
          ),
        radioButtons('hfOrderSorting','Processing order:',
          c(
            'Ascending'='hfOrderAsc',
            'Descending'='hfOrderDesc'
            ),
          selected='hfOrderDesc',
          inline=FALSE
          )
        #  )
        ),

      #
      # Module 3 : options
      #
      conditionalPanel(condition="input.moduleSelector=='module_3'",
        checkboxGroupInput('mod3param','Options:',choices=list(
            'Compute catchment area layer.'='vectCatch',
            'Remove covered population.'='rmPop',
            #Steeve recommends popBarrier by default. NOTE: the line bellow was commented, but popBarrier was a selected option ! Uncommented.
            'Compute map of population cells on barrier.'='popBarrier', 
            'Generate zonal statistics (select zones layer in data input panel).'='zonalPop'
            ),selected=c('rmPop','vectCatch','popBarrier'))
        ), 
      #
      # Set maximum walk time
      #
      conditionalPanel(condition="(
        input.moduleSelector=='module_2' | 
        input.moduleSelector=='module_3' 
        )",
      numericInput('maxTravelTime',
        label='Maximum travel time [minutes]',
        value=120,
        min=0,
        max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
        step=1
        )
      )
    )
  )
)
),
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        amAccordionGroup(id='accessibilityValidation',show=c(1),itemList=list(
            'validation'=list(
              #
              # Tags, validation message and compute button
              #

              title=div(icon('check-square-o'),'Validation and computation'),
              content=tagList(
                textInput('costTag','Add tags (minimum 1)',value=''),
                uiOutput('msgModule3'),
                uiOutput('msgModule3outData'),
                actionButton('btnComputeAccessibility','Compute'), 
                amProgressBar('cumulative-progress')
                )
              )
            )
          )
        )
      ),

    #
    # Right panel with table / Graphs
    #
    #column(id="accessibilityRightPanel",width=8,class="panel panel-default",style="height:1000px; overflow-y:scroll;",
    column(id="accessibilityRightPanel",width=7,
      conditionalPanel(condition="input.moduleSelector!='module_5'",

        conditionalPanel(condition="input.moduleSelector=='module_6'",
          fluidRow(
            amCenterTitle('Scaling up',sub="Configuration tables for the scaling up algorithm."),
            column(width=12,
              h4('Capacity table for new facilities creation'),
                div(style="margin-left:50px;",
                actionLink('btnAddRowCapacity',icon=icon("plus-circle"),'Add row'),
                actionLink('btnRmRowCapacity',icon=icon("minus-circle"),'Remove row'),
                hotable("capacityTable")
                )
              ),
            column(width=12,
              p(""),
              h4('Suitability factors'),
                div(style="margin-left:50px;",
                  actionLink("btnResetSuitTable",icon=icon('undo'),"Reset"),
                  actionLink("btnRmSuitTableSelect",icon=icon('minus-circle'),"Remove selected"),
                  hotable("suitabilityTable")
                  )
              ),
            column(width=12,
              p(""),
              h4('Exclusion areas'),
                div(style="margin-left:50px;",
                  actionLink("btnResetExcluTable",icon=icon('undo'),"Reset"),
                  actionLink("btnRmExcluSelect",icon=icon('minus-circle'),"Remove selected"),
                  hotable("exclusionTable")
                  )
              )
            )
          ),
        fluidRow(
          amCenterTitle('Travel scenario',sub='Define speed of travel by landcover classes.'),
          column(width=12,
            h4('Travel scenario to be processed'),
            div(style="margin-left:50px;",
              actionLink('speedTableUndo',icon=icon('undo'),'Reset to original value'),
              hotable("speedRasterTable")
              )
            ),
          column(width=12,
            h4('Existing scenario table'),
            div(style="margin-left:50px;",
              actionLink('speedTableMerge',icon=icon('magic'),'Update scenario with the values of this table'),
              uiOutput('speedTableMergeValidation'),
              hotable("speedSqliteTable")
            )
          )
        )
        ),
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        fluidRow(
          column(width=12,
            amCenterTitle('Facilities selection',sub="Filter and select facilities by attributes."),
            fluidRow(
              column(12,
                h4('Filter and select facilities'),
                div(class='btn-group',
                  actionButton('btnSelectAllHf','All',class='btn-inline'),
                  actionButton('btnSelecteNoHf','none',class='btn-inline'),
                  actionButton('btnSelectHfFromRule','apply rules',class='btn-inline')
                  ),
                conditionalPanel(condition="input.showDevelTools==true",
                  actionButton('btnSelectRandomHf','random (10%)'))
                #checkboxInput('hfDisplayRules','Display rules selection panel',value=F)
                ),
              column(3,
                conditionalPanel(condition="input.moduleSelector=='module_4'",
                  radioButtons('selHfFromTo','Target table',choice=c('From','To'),inline=T)
                  )),
              column(6,p()
                )
              ),
            fluidRow(
              column(width=12,
                # NOTE: hfDisplayRules is used in amServer to trigger hfFilter* fields.
                checkboxInput('hfDisplayRules','Display rules panel',value=FALSE), 
                conditionalPanel(condition="input.hfDisplayRules == true",
                  tagList(
                    sidebarPanel(width=5,
                      tagList(
                        h4('Add new rules'),
                        selectInput('hfFilterField','Select field',choices="",selected=""),
                        selectInput('hfFilterOperator','Operator',choices="",selected=""),
                        selectInput('hfFilterVal','Select values',choices="",selected="",multiple=T),
                        actionButton('btnAddHfRule','',icon=icon('plus'))
                        )
                      ),
                    column(width=7,
                      h4('Rules to apply'),
                      hotable('hfTableRules')
                      )
                    )
                  )
                )
              ),
            h4('Selected facilities'),
            conditionalPanel(condition="input.moduleSelector=='module_4'",
              tagList(
                tags$b('From')
                )
              ),
            hotable('hfTable',height="300px"),
            conditionalPanel(condition="input.moduleSelector=='module_4'",
              tags$b('To'),
              hotable('hfTableTo',height="300px")
              )
            )  
          ) 
        ),
      conditionalPanel(condition="input.moduleSelector=='module_5'",
        column(width=12,
          fluidRow(
            column(width=6,
              h4('Preview travel time area'),
              plotOutput('previewTravelTime')
              ),
            column(width=6,
              h4('Zonal statistics'),
              p('Population coverage for the accessibility analysis.'),
              hotable('zoneCoverageTable')
              )

            )
          )
        )

      )
    )

#
#
#      conditionalPanel(condition="input.moduleSelector!='module_5'",
#        amAccordionGroup(id='accessibilityTable',show=c(1,2,3),itemList=list(
#            'scalingUpTable'=list(
#              condition="input.moduleSelector=='module_6'",
#              title="Scaling up tables",
#              content=hr()              ),
#            'modelTable'=list(
#              title='Travel scenario',
#              content=hr()              ),
#            'hfTables'=list(
#              #condition="input.moduleSelector!='module_6'",
#              title='Facilities selection',
#              content=#              )
#            )
#          )
#        ),
#            )
   # )

