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
  column(width=3,
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
          # Select residual pop map
          #
          conditionalPanel(condition="input.moduleSelector=='module_6'",
            #  selectInput('popResSelect',
            #    label="Select uncovered population (raster)",
            #    choices=""),
            selectInput('capTblSelect',
              label="Select a capacity table template (table)",
              choices=""
              )
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
              div(style='margin-left:20px;',
                selectInput('hfIdxField',"Select facility ID field (unique)",choices=""),
                selectInput('hfNameField',"Select facility name field (text)",choices="") 
                )
              ),
            conditionalPanel(condition="input.moduleSelector=='module_4'",
              tags$b(icon('stop'),"To:"),
              selectInput('hfSelectTo','Select existing health facilities layer (vector)',choices=""), 
              div(style='margin-left:20px;',
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
            div(style='margin-left:20px;',
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
          div(style="margin-left:20px",
            selectInput('zoneId','Select zone unique ID (integer)',choices=''),
            selectInput('zoneLabel','Select zone name (text)',choices='')
            )
          ),
        conditionalPanel(condition="(
          input.moduleSelector=='module_5'
          )",
        sliderInput('sliderTimeAnalysis',"Select maximum travel time [minutes]",value=0,min=0, max=0),
        actionButton('btnZoneTravelTime','Update')
        )
      )
    ),
  'ModuleSettings'=list(
    condition="input.moduleSelector!='module_5'",
    title=div(icon('wrench'),'Analysis settings'),
    content=tagList(
      #
      # Settings anisotropic
      #
      conditionalPanel(condition="
        input.moduleSelector=='module_2' | 
        input.moduleSelector=='module_3' |
        input.moduleSelector=='module_4' 
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
            input.moduleSelector=='module_3'
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
      # Module 6 scaling up option
      #
      conditionalPanel(condition="
        input.moduleSelector=='module_6'
        ",
        amAccordionGroup(id='scalingUpSettings',style='margin-left:-10px;margin-right:-10px',show=NULL,itemList=list(
            'startLayer'=list(
              title=div('Initial new facilities layer'),
              content=tagList(
                #
                #  Choice of start layer
                #
                radioButtons('initialFacilityLayer',
                  label=paste(config$newFacilitiesShort,' initial layer'),
                  choices=c('Start with empty layer'='empty','Start using selected facilities'='existing'),
                  selected='empty'
                  )
                )
              ),
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
                    p('Note: if the radius is smaller than one map unit, AccessMod will use the original values.'),
                    numericInput('factorPopSumRadius',
                      label='Set a radius (km)',
                      value=0,
                      min=0,
                      max=5
                      )
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
                    radioButtons('exclusionType',
                      label='Choose exclusion method',
                      c(
                        'Exclude inside'='inside',
                        'Exclude outside'='outside'
                        )
                      ),
                    numericInput('exclusionBuffer',
                      label='Set a buffer (km)',
                      value=0,
                      min=0,
                      max=99
                      ),
                    actionButton('btbAddExclusion',icon=icon('plus-circle'),'Add')
                )
              ),
            'computeLimit'=list(
              title="Optional computation limit",
              content=tagList(
                checkboxInput('sUpComputeLimit',"Set computation limit",value=TRUE),
                conditionalPanel(condition='input.sUpComputeLimit == true',
                  div(style='margin-left:20px;',
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
              ),
            'generalOption'=list(
              title="Other settings",
              content=tagList(
                checkboxInput('rmPopPotential',
                  label='Remove potential population coverage at each iteration',
                  value=TRUE
                  )
                )
              )
            )
          )
        ),
        #
        # Set maximum walk time
        #
        conditionalPanel(condition="(
          input.moduleSelector=='module_2' | 
          input.moduleSelector=='module_3' |
          input.moduleSelector=='module_6'
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
    column(width=9, style="height:2000px; overflow-y:scroll;",

      conditionalPanel(condition="input.moduleSelector!='module_5'",

        conditionalPanel(condition="input.moduleSelector=='module_6'",
          fluidRow(
            h2('Scaling up'),
            column(width=12,
              h3('Capacity table for new facilities creation'),
              column(width=7,
                hotable("capacityTable")
                ),
              column(width=3,
                div(class='btn-group-vertical',
                  actionButton(class="btn-txt-left",'btnAddRowCapacity',icon=icon("plus-circle"),'Add row'),
                  actionButton(class="btn-txt-left",'btnRmRowCapacity',icon=icon("minus-circle"),'Remove row')
                  )
                )
              ),
            column(width=12,
              h3('Suitability factors'),
              column(width=7,
                hotable("suitabilityTable")
                ),
              column(width=3,
                div(class='btn-group-vertical',
                  actionButton(class="btn-txt-left","btnResetSuitTable",icon=icon('undo'),"Reset"),
                  actionButton(class="btn-txt-left","btnRmSuitTableSelect",icon=icon('minus-circle'),"Remove selected")
                  )
                )
              ),
            column(width=12,
              h3('Exclusion areas'),
              column(width=7,
                hotable("exclusionTable")
                ),
              column(width=3,
                div(class='btn-group-vertical',
                  actionButton(class="btn-txt-left","btnResetExcluTable",icon=icon('undo'),"Reset"),
                  actionButton(class="btn-txt-left","btnRmExcluSelect",icon=icon('minus-circle'),"Remove selected")
                  )
                )
              )
            )
          ),
        fluidRow(
          h2('Travel scenario'),
          column(width=12,
              h3('Existing scenario table'),
            column(width=7,
              hotable("speedSqliteTable")
              ),
            column(width=3,
              uiOutput('speedTableMergeValidation')
              )
            ),
          column(width=12,
              h3('Travel scenario to be processed'),
            column(width=7,
              hotable("speedRasterTable")
              ),
            column(width=3,
              tags$div(class='btn-group-vertical',
                actionButton(class='btn-txt-left','speedTableMerge',icon=icon('magic'),'Complete with existing scenario'),
                actionButton(class='btn-txt-left','speedTableUndo',icon=icon('undo'),'Reset to original value')
                )
              )
            )
          )
        ),





      conditionalPanel(condition="input.moduleSelector!='module_5'",
        amAccordionGroup(id='accessibilityTable',show=c(1,2,3),itemList=list(
            'scalingUpTable'=list(
              condition="input.moduleSelector=='module_6'",
              title="Scaling up tables",
              content=hr()              ),
            'modelTable'=list(
              title='Travel scenario',
              content=hr()              ),
            'hfTables'=list(
              #condition="input.moduleSelector!='module_6'",
              title='Facilities selection',
              content=fluidRow(
                column(width=12,
                  fluidRow(
                    column(12,
                      p(tags$label('Filter facilities')),
                      div(class='btn-group',
                        actionButton('btnSelectAllHf','All',class='btn-inline'),
                        actionButton('btnSelecteNoHf','none',class='btn-inline'),
                        actionButton('btnSelectHfFromRule','apply rules',class='btn-inline')
                        ),
                      conditionalPanel(condition="input.showDevelTools==true",
                        actionButton('btnSelectRandomHf','random (10%)')),
                      checkboxInput('hfDisplayRules','Display rules selection panel',value=F)
                      ),
                    column(3,
                      conditionalPanel(condition="input.moduleSelector=='module_4'",
                        radioButtons('selHfFromTo','Target table',choice=c('From','To'),inline=T)
                        )),
                    column(6,p()
                      )
                    ),
                  hr(),
                  fluidRow(
                    conditionalPanel(condition='input.hfDisplayRules==true',
                      tagList(
                        sidebarPanel(width=5,
                          tagList(
                            h4('Add new rules'),
                            selectInput('hfFilterField','Select field',choices="",selected=""),
                            selectInput('hfFilterOperator','Operator',choices="",selected=""),
                            selectInput('hfFilterVal','Select values',choices="",selected="",multiple=T),
                            actionButton('btnAddHfRule','',icon=icon('plus'))
                            )
                          )
                        ),
                      amPanel(width=7,
                        h4('Rules to apply'),
                        hotable('hfTableRules')
                        )
                      )
                    ),
                  p(tags$label('Selected facilities')),
                  conditionalPanel(condition="input.moduleSelector=='module_4'",
                    tagList(
                      tags$b('From')
                      )
                    ),
                  hotable('hfTable'),
                  conditionalPanel(condition="input.moduleSelector=='module_4'",
                    tags$b('To'),
                    hotable('hfTableTo')
                    )
                  )  
                )
              )
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
              h4('Zonal statistics'),
              p('Population coverage for the accessibility analysis.'),
              hotable('zoneCoverageTable')
              )

            )
          )
        )
      )
    )

