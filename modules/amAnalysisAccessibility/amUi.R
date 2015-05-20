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
          title=div(icon('sign-in'),'Select inputs'),
          content=tagList(
            #
            # select merged landcover and model table
            #
            conditionalPanel(condition="
              input.moduleSelector != 'module_5'
              ",
              selectInput("mergedSelect",'Select merged land cover map:',choices=""),
              selectInput("modelSelect",'Select model (template):',choices=""),
              conditionalPanel(condition="input.moduleSelector== 'module_4'",
                tags$b("From:")
                ),
              #
              # select facility tmap and columns
              #
              #conditionalPanel(condition="
              #  //input.moduleSelector != 'module_6'
              #  ",
              selectInput('hfSelect','Select existing health facilities map:',choices=""),
              conditionalPanel(condition="
                input.moduleSelector=='module_3' |
                input.moduleSelector=='module_4'
                ",
                selectInput('hfIdxField',"Select facilities index",choices=""),
                selectInput('hfNameField',"Select facilities label",choices="") 
                ),
              conditionalPanel(condition="input.moduleSelector=='module_4'",
                tags$b("To:"),
                selectInput('hfSelectTo','Select existing health facilities map:',choices=""),
                selectInput('hfIdxFieldTo',"Select facilities index",choices=""),
                selectInput('hfNameFieldTo',"Select facilities label",choices="") 
                )
              #   )
              ),
            #
            # Select health facilities capacity field  
            #
            conditionalPanel(condition="input.moduleSelector=='module_3'",
              selectInput('hfCapacityField','Select facilities capacity numeric field:',choices="")
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
          # Select residual pop map
          #
          conditionalPanel(condition="input.moduleSelector=='module_6'",
            selectInput('popResSelect',
              label="Select uncovered population",
              choices=""),
            selectInput('capTblSelect',
              label="Select a capacity table template",
              choices=""
              )
            ),
          #
          # Select cumulative cost map
          #
          conditionalPanel(condition="(
            input.moduleSelector=='module_5'
            //input.moduleSelector=='module_6'
            )",
          selectInput('cumulativeCostMapSelect',"Select cumulative cost map",choices="")
          ),
        conditionalPanel(condition="(
          input.moduleSelector=='module_5'
          )",
        sliderInput('sliderTimeAnalysis',"Select time value [minutes]",value=0,min=0, max=0)
        )
      )
    ),
  'ModuleSettings'=list(
    title=div(icon('wrench'),'Module settings'),
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
      # Module 3: sorting parameters
      #
      conditionalPanel(condition="input.moduleSelector=='module_3'",
        radioButtons('hfOrder','Facilities processing order according to:',
          c(
            'Health facilities table'='tableOrder',
            'Population living whithin a given travel time from facilities'='travelTime',
            'Population living in a circular buffer zone surrounding facilities'='circBuffer'
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
            'Compute catchment area layer.'='vectCatch',
            'Remove covered population.'='rmPop',
            #'Compute map of population cells on barrier.'='popBarrier', Steeve recommends popBarrier by default.
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
        (input.moduleSelector=='module_3' & 
          input.zonalPopOption.indexOf('zonalCoverage') != -1 &
          input.mod3param.indexOf('zonalPop') != -1
          ) |
        input.moduleSelector=='module_5' 
        ",
        selectInput('zoneSelect','Select zone admin map',choices=''),
        selectInput('zoneId','Select zone id (integer)',choices=''),
        selectInput('zoneLabel','Select zone label',choices='')
        ),
      #
      # Set maximum walk time
      #
      conditionalPanel(condition="(
        input.moduleSelector=='module_2' | 
        input.moduleSelector=='module_3' |
        input.moduleSelector=='module_6'
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
    # Module 6 scaling up option
    #
    conditionalPanel(condition="
      input.moduleSelector=='module_6'
      ",
      #   numericInput('sampleNumber',
      #     label='Number of sampling points',
      #     value=500,
      #     min=100,
      #     max=1000,
      #     step=1
      #     ),
      selectInput('excludeLandCoverClass',
        label='Skip location where land cover class are',
        choices='',
        multiple=TRUE
        ),
      numericInput('newHfNumber',
        label='Number of new facilities',
        value=10,
        min=1,
        max=500,
        step=1
        ),

      numericInput('minTravelTime',
        label='Skip location where travel time to other facilities is less than [minutes] ',
        value=20,
        min=0,
        max=1e6,
        step=1
        ),
      numericInput('maxProcessingTime',
        label='Set maximum processing time [minutes]',
        value=10,
        min=1,
        max=400
        ),
      checkboxInput('rmPopPotential',
        label='Remove potential population coverage at each iteration',
        value=TRUE
        )
      # numericInput('subSamplingFactor',
      #   label='Grid subsampling factor',
      #   min=10,
      #   max=100,
      #   value=10
      #   )
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

              title=div(icon('check-square-o'),'Validation'),
              content=tagList(
                textInput('costTag','Add tags (minimum 1)',value=''),
                uiOutput('msgModule3'),
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
    mainPanel(width=9,
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        amAccordionGroup(id='accessibilityTable',show=c(1,2,3),itemList=list(
            'modelTable'=list(
              title='Model tables',
              content=fluidRow(
                amPanel(width=6,
                  h4('Speed model to be processed'),
                  actionButton('speedTableUndo',icon=icon('undo'),'reset'),
                  hotable("speedRasterTable")
                  ),
                amPanel(width=6,
                  h4('Speed model template'),
                  actionButton('speedTableMerge',icon=icon('long-arrow-left'),'merge'),
                  hotable("speedSqliteTable")
                  )
                )
              ),
            'hfCapacityTable'=list(
              condition="input.moduleSelector=='module_6'",
              title="Capacity table",
              content=fluidRow(
                amPanel(width=6,
                  h4('Capacity attributes for new facility creation'),
                  hotable("capacityTable")
                  ),
              column(width=6,"")
                )

              ),
            'hfTables'=list(
              #condition="input.moduleSelector!='module_6'",
              title='Facilities tables',
              content=fluidRow(
                amPanel(width=12,
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
                  conditionalPanel(condition="input.moduleSelector=='module_4'",
                    tags$b('From facilities')
                    ),
                  hotable('hfTable'),
                  conditionalPanel(condition="input.moduleSelector=='module_4'",
                    tags$b('To facilities'),
                    hotable('hfTableTo')
                    )
                  )  
                )


              )
            ))
        ),
      conditionalPanel(condition="input.moduleSelector=='module_5'",
        amPanel(width=12,
          fluidRow(
            column(width=6,
              h4('Preview travel time area'),
              plotOutput('previewTravelTime')
              ),
            column(width=6,
              h4('Potential population coverage'),
              p('All facilities aggregated, without capacity analysis.'),
              hotable('zoneCoverageTable')
              )

            )
          )
        )
      )
    )

