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
              #conditionalPanel(condition="
              #  //input.moduleSelector != 'module_6'
              #  ",
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
            #'Compute map of population cells on barrier.'='popBarrier', Steeve recommends popBarrier by default.
            'Generate zonal statistics (select zones layer in data input panel).'='zonalPop'
            ),selected=c('rmPop','vectCatch','popBarrier'))
        ),
      #
      # Module 3  zonal stat options
      #
      #   conditionalPanel(condition="
      #     (input.moduleSelector=='module_3' & input.mod3param.indexOf('zonalPop') != -1)
      #     ",
      #     checkboxGroupInput('zonalPopOption','Select zonal options:',choices=c(
      #         'Compute table of coverage by zone.'='zonalCoverage'
      #         ## add others options here.
      #         )
      #       )
      #     ),
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
    mainPanel(width=9,
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        amAccordionGroup(id='accessibilityTable',show=c(1,2,3),itemList=list(
            'modelTable'=list(
              title='Travel scenario',
              content=fluidRow(
                amPanel(width=6,
                  h4('Travel scenario to be processed'),
                  hotable("speedRasterTable"),
                  hr(),
                  p('Table operations:'),
                  tags$div(class='btn-group',style='width:100%',
                    actionButton(class='btn-inline',style='width:50%','speedTableUndo',icon=icon('undo'),'Reset to original value'),
                    actionButton(class='btn-inline',style='width:50%','speedTableMerge',icon=icon('magic'),'Complete with existing scenario')
                    )

                  ),
                amPanel(width=6,
                  h4('Existing scenario table'),
                  hotable("speedSqliteTable")
                  )
                )
              ),
            'hfCapacityTable'=list(
              condition="input.moduleSelector=='module_6'",
              title="Capacity table",
              content=fluidRow(
                amPanel(width=6,
                  h4('Capacity table for new facilities creation'),
                  hotable("capacityTable"),
                  hr(),
                  tags$div(class='btn-group',style="min-width:100%",
                    actionButton(class="btn-inline",style="width:50%",'btnAddRowCapacity',icon=icon("plus-circle"),'Add row'),
                    actionButton(class="btn-inline",style="width:50%",'btnRmRowCapacity',icon=icon("minus-circle"),'Remove row')
                    )

                  ),
                column(width=6,"")
                )

              ),
            'hfTables'=list(
              #condition="input.moduleSelector!='module_6'",
              title='Facilities selection',
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
              h4('Zonal statistics'),
              p('Population coverage for the accessibility analysis.'),
              hotable('zoneCoverageTable')
              )

            )
          )
        )
      )
    )

