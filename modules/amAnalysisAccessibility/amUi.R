#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Accessibility modules ui.
# 
# USER INTERFACE
# TODO: break into small parts.

fluidRow(
  column(width=4,
    wellPanel(
      amCenterTitle(div(icon('sign-in'),'Data input'),h=3,m=0,sub="Select data to be used in this analysis"),
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
           conditionalPanel(condition="(
             input.moduleSelector=='module_6'
             )",
           selectInput('popResidualSelect','Select residual population layer (raster)',choices="")
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
              input.moduleSelector=='module_4' |
              input.moduleSelector=='module_6'
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
          conditionalPanel(condition="(
            input.moduleSelector=='module_6' |
            input.moduleSelector=='module_3'
            )",
            div(style='margin-left:10%;',
              selectInput('hfCapacityField','Select facilities coverage capacity field (numeric):',choices="")
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
      selectInput('capTableSelect',"Select external capacity table",choices=""),
      selectInput('exclusionTableSelect',"Select external exclusion table",choices=""),
      selectInput('suitabilityTableSelect',"Select external suitability table",choices="") 
      )),
      conditionalPanel(condition="input.moduleSelector!='module_5'",
wellPanel(
      amCenterTitle(div(icon('wrench'),'Module settings'),h=3,m=0,sub="Configure the parameters for this analysis."),
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
                selectInput('selFactor',"Select a factor for the suitability index",choices=c(
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
                    'Higher values are less suitable'='hvls'
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
                checkboxInput("showExclusionOptions","Show options",value=FALSE),
                conditionalPanel("input.showExclusionOptions == true",
                numericInput('exclusionBuffer',
                  label='Set an optional buffer (km)',
                  value=0,
                  min=0,
                  max=99
                  ),
                radioButtons('exclusionMethod',
                  label='Choose exclusion method',
                  c(
                    'Keep candidates outside buffer' = 'keepOutside',
                    'Keep candidates inside buffer' = 'keepInside'
                    )
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
            'A field in the health facility layer'='tableOrder',
            'The population living within a given travel time from the facilities'='travelTime',
            'The population living within a circular buffer around the facilities'='circBuffer'
            )
          ), 
        #  conditionalPanel( condition="input.hfOrder!='tableOrder'",
        conditionalPanel(condition="input.hfOrder=='tableOrder'",
          selectInput('hfOrderColumn','Select field from the facility layer',choices="")
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
        max=40*24*60,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1. Set to max 40 day.
        step=1
        )
      ),
    #
    # Module 3 and 6 more options
    #
    conditionalPanel(condition="(
      input.moduleSelector=='module_3'|
      input.moduleSelector=='modue_6'
      )",
      checkboxInput("moreOptions","Show options"),
      conditionalPanel("input.moreOptions==true",
      checkboxGroupInput('mod3param','Options:',choices=list(
          'Compute catchment area layer.'='vectCatch',
          'Remove covered population.'='rmPop',
          #Steeve recommends popBarrier by default. NOTE: the line bellow was commented, but popBarrier was a selected option ! Uncommented.
          'Compute map of population cells on barrier.'='popBarrier', 
          'Generate zonal statistics (select zones layer in data input panel).'='zonalPop'
          ),selected=c('rmPop','vectCatch','popBarrier'))
      )
      )
    )
      ),
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        wellPanel(
          amCenterTitle(div(icon('check-square-o'),'Validation'),m=0,h=3,sub=
            "Add short tags, review validation issues and compute."
            ),
          textInput('costTag','Add short tags',value=''),
          uiOutput('msgModule3'),
          uiOutput('msgModule3outData'),
          actionButton('btnComputeAccessibility','Compute'), 
          amProgressBar('cumulative-progress')
          )
        )
      ),
    #
    # Right panel with table / Graphs
    #
    column(id="accessibilityRightPanel",width=7,
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        #
        # Scenario tables
        #
        fluidRow(
          amCenterTitle('Travel scenario',sub='Define the speed of travel for each land cover class.'),
            fluidRow(class="amRowTable",
            h4('Travel scenario to be processed'),
            div(class="amTableMargin",
              actionLink('speedTableUndo',icon=icon('undo'),'Reset to original content'),'|',
              actionLink('speedTableMerge',icon=icon('magic'),"Import content from the selected scenario table"),'|',
              actionLink('helpLinkSpeedTable',icon=icon('question-circle'),''),
              hotable("speedRasterTable")
              )
            ),
            fluidRow(class="amRowTable",
            h4('Selected scenario table'),
            div(class="amTableMargin",
              uiOutput('speedTableMergeValidation'),
              hotable("speedSqliteTable")
            )
          )
        ),
      #
      # Scaling up tables
      #
        conditionalPanel(condition="input.moduleSelector=='module_6'",
            fluidRow(
            amCenterTitle('Scaling up',sub="Configuration tables for the scaling up algorithm."),
            fluidRow(class="amRowTable",
              h4('Capacity table for new facilities creation'),
              div(class="amTableMargin",
                actionLink('btnAddRowCapacity',icon=icon("plus-circle"),'Add row'),
                actionLink('btnRmRowCapacity',icon=icon("minus-circle"),'Remove row'),
                hotable("capacityTable")
                )
              ),
            fluidRow(class="amRowTable",
              h4('Suitability factors'),
              div(class="amTableMargin",
                  actionLink("btnResetSuitTable",icon=icon('undo'),"Reset"),
                  actionLink("btnRmSuitTableUnselected",icon=icon('minus-circle'),"Remove unselected row"),
                  hotable("suitabilityTable")
                  )
              ),
            fluidRow(class="amRowTable",
              h4('Exclusion areas'),
              div(class="amTableMargin",
                  actionLink("btnResetExcluTable",icon=icon('undo'),"Reset"),
                  actionLink("btnRmExcluUnselected",icon=icon('minus-circle'),"Remove unselected row"),
                  hotable("exclusionTable")
                  )
              )
            )
          )
        ),
      #
      # Facilities tables
      # 
      conditionalPanel(condition="input.moduleSelector!='module_5'",
        fluidRow(
          amCenterTitle('Facilities selection',sub="Filter and select the facilities on which the analysis will be applied.")
          # fluidRow(class="amRowTable",
          #   h4('Filter and select facilities'),
          #   div(class='btn-group',
          #     actionButton('btnSelectAllHf','All',class='btn-inline'),
          #     actionButton('btnSelecteNoHf','none',class='btn-inline'),
          #     actionButton('btnSelectHfFromRule','apply rules',class='btn-inline')
          #     ),
          #                 conditionalPanel(condition="input.moduleSelector=='module_4'",
          #     radioButtons('selHfFromTo','Target table',choice=c('From','To'),inline=T)
          #     )
          # )
          ),
        h4('Selected facilities'),
        checkboxInput('hfDisplayRules','Display the panel for creating selection rules',value=FALSE),
      #          radioButtons('selHfFromTo','Selection apply to',choice=c("Table 'From'"='From',"Table 'To'"='To'),inline=T)
      #        ),
        #
          # Rule Panel
          #
          fluidRow(
            column(width=12,
              # NOTE: hfDisplayRules is used in amServer to trigger hfFilter* fields.
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
          #
          #  Actions
          #
          div(class="amTableMargin",
            div("data-display-if"="input.moduleSelector=='module_4'",style="display:inline",
              div(id="selHfFromTo",class="form-group shiny-input-radiogroup shiny-input-container",style="display:inline",
                " Apply selection to table ",
                div(class="shiny-options-group",style="display:inline",
                  tags$input(type="radio",style="margin:4px",name="selHfFromTo",value="From",checked="checked","From"),
                  tags$input(type="radio",style="margin:4px",name="selHfFromTo",value="To",checked="checked","To")
                  ) 
                )," | "
              ),

            actionLink('btnSelectAllHf','All',icon=icon('check-square-o')),'|',
            actionLink('btnSelecteNoHf','None',icon=icon('square-o')),'|',
            actionLink('btnSelectHfFromRule','Apply rules',icon=icon('list-ol')),
            div("data-display-if"="input.showDevelTools==true",style="display:inline",
              '|',actionLink('btnSelectRandomHf','random (10%)')
              ),
            #
            # Facilities table
            #
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
          ),
        conditionalPanel(condition="input.moduleSelector=='module_5'",
          fluidRow(class='amTableMargin',
            amCenterTitle('Zonal statistic',sub="Population coverage by zone(s) for the accessibility analysis"),
            hotable('zoneCoverageTable'),
            amCenterTitle('Travel time preview',sub="Map of the selected travel time"),
            plotOutput('previewTravelTime')


            )
          )
        )
    )


