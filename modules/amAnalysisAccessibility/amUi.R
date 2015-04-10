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
  column(width=3,
    amAccordionGroup(id='accessibilitySettings',show=1,itemList=list(
        'inputSettings'=list(
          title=div(icon('sign-in'),'Select inputs'),
          content=tagList(
            #
            # select map and table for accessibility
            #
            conditionalPanel(condition="input.moduleSelector!= 'module_5'",
              selectInput('mergedSelect','Select merged land cover map:',choices=""),
              selectInput('modelSelect','Select optional model table:',choices=""),
              conditionalPanel(condition="input.moduleSelector== 'module_4'",
                tags$b("From:")
                ),
              selectInput('hfSelect','Select health facilities map:',choices=""),
              conditionalPanel(condition="
                input.moduleSelector=='module_3' |
                input.moduleSelector=='module_4'
                ",
                selectInput('hfIdxField',"Select facilities index",choices=""),
                selectInput('hfNameField',"Select facilities label",choices="") 
                ),
              conditionalPanel(condition="input.moduleSelector=='module_4'",
                tags$b("To:"),
                selectInput('hfSelectTo','Select health facilities map:',choices=""),
                selectInput('hfIdxFieldTo',"Select facilities index",choices=""),
                selectInput('hfNameFieldTo',"Select facilities label",choices="") 
                )
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
          # Select cumulative cost map
          #
          conditionalPanel(condition="input.moduleSelector=='module_5'",
            selectInput('cumulativeCostMapSelect',"Select cumulative cost map",choices=""),
            sliderInput('sliderTimeAnalysis',"Select time value",value='',min=0, max=0)
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
          )
          )
      )
    )
  ),
#
# Tags, validation message and compute button
#
conditionalPanel(condition="input.moduleSelector!='module_5'",
  amAccordionGroup(id='accessibilityValidation',show=c(1),itemList=list(
      'validation'=list(
        title=div(icon('check-square-o'),'Validation'),
        content=tagList(
          textInput('costTag','Add tags (minimum 1)',value=''),
          uiOutput('msgModule3'),
          actionButton('btnCreateTimeCostMap','Compute'), 
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
    amAccordionGroup(id='accessibilityTable',show=c(1,2),itemList=list(
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
        'hfTables'=list(
          title='Health facilities tables',
          content=fluidRow(
            amPanel(width=12,
              checkboxInput('hfDisplaySelect','Display selection panel',value=F),
              conditionalPanel(condition='input.hfDisplaySelect==true',
                fluidRow(
                  tagList(
                    sidebarPanel(width=5,
                      tagList(
                        h4('Select'),
                        conditionalPanel(condition="input.moduleSelector=='module_4'",
                          radioButtons('selHfFromTo','Target table',choice=c('From','To'),inline=T)
                          ),
                        p(tags$label('Action')),
                        div(class='btn-group',
                          actionButton('btnSelectAllHf','Select all',class='btn-inline'),
                          actionButton('btnSelecteNoHf','none',class='btn-inline'),
                          actionButton('btnSelectRandomHf','random (10%)',class='btn-inline'),
                          actionButton('btnSelectHfFromRule','apply rules',class='btn-inline')
                          ),

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
              h4('Health facilities'),  
              conditionalPanel(condition="input.moduleSelector=='module_4'",
                tags$b('From')
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
          h4('Potential population coverage under given cumulative cost map'),
          hotable('zoneCoverageTable')
          )

        )
      )
    )
  )
)

