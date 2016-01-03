
wellPanel(
  amCenterTitle(div(icon("table"),'Scaling up tables'),h=3,m=0,sub="Configure the tables for the scaling-up analysis."),
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
              value=5,
              min=0,
              max=10
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
          numericInput('exclusionBuffer',
            label='Set an optional buffer (km)',
            value=5,
            min=0,
            max=99
            ),
          radioButtons('exclusionMethod',
            label='Choose exclusion method',
            c(
              'Keep candidates outside buffer' = 'keepOutside',
              'Keep candidates inside buffer' = 'keepInside'
              )
            ),
          actionButton('btnAddExclusion',icon=icon('plus-circle'),'Add')
          )
        )
      )
    )
  )
