wellPanel(
  amCenterTitle(div(icon('sign-in'),'Data input'),h=3,m=0,sub="Select the data to be used in this analysis"),
  #
  # Select population layer
  #
  conditionalPanel(condition="(
    input.moduleSelector=='module_3' |
    input.moduleSelector=='module_5' |
    input.moduleSelector=='module_6'
    )",
  selectInput("popSelect","Select population layer (raster)",choices="")
  ),
conditionalPanel(condition="(input.moduleSelector=='module_6')",
  selectInput("popResidualSelect","Select residual population layer (raster)",choices="")
  ),
#
# select merged landcover and model table
#
conditionalPanel(condition="
  input.moduleSelector != 'module_5'
  ",
  selectInput("mergedSelect","Select merged land cover layer (raster)",choices=""),
  selectInput("modelSelect","Select scenario table (table)",choices=""),
  conditionalPanel(condition="input.moduleSelector== 'module_4'",
    tags$b(icon('play'),"From:")
    ),
  #
  # select facility tmap and columns
  #
  conditionalPanel(condition="!(input.moduleSelector=='module_6' & input.useExistingHf == 'FALSE')",
    selectInput("hfSelect","Select existing health facilities layer (vector)",choices=""),
    conditionalPanel(condition="
      input.moduleSelector=='module_3' |
      input.moduleSelector=='module_4' |
      input.moduleSelector=='module_6'
      ",
      div(style="margin-left:10%;",
        selectInput("hfIdxField","Select facility ID field (unique)",choices=""),
        selectInput("hfNameField","Select facility name field (text)",choices="") 
        )
      ),
    conditionalPanel(condition="input.moduleSelector=='module_4'",
      tags$b(icon("stop"),"To:"),
      selectInput("hfSelectTo","Select existing health facilities layer (vector)",choices=""), 
      div(style="margin-left:10%;",
        selectInput("hfIdxFieldTo","Select facility ID field (unique)",choices=""),
        selectInput("hfNameFieldTo","Select facility name field (text)",choices="") 
        )
      ),
    #
    # Select health facilities capacity field  
    #
    conditionalPanel(condition="(
      input.moduleSelector=='module_6' |
      input.moduleSelector=='module_3'
      )",
    div(style="margin-left:10%;",
      selectInput("hfCapacityField","Select facilities capacity field (numeric):",choices="")
      )
    )
  )
),
#
# Select cumulative cost map
#
conditionalPanel(condition="(
  input.moduleSelector=='module_5'
  )",
selectInput("travelTimeSelect","Select travel time layer (raster)",choices="")
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
  selectInput("zoneSelect","Select zones layer (vector)",choices=""),
  div(style="margin-left:10%;",
    selectInput("zoneId","Select zone unique ID (integer)",choices=""),
    selectInput("zoneLabel","Select zone name (text)",choices="")
    )
  ),
conditionalPanel(condition="(
  input.moduleSelector=='module_5'
  )",
sliderInput("sliderTimeAnalysis","Select maximum travel time [minutes]",value=0,min=0, max=0,step=1),
#tags$label("Set numeric travel time [minutes]"),
#tags$input(type="number",class="form-control",id="numZonal",min=0,max=10000,onchange="
  #$('#sliderTimeAnalysis').data('ionRangeSlider').update({from:this.value})
  #"),
actionButton("btnZonalStat","Update")
),
conditionalPanel(condition="(
  input.moduleSelector=='module_6'
  )",
#
# select external capacity table 
#
selectInput("capTableSelect","Select existing capacity table",choices=""),
selectInput("suitabilityTableSelect","Select existing suitability table",choices=""),
selectInput("exclusionTableSelect","Select existing exclusion table",choices="")
)
)

