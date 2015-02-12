#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# Module 3: existing network analysis
# Uses r.walk.accessmod, a custom module made for GRASS GIS

#----------------------------------------{ UI : validation
output$module3<-renderUI({
  # conditional module display
  mapMerged<-dataList$merged
  mapHf<-dataList$hf
  validInput<-c(
    'm'=length(mapMerged)>0,
    'h'=length(mapHf)>0,
    'g'=length(listen$gisLock)>0
    )
  msgList<-tagList()
  if(!all(validInput)){
    msgList$g<-ifelse(!validInput['g'],msgNoLocation,'')
    msgList$h<-ifelse(!validInput['h'],'No health facilities map found. ','')
    msgList$m<-ifelse(!validInput['m'],'No merged land cover map found. ','') 
    amPanel(width=12,tagList(icon('exclamation-triangle'),msgList))
    # display ui if everything is ok.
  }else{
    fluidRow(
      sidebarPanel(width=3,
        formCreateTimeCostMap
        ),
      mainPanel(width=9,
        formTablePanel
        )
      )
  }
})


#----------------------------------------{ UI : create cumulative cost map
formCreateTimeCostMap<-renderUI({
  tableSqlite<-dataList$table
  tableSqlite<-tableSqlite[grep('table_model',tableSqlite)]
  tagList(
    h4('Compute accessibility to health facilities'),
    amProgressBar('cumulative-progress'),
    p('Create an anistropic cummulative cost map.'),
    selectInput('mergedSelect','Select merged land cover map:',choices=dataList$merged),
    selectInput('hfSelect','Select health facilities map:',choices=dataList$hf),
    selectInput('modelSelect','Select optional model table:',choices=tableSqlite),
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
    radioButtons("colTable","Use a color table for cummulative map",
      c( "White-blue-black" = "blue",
        "Yellow-green-blue-red-black" = "slope",
        "none" = "none"
        ),selected="slope"),
    numericInput('maxTimeWalk',
      label='Maximum transportation time [minutes]',
      value=120,
      min=0,
      max=1080,# note: max value un raster cell for geotiff with color palette (unint16) :2^16-1
      step=1
      ),
    textInput('costTag','Add tags (minimum 1)',value=''),
    actionButton('btnCreateTimeCostMap','Compute speed map and cumulative cost map')
    )
})



#----------------------------------------{ UI : display model table
formTablePanel<-renderUI({
  tagList(
    h4('Table of speed by category and transport mode.'),
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
    p(list(strong('Mode'), 'mode of transportation :',names(transpModList)))
    )

})

