#       __                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# main server file.


function(input, output, session){
  amErrorAction(title="Shiny server",
    pBarFinalRm=F,{
      #
      # Auto update 
      #
      if(identical(as.character(config$hostname),"accessmod")){
        system("/bin/bash sh/update.sh",wait=F)
      }

      #
      # Session reactive values
      #

      # reactive value to hold event and logic 
      listen <- reactiveValues()
      # reactive object to hold variables in module "manage data" NOTE: could be merged with "listen"
      dataMetaList <- reactiveValues()
      # set global grassSession reactive values
      grassSession<-reactiveValues()
      # reactive values to store list of data set
      dataList <- reactiveValues()

      #
      # Grass session
      #

      # check if there is already an active grass session and update value accordingly.
      if(isTRUE(nchar(get.GIS_LOCK())>0)){
        grassSession$mapset <- execGRASS("g.mapset",flags="p",intern=T)
      }
      # initiate gisLock
      grassSession$gisLock<-NULL

      #
      # Data list update
      #
      observeEvent(listen$dataListUpdate,{
        amErrorAction(title="Data list observer",{
          # get available grass locations (does not need grass env yet)
          grassSession$locations <- amGetGrassListLoc(config$pathGrassDataBase)
          amDataManager(config,dataList,grassSession)
})
},priority=100)

      # modules checker. 
      # we want to prevent all reactives values to be triggered at the same time,
      # so, we have put an observer in GIS and analysis module that will launch
      # as soon as input$whichTab change (ui menu) give their ID.
      # BUT. this will also invalidate all reactive value contained. We don"t want that.
      # This code will only produce one update, trigger all reactive values and stay as 
      # it for the rest of the shiny session.
      observe({
        tab<-input$whichTab
        tab<-sprintf("tabControl_%s",tab)
        listen[[tab]]<-TRUE
      })
      #source modules (amServer files in given module path)
      modList<-dir(config$pathModule,full.names = T)
      for(m in modList){
        amServPath<-file.path(m,"amServer.R")
        amHelpPath<-file.path(m,"amHelp.R")
        if(file.exists(amServPath)){
          source(amServPath,local=TRUE)
        }
        if(file.exists(amHelpPath)){
          source(amHelpPath,local=TRUE)
        }

      }
    })
}


