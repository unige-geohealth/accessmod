#       __                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
# main server file.

# shortcut for development
s <- function(port=3939){
  library(shiny)
  runApp('.',port=port,launch.browser=FALSE)
}

function(input, output, session){
  amErrorAction(title="Shiny server",
    pBarFinalRm=F,{
      #automatic update..
      if(identical(as.character(config$hostname),"accessmod")){
        system("/bin/bash sh/update.sh",wait=F)
      }
      # set a cookie in client browser
      amSetCookie(cookie=list("dateSession"=date()))  
      # Session reactive values :
      # reactive value to hold event and logic 
      listen<-reactiveValues()
      # reactive object to hold variables in module "manage data"
      dataMetaList<-reactiveValues()
      # reactive values to store list of data set
      dataList<-reactiveValues()

      # set global grassSession reactive values
      grassSession<-reactiveValues()
      # check if there is already an active grass session and update value accordingly.
      if(isTRUE(nchar(get.GIS_LOCK())>0)){
        grassSession$mapset<-execGRASS("g.mapset",flags="p",intern=T)
      }



      # read cookie and parse content
      observeEvent(input$readCookie,{
        cookie <- input$readCookie
        loc <- cookie$location
        listen$defaultLoc <- loc
})
      # initiat gisLock
      grassSession$gisLock<-NULL
      # update data list if requested
      observeEvent(listen$dataListUpdate,{
        amErrorAction(title="Data list observer",{
          # get available grass locations (does not need grass env yet)
          grassSession$locations<-amGetGrassListLoc(config$pathGrassDataBase)
          # parse grass variable
          grassSession$pathShapes<-system(paste("echo",config$pathShapes),intern=T)
          # 
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
        tab<-paste0("tabControl_",tab)
        listen[[tab]]<-TRUE
      })
      #ource modules (amServer files in given module path)
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


