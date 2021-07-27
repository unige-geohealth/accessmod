#library(httpuv)
#library(jsonlite)
#library(shiny)

#s <- startServer("0.0.0.0", 5099,
  #list(
    #call = function(req) {
      #print(req$PATH_INFO)  

      #switch(req$PATH_INFO,
        #"/process/stop" = {

          #q = parseQueryString(req$QUERY_STRING)
          #if(!is.null(q$project)){
            #r <- toJSON(list(
                #state = "stop",
                #project = q$project
                #),
              #auto_unbox = T
            #)
            #write(r,sprint("/tmp/am_process_stop.json")
          #}
        #},
        #"/process/resume" = {
          
          #write('resume',"/tmp/am_process_stop")
        #}
      #)

      #list(
        #status = 200L,
        #headers = list(
          #'Content-Type' = 'text/html'
          #),
        #body = "OK"
      #)
    #} )
#)

