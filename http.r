#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(httpuv)
library(jsonlite)
library(promises)
library(shiny)
library(cachem)
source('tools/R/amProgress.R')
source('tools/R/amFunctions.R')
source('tools/R/amDockerHelpers.R')

amProgressStopClean()

res <- list(
  body = NULL,
  status = 200L,
  headers = list(
    'Content-Type' = 'text/html',
    'Access-Control-Allow-Origin'= '*'
  )
)

host <- '0.0.0.0'
port <- as.numeric(Sys.getenv('AM5_PORT_HTTP'))


tryCatch({
  #
  # Start server
  #
  stopAllServers()
  srv <- startServer(host = host, port = port,
    list(
      call = function(req) {
        out <- res
        switch(
          req$PATH_INFO,
          #
          # Check service status ( healthcheck )
          # 
          '/status'= {
            out$body = 'ok'
            return(out);
          },
          #
          # Stop process from client
          # 
          '/progress/stop'={
            out <- promise(function(resolve, reject) {
              amProgressStopWrite()
              out$body <- 'stop_requested'
              resolve(out)
              })
            return(out);
          },
          #
          # Get a summary of mapx versions 
          # 
          '/versions/summary.json'={
            out <- promise(function(resolve, reject) {
              summary <- amDockerVersionsSummary()
              out$body <- toJSON(
                summary,
                auto_unbox = TRUE,
                pretty = TRUE
              )
              resolve(out)
              })
            return(out);
          }
        )
      }
    )
  )
  #
  # Print listening message
  #
  print(sprintf('Http server listening on http://%1$s:%2$s',host,port))
  service(0)
},error = function(e){
  warning(e)
  quit(
    save = 'no',
    status = 1
  )
})
