library(httpuv)
library(promises)
source('tools/R/amProgress.R')
source('tools/R/amFunctions.R')

amProgressStopClean()

res <- list(
  body = NULL,
  status = 200L,
  headers = list(
    'Content-Type' = 'text/html',
    'Access-Control-Allow-Origin'= '*'
  )
)

args <- commandArgs(trailingOnly=TRUE)
port <- 5099
host <- '0.0.0.0'

if(length(args)>0){
  port <- as.numeric(args[1])
}

tryCatch({
  startServer(host = host, port = port,
    list(
      call = function(req) {
        out <- res
        if (req$PATH_INFO == '/status') {
          out$body <- 'ok'
          return(out)
        }else if (req$PATH_INFO == '/progress/stop') {
          promise(
            function(resolve, reject) {
              amProgressStopWrite()
              out <- res
              out$body = 'ok'
              resolve(out)}
          )
        }else{
          stop("Unknown path",req$PATH_INFO)
        }
      }
    )
  )
  print(sprintf('Listening on http://%1$s:%2$s',host,port))
  service(0)
},error = function(e){
  warning(e)
  quit(
    save = 'no',
    status = 1
  )
})
