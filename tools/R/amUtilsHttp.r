
host <- "0.0.0.0"
port <- as.numeric(Sys.getenv("AM5_PORT_HTTP"))
res <- list(
  body = NULL,
  status = 200L,
  headers = list(
    "Content-Type" = "text/html",
    "Access-Control-Allow-Origin" = "*"
  )
)

resJson <- list(
  body = "{}",
  status = 200L,
  headers = list(
    "Content-Type" = "application/json",
    "Access-Control-Allow-Origin" = "*"
  )
)


resNotFound <- list(
  body = "not found",
  status = 404L,
  headers = list(
    "Content-Type" = "text/html",
    "Access-Control-Allow-Origin" = "*"
  )
)

tryCatch(
  {
    #
    # Start server
    #
    srv <- startServer(
      host = host, port = port,
      list(
        call = function(req) {
          #
          # Check service status ( healthcheck )
          #
          if (req$PATH_INFO == "/status") {
            out <- res
            out$body <- "ok"
            return(out)
          }

          #
          # Stop process from client
          #
          if (req$PATH_INFO == "/progress/stop") {
            out <- res
            out <- promise(function(resolve, reject) {
              amProgressStopWrite()
              out$body <- "stop_requested"
              resolve(out)
            })
            return(out)
          }

          #
          # Get a summary of mapx versions
          #
          if (req$PATH_INFO == "/versions/summary.json") {
            out <- resJson
            out <- promise(function(resolve, reject) {
              summary <- amDockerVersionsSummary()
              out$body <- toJSON(
                summary,
                auto_unbox = TRUE,
                pretty = TRUE
              )
              resolve(out)
            })
            return(out)
          }

          #
          # Monitor stat
          #
          if (req$PATH_INFO == "/monitor/stats.json") {
            #https://docs.docker.com/engine/api/v1.41/#operation/ContainerStats
            out <- resJson
            out <- promise(function(resolve, reject) {
              stats <- amDockerMonitorStat()
              out$body <- toJSON(
                stats,
                auto_unbox = TRUE,
                pretty = TRUE
              )
              resolve(out)
            })
            return(out)
          }

          return(resNotFound)
        }
      )
    )
    #
    # Print listening message
    #
    print(sprintf("Http server listening on http://%1$s:%2$s", host, port))
    service(0)
  },
  error = function(e) {
    warning(e)
    quit(
      save = "no",
      status = 1
    )
  }
)
