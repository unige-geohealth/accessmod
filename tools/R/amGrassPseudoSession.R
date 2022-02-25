
amGrassPseudoSessionStart <- function(...){

  args <- list(...)

  gisrc <- tempfile()

  if(!is.null(args$project)){
    args$location <- args$project
    args$mapset <- args$project
  }
  if(is.null(args$location)){
    args$location <- Sys.getenv('LOCATION')
  }
  if(is.null(args$location)){
    args$location <- Sys.getenv('LOCATION_NAME')
  }
  if(is.null(args$mapset)){
    args$mapset <- Sys.getenv('MAPSET')
  }
  if(is.null(args$gisdbase)){
    args$gisdbase <- Sys.getenv('GISDBASE')
  }

  gisrcValue <- list(
    "GISDBASE" = args$gisdbase,
    "LOCATION" = args$location,
    "LOCATION_NAME" = args$location,
    "MAPSET" = args$mapset
  )

  Sys.setenv("GISRC"=gisrc)
  Sys.setenv("LOCATION"=args$location)
  Sys.setenv("LOCATION_NAME"=args$location)
  Sys.setenv("MAPSET"=args$mapset)
  Sys.setenv("GISDBASE"=args$gisdbase)

  write.dcf(gisrcValue,file=gisrc)

  # clean non-grass stuff
  args[['project']] <- NULL
  args[['location']] <- NULL
  args[['mapset']] <- NULL
  args[['gisdbase']] <- NULL

  # return config object
  return(list(
      args = args,
      gisrc = gisrc      
      ))
}

amGrassPseudoSessionEnd <- function(config){
  unlink(config$gisrc)
  Sys.setenv("GISRC"="")
  Sys.setenv("LOCATION"="")
  Sys.setenv("LOCATION_NAME"="")
  Sys.setenv("MAPSET"="")
  Sys.setenv("GISDBASE"="")
}


#' Reassign execGRASS for pseudo session
#' note : amReasign create a copy of the function as <name>_orig
#'
amReasign('rgrass7','execGRASS',function(...){

  usePerf <- exists("config") && "perf" %in% config$logMode 

  g_session <- amGrassPseudoSessionStart(...)

  if(usePerf){
    amTimer("start",args[[1]])
  }

  on.exit({
    amGrassPseudoSessionEnd(g_session)
  })

  out <- do.call(
    'execGRASS_orig',
    g_session$args
  )

  if(usePerf){
    d <- amTimer()
    amDebugMsgPerf(d$title,d$diff)
  }

  return(out) 

  })

#' Reassign gmeta for pseudo session
#' note : amReasign create a copy of the function as <name>_orig
#'
amReasign('rgrass7','gmeta',function(...){

  g_session <- amGrassPseudoSessionStart(...)

  on.exit({
    amGrassPseudoSessionEnd(g_session)
  })

  do.call(
    'gmeta_orig',
    g_session$args
  )
  })







