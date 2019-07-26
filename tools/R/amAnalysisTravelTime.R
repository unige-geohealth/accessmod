
#'amIsotropicTraveTime
#'@export
amIsotropicTravelTime<-function(
  inputFriction,
  inputHf,
  inputStop=NULL,
  inputCoord=NULL,
  outputDir=NULL,
  outputCumulative,
  maxCost=0,
  maxSpeed=0,
  minCost=NULL,
  timeoutValue=-1L,
  getMemDiskRequirement=FALSE,
  ratioMemory=1
  ){

  vInfo = amParseOptions(execGRASS("v.info",flags=c("t"),map=inputHf,intern=T))
  vHasLines = as.numeric(vInfo$lines) > 0
  tmpStart = NULL
  if(vHasLines){
    tmpStart =  amRandomName("tmp__raster_start")
    suppressWarnings({
      execGRASS("v.to.rast",input=inputHf,output=tmpStart,use="val",value=1)
    })
    inputRaster=tmpStart
    inputHf=NULL
  }else{
    inputRaster=NULL
  }

  # default memory allocation
  free = 300
  disk = 10
  # dynamic memory allocation
  tryCatch({
    free = sysEvalFreeMbMem()
  },error=function(cond){
    amMsg(
      type="log",
      text=cond$message
      )
  })

  tryCatch({
    disk = as.integer(sysEvalFreeMbDisk())
  },error=function(cond){
    amMsg(
      type="log",
      text=cond$message
      )
  })


  amParam=list(
    input = inputFriction,
    output = outputCumulative,
    start_points = inputHf,
    start_raster = inputRaster,
    start_coordinates = inputCoord,
    stop_points = inputStop,
    outdir = outputDir,
    max_cost = as.integer(maxCost * 60),
    memory = as.integer(free * 0.8 * ratioMemory)
    )

  amParam <- amParam[!sapply(amParam,is.null)]

  diskRequire <- disk
  memRequire <- free

  tryCatch({
    testSysLimit = execGRASS('r.cost',
      parameters=amParam,
      flags=c('i','overwrite'),
      intern=T
      )
    diskRequire <- as.integer(gsub("[a-zA-Z]","",testSysLimit[grepl("disk space",testSysLimit)]))
    memRequire <- as.integer(gsub("[a-zA-Z]","",testSysLimit[grepl("of memory",testSysLimit)]))

  },error=function(cond){
    amMsg(
      type = "log",
      text = cond$message
      )
  })

  if(!getMemDiskRequirement && diskRequire > disk * 0.8 ) stop(sprintf("Insufficient disk space. Required= %1$s MB, Available= %2$s MB",diskRequire,disk))
  if(!getMemDiskRequirement && memRequire > free * 0.8 ) stop(sprintf("Insufficient memory. Required= %1$s MB, Available= %2$s MB",memRequire,free))

  if(!getMemDiskRequirement){
    amMsg(
      type="log",
      text=sprintf("Memory required for r.cost = %1$s MB. Memory available = %2$s MB. Disk space required = %3$s MB. Disk space available = %4$s MB",
        memRequire,
        free,
        diskRequire,
        disk
        )
      )
  }

  if(!getMemDiskRequirement){

    if(maxSpeed>0 && maxCost>0){
      on.exit(amSpeedBufferRegionRestore())
      amSpeedBufferRegionInit(c(inputHf,inputStop),maxSpeed/3.6,maxCost*60)
    }

    execGRASS('r.cost',
      parameters=amParam,
      flags='overwrite'
      )

    amCleanTravelTime(
      map = outputCumulative,
      maxCost = maxCost,
      minCost = minCost,
      timeoutValue=timeoutValue,
      convertToMinutes = TRUE
      )

    rmRastIfExists(tmpStart)
  }else{
    return(
      list(
        required = list(
          memory = memRequire,
          disk = diskRequire
          ),
        available = list(
          memory = free,
          disk = disk
          )
        )
      )

  }
}
#'amAnisotropicTravelTime 
#' @param maxCost maximum cost in minute
#'@export
amAnisotropicTravelTime <- function(
  inputSpeed,
  inputHf,
  inputCoord=NULL,
  inputStop=NULL,
  outputDir=NULL,
  outputCumulative=NULL,
  returnPath=FALSE,
  maxCost=0,
  minCost=NULL,
  maxSpeed=0,
  timeoutValue='null()',
  getMemDiskRequirement=FALSE,
  ratioMemory = 1
  ){

  #  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''),ifelse(keepNull,'n',''))
  flags=c(c('overwrite','s'),ifelse(returnPath,'t',''))
  flags<-flags[!flags %in% character(1)]

  # default memory allocation
  free = 300
  disk = 10

  # dynamic memory allocation
  tryCatch({
    free = as.integer(sysEvalFreeMbMem())
  },error=function(cond){
    amMsg(
      type="log",
      text=cond$message
      )
  })

  tryCatch({
    disk = as.integer(sysEvalFreeMbDisk())
  },error=function(cond){
    amMsg(
      type="log",
      text=cond$message
      )
  })


  #
  # Convert vector line starting point to raster
  #
  vInfo = amParseOptions(execGRASS("v.info",flags=c("t"),map=inputHf,intern=T))

  vHasLines = as.numeric(vInfo$lines) > 0

  tmpStart = NULL

  if(vHasLines){
    tmpStart =  amRandomName("tmp__raster_start")
    suppressWarnings({
      execGRASS("v.to.rast",input=inputHf,output=tmpStart,use="val",value=1)
    })
    inputRaster = tmpStart
    inputHf = NULL
  }else{
    inputRaster = NULL
  }

  #
  # set
  #

  amParam = list(
    elevation = config$mapDem,
    friction = inputSpeed,
    output = outputCumulative,
    start_points = inputHf,
    start_raster = inputRaster,
    start_coordinates = inputCoord,
    stop_points = inputStop,
    outdir = outputDir,
    memory = as.integer(free * 0.8 * ratioMemory),
    max_cost = as.integer(maxCost * 60) # max cost in seconds.
    )

  amParam <- amParam[!sapply(amParam,is.null)]


  diskRequire = 0
  memRequire = 0

  tryCatch({
    testSysLimit = execGRASS('r.walk.accessmod',
      parameters=amParam,
      flags=c('i',flags),
      intern=T
      )
    diskRequire <- as.integer(gsub("[a-zA-Z]","",testSysLimit[grepl("disk space",testSysLimit)]))
    memRequire <- as.integer(gsub("[a-zA-Z]","",testSysLimit[grepl("of memory",testSysLimit)]))

  },error=function(cond){
    amMsg(
      type = "log",
      text = cond$message
      )
  })

  if(!getMemDiskRequirement && diskRequire > disk * 0.8 ) stop(sprintf("Insufficient disk space. Required= %1$s MB, Available= %2$s MB",diskRequire,disk))
  if(!getMemDiskRequirement && memRequire > free * 0.8 ) stop(sprintf("Insufficient memory. Required= %1$s MB, Available= %2$s MB",memRequire,free))

  if(!getMemDiskRequirement){
    amMsg(
      type="log",
      text=sprintf("Memory required for r.walk.accessmod = %1$s MB. Memory available = %2$s MB. Disk space required = %3$s MB. Disk space available = %4$s MB",
        memRequire,
        free,
        diskRequire,
        disk
        )
      )
  }

  if(!getMemDiskRequirement){

    if(maxSpeed>0 && maxCost>0){
      on.exit(amSpeedBufferRegionRestore())
      amSpeedBufferRegionInit(c(inputHf,inputStop),maxSpeed/3.6,maxCost*60)
    }

    execGRASS('r.walk.accessmod',
      parameters=amParam,
      flags=flags
      ) 

    amCleanTravelTime(
      map = outputCumulative,
      maxCost = maxCost,
      minCost = minCost,
      timeoutValue=timeoutValue,
      convertToMinutes = TRUE
      )
    rmRastIfExists(tmpStart)
  }else{
    return(
      list(
        required = list(
          memory = memRequire,
          disk = diskRequire
          ),
        available = list(
          memory = free,
          disk = disk
          )
        )
      )
  }

}


