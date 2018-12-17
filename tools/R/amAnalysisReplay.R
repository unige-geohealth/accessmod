
amAnalysisGetPath <- function(name=NULL){
  cacheDir <- config$pathCacheDir
  if(!dir.exists(cacheDir)) stop('Cache directory not found')
  analysisPathDir <- paste0(cacheDir,'/amAnalysis')
  if(!is.null(name)){
    analysisPathFile <- paste0(analysisPathDir,'/amAnalysis_',name,'.rdata')
  }else{
    analysisPathFile <- "" 
  }

  if(!dir.exists(analysisPathDir)){
    dir.create(analysisPathDir)
  }
  return(analysisPathFile)
}

amAnalysisSave <-function(name="default"){
  e = parent.frame()
  params = as.list(e)
  pathFile = amAnalysisGetPath(name)
  call = as.list(eval(quote(match.call()),env=e))
  fun = call[[1]]

  out <- list(
    fun = as.character(fun),
    params = params
    )

  saveRDS(out,pathFile)
}


amAnalysisList <-function(){
  path = amAnalysisGetPath()
  files = list.files(path)
  getBase = function(x){
    str_split(a,'_|\\.')[[1]][2]
  }
  vapply(files,getBase, character(1), USE.NAMES=F)
}

amAnalysisGet <-function(name="default"){

  pathFile = amAnalysisGetPath(name)
  if(file.exists(pathFile)){
  return(readRDS(pathFile))
  }

}

amAnalysisReplay <- function(name="default"){
  a = amAnalysisGet(name)
  do.call(a$fun,a$params)
}
