


amReMemoizeCostlyFunctions = function(){
  amMemoizeThat('amGetRasterCategory')
  amMemoizeThat('amGetRasterStat')
  amMemoizeThat('amGetFacilitiesTable')
  amMemoizeThat('amZonalAnalysis')
}


amMemoizeThat = function(funName){
  funNameCached = paste0(funName,'_cached')
  if(exists(funNameCached) && is.memoised(get(funNameCached))){
      forget(get(funNameCached))
    }else{
      assign(funNameCached, memoise(get(funName)), envir=environment(get(funName)))
    }
}
