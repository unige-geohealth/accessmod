



  execGRASS('g.copy',raster=c(inputPop,outputPopResidual),flags='overwrite')















  
  # create temp version of population
  execGRASS('g.copy',raster=c(inputPop,'tmp_pop'),flags='overwrite')

  # filter lcv to create initial sampling grid. Set


  #lcvClassToIgnore=c(1,3,4,5)


  if(length(lcvClassToIgnore)>0){
    exp = paste0('tmp_candidate_grid=if(',paste0(paste0(inputLandCover,"=="),lcvClassToIgnore,collapse='|'),',null(),1)')
    execGRASS('r.mapcalc',expression=exp,flags='overwrite') 
  }else{
    exp = paste0('tmp_candidate_grid=if(!isnull(%s),i)') 
  }



  if(length(lcvClassToIgnore)>0){
    tmpFile<-tempfile()
    lcvCat<-read.table(text=execGRASS('r.category',map=inputLandCover,intern=T,separator='comma'),sep=',')$V1
    lcvCat=lcvCat[! lcvCat %in% lcvClassToIgnore] 
    val=paste(paste(lcvCat,collapse=' '),'= 1')
    write(val,tmpFile)
    execGRASS('r.reclass',input=inputLandCover,output='tmp_candidates_grid',rules=tmpFile,flags='overwrite')
  }else{
    # if no filter set, just copy the raster under a tmp name.
    execGRASS('g.copy',raster=c(inputLandCover,'tmp_candidates_grid'),flags='overwrite')
  }


  # for each new HF optimum location
timing<-system.time({
  for(i in 1:nFacilities){
    rmRastIfExists('MASK')




    if(!useExistingFacilities && i==1){
      fTmp<-amScalingCoef_Friction(
        inputMask='tmp_candidate_grid',
        inputFriction=inputFriction)

      pTmp<-amScalingCoef_Pop(
        inputMask=fTmp,
        inputPop='tmp_pop',
        radiusKm=10,
        mapResolution=nsRes)

    }








 
    # filter candidates grid: if non-null value exists in tmp cumulative map or in tmp pop, keep original candidates cell value, else null.
    exp <- paste("tmp_candidates_grid_residual=if(isnull(tmp_pop)|isnull(tmp_cumul),null(),tmp_candidates_grid)")
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    # limit next comuptation to residual candidates grid.
    execGRASS('r.mask',raster='tmp_candidates_grid_residual',flags='overwrite')
    
    if(useMovingWindow){
      # create a density map using a  moving window sum of population on a radius
      execGRASS('r.neighbors',flags=c('c','overwrite'),input='tmp_pop',output='tmp_pop_density',method='sum',size=neighbourSize)
    }else{
      execGRASS('g.copy',raster=c('tmp_pop','tmp_pop_density'))
    }
    # create rescaled base map for priority index
    execGRASS('r.rescale.eq',flags='overwrite',input="tmp_pop_density",output="tmp_pop_density_rescale",to=c(0L,100L))
    execGRASS('r.rescale.eq',flags='overwrite',input=inputFriction,output="tmp_friction_rescale",to=c(0L,100L))
    execGRASS('r.rescale.eq',flags='overwrite',input="tmp_cumul",output="tmp_cumul_rescale",to=c(0L,100L))
    # set weight. NOTE: Could be an option?
    weightFriction=0.4
    weightPopDensity=1
    weightTravelTime=0.01
    weightSum=weightFriction+weightTravelTime+weightPopDensity
    # set the candidate base map 
    exp=paste(
      "tmp_candidates_base=(",
      "(100-tmp_friction_rescale)*",weightFriction,
      "+tmp_pop_density_rescale*",weightPopDensity,
      "+tmp_cumul_rescale*",weightTravelTime,
      ")/",weightSum)
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    # get 99th percentil
    candidates99Percentile<-as.numeric(
      unlist(
        strsplit(grep("percentile_99",
            execGRASS('r.univar',map='tmp_candidates_base',flags=c('e','g'),percentile=99,intern=T)
            ,value=T),
          '=')
        )[[2]])
    # filter 99th percentile
    exp=paste("tmp_candidates_pool=if(tmp_candidates_base>",candidates99Percentile,",tmp_candidates_base,null())")
    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    # export as vector points
    execGRASS('r.to.vect',flags='overwrite',type='point',input='tmp_candidates_pool',output='tmp_candidates_pool',column="amPriorityIndex")
    # get max 10 highest priority hf
    candidatesTable<-dbGetQuery(dbCon,'SELECT * FROM tmp_candidates_pool ORDER BY amPriorityIndex DESC LIMIT 10')
    rmRastIfExists('MASK')
    # loop on HF to find the best candidate
    candidatesEval<-data.frame()
    for(j in candidatesTable$cat ){
      progNum=progNum+1
      hfTest<-paste0('tmp_hf_test_',j)
      hfTestCumul<-paste0('tmp_hf_test_catch_',j)
      
      execGRASS('v.extract',input='tmp_candidates_pool',output=hfTest,cats=paste(j),type='point',flags='overwrite')

      # For this candidate, analyse cumulative cost map without mask
      switch(typeAnalysis,
        'anisotropic'= amAnisotropicTravelTime(
          inputSpeed       = inputSpeed,
          inputHf          = hfTest,
          outputCumulative = hfTestCumul,
          returnPath       = TRUE,
          maxCost          = maxCost,
          minCost          = NULL),
        'isotropic'= amIsotropicTravelTime(
          inputFriction    = mapFriction,
          inputHf          = hfTest,
          outputCumulative = hfTestCumul,
          maxCost          = maxCost,
          minCost          = NULL
          )
        )
      # compute integer version of cumulative cost map to use with r.univar, by minutes
      expr=paste(hfTestCumul,'=int(',hfTestCumul,')')
      execGRASS('r.mapcalc',expression=expr,flags='overwrite')
      # compute zonal statistic : time isoline as zone
      tblPopByZone<-read.table(
        text=execGRASS(
          'r.univar',
          flags  = c('g','t','overwrite'),
          map    = 'tmp_pop',
          zones  = hfTestCumul,
          intern = T
          ),sep='|',header=T)
      # calculate cumulated sum of pop at each zone
      tblPopByZone$cumSum<-cumsum(tblPopByZone$sum)
      tblPopByZone<-tblPopByZone[c('zone','sum','cumSum')]
      # After cumulated sum, order was not changed, we can use tail/head to extract min max
      totalPop<-tail(tblPopByZone,n=1)$cumSum
      # Set capacity using capacity table
      resCap<-inputTableCap[totalPop<=as.integer(inputTableCap$max) & totalPop>as.integer(inputTableCap$min), ]
      # If nothing match, stop the process.
      if(!nrow(resCap)==1) stop(paste('amScalingUp did not found a suitable capacity value for a new facility in the provided capacity table. Please make sure that one (and only one) interval min/max can handle a total population potential coverage of',totalPop))
      # find the zone that overpass or equal capacity
      #timeLimitCap<-tblPopByZone[as.integer(resCap$capacity) <= tblPopByZone$cumSum,'zone'][1]
      # check time vs pop correlation : negative value = covered pop decrease with dist; positive value = covered pop increase with dist
      corPopTime <- cor(tblPopByZone$zone,tblPopByZone$sum)
      # bind current summary to previous
      candidatesEval<-rbind(
        candidatesEval,
        data.frame(
          amProcessingOrder    =  i,
          amRasterCumul        =  hfTestCumul,
          amVectorPoint        =  hfTest,
          amCorrPopTime        =  corPopTime,
          amTimeMax            =  maxCost,
          amPopTimeMax         =  totalPop,
          amCapacity           =  resCap$capacity,
          #amTimeLimitCapacity  =  timeLimitCap,
          amLabel              =  resCap$label
          )
        )
    amUpdateProgressBar(session,"cumulative-progress",round(progInc*progNum))
    }
   
    # select best facility
    # criteria: more people covered under time max
    # other criterias could be :
    #     min time to reach a capacity value. Ex. reached capacity of 4000 in 3 minutes: keep only 180 seconds from cumulativecost
    #     most negative correlation between population covered and time traveled. Ex. corr=-0.4, the farest we go, the less we cover population.
    #     ... A lot of choices. What is the best one ??
    candidatBest<-candidatesEval[order(candidatesEval$amPopTimeMax),][1,]
    # get the name of choosen facility vector point
    vectBest<-paste(candidatBest$amVectorPoint)
    # population covered


     if(removePop){
       #TODO: set original population as input, and calc percentage covered at each iteration
       # update population residual: remove potentialy covered population.
       #exp<-paste("tmp_mask_pop=if(",candidatBest$amRasterCumul,">=",candidatBest$amTimeLimitCapacity,",tmp_pop,null())")
       exp<-paste("tmp_pop=if(isnull(",candidatBest$amRasterCumul,"),tmp_pop,null())")
       execGRASS('r.mapcalc',expression=exp,flags='overwrite')
    }
    # update column for select hf
    sql=paste("SELECT * FROM",vectBest)
    candidatTable<-dbGetQuery(dbCon,sql)
    tblNewHf<-cbind(candidatTable,candidatBest[,c('amProcessingOrder','amCorrPopTime','amTimeMax','amPopTimeMax','amLabel','amCapacity')])
    dbWriteTable(dbCon,vectBest,tblNewHf,overwrite=TRUE)
    if(i==1){
      rmVectIfExists(outputFacilities)
      execGRASS('g.copy',vector=c(vectBest,outputFacilities))
    }else{
      execGRASS('v.patch',flags=c('e','a','overwrite'),input=vectBest,output=outputFacilities)
    }
    # Add new HF to existing one for cumulative filtering
    execGRASS('v.patch',input=vectBest,output='tmp_hf_all',flags=c('overwrite','a'))
  }
  # store only cat in final vector, and attribute table as separate file. Why? shapefile limited column naming length.  
  outTableAttr<-dbGetQuery(dbCon,paste('SELECT * FROM',outputFacilities))
  dbWriteTable(dbCon,outputFacilities,outTableAttr['cat'],overwrite=TRUE)
  dbWriteTable(dbCon,outputTable,outTableAttr,overwrite=TRUE)
  rmRastIfExists('tmp_*') 
  rmVectIfExists('tmp_*')
  amUpdateProgressBar(session,"cumulative-progress",round(progInc*progNum)+10)
})



#
#    # get max value (floored to for the comparison in mapcalc. grass does not found back the original max value in map)
#    maxVal<-floor(amGetRasterStat('tmp_pop_density','max')*1000)/1000
#    # exctact point on the max pop density 
#    exp=paste('tmp_sample_point = if(tmp_pop_density>',maxVal,',tmp_pop_density,null())')
#    execGRASS('r.mapcalc',expression=exp,flags='overwrite')
#    if(!amGetRasterStat('tmp_sample_point')==1){
#      # NOTE: if the module found 0 location, reduce minimum distance?
#      stop('AccessMod found more than one (',amGetRasterStat('tmp_sample_point'),'points) candidate for the facility location.')
#    }
#    execGRASS('r.to.vect',type='point',input='tmp_sample_point',column='amPopSumMovingWindow',output='tmp_sample_point',flags='overwrite')
#
#

