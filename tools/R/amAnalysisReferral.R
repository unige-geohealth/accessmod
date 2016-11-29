

#'amReferralTable
#'@export
amReferralTable<-function(
  session=shiny:::getDefaultReactiveDomain(),
  inputSpeed,
  inputFriction,
  inputHf,
  inputHfTo,
  inputTableHf,
  inputTableHfTo,
  idField,
  idFieldTo,
  labelField,
  labelFieldTo,
  typeAnalysis,
  resol,
  dbCon,
  unitCost=c('s','m','h'),
  unitDist=c('m','km'),
  outReferral,
  outNearestDist,
  outNearestTime,
  pBarTitle="Referral analysis"
  ){


  #
  # set increment for the progress bar.
  #
  incN=0
  incTot = nrow(inputTableHf)
  inc=100/nrow(incTot)

  #
  # set output table label
  #
  hIdField <- paste0('from','__',amSubPunct(idField)) # amSubPunt to avoid unwanted char (accent, ponctuation..)
  hLabelField <- paste0('from','__',amSubPunct(labelField))
  hIdFieldTo <- paste0('to','__',amSubPunct(idFieldTo))
  hLabelFieldTo <- paste0('to','__',amSubPunct(labelFieldTo))
  hIdFieldNearest <-  paste0('nearest','__',amSubPunct(idFieldTo))
  hLabelFieldNearest <-  paste0('nearest','__',amSubPunct(labelFieldTo))
  hDistUnit <-paste0('distance','_',unitDist)
  hTimeUnit <- paste0('time','_',unitCost)

  #
  # set identifier columns
  #
  idCol <- config$vectorKey
  idColTo <- paste0(config$vectorKey,"_to")


  #
  # Create destination HF subset "TO". 
  #
  qSqlTo <- sprintf(" %1$s IN ( %2$s )",
    idCol,
    paste0(inputTableHfTo[[idCol]],collapse=',')
    )
  execGRASS("v.extract",flags=c('overwrite'),input=inputHfTo,where=qSqlTo,output='tmp_ref_to')

  amTimer("start")

  #
  # Send progress state. Here, first message
  #
  pbc(
    visible = TRUE,
    timeOut = 2,
    percent = 1,
    title   = pBarTitle,
    text    = sprintf("Compute referral for %s facilities, please be patient. Click on stop button to interrupt."
      , incTot
      )
    )

  # cost and dist from one to all selected in table 'to'
  for(i in inputTableHf[[idCol]]){  

    incN=incN+1

    pbc(
      visible = TRUE,
      timeOut = 0.001,
      percent = (incN-1)*inc,
      title   = pBarTitle,
      text    = sprintf("Extract vector data. %s. %s/%s"
        , amTimer()
        , incN
        , incTot
        )
      )

    # create temporary origine facility map (from) 
    qSqlFrom <- sprintf("%s==%s",
      idCol,
      i
      )
    execGRASS("v.extract",flags=c('overwrite'),input=inputHf,where=qSqlFrom,output='tmp__ref_from')

    #
    # create cumulative cost map for each hf : iso or aniso
    # 
    pbc(
      visible = TRUE,
      timeOut = 0.001,
      percent = (incN-1)*inc,
      title   = pBarTitle,
      text    = sprintf("Compute travel time. %s. %s/%s"
        , amTimer()
        , incN
        , incTot
        )
      )

    switch(typeAnalysis,
      'anisotropic'=amAnisotropicTravelTime(
        inputSpeed=inputSpeed,
        inputHf='tmp__ref_from',
        inputStop='tmp_ref_to',
        outputCumulative='tmp__cost', 
        outputDir='tmp__ref_dir',
        returnPath=FALSE,
        maxCost=0
        ),
      'isotropic'=amIsotropicTravelTime(
        inputFriction=inputFriction,
        inputHf='tmp__ref_from',
        inputStop='tmp_ref_to',
        outputCumulative='tmp__cost',
        outputDir='tmp__ref_dir',
        maxCost=0
        )
      )

    #
    # extract time cost V1 = hf id dest; V2 = time to reach hf
    #
    pbc(
      visible = TRUE,
      timeOut = 0.001,
      percent = (incN-1)*inc,
      title   = pBarTitle,
      text    = sprintf("Extract travel time. %s. %s/%s"
        , amTimer()
        , incN
        , incTot
        )
      )

    refTime=execGRASS(
      'v.what.rast',
      map='tmp_ref_to',
      raster='tmp__cost',
      flags='p',
      intern=T
      )%>%
    gsub('\\*',NA,.) %>%
    na.omit %>%
    read.table(text=.,sep='|')


  # rename grass output
  names(refTime)<-c(idColTo,hTimeUnit)
  #unit transformation 
  if(!unitCost =='m'){
    div<-switch(unitCost,
      's'=1/60,
      'm'=1,
      'h'=60,
      'd'=24
      )
    refTime[hTimeUnit]<-refTime[hTimeUnit]/div
  }

  refTime[[config$vectorKey]]=i

  #
  # extract distance
  #
  pbc(
    visible = TRUE,
    timeOut = 0.001,
    percent = (incN-1)*inc,
    title   = pBarTitle,
    text    = sprintf("Compute least cost path. %s. %s/%s"
      , amTimer()
      , incN
      , incTot
      )
    )
  # least cost path using direction and cost
  execGRASS('r.drain',
    input='tmp__cost',
    direction='tmp__ref_dir',
    output='tmp__drain',
    drain='tmp__drain',
    flags=c('overwrite','c','d'),
    start_points='tmp_ref_to'
    )
  # create new layer with start point as node
  pbc(
    visible = TRUE,
    timeOut = 0.001,
    percent = (incN-1)*inc,
    title   = pBarTitle,
    text    = sprintf("Build vector network. %s. %s/%s"
      , amTimer()
      , incN
      , incTot
      )
    )
  execGRASS('v.net',
    input='tmp__drain',
    points='tmp__ref_from',
    output='tmp__net_from',
    node_layer='2',
    operation='connect',
    threshold=resol-1,
    flags='overwrite'
    )
  # create new layer with stop points as node
  execGRASS('v.net',
    input='tmp__net_from',
    points='tmp_ref_to',
    output='tmp__net_all',
    node_layer='3',
    operation='connect',
    threshold=resol-1,
    flags='overwrite'
    )
  # extract distance for each end node.
  pbc(
    visible = TRUE,
    timeOut = 0.001,
    percent = (incN-1)*inc,
    title   = pBarTitle,
    text    = sprintf("Calculate distances. %s. %s/%s"
      , amTimer()
      , incN
      , incTot
      )
    )

  execGRASS('v.net.distance',
    input='tmp__net_all',
    output='tmp__net_dist',
    from_layer='3', # calc distance from all node in 3 to layer 2 (start point)     
    to_layer='2',
    intern=T,
    flags='overwrite'
    )

  #
  # read attribute table of distance network.
  #
  pbc(
    visible = TRUE,
    timeOut = 0.001,
    percent = (incN-1)*inc,
    title   = pBarTitle,
    text    = sprintf("Extract result and agreggate. %s. %s/%s"
      , amTimer()
      , incN
      , incTot
      )
    )

  refDist <- dbReadTable(dbCon,'tmp__net_dist')
  # rename grass output
  names(refDist)<-c(idColTo,idCol,hDistUnit)
  # distance conversion
  if(!unitDist=='m'){
    div<-switch(unitDist,
      'km'=1000
      )
    refDist[,hDistUnit]<-refDist[,hDistUnit]/div
  }

  # using data.table.
  refTime <- as.data.table(refTime)
  setkeyv( refTime, cols=c( idCol, idColTo ) )
  refDist<-as.data.table(refDist)
  setkeyv( refDist, cols=c( idCol, idColTo ) )
  
  refTimeDist <- refDist[refTime]

  #create or update table
  if(incN==1){
    tblRef <- refTimeDist
  }else{
    tblRef <- rbind(tblRef,refTimeDist)
  }

  pbc(
    visible = TRUE,
    timeOut = 0.001,
    percent = 100,
    title   = pBarTitle,
    text    = "Cleaning temp files"
    )

  # remove tmp map
  rmRastIfExists('tmp__*')
  rmVectIfExists('tmp__*')

  } # end of loop

  pbc(
    visible = TRUE,
    timeOut = 0.001,
    percent = 100,
    title   = pBarTitle,
    text    = sprintf("Referral analysis done in %s. Creation of output tables."
      , amTimer()
      )
    )
  # set key to ref
  setkeyv( tblRef, cols=c( idCol, idColTo ) )

  # Remove tmp map
  rmVectIfExists('tmp_*')

  # mergin from hf subset table and renaming.
  valFrom<-inputTableHf[inputTableHf[[config$vectorKey]] %in% tblRef[[config$vectorKey]], c(config$vectorKey,idField,labelField)]
  names(valFrom) <- c(idCol,hIdField,hLabelField)
  valFrom <- as.data.table(valFrom)
  setkeyv(valFrom,cols=c(idCol))

  valTo<-inputTableHfTo[inputTableHfTo[[config$vectorKey]] %in% tblRef[[idColTo]],c(idCol,idFieldTo,labelFieldTo)]
  names(valTo)<-c(idColTo,hIdFieldTo,hLabelFieldTo)
  valTo<-as.data.table(valTo)
  setkeyv(valTo,cols=c(idColTo))
  setkeyv(tblRef,cols=c(idCol))
 
  tblRef<- tblRef[valFrom]
  setkeyv(tblRef,cols=c(idColTo))
  tblRef<- tblRef[valTo]


  # set column subset and order
  tblRefOut<-tblRef[,c(
    hIdField,
    hLabelField,
    hIdFieldTo,
    hLabelFieldTo,
    hDistUnit,
    hTimeUnit
    ),with=F]


  # set expression to evaluate nested query by group
  expD<-as.expression(sprintf(".SD[which.min(%s)]",hDistUnit))
  expT<-as.expression(sprintf(".SD[which.min(%s)]",hTimeUnit))

  # exclude time or dist == 0
  expD0<-as.expression(sprintf("%s>0",hDistUnit))
  expT0<-as.expression(sprintf("%s>0",hTimeUnit))

  # subset and select. Try to figure why variable can't be used as columns name
  tblRefNearestDist<-eval(parse(
    text=sprintf(
      "tblRefOut[%1$s>0,.SD[which.min(%1$s)],by=%2$s]"
      , hDistUnit
      , hIdField
      )
    ))
   tblRefNearestTime <-eval(parse(
    text=sprintf(
      "tblRefOut[%1$s>0,.SD[which.min(%1$s)],by=%2$s]"
      , hTimeUnit
      , hIdField
      )
    ))
    #tblRefOut[expD0,expD,by=hIdField]
  #tblRefNearestTime<-tblRefOut[eval(expT0),eval(expT),by=hIdField,with=TRUE]

  # Return meta data
  meta<-list(
    'Function'='amReferralTable',
    'AccessMod revision'=amGetAppVersionLocal(),
    'Date'=amSysTime(),
    'Iterations'=nrow(inputTableHf),
    'Arguments'=list(
      'input'=list(
        'map'=list(
          'cost'=list(
            'speed'=inputSpeed,
            'friction'=inputFriction
            ),
          'facilities'=list(
            'from'=inputHf,
            'to'=inputHfTo
            )
          ),
        'table'=list(
          'id'=list(
            'from'=inputTableHf[[config$vectorKey]],
            'to'=inputTableHfTo[[config$vectorKey]]
            ),
          'names'=list(
            'from'=names(inputTableHf),
            'to'=names(inputTableHfTo)
            )
          )
        ),
      'analysis'=typeAnalysis,
      'unit'=list(
        'distance'=unitDist,
        'cost'=unitCost
        ),
      'resol'=resol
      ),
    'Output'=list(
      outReferral,
      outNearestDist,
      outNearestTime
      ) 
    )

  dbWriteTable(dbCon,outReferral,tblRefOut,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestDist,tblRefNearestDist,overwrite=T,row.names=F)
  dbWriteTable(dbCon,outNearestTime,tblRefNearestTime,overwrite=T,row.names=F)


  pbc(
    visible = FALSE
    )

}


