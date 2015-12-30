




amZonalAnalysis <- function(
  inputTravelTime,
  inputPop,
  inputZone,
  inputZoneTemp,
  timeCumCost,
  zoneIdField,
  zoneLabelField,
  resolution,
  nColumns,
  mapDem
  ){



  res <- list(
    plot = NULL,
    table= NULL
    )



  if(timeCumCost>0 && !is.null(inputZone) && !is.null(inputPop)){
    tmpZoneExists<-isTRUE(inputZoneTemp == execGRASS('g.list',type='raster',pattern=inputZoneTemp,intern=T))
    if(tmpZoneExists){


      # reclass travel time to keep only the slider value
      tmpRulesFile<-tempfile()
      tmpRules<-paste(
        "0 thru", timeCumCost, "=" ,timeCumCost, "travelTime"
        )
      write(tmpRules,tmpRulesFile)
      execGRASS('r.reclass',
        input=inputTravelTime,
        output='tmp__cum_cost',
        rules=tmpRulesFile,
        flags='overwrite'
        )
      # extract population under coverage area.
      execGRASS('r.mapcalc',expression=paste(
          'tmp__pop_under_travel_time=if(tmp__cum_cost==',timeCumCost,',',inputPop,',null())'
          ),flags='overwrite')

      # produce map
      tmpMapTt<-file.path(tempdir(),'mapTravelTime.tiff')
    
      execGRASS('g.region',res=paste(resolution*nColumns/500))
      exp=paste('tmp__cum_cost_preview=if(',inputTravelTime,'<',timeCumCost,',',inputTravelTime,',null())')
      execGRASS('r.mapcalc',expression=exp,flags='overwrite')
      execGRASS('r.out.gdal',
        flags =c('overwrite'),
        input='tmp__cum_cost_preview',
        output=tmpMapTt,
        format="GTiff",
        createopt='TFW=YES'
        )
      execGRASS('g.region',raster=mapDem)
      rTt<-raster(tmpMapTt)

      ## labels
      labeltext = paste("Travel time",timeCumCost,"[min]")

      ## plot
      res$plot <- rTt 
       
      statZonePopTravelTime<-read.table(text=
        execGRASS('r.univar',
          map='tmp__pop_under_travel_time',
          zones=inputZoneTemp,
          flags=c('g','t'),
          intern=T
          ),sep='|',header=T
        )[,c('zone','label','sum')]

      statZonePopTotal<-read.table(text=
        execGRASS('r.univar',
          map=inputPop,
          zones=inputZoneTemp,
          flags=c('g','t'),
          intern=T
          ),sep='|',header=T
        )[,c('zone','label','sum')]

      statZoneMerge<-merge(statZonePopTotal,statZonePopTravelTime,by=c('zone','label'),all.x=TRUE)
      names(statZoneMerge)<-c(zoneIdField,zoneLabelField,'popTotal','popTravelTime')
      statZoneMerge$popCoveredPercent<-(statZoneMerge$popTravelTime/statZoneMerge$popTotal)*100
      statZoneMerge[is.na(statZoneMerge)]<-0
      res$table <- statZoneMerge[order(statZoneMerge$popCoveredPercent),]
    }
  }
    return(res)
}
