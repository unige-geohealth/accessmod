#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/




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

  validCost <- timeCumCost > 0
  hasZone <- !is.null(inputZone)
  hasPop <- !is.null(inputPop)
  checkTempZone <- execGRASS('g.list',type='raster',pattern=inputZoneTemp,intern=T)
  hasTempZone <- isTRUE(inputZoneTemp == checkTempZone )

  if( validCost && hasZone && hasPop && hasTempZone ){
    #
    # extract population under coverage area ignore negative.
    #
    popUnderTravelTime <- sprintf(
      "tmp__pop_under_travel_time = ( %1$s >= 0 && %1$s <= %2$s ? %3$s : null()",
      inputTravelTime,
      timeCumCost,
      inputPop
      )

    execGRASS('r.mapcalc',expression=popUnderTravelTime,flags='overwrite')

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

    statZoneMerge<-merge(
      statZonePopTotal,
      statZonePopTravelTime,
      by=c('zone','label'),
      all.x=TRUE
      )

    names(statZoneMerge)<-c(zoneIdField,zoneLabelField,'popTotal','popTravelTime')

    statZoneMerge$popCoveredPercent<-(statZoneMerge$popTravelTime/statZoneMerge$popTotal)*100

    statZoneMerge[is.na(statZoneMerge)]<-0

    res$table <- statZoneMerge[order(statZoneMerge$popCoveredPercent),]
  }
    return(res)
}
