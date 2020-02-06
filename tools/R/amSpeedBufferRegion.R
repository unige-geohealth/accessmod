
tmpMask <- "tmp_speed_buffer_mask"
tmpInput <- "tmp_speed_buffer_combined"
tmpHull <- "tmp_speed_buffer_hull"

amSpeedBufferRegionInit <- function(inputVector,maxSpeed=0,maxTime=0){
  radius = maxSpeed * maxTime;

  nCellsBefore <- amMapMeta()$grid$cells 

  if(!amNoDataCheck(radius) && radius > 0){
    if(length(inputVector) > 1){
      execGRASS(
        'v.patch',
        flags = c('overwrite'),
        input = inputVector,
        output = tmpInput
      )
      on.exit({
        rmVectIfExists(tmpInput)
      })
    }else{
      tmpInput <- inputVector
    }
    execGRASS(
      'v.hull',
      flags = c('overwrite','f'),
      input = tmpInput,
      output = tmpHull
    )
    execGRASS(
      'v.buffer',
      input = tmpHull,
      flags = c('s','overwrite'),
      output = tmpMask,
      distance = radius
    )
    amRegionSet(vectors = tmpMask)

    nCellsAfter <- amMapMeta()$grid$cells 

    if(nCellsAfter > nCellsBefore){
       amSpeedBufferRegionRestore()
    }
  }
}

amSpeedBufferRegionRestore <- function(){
  amRegionReset()
  rmVectIfExists('tmp_speed_buffer*')
}


