amSpeedBufferRegionInit <- function(inputVector,maxSpeed=0,maxTime=0){
  radius = maxSpeed * maxTime;

  if(!amNoDataCheck(radius) && radius > 0){
    
    tmpMask <- "tmp_speed_buffer_mask"
    tmpInput <- "tmp_speed_buffer_combined"

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
      'v.buffer',
      input = tmpInput,
      output = 'tmp_speed_buffer_mask',
      distance = radius
      )
    amRegionSet(vectors = 'tmp_speed_buffer_mask')
  }
}

amSpeedBufferRegionRestore <- function(){
  hasBuffer <- amVectExists('tmp_speed_buffer_mask')
  if(hasBuffer){
    amRegionReset()
    rmVectIfExists('tmp_speed_buffer_mask')
  }
}


