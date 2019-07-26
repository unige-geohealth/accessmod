amSpeedBufferRegionInit <- function(inputVector,maxSpeed=0,maxTime=0){
  radius = maxSpeed * maxTime;

  if(!amNoDataCheck(radius) && radius > 0){
    execGRASS(
      'v.buffer',
      input = inputVector,
      output = 'tmp_speed_buffer_mask',
      distance = radius
      )
    execGRASS(
      'g.region',
      vector = 'tmp_speed_buffer_mask'
      )
  }
}

amSpeedBufferRegionRestore <- function(){
  hasBuffer <- amVectExists('tmp_speed_buffer_mask')
  if(hasBuffer){
    execGRASS(
      'g.region',
      raster=config$mapDem
      )
    rmVectIfExists('tmp_speed_buffer_mask')
  }
}


