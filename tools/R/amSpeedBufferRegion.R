#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#    
#    Copyright (c) 2014-2020  WHO, Frederic Moser (GeoHealth group, University of Geneva)
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    vTopo <- read.table(
      text = execGRASS('v.info',
        map = tmpInput,
        flags = c('t'),
        intern = TRUE
        ),
      sep = '='
    )
    nPoints = vTopo[vTopo$V1 == 'points',]$V2
    if(nPoints >= 3){
      execGRASS(
        'v.hull',
        flags = c('overwrite','f'),
        input = tmpInput,
        output = tmpHull
      )
    }else{
      tmpHull = tmpInput
    }
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


