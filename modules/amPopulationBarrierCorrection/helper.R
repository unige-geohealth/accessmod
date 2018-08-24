#######################################################################
#      ___                                  __  ___            __   ______
#     /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#    / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#   / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#  /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \____/  /_____/
#
# Population on barrier redistribution over polygons
#
# Authors : Carlos Ochoa (1), Fred Moser (2) 
#
# (1) Adaptation from AccessMod doc annex procedure
# (2) Conversion to AccessMod module
#
#######################################################################


#' Redistribute population on barrier on selected zones 
#' 
#' @param inputBorder {Character} name zonal vector layer
#' @param inputPopulation {Character} name of the population layer
#' @param inputLandCover {Character} name of the landcover layer
#' @param outputPopulation {Chraceter} name of the redistributed population
#' @param progressCallback {Function} A function with two parameters : 
#'        percent {Numeric} to update progress bar. 
#'        message {Chracter} progress message.
#' @return stat {List} Result/Stat lists
amPopulationBarrierCorrection <- function(
  inputBorder, 
  inputPopulation, 
  inputLandCover, 
  outputPopulation = "tmp_pop",
  progressCallback = function(percent,message){
    print(
      sprintf("(%1$s%%) %2$s"
        , round(percent*100)
        , message
        )
      )
  }
  ){
 

  start <- Sys.time()
  end <- Sys.time()
  nSteps <- 10
  prog <- progressCallback 
  
  pp <- function(n){
    ceiling((n/nSteps)*100)
  }

  result <- list(
    timing = 0,
    popOrig = 0,
    popOnBarrier = 0,
    popFinal = 0,
    popDiff = 0
    )

  tmpConf <- list(
    border = amRandomName("tmp_divisions"),
    borderRaster = amRandomName("tmp_border"),
    partPopRaster = amRandomName("tmp_pop_part")
    )

  tryCatch(finally = {
    rmRastIfExists("tmp_*")
    rmVectIfExists("tmp_*")
      prog(
        percent = 100,
        message = "Done"
        )
    }, {

      #
      # Intial stuff
      #
      prog(
        percent = pp(1),
        message = "Count initial population"
        )
      result$popOrig <- amGetRasterStat(inputPopulation,'sum')

      #
      # Don't alter the original border
      # dataset, create a temporary one instead
      #
      prog(
        percent = pp(2),
        message  = "Copy zones"
        )

      execGRASS("g.copy",
        flags = "overwrite",
        vector = c(
          inputBorder
          , tmpConf$border
          )
        )

      #
      # Computation of the partial population.
      #
      prog(
        percent = pp(3) ,
        message  = "Remove population on barrier"
        )

      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = if(isnull(%2$s), null(), %3$s)"
          , tmpConf$partPopRaster
          , inputLandCover
          , inputPopulation
          )
        )
      result$popOnBarrier <- result$popOrig - amGetRasterStat(tmpConf$partPopRaster,'sum')

      #
      # New column for the sub-national divisions shp file.
      #
       prog(
        percent = pp(4),
        message  = "Add ratio column in temporary vector"
        )
      execGRASS("v.db.addcolumn",
        map = tmpConf$border,
        columns = "am_pop_ratio"
        )

      #
      # Compute full population values per area 
      #
       prog(
        percent = pp(5),
        message  = "Get zonal stats : sum of population per zone"
        )
      execGRASS("v.rast.stats",
        map = tmpConf$border,
        raster = inputPopulation, 
        column_prefix = "am_pop_full",
        method = "sum"
        )

      #
      # Compute partial population values per area
      #
       prog(
        percent = pp(6),
        message  = "Get zonal stats : sum of partial population per zone"
        )
      execGRASS("v.rast.stats",
        map = tmpConf$border,
        raster = tmpConf$partPopRaster,
        column_prefix = "am_pop_part",
        method = "sum"
        )

      #
      # Compute the Ratio between full and partial populations
      #
       prog(
        percent = pp(7),
        message  = "Calculate ratio column"
        )
      execGRASS("v.db.update",
        map = tmpConf$border,
        layer = c("1"),
        column = "am_pop_ratio", 
        query_column =  "am_pop_full_sum/am_pop_part_sum"
        )

      #
      # Rasterize the divisions shp file based on the Ratio value
      #
       prog(
        percent = pp(8),
        message  = "Rasterize zones using ratio as values"
        )
      execGRASS("v.to.rast",
        flags = "overwrite",
        input = tmpConf$border,
        output = tmpConf$borderRaster,
        use = "attr",
        attribute_column = "am_pop_ratio"
        )

      #
      # Compute the final adjusted population
      #
      prog(
        percent = pp(9) ,
        message  = "Calculate new distributed population"
        )
      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = %2$s * %3$s"
          , outputPopulation
          , tmpConf$partPopRaster
          , tmpConf$borderRaster
          )
        )

      end <- Sys.time()
      result$popFinal <-  amGetRasterStat(outputPopulation,'sum')
      result$popDiff <- result$popOrig - result$popFinal
      result$timing <-  round(difftime(end,start,units="m"),3)

      prog(
        percent = pp(10),
        message  = "Done"
        )
    })

  return(result)

}
