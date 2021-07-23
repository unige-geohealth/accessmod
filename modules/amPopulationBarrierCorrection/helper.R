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


# Population on barrier redistribution over polygons
#
# Authors : Carlos Ochoa (1), Fred Moser (2) 
#
# (1) Adaptation from AccessMod doc annex procedure
# (2) Conversion to AccessMod module
#


#' Redistribute population on barrier on selected zones 
#' 
#' @param modePopKnown {Boolean} The final population by zone is known : a fields of the zones
#' @param inputZone {Character} name zonal vector layer
#' @param inputPopulation {Character} name of the population layer
#' @param inputLandCover {Character} name of the landcover layer
#' @param inputPopulationColumn {Character} name of the population field in zones layer (modePopKnow only)
#' @param outputPopulation {Chraceter} name of the redistributed population
#' @param progressCallback {Function} A function with two parameters : 
#'        percent {Numeric} to update progress bar. 
#'        message {Chracter} progress message.
#' @return stat {List} Result/Stat lists
amPopulationBarrierCorrection <- function(
  modePopKnown = FALSE,
  inputZone, 
  inputPopulation, 
  inputLandCover,
  inputPopulationColumn = NULL,
  outputPopulation = "tmp_pop",
  outputSummary = "tmp_pop_cor_summary",
  progressCallback = function(percent, message){
    print(
      sprintf("(%1$s%%) %2$s"
        , round(percent*100)
        , message
        )
      )
  },
  dbCon 
  ){
 

  start <- Sys.time()
  end <- Sys.time()
  nSteps <- 12
  prog <- progressCallback 
  
  pp <- function(n){
    ceiling((n/nSteps)*100)
  }

  result <- list()

  tmpConf <- list(
    zone = amRandomName("tmp_divisions"),
    zoneRasterRatio = amRandomName("tmp_zone_ratio"),
    partPopRaster = amRandomName("tmp_pop_part"),
    partPopRasterZone = amRandomName("tmp_pop_part_zone"),
    barrierPopRasterAfter = amRandomName("tmp_pop_barrier_after"),
    popOrig = amRandomName("tmp_pop_orig"),
    ldcOrig = amRandomName("tmp_ldc_orig")
    )

  tryCatch(finally = {
    rmRastIfExists("tmp_*")
    rmVectIfExists("tmp_*")
      prog(
        percent = 100,
        message = ams("helper_pop_correction_process_done")
        )
    }, {

      #
      # Intial stuff
      #
      prog(
        percent = pp(1),
        message = ams("helper_pop_correction_copy_original_ldc_pop")
        )
      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = %2$s"
          , tmpConf$popOrig
          , inputPopulation
          )
        )
      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = %2$s"
          , tmpConf$ldcOrig
          , inputLandCover
          )
        )
      #
      # Don't alter the original zone
      # dataset, create a temporary one instead
      #
      prog(
        percent = pp(2),
        message = ams("helper_pop_correction_copy_zones")
        )

      execGRASS("g.copy",
        flags = "overwrite",
        vector = c(
          inputZone
          , tmpConf$zone
          )
        )

      #
      # Computation of the partial population.
      #
      prog(
        percent = pp(3) ,
        message  = ams("helper_pop_correction_calculate_pop_barrier")
        )

      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = if(isnull(%2$s), null(), %3$s)"
          , tmpConf$partPopRaster
          , tmpConf$ldcOrig
          , tmpConf$popOrig
          )
        )

      #
      # New column for the sub-national divisions shp file.
      #
      prog(
        percent = pp(4),
        message  = ams("helper_pop_correction_add_ratio_column")
        )
      execGRASS("v.db.addcolumn",
        map = tmpConf$zone,
        columns = "am_pop_ratio double precision"
        )

      #
      # Compute full population values per area 
      #
      prog(
        percent = pp(5),
        message  = ams("helper_pop_correction_zonal_statistics_full")
        )
      execGRASS("v.rast.stats",
        map = tmpConf$zone,
        raster = tmpConf$popOrig, 
        column_prefix = "am_pop_full",
        method = "sum"
        )
      
      #
      # Compute partial population values per area
      #
      prog(
        percent = pp(6),
        message  = ams("helper_pop_correction_zonal_statistics_partial")
        )
      execGRASS("v.rast.stats",
        map = tmpConf$zone,
        raster = tmpConf$partPopRaster,
        column_prefix = "am_pop_part",
        method = "sum"
        )

      #
      # Compute the Ratio between full and partial populations
      #
      prog(
        percent = pp(7),
        message  = ams("helper_pop_correction_calculate_ratio_column")
        )
      colFull = ifelse(modePopKnown,inputPopulationColumn,'am_pop_full_sum')
      execGRASS("v.db.update",
        map = tmpConf$zone,
        layer = c("1"),
        column = "am_pop_ratio", 
        query_column =  sprintf("%s/am_pop_part_sum",colFull)
        )

      #
      # Rasterize the divisions shp file based on the Ratio value
      #
      prog(
        percent = pp(8),
        message  = ams("helper_pop_correction_rasterize_ratio")
        )
      execGRASS("v.to.rast",
        flags = "overwrite",
        input = tmpConf$zone,
        output = tmpConf$zoneRasterRatio,
        use = "attr",
        attribute_column = "am_pop_ratio"
        )


      #
      # Compute the final adjusted population
      #
      prog(
        percent = pp(9) ,
        message  = ams("helper_pop_correction_calculate_new_pop")
        )
      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = %2$s * %3$s"
          , outputPopulation
          , tmpConf$partPopRaster
          , tmpConf$zoneRasterRatio
          )
        )

      #
      # Population outside zone
      # NOTE: Not needed step, as it should always be 'pop before - pop after'. Requested in #200
      #
      prog(
        percent = pp(10) ,
        message  = ams("helper_pop_correction_calculate_pop_outside_zone")
        )
      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = if(isnull(%2$s), %3$s, null())"
          , tmpConf$partPopRasterZone
          , tmpConf$zoneRasterRatio
          , tmpConf$popOrig
          )
        )

      #
      # Population on barrier after.
      # NOTE: Not needed step, as it should always be zero. Requested in #200
      #
      prog(
        percent = pp(11) ,
        message  = ams("helper_pop_correction_calculate_pop_barrier")
        )
      execGRASS("r.mapcalc",
        flags = "overwrite",
        expression = sprintf(
          "%1$s = if(isnull(%2$s), %3$s, null())"
          , tmpConf$barrierPopRasterAfter
          , tmpConf$ldcOrig
          , outputPopulation
          )
        )
      #
      # Summary table : cat, count, %barrier, count_after
      #
      result$tblSummary = dbGetQuery(dbCon,
        sprintf('
          SELECT cat, 
          am_pop_full_sum as pop_orig,
          am_pop_ratio as pop_ratio, 
          (am_pop_part_sum * am_pop_ratio) as pop_output
          %1$s
          FROM %2$s',
          ifelse(
          modePopKnown,
          sprintf(', %1$s as pop_known',inputPopulationColumn),
          ''
          ),
        tmpConf$zone
        )
      )
      nZones <- nrow(result$tblSummary)
      dbWriteTable(dbCon,outputSummary,result$tblSummary, overwrite=TRUE)
      end <- Sys.time()
      result$popOutsideZone <- amGetRasterStat(tmpConf$partPopRasterZone,'sum')
      result$popOrig <- amGetRasterStat(tmpConf$popOrig,'sum')
      result$popFinal <-  amGetRasterStat(outputPopulation,'sum')
      result$popOnBarrierBefore <- result$popOrig - result$popOutsideZone - amGetRasterStat(tmpConf$partPopRaster,'sum')
      result$popOnBarrierAfter <- amGetRasterStat(tmpConf$barrierPopRasterAfter,'sum')
      result$popDiff <- result$popOrig - result$popFinal
      result$timing <-  round(difftime(end,start,units="m"),3)
      result$nZonesWithoutPop <- nrow(result$tblSummary[is.na(result$tblSummary$pop_orig),])
      result$nZones <- nZones
      
      prog(
        percent = pp(12),
        message  = ams("helper_pop_correction_final_done")
        )
    })

  return(result)

}
