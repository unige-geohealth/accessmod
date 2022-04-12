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

amZonalAnalysis <- function(inputTravelTime,
                            inputPop,
                            inputZone,
                            timeCumCosts,
                            zoneIdField,
                            zoneLabelField) {
  pBarTitle <- ams("analysis_zonal_stat_title")

  on_exit_add({
    pbc(
      id = "zonalStat",
      visible = FALSE,
    )
  })


  res <- list(
    table = data.frame(
      id                = "-",
      label             = "-",
      time_m            = "-",
      popTotal          = "-",
      popTravelTime     = "-",
      popCoveredPercent = "-"
    ),
    empty = TRUE
  )

  inputZoneTemp <- sprintf(
    "tmp_zones_%s",
    digest::digest(c(
      inputZone,
      zoneLabelField,
      zoneIdField
    ))
  )

  if (!amRastExists(inputZoneTemp)) {
    #
    # Create raster version of admin zone.
    #
    execGRASS("v.to.rast",
      input = inputZone,
      output = inputZoneTemp,
      type = "area",
      use = "attr",
      label_column = zoneLabelField,
      attribute_column = zoneIdField,
      flags = "overwrite"
    )
  }


  validCost <- all(timeCumCosts > 0)
  hasZone <- !is.null(inputZone)
  hasPop <- !is.null(inputPop)
  checkTempZone <- execGRASS("g.list",
    type = "raster",
    pattern = inputZoneTemp,
    intern = T
  )
  hasTempZone <- isTRUE(inputZoneTemp == checkTempZone)

  if (validCost && hasZone && hasPop && hasTempZone) {
    res$empty <- FALSE
    timeCumCosts <- unique(timeCumCosts)
    timeCumCosts <- sort(timeCumCosts)
    lTimeCumCosts <- length(timeCumCosts)
    for (i in 1:lTimeCumCosts) {
      pbc(
        id = "zonalStat",
        visible = TRUE,
        title = pBarTitle,
        percent = ((i - 1) / lTimeCumCosts) * 100,
        text = sprintf(
          ams("analysis_zonal_progress"),
          i - 1,
          lTimeCumCosts
        )
      )

      cost <- timeCumCosts[i]
      #
      # extract population under coverage area ignore negative.
      #
      popUnderTravelTime <- sprintf(
        "tmp__pop_under_travel_time = ( %1$s >= 0 && %1$s < %2$s ) ? %3$s : null()",
        inputTravelTime,
        cost,
        inputPop
      )

      execGRASS("r.mapcalc",
        expression = popUnderTravelTime,
        flags = "overwrite"
      )

      statZonePopTravelTime <- execGRASS("r.univar",
        map    = "tmp__pop_under_travel_time",
        zones  = inputZoneTemp,
        flags  = c("g", "t"),
        intern = T
      ) %>%
        amCleanTableFromGrass(cols = c("zone", "label", "sum"))

      statZonePopTotal <- execGRASS("r.univar",
        map    = inputPop,
        zones  = inputZoneTemp,
        flags  = c("g", "t"),
        intern = T
      ) %>%
        amCleanTableFromGrass(cols = c("zone", "label", "sum"))

      statZoneMerge <- merge(
        statZonePopTotal,
        statZonePopTravelTime,
        by = c("zone", "label"),
        all.x = TRUE
      )

      names(statZoneMerge) <- c(
        zoneIdField,
        zoneLabelField,
        "popTotal",
        "popTravelTime"
      )

      #
      # Compute percentage
      #
      statZoneMerge$popCoveredPercent <- (
        statZoneMerge$popTravelTime / statZoneMerge$popTotal
      ) * 100

      #
      # Replace na by zero
      #
      statZoneMerge[is.na(statZoneMerge)] <- 0

      #
      # Add costs (if multiple costs)
      #
      statZoneMerge$time_m <- cost

      #
      # Re-order
      #
      statZoneMerge <- statZoneMerge[, c(
        zoneIdField,
        zoneLabelField,
        "time_m",
        "popTotal",
        "popTravelTime",
        "popCoveredPercent"
      )]


      if (i == 1) {
        res$table <- statZoneMerge[order(statZoneMerge$popCoveredPercent), ]
      } else {
        res$table <- rbind(
          res$table,
          statZoneMerge[order(statZoneMerge$popCoveredPercent), ]
        )
      }
    }
  }

  return(res)
}
