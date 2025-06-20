#         ___                                  __  ___            __   ______
#        /   |  _____ _____ ___   _____ _____ /  |/  /____   ____/ /  / ____/
#       / /| | / ___// ___// _ \ / ___// ___// /|_/ // __ \ / __  /  /___ \
#      / ___ |/ /__ / /__ /  __/(__  )(__  )/ /  / // /_/ // /_/ /  ____/ /
#     /_/  |_|\___/ \___/ \___//____//____//_/  /_/ \____/ \__,_/  /_____/
#
#    AccessMod 5 Supporting Universal Health Coverage by modelling physical accessibility to health care
#
#    Copyright (c) 2014-present WHO, Frederic Moser (GeoHealth group, University of Geneva)
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

# Common validation functions for AccessMod modules

#' Main validation dispatcher
#'
#' Routes validation to appropriate module-specific functions while handling
#' common validation requirements shared across modules.
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateCurrentModule <- function(ctx) {
  # Initialize result structure
  result <- list(
    err = character(0),
    info = character(0),
    dubious = character(0)
  )

  # Common validations (for modules 2,3,4,6)
  if (ctx$module != "module_5") {
    common_result <- amValidateCommonRequirements(ctx)

    result$err <- c(result$err, common_result$err)
    result$info <- c(result$info, common_result$info)
    result$dubious <- c(result$dubious, common_result$dubious)
  }

  # Module-specific validations - now clean and explicit!
  module_result <- switch(ctx$module,
    "module_2" = amValidateModule2(ctx),
    "module_3" = amValidateModule3(ctx),
    "module_4" = amValidateModule4(ctx),
    "module_5" = amValidateModule5(ctx),
    "module_6" = amValidateModule6(ctx),
    "module_7" = amValidateModule7(ctx), # Easy to add!
    list(err = character(0), info = character(0), dubious = character(0))
  )

  # Combine results
  result$err <- c(result$err, module_result$err)
  result$info <- c(result$info, module_result$info)
  result$dubious <- c(result$dubious, module_result$dubious)

  return(result)
}

#' Common validation requirements
#'
#' Validates requirements shared across multiple modules (2,3,4,6)
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateCommonRequirements <- function(ctx) {
  err <- character(0)
  info <- character(0)
  dubious <- character(0)

  # Data layer validations
  data_layer_result <- amValidateDataLayers(
    merged_select = ctx$merged_select,
    hf_select = ctx$hf_select,
    data_list = ctx$data_list
  )
  err <- c(err, data_layer_result$err)

  # Travel time validation
  travel_time_result <- amValidateTravelTime(
    max_travel_time = ctx$max_travel_time
  )
  err <- c(err, travel_time_result$err)
  info <- c(info, travel_time_result$info)

  # Facility validation
  facility_result <- amValidateFacilities(
    tbl_hf_subset = ctx$tbl_hf_subset
  )
  err <- c(err, facility_result$err)

  # Tag validation
  tag_result <- amValidateTags(
    cost_tag = ctx$cost_tag
  )
  err <- c(err, tag_result$err)

  # Speed table validation
  speed_result <- amValidateSpeedTable(
    tbl_speed_raster = ctx$tbl_speed_raster
  )
  info <- c(info, speed_result$info)

  return(list(
    err = err,
    info = info,
    dubious = dubious
  ))
}

#' Data layer validation
#'
#' Validates existence of required data layers
#'
#' @param merged_select Selected merged land cover layer
#' @param hf_select Selected health facility layer
#' @param data_list Available data layers
#' @return List with err vector
amValidateDataLayers <- function(merged_select, hf_select, data_list) {
  err <- character(0)

  merged_exists <- !is.null(amNameCheck(data_list, merged_select, "raster"))
  if (!merged_exists) {
    err <- c(err, ams("srv_analysis_accessibility_missing_merged_lc_warning"))
  }

  hf_exists <- !is.null(amNameCheck(data_list, hf_select, "vector"))
  if (!hf_exists) {
    err <- c(err, ams("srv_analysis_accessibility_missing_facility_layer"))
  }

  return(list(err = err))
}

#' Travel time validation
#'
#' Validates travel time input parameters
#'
#' @param max_travel_time Maximum travel time setting
#' @return List with err and info vectors
amValidateTravelTime <- function(max_travel_time) {
  err <- character(0)
  info <- character(0)

  wrong_tt <- !is.numeric(max_travel_time) ||
    isEmpty(max_travel_time) ||
    max_travel_time < 0 ||
    max_travel_time > 2147483647

  if (wrong_tt) {
    err <- c(err, ams("srv_analysis_accessibility_max_travel_time_input"))
  }

  unlimited_tt <- isTRUE(max_travel_time == 0)
  if (unlimited_tt) {
    info <- c(info, ams("srv_analysis_accessibility_max_travel_time_set_0min"))
  }

  return(list(
    err = err,
    info = info
  ))
}

#' Facility validation
#'
#' Validates health facility data quality
#'
#' @param tbl_hf_subset Health facility subset table
#' @return List with err vector
amValidateFacilities <- function(tbl_hf_subset) {
  err <- character(0)

  if (isEmpty(tbl_hf_subset)) {
    return(list(err = err))
  }

  hf_on_barrier <- any(tbl_hf_subset$amOnBarrier == "yes")
  if (hf_on_barrier) {
    err <- c(err, ams("srv_analysis_accessibility_facilities_on_barrier"))
  }

  hf_on_zero <- any(tbl_hf_subset$amOnZero == "yes")
  if (hf_on_zero) {
    err <- c(err, ams("srv_analysis_accessibility_facilities_on_0kmh"))
  }

  hf_outside_dem <- any(tbl_hf_subset$amOutsideDem == "yes")
  if (hf_outside_dem) {
    err <- c(err, ams("srv_analysis_accessibility_facilities_outside_dem"))
  }

  return(list(err = err))
}

#' Tag validation
#'
#' Validates cost tags for output naming
#'
#' @param cost_tag Cost tags input
#' @return List with err vector
amValidateTags <- function(cost_tag) {
  err <- character(0)

  tags_clean <- amGetUniqueTags(cost_tag)
  tags_valid <- isTRUE(length(tags_clean) > 0)

  if (!tags_valid) {
    err <- c(err, ams("srv_analysis_accessibility_add_tag_instruction"))
  }

  return(list(err = err))
}

#' Speed table validation
#'
#' Validates speed raster table and provides information about zero speeds
#'
#' @param tbl_speed_raster Speed raster table
#' @return List with info vector
amValidateSpeedTable <- function(tbl_speed_raster) {
  info <- character(0)

  if (isEmpty(tbl_speed_raster)) {
    return(list(info = info))
  }

  tbl_speed_has_zero <- isTRUE(0 %in% tbl_speed_raster$speed)
  if (tbl_speed_has_zero) {
    info <- c(info, ams("srv_analysis_accessibility_tbl_speed_has_zero"))
  }

  return(list(info = info))
}
