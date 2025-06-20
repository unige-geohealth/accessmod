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

# Module-specific validation functions for AccessMod

#' Module 2 validation (Accessibility Analysis)
#'
#' Validates inputs specific to accessibility analysis module
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateModule2 <- function(ctx) {
  err <- character(0)
  info <- character(0)

  add_nearest <- ctx$input_show_advanced_tools && ctx$input_check_with_nearest
  missing_nearest_idx <- add_nearest && isEmpty(ctx$input_hf_idx_int_field)

  if (missing_nearest_idx) {
    err <- c(err, ams("srv_analysis_accessibility_missing_nearest_idx"))
  }

  hf_no_selected <- !any(ctx$tbl_hf_subset$amSelect)
  if (hf_no_selected) {
    err <- c(err, ams("srv_analysis_accessibility_select_facilities"))
  }

  return(list(
    err = err,
    info = info,
    dubious = character(0)
  ))
}

#' Module 3 validation (Coverage Analysis)
#'
#' Validates inputs specific to coverage analysis module
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateModule3 <- function(ctx) {
  err <- character(0)
  info <- character(0)

  # Parameter validation
  hf_idx <- isTRUE(nchar(ctx$input_hf_idx_field) > 0)
  cap_field <- isTRUE(nchar(ctx$input_hf_capacity_field) > 0)
  hf_buffer <- isTRUE(ctx$input_hf_order == "circBuffer")
  pop_buffer <- isTRUE(ctx$input_pop_buffer_radius > ctx$listen$mapMeta$grid$nsres)
  zonal_pop <- isTRUE("zonalPop" %in% ctx$input_mod3param)
  ignore_capacity <- isTRUE("ignoreCapacity" %in% ctx$input_mod3param)
  pop_barrier <- isTRUE("popBarrier" %in% ctx$input_mod3param)
  pop_rm <- isTRUE("rmPop" %in% ctx$input_mod3param)
  hf_order_inconsistency <- isTRUE(ctx$input_hf_order != "tableOrder" && pop_rm)
  zonal_coverage_inconsistency <- isTRUE(zonal_pop && pop_rm)
  capacities <- ctx$tbl_hf_subset[[ctx$input_hf_capacity_field]]
  capacities_not_valid <- any(is.na(capacities)) || any(is.null(capacities))
  hf_no_selected <- !any(ctx$tbl_hf_subset$amSelect)
  has_pop_barrier <- isTRUE(ctx$pop_on_barrier_stat$sum > 0)

  # Population layer validation
  if (!ctx$pop_select) {
    err <- c(err, ams("srv_analysis_accessibility_select_population"))
  }

  # Health facility validations
  if (!hf_idx) {
    err <- c(err, ams("srv_analysis_accessibility_no_group_warning"))
  }

  if (hf_no_selected) {
    err <- c(err, ams("srv_analysis_accessibility_select_one_facility_warning"))
  }

  # Capacity validation
  if (!cap_field && !ignore_capacity) {
    err <- c(err, ams("srv_analysis_accessibility_set_capacity_warning"))
  }

  if (!ignore_capacity && cap_field) {
    if (capacities_not_valid) {
      err <- c(err, ams("srv_analysis_accessibility_capacities_not_valid"))
    }
  }

  # Buffer validation
  if (hf_buffer && !pop_buffer) {
    err <- c(err, ams("srv_analysis_accessibility_circular_buffer_warning"))
  }

  # Information messages
  if (!pop_barrier) {
    info <- c(info, ams("srv_analysis_accessibility_no_pop_barrier_warning"))
  }

  if (hf_order_inconsistency) {
    info <- c(info, ams("srv_analysis_accessibility_facilities_processing_order"))
  }

  # Zonal population validation
  if (zonal_pop) {
    zonal_select <- isTRUE(!is.null(amNameCheck(ctx$data_list, ctx$input_zone_select, "vector")))
    zone_id <- isTRUE(length(ctx$input_zone_id) > 0)
    zone_label <- isTRUE(length(ctx$input_zone_label) > 0)

    if (!zonal_select) {
      err <- c(err, ams("srv_analysis_accessibility_select_zone_warning"))
    }
    if (!zone_id) {
      err <- c(err, ams("srv_analysis_accessibility_zonal_id_missing"))
    }
    if (!zone_label) {
      err <- c(err, ams("srv_analysis_accessibility_zonal_label_missing"))
    }
  }

  if (zonal_coverage_inconsistency) {
    err <- c(err, ams("srv_analysis_accessibility_remove_covered_pop_warning"))
  }

  # Population on barrier information
  if (isEmpty(err) && has_pop_barrier) {
    info <- c(info, sprintf(
      ams("srv_analysis_accessibility_pop_on_barrier_removed"),
      ctx$pop_on_barrier_stat$cells,
      ctx$pop_on_barrier_stat$sum,
      ctx$pop_on_barrier_stat$percent
    ))
  }

  return(list(
    err = err,
    info = info,
    dubious = character(0)
  ))
}

#' Module 4 validation (Referral Analysis)
#'
#' Validates inputs specific to referral analysis module
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateModule4 <- function(ctx) {
  err <- character(0)
  info <- character(0)

  hf_no_selected <- !any(ctx$tbl_hf_subset$amSelect)
  if (hf_no_selected) {
    err <- c(err, ams("srv_analysis_accessibility_select_one_facility_from"))
  }

  hf_no_selected_to <- !any(ctx$tbl_hf_subset_to$amSelect)
  if (hf_no_selected_to) {
    err <- c(err, ams("srv_analysis_accessibility_select_one_facility_to"))
  }

  # Check barriers for "to" facilities as well
  if (!isEmpty(ctx$tbl_hf_subset_to)) {
    hf_on_barrier_to <- any(ctx$tbl_hf_subset_to$amOnBarrier == "yes")
    hf_on_zero_to <- any(ctx$tbl_hf_subset_to$amOnZero == "yes")
    hf_outside_dem_to <- any(ctx$tbl_hf_subset_to$amOutsideDem == "yes")

    if (hf_on_barrier_to) {
      err <- c(err, ams("srv_analysis_accessibility_facilities_on_barrier"))
    }
    if (hf_on_zero_to) {
      err <- c(err, ams("srv_analysis_accessibility_facilities_on_0kmh"))
    }
    if (hf_outside_dem_to) {
      err <- c(err, ams("srv_analysis_accessibility_facilities_outside_dem"))
    }
  }

  return(list(
    err = err,
    info = info,
    dubious = character(0)
  ))
}

#' Module 5 validation (Zonal Analysis)
#'
#' Validates inputs specific to zonal analysis module
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateModule5 <- function(ctx) {
  err <- character(0)
  info <- character(0)

  # Check if data layers exist
  layer_ok_tt <- !is.null(amNameCheck(ctx$data_list, ctx$travel_time_select, "raster"))
  layer_ok_zones <- !is.null(amNameCheck(ctx$data_list, ctx$zone_select, "vector"))
  layer_ok_pop <- !is.null(amNameCheck(ctx$data_list, ctx$pop_select, "raster"))

  if (!layer_ok_tt) {
    err <- c(err, ams("srv_analysis_accessibility_missing_travel_time"))
  }

  if (!layer_ok_zones) {
    err <- c(err, ams("srv_analysis_accessibility_missing_zone"))
  }

  if (!layer_ok_pop) {
    err <- c(err, ams("srv_analysis_accessibility_missing_population"))
  }

  # Travel time range validation
  if (layer_ok_tt) {
    tt_validation_result <- amValidateModule5TravelTimeRange(
      travel_time_select = ctx$travel_time_select,
      text_time_cum_costs = ctx$text_time_cum_costs
    )
    err <- c(err, tt_validation_result$err)
  }

  return(list(
    err = err,
    info = info,
    dubious = character(0)
  ))
}

#' Module 5 travel time range validation
#'
#' Validates travel time range inputs for zonal analysis
#'
#' @param travel_time_select Travel time layer selection
#' @param text_time_cum_costs Travel time cumulative costs text
#' @return List with err vector
amValidateModule5TravelTimeRange <- function(travel_time_select, text_time_cum_costs) {
  err <- character(0)

  max_tt <- ceiling(amGetRasterStat_cached(travel_time_select, c("max")))
  min_tt <- floor(amGetRasterStat_cached(travel_time_select, c("min")))
  select_tt <- amSplitToNum(text_time_cum_costs, default = NULL)
  tt_in_range <- isNotEmpty(select_tt) && all(select_tt <= max_tt & select_tt > min_tt)

  tt_zero <- isTRUE(max_tt == 0)
  if (tt_zero) {
    err <- c(err, ams("srv_analysis_accessibility_travel_time_input_zero"))
    return(list(err = err))
  }

  if (!tt_in_range) {
    err <- c(err, sprintf(
      ams("srv_analysis_accessibility_travel_time_input"),
      min_tt + 1,
      max_tt
    ))
  }

  return(list(err = err))
}

#' Module 6 validation (Scaling Up Analysis)
#'
#' Validates inputs specific to scaling up analysis module
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateModule6 <- function(ctx) {
  err <- character(0)
  info <- character(0)
  dubious <- character(0)

  without_facility <- isTRUE(ctx$use_existing_hf == "FALSE")
  pop_residual_is_residual <- isTRUE(amGetClass(ctx$pop_residual_select) == "rPopulationResidual")

  # Population layer validation
  if (!ctx$pop_select) {
    err <- c(err, ams("srv_analysis_accessibility_select_population"))
  }

  # Capacity table validation
  if (isNotEmpty(ctx$capacity_table_data)) {
    capacity_result <- amValidateModule6CapacityTable(ctx$capacity_table_data)
    err <- c(err, capacity_result$err)
    info <- c(info, capacity_result$info)
  }

  # Suitability table validation
  if (isNotEmpty(ctx$suitability_table_data)) {
    suitability_result <- amValidateModule6SuitabilityTable(
      suitability_table_data = ctx$suitability_table_data,
      data_list = ctx$data_list,
      without_facility = without_facility,
      hf_select = ctx$hf_select,
      tbl_hf_subset = ctx$tbl_hf_subset,
      config = ctx$config
    )
    err <- c(err, suitability_result$err)
  }

  # Exclusion table validation
  if (isNotEmpty(ctx$exclusion_table_data)) {
    exclusion_result <- amValidateModule6ExclusionTable(
      exclusion_table_data = ctx$exclusion_table_data,
      data_list = ctx$data_list,
      config = ctx$config
    )
    err <- c(err, exclusion_result$err)
  }

  # Facility selection validation
  hf_exists <- isNotEmpty(amNameCheck(ctx$data_list, ctx$hf_select, "vector"))
  hf_no_selected <- !any(ctx$tbl_hf_subset$amSelect)

  if (!without_facility && hf_no_selected) {
    err <- c(err, ams("srv_analysis_accessibility_select_facilities"))
  }

  # Population residual validation
  if (!hf_no_selected && !pop_residual_is_residual) {
    dubious <- c(dubious, ams("srv_analysis_accessibility_facilities_residual_pop_warning"))
  }

  if (hf_no_selected && pop_residual_is_residual) {
    dubious <- c(dubious, ams("srv_analysis_accessibility_residual_pop_no_facilities"))
  }

  # Scale up limits information
  max_sc_up_hf_no_limit <- isTRUE(ctx$max_sc_up_new_hf < 1)
  max_sc_up_time_no_limit <- isTRUE(ctx$max_sc_up_time < 1)
  max_sc_up_pop_goal_no_limit <- isTRUE(ctx$max_sc_up_pop_goal < 1)

  all_sc_up_no_limit <- all(c(
    max_sc_up_pop_goal_no_limit,
    max_sc_up_hf_no_limit,
    max_sc_up_time_no_limit
  ))

  if (all_sc_up_no_limit) {
    info <- c(info, ams("srv_analysis_accessibility_scaling_up_unlimited"))
  } else {
    if (max_sc_up_pop_goal_no_limit) {
      info <- c(info, ams("srv_analysis_accessibility_coverage_100percent"))
    }
    if (max_sc_up_time_no_limit) {
      info <- c(info, ams("srv_analysis_accessibility_processing_unlimited"))
    }
    if (max_sc_up_hf_no_limit) {
      info <- c(info, ams("srv_analysis_accessibility_new_facilities_unlimited"))
    }
  }

  # Additional information
  if (!without_facility) {
    info <- c(info, ams("srv_analysis_accessibility_selected_facilities_verification"))
  }

  tbl_cap_without_but_hf_select <- without_facility && !hf_no_selected && hf_exists
  if (tbl_cap_without_but_hf_select) {
    info <- c(info, ams("srv_analysis_accessibility_start_with_empty_layer_warning"))
  }

  return(list(
    err = err,
    info = info,
    dubious = dubious
  ))
}

#' Module 6 capacity table validation
#'
#' Validates capacity table data for scaling up analysis
#'
#' @param capacity_table_data Capacity table data
#' @return List with err and info vectors
amValidateModule6CapacityTable <- function(capacity_table_data) {
  err <- character(0)
  info <- character(0)

  # Check for missing values
  tbl_cap_missing_ok <- all(sapply(capacity_table_data, function(x) {
    all(stringr::str_length(x) > 0)
  }))

  if (!tbl_cap_missing_ok) {
    err <- c(err, ams("srv_analysis_accessibility_scaleup_table_missing_value"))
    return(list(err = err, info = info))
  }

  # Type validation
  tbl_cap_type_ok <- all(
    is.numeric(capacity_table_data$min),
    is.numeric(capacity_table_data$max),
    is.numeric(capacity_table_data$capacity),
    is.character(capacity_table_data$label)
  )

  if (!tbl_cap_type_ok) {
    err <- c(err, ams("srv_analysis_accessibility_scaleup_table_type_error"))
    return(list(err = err, info = info))
  }

  # Min/Max validation
  tbl_cap_min_max_ok <- all(capacity_table_data$min < capacity_table_data$max)
  if (!tbl_cap_min_max_ok) {
    err <- c(err, ams("srv_analysis_accessibility_scaleup_table_min_max_equality"))
  }

  # First row should start with 0
  tbl_cap_begin_with_zero <- isTRUE(capacity_table_data$min[1] == 0)
  if (!tbl_cap_begin_with_zero) {
    err <- c(err, ams("srv_analysis_accessibility_scaleup_table_first_min_value_0"))
  }

  # Capacity in range validation
  tbl_cap_in_range_ok <- isTRUE(
    all(capacity_table_data$capacity <= capacity_table_data$max &
      capacity_table_data$capacity >= capacity_table_data$min)
  )
  if (!tbl_cap_in_range_ok) {
    info <- c(info, ams("srv_analysis_accessibility_scaleup_values_not_in_range"))
  }

  # Unique labels validation
  tbl_cap_label_ok <- isTRUE(length(unique(capacity_table_data$label)) ==
    length(capacity_table_data$label))
  if (!tbl_cap_label_ok) {
    err <- c(err, ams("srv_analysis_accessibility_scaleup_capacity_duplicate_labels"))
  }

  # Sequential validation for multiple rows
  n_r <- nrow(capacity_table_data)
  if (n_r > 1) {
    for (i in 2:n_r) {
      # Capacity is greater than previous capacity
      tbl_cap_greater_than_prev_ok <- isTRUE(
        capacity_table_data[i, "capacity"] > capacity_table_data[i - 1, "capacity"]
      )
      if (!tbl_cap_greater_than_prev_ok) {
        err <- c(err, ams("srv_analysis_accessibility_scaleup_capacity_not_incremental"))
        break
      }

      # Min > previous max (no overlap)
      tbl_cap_overlap_ok <- isTRUE(
        capacity_table_data[i, "min"] > capacity_table_data[i - 1, "max"]
      )
      if (!tbl_cap_overlap_ok) {
        err <- c(err, ams("srv_analysis_accessibility_scaleup_min_value_greater_previous_max"))
        break
      }
    }
  }

  return(list(err = err, info = info))
}

#' Module 6 suitability table validation
#'
#' Validates suitability table data for scaling up analysis
#'
#' @param suitability_table_data Suitability table data
#' @param data_list Available data layers
#' @param without_facility Without facility flag
#' @param hf_select Health facility layer selection
#' @param tbl_hf_subset Health facility subset table
#' @param config Configuration object
#' @return List with err vector
amValidateModule6SuitabilityTable <- function(suitability_table_data,
  data_list,
  without_facility,
  hf_select,
  tbl_hf_subset,
  config) {
  err <- character(0)

  tbl_suit_ok <- nrow(na.omit(suitability_table_data)) > 0
  if (!tbl_suit_ok) {
    err <- c(err, ams("srv_analysis_accessibility_suitability_table_missing_value"))
    return(list(err = err))
  }

  # Check for dynamic facilities only scenario
  hf_exists <- !is.null(amNameCheck(data_list, hf_select, "vector"))
  hf_no_selected <- !any(tbl_hf_subset$amSelect)

  tbl_suit_only_dyn_fac <- without_facility &&
    all(suitability_table_data$layer == config$dynamicFacilities) &&
    !hf_no_selected &&
    hf_exists

  if (tbl_suit_only_dyn_fac) {
    err <- c(err, ams("srv_analysis_accessibility_suitability_table_add_non_dynamic_layer"))
  }

  # Validate layer names
  suit_layers <- suitability_table_data$layer[!suitability_table_data$layer %in% config$dynamicLayers]
  tbl_suit_layer_missing <- suit_layers[!sapply(suit_layers, amMapExists)]

  if (length(tbl_suit_layer_missing) > 0) {
    err <- c(err, sprintf(
      ams("srv_analysis_accessibility_suitability_table_missing_layer"),
      paste(tbl_suit_layer_missing, collapse = ", ")
    ))
  }

  return(list(err = err))
}

#' Module 6 exclusion table validation
#'
#' Validates exclusion table data for scaling up analysis
#'
#' @param exclusion_table_data Exclusion table data
#' @param data_list Available data layers
#' @param config Configuration object
#' @return List with err vector
amValidateModule6ExclusionTable <- function(exclusion_table_data,
  data_list,
  config) {
  err <- character(0)

  # Validate layer names
  excl_layers <- exclusion_table_data$layer[!exclusion_table_data$layer %in% config$dynamicLayers]
  tbl_excl_layer_missing <- excl_layers[!sapply(excl_layers, amMapExists)]

  if (length(tbl_excl_layer_missing) > 0) {
    err <- c(err, sprintf(
      ams("srv_analysis_accessibility_exclusion_table_missing_layer"),
      paste(tbl_excl_layer_missing, collapse = ", ")
    ))
  }

  return(list(err = err))
}

#' Module 7 validation (Future Module)
#'
#' Placeholder validation for future module 7
#'
#' @param ctx Validation context environment containing all required data
#' @return List with err, info, and dubious message vectors
amValidateModule7 <- function(ctx) {
  # Module 7 specific validation logic
  # Easy to add without modifying existing code!

  err <- character(0)
  info <- character(0)
  dubious <- character(0)

  # Add module 7 validation rules here when needed

  return(list(
    err = err,
    info = info,
    dubious = dubious
  ))
}
