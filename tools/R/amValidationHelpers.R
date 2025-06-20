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

# Message formatting and utility functions for validation

#' Format validation messages for UI display
#'
#' Converts validation result vectors into HTML formatted messages
#' for display in the Shiny UI
#'
#' @param err Error message vector
#' @param info Information message vector
#' @param dubious Dubious/warning message vector
#' @return tagList object for UI rendering
amFormatValidationMessages <- function(err, info, dubious) {
  msg_list <- character(0)

  if (length(err) > 0) {
    plur <- if (length(err) > 1) "s" else ""
    err_html <- HTML(paste("<div>",
      icon("exclamation-triangle"),
      err,
      "</div>",
      collapse = ""
    ))
    msg_list <- tagList(
      tags$b(sprintf(ams("srv_analysis_accessibility_validation_issues"), plur)),
      err_html
    )
  }

  if (length(info) > 0) {
    info_html <- HTML(paste("<div>",
      icon("info-circle"),
      info,
      "</div>",
      collapse = ""
    ))
    msg_list <- tagList(
      msg_list,
      tags$b(ams("srv_analysis_accessibility_validation_information")),
      info_html
    )
  }

  if (length(dubious) > 0) {
    dubious_html <- HTML(paste("<div>",
      icon("question-circle"),
      dubious,
      "</div>",
      collapse = ""
    ))
    msg_list <- tagList(
      msg_list,
      tags$b("Information:"),
      dubious_html
    )
  }

  return(msg_list)
}

#' Generate resource estimation and output naming information
#'
#' Creates resource estimation messages and output dataset information
#' for modules that require it (not module 5)
#'
#' @param module Current module identifier
#' @param hf_select Health facility layer selection
#' @param tags_clean Cleaned cost tags
#' @param data_list Available data layers
#' @param is_anisotropic Anisotropic analysis flag
#' @param is_isotropic Isotropic analysis flag
#' @param add_nearest Add nearest facility flag
#' @param zonal_pop Zonal population flag
#' @param pop_barrier Population barrier flag
#' @param ref_limit_closest Referral limit closest flag
#' @param ref_keep_net_dist_layer Keep network distance layer flag
#' @return List with info messages and output HTML
amGenerateResourceAndOutputInfo <- function(
  module,
  hf_select,
  tags_clean,
  data_list,
  is_anisotropic = FALSE,
  is_isotropic = FALSE,
  add_nearest = FALSE,
  zonal_pop = FALSE,
  pop_barrier = FALSE,
  ref_limit_closest = FALSE,
  ref_keep_net_dist_layer = FALSE) {
  info <- character(0)
  out <- character(0)

  # Resource estimation (not for module 5)
  if (module != "module_5") {
    r_est <- amGetRessourceEstimate(hf_select)
    r_required <- r_est$required
    r_available <- r_est$available

    info <- c(info, sprintf(
      ams("srv_analysis_accessibility_estimate_required_memory"),
      r_required$memory,
      r_available$memory
    ))

    info <- c(info, sprintf(
      ams("srv_analysis_accessibility_estimate_disk_space"),
      r_required$disk,
      r_available$disk
    ))

    # Generate output dataset names
    class_mod <- amGetModuleOutputClasses(
      module = module,
      is_anisotropic = is_anisotropic,
      is_isotropic = is_isotropic,
      add_nearest = add_nearest,
      zonal_pop = zonal_pop,
      pop_barrier = pop_barrier,
      ref_limit_closest = ref_limit_closest,
      ref_keep_net_dist_layer = ref_keep_net_dist_layer
    )

    if (length(class_mod) > 0) {
      v_names <- amCreateNames(class_mod, tags_clean, data_list)

      out <- tagList(
        tags$b(ams("srv_analysis_accessibility_validation_output_dataset")),
        HTML(paste("<div>",
          icon("sign-out-alt"),
          v_names$html,
          "<div/>",
          collapse = ""
        ))
      )
    }
  }

  return(list(
    info = info,
    out = out
  ))
}

#' Get module output classes
#'
#' Determines the output classes that will be generated for each module
#'
#' @param module Current module identifier
#' @param is_anisotropic Anisotropic analysis flag
#' @param is_isotropic Isotropic analysis flag
#' @param add_nearest Add nearest facility flag
#' @param zonal_pop Zonal population flag
#' @param pop_barrier Population barrier flag
#' @param ref_limit_closest Referral limit closest flag
#' @param ref_keep_net_dist_layer Keep network distance layer flag
#' @return Character vector of output classes
amGetModuleOutputClasses <- function(module,
  is_anisotropic = FALSE,
  is_isotropic = FALSE,
  add_nearest = FALSE,
  zonal_pop = FALSE,
  pop_barrier = FALSE,
  ref_limit_closest = FALSE,
  ref_keep_net_dist_layer = FALSE) {
  class_mod <- character(0)

  switch(module,
    "module_2" = {
      class_mod <- c(
        "tScenarioOut",
        if (is_anisotropic) "rSpeed",
        if (is_isotropic) "rFriction",
        "rTravelTime",
        if (add_nearest) "rNearest",
        "lAnalysisParameters"
      )
    },
    "module_3" = {
      class_mod <- c(
        "tScenarioOut",
        if (is_anisotropic) "rSpeed",
        if (is_isotropic) "rFriction",
        "tCapacityStat",
        if (zonal_pop) "tZonalStat",
        "rPopulationResidual",
        if (pop_barrier) "rPopulationOnBarrier",
        "vCatchment",
        "lAnalysisParameters"
      )
    },
    "module_4" = {
      class_mod <- c(
        "tScenarioOut",
        if (is_anisotropic) "rSpeed",
        if (is_isotropic) "rFriction",
        "tReferral",
        if (!ref_limit_closest) "tReferralDist",
        "tReferralTime",
        if (ref_keep_net_dist_layer) "vReferralNetwork",
        "lAnalysisParameters"
      )
    },
    "module_5" = {
      class_mod <- c()
    },
    "module_6" = {
      class_mod <- c(
        "tScenarioOut",
        "rSpeed",
        "rFriction",
        "rPopulationResidual",
        "vFacilityNew",
        "tCapacityOut",
        "tCapacityStatNew",
        "vCatchmentNew",
        "tExclusionOut",
        "tSuitabilityOut",
        "lAnalysisParameters"
      )
    }
  )

  # Remove NULL values
  class_mod <- class_mod[!is.null(class_mod)]

  return(class_mod)
}

#' Validate module-specific travel time information messages
#'
#' Generates module-specific information messages about travel time settings
#'
#' @param module Current module identifier
#' @param unlimited_tt Unlimited travel time flag
#' @return Character vector of info messages
amGetTravelTimeInfoMessages <- function(module, unlimited_tt) {
  info <- character(0)

  if (unlimited_tt && module != "module_4") {
    info <- c(info, ams("srv_analysis_accessibility_max_travel_time_set_0min"))
  }

  if (unlimited_tt && module == "module_2") {
    info <- c(info, ams("srv_analysis_accessibility_max_travel_time_warning"))
  }

  if (unlimited_tt && !module %in% c("module_2", "module_4")) {
    info <- c(info, ams("srv_analysis_accessibility_travel_time_>32727_ignored"))
  }

  return(info)
}
