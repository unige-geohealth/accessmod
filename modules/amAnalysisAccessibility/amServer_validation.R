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

# Load modular validation functions
# Module validation observer - refactored to use modular functions
idModule <- "module_analysis"

observe(
  {
    amErrorAction(title = "Module validation", {
      # === REACTIVE INPUT ACCESS (maintains reactive graph) ===
      current_module <- input$moduleSelector
      module_2 <- isTRUE(current_module == "module_2")
      module_3 <- isTRUE(current_module == "module_3")
      module_4 <- isTRUE(current_module == "module_4")
      module_5 <- isTRUE(current_module == "module_5")
      module_6 <- isTRUE(current_module == "module_6")

      # Common reactive inputs
      merged_select <- input$mergedSelect
      hf_select <- input$hfSelect
      max_travel_time <- input$maxTravelTime
      cost_tag <- input$costTag

      # Analysis type flags
      is_anisotropic <- isTRUE(input$typeAnalysis == "anisotropic")
      is_isotropic <- isTRUE(input$typeAnalysis == "isotropic")

      # All reactive inputs
      # Module 2 inputs
      input_show_advanced_tools <- isTRUE(input$showAdvancedTools)
      input_check_with_nearest <- isTRUE(input$checkWithNearest)
      input_hf_idx_int_field <- input$hfIdxIntField

      # Module 3 inputs
      input_hf_idx_field <- input$hfIdxField
      input_hf_capacity_field <- input$hfCapacityField
      input_hf_order <- input$hfOrder
      input_pop_buffer_radius <- input$popBufferRadius
      input_mod3param <- input$mod3param
      input_zone_select <- input$zoneSelect
      input_zone_id <- input$zoneId
      input_zone_label <- input$zoneLabel
      pop_select <- amNameCheck(dataList, input$popSelect, "raster")

      # Module 4 inputs
      tbl_hf_subset_to <- tblHfSubsetTo()
      ref_limit_closest <- isTRUE(input$checkReferralLimitClosest)
      ref_keep_net_dist_layer <- isTRUE(input$checkReferralKeepNetwork)

      # Module 5 inputs
      travel_time_select <- input$travelTimeSelect
      zone_select <- input$zoneSelect
      text_time_cum_costs <- input$textTimeCumCosts

      # Module 6 inputs
      capacity_table_data <- tabulator_to_df(input$capacityTable_data)
      exclusion_table_data <- tabulator_to_df(input$exclusionTable_data)
      suitability_table_data <- tabulator_to_df(input$suitabilityTable_data)
      use_existing_hf <- isTRUE(input$useExistingHf)
      max_sc_up_new_hf <- input$maxScUpNewHf
      max_sc_up_time <- input$maxScUpTime
      max_sc_up_pop_goal <- input$maxScUpPopGoal
      max_sc_up_pop_goal_over <- isTRUE(max_sc_up_pop_goal > 100)
      pop_residual_select <- input$popResidualSelect



      # === VALIDATION LOGIC (using context environment) ===
      # Create validation context environment
      ctx <- new.env()
      
      # Core module and common data
      ctx$module <- current_module
      ctx$merged_select <- merged_select
      ctx$hf_select <- hf_select
      ctx$max_travel_time <- max_travel_time
      ctx$cost_tag <- cost_tag
      ctx$data_list <- dataList
      ctx$tbl_hf_subset <- tblHfSubset()
      ctx$tbl_speed_raster <- tblSpeedRaster()
      
      # Module 2 inputs
      ctx$input_show_advanced_tools <- input_show_advanced_tools
      ctx$input_check_with_nearest <- input_check_with_nearest
      ctx$input_hf_idx_int_field <- input_hf_idx_int_field
      
      # Module 3 inputs
      ctx$input_hf_idx_field <- input_hf_idx_field
      ctx$input_hf_capacity_field <- input_hf_capacity_field
      ctx$input_hf_order <- input_hf_order
      ctx$input_pop_buffer_radius <- input_pop_buffer_radius
      ctx$input_mod3param <- input_mod3param
      ctx$input_zone_select <- input_zone_select
      ctx$input_zone_id <- input_zone_id
      ctx$input_zone_label <- input_zone_label
      ctx$pop_select <- pop_select
      ctx$listen <- listen
      ctx$pop_on_barrier_stat <- popOnBarrierStat()
      
      # Module 4 inputs
      ctx$tbl_hf_subset_to <- tbl_hf_subset_to
      
      # Module 5 inputs
      ctx$travel_time_select <- travel_time_select
      ctx$zone_select <- zone_select
      ctx$text_time_cum_costs <- text_time_cum_costs
      
      # Module 6 inputs
      ctx$capacity_table_data <- capacity_table_data
      ctx$exclusion_table_data <- exclusion_table_data
      ctx$suitability_table_data <- suitability_table_data
      ctx$use_existing_hf <- use_existing_hf
      ctx$pop_residual_select <- pop_residual_select
      ctx$max_sc_up_new_hf <- max_sc_up_new_hf
      ctx$max_sc_up_time <- max_sc_up_time
      ctx$max_sc_up_pop_goal <- max_sc_up_pop_goal
      
      # Configuration
      ctx$config <- config
      
      # Call validation with clean context
      validation_result <- amValidateCurrentModule(ctx)

      # Add module-specific travel time info messages
      unlimited_tt <- isTRUE(max_travel_time == 0)
      if (!module_5) {
        tt_info <- amGetTravelTimeInfoMessages(current_module, unlimited_tt)
        validation_result$info <- c(validation_result$info, tt_info)
      }


      # === MESSAGE GENERATION AND UI UPDATES ===


      # Auto correction for Module 6
      if (module_6 && max_sc_up_pop_goal_over) {
        updateNumericInput(session, "maxScUpPopGoal", value = 100)
        max_sc_up_pop_goal <- 100
      }

      disable_btn <- length(validation_result$err) > 0

      # Generate resource and output information for successful validations
      resource_output_info <- list(info = character(0), out = character(0))

      generate_message <- !disable_btn && current_module != "module_5"


      if (generate_message) {
        # Clean tags for output naming
        tags_clean <- amGetUniqueTags(cost_tag)

        # Determine analysis flags
        add_nearest <- module_2 && input_show_advanced_tools && input_check_with_nearest
        zonal_pop <- module_3 && isTRUE("zonalPop" %in% input_mod3param)
        pop_barrier <- module_3 && isTRUE("popBarrier" %in% input_mod3param)

        resource_output_info <- amGenerateResourceAndOutputInfo(
          module = current_module,
          hf_select = hf_select,
          tags_clean = tags_clean,
          data_list = dataList,
          is_anisotropic = is_anisotropic,
          is_isotropic = is_isotropic,
          add_nearest = add_nearest,
          zonal_pop = zonal_pop,
          pop_barrier = pop_barrier,
          ref_limit_closest = ref_limit_closest,
          ref_keep_net_dist_layer = ref_keep_net_dist_layer
        )

        # Store output names for analysis launch
        if (length(resource_output_info$out) > 0) {
          class_mod <- amGetModuleOutputClasses(
            module = current_module,
            is_anisotropic = is_anisotropic,
            is_isotropic = is_isotropic,
            add_nearest = add_nearest,
            zonal_pop = zonal_pop,
            pop_barrier = pop_barrier,
            ref_limit_closest = ref_limit_closest,
            ref_keep_net_dist_layer = ref_keep_net_dist_layer
          )

          if (length(class_mod) > 0) {
            v_names <- amCreateNames(class_mod, tags_clean, dataList)
            listen$outputNames <- v_names
          }
        }
      }

      # Combine all info messages
      all_info <- c(validation_result$info, resource_output_info$info)

      # Format messages for UI
      msg_list <- amFormatValidationMessages(
        err = validation_result$err,
        info = all_info,
        dubious = validation_result$dubious
      )

      # Add output information if available
      if (length(resource_output_info$out) > 0) {
        msg_list <- tagList(msg_list, resource_output_info$out)
      }

      # Update UI elements
      amActionButtonToggle(
        session = session,
        "btnComputeAccessibility",
        disable = disable_btn
      )
      amActionButtonToggle(
        session = session,
        "btnZonalStat",
        disable = disable_btn
      )

      # update messages
      output$msgModule3 <- renderUI({
        msg_list
      })
    })
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "validate_accessibility")

observe(
  {
    travel_time_select <- input$travelTimeSelect
    module_5 <- isTRUE(input$moduleSelector == "module_5")
    has_travel_time <- amNameCheck(dataList, travel_time_select, "raster")
    if (module_5 && has_travel_time) {
      max_tt <- ceiling(amGetRasterStat_cached(travel_time_select, c("max")))
      min_tt <- floor(amGetRasterStat_cached(travel_time_select, c("min")))

      amUpdateText("txtZonalMinMax",
        text = sprintf(
          "min=%1$s, max=%2$s",
          min_tt + 1,
          max_tt
        )
      )
    }
  },
  suspended = TRUE
) %>% amStoreObs(idModule, "update_zonal_min_max")
