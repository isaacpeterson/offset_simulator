run_collate_routines <- function(simulation_outputs, feature_dynamics, feature_dynamics_modes, initial_feature_layer, simulation_params, feature_params, 
                                 global_params, landscape_dims, current_data_dir, file_prefix, use_offset_metric){
  

  
  intervention_pool = lapply(seq_along(simulation_outputs$interventions), function(i) simulation_outputs$interventions[[i]]$site_indexes)
  object_name_pool = unlist(lapply(seq_along(simulation_outputs$interventions), function(i) rep(names(simulation_outputs$interventions)[i], length(intervention_pool[[i]]))))
  intervention_pool = unlist(intervention_pool)
  intervention_yrs_pool = unlist(lapply(seq_along(simulation_outputs$interventions), function(i) simulation_outputs$interventions[[i]]$intervention_yrs))
  site_num_remaining_pool = unlist(lapply(seq_along(simulation_outputs$interventions), function(i) simulation_outputs$interventions[[i]]$site_num_remaining))
  
  projection_yrs_pool = lapply(seq(length(unlist(intervention_pool))), 
                               function(i) lapply(seq(simulation_params$feature_num), 
                                                  function(j) rep(list(intervention_yrs_pool[i]), 
                                                                  length(feature_dynamics_modes[[intervention_pool[i] ]][[j]])) ))
  
  background_projection_yrs_pool = lapply(seq_along(initial_feature_layer), 
                                          function(i) lapply(seq(simulation_params$feature_num), 
                                                             function(j) rep(list(1), length(feature_dynamics_modes[[i]][[j]])) ))  
  
  site_features_at_intervention_set = vector('list', length(initial_feature_layer))
  
  for (current_feature_ind in seq(simulation_params$feature_num)){
    site_features_at_intervention = build_site_features_at_intervention(length(initial_feature_layer), current_data_dir, intervention_pool, intervention_yrs_pool, simulation_params, current_feature_ind)
    site_features_at_intervention_set = lapply(seq_along(site_features_at_intervention_set), function(i) append(site_features_at_intervention_set[[i]], site_features_at_intervention[[i]]))
  }
  
  site_scale_cfacs = vector('list', length(initial_feature_layer))
  
  if (use_offset_metric == FALSE){
    flog.info('building counterfactuals over time series (this may take a while)')
  } else {
    flog.info('building user metric counterfactuals over time series (this may take a while)')
  }
  
  site_scale_cfacs[intervention_pool] = collate_cfacs(site_features_at_intervention_set[intervention_pool],
                                                      simulation_params, 
                                                      feature_params,
                                                      feature_dynamics[intervention_pool],
                                                      feature_dynamics_modes[intervention_pool],
                                                      projection_yrs_pool,
                                                      intervention_yrs_pool,
                                                      site_num_remaining_pool,
                                                      cfac_type = 'site_scale',
                                                      object_type = object_name_pool, 
                                                      use_cfac_type_in_sim = TRUE, 
                                                      condition_class_bounds = feature_params$condition_class_bounds, 
                                                      use_offset_metric)
  
  background_cfacs = collate_cfacs(initial_feature_layer,
                                   simulation_params, 
                                   feature_params,
                                   feature_dynamics,
                                   feature_dynamics_modes,
                                   background_projection_yrs_pool,
                                   intervention_yrs = rep(1, length(initial_feature_layer)),
                                   vector(),
                                   cfac_type = 'background',
                                   object_type = vector(), 
                                   use_cfac_type_in_sim = FALSE, 
                                   condition_class_bounds = feature_params$condition_class_bounds, 
                                   use_offset_metric)
  
  saveRDS(background_cfacs, paste0(file_prefix, '_background_cfacs.rds'))
  
  if (use_offset_metric == FALSE){
    features_to_collate = seq(simulation_params$feature_num)
  } else {
    features_to_collate = 1
  }
  
  summed_site_features_at_intervention = sum_sites(site_features_at_intervention_set, use_offset_metric, simulation_params$transform_params)
  
  if (global_params$save_output_raster == TRUE){
    
    
    for (feature_ind in features_to_collate){
      
      if (feature_ind == 1){
        element_index_grouped_by_condition_class = readRDS(paste0(global_params$simulation_inputs_folder, 'element_index_grouped_by_condition_class.rds'))
        current_raster_folder = paste0(current_data_dir, '/output_rasters/')
        
        if (!file.exists(current_raster_folder)){
          dir.create(current_raster_folder, recursive = TRUE)
        }
      }
    
      current_element_index_grouped_by_condition_class = lapply(seq_along(element_index_grouped_by_condition_class), function(i) element_index_grouped_by_condition_class[[i]][[feature_ind]])
      
      for (yr in 0:simulation_params$time_steps){
        
        if (use_offset_metric == FALSE){

          feature_layer_to_use = readRDS(paste0(current_data_dir, 'feature_', formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0"), 
                                                                           '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
        } else {

          feature_layer_to_use = readRDS(paste0(current_data_dir, 'metric_layer_', formatC(feature_ind, width = 3, format = "d", flag = "0"), '_yr_', 
                                                formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
        }
        
        feature_layer_to_use = lapply(seq_along(feature_layer_to_use), 
                                      function(i) lapply(seq_along(feature_layer_to_use[[i]]), function(j) as.matrix(feature_layer_to_use[[i]][[j]])))
        current_feature_layer = matrix(0, nrow = landscape_dims[1], ncol = landscape_dims[2])
        current_feature_layer[unlist(current_element_index_grouped_by_condition_class)] = unlist(feature_layer_to_use)
        current_feature_raster = raster(current_feature_layer)
        
        raster_filename = paste0(current_raster_folder, 'feature_', formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0"), 
                                 '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.tif')
        
        writeRaster(current_feature_raster, raster_filename, overwrite = TRUE)
      }
    }
  }
  
  for (feature_ind in features_to_collate){
    flog.info('collating feature %s', feature_ind)
    collated_object = list()
    if (use_offset_metric == FALSE){
      
      collated_object$site_scale_outcomes = sum_data_stack(current_data_dir, 
                                                           file_pattern = paste0('feature_', formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0")), 
                                                           simulation_params$time_steps)
      
      collated_object$summed_site_features_at_intervention = select_subset(summed_site_features_at_intervention, feature_ind)
      collated_object$site_scale_cfacs = select_subset(site_scale_cfacs, feature_ind)
      collated_object$background_cfacs = select_subset(background_cfacs, feature_ind)
    } else {
      collated_object$site_scale_outcomes = sum_data_stack(current_data_dir, file_pattern = paste0('metric_'), simulation_params$time_steps)
      collated_object$summed_site_features_at_intervention = summed_site_features_at_intervention
      collated_object$site_scale_cfacs = site_scale_cfacs
      collated_object$background_cfacs = background_cfacs
    }
    
    collated_object$site_scale_impacts = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                                         function(i) calc_site_scale_impacts(current_simulation_outputs = simulation_outputs$interventions[[i]], 
                                                                                             current_site_sets = simulation_outputs$index_object$site_indexes_used[[i]], 
                                                                                             site_scale_cfacs = collated_object$site_scale_cfacs, 
                                                                                             collated_object$summed_site_features_at_intervention, 
                                                                                             site_scale_outcomes = collated_object$site_scale_outcomes,  
                                                                                             collate_type = names(simulation_outputs$interventions)[[i]], 
                                                                                             simulation_params, 
                                                                                             feature_params, 
                                                                                             use_offset_metric)), names(simulation_outputs$interventions))
    
    collated_object$site_scale_net_impacts <- collate_site_scale_net_impacts(collated_site_scale_offsets = collated_object$site_scale_impacts$offsets_object$summed_gains_degs$nets,
                                                                             collated_site_scale_devs = collated_object$site_scale_impacts$dev_object$summed_gains_degs$nets)
    
    collated_object$landscape_scale <- calc_landscape_characteristics(collated_object$site_scale_outcomes, collated_object$background_cfacs, use_offset_metric)
    
    collated_object$program_outcomes <- collate_program_scale_outcomes(simulation_outputs, collated_object$site_scale_outcomes)
    
    collated_object$program_scale_impacts <- collate_program_scale_impacts(collated_object)
    
    collated_object$site_scale_NNL = assess_collated_NNL(assess_type = 'site_scale', 
                                                         impacts = collated_object$site_scale_impacts$net_impacts, 
                                                         intervention_yrs_to_use = collated_object$collated_offsets$intervention_yrs, 
                                                         site_indexes = simulation_outputs$index_object$site_indexes_used$offsets)
    
    collated_object$program_scale_NNL = assess_collated_NNL(assess_type = 'program', 
                                                            impacts = collated_object$program_scale_impacts$program_total, 
                                                            intervention_yrs_to_use = list(1), 
                                                            site_indexes = vector())
    
    collated_object$landscape_scale_NNL = assess_collated_NNL(assess_type = 'landscape', 
                                                              impacts = collated_object$landscape_scale$landscape_scale_impact, 
                                                              intervention_yrs_to_use = list(1), 
                                                              site_indexes = vector())
    
    collated_object$net_program_loss = assess_fractional_loss(net_vals = collated_object$program_outcomes$net_outcome, 
                                                              NNL_yr = collated_object$program_scale_NNL$NNL)
    
    collated_object$net_landscape_loss = assess_fractional_loss(net_vals = collated_object$landscape_scale$landscape_outcome, 
                                                                NNL_yr = collated_object$landscape_scale_NNL$NNL)
    
    collated_object$sites_used = find_sites_used(simulation_outputs)
    
    if (use_offset_metric == FALSE){
      collated_filename = paste0(file_prefix, '_feature_',
                                 formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0"), '.rds')
    }  else {
      collated_filename = paste0(file_prefix, '_metric', '.rds')
    }
    saveRDS(collated_object, collated_filename)
    
  }
  
}



select_subset <- function(current_object, subset_ind, output_type){
  sets_to_use = which(unlist(lapply(seq_along(current_object), function(i) length(current_object[[i]]) > 0)))
  
  subset_object = vector('list', length(current_object))
  subset_object[sets_to_use] <- lapply(sets_to_use, function(i) matrix(current_object[[i]][, subset_ind], ncol = 1))
  
  return(subset_object)
}


build_site_layer_stack <- function(current_data_dir, file_pattern, current_pool, current_intervention_yrs){
  
  current_filenames <- list.files(path = current_data_dir,
                                  pattern = file_pattern, all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)
  
  data_stack = vector('list', length(current_pool))
  
  for (yr in unique(current_intervention_yrs)){
    current_yr_set = as.vector(which(current_intervention_yrs == yr))
    current_site_feature_layer_filename = list.files(path = current_data_dir,
                                                     pattern = paste0(file_pattern, '_yr_', formatC((yr - 1), width = 3, format = "d", flag = "0")), 
                                                     all.files = FALSE,
                                                     include.dirs = FALSE, no.. = FALSE)
    current_site_feature_layer = readRDS(paste0(current_data_dir, current_filenames[yr]))
    data_stack[current_yr_set] = lapply(current_yr_set, function(i) list(current_site_feature_layer[[ current_pool[i] ]]))
  }
  
  return(data_stack)
}



build_site_features_at_intervention <- function(land_site_num, current_data_dir, intervention_pool, intervention_yrs_pool, simulation_params, feature_ind){
  site_features_at_intervention = vector('list', land_site_num)
  site_features_at_intervention[intervention_pool] = build_site_layer_stack(current_data_dir, 
                                                                            file_pattern = paste0('feature_', formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0")), 
                                                                            intervention_pool,
                                                                            intervention_yrs_pool)
  return(site_features_at_intervention)
}


calc_landscape_characteristics <- function(site_scale_outcomes, background_cfacs, use_offset_metric){
  
  landscape_scale_object = list()
  landscape_scale_object$landscape_cfacs = background_cfacs
  landscape_scale_object$landscape_outcome = sum_list(site_scale_outcomes)
  landscape_scale_object$net_landscape_cfac = sum_list(background_cfacs)
  landscape_scale_object$landscape_scale_impact = mapply('-', landscape_scale_object$landscape_outcome, landscape_scale_object$net_landscape_cfac, SIMPLIFY = FALSE)
  
  return(landscape_scale_object)
}


find_collated_files <- function(file_path, scenario_string, feature_string, realisation_num){
  
  scenario_filenames <- list.files(path = file_path, all.files = FALSE, 
                                   pattern = paste0('scenario_', scenario_string),
                                   full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                   include.dirs = FALSE, no.. = FALSE)
  
  feature_filenames <- list.files(path = file_path, all.files = FALSE, 
                                  pattern = paste0('feature_', feature_string),
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  
  current_filenames = intersect(feature_filenames, scenario_filenames)
  net_realisation_num = length(current_filenames)
  
  if (realisation_num == 'all'){
    realisation_num = net_realisation_num
  } 
  
  if (net_realisation_num == 0){
    stop(paste0('\n ERROR: No files found for scenario ', scenario_string, ', feature ', feature_string, ' in ', file_path))
  } 
  
  if (net_realisation_num < realisation_num){
    realisation_num = net_realisation_num
    print(paste0('WARNING: Only found ', realisation_num, ' realisation files for scenario ', scenario_string, ', feature ', feature_string, ', in ', file_path))
  }
  
  
  filenames_to_use = current_filenames[seq(realisation_num)]
  filenames_to_use = lapply(seq_along(filenames_to_use), function(i) paste0(file_path, '/', filenames_to_use[i]))
  
  return(filenames_to_use)
}

bind_collated_realisations <- function(collated_filenames){
  realisation_num = length(collated_filenames)
  
  for (realisation_ind in seq(realisation_num)){
    current_collated_realisation = readRDS(collated_filenames[[realisation_ind]])
    if (realisation_ind == 1){
      collated_realisations = lapply(seq_along(current_collated_realisation), 
                                     function(i) nest_list(current_collated_realisation[[i]]))
    } else {
      collated_realisations <- lapply(seq_along(current_collated_realisation), 
                                      function(i) append_nested_realisation(collated_realisations[[i]], current_collated_realisation[[i]], realisation_ind))
    }
    names(collated_realisations) = names(current_collated_realisation)
  }
  
  collated_realisations$realisation_num = realisation_num
  return(collated_realisations)
}


nest_list <- function(list_a){
  nested_list = lapply(seq_along(list_a), function(j) list(list_a[[j]]))
  names(nested_list) = names(list_a)
  return(nested_list)
}

expand_current_collated_realisation <- function(expand_type, collated_object, net_names, realisation_ind){
  expanded_collated_object = vector('list', length(net_names))
  common_inds = (match(names(collated_object), net_names))
  new_inds = setdiff(seq_along(net_names), common_inds)
  expanded_collated_object[common_inds] = collated_object
  if (expand_type == 'set'){
    expanded_collated_object[new_inds] = lapply(seq_along(new_inds), function(i) vector('list', realisation_ind - 1))
  } else {
    expanded_collated_object[new_inds] = lapply(seq_along(new_inds), function(i) vector('list', 1))
  }
  names(expanded_collated_object) = net_names
  return(expanded_collated_object)
}


append_nested_realisation <- function(collated_set, collated_element, realisation_ind){
  
  net_names = union(names(collated_set), names(collated_element))
  
  collated_set = expand_current_collated_realisation(expand_type = 'set', collated_set, net_names, realisation_ind)
  collated_element = expand_current_collated_realisation(expand_type = 'single', collated_element, net_names, realisation_ind)
  
  appended_object = append_nested_object(collated_set, collated_element)
  return(appended_object)
  
}

append_nested_object <- function(object_a, object_b){
  
  appended_object <- lapply(seq_along(object_a), function(j) append(object_a[[j]], list(object_b[[j]])))
  names(appended_object) = names(object_b)
  return(appended_object)  
}

merge_lists <- function(list_a, list_b, merge_indexes){
  merged_list <- lapply(seq_along(list_a),
                        function(i) merge_vectors(list_a[[i]], list_b[[i]], merge_indexes[i]))
  return(merged_list)
}

collate_program_cfacs <- function(simulation_outputs, background_cfacs, collated_offsets, collated_devs, collated_dev_credit, 
                                  collated_offset_bank, collated_unregulated_loss){
  
  program_cfacs_object = list()
  program_cfacs_object$offset_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$offsets_object$site_indexes)], collated_offsets$cfacs, unlist(simulation_outputs$offsets_object$intervention_yrs))
  program_cfacs_object$dev_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$dev_object$site_indexes)], collated_devs$cfacs, unlist(simulation_outputs$dev_object$intervention_yrs))
  program_cfacs_object$dev_credit_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$credit_object$site_indexes)], collated_dev_credit$cfacs, unlist(simulation_outputs$credit_object$intervention_yrs))
  program_cfacs_object$offset_bank_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$offset_bank_object$site_indexes)], collated_offset_bank$cfacs, unlist(simulation_outputs$offset_bank_object$intervention_yrs))
  program_cfacs_object$program_cfac_sum <- sum_list(unlist(program_cfacs_object, recursive = FALSE))
  
  return(program_cfacs_object)
  
}




collate_cfacs <- function(site_features_group, simulation_params, feature_params, feature_dynamics, feature_dynamics_modes, projection_yrs, intervention_yrs, 
                          site_num_remaining_pool, cfac_type, object_type, use_cfac_type_in_sim, condition_class_bounds, use_offset_metric){
  
  
  if ((use_cfac_type_in_sim == FALSE) || (cfac_type == 'background')){
    cfac_params = lapply(seq_along(site_features_group), function(i) setNames(rep(list(FALSE), 4), 
                                                                                c('include_potential_developments', 
                                                                                  'include_potential_offsets', 
                                                                                  'include_unregulated_loss', 
                                                                                  'adjust_cfacs_flag')))
  } else {
    cfac_params = lapply(seq_along(site_features_group), function(i) select_cfac_params(object_type[[i]], simulation_params))
  }
  
  
  if (cfac_type == 'background'){ 
    
    time_horizons <- generate_time_horizons(project_type = 'future', 
                                            yr = 1, 
                                            intervention_yrs = rep(1, length(site_features_group)), 
                                            time_horizon = (simulation_params$time_steps - 1), 
                                            length(site_features_group))
    
    
  } else if (cfac_type == 'site_scale'){
    time_horizons = generate_time_horizons(project_type = 'current', 
                                           yr = simulation_params$time_steps, 
                                           unlist(intervention_yrs),
                                           time_horizon = (simulation_params$time_steps - 1), 
                                           length(site_features_group))
  }
  
  cfac_weights_group = lapply(seq_along(cfac_params), function(i) unlist(calc_cfac_weights(site_num = 1, 
                                                                                           cfac_params[[i]]$include_potential_developments,
                                                                                           cfac_params[[i]]$include_potential_offsets,
                                                                                           cfac_params[[i]]$include_unregulated_loss,
                                                                                           dev_probability_list = vector(), 
                                                                                           offset_probability_list = vector(), 
                                                                                           simulation_params, 
                                                                                           feature_params, 
                                                                                           site_num_remaining_pool[[i]], 
                                                                                           time_horizons[i], 
                                                                                           unlist(intervention_yrs)[i]), recursive = FALSE))
  
  time_horizons = lapply(seq_along(time_horizons), function(i) 0:time_horizons[i])
  
  #   if (adjust_cfacs_flag == FALSE){
  #     time_fill = FALSE
  #     cfac_weights = lapply(seq_along(site_features), function(i) array(1, time_horizons[[i]]))
  #   } else {
  #     cfac_weights = calc_cfac_weights(site_num = length(site_features), include_potential_developments, include_potential_offsets, include_unregulated_loss,
  #                                      dev_probability_list, offset_probability_list, simulation_params, feature_params, parcel_num_remaining, time_horizons, unlist(intervention_yrs))
  #     time_fill = TRUE
  #   }
  
  if (use_offset_metric == FALSE){

    cfacs = lapply(seq_along(site_features_group),
                   function(i) do.call(rbind, lapply(seq_along(time_horizons[[i]]), 
                                                     function(j) matrix(do.call(cbind, sum_site_features( calc_site_cfacs(site_features_group[[i]],
                                                                                                                 projection_yrs[[i]],
                                                                                                                 cfac_weights_group[[i]],
                                                                                                                 simulation_params,
                                                                                                                 feature_params,
                                                                                                                 feature_dynamics[[i]],
                                                                                                                 feature_dynamics_modes[[i]],
                                                                                                                 time_horizons[[i]][j],
                                                                                                                 cfac_params[[i]]$include_potential_developments,
                                                                                                                 cfac_params[[i]]$include_potential_offsets,
                                                                                                                 cfac_params[[i]]$include_unregulated_loss,
                                                                                                                 cfac_params[[i]]$adjust_cfacs_flag,
                                                                                                                 time_fill = FALSE,
                                                                                                                 bind_condition_classes = TRUE))), nrow = 1))) )
    
    
  } else {
    
    cfacs = lapply(seq_along(site_features_group),
                   function(i) do.call(rbind, lapply(seq_along(time_horizons[[i]]), 
                                                     function(j) matrix(apply(user_transform_function( calc_site_cfacs(site_features_group[[i]],
                                                                                                                       projection_yrs[[i]],
                                                                                                                       cfac_weights_group[[i]],
                                                                                                                       simulation_params,
                                                                                                                       feature_params,
                                                                                                                       feature_dynamics[[i]],
                                                                                                                       feature_dynamics_modes[[i]],
                                                                                                                       time_horizons[[i]][j],
                                                                                                                       cfac_params[[i]]$include_potential_developments,
                                                                                                                       cfac_params[[i]]$include_potential_offsets,
                                                                                                                       cfac_params[[i]]$include_unregulated_loss,
                                                                                                                       cfac_params[[i]]$adjust_cfacs_flag,
                                                                                                                       time_fill = FALSE,
                                                                                                                       bind_condition_classes = TRUE), simulation_params$transform_params ), 1, 'sum'), nrow = 1) )))
    
  }
  
  return(cfacs)
  
}




collate_program_scale_outcomes <- function(simulation_outputs, site_scale_outcomes){
  
  
  program_scale_outcomes = lapply(seq_along(simulation_outputs$interventions), 
                                  function(i) sum_list(site_scale_outcomes[unlist(simulation_outputs$interventions[[i]]$site_indexes)]))
  names(program_scale_outcomes) = names(simulation_outputs$interventions)
  
  program_scale_outcomes$net_offsets <- sum_list(append(program_scale_outcomes$offsets_object, program_scale_outcomes$offset_bank_object))
  program_scale_outcomes$net_devs <- sum_list(append(program_scale_outcomes$dev_object, program_scale_outcomes$credit_object))
  program_scale_outcomes$net_outcome <- sum_list(append(program_scale_outcomes$net_offsets, program_scale_outcomes$net_devs))
  
  return(program_scale_outcomes)
}


collate_program_scale_impacts <- function(collated_data){
  
  program_scale_impacts = lapply(seq_along(collated_data$site_scale_impacts), function(i) collated_data$site_scale_impacts[[i]]$summed_gains_degs$nets)
  names(program_scale_impacts) = names(collated_data$site_scale_impacts)
  
  program_scale_impacts$net_offset_gains = sum_list(append(program_scale_impacts$offsets_object, program_scale_impacts$offset_bank_object))
  program_scale_impacts$net_dev_losses = sum_list(append(program_scale_impacts$dev_object, program_scale_impacts$credit_object))
  program_scale_impacts$program_total = sum_list(append(program_scale_impacts$net_offset_gains, program_scale_impacts$net_dev_losses))
  
  return(program_scale_impacts)
}



calc_site_scale_impacts <- function(current_simulation_outputs, current_site_sets, site_scale_cfacs, summed_site_features_at_intervention, site_scale_outcomes,  
                                    collate_type, simulation_params, feature_params, use_offset_metric){
  
  current_pool = unlist(current_simulation_outputs$site_indexes)
  if (length(current_pool) == 0){
    return(NULL)
  }
  
  collated_object <- assess_gains_degs(site_scale_outcomes_to_use = site_scale_outcomes[current_pool],
                                       cfacs_to_use = site_scale_cfacs[current_pool],
                                       summed_site_features_at_intervention[current_pool],
                                       current_intervention_yrs = unlist(current_simulation_outputs$intervention_yrs),
                                       collate_type, 
                                       simulation_params, feature_params,
                                       time_steps = simulation_params$time_steps, 
                                       use_offset_metric)
  
  collated_object$grouped_gains_degs = group_gains_degs(collated_object, current_site_sets)
  collated_object$summed_gains_degs = sum_gains_degs(collated_object$grouped_gains_degs)
  
  
  collated_object$site_indexes = current_site_sets
  collated_object$intervention_yrs = current_simulation_outputs$intervention_yrs
  collated_object$cfacs = site_scale_cfacs$cfacs
  return(collated_object)
}


sum_gains_degs <- function(grouped_gains_degs){
  summed_gains_degs <- lapply(seq_along(grouped_gains_degs), 
                              function(i) (lapply(seq_along(grouped_gains_degs[[i]]), function(j) Reduce('+', grouped_gains_degs[[i]][[j]]))))
  
  names(summed_gains_degs) = names(grouped_gains_degs) 
  return(summed_gains_degs)
}


group_gains_degs <- function(collated_object, site_indexes){ 
  
  grouped_gains_degs <- lapply(seq_along(collated_object), 
                               function(i) (lapply(seq_along(site_indexes), 
                                                   function(j) collated_object[[i]][which(unlist(site_indexes) %in% site_indexes[[j]])])))
  names(grouped_gains_degs) = names(collated_object)
  
  return(grouped_gains_degs)
  
}



sum_data_stack <- function(current_data_dir, file_pattern, time_steps){
  
  current_filenames <- list.files(path = current_data_dir,
                                  pattern = file_pattern, all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)
  for (yr in seq(time_steps)){
    site_features = readRDS(paste0(current_data_dir, current_filenames[yr]))
    if (yr == 1){
      data_stack = rep(list(matrix(0, nrow = time_steps, ncol = 1)), length(site_features))
    }

    summed_site_features = sum_site_features(site_features)

    data_stack <- lapply(seq_along(summed_site_features), function(i) stack_yr(data_stack[[i]], summed_site_features[[i]], yr))
  }
  
  return(data_stack)
}

stack_yr <- function(current_site, current_val, yr){
  current_site[yr] = current_val
  return(current_site)
}



merge_vectors <- function(vec_a, vec_b, start_ind){
  vec_a[start_ind:(start_ind + length(vec_b) - 1)] = vec_b
  return(vec_a)
}


assess_gains_degs <- function(site_scale_outcomes_to_use, cfacs_to_use, summed_site_features_at_intervention, 
                              current_intervention_yrs, collate_type, simulation_params, feature_params, time_steps, use_offset_metric){
  collated_object = list()
  
  site_num = length(site_scale_outcomes_to_use)
  impact_trajectories = lapply(seq(site_num), function(i) site_scale_outcomes_to_use[[i]][current_intervention_yrs[i]:time_steps])
  avoided_loss = lapply(seq(site_num), function(i) rep(summed_site_features_at_intervention[[i]], length(cfacs_to_use[[i]])) - cfacs_to_use[[i]])
  rest_gains = lapply(seq(site_num), function(i) impact_trajectories[[i]] - rep(summed_site_features_at_intervention[[i]], length(impact_trajectories[[i]])))
  net_gains = mapply('-', impact_trajectories, cfacs_to_use, SIMPLIFY = FALSE)
  
  collated_object = list(net_gains, avoided_loss, rest_gains)
  collated_object <- lapply(seq_along(collated_object),
                            function(i) lapply(seq_along(collated_object[[i]]), 
                                               function(j) merge_vectors(array(0, time_steps), collated_object[[i]][[j]], current_intervention_yrs[j])))
  
  names(collated_object) = c('nets', 'avoided_loss', 'rest_gains')
  
  
  if ((collate_type == 'offsets_object') | (collate_type == 'offset_bank_object')){
    if (simulation_params$offset_calc_type == 'restoration_gains'){
      collated_object$nets = collated_object$rest_gains
    } else if (simulation_params$offset_calc_type == 'avoided_loss'){
      collated_object$nets = collated_object$avoided_loss
    } else if (simulation_params$offset_calc_type == 'net_gains'){
      collated_object$nets = collated_object$nets
    }
  } else {
    if (simulation_params$dev_calc_type == 'future_condition'){
      collated_object$nets = collated_object$nets
    } else if (simulation_params$dev_calc_type == 'current_condition'){
      collated_object$nets = collated_object$rest_gains
    }
    
  }
  
  return(collated_object)
}


find_sites_used <- function(simulation_outputs){
  sites_used = lapply(seq_along(simulation_outputs$interventions), function(i) find_current_sites_used(simulation_outputs$interventions[[i]]$site_indexes))
  names(sites_used) = names(simulation_outputs$interventions)
  return(sites_used)
}

find_current_sites_used <- function(current_sites_list){
  if (length(current_sites_list) == 0){
    sites_used = list()
    return(sites_used)
  } else {
    sites_used = length(unlist(current_sites_list))
    return(sites_used)
  }
}

# determine cumulative value of all sites within parcel feature_layers for multiple features
sum_sites <- function(site_features, use_offset_metric, transform_params){
  
  sets_to_use = which(unlist(lapply(seq_along(site_features), function(i) length(site_features[[i]]) > 0)))
  
  summed_site_features = vector('list', length(site_features))
  if (use_offset_metric == TRUE){
    site_features[sets_to_use] <- lapply(sets_to_use,  function(i) lapply(seq_along(site_features[[i]]), function(j) do.call(cbind, site_features[[i]][[j]])))
    
    summed_site_features[sets_to_use] = lapply(sets_to_use, function(i) sum(user_transform_function(site_features[[i]], transform_params)))
    
  } else {
    summed_site_features[sets_to_use] <- lapply(sets_to_use, function(i) matrix(do.call(cbind, sum_site_features(site_features[[i]])), nrow = 1))
  }
  
  return(summed_site_features)
  
}

collate_site_scale_net_impacts <- function(collated_site_scale_offsets, collated_site_scale_devs){
  site_scale_impacts = list()
  if ((length(collated_site_scale_offsets) > 0) & (length(collated_site_scale_devs) > 0)){
    site_scale_impacts$net_impacts <- mapply('+', collated_site_scale_offsets, collated_site_scale_devs, SIMPLIFY = FALSE)
  } else {
    site_scale_impacts = list()
  }
  
  return(site_scale_impacts)
}


sum_list <- function(list_to_sum){
  if (length(list_to_sum) == 0){
    summed_list = list()
  } else {
    sets_to_use = which(unlist(lapply(seq_along(list_to_sum), function(i) length(list_to_sum[[i]]) > 0)))
    if (length(sets_to_use) == 0){
      summed_list = list()
    } else {
      summed_list <- list(Reduce('+', list_to_sum[sets_to_use]))
    }
  }
  return(summed_list)
}


# find_list_mean <- function(list_to_sum){
# 
#   sets_to_use = which(unlist(lapply(seq_along(list_to_sum), function(i) length(list_to_sum[[i]]) > 0)))
#   if (length(sets_to_use) == 0){
#     list_mean = list()
#   } else {
#     list_mean <-  list(Reduce('+', list_to_sum[sets_to_use])/length(sets_to_use))
#   }
#   return(list_mean)
# }


assess_NNL <- function(current_impacts){
  
  NNL_flag = FALSE
  
  for (NNL_yr in seq_along(current_impacts)){
    NNL_flag = all(current_impacts[NNL_yr:length(current_impacts)] > 0)
    if (NNL_flag == TRUE){
      break
    }
  }
  
  if (NNL_flag == FALSE){
    NNL_yr = vector()
  } 
  
  return(NNL_yr)
}


assess_collated_NNL <- function(assess_type, impacts, intervention_yrs_to_use, site_indexes){
  NNL_object <- list()
  
  if (length(unlist(impacts)) == 0){
    return(NNL_object)
  }
  
  
  if (assess_type == 'site_scale'){
    
    site_indexes_to_use = match(unlist(lapply(seq_along(site_indexes), function(i) site_indexes[[i]][[1]])), unlist(site_indexes))
    intervention_yrs_to_use = intervention_yrs_to_use[site_indexes_to_use]
  } 
  
  
  NNL_absolute = lapply(seq_along(impacts), function(i) assess_NNL(impacts[[i]]) )
  
  NNL_object$NNL = lapply(seq_along(NNL_absolute), function(i) (NNL_absolute[[i]] - intervention_yrs_to_use[[i]]))
  
  if (length(unlist(NNL_object$NNL)) >0){
    NNL_object$NNL_success = length(unlist(NNL_object$NNL))/length(NNL_object$NNL)
    NNL_object$NNL_mean = mean(unlist(NNL_object$NNL))
  } else {
    NNL_object$NNL_mean = vector()
  }
  
  return(NNL_object)
}


assess_fractional_loss <- function(net_vals, NNL_yr){
  fractional_loss = list()
  
  net_vals = unlist(net_vals)
  NNL_yr = unlist(NNL_yr)
  
  sc_factor = net_vals[1]
  
  if (sc_factor == 0){
    return(fractional_loss)
  } else {
    
    if (length(net_vals) > 0){
      fractional_loss$total_loss = 1 - net_vals[length(net_vals)]/net_vals[1]
    }
    if (length(NNL_yr) > 0){
      fractional_loss$NNL_loss = 1 - net_vals[NNL_yr]/net_vals[1]
    }
    
  }
  
  return(fractional_loss)
}


select_cfac_params <- function(object_type, simulation_params){
  
  if ((object_type == "dev_object") | (object_type == "credit_object") | (object_type == "unregulated_loss_object")){
    cfac_params = list(simulation_params$include_potential_developments_in_dev_calc,
                       simulation_params$include_potential_offsets_in_dev_calc,
                       simulation_params$include_unregulated_loss_in_dev_calc,
                       simulation_params$adjust_dev_cfacs_flag)
  } else {
    cfac_params = list(simulation_params$include_potential_developments_in_offset_calc,
                       simulation_params$include_potential_offsets_in_offset_calc,
                       simulation_params$include_unregulated_loss_in_offset_calc,
                       simulation_params$adjust_offset_cfacs_flag)
  }
  names(cfac_params) = c('include_potential_developments', 'include_potential_offsets', 'include_unregulated_loss', 'adjust_cfacs_flag')
  
  return(cfac_params)
}




sum_cols <- function(array_to_sum){
  if (is.null(array_to_sum)){
    return(NULL)
  } else {
    if (length(dim(array_to_sum)) <= 1){
      summed_array = sum(array_to_sum)
    } else if (length(dim(array_to_sum)) == 2){
      summed_array = apply(array_to_sum, MARGIN = 1, sum)
    } else if (length(dim(array_to_sum)) == 3){
      summed_array = apply(array_to_sum, MARGIN = c(1, 3), sum)
      dim(summed_array) = c(dim(summed_array), 1)
      summed_array = aperm(summed_array, c(1, 3, 2))
    }
  }
  return(summed_array)
}


threshold_array <- function(arr_in, thresh_level){
  thresh_array = rep(thresh_level, length(arr_in))
  dim(thresh_array) = dim(arr_in)
  arr_out = arr_in * (abs(arr_in) > thresh_array)
  return(arr_out)
}





