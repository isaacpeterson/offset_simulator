collate_simulation_outputs <- function(simulation_data_object, background_cfacs_object, scenario_ind, realisation_ind){
  
  current_data_dir = write_folder(paste0(simulation_data_object$global_params$output_folder, 
                                         'scenario_', formatC(scenario_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
                                         '/realisation_', formatC(realisation_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), '/'))
  
  file_prefix = paste0(simulation_data_object$global_params$collated_folder,
                       'collated_scenario_',  formatC(scenario_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                       '_realisation_', formatC(realisation_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"))
  
  simulation_outputs = readRDS(paste0(current_data_dir, 'realisation_',
                                      formatC(realisation_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                                      '_outputs.rds'))
  
  collated_object <- run_collate_routines(simulation_outputs,
                                          background_cfacs_object$background_cfacs,
                                          simulation_data_object$feature_dynamics, 
                                          simulation_data_object$feature_dynamics_modes,
                                          simulation_data_object$site_features,
                                          simulation_data_object$simulation_params,
                                          simulation_data_object$feature_params,
                                          simulation_data_object$global_params, 
                                          current_data_dir, 
                                          file_prefix,
                                          use_offset_metric = FALSE, 
                                          simulation_data_object$user_transform_function)
  
  
  if (simulation_data_object$simulation_params$use_offset_metric == TRUE){
    run_collate_routines(simulation_outputs,
                         background_cfacs_object$user_metric_background_cfacs,
                         simulation_data_object$feature_dynamics, 
                         simulation_data_object$feature_dynamics_modes,
                         simulation_data_object$site_features,
                         simulation_data_object$simulation_params,
                         simulation_data_object$feature_params,
                         simulation_data_object$global_params, 
                         current_data_dir, 
                         file_prefix,
                         use_offset_metric = TRUE, 
                         simulation_data_object$user_transform_function)
  }
  
}

build_background_cfacs <- function(simulation_data_object){
  
  background_cfacs_object = list()
  background_projection_yrs_pool = lapply(seq_along(simulation_data_object$site_features), 
                                          function(i) lapply(seq(simulation_data_object$simulation_params$feature_num), 
                                                             function(j) rep(list(1), length(simulation_data_object$feature_dynamics_modes[[i]][[j]])) ))  
  
  background_cfacs_object$background_cfacs = collate_cfacs(simulation_data_object$site_features,
                                                           simulation_data_object$simulation_params, 
                                                           simulation_data_object$feature_params,
                                                           simulation_data_object$feature_dynamics,
                                                           simulation_data_object$feature_dynamics_modes,
                                                           simulation_data_object$site_element_index_key,
                                                           background_projection_yrs_pool,
                                                           intervention_yrs = rep(1, length(initial_feature_layer)),
                                                           vector(),
                                                           cfac_type = 'background',
                                                           object_type = vector(), 
                                                           use_cfac_type_in_sim = FALSE, 
                                                           condition_class_bounds = simulation_data_object$feature_params$condition_class_bounds, 
                                                           use_offset_metric = FALSE, 
                                                           simulation_data_object$user_transform_function)
  
  saveRDS(background_cfacs_object$background_cfacs, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_cfacs.rds'))
  
  if (simulation_data_object$simulation_params$use_offset_metric == TRUE){
    
    background_cfacs_object$user_metric_background_cfacs = collate_cfacs(simulation_data_object$site_features,
                                                                         simulation_data_object$simulation_params, 
                                                                         simulation_data_object$feature_params,
                                                                         simulation_data_object$feature_dynamics,
                                                                         simulation_data_object$feature_dynamics_modes,
                                                                         simulation_data_object$site_element_index_key,
                                                                         background_projection_yrs_pool,
                                                                         intervention_yrs = rep(1, length(initial_feature_layer)),
                                                                         vector(),
                                                                         cfac_type = 'background',
                                                                         object_type = vector(), 
                                                                         use_cfac_type_in_sim = FALSE, 
                                                                         condition_class_bounds = simulation_data_object$feature_params$condition_class_bounds, 
                                                                         use_offset_metric = TRUE, 
                                                                         simulation_data_object$user_transform_function)
    
    saveRDS(background_cfacs_object$background_cfacs, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'user_metric_background_cfacs.rds'))
    
  }
  
  return(background_cfacs_object)
}



run_landscape_scale_collate_routines <- function(){
  for (feature_ind in features_to_collate){
    
    collated_object = list()
    if (use_offset_metric == FALSE){
      flog.info('collating landscape feature %s', feature_ind)
      
      background_cfacs_to_use = select_subset(background_cfacs, feature_ind)
    } else {
      flog.info('collating metric')
      
      background_cfacs_to_use = background_cfacs
    }
    
    
    
  } 
}

run_collate_routines <- function(simulation_outputs, background_cfacs, feature_dynamics, feature_dynamics_modes, initial_feature_layer, simulation_params, feature_params, 
                                 global_params, current_data_dir, file_prefix, use_offset_metric, user_transform_function){
  
  intervention_pool = setNames(lapply(seq_along(simulation_outputs$interventions), function(i) simulation_outputs$interventions[[i]]$internal_site_indexes), names(simulation_outputs$interventions))
  
  if (length(unlist(intervention_pool)) > 0){
    intervention_object <- run_pre_collate_intervention_routines(simulation_outputs, feature_dynamics, feature_dynamics_modes, initial_feature_layer, simulation_params, feature_params, 
                                                                 global_params, current_data_dir, use_offset_metric, user_transform_function, intervention_pool)
  }
  
  if (use_offset_metric == FALSE){
    features_to_collate = seq(simulation_params$feature_num)
  } else {
    features_to_collate = 1
  }
  
  for (feature_ind in features_to_collate){
    
    collated_object = list()
    if (use_offset_metric == FALSE){
      flog.info('collating feature %s', feature_ind)
      site_scale_outcomes_to_use = sum_data_stack(current_data_dir, 
                                                  file_pattern = paste0('feature_', formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = global_params$numeric_placeholder_width, format = "d", flag = "0")), 
                                                  simulation_params$time_steps)
      
      background_cfacs_to_use = select_subset(background_cfacs, feature_ind)
      
    } else {
      flog.info('collating metric')
      site_scale_outcomes_to_use = sum_data_stack(current_data_dir, file_pattern = paste0('metric_'), simulation_params$time_steps)
      background_cfacs_to_use = background_cfacs
    }
    
    collated_object$landscape_scale <- calc_landscape_characteristics(site_scale_outcomes_to_use, background_cfacs_to_use)
    collated_object$NNL$landscape_scale = assess_NNL(collated_object$landscape_scale$landscape_scale_impact, 
                                                     intervention_yr = 1)
    collated_object$net_landscape_loss = assess_fractional_loss(net_vals = collated_object$landscape_scale$landscape_outcome, 
                                                                NNL_yr = collated_object$landscape_scale_NNL$NNL)
    
    if (length(unlist(intervention_pool)) > 0){
      collated_object$intervention_pool <- intervention_pool
      collated_object <- run_collate_intervention_routines(collated_object, intervention_object, simulation_outputs,  site_scale_outcomes_to_use, background_cfacs,
                                                           simulation_params, feature_params, global_params, current_data_dir, use_offset_metric, feature_ind)
    }
    
    
    if (use_offset_metric == FALSE){
      collated_filename = paste0(file_prefix, '_feature_',
                                 formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = global_params$numeric_placeholder_width, format = "d", flag = "0"), '.rds')
    }  else {
      collated_filename = paste0(file_prefix, '_metric', '.rds')
    }
    saveRDS(collated_object, collated_filename)
    
    
  }
  return(collated_object)
}



run_collate_intervention_routines <- function(collated_object, intervention_object, simulation_outputs,  site_scale_outcomes_to_use, background_cfacs,
                                              simulation_params, feature_params,  global_params, current_data_dir, use_offset_metric, feature_ind){
  if (use_offset_metric == FALSE){
    
    summed_site_features_at_intervention_to_use = select_subset(intervention_object$summed_site_features_at_intervention, feature_ind)
    site_scale_cfacs_to_use = select_subset(intervention_object$site_scale_cfacs, feature_ind)
    
  } else {
    summed_site_features_at_intervention_to_use = intervention_object$summed_site_features_at_intervention
    site_scale_cfacs_to_use = intervention_object$site_scale_cfacs
    
  }


  collated_object$site_scale_impacts = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                                       function(i) calc_site_scale_impacts(current_intervention_simulation_outputs = simulation_outputs$interventions[[i]], 
                                                                                           current_intervention_site_sets = simulation_outputs$index_object$internal_site_indexes_used[[i]], 
                                                                                           current_collate_type = names(simulation_outputs$interventions)[[i]], 
                                                                                           site_scale_cfacs_to_use, 
                                                                                           summed_site_features_at_intervention_to_use, 
                                                                                           site_scale_outcomes_to_use,  
                                                                                           simulation_params, 
                                                                                           feature_params, 
                                                                                           use_offset_metric)), names(simulation_outputs$interventions))
  
  collated_object$program_scale_cfacs = collate_program_scale_cfacs(site_scale_cfacs_to_use, simulation_outputs$interventions, background_cfacs, intervention_object$intervention_yrs_pool)
  
  collated_object$site_scale_net_impacts <- collate_site_scale_net_impacts(collated_site_scale_offsets = collated_object$site_scale_impacts$offsets_object$summed_gains_degs$nets,
                                                                           collated_site_scale_devs = collated_object$site_scale_impacts$dev_object$summed_gains_degs$nets)
  collated_object$program_outcomes <- collate_program_scale_outcomes(simulation_outputs, site_scale_outcomes_to_use)
  
  collated_object$program_scale_impacts <- collate_program_scale_impacts(collated_object)
  
  #     collated_object$site_scale_NNL = assess_site_scale_NNL(assess_type = 'site_scale', 
  #                                                          impacts = collated_object$site_scale_impacts$net_impacts, 
  #                                                          intervention_yrs_to_use = collated_object$collated_offsets$intervention_yrs, 
  #                                                          internal_site_indexes = simulation_outputs$index_object$internal_site_indexes_used$offsets)
  
  collated_object$NNL$program_scale = assess_NNL(current_impacts = collated_object$program_scale_impacts$program_total, 
                                                 intervention_yr = 1)

  collated_object$net_program_loss = assess_fractional_loss(net_vals = collated_object$program_outcomes$net_outcome, 
                                                            NNL_yr = collated_object$program_scale_NNL$NNL)
  
  collated_object$sites_used = find_sites_used(simulation_outputs)
  
  collated_object$site_scale$outcomes = site_scale_outcomes_to_use
  collated_object$site_scale$summed_site_features_at_intervention = summed_site_features_at_intervention_to_use
  collated_object$site_scale$site_scale_cfacs = site_scale_cfacs_to_use
  
  return(collated_object)
}


run_pre_collate_intervention_routines <- function(simulation_outputs, feature_dynamics, feature_dynamics_modes, initial_feature_layer, simulation_params, feature_params, 
                                                  global_params, current_data_dir, use_offset_metric, user_transform_function, intervention_pool){
  
  intervention_yrs_pool = setNames(lapply(seq_along(simulation_outputs$interventions), function(i) simulation_outputs$interventions[[i]]$intervention_yrs), names(simulation_outputs$interventions))
  object_name_pool = unlist(lapply(seq_along(simulation_outputs$interventions), function(i) rep(names(simulation_outputs$interventions)[i], length(intervention_pool[[i]]))))
  site_num_remaining_pool = unlist(lapply(seq_along(simulation_outputs$interventions), function(i) simulation_outputs$interventions[[i]]$site_num_remaining))
  
  intervention_object = list()
  projection_yrs_pool = lapply(seq(length(unlist(intervention_pool))), 
                               function(i) lapply(seq(simulation_params$feature_num), 
                                                  function(j) rep(list(unlist(intervention_yrs_pool)[i]), 
                                                                  length(feature_dynamics_modes[[unlist(intervention_pool)[i] ]][[j]])) ))
  
  site_features_at_intervention_set = vector('list', length(initial_feature_layer))
  
  for (current_feature_ind in seq(simulation_params$feature_num)){
    site_features_at_intervention = build_site_features_at_intervention(length(initial_feature_layer), current_data_dir, unlist(intervention_pool), unlist(intervention_yrs_pool), simulation_params, current_feature_ind, global_params$numeric_placeholder_width)
    site_features_at_intervention_set = lapply(seq_along(site_features_at_intervention_set), function(i) append(site_features_at_intervention_set[[i]], site_features_at_intervention[[i]]))
  }
  
  if (use_offset_metric == FALSE){
    flog.info('building counterfactuals over time series - this may take a while')
  } else {
    flog.info('building user metric counterfactuals over time series - this may take a while')
  }
  
  site_element_index_key = readRDS(paste0(global_params$simulation_inputs_folder, 'site_element_index_key.rds'))
  
  site_scale_cfacs = vector('list', length(initial_feature_layer))
  
  site_scale_cfacs[unlist(intervention_pool)] = collate_cfacs(site_features_at_intervention_set[unlist(intervention_pool)],
                                                              simulation_params, 
                                                              feature_params,
                                                              feature_dynamics[unlist(intervention_pool)],
                                                              feature_dynamics_modes[unlist(intervention_pool)],
                                                              site_element_index_key[unlist(intervention_pool)],
                                                              projection_yrs_pool,
                                                              unlist(intervention_yrs_pool),
                                                              site_num_remaining_pool,
                                                              cfac_type = 'site_scale',
                                                              object_type = object_name_pool, 
                                                              use_cfac_type_in_sim = TRUE, 
                                                              condition_class_bounds = feature_params$condition_class_bounds, 
                                                              use_offset_metric, 
                                                              user_transform_function)
  
  summed_site_features_at_intervention = sum_sites(site_features_at_intervention_set, use_offset_metric, user_transform_function, simulation_params$transform_params)
  
  intervention_object = list()
  intervention_object$site_scale_cfacs = site_scale_cfacs
  intervention_object$summed_site_features_at_intervention = summed_site_features_at_intervention
  intervention_object$intervention_yrs_pool = intervention_yrs_pool
  return(intervention_object)
}

select_subset <- function(current_object, subset_ind, output_type){
  sets_to_use = which(unlist(lapply(seq_along(current_object), function(i) length(current_object[[i]]) > 0)))
  
  subset_object = vector('list', length(current_object))
  subset_object[sets_to_use] <- lapply(sets_to_use, function(i) matrix(current_object[[i]][, subset_ind], ncol = 1))
  
  return(subset_object)
}


build_site_layer_stack <- function(current_data_dir, file_pattern, current_pool, current_intervention_yrs, numeric_placeholder_width){
  
  current_filenames <- list.files(path = current_data_dir,
                                  pattern = file_pattern, all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)
  
  data_stack = vector('list', length(current_pool))
  
  for (yr in unique(current_intervention_yrs)){
    current_yr_set = as.vector(which(current_intervention_yrs == yr))
    
    current_site_feature_layer_filename = list.files(path = current_data_dir,
                                                     pattern = paste0(file_pattern, '_yr_', formatC((yr - 1), width = numeric_placeholder_width, format = "d", flag = "0")), 
                                                     all.files = FALSE,
                                                     include.dirs = FALSE, no.. = FALSE)
    current_site_feature_layer = readRDS(paste0(current_data_dir, current_filenames[yr]))
    data_stack[current_yr_set] = lapply(current_yr_set, function(i) list(current_site_feature_layer[[ current_pool[i] ]]))
  }
  
  return(data_stack)
}



build_site_features_at_intervention <- function(land_site_num, current_data_dir, intervention_pool, intervention_yrs_pool, simulation_params, feature_ind, numeric_placeholder_width){
  site_features_at_intervention = vector('list', land_site_num)
  site_features_at_intervention[intervention_pool] = build_site_layer_stack(current_data_dir, 
                                                                            file_pattern = paste0('feature_', formatC(simulation_params$features_to_use_in_simulation[feature_ind], width = numeric_placeholder_width, format = "d", flag = "0")), 
                                                                            intervention_pool,
                                                                            intervention_yrs_pool, 
                                                                            numeric_placeholder_width)
  return(site_features_at_intervention)
}


calc_landscape_characteristics <- function(site_scale_outcomes, background_cfacs){

  # browser()
  landscape_scale_object = list()
  landscape_scale_object$background_cfacs = background_cfacs
  landscape_scale_object$landscape_outcome = sum_list(site_scale_outcomes)
  landscape_scale_object$net_landscape_cfac = sum_list(background_cfacs)
  landscape_scale_object$landscape_scale_impact = landscape_scale_object$landscape_outcome-landscape_scale_object$net_landscape_cfac
  
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


collate_program_scale_cfacs <- function(site_scale_cfacs, interventions, background_cfacs, intervention_yrs_pool){
  
  program_scale_cfacs = setNames(lapply(seq_along(interventions), 
                                        function(i) merge_lists(background_cfacs[unlist(interventions[[i]]$internal_site_indexes)], 
                                                                site_scale_cfacs[unlist(interventions[[i]]$internal_site_indexes)], unlist(intervention_yrs_pool[[i]]))), 
                                 names(interventions))
  program_scale_cfacs$program_cfac_sum <- Reduce('+', unlist(program_scale_cfacs, recursive = FALSE))
  
  return(program_scale_cfacs)
  
}




collate_cfacs <- function(site_features_group, simulation_params, feature_params, feature_dynamics, feature_dynamics_modes, site_element_index_key, projection_yrs, intervention_yrs, 
                          site_num_remaining_pool, cfac_type, object_type, use_cfac_type_in_sim, condition_class_bounds, use_offset_metric, user_transform_function){
  
  
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
                                                                                                                          unlist_condition_classes = FALSE, 
                                                                                                                          site_element_index_key = vector()))), nrow = 1))) )
    
    
  } else {
    
    cfacs = lapply(seq_along(site_features_group),
                   function(i) do.call(rbind, lapply(seq_along(time_horizons[[i]]), 
                                                     function(j) matrix(sum(user_transform_function( calc_site_cfacs(site_features_group[[i]],
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
                                                                                                                     unlist_condition_classes = TRUE, 
                                                                                                                     site_element_index_key[[i]]), 
                                                                                                     simulation_params$transform_params )), nrow = 1) )))
    
  }
  
  return(cfacs)
  
}


collate_program_scale_outcomes <- function(simulation_outputs, site_scale_outcomes){
  
  program_scale_outcomes = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                           function(i) sum_list(site_scale_outcomes[unlist(simulation_outputs$interventions[[i]]$internal_site_indexes)])), names(simulation_outputs$interventions))
  program_scale_outcomes$net_outcome <- sum_list(program_scale_outcomes)
  program_scale_outcomes$net_offsets <- sum_list(program_scale_outcomes[match(c('offsets_object', 'offset_bank_object'), names(program_scale_outcomes))])
  program_scale_outcomes$net_devs <- sum_list(program_scale_outcomes[match(c('dev_object', 'credit_object'), names(program_scale_outcomes))])
  
  
  return(program_scale_outcomes)
}


collate_program_scale_impacts <- function(collated_data){
  
  program_scale_impacts = setNames(lapply(seq_along(collated_data$site_scale_impacts), function(i) collated_data$site_scale_impacts[[i]]$summed_gains_degs$nets), names(collated_data$site_scale_impacts))
  
  program_scale_impacts$net_offset_gains = sum_list(append(program_scale_impacts$offsets_object, program_scale_impacts$offset_bank_object))
  program_scale_impacts$net_dev_losses = sum_list(append(program_scale_impacts$dev_object, program_scale_impacts$credit_object))
  program_scale_impacts$program_total = sum_list(list(program_scale_impacts$net_offset_gains, program_scale_impacts$net_dev_losses))
  
  return(program_scale_impacts)
}


calc_site_scale_impacts <- function(current_intervention_simulation_outputs, current_intervention_site_sets, current_collate_type, 
                                    site_scale_cfacs_to_use, summed_site_features_at_intervention_to_use, site_scale_outcomes_to_use,  
                                    simulation_params, feature_params, use_offset_metric){
  
  current_pool = unlist(current_intervention_simulation_outputs$internal_site_indexes)
  if (length(current_pool) == 0){
    return(NULL)
  }
  
  collated_object <- assess_gains_degs(current_intervention_site_scale_outcomes = site_scale_outcomes_to_use[current_pool],
                                      current_intervention_cfacs = site_scale_cfacs_to_use[current_pool],
                                      current_intervention_summed_site_features = summed_site_features_at_intervention_to_use[current_pool],
                                      current_intervention_yrs = unlist(current_intervention_simulation_outputs$intervention_yrs),
                                      current_collate_type, 
                                      simulation_params, feature_params,
                                      time_steps = simulation_params$time_steps, 
                                      use_offset_metric)
  
  collated_object$grouped_gains_degs = group_gains_degs(collated_object, current_intervention_site_sets)
  collated_object$summed_gains_degs = sum_gains_degs(collated_object$grouped_gains_degs)
  
  
  collated_object$internal_site_indexes = current_intervention_site_sets
  collated_object$intervention_yrs = current_intervention_simulation_outputs$intervention_yrs
  
  return(collated_object)
}


sum_gains_degs <- function(grouped_gains_degs){
  summed_gains_degs <- lapply(seq_along(grouped_gains_degs), 
                              function(i) (lapply(seq_along(grouped_gains_degs[[i]]), function(j) Reduce('+', grouped_gains_degs[[i]][[j]]))))
  
  names(summed_gains_degs) = names(grouped_gains_degs) 
  return(summed_gains_degs)
}


group_gains_degs <- function(collated_object, internal_site_indexes){ 
  
  grouped_gains_degs <- lapply(seq_along(collated_object), 
                               function(i) (lapply(seq_along(internal_site_indexes), 
                                                   function(j) collated_object[[i]][which(unlist(internal_site_indexes) %in% internal_site_indexes[[j]])])))
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


assess_gains_degs <- function(current_intervention_site_scale_outcomes, current_intervention_cfacs, current_intervention_summed_site_features, 
                              current_intervention_yrs, current_intervention_collate_type, simulation_params, feature_params, time_steps, use_offset_metric){
  collated_object = list()
  
  site_num = length(current_intervention_site_scale_outcomes)
  impact_trajectories = lapply(seq(site_num), function(i) matrix(current_intervention_site_scale_outcomes[[i]][current_intervention_yrs[i]:time_steps], ncol = 1))
  avoided_loss = lapply(seq(site_num), function(i) rep(current_intervention_summed_site_features[[i]], length(current_intervention_cfacs[[i]])) - current_intervention_cfacs[[i]])
  rest_gains = lapply(seq(site_num), function(i) impact_trajectories[[i]] - rep(current_intervention_summed_site_features[[i]], length(impact_trajectories[[i]])))
  
  net_gains = mapply('-', impact_trajectories, current_intervention_cfacs, SIMPLIFY = FALSE)
  
  collated_object = list(net_gains, avoided_loss, rest_gains)
  collated_object <- lapply(seq_along(collated_object),
                            function(i) lapply(seq_along(collated_object[[i]]), 
                                               function(j) merge_vectors(array(0, time_steps), collated_object[[i]][[j]], current_intervention_yrs[j])))
  
  names(collated_object) = c('nets', 'avoided_loss', 'rest_gains')
  
  
  if ((current_intervention_collate_type == 'offsets_object') | (current_intervention_collate_type == 'offset_bank_object')){
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
  sites_used = lapply(seq_along(simulation_outputs$interventions), function(i) find_current_sites_used(simulation_outputs$interventions[[i]]$internal_site_indexes))
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
sum_sites <- function(site_features, use_offset_metric, user_transform_function, transform_params){
  
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
      #summed_list <- list(Reduce('+', list_to_sum[sets_to_use]))
      summed_list <- Reduce('+', list_to_sum[sets_to_use])
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


assess_NNL <- function(current_impacts, intervention_yr){
  
  NNL_yr = vector()
  if (length(current_impacts) == 0){
    return(NNL_yr)
  }
  
  for (yr_to_test in seq_along(current_impacts)){
    NNL_flag = all(current_impacts[yr_to_test:length(current_impacts)] > 0)
    if (NNL_flag == TRUE){
      NNL_yr = yr_to_test - intervention_yr
    }
  }
  
  return(NNL_yr)
}


# assess_collated_NNL <- function(assess_type, impacts, intervention_yrs_to_use, internal_site_indexes){
# 
#   browser()
#   
#   if (length(impacts) == 0){
#     return(list())
#   }
#   
# #   if (assess_type == 'site_scale'){
# #     
# #     internal_site_indexes_to_use = match(unlist(lapply(seq_along(internal_site_indexes), function(i) internal_site_indexes[[i]][[1]])), unlist(internal_site_indexes))
# #     intervention_yrs_to_use = intervention_yrs_to_use[internal_site_indexes_to_use]
# #   } 
#   
#   NNL_object <- list()
#   NNL_absolute = assess_NNL(impacts)
#   
#   if (length(NN_absolute) > 0){
#     NNL_object$NNL = NNL_absolute - intervention_yrs_to_use
#   } else {
#   }
#   
#   if (length(NNL_object$NNL) > 0){
#     NNL_object$NNL_success = length(unlist(NNL_object$NNL))/length(NNL_object$NNL)
#     NNL_object$NNL_mean = mean(unlist(NNL_object$NNL))
#   } else {
#     NNL_object$NNL_mean = vector()
#   }
#   
#   return(NNL_object)
# }


assess_fractional_loss <- function(net_vals, NNL_yr){
  
  fractional_loss = list()
  if (length(net_vals) == 0){
    return(fractional_loss)
  } 
  
  net_vals = unlist(net_vals)
  sc_factor = net_vals[1]
  
  if ((sc_factor == 0)){
    return(fractional_loss)
  } else {
    
    if (length(net_vals) > 0){
      fractional_loss$total_loss = 1 - net_vals[length(net_vals)]/net_vals[1]
    }
    if (length(unlist(NNL_yr)) > 0){
      fractional_loss$NNL_loss = 1 - net_vals[unlist(NNL_yr)]/net_vals[1]
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





