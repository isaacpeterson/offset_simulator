collate_simulation_outputs <- function(input_data_object, simulation_params, background_cfacs_object, scenario_ind, realisation_ind){
  
  current_data_dir = write_folder(paste0(input_data_object$global_params$output_folder, 
                                         'scenario_', formatC(scenario_ind, width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
                                         '/realisation_', formatC(realisation_ind, width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), '/'))
  
  file_prefix = paste0(input_data_object$global_params$collated_folder,
                       'collated_scenario_',  formatC(scenario_ind, width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                       '_realisation_', formatC(realisation_ind, width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"))
  
  simulation_outputs = readRDS(paste0(current_data_dir, 'realisation_',
                                      formatC(realisation_ind, width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                                      '_outputs.rds'))
  
  intervention_object <- run_pre_collate_routines(simulation_outputs, input_data_object, simulation_params, current_data_dir)
  
  collated_object <- run_collate_routines(intervention_object, 
                                          simulation_outputs,
                                          input_data_object,
                                          background_cfacs_object$background_cfacs,
                                          simulation_params,
                                          current_data_dir, 
                                          file_prefix,
                                          use_offset_metric = FALSE)
  
  if (simulation_params$use_offset_metric == TRUE){
    run_collate_routines(intervention_object, 
                         simulation_outputs,
                         input_data_object,
                         background_cfacs_object$user_metric_background_cfacs,
                         simulation_params,
                         current_data_dir, 
                         file_prefix,
                         use_offset_metric = TRUE)
  }
  
}



run_collate_routines <- function(intervention_object, simulation_outputs, input_data_object, background_cfacs,
                                 simulation_params, current_data_dir, file_prefix, use_offset_metric){
  
  if (use_offset_metric == FALSE){
    flog.info('building site scale counterfactuals - this may take a while')
    features_to_collate = seq(input_data_object$global_params$feature_num)
  } else {
    flog.info('building counterfactuals for user metric - this may take a while')
    features_to_collate = 1
  }
  
  if (intervention_object$intervention_flag == TRUE){
    
    intervention_object$summed_site_features_at_intervention = sum_sites(intervention_object$site_features_at_intervention_set, 
                                                                         use_offset_metric, 
                                                                         input_data_object$global_params$user_transform_function, 
                                                                         simulation_params$transform_params)
    
    intervention_object$site_scale_cfacs = vector('list', length(input_data_object$site_characteristics$land_parcels))
    intervention_object$site_scale_cfacs[intervention_object$intervention_pool] = collate_cfacs(intervention_object$site_features_at_intervention_set[intervention_object$intervention_pool],
                                                                                                simulation_params, 
                                                                                                input_data_object$feature_params,
                                                                                                input_data_object$global_params,
                                                                                                input_data_object$feature_dynamics[intervention_object$intervention_pool],
                                                                                                input_data_object$feature_dynamics_modes[intervention_object$intervention_pool],
                                                                                                input_data_object$site_element_index_key[intervention_object$intervention_pool],
                                                                                                intervention_object$projection_yrs_pool,
                                                                                                intervention_yrs = unlist(intervention_object$intervention_yrs_pool),
                                                                                                intervention_object$site_num_remaining_pool,
                                                                                                cfac_type = 'site_scale',
                                                                                                object_type = intervention_object$object_name_pool, 
                                                                                                use_cfac_type_in_sim = TRUE, 
                                                                                                condition_class_bounds = input_data_object$feature_params$condition_class_bounds, 
                                                                                                use_offset_metric, 
                                                                                                input_data_object$global_params$user_transform_function)
    
  }
  
  for (feature_ind in features_to_collate){
    
    collated_object = list()
    
    if (use_offset_metric == FALSE){
      flog.info('collating feature %s time series data', feature_ind)
      site_scale_outcomes = sum_data_stack(current_data_dir, 
                                           file_pattern = paste0('feature_', formatC(input_data_object$global_params$features_to_use_in_simulation[feature_ind],
                                                                                     width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0")), 
                                           input_data_object$global_params$time_steps)
      
      background_cfacs_to_use = select_subset(background_cfacs, feature_ind)
      
    } else {
      flog.info('collating metric')
      site_scale_outcomes = sum_data_stack(current_data_dir, file_pattern = paste0('metric_'), input_data_object$global_params$time_steps)
      background_cfacs_to_use = background_cfacs
    }
    
    collated_object$landscape_scale <- run_landscape_scale_routines(background_cfacs_to_use, site_scale_outcomes, input_data_object$global_params$threshold_control)
    
    if (intervention_object$intervention_flag == TRUE){
      
      if (use_offset_metric == FALSE){
        
        summed_site_features_at_intervention = select_subset(intervention_object$summed_site_features_at_intervention, feature_ind)
        
        site_scale_cfacs = select_subset(intervention_object$site_scale_cfacs, feature_ind) 
      } else {
        summed_site_features_at_intervention = intervention_object$summed_site_features_at_intervention
        site_scale_cfacs = intervention_object$site_scale_cfacs
      }
      
      collated_object$site_scale <- run_site_scale_routines(intervention_object, 
                                                            summed_site_features_at_intervention,
                                                            site_scale_cfacs,
                                                            site_scale_outcomes,
                                                            intervention_object$intervention_yrs_pool, 
                                                            simulation_outputs,  
                                                            background_cfacs_to_use,
                                                            simulation_params, 
                                                            input_data_object$feature_params, 
                                                            input_data_object$global_params, 
                                                            use_offset_metric)
      
      collated_object$program_scale <- run_program_scale_routines(collated_object$site_scale$impacts, 
                                                    site_scale_cfacs, 
                                                    site_scale_outcomes, 
                                                    intervention_object$intervention_yrs_pool, 
                                                    simulation_outputs, 
                                                    background_cfacs_to_use, 
                                                    input_data_object$global_params$threshold_control)
    }
    
    
    if (use_offset_metric == FALSE){
      collated_filename = paste0(file_prefix, '_feature_',
                                 formatC(input_data_object$global_params$features_to_use_in_simulation[feature_ind], 
                                         width = input_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), '.rds')
    }  else {
      collated_filename = paste0(file_prefix, '_metric', '.rds')
    }
    saveRDS(collated_object, collated_filename)
  }
  
  return(collated_object)
}


run_landscape_scale_routines <- function(background_cfacs_to_use, site_scale_outcomes_to_use, threshold_control){
  
  landscape_scale_outcome = sum_list(site_scale_outcomes_to_use)
  net_landscape_cfac = sum_list(background_cfacs_to_use)
  landscape_scale_impact = mapply('-', landscape_scale_outcome, net_landscape_cfac)
  
  landscape_scale = list()

  landscape_scale$cfacs$net_cfac = net_landscape_cfac
  landscape_scale$impacts$net_impact = list(landscape_scale_impact)
  landscape_scale$outcomes$net_outcome = landscape_scale_outcome
  landscape_scale$loss_characteristics = assess_loss(current_impacts = landscape_scale_impact, 
                                                     current_outcomes = unlist(landscape_scale_outcome), 
                                                     intervention_yr = 1, 
                                                     threshold_control)
  
  return(landscape_scale)
}



run_site_scale_routines <- function(intervention_object, summed_site_features_at_intervention, site_scale_cfacs, site_scale_outcomes, intervention_yrs_pool, simulation_outputs,  background_cfacs_to_use,
                                    simulation_params, feature_params,  global_params, use_offset_metric){
  
  site_scale = list()
  
  site_scale$impacts = setNames(vector('list', length(simulation_outputs$interventions)), 
                                names(simulation_outputs$interventions))
  
  interventions_to_use = which(as.vector(unlist(lapply(intervention_object$grouped_intervention_pool, 'length') ) > 0 ))

  site_scale$impacts[interventions_to_use] = lapply(interventions_to_use, 
                                                    function(i) calc_intervention_impacts(current_intervention_simulation_outputs = simulation_outputs$interventions[[i]], 
                                                                                          current_intervention_site_sets = simulation_outputs$index_object$site_indexes_used[[i]], 
                                                                                          current_collate_type = names(simulation_outputs$interventions)[[i]], 
                                                                                          site_scale_cfacs[ unlist(intervention_object$grouped_intervention_pool[[i]]) ], 
                                                                                          summed_site_features_at_intervention[ unlist(intervention_object$grouped_intervention_pool[[i]]) ], 
                                                                                          site_scale_outcomes[ unlist(intervention_object$grouped_intervention_pool[[i]]) ],  
                                                                                          simulation_params, 
                                                                                          feature_params, 
                                                                                          global_params,
                                                                                          use_offset_metric))
  
  
  site_scale$net_impacts <- list(calc_net_site_scale_impacts(site_scale$impacts$offsets_object$nets,
                                                                             site_scale$impacts$development_object$nets))
  
  #additional list nesting is to maintain structure for collate routines
  site_scale$outcomes = list(site_scale_outcomes)
  site_scale$summed_site_features_at_intervention = list(summed_site_features_at_intervention)
  site_scale$site_scale_cfacs = list(site_scale_cfacs)
  #     site_scale$NNL = assess_site_scale_NNL(assess_type = 'site_scale', 
  #                                                          impacts = site_scale_impacts$net_impacts, 
  #                                                          intervention_yrs_to_use = collated_object$collated_offsets$intervention_yrs, 
  #                                                          site_indexes = simulation_outputs$index_object$site_indexes_used$offsets)
  return(site_scale)
}


run_program_scale_routines <- function(site_scale_impacts, site_scale_cfacs, site_scale_outcomes, intervention_yrs_pool, 
                                       simulation_outputs,  background_cfacs_to_use, threshold_control){
  
  program_scale = list()
  
  program_scale$cfacs = collate_program_scale_cfacs(site_scale_cfacs, 
                                                    simulation_outputs$interventions, 
                                                    background_cfacs_to_use, 
                                                    intervention_yrs_pool)
  program_scale$outcomes <- collate_program_scale_outcomes(simulation_outputs, site_scale_outcomes)
  program_scale$impacts <- collate_program_scale_impacts(site_scale_impacts)
  
  program_scale$loss_characteristics = assess_loss(current_impacts = unlist(program_scale$impacts$net_impacts, recursive = FALSE), 
                                                   current_outcomes = unlist(program_scale$outcomes$net_outcome, recursive = FALSE), 
                                                   intervention_yr = 1, 
                                                   threshold_control)
  program_scale$sites_used = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                             function(i) list(length(simulation_outputs$interventions[[i]]$site_indexes))),
                                      names(simulation_outputs$interventions))

  return(program_scale)
}



run_pre_collate_routines <- function(simulation_outputs, input_data_object, simulation_params, current_data_dir, use_offset_metric){
  
  intervention_object = list()
  
  intervention_object$grouped_intervention_pool = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                                                  function(i) simulation_outputs$interventions[[i]]$site_indexes), names(simulation_outputs$interventions))
  intervention_object$intervention_yrs_pool = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                                              function(i) simulation_outputs$interventions[[i]]$intervention_yrs), names(simulation_outputs$interventions))
  intervention_object$object_name_pool = unlist(lapply(seq_along(simulation_outputs$interventions), 
                                                       function(i) rep(names(simulation_outputs$interventions)[i], length(intervention_object$grouped_intervention_pool[[i]]))))
  intervention_object$site_num_remaining_pool = unlist(lapply(seq_along(simulation_outputs$interventions), 
                                                              function(i) simulation_outputs$interventions[[i]]$site_num_remaining))
  
  intervention_object$intervention_pool = as.vector(unlist(intervention_object$grouped_intervention_pool))
  
  if (length(intervention_object$intervention_pool) == 0){
    intervention_object$intervention_flag = FALSE
    return(intervention_object)
    
  } else {
    intervention_object$intervention_flag = TRUE
    
    intervention_object$projection_yrs_pool = lapply(seq_along(intervention_object$intervention_pool), 
                                                     function(i) lapply(seq(input_data_object$global_params$feature_num), 
                                                                        function(j) rep(list(intervention_object$intervention_pool[i]), 
                                                                                        length(input_data_object$feature_dynamics_modes[[ intervention_object$intervention_pool[i] ]][[j]])) ))
    
    site_features_at_intervention_set = vector('list', length(input_data_object$site_characteristics$land_parcels))
    
    for (current_feature_ind in seq(input_data_object$global_params$feature_num)){
      site_features_at_intervention = build_site_features_at_intervention(length(input_data_object$site_characteristics$land_parcels), 
                                                                          current_data_dir, 
                                                                          intervention_object$intervention_pool, 
                                                                          unlist(intervention_object$intervention_yrs_pool), 
                                                                          input_data_object$global_params, 
                                                                          current_feature_ind, 
                                                                          input_data_object$global_params$numeric_placeholder_width)
      
      site_features_at_intervention_set = lapply(seq(length(input_data_object$site_characteristics$land_parcels)), 
                                                 function(i) append(site_features_at_intervention_set[[i]], 
                                                                    site_features_at_intervention[[i]]))
    }
    
    intervention_object$site_features_at_intervention_set = site_features_at_intervention_set
    return(intervention_object)
  }
}

select_subset <- function(current_object, subset_ind, output_type){
  set_lengths = lapply(current_object, 'length')
  sets_to_use = which(unlist(set_lengths) > 0)
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



build_site_features_at_intervention <- function(site_num, current_data_dir, intervention_pool, intervention_yrs_pool, global_params, feature_ind, numeric_placeholder_width){
  site_features_at_intervention = vector('list', site_num)
  site_features_at_intervention[intervention_pool] = build_site_layer_stack(current_data_dir, 
                                                                            file_pattern = paste0('feature_', formatC(global_params$features_to_use_in_simulation[feature_ind], width = numeric_placeholder_width, format = "d", flag = "0")), 
                                                                            intervention_pool,
                                                                            intervention_yrs_pool, 
                                                                            numeric_placeholder_width)
  return(site_features_at_intervention)
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



nest_list <- function(list_a){
  nested_list = setNames(lapply(seq_along(list_a), function(j) list(list_a[[j]])), names(list_a))
  return(nested_list)
}




run_bind_collated_realisations_routines <- function(collated_filenames){

  realisation_num = length(collated_filenames)
  
  collated_group = lapply(seq(realisation_num), function(i) readRDS(collated_filenames[[i]]))
  
  nested_group = lapply(seq_along(collated_group), 
                        function(i) setNames(lapply(seq_along(collated_group[[i]]), 
                                                    function(j) setNames( lapply(seq_along(collated_group[[i]][[j]]), 
                                                                                 function(k) nest_list(collated_group[[i]][[j]][[k]]) ), 
                                                                          names(collated_group[[i]][[j]])) ),
                                             names(collated_group[[i]])))
  
  scale_names = c('program_scale', 'landscape_scale')
  collated_realisation_object = setNames(lapply(seq_along(scale_names), 
                                                function(i) bind_current_group(nested_group, bind_type = scale_names[i], realisation_num)), 
                                         scale_names)
  collated_realisation_object$realisation_num = realisation_num
  
  return(collated_realisation_object)
}

bind_current_group <- function(nested_group, bind_type, realisation_num){
  
  for (realisation_ind in seq(realisation_num)){ 
    
    if (bind_type == 'site_scale'){
      current_collated_realisation = nested_group[[realisation_ind]]$site_scale
    } else if (bind_type == 'program_scale'){
      current_collated_realisation = nested_group[[realisation_ind]]$program_scale
    } else if (bind_type == 'landscape_scale'){
      current_collated_realisation = nested_group[[realisation_ind]]$landscape_scale
    } 
    
    if (realisation_ind == 1){
      collated_realisation_group = current_collated_realisation
    } else {
      collated_realisation_group <- setNames(lapply(seq_along(collated_realisation_group), 
                                                    function(i) append_collated_realisation(collated_realisation_group[[i]], 
                                                                                            current_collated_realisation[[i]], 
                                                                                            realisation_ind)), 
                                             names(current_collated_realisation))
      
    }
    
  }
  return(collated_realisation_group)
  
}




append_collated_realisation <- function(collated_group, current_collated_set, realisation_ind){
  
  net_names = union(names(collated_group), names(current_collated_set))
  
  collated_group = expand_current_collated_realisation(expand_type = 'set', collated_group, net_names, realisation_ind)
  current_collated_set = expand_current_collated_realisation(expand_type = 'single', current_collated_set, net_names, realisation_ind)
  
  collated_group = setNames(lapply(seq_along(collated_group), function(i) append(collated_group[[i]], current_collated_set[[i]])), net_names)
  
  return(collated_group)
  
}




expand_current_collated_realisation <- function(expand_type, collated_object, net_names, realisation_ind){
  
  if (expand_type == 'set'){
    list_length = realisation_ind - 1
  } else {
    list_length = 1
  }
  
  common_inds = match(names(collated_object), net_names)
  new_inds = setdiff(seq_along(net_names), common_inds)
  
  expanded_collated_object = vector('list', length(net_names))
  expanded_collated_object[common_inds] = collated_object
  expanded_collated_object[new_inds] = lapply(seq_along(new_inds), function(i) vector('list', list_length))
  names(expanded_collated_object) = net_names
  
  return(expanded_collated_object)
}



merge_lists <- function(list_a, list_b, merge_indexes){
  merged_list <- lapply(seq_along(list_a),
                        function(i) merge_vectors(list_a[[i]], list_b[[i]], merge_indexes[i]))
  return(merged_list)
}


collate_program_scale_cfacs <- function(site_scale_cfacs, interventions, background_cfacs, intervention_yrs_pool){
  
  program_scale_cfacs = setNames(lapply(seq_along(interventions), 
                                        function(i) merge_lists(background_cfacs[unlist(interventions[[i]]$site_indexes)], 
                                                                site_scale_cfacs[unlist(interventions[[i]]$site_indexes)], unlist(intervention_yrs_pool[[i]]))), 
                                 names(interventions))
  program_scale_cfacs$net_cfac <- sum_list(unlist(program_scale_cfacs, recursive = FALSE))

  program_scale_cfacs$net_offsets <- sum_list(unlist(program_scale_cfacs[match(c('offsets_object', 'offset_bank_object'), 
                                                                               names(program_scale_cfacs))], recursive = FALSE))
  program_scale_cfacs$net_devs <- sum_list(unlist(program_scale_cfacs[match(c('development_object', 'development_credit_object'), 
                                                                            names(program_scale_cfacs))], recursive = FALSE))
  
  return(program_scale_cfacs)
  
}




collate_cfacs <- function(site_features_group, simulation_params, feature_params, global_params, feature_dynamics, feature_dynamics_modes, 
                          site_element_index_key, projection_yrs, intervention_yrs, site_num_remaining_pool, cfac_type, 
                          object_type, use_cfac_type_in_sim, condition_class_bounds, use_offset_metric, user_transform_function){
  
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
                                            time_horizon = (global_params$time_steps - 1), 
                                            length(site_features_group))
  } else if (cfac_type == 'site_scale'){
    time_horizons = generate_time_horizons(project_type = 'current', 
                                           yr = global_params$time_steps, 
                                           unlist(intervention_yrs),
                                           time_horizon = (global_params$time_steps - 1), 
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
    
    ###### check if nrow = 1 is necessary or to use drop = FALSE
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
                                                                                                                          site_element_index_key = vector()))), nrow = 1)) ) )
    
    
  } else {
    ###### check if nrow = 1 is necessary or to use drop = FALSE
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
                                           function(i) sum_list(site_scale_outcomes[unlist(simulation_outputs$interventions[[i]]$site_indexes)])), 
                                    names(simulation_outputs$interventions))
  
  program_scale_outcomes$net_outcome <- sum_list(unlist(program_scale_outcomes, recursive = FALSE))
  program_scale_outcomes$net_offsets <- sum_list(unlist(program_scale_outcomes[match(c('offsets_object', 'offset_bank_object'), names(program_scale_outcomes))], recursive = FALSE))
  program_scale_outcomes$net_devs <- sum_list(unlist(program_scale_outcomes[match(c('development_object', 'development_credit_object'), names(program_scale_outcomes))], recursive = FALSE))
  
  return(program_scale_outcomes)
}


collate_program_scale_impacts <- function(site_scale_impacts){
  
  program_impacts = setNames(lapply(seq_along(site_scale_impacts), function(i) site_scale_impacts[[i]]$nets), 
                             names(site_scale_impacts))
  
  program_impacts$net_offset_gains = sum_list(append(program_impacts$offsets_object, program_impacts$offset_bank_object))
  program_impacts$net_dev_losses = sum_list(append(program_impacts$development_object, program_impacts$development_credit_object))
  program_impacts$net_impacts = sum_list(append(program_impacts$net_offset_gains, program_impacts$net_dev_losses))
  
  return(program_impacts)
}


calc_intervention_impacts <- function(current_intervention_simulation_outputs, current_intervention_site_sets, current_collate_type, 
                                      site_scale_cfacs_to_use, summed_site_features_at_intervention_to_use, site_scale_outcomes_to_use,  
                                      simulation_params, feature_params, global_params, use_offset_metric){
  
  site_scale_impacts <- calc_site_scale_impacts(current_intervention_site_scale_outcomes = site_scale_outcomes_to_use,
                                                current_intervention_cfacs = site_scale_cfacs_to_use,
                                                current_intervention_summed_site_features = summed_site_features_at_intervention_to_use,
                                                current_intervention_yrs = unlist(current_intervention_simulation_outputs$intervention_yrs),
                                                current_collate_type, 
                                                simulation_params, 
                                                feature_params,
                                                time_steps = global_params$time_steps, 
                                                use_offset_metric)
  
  site_scale_impacts$grouped_impacts = group_site_scale_impacts(site_scale_impacts, current_intervention_site_sets)
  site_scale_impacts$summed_impacts = setNames(lapply(seq_along(site_scale_impacts$grouped_impacts), 
                                                      function(i) lapply(seq_along(site_scale_impacts$grouped_impacts[[i]]), 
                                                                         function(j) Reduce('+', site_scale_impacts$grouped_impacts[[i]][[j]]))), 
                                               site_scale_impacts$grouped_impacts)
  
  site_scale_impacts$site_indexes = list(current_intervention_site_sets)
  site_scale_impacts$intervention_yrs = list(current_intervention_simulation_outputs$intervention_yrs)

  return(site_scale_impacts)
  
}


sum_gains_degs <- function(grouped_gains_degs){
  
  summed_impacts <- setNames(lapply(seq_along(grouped_gains_degs), 
                                    function(i) (lapply(seq_along(grouped_gains_degs[[i]]), function(j) Reduce('+', grouped_gains_degs[[i]][[j]])))), 
                             names(grouped_gains_degs))
  return(summed_impacts)
}


group_site_scale_impacts <- function(collated_object, site_indexes){ 
  
  grouped_gains_degs <- setNames(lapply(seq_along(collated_object), 
                                        function(i) (lapply(seq_along(site_indexes), 
                                                            function(j) collated_object[[i]][which(unlist(site_indexes) %in% site_indexes[[j]])]))), 
                                 names(collated_object))
  
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


calc_site_scale_impacts <- function(current_intervention_site_scale_outcomes, current_intervention_cfacs, current_intervention_summed_site_features, 
                                    current_intervention_yrs, current_intervention_collate_type, simulation_params, feature_params, time_steps, use_offset_metric){
  
  site_num = length(current_intervention_site_scale_outcomes)
  
  impact_trajectories = lapply(seq(site_num), function(i) current_intervention_site_scale_outcomes[[i]][current_intervention_yrs[i]:time_steps, , drop = FALSE])
  
  site_features_at_intervention_block = lapply(seq(site_num), function(i) array(rep(current_intervention_summed_site_features[[i]], length(current_intervention_cfacs[[i]])), dim(current_intervention_cfacs[[i]])))  
  
  avoided_loss = mapply('-', site_features_at_intervention_block, current_intervention_cfacs, SIMPLIFY = FALSE)
  rest_gains = mapply('-', impact_trajectories, site_features_at_intervention_block, SIMPLIFY = FALSE)
  net_gains = mapply('-', impact_trajectories, current_intervention_cfacs, SIMPLIFY = FALSE)
  
  collated_object = list(net_gains, avoided_loss, rest_gains)
  collated_object <- setNames(lapply(seq_along(collated_object), 
                                     function(i) lapply(seq(site_num), 
                                                        function(j) merge_vectors(array(0, c(time_steps, 1)), collated_object[[i]][[j]], current_intervention_yrs[j]))), 
                              c('nets', 'avoided_loss', 'rest_gains'))
  
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



# determine cumulative value of all sites within parcel feature_layers for multiple features
sum_sites <- function(site_features, use_offset_metric, user_transform_function, transform_params){
  
  summed_site_features = vector('list', length(site_features))
  
  set_lengths = lapply(site_features, 'length')
  sets_to_use = which(unlist(set_lengths) > 0)
  
  if (use_offset_metric == TRUE){
    site_features[sets_to_use] <- lapply(sets_to_use,  function(i) lapply(seq_along(site_features[[i]]), function(j) do.call(cbind, site_features[[i]][[j]])))
    
    summed_site_features[sets_to_use] = lapply(sets_to_use, function(i) sum(user_transform_function(site_features[[i]], transform_params)))
    
  } else {
    summed_site_features[sets_to_use] <- lapply(sets_to_use, function(i) matrix(do.call(cbind, sum_site_features(site_features[[i]])), nrow = 1))
  }
  
  return(summed_site_features)
  
}

calc_net_site_scale_impacts <- function(collated_site_scale_offsets, collated_site_scale_devs){
  
  if ((length(collated_site_scale_offsets) > 0) & (length(collated_site_scale_devs) > 0)){
    net_impacts <- mapply('+', collated_site_scale_offsets, collated_site_scale_devs, SIMPLIFY = FALSE)
  } else {
    net_impacts = list()
  }
  
  return(net_impacts)
}


sum_list <- function(list_to_sum){

  list_lengths = lapply(list_to_sum, 'length')
  summed_list = vector('list', 1)
  
  if (length(list_lengths) == 0){
    return(summed_list)
  } else {
    sets_to_use = as.vector(which(unlist(list_lengths) > 0))
    if (length(sets_to_use) == 0){
      return(summed_list)
    } else {
      summed_list[[1]] <- Reduce('+', list_to_sum[sets_to_use])
      return(summed_list)
    }
  }
  
}



# assess_collated_NNL <- function(assess_type, impacts, intervention_yrs_to_use, site_indexes){
# 
#   
#   if (length(impacts) == 0){
#     return(list())
#   }
#   
# #   if (assess_type == 'site_scale'){
# #     
# #     site_indexes_to_use = match(unlist(lapply(seq_along(site_indexes), function(i) site_indexes[[i]][[1]])), unlist(site_indexes))
# #     intervention_yrs_to_use = intervention_yrs_to_use[site_indexes_to_use]
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

assess_loss <- function(current_impacts, current_outcomes, intervention_yr, threshold_control){
  
  loss_characteristics = setNames(vector('list', 3), c('NNL', 'loss_at_final', 'loss_at_NNL'))
  loss_characteristics$NNL = list(assess_NNL(current_impacts, intervention_yr, threshold_control))
  sc_factor = current_outcomes[1]

  if (sc_factor == 0){
    return(loss_characteristics)
  } else {
    loss_characteristics$loss_at_final = list(1 - current_outcomes[length(current_outcomes)]/sc_factor)
    if (length(loss_characteristics$NNL) > 0){
      loss_characteristics$loss_at_NNL = list(1 - current_outcomes[unlist(loss_characteristics$NNL)]/sc_factor)
    }
    return(loss_characteristics)
  }
  
}


assess_NNL <- function(current_impacts, intervention_yr, threshold_control){
  
  if (length(current_impacts) == 0){
    return(NULL)
  } else {
    current_impacts[abs(current_impacts) < threshold_control] = 0

    for (yr_to_test in seq_along(current_impacts)){
      NNL_flag = all(current_impacts[yr_to_test:length(current_impacts)] >= 0)
      if (NNL_flag == TRUE){
        break
      }
    }
    
    if (NNL_flag == TRUE){
      NNL_yr = (yr_to_test - intervention_yr) + 1
      return(NNL_yr)
    } else {
      return(NULL)
    }
    
  }
}


assess_fractional_loss <- function(net_vals, NNL_yr){
  
  if (length(net_vals) == 0){
    return(vector('list', 2))
  } else {
    
    net_vals_to_use = unlist(net_vals)
    sc_factor = net_vals_to_use[1]
    
    if (sc_factor == 0){
      return(list())
    } else {
      fractional_loss = list()
      fractional_loss$total_loss = 1 - net_vals_to_use[length(net_vals_to_use)]/sc_factor
      
      if (length(NNL_yr) > 0){
        fractional_loss$NNL_loss = 1 - net_vals_to_use[unlist(NNL_yr)]/sc_factor
      }
      
    }
    
    return(fractional_loss)
  }
}


select_cfac_params <- function(object_type, simulation_params){
  
  if ((object_type == "development_object") | (object_type == "development_credit_object") | (object_type == "unregulated_loss_object")){
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



