#use_cfac_type_in_sim = TRUE
run_collate_routines <- function(simulation_outputs, data_object, simulation_input_object, current_data_dir, current_simulation_params, feature_params, realisation_ind, feature_ind, use_offset_metric){

  current_feature_dynamics_initial = select_nested_subset(nested_object = simulation_input_object$feature_dynamics, feature_ind, output_type = 'nested')
  current_initial_feature_layer = select_nested_subset(nested_object = data_object$site_feature_layers_initial, feature_ind, output_type = 'nested')
  current_feature_dynamics_modes_initial = select_nested_subset(nested_object = simulation_input_object$feature_dynamics_modes, feature_ind, output_type = 'nested')
  collated_data = collate_simulation_outputs(simulation_outputs, current_data_dir, data_object, current_feature_dynamics_initial, current_feature_dynamics_modes_initial, current_initial_feature_layer, 
                                             current_simulation_params, feature_params, use_cfac_type_in_sim = TRUE, parcels, feature_ind, use_offset_metric)
  
  return(collated_data)
  
}


collate_simulation_outputs <- function(simulation_outputs, current_data_dir, data_object, current_feature_dynamics_initial, current_feature_dynamics_modes_initial, 
                                       current_initial_feature_layer, current_simulation_params, feature_params, use_cfac_type_in_sim, parcels, feature_ind, use_offset_metric){
  
  collated_data = list()
  
  collated_data$site_scale$outcomes = sum_data_stack(current_data_dir, 
                                       file_pattern = paste0('feature_', formatC(current_simulation_params$features_to_use_in_offset_calc[feature_ind], width = 3, format = "d", flag = "0")), 
                                       land_parcels = data_object$parcel_characteristics$land_parcels, 
                                       current_simulation_params$time_steps, 
                                       use_offset_metric, 
                                       current_transform_param = current_simulation_params$transform_params[[feature_ind]])
  
  collated_data$collated_devs = run_site_scale_collate_routine(current_simulation_outputs = simulation_outputs$dev_object,
                                                               current_site_sets = simulation_outputs$index_object$site_indexes_used$devs,
                                                               collated_data$site_scale$outcomes,  
                                                               current_data_dir,
                                                               current_feature_dynamics_initial, 
                                                               current_feature_dynamics_modes_initial, 
                                                               collate_type = 'devs', 
                                                               current_simulation_params, 
                                                               feature_params,
                                                               use_cfac_type_in_sim, 
                                                               feature_ind, 
                                                               use_offset_metric)
  
  collated_data$collated_offsets <- run_site_scale_collate_routine(current_simulation_outputs = simulation_outputs$offsets_object,
                                                                   current_site_sets = simulation_outputs$index_object$site_indexes_used$offsets,
                                                                   collated_data$site_scale$outcomes,  
                                                                   current_data_dir, 
                                                                   current_feature_dynamics_initial, 
                                                                   current_feature_dynamics_modes_initial, 
                                                                   collate_type = 'offsets', 
                                                                   current_simulation_params, 
                                                                   feature_params,
                                                                   use_cfac_type_in_sim, 
                                                                   feature_ind, 
                                                                   use_offset_metric)
  
  
  collated_data$collated_dev_credit = run_site_scale_collate_routine(current_simulation_outputs = simulation_outputs$credit_object, 
                                                                     current_site_sets = simulation_outputs$index_object$site_indexes_used$dev_credits,
                                                                     collated_data$site_scale$outcomes,  
                                                                     current_data_dir, 
                                                                     current_feature_dynamics_initial, 
                                                                     current_feature_dynamics_modes_initial, 
                                                                     collate_type = 'dev_credit', 
                                                                     current_simulation_params, 
                                                                     feature_params,
                                                                     use_cfac_type_in_sim, 
                                                                     feature_ind, 
                                                                     use_offset_metric)
  
  collated_data$collated_offset_bank = run_site_scale_collate_routine(current_simulation_outputs = simulation_outputs$offset_bank_object, 
                                                                      current_site_sets = simulation_outputs$index_object$site_indexes_used$banking,
                                                                      collated_data$site_scale$outcomes,   
                                                                      current_data_dir,
                                                                      current_feature_dynamics_initial, 
                                                                      current_feature_dynamics_modes_initial, 
                                                                      collate_type = 'offset_bank', 
                                                                      current_simulation_params, 
                                                                      feature_params,
                                                                      use_cfac_type_in_sim, 
                                                                      feature_ind, 
                                                                      use_offset_metric)
  
  collated_data$collated_unregulated_loss = run_site_scale_collate_routine(current_simulation_outputs = simulation_outputs$unregulated_loss_object,
                                                                           current_site_sets = simulation_outputs$index_object$site_indexes_used$unregulated,
                                                                           collated_data$site_scale$outcomes,   
                                                                           current_data_dir,
                                                                           current_feature_dynamics_initial, 
                                                                           current_feature_dynamics_modes_initial, 
                                                                           collate_type = 'unregulated_loss', 
                                                                           current_simulation_params, 
                                                                           feature_params,
                                                                           use_cfac_type_in_sim, 
                                                                           feature_ind, 
                                                                           use_offset_metric)
  
  collated_data$site_scale_impacts <- collate_site_scale_impacts(collated_site_scale_offsets = collated_data$collated_offsets$summed_gains_degs$nets,
                                                                 collated_site_scale_devs = collated_data$collated_devs$summed_gains_degs$nets)
  
  
  collated_data$landscape <- calc_landscape_characteristics(collated_data$site_scale$outcomes, current_initial_feature_layer, current_simulation_params, feature_params, use_offset_metric, 
                                                            current_feature_dynamics_initial, current_feature_dynamics_modes_initial, feature_ind)
  
  collated_data$program_outcomes <- collate_program_scale_outcomes(simulation_outputs, collated_data$site_scale$outcomes)
  
  collated_data$program_scale_impacts <- collate_program_scale_impacts(collated_data)
  
  collated_data$program_cfacs = collate_program_cfacs(simulation_outputs, 
                                                      collated_data$landscape$landscape_cfacs, 
                                                      collated_data$collated_offsets, 
                                                      collated_data$collated_devs, 
                                                      collated_data$collated_dev_credit, 
                                                      collated_data$collated_offset_bank, 
                                                      collated_data$collated_unregulated_loss)
  
  collated_data$site_scale_NNL = assess_collated_NNL(assess_type = 'site_scale', 
                                                     impacts = collated_data$site_scale_impacts$net_impacts, 
                                                     offset_yrs_to_use = collated_data$collated_offsets$offset_yrs, 
                                                     site_indexes = simulation_outputs$index_object$site_indexes_used$offsets)
  
  collated_data$program_scale_NNL = assess_collated_NNL(assess_type = 'program', 
                                                        impacts = collated_data$program_scale_impacts$program_total, 
                                                        offset_yrs_to_use = list(1), 
                                                        site_indexes = vector())
  
  collated_data$landscape_scale_NNL = assess_collated_NNL(assess_type = 'landscape', 
                                                          impacts = collated_data$landscape$landscape_impact, 
                                                          offset_yrs_to_use = list(1), 
                                                          site_indexes = vector())
  
  collated_data$net_program_loss = assess_fractional_loss(net_vals = collated_data$program_outcomes$net_outcome, 
                                                          NNL_yr = collated_data$program_scale_NNL$NNL)
  
  collated_data$net_landscape_loss = assess_fractional_loss(net_vals = collated_data$landscape$net_landscape, 
                                                            NNL_yr = collated_data$landscape_scale_NNL$NNL)
  
  collated_data$sites_used = find_sites_used(collated_data)
  
  return(collated_data)
  
}

calc_landscape_characteristics <- function(site_scale_outcomes, current_initial_feature_layer, current_simulation_params, feature_params, 
                                           use_offset_metric, current_feature_dynamics_initial, current_feature_dynamics_modes_initial, feature_ind){

  landscape_object = list()
  
  projection_yrs = lapply(seq_along(current_initial_feature_layer), function(i) rep(list(1), length(current_initial_feature_layer[[i]])))
  
  landscape_cfacs = collate_cfacs(current_simulation_params, feature_params,
                                  current_site_feature_layers = current_initial_feature_layer,
                                  current_feature_dynamics = current_feature_dynamics_initial,
                                  current_feature_dynamics_modes_initial,
                                  projection_yrs, 
                                  current_parcel_num_remaining = vector(),
                                  cfac_type = 'landscape',
                                  collate_type = vector(), 
                                  use_cfac_type_in_sim = FALSE, 
                                  condition_class_bounds = feature_params$condition_class_bounds[feature_ind], 
                                  use_offset_metric, 
                                  current_transform_param = current_simulation_params$transform_params[[feature_ind]])

  landscape_object$landscape_cfacs = landscape_cfacs$cfacs
  landscape_object$net_landscape = sum_list(site_scale_outcomes)
  landscape_object$net_landscape_cfac = sum_list(landscape_cfacs$cfacs)
  landscape_object$landscape_impact = mapply('-', landscape_object$net_landscape, landscape_object$net_landscape_cfac, SIMPLIFY = FALSE)

  return(landscape_object)
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

collate_program_cfacs <- function(simulation_outputs, landscape_cfacs, collated_offsets, collated_devs, collated_dev_credit, 
                                  collated_offset_bank, collated_unregulated_loss){
  
  program_cfacs_object = list()
  program_cfacs_object$offset_cfacs <- merge_lists(landscape_cfacs[unlist(simulation_outputs$offsets_object$site_indexes)], collated_offsets$cfacs, unlist(simulation_outputs$offsets_object$offset_yrs))
  program_cfacs_object$dev_cfacs <- merge_lists(landscape_cfacs[unlist(simulation_outputs$dev_object$site_indexes)], collated_devs$cfacs, unlist(simulation_outputs$dev_object$offset_yrs))
  program_cfacs_object$dev_credit_cfacs <- merge_lists(landscape_cfacs[unlist(simulation_outputs$credit_object$site_indexes)], collated_dev_credit$cfacs, unlist(simulation_outputs$credit_object$offset_yrs))
  program_cfacs_object$offset_bank_cfacs <- merge_lists(landscape_cfacs[unlist(simulation_outputs$offset_bank_object$site_indexes)], collated_offset_bank$cfacs, unlist(simulation_outputs$offset_bank_object$offset_yrs))
  
  #cfac_sums = list(program_cfacs_object$offset_cfacs, program_cfacs_object$dev_cfacs, program_cfacs_object$dev_credit_cfacs, program_cfacs_object$offset_bank_cfacs)
  
  #cfacs_to_use = unlist(lapply(seq_along(cfac_sums), function(i) length(cfac_sums[[i]]))) > 0
  #program_cfacs_object$program_cfac_sum <-  list(Reduce('+', unlist(cfac_sums[cfacs_to_use], recursive = FALSE))
  program_cfacs_object$program_cfac_sum <- sum_list(unlist(program_cfacs_object, recursive = FALSE))
  
  return(program_cfacs_object)
  
}


collate_cfacs <- function(current_simulation_params, feature_params, current_site_feature_layers, current_feature_dynamics, current_feature_dynamics_modes, projection_yrs, 
                          current_parcel_num_remaining, cfac_type, collate_type, use_cfac_type_in_sim, condition_class_bounds, use_offset_metric, current_transform_param){
  
  cfac_params <- select_cfac_type(collate_type, use_cfac_type_in_sim, current_simulation_params, feature_params)
  
  parcel_count = length(current_site_feature_layers)
  
  if (cfac_type == 'landscape'){                    
    time_horizons <- generate_time_horizons(project_type = 'future', 
                                            yr = 1, 
                                            offset_yrs = rep(1, parcel_count), 
                                            time_horizon = (current_simulation_params$time_steps - 1), 
                                            parcel_count)
    
    include_potential_developments = FALSE
    include_potential_offsets = FALSE
    #TODO REINSTATE THIS TO TRUE
    adjust_cfacs_flag = FALSE
    include_unregulated_loss = FALSE

  } else if (cfac_type == 'site_scale'){

    time_horizons = generate_time_horizons(project_type = 'current', 
                                           yr = current_simulation_params$time_steps, 
                                           offset_yrs = projection_yrs, 
                                           time_horizon = (current_simulation_params$time_steps - 1), 
                                           parcel_count)
    
    adjust_cfacs_flag = cfac_params$adjust_cfacs_flag
    include_potential_developments = cfac_params$include_potential_developments
    include_potential_offsets = cfac_params$include_potential_offsets
    include_unregulated_loss = cfac_params$include_unregulated_loss
    
  }
  cfacs_object = list()
  
  if (use_offset_metric == TRUE){
    
    cfacs = lapply(seq_along(current_site_feature_layers), 
                   function(i) list(matrix(sum_cols( scale_scores(unlist(project_features(current_site_feature_layers[i],
                                                                              feature_params$background_projection_type,
                                                                              feature_params$background_update_dynamics_by_differential, 
                                                                              current_feature_dynamics[i],
                                                                              current_feature_dynamics_modes[i],
                                                                              time_horizons[i],
                                                                              current_simulation_params,
                                                                              perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift,
                                                                              time_fill = TRUE,
                                                                              unique_site_vals = feature_params$unique_site_vals,
                                                                              projection_yrs = projection_yrs[i], 
                                                                              condition_class_bounds), 
                                                             recursive = FALSE), current_transform_param )[[1]]), ncol = 1)))
    
  } else {

    cfacs = project_features(current_site_feature_layers,
                             feature_params$background_projection_type,
                             feature_params$background_update_dynamics_by_differential, 
                             feature_dynamics_to_use = current_feature_dynamics,
                             feature_dynamics_modes_to_use = current_feature_dynamics_modes,
                             time_horizons,
                             current_simulation_params,
                             perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift,
                             time_fill = TRUE, 
                             unique_site_vals = feature_params$unique_site_vals,
                             projection_yrs, 
                             condition_class_bounds)
    
  }
  
  if (adjust_cfacs_flag == TRUE){
    cfacs_object$background_cfacs = cfacs
    cfacs = adjust_cfacs(cfacs,
                         include_potential_developments,
                         include_potential_offsets,
                         include_unregulated_loss,
                         current_simulation_params,
                         parcel_num_remaining,
                         time_horizons,
                         offset_yrs, 
                         yr)
  } 
  
  
  cfacs = unlist(cfacs, recursive = FALSE)
  
  if (cfac_type == 'landscape'){
    cfacs_object$cfacs = lapply(seq_along(cfacs), function(i) sum_cols(cfacs[[i]]))
  } else {
    cfacs_object$cfacs = lapply(seq_along(cfacs), function(i) sum_cols(cfacs[[i]]))
  }
  return(cfacs_object)
}


collate_metric_layer <- function(){
  
  projection_yrs = lapply(seq_along(data_object$site_feature_layers_initial), function(i) rep(list(1), length(data_object$site_feature_layers_initial[[i]])))
  
  cfacs = lapply(seq_along(data_object$site_feature_layers_initial), 
                 function(i) sum_cols(user_transform_function(unlist(project_features(data_object$site_feature_layers_initial[i],
                                                                                            feature_params$background_projection_type,
                                                                                            feature_params$background_update_dynamics_by_differential, 
                                                                                            simulation_input_object$feature_dynamics[i],
                                                                                            simulation_input_object$feature_dynamics_modes[i],
                                                                                            time_horizons[i],
                                                                                            current_simulation_params,
                                                                                            perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift,
                                                                                            time_fill = TRUE,
                                                                                            unique_site_vals = feature_params$unique_site_vals,
                                                                                            projection_yrs[i], 
                                                                                            condition_class_bounds = feature_params$condition_class_bounds), recursive = FALSE), 
                                                                    current_simulation_params$transform_params)[[1]]))
  net_cfacs = sum_list(landscape_cfacs)
}


collate_program_scale_outcomes <- function(simulation_outputs, site_scale_outcomes){
  program_scale_outcomes = list()

  program_scale_outcomes$offsets <- sum_list(site_scale_outcomes[unlist(simulation_outputs$offsets_object$site_indexes)])
  program_scale_outcomes$devs <- sum_list(site_scale_outcomes[unlist(simulation_outputs$dev_object$site_indexes)])
  program_scale_outcomes$dev_credit <- sum_list(site_scale_outcomes[unlist(simulation_outputs$credit_object$site_indexes)])
  program_scale_outcomes$offset_bank <- sum_list(site_scale_outcomes[unlist(simulation_outputs$offset_bank$site_indexes)])
  
  program_scale_outcomes$net_offsets <- sum_list(append(program_scale_outcomes$offsets, program_scale_outcomes$offset_bank))
  program_scale_outcomes$net_devs <- sum_list(append(program_scale_outcomes$devs, program_scale_outcomes$dev_credit))
  program_scale_outcomes$net_outcome <- sum_list(append(program_scale_outcomes$net_offsets, program_scale_outcomes$net_devs))

  return(program_scale_outcomes)
}


collate_program_scale_impacts <- function(collated_data){
  program_scale_impacts = list()

  program_scale_impacts$offset_site_gains =  sum_list(collated_data$collated_offsets$summed_gains_degs$nets)
  program_scale_impacts$offset_bank_gains =  sum_list(collated_data$collated_offset_bank$summed_gains_degs$nets)
  program_scale_impacts$dev_site_losses =  sum_list(collated_data$collated_devs$summed_gains_degs$nets)
  program_scale_impacts$dev_credit_losses =  sum_list(collated_data$collated_dev_credit$summed_gains_degs$nets)
  program_scale_impacts$unregulated_loss =  sum_list(collated_data$collated_unregulated_loss$summed_gains_degs$nets)
  
  program_scale_impacts$net_offset_gains = sum_list(append(program_scale_impacts$offset_site_gains, program_scale_impacts$offset_bank_gains))
  program_scale_impacts$net_dev_losses = sum_list(append(program_scale_impacts$dev_site_losses, program_scale_impacts$dev_credit_losses))
  program_scale_impacts$program_total = sum_list(append(program_scale_impacts$net_offset_gains, program_scale_impacts$net_dev_losses))

  return(program_scale_impacts)
}





run_site_scale_collate_routine <- function(current_simulation_outputs, current_site_sets, site_scale_outcomes,  current_data_dir, current_feature_dynamics_initial, current_feature_dynamics_modes_initial, 
                                           collate_type, current_simulation_params, feature_params, use_cfac_type_in_sim, feature_ind, use_offset_metric){
  
  current_pool = unlist(current_simulation_outputs$site_indexes)
  if (length(current_pool) == 0){
    return(NULL)
  }
  # read current feature layer files over time series and arrange into data stack
  
  current_site_feature_layers = form_site_layer_stack(current_data_dir, 
                                                      file_pattern = paste0('feature_', formatC(current_simulation_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0")), 
                                                      current_pool, 
                                                      current_offset_yrs = current_simulation_outputs$offset_yrs)
  
#   (current_simulation_params, feature_params, current_site_feature_layers, current_feature_dynamics, current_feature_dynamics_modes, projection_yrs, 
#   current_parcel_num_remaining, cfac_type, collate_type, use_cfac_type_in_sim, condition_class_bounds, use_offset_metric, current_transform_param)
  
  site_scale_cfacs = collate_cfacs(current_simulation_params, 
                                   feature_params, 
                                   current_site_feature_layers,
                                   current_feature_dynamics = current_feature_dynamics_initial[current_pool],
                                   current_feature_dynamics_modes = current_feature_dynamics_modes_initial[current_pool],
                                   projection_yrs = unlist(current_simulation_outputs$offset_yrs),
                                   current_parcel_num_remaining = current_simulation_outputs$parcel_num_remaining,
                                   cfac_type = 'site_scale',
                                   collate_type, 
                                   use_cfac_type_in_sim,
                                   condition_class_bounds = feature_params$condition_class_bounds[feature_ind], 
                                   use_offset_metric, 
                                   current_transform_param = current_simulation_params$transform_params[[feature_ind]])
  
  if (use_offset_metric == FALSE){
    current_site_feature_layers = unlist(current_site_feature_layers, recursive = FALSE)
    
    collated_object <- assess_gains_degs(site_scale_outcomes_to_use = site_scale_outcomes[current_pool],
                                             cfacs_to_use = site_scale_cfacs$cfacs,
                                             current_site_feature_layers,
                                             current_offset_yrs = unlist(current_simulation_outputs$offset_yrs),
                                             collate_type, 
                                             current_simulation_params, feature_params,
                                             time_steps = current_simulation_params$time_steps)
    
    collated_object$grouped_gains_degs = group_gains_degs(collated_object, current_site_sets)
    collated_object$summed_gains_degs = sum_gains_degs(collated_object$grouped_gains_degs)

  } else {
    collated_object = list()
  }
  collated_object$site_indexes = current_site_sets
  collated_object$offset_yrs = current_simulation_outputs$offset_yrs
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



sum_data_stack <- function(current_data_dir, file_pattern, land_parcels, time_steps, use_offset_metric, current_transform_param){
  
  current_filenames <- list.files(path = current_data_dir,
                                  pattern = file_pattern, all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)
  
  data_stack = rep(list(matrix(0, nrow = time_steps, ncol = 1)), length(land_parcels))

  for (yr in seq(time_steps)){
    site_feature_layers = readRDS(paste0(current_data_dir, current_filenames[yr]))
    if (use_offset_metric == TRUE){
      site_feature_layers = lapply(seq_along(site_feature_layers), function(i) scale_scores(site_feature_layers[i], current_transform_param)[[1]])
    }
    site_feature_layers = lapply(seq_along(site_feature_layers), function(i) sum(site_feature_layers[[i]]))
    data_stack <- lapply(seq_along(land_parcels), function(i) stack_yr(data_stack[[i]], site_feature_layers[[i]], yr))
  }

  return(data_stack)
}

stack_yr <- function(current_site, current_val, yr){
  current_site[yr] = current_val
  return(current_site)
}


form_old_data_stack <- function(current_data_dir, file_pattern, projection_yrs, land_parcels, time_steps){
  
  current_filenames <- list.files(path = current_data_dir,
                                  pattern = file_pattern, all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)
  
  data_stack = lapply(seq_along(land_parcels), function(i) array(0, c(time_steps, length(land_parcels[[i]]))))
  
  for (yr in seq(time_steps)){
    site_feature_layers = readRDS(paste0(current_data_dir, current_filenames[yr]))
    data_stack <- lapply(seq_along(data_stack), function(i) stack_current_yr(data_stack[[i]], site_feature_layers[[i]], yr))
  }
  
  return(data_stack)
  
}


stack_current_yr <- function(current_site, current_vals, yr){
  current_site[yr, ] = current_vals
  return(current_site)
}


form_site_layer_stack <- function(current_data_dir, file_pattern, current_pool, current_offset_yrs){

  current_filenames <- list.files(path = current_data_dir,
                                  pattern = file_pattern, all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)

  data_stack = vector('list', length(current_pool))
  
  for (yr in unique(current_offset_yrs)){
    current_set = which(current_offset_yrs == yr)
    current_site_feature_layer = readRDS(paste0(current_data_dir, current_filenames[yr]))
    data_stack[current_set] = lapply(current_set, function(i) list(matrix( current_site_feature_layer[[ current_pool[i] ]], nrow = 1)))
  }
  
  return(data_stack)
}



merge_vectors <- function(vec_a, vec_b, start_ind){

  if (length(vec_a) == 0){
    browser()
  }
  
  if (length(vec_b) == 0){
    browser()
  }
  
  if (length(start_ind) == 0){
    browser()
  }
  if (any(is.na(vec_b))){
    browser()
  }
  
  if (any(is.na(vec_a))){
    browser()
  }
  
  if (length(vec_a) < (start_ind + length(vec_b) - 1)){
    browser()
  }

  
  
  vec_a[start_ind:(start_ind + length(vec_b) - 1)] = vec_b
  return(vec_a)
}


assess_gains_degs <- function(site_scale_outcomes_to_use, cfacs_to_use, site_feature_layers_to_use, current_offset_yrs, collate_type, current_simulation_params, feature_params, time_steps){
  

  collated_object = list()
  parcel_num = length(site_scale_outcomes_to_use)
  
  impact_trajectories = lapply(seq(parcel_num), function(i) site_scale_outcomes_to_use[[i]][current_offset_yrs[i]:time_steps])
  
  avoided_loss = lapply(seq(parcel_num), function(i) sum(site_feature_layers_to_use[[i]]) - cfacs_to_use[[i]])
  rest_gains = lapply(seq(parcel_num), function(i) impact_trajectories[[i]] - sum(site_feature_layers_to_use[[i]]))
  net_gains = mapply('-', impact_trajectories, cfacs_to_use, SIMPLIFY = FALSE)
  
  collated_object = list(net_gains, avoided_loss, rest_gains)

  if (any(is.na(unlist(collated_object)))){
    browser()
  }
  collated_object <- lapply(seq_along(collated_object),
                            function(i) lapply(seq_along(collated_object[[i]]), 
                                               function(j) merge_vectors(array(0, time_steps), collated_object[[i]][[j]], current_offset_yrs[j])))
  
  names(collated_object) = c('nets', 'avoided_loss', 'rest_gains')
  
  if ((collate_type == 'offsets') | (collate_type == 'offset_bank')){
    if (current_simulation_params$offset_calc_type == 'restoration_gains'){
      collated_object$nets = collated_object$rest_gains
    } else if (current_simulation_params$offset_calc_type == 'avoided_loss'){
      collated_object$nets = collated_object$avoided_loss
    } else if (current_simulation_params$offset_calc_type == 'net_gains'){
      collated_object$nets = collated_object$nets
    }
  } else {
    if (current_simulation_params$dev_calc_type == 'future_condition'){
      collated_object$nets = collated_object$nets
    } else if (current_simulation_params$dev_calc_type == 'current_condition'){
      collated_object$nets = collated_object$rest_gains
    }
    
  }
  
  return(collated_object)
}



select_nested_subset <- function(nested_object, nested_ind, output_type){
  
  if (output_type == 'nested'){
    nested_subset_object <- lapply(seq_along(nested_object), function(i) nested_object[[i]][nested_ind])
  } else {
    nested_subset_object <- lapply(seq_along(nested_object), function(i) nested_object[[i]][[nested_ind]])
  }
  return(nested_subset_object)
}



# offset_yrs = collated_realisations$collated_offsets$offset_yrs
# offset_sites_used = lapply(seq_along(offset_yrs), function(i) length(offset_yrs[[i]]))
# dev_sites_used = lapply(seq_along(dev_yrs), function(i) length(dev_yrs[[i]]))
# net_sites_used = sum_lists(offset_sites_used, dev_sites_used)
# lapply(seq_along(net_sites_used), function(i) net_sites_used[[i]] > length(parcels$land_parcels))


find_sites_used <- function(collated_data){
  sites_used = list()
  sites_used$offsets = find_current_sites_used(collated_data$collated_offsets$site_indexes)
  sites_used$devs = find_current_sites_used(collated_data$collated_devs$site_indexes)
  
  sites_used$offset_bank = find_current_sites_used(collated_data$collated_offset_bank$site_indexes)
  sites_used$dev_credit = find_current_sites_used(collated_data$collated_dev_credit$site_indexes)
  
  sites_used$unregulated = find_current_sites_used(collated_data$collated_unregulated_loss$site_indexes)
  
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




collate_site_scale_impacts <- function(collated_site_scale_offsets, collated_site_scale_devs){
  site_scale_impacts = list()
  if ((length(collated_site_scale_offsets) > 0) & (length(collated_site_scale_devs) > 0)){
    site_scale_impacts$net_impacts <- mapply('+', collated_site_scale_offsets, collated_site_scale_devs, SIMPLIFY = FALSE)
  } else {
    site_scale_impacts = list()
  }
  
  return(site_scale_impacts)
}

get_current_sim_characteristics <- function(current_current_simulation_params, feature_params, realisation_num){
  
  sim_characteristics = vector()
  sim_characteristics = paste0(sim_characteristics, current_current_simulation_params$offset_calc_type, '_')
  sim_characteristics = paste0(sim_characteristics, 'offset_bank_', current_current_simulation_params$use_offset_bank, '_')
  if ((current_current_simulation_params$use_offset_time_horizon == TRUE) & (current_current_simulation_params$use_offset_bank == FALSE)){                                   
    sim_characteristics = paste0(sim_characteristics, 'time_horizon_', current_current_simulation_params$offset_time_horizon)
  }
  sim_characteristics = paste0(sim_characteristics, '_include_unregulated_loss_', current_current_simulation_params$include_unregulated_loss_in_offset_calc)
  
  sim_characteristics = paste0(sim_characteristics, '_reals_', realisation_num, '_')
  #   sim_characteristics = paste0(current_current_simulation_params$offset_calc_type, '_', current_current_simulation_params$dev_calc_type, '_', current_current_simulation_params$cfac_type_in_offset_calc,  '_cfac_offset_bank_', 
  #                                current_current_simulation_params$use_offset_bank, '_')
  #   
  #   if (current_current_simulation_params$use_offset_bank == TRUE){                                   
  #     sim_characteristics = paste0(sim_characteristics, current_current_simulation_params$offset_bank_start, '_', current_current_simulation_params$offset_bank_end, '_', 
  #                                  current_current_simulation_params$offset_bank_num, '_', current_current_simulation_params$match_type)
  #   }
  #   
  #   sim_characteristics = paste0(sim_characteristics, '_', current_current_simulation_params$offset_action_type, '_')
  #   if (current_current_simulation_params$offset_action_type == 'restore'){
  #     sim_characteristics = paste0(sim_characteristics, current_current_simulation_params$restoration_rate, '_')
  #   }
  #   
  #   if (current_current_simulation_params$use_offset_time_horizon == TRUE){                                   
  #     sim_characteristics = paste0(sim_characteristics, '_time_horizon_', current_current_simulation_params$offset_time_horizon)
  #   }
  
  
  #  sim_characteristics = paste0(sim_characteristics, '_offsets_potential_developments_', current_current_simulation_params$include_potential_developments_in_offset_calc)
  
  #  sim_characteristics = paste0(sim_characteristics, '_offsets_potential_offsets_', current_current_simulation_params$include_potential_offsets_in_offset_calc)
  
  #  sim_characteristics = paste0(sim_characteristics, '_devs_unregulated_loss_', current_current_simulation_params$include_unregulated_loss_in_dev_calc)
  
  # sim_characteristics = paste0(sim_characteristics, '_devs_potential_developments_', current_current_simulation_params$include_potential_developments_in_dev_calc)
  
  #  sim_characteristics = paste0(sim_characteristics, '_devs_potential_offsets_', current_current_simulation_params$include_potential_offsets_in_dev_calc)
  
  
  return(sim_characteristics)
}

sum_list <- function(list_to_sum){
  if (length(list_to_sum) == 0){
    summed_list = list()
  } else {
    empties = which(unlist(lapply(seq_along(list_to_sum), function(i) length(list_to_sum[[i]]) == 0)))
    sets_to_use = setdiff(seq_along(list_to_sum), empties)
    if (length(sets_to_use) == 0){
      summed_list = list()
    } else {
      summed_list <- list(Reduce('+', list_to_sum[sets_to_use]))
    }
  }
  return(summed_list)
}


find_list_mean <- function(list_to_sum){
  empties = which(unlist(lapply(seq_along(list_to_sum), function(i) length(list_to_sum[[i]]) == 0)))
  sets_to_use = setdiff(seq_along(list_to_sum), empties)
  if (length(sets_to_use) == 0){
    list_mean = list()
  } else {
    list_mean <-  list(Reduce('+', list_to_sum[sets_to_use])/length(sets_to_use))
  }
  return(list_mean)
}

prepare_realisations <- function(realisations){   #remove unsuccessful offset programs
  offset_success_flag = unlist(lapply(seq_along(realisations), function(i) realisations[[i]]$offset_success_flag))
  success_inds = which(offset_success_flag == TRUE)
  realisations <- lapply(success_inds, function(i) realisations[[i]])
  return(realisations)
}



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


assess_collated_NNL <- function(assess_type, impacts, offset_yrs_to_use, site_indexes){
  NNL_object <- list()
  
  if (length(unlist(impacts)) == 0){
    return(NNL_object)
  }
  

  if (assess_type == 'site_scale'){

    site_indexes_to_use = match(unlist(lapply(seq_along(site_indexes), function(i) site_indexes[[i]][[1]])), unlist(site_indexes))
    offset_yrs_to_use = offset_yrs_to_use[site_indexes_to_use]
  } 
  

  NNL_absolute = lapply(seq_along(impacts), function(i) assess_NNL(impacts[[i]]) )
  
  NNL_object$NNL = lapply(seq_along(NNL_absolute), function(i) (NNL_absolute[[i]] - offset_yrs_to_use[[i]]))

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


select_cfac_type <- function(collate_type, use_cfac_type_in_sim, current_simulation_params, feature_params){
  cfac_params = list()
  
  if (use_cfac_type_in_sim == FALSE){
    include_unregulated_loss = FALSE
    include_potential_developments = FALSE
    include_potential_offsets = FALSE
    adjust_cfacs_flag = FALSE
  } else {
    
    if ((collate_type == 'devs') | (collate_type == 'dev_credit')){
      include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_dev_calc
      include_potential_developments = current_simulation_params$include_potential_developments_in_dev_calc
      include_potential_offsets = current_simulation_params$include_potential_offsets_in_dev_calc
      adjust_cfacs_flag = current_simulation_params$adjust_dev_cfacs_flag
    } else {
      include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_offset_calc
      include_potential_developments = current_simulation_params$include_potential_developments_in_offset_calc
      include_potential_offsets = current_simulation_params$include_potential_offsets_in_offset_calc
      adjust_cfacs_flag = current_simulation_params$adjust_offset_cfacs_flag
    }
  }
  cfac_params$include_unregulated_loss = include_unregulated_loss
  cfac_params$include_potential_developments = include_potential_developments
  cfac_params$include_potential_offsets = include_potential_offsets
  cfac_params$adjust_cfacs_flag = adjust_cfacs_flag
  return(cfac_params)
}




sum_cols <- function(array_to_sum){
  
  if (length(dim(array_to_sum)) <= 1){
    summed_array = sum(array_to_sum)
  } else if (length(dim(array_to_sum)) == 2){
    summed_array = apply(array_to_sum, MARGIN = 1, sum)
  } else if (length(dim(array_to_sum)) == 3){
    summed_array = apply(array_to_sum, MARGIN = c(1, 3), sum)
    dim(summed_array) = c(dim(summed_array), 1)
    summed_array = aperm(summed_array, c(1, 3, 2))
  }
  return(summed_array)
}


threshold_array <- function(arr_in, thresh_level){
  thresh_array = rep(thresh_level, length(arr_in))
  dim(thresh_array) = dim(arr_in)
  arr_out = arr_in * (abs(arr_in) > thresh_array)
  return(arr_out)
}





