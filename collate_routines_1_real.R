#use_cfac_type_in_sim = TRUE
run_collate_routines <- function(simulation_inputs, simulation_outputs, current_data_dir, simulation_params, realisation_ind){
  
  for (eco_ind in seq(simulation_params$ecology_params$eco_dims)){
    
    current_decline_rates_initial = select_nested_subset(nested_object = simulation_inputs$decline_rates_initial, 
                                                         nested_ind = eco_ind, output_type = 'nested')
    current_initial_ecology = select_nested_subset(nested_object = simulation_inputs$initial_ecology, 
                                                   nested_ind = eco_ind, output_type = 'nested')
    
    landscape_cfacs_object = collate_cfacs(simulation_params,
                                           current_parcel_ecologies = current_initial_ecology,
                                           current_decline_rates = current_decline_rates_initial, 
                                           current_offset_yrs = rep(1, length(current_initial_ecology)), 
                                           current_parcel_num_remaining = vector(),
                                           cfac_type = 'landscape',
                                           collate_type = vector(), 
                                           use_cfac_type_in_sim = FALSE)
    
    current_trajectories = readRDS(paste0(current_data_dir, 'trajectories_', realisation_ind, '_species_', eco_ind, '.rds'))
    simulation_outputs = readRDS(paste0(current_data_dir, 'realisation_', realisation_ind, '_outputs.rds'))
    
    landscape <- calc_landscape_characteristics(current_trajectories, landscape_cfacs_object)
    collated_realisations = collate_program(simulation_outputs, current_trajectories, landscape$summed_site_trajectories, 
                                       landscape_cfacs_object, current_decline_rates_initial, current_initial_ecology, 
                                       simulation_params, use_cfac_type_in_sim = TRUE, parcels, eco_ind)
    
    collated_realisations$landscape = landscape
    saveRDS(collated_realisations, paste0(run_params$collated_folder, 
                                          'single_collated_realisation_', realisation_ind, 
                                          '_scenario_', scenario_ind, 
                                          '_species_', eco_ind, '.rds'))
    
  }
  
}


calc_landscape_characteristics <- function(current_trajectories, landscape_cfacs_object){
  landscape_object = list()
  landscape_object$summed_site_trajectories = lapply(seq_along(current_trajectories), function(i) sum_cols(current_trajectories[[i]]))
  landscape_object$net_landscape = Reduce('+', landscape_object$summed_site_trajectories)
  landscape_object$landscape_cfacs = landscape_cfacs_object$net_background_cfacs
  landscape_object$landscape_impact = landscape_object$net_landscape - landscape_cfacs_object$net_background_cfacs
  return(landscape_object)
}

bind_collated_realisations <- function(scenario_ind, file_path, eco_dims){
  
  for (eco_ind in seq(eco_dims)){
    current_filenames <- list.files(path = file_path, all.files = FALSE, 
                                    pattern = paste0('scenario_', scenario_ind, '_species_', eco_ind),
                                    full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                    include.dirs = FALSE, no.. = FALSE)
    
    realisation_num = length(current_filenames)
    
    if (realisation_num == 0){
      print(paste0('no files found for scenario ', scenario_ind, ', species ', species_ind))
      return()
    }
    
    for (realisation_ind in seq(realisation_num)){
      current_collated_realisation = readRDS(paste0(file_path, current_filenames[realisation_ind]))
      if (realisation_ind == 1){
        collated_realisations = lapply(seq_along(current_collated_realisation), 
                                       function(i) nest_list(current_collated_realisation[[i]]))
      } else {
        collated_realisations <- lapply(seq_along(current_collated_realisation), 
                                        function(i) append_nested_object(collated_realisations[[i]], current_collated_realisation[[i]]))
      }
    }
    names(collated_realisations) = names(current_collated_realisation)
    saveRDS(collated_realisations, paste0(file_path, 'bound_collated_realisations_scenario_', scenario_ind, 
                                                 '_species_', eco_ind, '.rds'))
  }
  
  return(collated_realisations)
}


nest_list <- function(list_a){
  nested_list = lapply(seq_along(list_a), function(j) list(list_a[[j]]))
  names(nested_list) = names(list_a)
  return(nested_list)
}

append_nested_object <- function(object_a, object_b){
  appended_object <- lapply(seq_along(object_b), function(j) append(object_a[[j]], list(object_b[[j]])))
  names(appended_object) = names(object_b)
  return(appended_object)  
}

merge_lists <- function(list_a, list_b, merge_indexes){
  merged_list <- lapply(seq_along(list_a),
                        function(i) merge_vectors(list_a[[i]], list_b[[i]], merge_indexes[i]))
  return(merged_list)
}


#background_cfacs = landscape_cfacs_object$background_cfacs

collate_program_cfacs <- function(simulation_outputs, background_cfacs, collated_offsets, collated_devs, collated_dev_credit, 
                                  collated_offset_bank, collated_illegal_clearing){
  
  program_cfacs_object = list()
  program_cfacs_object$offset_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$offsets_object$parcel_indexes)], collated_offsets$cfacs, unlist(simulation_outputs$offsets_object$offset_yrs))
  program_cfacs_object$dev_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$dev_object$parcel_indexes)], collated_devs$cfacs, unlist(simulation_outputs$dev_object$offset_yrs))
  program_cfacs_object$dev_credit_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$dev_credit_object$parcel_indexes)], collated_dev_credit$cfacs, unlist(simulation_outputs$dev_credit_object$offset_yrs))
  program_cfacs_object$offset_bank_cfacs <- merge_lists(background_cfacs[unlist(simulation_outputs$offset_bank_object$parcel_indexes)], collated_offset_bank$cfacs, unlist(simulation_outputs$offset_bank_object$offset_yrs))
  
  cfac_sums = list(program_cfacs_object$offset_cfacs, program_cfacs_object$dev_cfacs, program_cfacs_object$dev_credit_cfacs, program_cfacs_object$offset_bank_cfacs)
  cfacs_to_use = unlist(lapply(seq_along(cfac_sums), function(i) length(cfac_sums[[i]]))) > 0
  
  program_cfacs_object$program_cfac_sum <- Reduce('+', unlist(cfac_sums[cfacs_to_use], recursive = FALSE))
  
  return(program_cfacs_object)
  
}





collate_cfacs <- function(simulation_params, current_parcel_ecologies, current_decline_rates, current_offset_yrs, 
                          current_parcel_num_remaining, cfac_type, collate_type, use_cfac_type_in_sim){
  
  cfac_params <- select_cfac_type(collate_type, use_cfac_type_in_sim, simulation_params$policy_params)
  
  parcel_count = length(current_parcel_ecologies)
  
  if (cfac_type == 'landscape'){                    
    time_horizons <- generate_time_horizons(project_type = 'future', 
                                            yr = 1, 
                                            offset_yrs = rep(1, parcel_count), 
                                            time_horizon = (simulation_params$run_params$time_steps - 1), 
                                            parcel_count)
    adjust_cfacs_flag = TRUE
    include_potential_developments = FALSE
    include_potential_offsets = FALSE
    include_illegal_clearing = TRUE
    
  } else {
    time_horizons = generate_time_horizons(project_type = 'current', 
                                           yr = simulation_params$run_params$time_steps, 
                                           offset_yrs = current_offset_yrs, 
                                           time_horizon = (simulation_params$run_params$time_steps - 1), 
                                           parcel_count)
    adjust_cfacs_flag = cfac_params$adjust_cfacs_flag
    include_potential_developments = cfac_params$include_potential_developments
    include_potential_offsets = cfac_params$include_potential_offsets
    include_illegal_clearing = cfac_params$include_illegal_clearing
  }
  
  cfacs_object = calc_cfacs(parcel_ecologies = current_parcel_ecologies, 
                            parcel_num_remaining = current_parcel_num_remaining,
                            simulation_params$run_params,
                            simulation_params$ecology_params,  
                            simulation_params$policy_params,
                            current_decline_rates, 
                            time_horizons, 
                            offset_yrs = current_offset_yrs, 
                            include_potential_developments,
                            include_potential_offsets,
                            include_illegal_clearing,
                            adjust_cfacs_flag,
                            time_fill = TRUE, 
                            dims_to_use = 1)
  
  if (cfac_type == 'landscape'){
    background_cfacs = lapply(seq_along(cfacs_object$cfacs), function(i) cfacs_object$cfacs[[i]][[1]])    #extract from nested list
    cfacs_object$background_cfacs = lapply(seq_along(background_cfacs), function(i) sum_cols(background_cfacs[[i]]))
    cfacs_object$net_background_cfacs = Reduce('+', cfacs_object$background_cfacs)
    cfacs_object$net_cfacs_including_clearing = Reduce('+', lapply(seq_along(cfacs_object$cfacs_to_use), function(i) cfacs_object$cfacs_to_use[[i]][[1]]))
  } else {
    if (adjust_cfacs_flag == TRUE){
      cfacs_object$cfacs = lapply(seq_along(cfacs_object$adjusted_cfacs), function(i) cfacs_object$adjusted_cfacs[[i]][[1]])    #extract from nested list
    } else {
      cfacs_object$cfacs = lapply(seq_along(cfacs_object$cfacs), function(i) cfacs_object$cfacs[[i]][[1]])    #extract from nested list
    }
  }
  
  return(cfacs_object)
}



collate_net_program_outcomes <- function(simulation_outputs, summed_site_trajectories){
  collated_program_outcomes = list()
  collated_program_outcomes$offsets <- Reduce('+', summed_site_trajectories[unlist(simulation_outputs$offsets_object$parcel_indexes)])
  collated_program_outcomes$devs <- Reduce('+', summed_site_trajectories[unlist(simulation_outputs$dev_object$parcel_indexes)])
  collated_program_outcomes$dev_credit <- Reduce('+', summed_site_trajectories[unlist(simulation_outputs$dev_credit_object$parcel_indexes)])
  collated_program_outcomes$offset_bank <- Reduce('+', summed_site_trajectories[unlist(simulation_outputs$offset_bank$parcel_indexes)])
  
  collated_program_outcomes$net_offsets <- sum_program_elements(list(collated_program_outcomes$offsets, collated_program_outcomes$offset_bank))
  collated_program_outcomes$net_devs <- sum_program_elements(list(collated_program_outcomes$devs, collated_program_outcomes$dev_credit))
  collated_program_outcomes$net <- sum_program_elements(list(collated_program_outcomes$net_offsets, collated_program_outcomes$net_devs))
  
  return(collated_program_outcomes)
}



collate_net_program_impacts <- function(collated_program){
  collated_program_impacts = list()
  collated_program_impacts$net_site_scale <- mapply('+', collated_program$collated_offsets$summed_gains_degs$site_nets, 
                                                      collated_program$collated_devs$summed_gains_degs$site_nets, SIMPLIFY = FALSE)
  collated_program_impacts$net_site_offset_gains = Reduce('+', collated_program$collated_offsets$summed_gains_degs$site_nets)
  collated_program_impacts$net_offset_bank_gains = Reduce('+', collated_program$collated_offset_bank$summed_gains_degs$site_nets)
  collated_program_impacts$net_site_dev_losses = Reduce('+', collated_program$collated_devs$summed_gains_degs$site_nets)
  collated_program_impacts$net_dev_credit_losses = Reduce('+', collated_program$collated_dev_credit$summed_gains_degs$site_nets)
  collated_program_impacts$net_illegal_clearing <- Reduce('+', collated_program$collated_illegal_clearing$summed_gains_degs$site_nets)
  
  collated_program_impacts$net_offset_gains = sum_program_elements(list(collated_program_impacts$net_site_offset_gains, collated_program_impacts$net_collated_offset_bank_gains))
  collated_program_impacts$net_dev_losses = sum_program_elements(list(collated_program_impacts$net_site_dev_losses, collated_program_impacts$net_collated_dev_credit_losses))
  collated_program_impacts$net_program_outcomes <- sum_program_elements(list(collated_program_impacts$net_offset_gains, collated_program_impacts$net_dev_losses))
  return(collated_program_impacts)
}





run_site_scale_collate_routine <- function(current_model_outputs, current_trajectories, current_decline_rates_initial, 
                                           collate_type, simulation_params, use_cfac_type_in_sim, eco_ind){
  
  collated_object = list()
  collated_object = collate_gains_degs(current_model_outputs, 
                                       current_trajectories,
                                       current_decline_rates_initial ,
                                       collate_type, 
                                       simulation_params,
                                       use_cfac_type_in_sim, 
                                       eco_ind)
  
  if (length(collated_object) > 0){
    collated_object$grouped_gains_degs = group_gains_degs(collated_object, current_model_outputs$parcel_indexes)
    collated_object$summed_gains_degs = sum_gains_degs(collated_object$grouped_gains_degs)
    collated_object$parcel_indexes = current_model_outputs$parcel_indexes
    collated_object$offset_yrs = current_model_outputs$offset_yrs
  } else {
    return(NULL)
  }
  return(collated_object)
}



sum_gains_degs <- function(grouped_gains_degs){
  summed_gains_degs <- lapply(seq_along(grouped_gains_degs), 
                              function(i) (lapply(seq_along(grouped_gains_degs[[i]]), function(j) Reduce('+', grouped_gains_degs[[i]][[j]]))))
  
  names(summed_gains_degs) = names(grouped_gains_degs) 
  
  return(summed_gains_degs)
}



group_gains_degs <- function(collated_object, parcel_indexes){ 
  
  grouped_gains_degs <- lapply(seq_along(collated_object), 
                               function(i) (lapply(seq_along(parcel_indexes), 
                                                   function(j) collated_object[[i]][which(unlist(parcel_indexes) 
                                                                                          %in% parcel_indexes[[j]])])))
  names(grouped_gains_degs) = names(collated_object)
  
  return(grouped_gains_degs)
  
}


collate_gains_degs <- function(current_model_outputs, current_trajectories, current_decline_rates_initial, 
                               collate_type, simulation_params, use_cfac_type_in_sim, eco_ind){ 
  
  
  if (length(unlist(current_model_outputs$parcel_indexes)) == 0){
    return(NULL)
  }
  
  current_parcel_ecologies = select_nested_subset(nested_object = current_model_outputs$parcel_ecologies, nested_ind = eco_ind, output_type = 'nested') 
  
  current_cfacs = collate_cfacs(simulation_params, 
                                current_parcel_ecologies = current_model_outputs$parcel_ecologies,
                                current_decline_rates = current_decline_rates_initial[unlist(current_model_outputs$parcel_indexes)], 
                                current_offset_yrs =  unlist(current_model_outputs$offset_yrs),
                                current_parcel_num_remaining = current_model_outputs$parcel_num_remaining,
                                cfac_type = 'site_scale',
                                collate_type, 
                                use_cfac_type_in_sim)
  
  parcel_ecologies_to_use = select_nested_subset(nested_object = current_model_outputs$parcel_ecologies, nested_ind = eco_ind, output_type = 'non-nested') 
  
  collated_gains_degs <- assess_gains_degs(trajectories_to_use = current_trajectories[unlist(current_model_outputs$parcel_indexes)],
                                           cfacs_to_use = current_cfacs$cfacs,
                                           parcel_ecologies_to_use,
                                           current_offset_yrs = unlist(current_model_outputs$offset_yrs),
                                           collate_type, 
                                           simulation_params$policy_params,
                                           time_steps = run_params$time_steps)
  
  collated_gains_degs$cfacs = lapply(seq_along(current_cfacs$cfacs_to_use), function(i) current_cfacs$cfacs_to_use[[i]][[1]])
  
  return(collated_gains_degs)
  
}



merge_vectors <- function(vec_a, vec_b, start_ind){
  vec_a[start_ind:(start_ind + length(vec_b) - 1)] = vec_b
  return(vec_a)
}


assess_gains_degs <- function(trajectories_to_use, cfacs_to_use, parcel_ecologies_to_use, current_offset_yrs, collate_type, policy_params, time_steps){
  
  tmp_object = list()
  parcel_num = length(trajectories_to_use)
  
  impact_trajectories = lapply(seq(parcel_num), function(i) sum_cols(trajectories_to_use[[i]][current_offset_yrs[i]:time_steps, ]))
  
  tmp_object$nets = lapply(seq(parcel_num), function(i) impact_trajectories[[i]] - sum_cols(cfacs_to_use[[i]]))
  
  tmp_object$rest_gains = lapply(seq(parcel_num), function(i) impact_trajectories[[i]] - sum(parcel_ecologies_to_use[[i]]))
  
  tmp_object$avoided_degs = lapply(seq(parcel_num), function(i) sum(parcel_ecologies_to_use[[i]]) - sum_cols(cfacs_to_use[[i]]))
  
  collated_object <- lapply(seq_along(tmp_object),
                            function(i) lapply(seq_along(tmp_object[[i]]), 
                                               function(j) merge_vectors(array(0, time_steps), tmp_object[[i]][[j]], current_offset_yrs[j])))
  names(collated_object) = names(tmp_object)
  
  if ((collate_type == 'offsets') | (collate_type == 'offset_bank')){
    if (policy_params$offset_calc_type == 'restoration_gains'){
      collated_object$site_nets = collated_object$rest_gains
    } else if (policy_params$offset_calc_type == 'avoided_degs'){
      collated_object$site_nets = collated_object$avoided_degs
    } else if (policy_params$offset_calc_type == 'net_gains'){
      collated_object$site_nets = collated_object$nets
    }
  } else {
    if (policy_params$dev_calc_type == 'future_condition'){
      collated_object$site_nets = collated_object$nets
    } else if (policy_params$dev_calc_type == 'current_condition'){
      collated_object$site_nets = collated_object$rest_gains
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


collate_current_realisation <- function(landscape_cfacs_object, current_initial_ecology, current_decline_rates_initial, 
                                        simulation_params, current_trajectories, simulation_outputs, parcels, eco_ind, use_cfac_type_in_sim){
  
  summed_site_trajectories = lapply(seq_along(current_trajectories), function(i) sum_cols(current_trajectories[[i]]))
  net_landscape = Reduce('+', summed_site_trajectories)
  collated_program = collate_program(simulation_outputs, current_trajectories, summed_site_trajectories, 
                                     landscape_cfacs_object, current_decline_rates_initial, current_initial_ecology, 
                                     simulation_params, use_cfac_type_in_sim, parcels, eco_ind)
  
  # system_NNL <- assess_system_NNL(landscape_rel_to_cfac_including_clearing, 
  #                                 time_steps = run_params$time_steps, 
  #                                 offset_time_horizon = policy_params$offset_time_horizon)
  # landscape_loss = assess_landscape_loss(net_landscape, 
  #                                        system_NNL$NNL_yrs, 
  #                                        time_steps = run_params$time_steps)
  
  
  return(collated_program)
}

collate_program <- function(simulation_outputs, current_trajectories, summed_site_trajectories, landscape_cfacs_object, 
                            current_decline_rates_initial, current_initial_ecology, simulation_params, use_cfac_type_in_sim, parcels, eco_ind){
  
  collated_program = list()
  
  collated_program$collated_offsets <- run_site_scale_collate_routine(current_model_outputs = simulation_outputs$offsets_object, 
                                                                      current_trajectories, 
                                                                      current_decline_rates_initial, 
                                                                      collate_type = 'offsets', 
                                                                      simulation_params,
                                                                      use_cfac_type_in_sim, 
                                                                      eco_ind)
  
  collated_program$collated_devs = run_site_scale_collate_routine(current_model_outputs = simulation_outputs$dev_object, 
                                                                  current_trajectories, 
                                                                  current_decline_rates_initial, 
                                                                  collate_type = 'devs', 
                                                                  simulation_params,
                                                                  use_cfac_type_in_sim, 
                                                                  eco_ind)
  
  collated_program$collated_dev_credit = run_site_scale_collate_routine(current_model_outputs = simulation_outputs$dev_credit_object, 
                                                                        current_trajectories, 
                                                                        current_decline_rates_initial, 
                                                                        collate_type = 'dev_credit', 
                                                                        simulation_params,
                                                                        use_cfac_type_in_sim, 
                                                                        eco_ind)
  
  collated_program$collated_offset_bank = run_site_scale_collate_routine(current_model_outputs = simulation_outputs$offset_bank_object, 
                                                                         current_trajectories, 
                                                                         current_decline_rates_initial, 
                                                                         collate_type = 'offset_bank', 
                                                                         simulation_params,
                                                                         use_cfac_type_in_sim, 
                                                                         eco_ind)
  
  collated_program$collated_illegal_clearing = run_site_scale_collate_routine(current_model_outputs = simulation_outputs$illegal_clearing_object, 
                                                                              current_trajectories, 
                                                                              current_decline_rates_initial, 
                                                                              collate_type = 'illegal_clearing', 
                                                                              simulation_params,
                                                                              use_cfac_type_in_sim, 
                                                                              eco_ind)
  
  collated_program$program_outcomes <- collate_net_program_outcomes(simulation_outputs, summed_site_trajectories)
  collated_program$program_impacts <- collate_net_program_impacts(collated_program)
  collated_program$program_cfacs = collate_program_cfacs(simulation_outputs, 
                                                         landscape_cfacs_object$background_cfacs, 
                                                         collated_program$collated_offsets, 
                                                         collated_program$collated_devs, 
                                                         collated_program$collated_dev_credit, 
                                                         collated_program$collated_offset_bank, 
                                                         collated_program$collated_illegal_clearing)
  
  
  
  #   if (length(parcel_set_outcomes) > 0){
  #     site_NNL = assess_site_NNL(net_outcomes = parcel_set_outcomes, 
  #                                            offset_yrs = collated_offsets$offset_yrs, 
  #                                            parcel_set_nums = collated_offsets$parcel_set_nums, 
  #                                            time_steps = run_params$time_steps, 
  #                                            offset_time_horizon = policy_params$offset_time_horizon)
  #   } else{
  #     site_NNL = list()
  #   }
  #   
  #   if (length(net_program_outcomes) > 0){
  #     program_NNL <- assess_system_NNL(net_program_outcomes, 
  #                                      time_steps = run_params$time_steps, 
  #                                      offset_time_horizon = policy_params$offset_time_horizon
  #     )
  #   } else {
  #     program_NNL = list()
  #   }
  #   
  # 
  #   net_program_loss = assess_landscape_loss(cfacs_object$program_cfac_sum, 
  #                                            program_NNL$NNL_yrs, 
  #                                            time_steps = run_params$time_steps)
  #   
  
  
  return(collated_program)
  
}

sum_program_elements <- function(program_elements){
  elements_to_sum = which(unlist(lapply(seq_along(program_elements), function(i) length(program_elements[[i]]) > 0)))
  summed_program <- Reduce('+', program_elements[elements_to_sum])
  return(summed_program)
}


prepare_realisations <- function(realisations){   #remove unsuccessful offset programs
  offset_success_flag = unlist(lapply(seq_along(realisations), function(i) realisations[[i]]$offset_success_flag))
  success_inds = which(offset_success_flag == TRUE)
  realisations <- lapply(success_inds, function(i) realisations[[i]])
  return(realisations)
}



assess_system_NNL <- function(net_program_outcomes, realisation_num, time_steps, offset_time_horizon, eco_dims){
  system_NNL <- list()
  NNL_yrs <- find_NNL(net_program_outcomes, assess_type = 'system', eco_dims, assess_num = realisation_num, time_steps, offset_yrs = vector(), offset_time_horizon)
  system_NNL$NNL_yrs <- NNL_yrs
  system_NNL$NNL_success = assess_NNL(system_NNL$NNL_yrs, assess_type = 'success', eco_dims)
  system_NNL$NNL_mean = assess_NNL(system_NNL$NNL_yrs, assess_type = 'mean', eco_dims)
  return(system_NNL)
}




assess_site_NNL <- function(net_outcomes, offset_yrs, parcel_set_nums, time_steps, realisation_num,  eco_dims, offset_time_horizon){
  site_NNL <- list()
  site_NNL$NNL_yrs <- find_site_NNL_yrs(net_outcomes, offset_yrs, parcel_set_nums, time_steps, realisation_num,  eco_dims, offset_time_horizon)
  site_NNL$NNL_success = assess_NNL(unlist(site_NNL$NNL_yrs, recursive = FALSE), assess_type = 'success', eco_dims)
  site_NNL$NNL_mean = assess_NNL(unlist(site_NNL$NNL_yrs, recursive = FALSE), assess_type = 'mean', eco_dims)
  return(site_NNL)
}



assess_landscape_loss <- function(landscape_vals, NNL_yrs, realisation_num, eco_dims, time_steps){
  landscape_loss = list()
  landscape_loss$NNL_loss = assess_loss_element(landscape_vals, loss_type = 'NNL', NNL_yrs, realisation_num, eco_dims, time_steps)
  landscape_loss$total_loss = assess_loss_element(landscape_vals, loss_type = 'total', NNL_yrs, realisation_num, eco_dims, time_steps)
  return(landscape_loss)
}

assess_loss_element <- function(landscape_vals, loss_type, NNL_yrs, realisation_num, eco_dims, time_steps){
  
  NNL_loss <- generate_nested_list(realisation_num, eco_dims)
  
  for (realisation_ind in seq_len(realisation_num)){
    for (eco_ind in seq(eco_dims)){
      initial_landscape_val = landscape_vals[[realisation_ind]][[eco_ind]][1]
      if (loss_type == 'NNL'){
        current_NNL_yr = NNL_yrs[[realisation_ind]][[eco_ind]]
      } else if (loss_type == 'total'){
        current_NNL_yr = time_steps
      }
      if (length(current_NNL_yr) > 0){
        current_NNL_val = landscape_vals[[realisation_ind]][[eco_ind]][current_NNL_yr]
        NNL_loss[[realisation_ind]][[eco_ind]] = 1 - current_NNL_val/initial_landscape_val
      }
    }
  }
  
  #   if (loss_type == 'NNL'){
  #     success_inds = which(rowSums(NNL_loss) != 0)
  #     NNL_loss = NNL_loss[success_inds]
  #   }
  
  return(NNL_loss)
} 

assess_net_gains <- function(net_offset_outcome, net_dev_outcome){
  net_gains = sum_nested_lists(list(net_offset_outcome, net_dev_outcome))
  return(net_gains)
}




assess_parcels_used <- function(realisations, offset_bank_num){
  program_nums = list()
  program_nums$dev_nums = lapply(seq_along(realisations), function(i) length(realisations[[i]]$dev_object$parcel_indexes))
  program_nums$dev_credit_nums = lapply(seq_along(realisations), function(i) length(realisations[[i]]$dev_credit_object$parcel_indexes))
  program_nums$offset_nums = lapply(seq_along(realisations), function(i) length(realisations[[i]]$offsets_object$parcel_indexes))
  program_nums$offset_bank_nums = rep(offset_bank_num, length(realisations))
  return(program_nums)
}




find_site_NNL_yrs <- function(net_outcomes, offset_yrs, parcel_set_nums, time_steps, realisation_num, eco_dims, offset_time_horizon){
  NNL_yrs = vector('list', realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){
    current_offset_yrs = offset_yrs[[realisation_ind]]
    current_net_outcome = net_outcomes[[realisation_ind]]
    parcel_set_num = parcel_set_nums[[realisation_ind]]
    current_NNL = find_NNL(current_net_outcome, 
                           assess_type = 'parcel_set', 
                           eco_dims, 
                           assess_num = parcel_set_num, 
                           time_steps, 
                           current_offset_yrs, 
                           offset_time_horizon)
    NNL_yrs[[realisation_ind]] = current_NNL
  }
  
  return(NNL_yrs)
  
}



find_NNL <- function(net_gains, assess_type, eco_dims, assess_num, time_steps, offset_yrs, offset_time_horizon){
  
  NNL_yrs = generate_nested_list(outer_dim = assess_num, inner_dim = eco_dims)
  thresh = 0
  
  for (assess_ind in seq_len(assess_num)){
    
    for (eco_ind in 1:eco_dims){
      
      if (assess_type == 'system'){
        current_net_gain = net_gains[[assess_ind]][[eco_ind]]               #select current net_gain to test
      } else if (assess_type == 'parcel_set'){
        current_net_gain = net_gains[[eco_ind]][, assess_ind]               #select current net_gain to test
      }
      test_NNLs = which(current_net_gain > thresh)
      
      if (length(test_NNLs) > 0){ 
        
        for (test_ind in 1:length(test_NNLs)){
          test_yr = test_NNLs[test_ind]
          current_test_net_gain = current_net_gain[test_yr:length(current_net_gain)]
          
          if (all(current_test_net_gain > thresh)){
            
            if (assess_type == 'parcel_set'){
              
              offset_yr = min(unlist(offset_yrs[[assess_ind]]))
              NNL_yrs[[assess_ind]][[eco_ind]] = test_yr - offset_yr
            } else {
              NNL_yrs[[assess_ind]][[eco_ind]] = test_yr
            }
            
            break
            
          } 
        }
      }
    }
  }  
  
  return(NNL_yrs)
  
}


find_list_subset <- function(list_a, subset_inds){
  list_length = length(list_a)
  for (list_ind in seq(list_length)){
    list_a[[list_ind]] = list_a[[list_ind]][subset_inds]
  }
  return(list_a)
}



select_cfac_type <- function(collate_type, use_cfac_type_in_sim, policy_params){
  cfac_params = list()
  
  if (use_cfac_type_in_sim == FALSE){
    include_illegal_clearing = FALSE
    include_potential_developments = FALSE
    include_potential_offsets = FALSE
    adjust_cfacs_flag = FALSE
  } else {
    
    if ((collate_type == 'devs') | (collate_type == 'dev_credit')){
      include_illegal_clearing = policy_params$include_illegal_clearing_in_dev_calc
      include_potential_developments = policy_params$include_potential_developments_in_dev_calc
      include_potential_offsets = policy_params$include_potential_offsets_in_dev_calc
      adjust_cfacs_flag = policy_params$adjust_dev_cfacs_flag
    } else {
      include_illegal_clearing = policy_params$include_illegal_clearing_in_offset_calc
      include_potential_developments = policy_params$include_potential_developments_in_offset_calc
      include_potential_offsets = policy_params$include_potential_offsets_in_offset_calc
      adjust_cfacs_flag = policy_params$adjust_offset_cfacs_flag
    }
  }
  cfac_params$include_illegal_clearing = include_illegal_clearing
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


assess_NNL <- function(NNL_yrs, assess_type, eco_dims){
  
  assessed_NNL_object = vector('list', eco_dims)
  for (eco_ind in seq_len(eco_dims)){
    current_NNLs = lapply(seq_along(NNL_yrs), function(i) NNL_yrs[[i]][[eco_ind]])
    current_NNL_vec = unlist(current_NNLs)
    if (assess_type == 'success'){
      assessed_NNL_object[[eco_ind]] = length(current_NNL_vec)/length(current_NNLs)
    } else if ((assess_type == 'mean') & (length(current_NNL_vec) > 0)){
      assessed_NNL_object[[eco_ind]] = mean(current_NNL_vec)
    }
  }
  return(assessed_NNL_object)
}





