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
  
  intervention_object <- run_pre_collate_routines(simulation_outputs, 
                                                  input_data_object, 
                                                  simulation_params, 
                                                  current_data_dir)
  
  run_collate_routines(intervention_object, 
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
    features_to_collate = seq(input_data_object$global_params$feature_num)
  } else {
    features_to_collate = 1
  }
  
  site_scale_outcomes = sum_data_stack(current_data_dir, 
                                       file_pattern = 'feature_outputs_yr_',
                                       use_offset_metric, 
                                       features_to_collate,
                                       input_data_object$site_characteristics$cell_num, 
                                       input_data_object$site_scale_condition_class_key,
                                       input_data_object$global_params$transform_function,
                                       simulation_params$transform_params,
                                       input_data_object$global_params$time_steps, 
                                       input_data_object$global_params$numeric_placeholder_width)
  
  if (intervention_object$intervention_flag == TRUE){

    intervention_object$value_at_intervention = vector('list', length(input_data_object$site_characteristics$cell_id_groups))
    intervention_object$value_at_intervention[intervention_object$intervention_pool] = sum_sites(intervention_object$site_scale_features_at_intervention_set[intervention_object$intervention_pool], 
                                                                                                                      input_data_object$condition_classes[intervention_object$intervention_pool],
                                                                                                                      input_data_object$site_scale_condition_class_key[intervention_object$intervention_pool],
                                                                                                                      input_data_object$site_characteristics$cell_num[intervention_object$intervention_pool], 
                                                                                                                      use_offset_metric, 
                                                                                                                      input_data_object$global_params$user_transform_function, 
                                                                                                                      simulation_params$transform_params)
    
    intervention_object$site_scale_cfacs = vector('list', length(input_data_object$site_characteristics$cell_id_groups))
    

    intervention_object$site_scale_cfacs[intervention_object$intervention_pool] = build_cfacs(intervention_object$site_scale_features_at_intervention_set[intervention_object$intervention_pool],
                                                                                                input_data_object$feature_dynamics,
                                                                                                input_data_object$condition_classes,
                                                                                                input_data_object$feature_dynamics_modes[intervention_object$intervention_pool],
                                                                                                input_data_object$mode_characteristics, 
                                                                                                input_data_object$site_characteristics$cell_num[intervention_object$intervention_pool],
                                                                                                projection_yrs = as.vector(unlist(intervention_object$intervention_yrs_pool)),
                                                                                                intervention_yrs = as.vector(unlist(intervention_object$intervention_yrs_pool)),
                                                                                                intervention_object$site_num_remaining_pool,
                                                                                                cfac_type = 'site_scale',
                                                                                                object_type = intervention_object$object_name_pool, 
                                                                                                use_cfac_type_in_sim = TRUE, 
                                                                                                input_data_object$feature_params$condition_class_characteristics, 
                                                                                                use_offset_metric, 
                                                                                                input_data_object$global_params$user_transform_function, 
                                                                                                simulation_params, 
                                                                                                input_data_object$feature_params,
                                                                                                input_data_object$global_params)
    
  }
  

  for (feature_ind in features_to_collate){
    
    if (use_offset_metric == FALSE){
      flog.info('collating feature %s time series data', feature_ind)
      background_cfacs_to_use = subset_current_group(background_cfacs, feature_ind)
    } else {
      flog.info('collating metric')
      background_cfacs_to_use = background_cfacs
    }
    
    collated_object = list()
    
    collated_object$site_scale <- run_site_scale_outcome_routines(intervention_object, 
                                                                  site_scale_outcomes, 
                                                                  feature_ind, 
                                                                  use_offset_metric)
      
    collated_object$landscape_scale <- run_landscape_scale_routines(collated_object$site_scale$outcomes$net_outcome, 
                                                                    background_cfacs_to_use, 
                                                                    input_data_object$global_params$threshold_control)
    
    if (intervention_object$intervention_flag == TRUE){
      
      collated_object$site_scale <- run_site_scale_impact_routines(intervention_object, 
                                                                   collated_object$site_scale,
                                                                   intervention_object$intervention_yrs_pool, 
                                                                   simulation_outputs,  
                                                                   background_cfacs_to_use,
                                                                   simulation_params, 
                                                                   input_data_object$feature_params, 
                                                                   input_data_object$global_params, 
                                                                   use_offset_metric)
      
      collated_object$program_scale <- run_program_scale_routines(collated_object$site_scale$impacts, 
                                                                  collated_object$site_scale$site_scale_cfacs, 
                                                                  collated_object$site_scale$outcomes$net_outcome, 
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
  
}

sum_site_scale_features <- function(site_scale_features){
  site_sums = lapply(seq_along(site_scale_features), function(i) do.call(sum, lapply(site_scale_features[[i]], sum)))
  return(site_sums)
}


run_landscape_scale_routines <- function(site_scale_outcomes_to_use, background_cfacs_to_use, threshold_control){
  
  landscape_scale = list()
  landscape_scale$outcomes$net_outcome = sum_list(site_scale_outcomes_to_use)
  landscape_scale$cfacs$net_cfac = sum_list(background_cfacs_to_use)
  landscape_scale$impacts$net_impacts = list(mapply('-', landscape_scale$outcomes$net_outcome, landscape_scale$cfacs$net_cfac))

  landscape_scale$loss_characteristics = assess_loss(unlist(landscape_scale$impacts$net_impacts), 
                                                     unlist(landscape_scale$outcomes$net_outcome), 
                                                     intervention_yr = 1, 
                                                     threshold_control)
  
  return(landscape_scale)
}


run_site_scale_outcome_routines <- function(intervention_object, site_scale_outcomes, feature_ind, use_offset_metric){
  
  site_scale = list()
  
  if (use_offset_metric == FALSE){
    site_scale$outcomes$net_outcome = subset_current_group(site_scale_outcomes, feature_ind)
    site_scale$value_at_intervention = subset_current_group(intervention_object$value_at_intervention, feature_ind)
    site_scale$site_scale_cfacs = subset_current_group(intervention_object$site_scale_cfacs, feature_ind) 
  } else {
    site_scale$outcomes$net_outcome = site_scale_outcomes
    site_scale$value_at_intervention = list(intervention_object$value_at_intervention)
    site_scale$site_scale_cfacs = list(intervention_object$site_scale_cfacs)
  }
  
  return(site_scale)
}

run_site_scale_impact_routines <- function(intervention_object, site_scale, intervention_yrs_pool, simulation_outputs,  background_cfacs_to_use,
                                    simulation_params, feature_params,  global_params, use_offset_metric){
  
  site_scale$impacts = setNames(vector('list', length(simulation_outputs$interventions)), 
                                names(simulation_outputs$interventions))
  
  interventions_to_use = which(unlist(lapply(seq_along(intervention_object$grouped_intervention_pool), 
                                             function(i) length(intervention_object$grouped_intervention_pool[[i]]) > 0 ) ))

  site_scale$impacts[interventions_to_use] = lapply(interventions_to_use, 
                                                    function(i) calc_intervention_impacts(simulation_outputs$interventions[[i]], 
                                                                                          simulation_outputs$index_object$site_indexes_used[[i]], 
                                                                                          names(simulation_outputs$interventions)[i], 
                                                                                          site_scale$site_scale_cfacs[ unlist(intervention_object$grouped_intervention_pool[[i]]) ], 
                                                                                          site_scale$value_at_intervention[ unlist(intervention_object$grouped_intervention_pool[[i]]) ], 
                                                                                          site_scale$outcomes$net_outcome[ unlist(intervention_object$grouped_intervention_pool[[i]]) ],  
                                                                                          simulation_params, 
                                                                                          feature_params, 
                                                                                          global_params,
                                                                                          use_offset_metric))
  
  site_scale$net_impacts <- list(calc_net_site_scale_impacts(site_scale$impacts$offset_object$summed_impacts$nets,
                                                             site_scale$impacts$development_object$summed_impacts$nets))
  

  #     site_scale$NNL = assess_site_scale_NNL(assess_type = 'site_scale', 
  #                                                          impacts = site_scale_impacts$net_impacts, 
  #                                                          intervention_yrs_to_use = collated_object$collated_offsets$intervention_yrs, 
  #                                                          site_indexes = simulation_outputs$index_object$site_indexes_used$offset_object)
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
  
  program_scale$loss_characteristics = assess_loss(program_scale$impacts$net_impacts[[1]], 
                                                   program_scale$outcomes$net_outcome[[1]], 
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
  intervention_object$site_num_remaining_pool = unlist(lapply(seq_along(simulation_outputs$interventions), 
                                                              function(i) simulation_outputs$interventions[[i]]$site_num_remaining))
  length_discriminator <- which(unlist(lapply(seq_along(simulation_outputs$interventions), 
                                 function(i) length(intervention_object$grouped_intervention_pool[[i]]) > 0)))
  
  intervention_object$object_name_pool = unlist(lapply(length_discriminator, 
                                                       function(i) rep(names(simulation_outputs$interventions)[i], nrow(do.call('rbind', intervention_object$grouped_intervention_pool[[i]])))))
  
  intervention_object$intervention_pool = as.vector(unlist(intervention_object$grouped_intervention_pool))
  
  if (length(intervention_object$intervention_pool) == 0){
    intervention_object$intervention_flag = FALSE
    return(intervention_object)
    
  } else {
    intervention_object$intervention_flag = TRUE
    #projection_yrs = as.vector(unlist(intervention_object$intervention_yrs_pool))
    # intervention_object$projection_yrs_pool = lapply(seq_along(intervention_object$intervention_pool), 
    #                                                  function(i) lapply(seq(input_data_object$global_params$feature_num), 
    #                                                                     function(j) rep(list(projection_yrs[i]), 
    #                                                                                     length(input_data_object$condition_classes[[ intervention_object$intervention_pool[i] ]][[j]])
    #                                                                                     ) 
    #                                                                     )
    #                                                  )
    
    intervention_object$site_scale_features_at_intervention_set = build_site_scale_features_at_intervention(length(input_data_object$site_characteristics$cell_id_groups), 
                                                                                                            current_data_dir, 
                                                                                                            intervention_object$intervention_pool, 
                                                                                                            unlist(intervention_object$intervention_yrs_pool), 
                                                                                                            file_pattern = 'feature_outputs_yr_', 
                                                                                                            input_data_object$global_params$numeric_placeholder_width)
    return(intervention_object)
  }
}

subset_current_group <- function(current_object, subset_ind, output_type){

  set_lengths = lapply(current_object, 'length')
  sets_to_use = which(unlist(set_lengths) > 0)
  output_object = vector('list', length(current_object))
  output_object[sets_to_use] <- lapply(sets_to_use, function(i) current_object[[i]][ , subset_ind, drop = FALSE])

  return(output_object)
}


# build_site_layer_stack <- function(current_data_dir, file_pattern, current_pool, current_intervention_yrs, numeric_placeholder_width){
#   
#   current_filenames <- list.files(path = current_data_dir,
#                                   pattern = file_pattern, all.files = FALSE,
#                                   include.dirs = FALSE, no.. = FALSE)
#   
#   current_filenames <- list.files(path = current_data_dir,
#                                   all.files = FALSE,
#                                   include.dirs = FALSE, no.. = FALSE)
#   
#   data_stack = vector('list', length(current_pool))
#   
#   for (yr in unique(current_intervention_yrs)){
# 
#     current_intervention_set = as.vector(which(current_intervention_yrs == yr))
#     
#     current_site_scale_feature_layer = readRDS(paste0(current_data_dir, current_filenames[yr]))
#     current_site_scale_feature_layer = current_site_scale_feature_layer[current_intervention_set]
#     
#     data_stack[current_intervention_set] = lapply(seq_along(current_site_scale_feature_layer), 
#                                         function(i) list(current_site_scale_feature_layer[[ current_pool[i] ]]))
#     
#   }
#   
#   return(data_stack)
#   
# }



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
  
  scale_names = c('site_scale', 'program_scale', 'landscape_scale')
  
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

  program_scale_cfacs$net_offsets <- sum_list(unlist(program_scale_cfacs[match(c('offset_object', 'uncoupled_offset_object'), 
                                                                               names(program_scale_cfacs))], recursive = FALSE))
  program_scale_cfacs$net_devs <- sum_list(unlist(program_scale_cfacs[match(c('development_object', 'uncoupled_development_object'), 
                                                                            names(program_scale_cfacs))], recursive = FALSE))
  
  return(program_scale_cfacs)
  
}




collate_program_scale_outcomes <- function(simulation_outputs, site_scale_outcomes){
  
  
  program_scale_outcomes = setNames(lapply(seq_along(simulation_outputs$interventions), 
                                           function(i) sum_list(site_scale_outcomes[unlist(simulation_outputs$interventions[[i]]$site_indexes)])), 
                                    names(simulation_outputs$interventions))
  
  program_scale_outcomes$net_outcome <- sum_list(unlist(program_scale_outcomes, recursive = FALSE))
  program_scale_outcomes$net_offsets <- sum_list(unlist(program_scale_outcomes[match(c('offset_object', 'uncoupled_offset_object'), names(program_scale_outcomes))], recursive = FALSE))
  program_scale_outcomes$net_devs <- sum_list(unlist(program_scale_outcomes[match(c('development_object', 'uncoupled_development_object'), names(program_scale_outcomes))], recursive = FALSE))
  
  return(program_scale_outcomes)
}


collate_program_scale_impacts <- function(site_scale_impacts){
  
  program_impacts = setNames(lapply(seq_along(site_scale_impacts), function(i) site_scale_impacts[[i]]$nets), 
                             names(site_scale_impacts))
  
  program_impacts$net_offset_gains = sum_list(append(program_impacts$offset_object, program_impacts$uncoupled_offset_object))
  program_impacts$net_dev_losses = sum_list(append(program_impacts$development_object, program_impacts$uncoupled_development_object))
  program_impacts$net_impacts = sum_list(append(program_impacts$net_offset_gains, program_impacts$net_dev_losses))
  #sum_list(unlist(program_scale_outcomes[match(c('offset_object', 'uncoupled_offset_object'), names(program_scale_outcomes))], recursive = FALSE))
  
  return(program_impacts)
}


calc_intervention_impacts <- function(current_intervention_simulation_outputs, current_intervention_site_sets, current_collate_type, 
                                      site_scale_cfacs_to_use, site_scale_value_at_intervention, site_scale_outcomes_to_use,  
                                      simulation_params, feature_params, global_params, use_offset_metric){
  
  site_scale_impacts <- calc_site_scale_impacts(current_intervention_site_scale_outcomes = site_scale_outcomes_to_use,
                                                current_intervention_cfacs = site_scale_cfacs_to_use,
                                                current_intervention_summed_site_scale_features = site_scale_value_at_intervention,
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
                                               names(site_scale_impacts$grouped_impacts))
  
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
                                        function(i) lapply(seq_along(site_indexes), 
                                                            function(j) collated_object[[i]][which(unlist(site_indexes) %in% site_indexes[[j]])])), 
                                 names(collated_object))
  
  return(grouped_gains_degs)
  
}



sum_data_stack <- function(current_data_dir, file_pattern, use_offset_metric, features_to_collate, cell_num, site_scale_condition_class_key, 
                           transform_function, transform_params, time_steps, numeric_placeholder_width){
  
  if (use_offset_metric == FALSE){
    flog.info('building site scale outcomes...')
  } else {
    flog.info('building site scale outcomes for metric...')
  }
  
  for (yr in 0:time_steps){
    
    current_filename <- list.files(path = current_data_dir,
                                    pattern = paste0(file_pattern, formatC(yr, width = numeric_placeholder_width, format = "d", flag = "0")), 
                                    all.files = FALSE,
                                    include.dirs = FALSE, no.. = FALSE)
    
    site_scale_features = readRDS(paste0(current_data_dir, current_filename))
    
    if (yr == 0){
      data_stack = rep(list(matrix(0, nrow = (time_steps + 1), ncol = length(features_to_collate))), length(site_scale_features))
    }
    
    if (use_offset_metric == FALSE){
      summed_site_scale_features = lapply(seq_along(site_scale_features), 
                                          function(i) matrix(apply(site_scale_features[[i]], 2, 'sum'), nrow = 1))
    } else {

      summed_site_scale_features = lapply(seq_along(site_scale_features), 
                                          function(i) sum(user_transform_function(lapply(seq_along(site_scale_features[[i]]),
                                                                                         function(j) unwrap_condition_classes(array(0, cell_num[[i]]), 
                                                                                                                              site_scale_features[[i]][[j]], 
                                                                                                                              site_scale_condition_class_key[[i]][[j]])), 
                                                                                  transform_params)))
      
    }

    data_stack <- lapply(seq_along(data_stack), function(i) stack_yr(data_stack[[i]], summed_site_scale_features[[i]], yr))
    
  }
  
  return(data_stack)
}

stack_yr <- function(current_site, current_vals, yr){
  current_site[yr + 1, ] = current_vals
  return(current_site)
}



build_site_scale_features_at_intervention <- function(site_num, current_data_dir, intervention_pool, intervention_yrs_pool, file_pattern, numeric_placeholder_width){
  
  site_scale_features_at_intervention = vector('list', site_num)

  for (yr in unique(intervention_yrs_pool)){
    
    current_filename <- list.files(path = current_data_dir,
                                   pattern = paste0(file_pattern, formatC((yr - 1), width = numeric_placeholder_width, format = "d", flag = "0")), 
                                   all.files = FALSE,
                                   include.dirs = FALSE, no.. = FALSE)
    
    current_site_scale_feature_layer = readRDS(paste0(current_data_dir, current_filename))
    
    current_intervention_set = intervention_pool[as.vector(which(intervention_yrs_pool == yr))]
    site_scale_features_at_intervention[current_intervention_set] = current_site_scale_feature_layer[current_intervention_set]
    
  }
  
  return(site_scale_features_at_intervention)
}



merge_vectors <- function(vec_a, vec_b, start_ind){
  vec_a[start_ind:(start_ind + length(vec_b) - 1)] = vec_b
  return(vec_a)
}


calc_site_scale_impacts <- function(current_intervention_site_scale_outcomes, current_intervention_cfacs, current_intervention_summed_site_scale_features, 
                                    current_intervention_yrs, current_intervention_collate_type, simulation_params, feature_params, time_steps, use_offset_metric){
  
  site_num = length(current_intervention_site_scale_outcomes)
  
  impact_trajectories = lapply(seq(site_num), function(i) current_intervention_site_scale_outcomes[[i]][current_intervention_yrs[i]:time_steps, , drop = FALSE])
  
  site_scale_features_at_intervention_block = lapply(seq(site_num), function(i) array(rep(current_intervention_summed_site_scale_features[[i]], length(current_intervention_cfacs[[i]])), dim(current_intervention_cfacs[[i]])))  
  
  avoided_loss = mapply('-', site_scale_features_at_intervention_block, current_intervention_cfacs, SIMPLIFY = FALSE)
  rest_gains = mapply('-', impact_trajectories, site_scale_features_at_intervention_block, SIMPLIFY = FALSE)
  net_gains = mapply('-', impact_trajectories, current_intervention_cfacs, SIMPLIFY = FALSE)
  
  collated_object = list(net_gains, avoided_loss, rest_gains)
  collated_object <- setNames(lapply(seq_along(collated_object), 
                                     function(i) lapply(seq(site_num), 
                                                        function(j) merge_vectors(array(0, c(time_steps, 1)), collated_object[[i]][[j]], current_intervention_yrs[j]))), 
                              c('nets', 'avoided_loss', 'rest_gains'))
  
  if ((current_intervention_collate_type == 'offset_object') | (current_intervention_collate_type == 'uncoupled_offset_object')){
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
sum_sites <- function(site_scale_features, condition_classes, site_scale_condition_class_key, cell_num, use_offset_metric, user_transform_function, transform_params){
  
  if (use_offset_metric == TRUE){

    site_scale_features = lapply(seq_along(site_scale_features), function(i) lapply(seq_along(site_scale_features[[i]]),
                                                                                         function(j) unwrap_condition_classes(array(0, cell_num[[i]]),
                                                                                                                              site_scale_features[[i]][[j]], 
                                                                                                                              site_scale_condition_class_key[[i]][[j]])))
    
    summed_site_scale_features = lapply(seq_along(site_scale_features), function(i) sum(user_transform_function(site_scale_features[[i]], transform_params)))
    
  } else {

    # summed_site_scale_features = lapply(seq_along(site_scale_features), 
    #                                     function(i) apply(site_scale_features[[i]], 2, 'sum'))
    # 
    # summed_site_scale_features = lapply(seq_along(summed_site_scale_features), 
    #                                     function(i) Matrix(summed_site_scale_features[[i]], nrow = 1))
    
    summed_site_scale_features = lapply(seq_along(site_scale_features), 
                                        function(i) matrix(apply(site_scale_features[[i]], 2, 'sum'), nrow = 1))
    
    
  }
  
  return(summed_site_scale_features)
  
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
  } 
  
  current_impacts[abs(current_impacts) < threshold_control] = 0
  
  NNL_yr = FALSE
  
  for (yr_to_test in seq(nrow(current_impacts))){
    
    NNL_yr = all(current_impacts[yr_to_test:nrow(current_impacts)] >= 0)
    
    if (NNL_yr == TRUE){
      break
    }
    
  }
  
  
  if (NNL_yr == TRUE){
    
    NNL_yr = (yr_to_test - intervention_yr) + 1
    
  } 
  
  return(NNL_yr)
  
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
  
  if ((object_type == "development_object") | (object_type == "uncoupled_development_object") | (object_type == "unregulated_loss_object")){
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



