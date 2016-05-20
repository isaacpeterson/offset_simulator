
collate_realisations <- function(realisations, global_params, decline_rates_initial, land_parcels, initial_ecology){
  
  collated_realisations = list()
  
  realisation_num = length(realisations)
  eco_dims = global_params$eco_dims
  
  collated_offset_realisations = collate_realisations_parcel_sets(realisations, parcel_set_type = 'offsets', global_params, decline_rates_initial, land_parcels, initial_ecology)
  
  collated_dev_realisations = collate_realisations_parcel_sets(realisations, parcel_set_type = 'developments', global_params, decline_rates_initial, land_parcels, initial_ecology)
  
  initial_parcel_ecologies = ecology_to_parcels(initial_ecology, land_parcels)
  time_horizons <- generate_time_horizons(use_offset_bank = FALSE, global_params$offset_time_horizon_type, yr = 1, offset_yrs = 1, 
                                          time_horizon = (global_params$time_steps - 1), parcel_count = length(land_parcels))
  
  cfacs <- build_cfacs_list(global_params, decline_rates = decline_rates_initial, initial_parcel_ecologies, parcel_indexes = 1:length(land_parcels), time_horizons)
  cfac_trajs <- sum_trajectories_as_list(cfacs, eco_dims = global_params$eco_dims)
  
  summed_offset_realisations <- group_degs_gains(current_collated_reals = collated_offset_realisations, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims)
  summed_dev_realisations <- group_degs_gains(current_collated_reals = collated_dev_realisations, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims)                                              
  
  net_realisation_gains <- assess_net_gains(summed_offset_realisations$net_outcome, summed_dev_realisations$net_outcome)
  
  system_NNLs <- assess_NNL(net_realisation_gains$standard, assess_NNL_type = 'system', eco_dims = global_params$eco_dims, assess_num = realisation_num, 
                            time_steps = global_params$time_steps, offset_yrs = vector())
  
  landscape_vals = assess_landscape_realisations_vals(realisations, eco_dims = global_params$eco_dims, parcel_indexes = 1:length(land_parcels), time_horizon = global_params$time_steps)
  
  collated_realisations$collated_program_sums <- collate_program_sum_realisations(traj_type = 'trajectory', realisations, cfacs, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims)    #group individual net realisation counterfactual values
  collated_realisations$collated_program_cfac_sums <- collate_program_sum_realisations(traj_type = 'cfac', realisations, cfacs, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims)
  
  if (global_params$use_parcel_sets == TRUE){  
    collated_realisations$parcel_set_NNL_yrs <- assess_parcel_set_NNL_yrs(collated_offset_realisations, collated_dev_realisations, parcel_set_nums = collated_offset_realisations$parcel_set_nums,
                                                                          time_steps = global_params$time_steps, realisation_num,  eco_dims = global_params$eco_dims)
  }
  
  collated_realisations$net_realisation_gains = net_realisation_gains
  collated_realisations$system_NNLs = system_NNLs
  collated_realisations$offsets = collated_offset_realisations
  collated_realisations$devs = collated_dev_realisations
  collated_realisations$cfac_trajs = cfac_trajs
  collated_realisations$summed_offset_realisations = summed_offset_realisations
  collated_realisations$summed_dev_realisations = summed_dev_realisations
  collated_realisations$landscape_vals = landscape_vals
  
  return(collated_realisations)
  
}




assess_landscape_realisations_vals <- function(realisations, eco_dims, parcel_indexes, time_horizon){
  realisation_num = length(realisations)
  landscape_vals = array(0, c(time_horizon, realisation_num, eco_dims))
  for (realisation_ind in 1:realisation_num){
    current_landscape_trajs = sum_parcel_trajectories(realisations[[realisation_ind]]$traj_list, eco_dims, parcel_indexes, time_horizon)
    landscape_vals[, realisation_ind, ] = sum_cols_multi(current_landscape_trajs)
  }
  return(landscape_vals)
}


assess_net_gains <- function(offset_outcome, dev_outcome){
  net_gains = list()
  net_gains$standard = offset_outcome$standard + dev_outcome$standard
  if (global_params$adjust_cfacs_flag == TRUE){
    net_gains$adjusted = offset_outcome$adjusted + dev_outcome$adjusted
  }
  return(net_gains)
}


assess_parcel_set_NNL_yrs <- function(collated_offset_realisations, collated_dev_realisations, parcel_set_nums, 
                                      time_steps, realisation_num, eco_dims){
  
  net_offset_outcomes = list_add(collated_offset_realisations$rest_gains, collated_offset_realisations$avoided_degs)
  net_dev_outcomes = list_add(collated_dev_realisations$rest_gains, collated_dev_realisations$avoided_degs)
  net_outcomes = list_add(net_offset_outcomes, net_dev_outcomes)
  
  parcel_set_NNL_yrs = vector('list', realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){
    current_offset_yrs = collated_offset_realisations$offset_yrs[[realisation_ind]]
    current_net_outcome = net_outcomes[[realisation_ind]]
    parcel_set_num = parcel_set_nums[[realisation_ind]]
    parcel_set_NNL = assess_NNL(current_net_outcome, assess_NNL_type = 'parcel_set', eco_dims, assess_num = parcel_set_num, time_steps, current_offset_yrs)
    parcel_set_NNL_yrs[[realisation_ind]] = parcel_set_NNL$NNL_yrs
  }
  
  return(parcel_set_NNL_yrs)
  
}


assess_allowed_devs <- function(realisations){
  dev_nums = unlist(lapply(seq_along(realisations), function(i) (realisations[[i]]$developments$parcel_set_count)))
  dev_characteristics = c(mean(dev_nums))
  return(dev_characteristics)
}

assess_NNL <- function(net_gains, assess_NNL_type, eco_dims, assess_num, time_steps, offset_yrs){
  
  collate_array = array(0, c(assess_num, eco_dims))
  NNL_yrs = collate_array
  failed_parcel_sets = collate_array
  thresh = 0
  
  for (assess_ind in seq_len(assess_num)){
    
    for (eco_ind in 1:eco_dims){
      current_net_gain = net_gains[, assess_ind, eco_ind]               #select current net_gain to test
      test_NNLs = which(current_net_gain > thresh)
      
      if (length(test_NNLs) > 0){ 
        
        for (test_ind in 1:length(test_NNLs)){
          test_yr = test_NNLs[test_ind]
          current_test_net_gain = current_net_gain[test_yr:length(current_net_gain)]
          
          if (all(current_test_net_gain > thresh)){
            
            if (assess_NNL_type == 'parcel_set'){
              
              offset_yr = min(unlist(offset_yrs[[assess_ind]]))
              NNL_yrs[assess_ind, eco_ind] = test_yr - offset_yr
            } else {
              NNL_yrs[assess_ind, eco_ind] = test_yr
            }
            
            break
            
          } 
        }
      }
    }
  }  
  failed_parcel_sets = which(NNL_yrs == 0)
  NNL_object = list()
  NNL_object$NNL_yrs = NNL_yrs
  NNL_object$failed_parcel_sets = failed_parcel_sets
  NNL_object$success = 1 - length(failed_parcel_sets)/assess_num
  
  return(NNL_object)
  
}



initialise_collated_realisations_object <- function(realisation_num){
  collated_realisations = list()
  collate_list = vector('list', realisation_num)
  collated_realisations$parcel_sums_at_offset = collate_list
  collated_realisations$rest_gains = collate_list
  collated_realisations$avoided_degs = collate_list
  collated_realisations$avoided_degs_adjusted = collate_list
  collated_realisations$parcel_set_nums = collate_list
  collated_realisations$offset_yrs = collate_list
  return(collated_realisations)
}



collate_realisations_parcel_sets <- function(realisations, parcel_set_type, global_params, decline_rates_initial, land_parcels, initial_ecology){
  
  realisation_num = length(realisations)
  collated_realisation_parcel_sets = initialise_collated_realisations_object(realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]
    
    if (parcel_set_type == 'offsets'){
      current_model_outputs = model_outputs$offsets
      cfac_type = global_params$cfac_type_in_offset_calc
    } else {
      if ((global_params$use_offset_bank == TRUE) & (global_params$offset_bank_type == 'credit')){
        current_model_outputs = model_outputs$credited_developments_object
      } else{
        current_model_outputs = model_outputs$developments
      }
      
      cfac_type = global_params$cfac_type_in_dev_calc
    }
    
    parcel_set_indexes = unlist(current_model_outputs$parcel_indexes)
    parcel_set_num = current_model_outputs$parcel_set_count
    
    current_model_outputs$traj_list = model_outputs$traj_list[parcel_set_indexes]
    
    time_horizons = generate_time_horizons(use_offset_bank = TRUE, offset_time_horizon_type = 'current', yr = global_params$time_steps, offset_yrs = unlist(current_model_outputs$offset_yrs), 
                                           time_horizon = (global_params$time_steps - 1), parcel_count = length(unlist(current_model_outputs$offset_yrs)))
    current_cfacs = calc_cfacs(parcel_indexes = parcel_set_indexes, parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE), parcel_num_remaining = current_model_outputs$parcel_num_remaining, global_params, 
                               decline_rates_initial, time_horizons, offset_yrs = current_model_outputs$offset_yrs, cfac_type)
    
    collated_realisation_parcel_sets <- collate_realisations_object(collated_realisation_parcel_sets, realisation_ind, current_model_outputs, current_cfacs, 
                                                                    land_parcels, global_params, decline_rates_initial, parcel_set_num) 
    
  }
  return(collated_realisation_parcel_sets)
  
}



collate_realisations_object <- function(current_collated_realisations, realisation_ind, current_model_outputs, current_cfacs, land_parcels, 
                                        global_params, decline_rates_initial, parcel_set_num){
  eco_dims = global_params$eco_dims
  
  current_parcel_sets_object = collate_parcel_sets(current_model_outputs, current_cfacs, land_parcels, 
                                                   time_steps = global_params$time_steps, eco_dims, decline_rates_initial, parcel_set_num)
  
  current_collated_realisations$parcel_sums_at_offset[[realisation_ind]] = current_parcel_sets_object$parcel_sums_at_offset
  current_collated_realisations$rest_gains[[realisation_ind]] = current_parcel_sets_object$rest_gains
  current_collated_realisations$avoided_degs[[realisation_ind]] = current_parcel_sets_object$avoided_degs
  
  current_collated_realisations$avoided_degs_adjusted[[realisation_ind]] = current_parcel_sets_object$avoided_degs_adjusted
  current_collated_realisations$parcel_set_nums[[realisation_ind]] = parcel_set_num
  current_collated_realisations$offset_yrs[[realisation_ind]] = current_model_outputs$offset_yrs
  #   current_collated_realisations$avoided_degs_include_clearing_offsets[[realisation_ind]] = current_parcel_sets_object$avoided_degs_include_clearing_offsets
  #   
  return(current_collated_realisations)
}



collate_program_sum_realisations <- function(traj_type, realisations, cfacs, time_steps, eco_dims){
  
  realisation_num = length(realisations)
  summed_program_realisations = list()
  collate_list = vector('list', realisation_num) 
  summed_program_realisations$offset_trajs = collate_list
  summed_program_realisations$dev_trajs = collate_list
  summed_program_realisations$net_trajs = collate_list
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]
    if (traj_type == 'cfac'){
      traj_list = cfacs
    } else {traj_list = model_outputs$traj_list}
    parcel_set_num = model_outputs$index_object$parcel_set_count
    
    current_offset_sums = sum_program_parcels(traj_list, eco_dims, model_outputs$offsets$parcel_indexes, time_steps, parcel_set_num = model_outputs$offsets$parcel_set_count)
    current_dev_sums = sum_program_parcels(traj_list, eco_dims, model_outputs$developments$parcel_indexes, time_steps, parcel_set_num = model_outputs$developments$parcel_set_count)
    current_net_sums = current_offset_sums + current_dev_sums
    
    summed_program_realisations$net_trajs[[realisation_ind]] = current_net_sums
    summed_program_realisations$offset_trajs[[realisation_ind]] = current_offset_sums
    summed_program_realisations$dev_trajs[[realisation_ind]] = current_dev_sums
  }
  
  return(summed_program_realisations)
}





sum_program_cfacs <- function(parcel_cfac_trajs, parcel_indexes){
  program_cfacs = parcel_cfac_trajs[, parcel_indexes, ]
  program_cfac_sum = sum_cols_multi(program_cfacs)
  return(program_cfac_sum)
}


collate_program_cfacs <- function(parcel_cfac_trajs, offset_parcel_indexes, dev_parcel_indexes){
  program_cfacs = list()
  program_cfacs$offsets = sum_program_cfacs(parcel_cfac_trajs, offset_parcel_indexes)
  program_cfacs$devs =  sum_program_cfacs(parcel_cfac_trajs, dev_parcel_indexes)
  program_cfacs$net = sum_program_cfacs(parcel_cfac_trajs, c(offset_parcel_indexes, dev_parcel_indexes))
  return(program_cfacs)
}



collate_individual_parcel_array <- function(array_to_collate, parcel_indexes, parcel_set_num, time_steps, eco_dims){
  collated_array = array(0, c(time_steps, parcel_set_num, eco_dims))
  list_to_collate = unlist(parcel_indexes)
  for (parcel_set_ind in seq_len(parcel_set_num)){
    match_inds <- list_intersect(list_to_collate, parcel_indexes[[parcel_set_ind]])
    match_inds <- match_inds$match_ind
    collated_array[, parcel_set_ind, ] = sum_cols_multi(array_to_collate[ , match_inds, ])
  }
  return(collated_array)
}




collate_parcel_sets <- function(current_sets_object, current_cfacs, land_parcels, time_steps, eco_dims, decline_rates, parcel_set_num){
  
  parcel_sets_object = list()
  parcel_num = length(unlist(current_sets_object$parcel_indexes))
  rest_gains = find_gains_degs(current_sets_object, current_traj_list = current_sets_object$traj_list, trajectory_type = 'traj', parcel_num, eco_dims, time_steps)
  avoided_degs = find_gains_degs(current_sets_object, current_traj_list = current_cfacs$cfacs, trajectory_type = 'cfac', parcel_num, eco_dims, time_steps)
  
  rest_gains <- collate_individual_parcel_array(rest_gains, current_sets_object$parcel_indexes, parcel_set_num, time_steps, eco_dims)
  avoided_degs <- collate_individual_parcel_array(avoided_degs, current_sets_object$parcel_indexes, parcel_set_num, time_steps, eco_dims)
  
  if (global_params$adjust_cfacs_flag == TRUE){
    avoided_degs_adjusted = find_gains_degs(current_sets_object, current_traj_list = current_cfacs$adjusted_cfacs, trajectory_type = 'cfac', parcel_num, eco_dims, time_steps)
    avoided_degs_adjusted <- collate_individual_parcel_array(avoided_degs_adjusted, current_sets_object$offset_yrs, parcel_set_num, time_steps, eco_dims)
    
    parcel_sets_object$avoided_degs_adjusted = avoided_degs_adjusted
  }
  #avoided_degs_include_clearing_offsets = find_gains_degs(current_sets_object, current_traj_list = cfac_sets_object$cfacs_include_clearing_offsets, trajectory_type = 'cfac', parcel_set_num, time_horizon, eco_dims)
  parcel_sums_at_offset = collate_initial_sums(current_sets_object$parcel_sums, parcel_set_num)
  
  parcel_sets_object$parcel_sums_at_offset = parcel_sums_at_offset
  parcel_sets_object$rest_gains = rest_gains
  parcel_sets_object$avoided_degs = avoided_degs
  
  return(parcel_sets_object)
  
}  



collate_initial_sums <- function(initial_sums_list, parcel_set_num){
  initial_sums_array = vector()
  
  for (parcel_set_ind in 1:parcel_set_num){
    initial_sums_array = c(initial_sums_array, initial_sums_list[[parcel_set_ind]])
  }
  
  return(initial_sums_array)
  
}



find_gains_degs <- function(current_sets_object, current_traj_list, trajectory_type, parcel_num, eco_dims, time_steps){
  
  gains_degs = array(0, c(time_steps, parcel_num, eco_dims))
  offset_yrs = current_sets_object$offset_yrs
  
  if (class(offset_yrs) == 'list'){
    offset_yrs = unlist(offset_yrs)
  }
  
  parcel_ecologies = unlist(current_sets_object$parcel_ecologies, recursive = FALSE)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_yr = offset_yrs[parcel_count_ind]
    for (eco_ind in seq_len(global_params$eco_dims)){
      
      current_parcel_ecology = parcel_ecologies[[parcel_count_ind]][, , eco_ind]
      if (trajectory_type == 'traj'){
        current_parcel_traj = current_traj_list[[parcel_count_ind]][[eco_ind]][, , current_yr:time_steps]
      } else {
        current_parcel_traj = current_traj_list[[parcel_count_ind]][[eco_ind]]
      }
      current_sum = sum_rel_intial(current_parcel_traj, current_parcel_ecology, trajectory_type, time_steps, current_yr)
    }
    gains_degs[, parcel_count_ind, eco_ind] = current_sum
    
  }
  
  return(gains_degs)
}






group_degs_gains <- function(current_collated_reals, time_steps, eco_dims){
  
  summed_realisations = list()
  summed_realisations$rest_gains = sum_collated_realisations(current_collated_reals$rest_gains, time_steps, eco_dims)
  summed_realisations$avoided_degs = sum_collated_realisations(current_collated_reals$avoided_degs, time_steps, eco_dims)
  if (global_params$adjust_cfacs_flag == TRUE){
    summed_realisations$avoided_degs_adjusted = sum_collated_realisations(current_collated_reals$avoided_degs_adjusted, time_steps, eco_dims)
  }
  summed_realisations$net_outcome <- collate_net_realisation_outcome(summed_realisations)
  return(summed_realisations)
  
}




sum_collated_realisations <- function(reals, time_steps, eco_dims){
  realisation_num = length(realisations)
  summed_collated_realisations = array(0, c(time_steps, realisation_num, eco_dims))
  for (realisation_ind in seq_len(realisation_num)){
    summed_collated_realisations[, realisation_ind, ] = sum_cols_multi(reals[[realisation_ind]])
  }
  return(summed_collated_realisations)
}





collate_net_realisation_outcome <- function(current_summed_reals){
  net_outcome = list()
  net_outcome$standard = current_summed_reals$rest_gains + current_summed_reals$avoided_degs
  if (global_params$adjust_cfacs_flag == TRUE){
    net_outcome$adjusted = current_summed_reals$rest_gains + current_summed_reals$avoided_degs_adjusted
  }
  return(net_outcome)
}







sum_rel_intial <- function(current_parcel_traj, current_parcel_ecology, trajectory_type, time_steps, yr){
  rel_arr = array(0, c(dim(current_parcel_ecology), time_steps))
  rel_arr[, , yr:time_steps] = current_parcel_traj - as.vector(current_parcel_ecology)
  
  if (trajectory_type == 'cfac'){
    rel_arr = -rel_arr
  } 
  rel_arr = apply(rel_arr, 3, sum)
  return(rel_arr)
}




assess_failed_NNL <- function(NNLs){
  
  NNL_fails = which(NNLs == 0)
  if (length(NNL_fails) > 0){
    failed_frac = length(NNL_fails)/length(NNLs)
    NNLs = NNLs[-NNL_fails]
  } else {failed_frac = 0}
  
  if (failed_frac < 1){
    x_lab = paste('NNL at', round(mean(NNLs)), '\U00b1', round(max(range(NNLs) - (mean(NNLs))), 1), 'years', '(NNL success = ', round((1 - failed_frac)*100), '%)' )
  } else x_lab = 'All realisations failed NNL'
  
  NNL_plot_object = list()
  NNL_plot_object$failed_frac = failed_frac
  NNL_plot_object$NNLs = NNLs
  NNL_plot_object$x_lab = x_lab
  return(NNL_plot_object)
}



write_realisation_sums <- function(current_realisations, realisation_ind, current_summed_realisation){
  current_realisations$net_gains[, realisation_ind, ] = current_summed_realisation$net_gains
  current_realisations$avoided_degs[, realisation_ind, ] = current_summed_realisation$avoided_degs
  current_realisations$rest_gains[, realisation_ind, ] = current_summed_realisation$rest_gains
  return(current_realisations)
}

write_summed_programs <- function(summed_program_realisations, realisation_ind, current_summed_program){
  summed_program_realisations$net_program_value[, realisation_ind, ] = current_summed_program$net_trajs
  summed_program_realisations$net_offset_value[, realisation_ind, ] = current_summed_program$offset_trajs
  summed_program_realisations$net_development_value[, realisation_ind, ] = current_summed_program$dev_trajs
  return(summed_program_realisations)
}


collate_program_cfac_sum <- function(realisations, time_horizon, eco_dims){
  realisation_num = length(realisations)
  collate_array = array(0, c(time_horizon, realisation_num, eco_dims))
  for (realisation_ind in 1:realisation_num){
    collate_array[, realisation_ind, ] = realisations[[realisation_ind]]$summed_program_cfacs$net
  }
  return(collate_array)
}



collate_realisation_parcel_set_sums <- function(realisations, rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon, eco_dims){
  
  realisation_num = length(realisations)
  collate_array = array(0, c(time_horizon, realisation_num, eco_dims))
  summed_realisations = list()
  summed_realisations$net_gains = collate_array
  summed_realisations$avoided_degs = collate_array
  summed_realisations$rest_gains = collate_array
  
  rm(collate_array)
  
  for (realisation_ind in seq_len(realisation_num)){
    current_realisation = realisations[[realisation_ind]]
    collated_parcel_sets_object = current_realisation$collated_parcel_sets_object
    current_sum = sum_parcel_sets_as_list(rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon)
    summed_realisations = write_realisation_sums(summed_realisations, realisation_ind, current_sum)
  }
  
}


sum_program_parcels <- function(traj_list, eco_dims, parcel_indexes, time_steps, parcel_set_num){
  
  parcel_set_trajs = sum_parcel_trajectories(traj_list, eco_dims, unlist(parcel_indexes), time_steps) 
  parcel_set_trajs = collate_individual_parcel_array(parcel_set_trajs, parcel_indexes, parcel_set_num, time_steps, eco_dims)
  summed_set_trajs = sum_cols_multi(parcel_set_trajs)
  return(summed_set_trajs)
}



assess_degs <- function(parcel_set_to_assess, cfac_type, collate_dims){
  
  if (cfac_type == 'standard'){
    current_avoided_deg = apply(parcel_set_to_assess$avoided_degs, MARGIN = 1, sum)
  } else if (cfac_type == 'include_clearing'){
    current_avoided_deg = apply(parcel_set_to_assess$avoided_degs_include_clearing, MARGIN = 1, sum)
  } else if (cfac_type == 'include_clearing_offsets'){
    current_avoided_deg = apply(parcel_set_to_assess$avoided_degs_include_clearing_offsets, MARGIN = 1, sum)
  }
  current_avoided_deg = t(t(current_avoided_deg))
  return(current_avoided_deg)
}


