prepare_realisations <- function(realisations){   #remove unsuccessful offset programs
  offset_success_flag = unlist(lapply(seq_along(realisations), function(i) realisations[[i]]$offset_success_flag))
  success_inds = which(offset_success_flag == TRUE)
  realisations <- lapply(success_inds, function(i) realisations[[i]])
  return(realisations)
}


collate_realisations <- function(realisations, global_params, dev_vec, decline_rates_initial, land_parcels, initial_ecology){
  land_parcels = parcels$land_parcels
  collated_realisations = list()
  
  eco_dims = global_params$eco_dims
  
  realisation_num = length(realisations)
 
  collated_offset_realisations = collate_parcel_sets(realisations, realisation_num, collate_type = 'offsets', global_params, dev_vec, decline_rates_initial, land_parcels, initial_ecology)
  
  collated_dev_realisations = collate_parcel_sets(realisations, realisation_num, collate_type = 'developments', global_params, dev_vec, decline_rates_initial, land_parcels, initial_ecology)
  
  time_horizons <- generate_time_horizons(time_horizon_type = 'offsets', project_type = 'future', yr = 1, offset_yrs = 1, time_horizon = (global_params$time_steps - 1), parcel_count = length(land_parcels))

  cfacs <- calc_parcel_trajs(parcel_ecologies = initial_ecology, parcel_traj_type = 'protect', decline_rates = decline_rates_initial, time_horizons, global_params, time_fill = TRUE)
  cfac_trajs <- sum_trajectories(cfacs, eco_dims = global_params$eco_dims)
  net_cfac_sum = nested_list_sum(cfac_trajs)
  
  summed_offset_realisations <- group_gains_degs(current_collated_reals = collated_offset_realisations, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims, 
                                                 cfac_type = global_params$cfac_type_in_offset_calc, adjust_cfacs_flag =  global_params$adjust_offset_cfacs_flag)
  summed_dev_realisations <- group_gains_degs(current_collated_reals = collated_dev_realisations, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims, 
                                              cfac_type = global_params$cfac_type_in_dev_calc, adjust_cfacs_flag =  global_params$adjust_dev_cfacs_flag)                                              
  
  net_realisation_gains <- assess_net_gains(summed_offset_realisations$net_outcome, summed_dev_realisations$net_outcome, 
                                            use_adjusted_cfacs = (global_params$adjust_offset_cfacs_flag & global_params$adjust_dev_cfacs_flag), eco_dims)

  system_NNL_object <- assess_system_NNL(net_realisation_gains = net_realisation_gains$standard, realisation_num, time_steps = global_params$time_steps, 
                                         offset_time_horizon = global_params$offset_time_horizon, eco_dims)
 
  landscape_vals = assess_landscape(realisations, eco_dims = global_params$eco_dims, parcel_indexes = 1:length(land_parcels), time_horizon = global_params$time_steps)
  landscape_outcome <- assess_landscape_outcome(landscape_vals, net_cfac_sum, realisation_num, eco_dims = global_params$eco_dims)
    
  program_sums <- collate_program(traj_type = 'trajectory', realisations, cfacs, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims)    #group individual net realisation counterfactual values
  program_cfac_sums <- collate_program(traj_type = 'cfac', realisations, cfacs, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims)
  
  if (global_params$use_parcel_sets == TRUE){  
    parcel_set_NNL_object = assess_parcel_set_NNL(collated_offset_realisations, collated_dev_realisations, parcel_set_nums = collated_offset_realisations$parcel_set_nums, 
                                                  time_steps = global_params$time_steps, realisation_num,  eco_dims, offset_time_horizon = global_params$offset_time_horizon)
  } else{
    parcel_set_NNL_object = list()
    parcel_set_NNL_object$NNL_yrs = list()
    parcel_set_NNL_object$NNL_success = list()
  }
  
  landscape_loss = assess_landscape_loss(landscape_vals, system_NNL_object$NNL_yrs, realisation_num, eco_dims, time_steps = global_params$time_steps)
  program_loss = assess_landscape_loss(program_cfac_sums$net, system_NNL_object$NNL_yrs, realisation_num, eco_dims, time_steps = global_params$time_steps)
  
  collated_realisations$landscape_loss = landscape_loss
  collated_realisations$program_loss = program_loss
  collated_realisations$landscape_outcome = landscape_outcome
  collated_realisations$program_cfac_sums = program_cfac_sums
  collated_realisations$net_cfac_sum = net_cfac_sum
  collated_realisations$system_NNL_object = system_NNL_object
  
  collated_realisations$cfac_trajs = cfac_trajs
  collated_realisations$parcel_set_NNL_object = parcel_set_NNL_object
  collated_realisations$program_sums = program_sums
  
  collated_realisations$net_realisation_gains = net_realisation_gains
  
  collated_realisations$offsets = collated_offset_realisations
  collated_realisations$devs = collated_dev_realisations

  collated_realisations$summed_offset_realisations = summed_offset_realisations
  collated_realisations$summed_dev_realisations = summed_dev_realisations
  collated_realisations$landscape_vals = landscape_vals
  
  return(collated_realisations)
  
}

assess_system_NNL <- function(net_realisation_gains, realisation_num, time_steps, offset_time_horizon, eco_dims){
  system_NNL_object <- list()
  system_NNL_object$NNL_yrs <- find_NNL(net_realisation_gains, assess_type = 'system', eco_dims, assess_num = realisation_num, time_steps, offset_yrs = vector(), offset_time_horizon)
  system_NNL_object$NNL_success = assess_NNL(system_NNL_object$NNL_yrs, assess_type = 'success', eco_dims)
  system_NNL_object$NNL_mean = assess_NNL(system_NNL_object$NNL_yrs, assess_type = 'mean', eco_dims)
  return(system_NNL_object)
}


assess_parcel_set_NNL <- function(collated_offset_realisations, collated_dev_realisations, parcel_set_nums, time_steps, realisation_num,  eco_dims, offset_time_horizon){
  parcel_set_NNL_object <- list()
  parcel_set_NNL_object$NNL_yrs <- find_parcel_set_NNL_yrs(collated_offset_realisations, collated_dev_realisations, parcel_set_nums, time_steps, realisation_num,  eco_dims, offset_time_horizon)
  parcel_set_NNL_object$NNL_success = assess_NNL(unlist(parcel_set_NNL_object$NNL_yrs, recursive = FALSE), assess_type = 'success', eco_dims)
  parcel_set_NNL_object$NNL_mean = assess_NNL(unlist(parcel_set_NNL_object$NNL_yrs, recursive = FALSE), assess_type = 'mean', eco_dims)
  return(parcel_set_NNL_object)
}


assess_landscape_outcome <- function(landscape_vals, net_cfac_sum, realisation_num, eco_dims){
  landscape_outcome <- lapply(seq_len(realisation_num), function(i) mapply('-', landscape_vals[[i]], net_cfac_sum, SIMPLIFY = FALSE))
  return(landscape_outcome)
}
  





assess_landscape <- function(realisations, eco_dims, parcel_indexes, time_horizon){
  realisation_num = length(realisations)
  
  landscape_vals_list = vector('list', realisation_num)
  
  for (realisation_ind in 1:realisation_num){
    current_landscape_trajs = sum_trajectories(realisations[[realisation_ind]]$trajectories, eco_dims)
    landscape_vals_list[[realisation_ind]] = nested_list_sum(current_landscape_trajs)
  }
  return(landscape_vals_list)
}




#assess_landscape_loss(landscape_vals, loss_type = 'total_loss', system_NNLs$NNL_yrs, realisation_num, eco_dims, time_steps = global_params$time_steps)


# loss_type = 'total_loss'
# NNL_yrs = system_NNL_yrs





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

# assess_net_gains(summed_offset_realisations$net_outcome, summed_dev_realisations$net_outcome, 
#                  use_adjusted_cfacs = (global_params$adjust_offset_cfacs_flag & global_params$adjust_dev_cfacs_flag), eco_dims)

# net_offset_outcome = summed_offset_realisations$net_outcome
# net_dev_outcome = summed_dev_realisations$net_outcome
# use_adjusted_cfacs = (global_params$adjust_offset_cfacs_flag & global_params$adjust_dev_cfacs_flag)

assess_net_gains <- function(net_offset_outcome, net_dev_outcome, use_adjusted_cfacs, eco_dims){
  net_gains = list()
  net_gains$standard = sum_nested_lists(net_offset_outcome$standard, net_dev_outcome$standard, inner_dim = eco_dims)
  if (use_adjusted_cfacs == TRUE){
    net_gains$adjusted = sum_nested_lists(net_offset_outcome$adjusted, net_dev_outcome$adjusted, inner_dim = eco_dims)
  }
  return(net_gains)
}


find_parcel_set_NNL_yrs <- function(collated_offset_realisations, collated_dev_realisations, parcel_set_nums, 
                                      time_steps, realisation_num, eco_dims, offset_time_horizon){

  current_net_offset_outcome = sum_nested_lists(collated_offset_realisations$rest_gains, collated_offset_realisations$avoided_degs, eco_dims)
  current_net_dev_outcome = sum_nested_lists(collated_dev_realisations$rest_gains, collated_dev_realisations$avoided_degs, eco_dims)
  net_outcomes = sum_nested_lists(current_net_offset_outcome, current_net_dev_outcome, eco_dims)
  
  parcel_set_NNL_yrs = vector('list', realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){
    current_offset_yrs = collated_offset_realisations$offset_yrs[[realisation_ind]]
    current_net_outcome = net_outcomes[[realisation_ind]]
    parcel_set_num = parcel_set_nums[[realisation_ind]]
    NNL_yrs = find_NNL(current_net_outcome, assess_type = 'parcel_set', eco_dims, assess_num = parcel_set_num, time_steps, current_offset_yrs, offset_time_horizon)
    parcel_set_NNL_yrs[[realisation_ind]] = NNL_yrs
  }
  
  return(parcel_set_NNL_yrs)
  
}


assess_allowed_devs <- function(realisations){
  dev_nums = unlist(lapply(seq_along(realisations), function(i) length(realisations[[i]]$developments$parcel_indexes)))
  dev_characteristics = c(mean(dev_nums))
  return(dev_characteristics)
}



# system_NNLs <- assess_NNL(net_realisation_gains$standard, assess_type = 'system', eco_dims = global_params$eco_dims, assess_num = realisation_num, 
#                           time_steps = global_params$time_steps, offset_yrs = vector(), offset_time_horizon = global_params$offset_time_horizon)

# net_gains = net_realisation_gains$standard
# assess_type = 'system'
# eco_dims = global_params$eco_dims
# assess_num = realisation_num
# time_steps = global_params$time_steps
# offset_yrs = vector()

# net_gains = current_net_outcome
# assess_type = 'parcel_set'
# assess_num = parcel_set_num
# offset_yrs = current_offset_yrs
#assess_NNL(current_net_outcome, assess_type = 'parcel_set', eco_dims, assess_num = parcel_set_num, time_steps, current_offset_yrs)


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


# assess_NNL <- function(net_gains, assess_type, eco_dims, assess_num, time_steps, offset_yrs, offset_time_horizon){
#   
#   NNL_yrs = array(0, c(assess_num, eco_dims))
#   thresh = 0
#   
#   for (assess_ind in seq_len(assess_num)){
#     
#     for (eco_ind in 1:eco_dims){
#       
#       if (assess_type == 'system'){
#         current_net_gain = net_gains[[assess_ind]][[eco_ind]]               #select current net_gain to test
#       } else if (assess_type == 'parcel_set'){
#         current_net_gain = net_gains[[eco_ind]][, assess_ind]               #select current net_gain to test
#       }
#       test_NNLs = which(current_net_gain > thresh)
#       
#       if (length(test_NNLs) > 0){ 
#         
#         for (test_ind in 1:length(test_NNLs)){
#           test_yr = test_NNLs[test_ind]
#           current_test_net_gain = current_net_gain[test_yr:length(current_net_gain)]
#           
#           if (all(current_test_net_gain > thresh)){
#             
#             if (assess_type == 'parcel_set'){
#               
#               offset_yr = min(unlist(offset_yrs[[assess_ind]]))
#               NNL_yrs[assess_ind, eco_ind] = test_yr - offset_yr
#             } else {
#               NNL_yrs[assess_ind, eco_ind] = test_yr
#             }
#             
#             break
#             
#           } 
#         }
#       }
#     }
#   }  
#   
#   if (assess_type == 'parcel_set'){
#     NNL_yrs = NNL_yrs[1:(time_steps - offset_time_horizon)]
#   }
#   failed_parcel_sets = which(NNL_yrs == 0)
#   NNL_object = list()
#   NNL_object$NNL_yrs = NNL_yrs
#   NNL_object$failed_parcel_sets = failed_parcel_sets
#   NNL_object$success = 1 - length(failed_parcel_sets)/assess_num
#   
#   return(NNL_object)
#   
# }







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


#collated_offset_parcel_sets = collate_parcel_sets(realisations, realisation_num = 1, collate_type = 'offsets', global_params, dev_vec, decline_rates_initial, land_parcels, initial_ecology)

# realisation_num = length(realisations)
# collate_type = 'offsets'

find_list_subset <- function(list_a, subset_inds){
  list_length = length(list_a)
  for (list_ind in seq(list_length)){
    list_a[[list_ind]] = list_a[[list_ind]][subset_inds]
  }
  return(list_a)
}

collate_parcel_sets <- function(realisations, realisation_num, collate_type, global_params, dev_vec, decline_rates_initial, land_parcels, initial_ecology){
  
  collated_parcel_sets = initialise_collated_realisations_object(realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]
    
    if (collate_type == 'offsets'){
      current_model_outputs = model_outputs$offsets
     # parcel_sets_to_use <- which(lengths(model_outputs$offsets$parcel_indexes) > 0) #only consider parcel sets that are in the offset program
      cfac_type = global_params$cfac_type_in_offset_calc
      adjust_cfacs_flag = global_params$adjust_offset_cfacs_flag
      
    } else if (collate_type == 'developments'){
      if ((global_params$use_offset_bank == TRUE) & (global_params$offset_bank_type == 'credit')){
        current_model_outputs = model_outputs$credited_developments_object
      } else{
        #parcel_sets_to_use <- which(lengths(model_outputs$offsets$parcel_indexes) > 0) #only consider parcel sets that are in the offset program
        current_model_outputs = model_outputs$developments
      }
      
      cfac_type = global_params$cfac_type_in_dev_calc
      adjust_cfacs_flag = global_params$adjust_dev_cfacs_flag
    }
    
#     parcel_set_indexes = unlist(current_model_outputs$parcel_indexes)
#     parcel_set_num = current_model_outputs$parcel_set_count
#     
  # current_model_outputs = find_list_subset(current_model_outputs, parcel_sets_to_use)
    parcel_set_num = length(current_model_outputs$parcel_indexes)
    parcel_set_indexes = unlist(current_model_outputs$parcel_indexes)

    current_model_outputs$trajectories = model_outputs$trajectories[parcel_set_indexes]
    
    time_horizons = generate_time_horizons(time_horizon_type = 'offset_bank', project_type = 'current', yr = global_params$time_steps, offset_yrs = unlist(current_model_outputs$offset_yrs), 
                           time_horizon = (global_params$time_steps - 1), parcel_count = length(unlist(current_model_outputs$offset_yrs)))
    
#     time_horizons = generate_time_horizons(use_offset_bank = TRUE, time_horizon_type = 'current', yr = global_params$time_steps, offset_yrs = unlist(current_model_outputs$offset_yrs), 
#                                            time_horizon = (global_params$time_steps - 1), parcel_count = length(unlist(current_model_outputs$offset_yrs)))

    current_cfacs = calc_cfacs(unlist(current_model_outputs$parcel_ecologies, recursive = FALSE), parcel_num_remaining = current_model_outputs$parcel_num_remaining, global_params, dev_vec,
                               decline_rates_initial[parcel_set_indexes], time_horizons, offset_yrs = current_model_outputs$offset_yrs, cfac_type, adjust_cfacs_flag)
    
    current_parcel_sets_object = collate_current_parcel_sets(current_model_outputs, collate_type, current_cfacs, time_steps = global_params$time_steps, 
                                                             eco_dims = global_params$eco_dims, parcel_set_num, adjust_cfacs_flag)
      
    collated_parcel_sets <- update_parcel_sets_object(collated_parcel_sets, current_parcel_sets_object, realisation_ind, current_model_outputs$offset_yrs, parcel_set_num) 
    
  }
  
  return(collated_parcel_sets)
}


update_parcel_sets_object <- function(collated_parcel_sets, current_parcel_sets_object, realisation_ind, offset_yrs, parcel_set_num){
  
  collated_parcel_sets$parcel_sums_at_offset[[realisation_ind]] = current_parcel_sets_object$parcel_sums_at_offset
  collated_parcel_sets$rest_gains[[realisation_ind]] = current_parcel_sets_object$rest_gains
  collated_parcel_sets$avoided_degs[[realisation_ind]] = current_parcel_sets_object$avoided_degs
  
  collated_parcel_sets$avoided_degs_adjusted[[realisation_ind]] = current_parcel_sets_object$avoided_degs_adjusted
  collated_parcel_sets$parcel_set_nums[[realisation_ind]] = parcel_set_num
  collated_parcel_sets$offset_yrs[[realisation_ind]] = offset_yrs
  #   current_collated_realisations$avoided_degs_include_clearing_offsets[[realisation_ind]] = current_parcel_sets_object$avoided_degs_include_clearing_offsets
  #   
  return(collated_parcel_sets)
}




#collated_program_sums <- collate_program_sum_realisations(traj_type = 'trajectory', realisations, cfacs, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims) 


#collate_program(traj_type = 'trajectory', collate_type = 'offsets', realisations, cfacs, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims) 


collate_program <- function(traj_type, realisations, cfacs, time_steps, eco_dims){
  collated_program = list()
  collated_program$offsets <- collate_program_element(traj_type, collate_type = 'offsets', realisations, cfacs, time_steps, eco_dims)
  collated_program$devs <- collate_program_element(traj_type, collate_type = 'devs', realisations, cfacs, time_steps, eco_dims)
  collated_program$net <- sum_nested_lists(collated_program$offsets, collated_program$devs, inner_dim = eco_dims)
  return(collated_program)
}



collate_program_element <- function(traj_type, collate_type, realisations, cfacs, time_steps, eco_dims){
  
  realisation_num = length(realisations)
  collate_list = vector('list', realisation_num) 
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]
    if (traj_type == 'cfac'){
      trajectories = cfacs
    } else {
      trajectories = model_outputs$trajectories
    }
    
    if (collate_type == 'offsets'){
      parcel_indexes = model_outputs$offsets$parcel_indexes
    } else if (collate_type == 'devs'){
      parcel_indexes = model_outputs$developments$parcel_indexes
    }
    
    #parcel_sets_to_use <- which(lengths(model_outputs$offsets$parcel_indexes) > 0) #only consider parcel sets that are in the offset program
    #parcel_indexes <- parcel_indexes[parcel_sets_to_use]
    parcel_set_num <- length(parcel_indexes) 
    
    collate_list[[realisation_ind]] = sum_program_parcels(trajectories, eco_dims, unlist(parcel_indexes), time_steps, parcel_set_num)
    
  }
  
  return(collate_list)
}





# collate_program_sum_realisations <- function(traj_type, realisations, cfacs, time_steps, eco_dims){
#   
#   realisation_num = length(realisations)
#   summed_program_realisations = list()
#   collate_list = vector('list', realisation_num) 
#   summed_program_realisations$offset_trajs = collate_list
#   summed_program_realisations$dev_trajs = collate_list
#   summed_program_realisations$net_trajs = collate_list
#   
#   for (realisation_ind in seq_len(realisation_num)){
#     model_outputs = realisations[[realisation_ind]]
#     if (traj_type == 'cfac'){
#       trajectories = cfacs
#     } else {trajectories = model_outputs$trajectories}
#     parcel_set_num = model_outputs$index_object$parcel_set_count
#     
#     current_offset_sums = sum_program_parcels(trajectories, eco_dims, model_outputs$offsets$parcel_indexes, time_steps, parcel_set_num = model_outputs$offsets$parcel_set_count)
#     
#     current_dev_sums = sum_program_parcels(trajectories, eco_dims, model_outputs$developments$parcel_indexes, time_steps, parcel_set_num = model_outputs$developments$parcel_set_count)
#     current_net_sums = sum_lists(current_offset_sums, current_dev_sums)
#     
#     summed_program_realisations$net_trajs[[realisation_ind]] = current_net_sums
#     summed_program_realisations$offset_trajs[[realisation_ind]] = current_offset_sums
#     summed_program_realisations$dev_trajs[[realisation_ind]] = current_dev_sums
#   }
#   
#   return(summed_program_realisations)
# }


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


# combine_to_parcel_set(gains_degs_object$rest_gains, current_model_outputs$parcel_indexes, parcel_set_num, time_steps, eco_dims)
# 
# list_to_collate = gains_degs_object$rest_gains
# parcel_indexes = current_model_outputs$parcel_indexes

# lapply(seq_along(eco_dims) function(i) Reduce('+', list_to_collate[c(1, 2, 3)][[i]]))

#rest_gains <- combine_to_parcel_set(gains_degs_object$rest_gains, current_model_outputs$parcel_indexes, parcel_set_num, time_steps, eco_dims)

combine_to_parcel_set <- function(list_to_collate, parcel_indexes, parcel_set_num, time_steps, eco_dims){
  
  collate_list = vector('list', eco_dims)
  
  for (eco_ind in seq(eco_dims)){
    collate_list[[eco_ind]] = array(0, c(time_steps, parcel_set_num))
  }
  
  parcel_inds_to_collate = unlist(parcel_indexes)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    match_inds <- list_intersect(parcel_inds_to_collate, parcel_indexes[[parcel_set_ind]])
    match_inds <- match_inds$match_ind
    current_list = list_to_collate[match_inds]
    for (eco_ind in seq(eco_dims)){
      collate_list[[eco_ind]][, parcel_set_ind] = Reduce('+', lapply(seq_along(current_list), function(i) current_list[[i]][[eco_ind]]))
    }
  }
  
  return(collate_list)
}




# collate_current_parcel_sets(current_model_outputs, collate_type, current_cfacs, land_parcels, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims, decline_rates_initial, 
#                             parcel_set_num, cfac_type, adjust_cfacs_flag)

collate_current_parcel_sets <- function(current_model_outputs, collate_type, current_cfacs, time_steps, eco_dims, parcel_set_num, adjust_cfacs_flag){
  
  parcel_sets_object = list()
  parcel_num = length(unlist(current_model_outputs$parcel_indexes))
  gains_degs_object <- collate_gains_degs(current_model_outputs, collate_type, current_model_outputs$trajectories, current_cfacs$cfacs, parcel_num, eco_dims, time_steps)
  rest_gains <- combine_to_parcel_set(gains_degs_object$rest_gains, current_model_outputs$parcel_indexes, parcel_set_num, time_steps, eco_dims)
  avoided_degs <- combine_to_parcel_set(gains_degs_object$avoided_degs, current_model_outputs$parcel_indexes, parcel_set_num, time_steps, eco_dims)

  if (adjust_cfacs_flag == TRUE){
    gains_degs_object <- collate_gains_degs(current_model_outputs, collate_type, current_model_outputs$trajectories, current_cfacs$adjusted_cfacs, parcel_num, eco_dims, time_steps)
    avoided_degs_adjusted <- combine_to_parcel_set(gains_degs_object$avoided_degs, current_model_outputs$parcel_indexes, parcel_set_num, time_steps, eco_dims)
    parcel_sets_object$avoided_degs_adjusted = avoided_degs_adjusted
    
  }
  parcel_sums_at_offset = collate_initial_sums(current_model_outputs$parcel_sums, parcel_set_num)
  
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


# trajectories = current_model_outputs$trajectories
# cfac_list = current_cfacs$cfacs

collate_gains_degs <- function(current_model_outputs, collate_type, trajectories, cfac_list, parcel_num, eco_dims, time_steps){
  
  rest_gains = vector('list', parcel_num) 
  for (parcel_count_ind in 1:parcel_num){
    rest_gains[[parcel_count_ind]] = list_of_arrays(list_dims = eco_dims, array_dims = time_steps)
  }
  avoided_degs = rest_gains       # initialise array to store rest. gains / avoided degs
  offset_yrs = current_model_outputs$offset_yrs                         # vector containing year offset was initialised
  
  if (class(offset_yrs) == 'list'){
    offset_yrs = unlist(offset_yrs)
  }
  
  parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE)        #remove lowest list level 
  
  for (parcel_count_ind in seq_len(parcel_num)){                      #cycle through all parcels to collate
    current_yr = offset_yrs[parcel_count_ind]                         #select current offset year 
    
    for (eco_ind in seq_len(global_params$eco_dims)){                 # cycle through ecological dimensions
      
      current_parcel_ecology = parcel_ecologies[[parcel_count_ind]][[eco_ind]]
      current_parcel_traj = trajectories[[parcel_count_ind]][[eco_ind]][current_yr:time_steps, ]
      current_parcel_cfac = cfac_list[[parcel_count_ind]][[eco_ind]]
      current_gains_degs <- separate_gains_degs(current_parcel_traj, collate_type, current_parcel_cfac, current_parcel_ecology, current_time_horizon = (time_steps - current_yr + 1))
      rest_gains[[parcel_count_ind]][[eco_ind]][current_yr:time_steps] = current_gains_degs$gains
      avoided_degs[[parcel_count_ind]][[eco_ind]][current_yr:time_steps] = current_gains_degs$degs
    }

  }
  collate_object = list()
  collate_object$rest_gains = rest_gains
  collate_object$avoided_degs = avoided_degs
  return(collate_object)
}


#current_gains_degs <- separate_gains_degs(current_parcel_traj, collate_type, current_parcel_cfac, current_parcel_ecology)

separate_gains_degs <- function(current_parcel_traj, collate_type, current_parcel_cfac, current_parcel_ecology, current_time_horizon){
  current_eco_array = matrix(rep(current_parcel_ecology, current_time_horizon), nrow = current_time_horizon, byrow = TRUE)
  rest_gains = apply(current_parcel_traj - current_eco_array, 1, 'sum')
  avoided_degs = apply(current_eco_array - current_parcel_cfac, 1, 'sum')
  
  if (collate_type == 'offsets'){
    rest_gains = rest_gains * (rest_gains > 0)
    rest_fail = apply((current_parcel_traj - current_parcel_cfac), 1, 'sum')
    rest_fail_inds = which(rest_gains <= 0)
    avoided_degs[rest_fail_inds] = rest_fail[rest_fail_inds]
  } 
  
  gains_degs_object = list()
  gains_degs_object$gains = rest_gains * (avoided_degs > 1e-10)           #remove machine error
  gains_degs_object$degs = avoided_degs * (avoided_degs > 1e-10)
  return(gains_degs_object)
  
}


# 
# find_gains_degs <- function(current_model_outputs, current_trajectories, trajectory_type, parcel_num, eco_dims, time_steps){
#   
#   gains_degs = array(0, c(time_steps, parcel_num, eco_dims))
#   offset_yrs = current_model_outputs$offset_yrs
#   
#   if (class(offset_yrs) == 'list'){
#     offset_yrs = unlist(offset_yrs)
#   }
#   
#   parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE)
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     current_yr = offset_yrs[parcel_count_ind]
#     for (eco_ind in seq_len(global_params$eco_dims)){
#       
#       current_parcel_ecology = parcel_ecologies[[parcel_count_ind]]
#       if (trajectory_type == 'traj'){
#         current_parcel_traj = current_trajectories[[parcel_count_ind]][[eco_ind]][, , current_yr:time_steps]
#       } else {
#         current_parcel_traj = current_trajectories[[parcel_count_ind]][[eco_ind]]
#       }
#       current_sum = sum_rel_intial(current_parcel_traj, current_parcel_ecology, trajectory_type, time_steps, current_yr)
#     }
#     gains_degs[, parcel_count_ind, eco_ind] = current_sum
#     
#   }
#   
#   return(gains_degs)
# }
# 
# 


# group_gains_degs(current_collated_reals = collated_dev_realisations, time_steps = global_params$time_steps, eco_dims = global_params$eco_dims, cfac_type = global_params$cfac_type_in_dev_calc)                                              

# current_collated_reals = collated_offset_realisations
# time_steps = global_params$time_steps
# eco_dims = global_params$eco_dims
# 
# cfac_type = global_params$cfac_type_in_offset_calc
# adjust_cfacs_flag =  global_params$adjust_offset_cfacs_flag
                 

group_gains_degs <- function(current_collated_reals, time_steps, eco_dims, cfac_type, adjust_cfacs_flag){
  
  summed_realisations = list()
  summed_realisations$rest_gains = sum_collated_realisations(current_collated_reals$rest_gains, time_steps, eco_dims)
  summed_realisations$avoided_degs = sum_collated_realisations(current_collated_reals$avoided_degs, time_steps, eco_dims)
  if (adjust_cfacs_flag == TRUE){
    summed_realisations$avoided_degs_adjusted = sum_collated_realisations(current_collated_reals$avoided_degs_adjusted, time_steps, eco_dims)
  }
  summed_realisations$net_outcome <- collate_net_realisation_outcome(summed_realisations, adjust_cfacs_flag)
  return(summed_realisations)
  
}




#collated_list = current_collated_reals$rest_gains
sum_collated_realisations <- function(collated_list, time_steps, eco_dims){
  realisation_num = length(realisations)
  #summed_collated_realisations = list_of_arrays(list_dims = eco_dims, array_dims = c(time_steps, realisation_num))
  summed_collated_realisations <- generate_nested_list(outer_dim = realisation_num, inner_dim = eco_dims)
  for (realisation_ind in seq_len(realisation_num)){
    for (eco_ind in seq(eco_dims)){
      summed_collated_realisations[[realisation_ind]][[eco_ind]] = sum_cols_multi(collated_list[[realisation_ind]][[eco_ind]])
    }
  }
  return(summed_collated_realisations)
}





collate_net_realisation_outcome <- function(summed_realisations, adjust_cfacs_flag, eco_dims){
  net_outcome = list()
  net_outcome$standard = sum_nested_lists(summed_realisations$rest_gains, summed_realisations$avoided_degs, inner_dim = eco_dims)
  if (adjust_cfacs_flag == TRUE){
    net_outcome$adjusted = sum_nested_lists(summed_realisations$rest_gains, summed_realisations$avoided_degs_adjusted, inner_dim = eco_dims)
  }
  return(net_outcome)
}








sum_rel_intial <- function(current_parcel_traj, current_parcel_ecology, trajectory_type, time_steps, yr){
  rel_arr = array(0, c(dim(current_parcel_ecology), time_steps))
  rel_arr[, , yr:time_steps] = current_parcel_traj - as.vector(current_parcel_ecology)
  
  if (trajectory_type == 'cfac'){
    rel_arr = -rel_arr
  } 
  rel_arr = apply(rel_arr, 2, sum)
  return(rel_arr)
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



# assess_failed_NNL <- function(NNL_yrs){
#   NNLs = unlist(NNL_yrs)
#   failed_inds = which(NNLs == 0)
#   if (length(failed_inds) > 0){
#     NNL_failed = length(failed_inds)/length(NNLs)
#     NNLs = NNLs[-failed_inds]
#   } else {NNL_failed = 0}
#   
#   NNL_success = 1 - NNL_failed
#   NNL_plot_object = list()
#   NNL_plot_object$failed_inds = failed_inds
#   NNL_plot_object$NNL_failed = NNL_failed
#   NNL_plot_object$NNL_success = NNL_success
#   NNL_plot_object$NNLs = NNLs
#   NNL_plot_object$NNL_yrs = NNL_yrs
#   return(NNL_plot_object)
# }



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



#sum_program_parcels(trajectories, eco_dims, model_outputs$offsets$parcel_indexes, time_steps, parcel_set_num = model_outputs$offsets$parcel_set_count)


#sum_program_parcels(trajectories, eco_dims, parcel_indexes, time_steps, parcel_set_num)

sum_program_parcels <- function(trajectories, eco_dims, parcel_indexes, time_steps, parcel_set_num){
  
  parcel_set_trajs = sum_trajectories(trajectories[parcel_indexes], eco_dims) 
  parcel_set_trajs = combine_to_parcel_set(parcel_set_trajs, parcel_indexes, parcel_set_num, time_steps, eco_dims)
  summed_set_trajs = lapply(seq(eco_dims), function(i) apply(parcel_set_trajs[[i]], MARGIN = 1, 'sum'))
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


