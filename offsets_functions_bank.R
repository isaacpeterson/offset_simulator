
run_offsets_model <- function(global_params, region_params, initial_ecology, decline_rates_initial, parcels){ # run the model and return outputs
  
  if (global_params$set_seed == TRUE){
    set.seed(123)
  }
  
  trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
  offsets_object <- initialise_parcel_set_object(global_params$total_dev_num)   #initialise offsets object to store all offsets
  developments_object <- initialise_parcel_set_object(global_params$total_dev_num) #initialise developments object to store all offsets
  banked_offsets_object <- initialise_banked_offsets_object(global_params)    #initialise banked object to store offsets when using banking
  
  model_outputs <- run_system(trajectories, offsets_object, developments_object, banked_offsets_object, global_params, region_params,    # run the model, select and record parcel sets, calculate landscape condition time series
                              current_ecology = initial_ecology,  decline_rates = decline_rates_initial, parcels, index_object)  
  
  model_outputs$traj_list = build_traj_list(model_outputs$trajectories, land_parcels, parcel_indexes = 1:length(land_parcels), global_params$eco_dims)  #take model outputs in 3D array form and convert to list form separating into land-parcels
  model_outputs$index_object = model_outputs$index_object               #output index object - this is used to track what indexes are available, and have been used in offsets/developments
  model_outputs$offset_success_flag = length(unlist(model_outputs$offsets)) > 0         #used to flag whether the model actually found any offset/development matches
  #   model_outputs$offsets$traj_list = extract_parcel_set_trajs(model_outputs$traj_list, parcel_set_indexes = unlist(model_outputs$offsets$parcel_indexes))
  #   model_outputs$developments$traj_list = extract_parcel_set_trajs(model_outputs$traj_list, parcel_set_indexes = unlist(model_outputs$developments$parcel_indexes))
  
  return(model_outputs)
  
}  



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
  
  if (global_params$match_type == 'parcel_set'){  
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
  

# assess_parcel_set_NNL_yrs(collated_offset_realisations, collated_dev_realisations, parcel_set_nums = collated_offset_realisations$parcel_set_nums,
#                           time_steps = global_params$time_steps, realisation_num,  eco_dims = global_params$eco_dims, offset_yrs)

# parcel_set_nums = collated_offset_realisations$parcel_set_nums
# time_steps = global_params$time_steps
# eco_dims = global_params$eco_dims
# offset_yrs = collated_offset_realisations$offset_yrs

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



# net_gains = net_realisation_gains$standard
# assess_NNL_type = 'system'
# eco_dims = global_params$eco_dims
# assess_num = realisation_num


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



# 
# 
# assess_NNL <- function(net_gains, assess_NNL_type, eco_dims, assess_num, time_horizon, offset_yrs){
#   
#   collate_array = array(0, c(assess_num, eco_dims))
#   NNL_yrs = collate_array
#   failed_parcel_sets = collate_array
#   thresh = 0
#   for (parcel_set_ind in seq_len(assess_num)){
#     if (assess_NNL_type == 'parcel_set'){
#       offset_yr = offset_yrs[parcel_set_ind]
#     }
#     for (eco_ind in 1:eco_dims){
#       current_net_gain = net_gains[, parcel_set_ind, eco_ind]
#       current_NNL_yr = which(current_net_gain > thresh)
#       if (length(current_NNL_yr) > 0){
#         NNL_yrs[parcel_set_ind, eco_ind] = min(current_NNL_yr) 
#         if (assess_NNL_type == 'parcel_set'){
#           NNL_yrs[parcel_set_ind, eco_ind] = NNL_yrs[parcel_set_ind, eco_ind] - offset_yr
#         }
#       } 
#     }
#   }
#   
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
  


# parcel_set_type = 'offsets'
# realisation_ind = 1

collate_realisations_parcel_sets <- function(realisations, parcel_set_type, global_params, decline_rates_initial, land_parcels, initial_ecology){
  
  realisation_num = length(realisations)
  collated_realisation_parcel_sets = initialise_collated_realisations_object(realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]

    if (parcel_set_type == 'offsets'){
      current_model_outputs = model_outputs$offsets
      cfac_type = global_params$cfac_type_in_offset_calc
      parcel_set_indexes = unlist(model_outputs$offsets$parcel_indexes)
    } else {
      current_model_outputs = model_outputs$developments
      parcel_set_indexes = unlist(model_outputs$developments$parcel_indexes)
      cfac_type = global_params$cfac_type_in_dev_calc
    }
    
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


#   
# collate_program_sum_realisations(traj_type = 'trajectory', realisations, cfacs, time_horizon = global_params$time_steps, eco_dims = global_params$eco_dims) 
# 
# traj_type = 'trajectory'
# time_horizon = global_params$time_steps
# eco_dims = global_params$eco_dims

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



# offset_parcel_indexes = model_outputs$offsets$parcel_indexes
# dev_parcel_indexes = model_outputs$developments$parcel_indexes


# sum_program_parcels(traj_list, eco_dims, model_outputs$offsets$parcel_indexes, model_outputs$developments$parcel_indexes, time_steps)


sum_program_parcels <- function(traj_list, eco_dims, parcel_indexes, time_steps, parcel_set_num){
  
  parcel_set_trajs = sum_parcel_trajectories(traj_list, eco_dims, unlist(parcel_indexes), time_steps) 
  parcel_set_trajs = collate_individual_parcel_array(parcel_set_trajs, parcel_indexes, parcel_set_num, time_steps, eco_dims)
  summed_set_trajs = sum_cols_multi(parcel_set_trajs)
  return(summed_set_trajs)
}


sum_parcel_trajectories <- function(traj_list, eco_dims, parcel_indexes, time_steps){
  
  parcel_num = length(parcel_indexes)
  parcel_trajs = array(0, c(time_steps, parcel_num, eco_dims))
  for (parcel_count_ind in seq_len(parcel_num)){
    parcel_ind = parcel_indexes[parcel_count_ind]
    for (eco_ind in seq_len(eco_dims)){
      parcel_trajs[, parcel_count_ind, ] = apply(traj_list[[parcel_ind]][[eco_ind]], MARGIN=3, sum)
    }  
  }
  
  return(parcel_trajs)
  
}


sum_trajectories_as_list <- function(traj_list, eco_dims){
  
  parcel_num = length(traj_list)
  parcel_traj_list = vector('list', parcel_num)
  for (parcel_count_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      parcel_traj_list[[parcel_count_ind]][[eco_ind]] = apply(traj_list[[parcel_count_ind]][[eco_ind]], MARGIN=3, sum)
    }  
  }
  
  return(parcel_traj_list)
  
}




collate_program_cfacs <- function(parcel_cfac_trajs, offset_parcel_indexes, dev_parcel_indexes){
  program_cfacs = list()
  program_cfacs$offsets = sum_program_cfacs(parcel_cfac_trajs, offset_parcel_indexes)
  program_cfacs$devs =  sum_program_cfacs(parcel_cfac_trajs, dev_parcel_indexes)
  program_cfacs$net = sum_program_cfacs(parcel_cfac_trajs, c(offset_parcel_indexes, dev_parcel_indexes))
  return(program_cfacs)
}



# collate_parcel_sets(current_model_outputs, current_parcel_sets_cfacs, land_parcels, time_horizon, eco_dims, decline_rates_initial, parcel_set_num)


# rest_gains <- collate_individual_parcel_array(rest_gains, current_sets_object$parcel_indexes, parcel_set_num, time_steps, eco_dims)
# 
# array_to_collate = rest_gains
# parcel_indexes = current_sets_object$parcel_indexes

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




# current_sets_object = current_model_outputs
# current_traj_list = current_model_outputs$traj_list
# parcel_sets_traj_list = current_traj_list
# cfac_sets_object = current_cfacs
# time_steps = global_params$time_steps


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



#avoided_degs = find_gains_degs(current_sets_object, current_traj_list = current_cfacs$cfacs, trajectory_type = 'cfac', parcel_num, eco_dims, time_steps)


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


# 
# find_gains_degs <- function(current_sets_object, current_traj_list, trajectory_type, parcel_set_num, time_horizon, eco_dims){
#   
#   gains_degs = array(0, c(time_horizon, parcel_set_num, eco_dims))
#   offset_yrs = current_sets_object$offset_yrs
#   
#   if (class(offset_yrs) == 'list'){
#     offset_yrs = unlist(offset_yrs)
#   }
#   
#   for (parcel_set_ind in seq_len(parcel_set_num)){
#     
#     parcel_ecologies = current_sets_object$parcel_ecologies[[parcel_set_ind]]
#     parcel_indexes = unlist(current_sets_object$parcel_indexes[[parcel_set_ind]])
#     
#     parcel_num = length(parcel_indexes)
#     yr = offset_yrs[parcel_set_ind]
#     current_set_element = array(0, c(time_horizon, eco_dims))
#     
#     for (parcel_count_ind in seq_len(parcel_num)){
#       
#       for (eco_ind in seq_len(global_params$eco_dims)){
#         
#         current_parcel_ecology = parcel_ecologies[[parcel_count_ind]][, , eco_ind]
#         
#         if (trajectory_type == 'traj'){
#           current_parcel_traj = current_traj_list[[parcel_set_ind]][[parcel_count_ind]][[eco_ind]]
#         }
#         
#         current_sum = sum_rel_intial(current_parcel_traj, current_parcel_ecology, trajectory_type, time_horizon, yr)
#         current_set_element[, eco_ind] = current_set_element[, eco_ind] + current_sum
#       }
#     }
#     
#     gains_degs[, parcel_set_ind, eco_ind] = current_set_element
#   }  
#   
#   return(gains_degs)
# }




sum_rel_intial <- function(current_parcel_traj, current_parcel_ecology, trajectory_type, time_steps, yr){
  rel_arr = array(0, c(dim(current_parcel_ecology), time_steps))
  rel_arr[, , yr:time_steps] = current_parcel_traj - as.vector(current_parcel_ecology)
  
  if (trajectory_type == 'cfac'){
    rel_arr = -rel_arr
  } 
  rel_arr = apply(rel_arr, 3, sum)
  return(rel_arr)
}



#   collated_realisation_parcel_set_sums <- collate_realisation_parcel_set_sums(realisations, rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon, eco_dims)
#   
#   outs = list()
#   summed_dev_realisations = list()
#   
#   realisation_num = length(realisations)
#   collate_array = array(0, c(time_horizon, realisation_num, eco_dims))
#   summed_dev_realisations$net_gains = collate_array
#   summed_dev_realisations$avoided_degs = collate_array
#   summed_dev_realisations$rest_gains = collate_array
#   net_realisations = collate_array
#   net_parcel_set_realisations = collate_array
#   
#   summed_offset_realisations = summed_dev_realisations
#   
#   summed_program_realisations = list()
#   summed_program_realisations$net_program_value = collate_array
#   summed_program_realisations$net_offset_value = collate_array
#   summed_program_realisations$net_development_value = collate_array
#   
#   program_cfac_realisations = collate_array
#   
#   rm(collate_array)
#   
#   
#   
#   
#   system_NNLs = array(0, c(realisation_num, eco_dims))
#   collate_array = array(0, c(length(parcel_set_indexes), realisation_num))
#   ALL_parcel_set_NNLs = collate_array
#   ALL_offset_parcel_sums_at_offset = collate_array
#   rm(collate_array)
#   
#   
#   for (realisation_ind in seq_len(realisation_num)){
#     current_realisation = realisations[[realisation_ind]]
#     collated_object = current_realisation$collated_parcel_sets_object
#     current_summed_devs = sum_parcel_sets_as_list(rest_gains, avoided_degs, net_dev_gains, parcel_set_indexes, time_horizon)
#     summed_realisations = write_realisation_sums(summed_realisations, realisation_ind, current_summed_devs)
#     
#   }
#   
#   system_NNL_fails = which(system_NNLs == 0)
#   
#   outs$collated_net_cfacs = collated_net_cfacs
#   outs$system_NNL_fails = system_NNL_fails
#   outs$ALL_offset_parcel_sums_at_offset = ALL_offset_parcel_sums_at_offset
#   outs$ALL_parcel_set_NNLs = ALL_parcel_set_NNLs
#   outs$system_NNLs = system_NNLs
#   outs$summed_dev_realisations = summed_dev_realisations
#   outs$summed_offset_realisations = summed_offset_realisations
#   outs$net_parcel_set_realisations = net_parcel_set_realisations
#   outs$net_realisations = net_realisations
#   outs$summed_program_realisations = summed_program_realisations
#  return(outs)
#  
# }











# 
# 
# 
# collate_parcel_sets_object <- function(collated_realisations, realisation_ind, model_outputs, parcel_sets_cfacs, land_parcels, global_params, decline_rates_initial){
#   
#   parcel_set_num = global_params$total_dev_num
#   time_horizon = global_params$time_steps
#   eco_dims = global_params$eco_dims
#   
#   traj_list = model_outputs$traj_list
#   offset_traj_list = model_outputs$offset_traj_list
#   dev_traj_list = model_outputs$dev_traj_list
#   offset_yrs = model_outputs$offsets$offset_yrs
#   
#   collated_parcel_sets_object = list()
# 
#   collated_parcel_sets_object$offsets = collate_parcel_sets(model_outputs$offsets, offset_traj_list, parcel_sets_cfacs$offsets, land_parcels, parcel_set_num, time_horizon, eco_dims, decline_rates_initial)
#   collated_parcel_sets_object$devs = collate_parcel_sets(model_outputs$developments, dev_traj_list, parcel_sets_cfacs$developments, land_parcels, parcel_set_num, time_horizon, eco_dims, decline_rates_initial)
#   
#   collated_parcel_sets_object$NNL_object = assess_NNL(collated_parcel_sets_object$offsets$rest_gains, collated_parcel_sets_object$offsets$avoided_degs, 
#                                                       collated_parcel_sets_object$devs$rest_gains, collated_parcel_sets_object$devs$avoided_degs, eco_dims, parcel_set_num, time_horizon, offset_yrs)
#   collated_parcel_sets_object$summed_program_parcels = sum_program_parcels(traj_list, eco_dims, offset_parcel_indexes = (model_outputs$offset_list), 
#                                                                            dev_parcel_indexes = (model_outputs$development_list), time_horizon)
#   
#   return(collated_parcel_sets_object)
# }
# 








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

plot_NNL_hist <- function(NNL_plot_object, plot_tit, x_lab, x_lim){
  if (length(NNL_plot_object$NNLs) > 0){
    hist(NNL_plot_object$NNLs, main = plot_tit, xlab = NNL_plot_object$x_lab, xlim = x_lim, breaks=seq(min(NNL_plot_object$NNLs),max(NNL_plot_object$NNLs),by=1))
  } else {x_lab = ('All realisations failed NNL')}
  
}






#plot all realisations in restorations gains/avoided degredation form split into offsets and developments

plot_mean_gains_degs <- function(summed_realisations, outer_title, realisation_num){
  
  rest_gains = sum_cols_multi(summed_realisations$rest_gains)/realisation_num
  net_degs = sum_cols_multi(summed_realisations$avoided_degs)/realisation_num
  net_gains = net_degs + rest_gains
  
  if (global_params$adjust_cfacs_flag == TRUE){
    net_degs_adjusted = sum_cols_multi(summed_realisations$avoided_degs_adjusted)/realisation_num
    net_gains_adjusted = net_degs_adjusted + rest_gains
    adjusted_contribution = net_degs_adjusted - net_degs
    
  } else{
    net_gains_adjusted = vector()
    adjusted_contribution = vector()
  }
  
  plot_list = list(net_gains, net_gains_adjusted, net_degs, adjusted_contribution, rest_gains)
  
  plot_array = unlist(plot_list)
  mx = max(plot_array)
  mn = min(plot_array)
  
  col_vec = c('red', 'red', 'red', 'blue', 'black', 'darkgreen')
  lty_vec = c(1, 2, 3, 1, 2, 3)
  
  for (plot_ind in 1:length(plot_list)){
    if (plot_ind == 1){
      plot(plot_list[[plot_ind]], type = 'l', lty = lty_vec[plot_ind], lwd = 2, col = col_vec[plot_ind], ylim = c(mn, mx))
    } else {
      lines(plot_list[[plot_ind]], lty = lty_vec[plot_ind], lwd = 2, col = col_vec[plot_ind], ylim = c(mn, mx))
    }
  }
  
#   legend_vec = c('net outcome', 'net outcome including clearing', 'net outcome including clearing and offsets', 'avoided degredation', 
#                  'clearing contribution', 'clearing and offsets contribution', 'restoration gains')
#   legend('topleft', legend_vec, bty="n", lty = lty_vec, col = col_vec, lwd = 2)
  
}


plot_split_realisations <- function(summed_offset_realisations, offset_cfac_type, summed_dev_realisations, dev_cfac_type, eco_ind, lwd_vec, outer_title, col_vec_1, col_vec_2, realisation_num){
  

  plot_summed_realisations(summed_offset_realisations, cfac_type = offset_cfac_type, plot_title = 'Offset Realisations (Program Scale)', eco_ind, 
                           lwd_vec, col_vec = col_vec_1, legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains'), realisation_num)
  plot_summed_realisations(summed_dev_realisations, cfac_type = dev_cfac_type, plot_title = 'Development Realisations (Program Scale)', eco_ind, 
                           lwd_vec, col_vec = col_vec_2, legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Losses'), realisation_num)
  title(outer_title, outer = TRUE)
}





plot_realisation_hists <- function(system_NNL_plot_object, parcel_set_NNL_yrs, parcel_sums_at_offset_array, parcel_sum_lims, match_type, outer_title){
  
  setup_sub_plots(nx = 1, ny = 3, x_tit = TRUE)
  
  hist(parcel_sums_at_offset_array, main = 'selected offset parcel values', xlab = 'selected offset parcel values', xlim = parcel_sum_lims)
  plot_NNL_hist(system_NNL_plot_object, plot_tit = 'System NNL Assessment', x_lim = c(0, 100))
  if (match_type == 'parcel_set'){
    parcel_set_NNL_plot_object = assess_failed_NNL(parcel_set_NNL_yrs)
    plot_NNL_hist(parcel_set_NNL_plot_object, plot_tit = 'Parcel Set NNL Assessment', x_lim = c(0, 100)) 
  }
  title(outer_title, outer = TRUE)
}

plot_realisation_outcomes <- function(offset_gains, dev_losses, net_outcome, plot_title, x_lab, eco_ind, lwd_vec, col_vec, legend_vec, outer_title){
  
  #setup_sub_plots(nx = 1, ny = 1, x_tit = TRUE)

  plot_lims = c(min(cbind(net_outcome, dev_losses, offset_gains)), max(cbind(net_outcome, dev_losses, offset_gains)))

  plot_collated_realisation_set(offset_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = plot_lims)
  
  plot_collated_realisation_set(dev_losses, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = plot_lims)
  plot_collated_realisation_set(net_outcome, overlay_plots = TRUE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
                                x_lab = x_lab, plot_title = plot_title, plot_lims = plot_lims)
  
  legend('topleft', legend_vec, bty="n", lty = c(2, 2, 2), lwd = array(lwd_vec[1], 3), col = col_vec)
  
  title(outer_title, outer = TRUE)
}


plot_landscape_outcomes <- function(landscape_realisations, system_NNL_yrs, realisation_num, system_NNL_plot_object, net_cfac_sum, eco_dims, eco_ind, lwd_vec, col_vec,  legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  
  landscape_tit = assess_landscape_loss(landscape_realisations, system_NNL_yrs, realisation_num, eco_dims)
  landscape_tit = paste('Landscape Scale Value (', landscape_tit, ')' )
  
  plot_collated_realisation_set(landscape_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = system_NNL_plot_object$x_lab, plot_title = landscape_tit, plot_lims = c(0, max(landscape_realisations)))
  lines(net_cfac_sum, col = col_vec[2], lwd = 2)
  
  abline(h = mean(landscape_realisations[1, , ]), lty = 2)
  
  legend('topright', legend_vec, bty="n", lty = c(2, 1), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  plot_collated_realisation_set( (landscape_realisations - net_cfac_sum), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
                                 x_lab = system_NNL_plot_object$x_lab, plot_title = 'Landscape Value Relative to Counterfactual', plot_lims = vector())
 

  title(outer_title, outer = TRUE) 
}




plot_summed_program_realisations <- function(summed_program_realisations, summed_program_cfacs, system_NNL_yrs, eco_dims, eco_ind, lwd_vec, col_vec, legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  program_scale_tit = assess_landscape_loss(summed_program_cfacs, system_NNL_yrs, realisation_num, eco_dims)
  
  program_scale_tit = paste('Net Program Value (', program_scale_tit, ')')
  plot_collated_realisation_set(summed_program_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = program_scale_tit, plot_lims = c(0, max(summed_program_realisations)))
  plot_collated_realisation_set(summed_program_cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = vector())
  
  legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  
  plot_collated_realisation_set( (summed_program_realisations - summed_program_cfacs), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind = 1, lwd_vec, 
                                 x_lab = '', plot_title = 'Net Value Rel. to Counterfactual', plot_lims = vector() )
  
  title(outer_title, outer = TRUE)
  
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







plot_collated_realisations <- function(collated_realisations, realisation_num, global_params, parcel_sum_lims, eco_ind, lwd_vec, outer_title){
  
  lwd_vec = c(3, 0.2)
  eco_ind = 1
  outer_title = ''
  offset_cfac_type = 'include_clearing'
  summed_offset_realisations = collated_realisations$summed_offset_realisations
  summed_dev_realisations = collated_realisations$summed_dev_realisations
  col_vec_1 = c('blue', 'mediumorchid4', 'darkgreen')
  col_vec_2 = c('blue', 'mediumorchid4', 'red')
  
  time_horizon = global_params$time_steps
  eco_dims = global_params$eco_dims
 
  setup_sub_plots(nx = 1, ny = 3, x_tit = TRUE)
  
  plot_split_realisations(collated_realisations$summed_offset_realisations,  offset_cfac_type = global_params$cfac_type_in_offset_calc,
                          collated_realisations$summed_dev_realisations, dev_cfac_type = global_params$cfac_type_in_dev_calc, eco_ind, 
                          lwd_vec, outer_title, col_vec_1 = c('blue', 'mediumorchid4', 'darkgreen'), col_vec_2 = c('blue', 'mediumorchid4', 'red'), realisation_num)  #plot all realisations in restorations gains/avoided degredation form split into offsets and developments
  
#   setup_sub_plots(nx = 1, ny = 1, x_tit = TRUE)
#   plot_mean_gains_degs(summed_realisations = summed_offset_realisations, outer_title, realisation_num)
  
  system_NNL_yrs = collated_realisations$system_NNLs$NNL_yrs
  system_NNL_plot_object = assess_failed_NNL(system_NNL_yrs)
  
  plot_realisation_outcomes(offset_gains = collated_realisations$summed_offset_realisations$net_outcome$standard, 
                            dev_losses = collated_realisations$summed_dev_realisations$net_outcome$standard, 
                            net_outcome = collated_realisations$net_realisation_gains$standard, 
                            plot_title = 'All Realisation Program Outcomes', x_lab = system_NNL_plot_object$x_lab, eco_ind, lwd_vec, 
                            col_vec = c('darkgreen', 'red', 'black'), legend_vec = c('net offset gains', 'net development losses', 'net program outcomes'), outer_title)
  
  plot_realisation_hists(system_NNL_plot_object, unlist(collated_realisations$parcel_set_NNL_yrs), 
                         unlist(collated_realisations$offsets$parcel_sums_at_offset), parcel_sum_lims = c(0, 20000), match_type = global_params$match_type, outer_title)
  
  summed_program_realisations = simplify_list_to_array(collated_realisations$collated_program_sums$net_trajs)
  summed_program_cfacs = simplify_list_to_array(collated_realisations$collated_program_cfac_sums$net_trajs)
  
  plot_summed_program_realisations(summed_program_realisations, summed_program_cfacs, system_NNL_yrs, eco_dims = global_params$eco_dims, eco_ind, lwd_vec, col_vec = c('red', 'blue', 'black'),
                                   legend_vec = c('Net Value', 'Net Counterfactual Value'), outer_title)
  
  cfac_trajs = simplify2array(unlist(collated_realisations$cfac_trajs, recursive = FALSE))
  net_cfac_sum = apply(cfac_trajs, MARGIN = 1, sum)

  plot_landscape_outcomes(collated_realisations$landscape_vals, system_NNL_yrs, realisation_num, system_NNL_plot_object, net_cfac_sum, eco_dims, eco_ind, lwd_vec, col_vec = c('red', 'blue', 'red'), 
                          legend_vec = c('landscape value', 'counterfactual value'), outer_title)

}



assess_landscape_loss <- function(landscape_vals, NNL_yrs, realisation_num, eco_dims){
  
  landscape_loss = array(0, c(realisation_num, eco_dims))
  
  for (realisation_ind in seq_len(realisation_num)){
    current_NNL_yr = NNL_yrs[realisation_ind]
    if (current_NNL_yr != 0){
      landscape_loss[realisation_ind, ] = landscape_vals[current_NNL_yr, realisation_ind, ]
    }
  }

  success_inds = which(rowSums(landscape_loss) != 0)
  landscape_loss = landscape_loss[success_inds]
  landscape_loss = 100*(1 - landscape_loss/landscape_vals[1, success_inds, ])
  
  if (length(success_inds) > 0){
    landscape_title = paste('NNL at ', round(mean(landscape_loss)), '\U00b1', round(max(range(landscape_loss) - (mean(landscape_loss))), 1), ' % loss of landscape')
  } else landscape_title = 'All realisations failed NNL'
  
  return(landscape_title)
} 



simplify_list_to_array <- function(collated_list){
  collated_array = simplify2array(collated_list)
  collated_dims = dim(collated_array)
  dim(collated_array) = c(collated_dims[1], collated_dims[4], 1)
  return(collated_array)
}




# plot_summed_realisations(collated_summed_reals = summed_offset_realisations, cfac_type = offset_cfac_type, plot_title = 'Offset Realisations (Program Scale)', eco_ind, 
#                          lwd_vec, col_vec = col_vec_1, legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains'), realisation_num)


# collated_summed_reals = summed_offset_realisations
# cfac_type = offset_cfac_type
# plot_title = 'Offset Realisations (Program Scale)'
# legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains')



plot_summed_realisations <- function(collated_summed_reals, cfac_type, plot_title, eco_ind, lwd_vec, col_vec, legend_vec, realisation_num){
  
  rest_gains = collated_summed_reals$rest_gains
  
  if (cfac_type == 'standard'){
    degs = collated_summed_reals$avoided_degs
    nets = collated_summed_reals$net_outcome$standard
  } else if (cfac_type == 'adjusted'){
    degs = collated_summed_reals$avoided_degs_adjusted
    nets = collated_summed_reals$net_outcome$adjusted
  } 

  mx = max(cbind(degs, rest_gains, nets))
  mn = min(cbind(degs, rest_gains, nets))
  
  plot_collated_realisation_set(rest_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))
  plot_collated_realisation_set(degs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))
  plot_collated_realisation_set(nets, overlay_plots = TRUE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))  
  legend('topleft', legend_vec, bty="n", lty = c(2, 2, 2), lwd = array(lwd_vec[1], 3), col = col_vec)
  
}






plot_collated_realisation_set <- function(collated_realisations, overlay_plots, plot_col, realisation_num, eco_ind, lwd_vec, x_lab, plot_title, plot_lims){
  if (plot_col == 'blue'){
    back_plot_col = 'skyblue'
  } else if (plot_col == 'black'){
    back_plot_col = 'gray40'
  } else if (plot_col == 'red'){
    back_plot_col = 'darkorange'
  } else if (plot_col == 'mediumorchid4'){
    back_plot_col = 'mediumorchid1'
  } else if (plot_col == 'darkgreen'){
    back_plot_col = 'green'
  }
  
  if (length(plot_lims) == 0){
    mn = min(collated_realisations[, , eco_ind])
    mx = max(collated_realisations[, , eco_ind])
  } else {
    mn = plot_lims[1]
    mx = plot_lims[2]
  }
  realisation_mean = sum_cols_multi(collated_realisations)/realisation_num
  if (overlay_plots == FALSE){
    plot(collated_realisations[, 1, eco_ind], type = 'l', main = plot_title, xlab = x_lab, ylim = c(mn, mx), col = back_plot_col, lwd = lwd_vec[2])
  } else { lines(collated_realisations[, 1, eco_ind], lwd = lwd_vec[2], col = back_plot_col)
  }
  
  if (realisation_num > 1){
    for (realisation_ind in 2:realisation_num){
      lines(collated_realisations[, realisation_ind, eco_ind], col = back_plot_col, lwd = lwd_vec[2])
    }
  }
  lines(realisation_mean, ylim = c(mn, mx), col = plot_col, lwd = lwd_vec[1], lty = 2)
  abline(h = 0, lty = 2)
  
}



generate_single_realisation_plots <- function(global_params, realisations, net_cfac_sum, eco_ind){
  time_horizon = global_params$time_steps
  eco_dims = global_params$eco_dims
  realisation_ind = sample(length(realisations), 1)
  collated_parcel_sets_object = realisations[[realisation_ind]]$collated_parcel_sets_object
 # plot_sample_parcel_sets(collated_parcel_sets_object, plot_num = 9, global_params)
  
  setup_sub_plots(nx = 1, ny = 3, x_tit = FALSE)
  
  plot_parcel_set_from_collated_object(collated_parcel_sets_object, parcel_set_indexes = (1:global_params$total_dev_num), time_horizon, global_params$eco_dims, 
                                       headings = c('Single Realisation Net Program Developments', 'Single Realisation Net Program Offsets', 'Single Realisation Net Program Outcomes'))
  
  parcel_trajs <- sum_parcel_trajectories(collated_parcel_sets_object$traj_list, eco_dims, parcel_indexes = 1:(parcels$land_parcel_num), time_horizon)
  
  
  par(mfrow = c(2, 1))
  par(mar = c(6, 4, 2, 0), oma = c(0, 0, 0, 0))
  NNL_object = collated_parcel_sets_object$NNL_object
  success_NNL = which(NNL_object$NNL_yrs > 0)
  
  if (length(success_NNL) > 0){
    NNL_success_yrs = NNL_object$NNL_yrs[success_NNL]
    xl = t(cbind(paste('parcel set mean years to NNL = ', round(mean(NNL_success_yrs))), paste('NNL success = ', round(NNL_object$success*100), '%' )))
    hist(NNL_success_yrs, main = 'Single Realisation NNL frequencies', xlab = xl)
  }
  
  hist(collated_parcel_sets_object$offsets$parcel_sums_at_offset, main = 'Single Realisation Selected Parcel Values', xlab = 'Selected offset parcel values')
  setup_sub_plots(nx = 3, ny = 1, x_tit = FALSE)
  total_parcel_sums = apply(parcel_trajs, MARGIN = 1, sum)
  total_counter_sums = apply(parcel_cfac_trajs, MARGIN = 1, sum)
  plot_array = cbind(t(t(total_parcel_sums)), t(t(total_counter_sums)))
  mx = max(plot_array)
  mn = min(plot_array)
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  overlay_plots_as_vector(plot_array, yticks = 'y', axis_lab = TRUE, x_lab = '', ylims = c(mn, mx), (heading = "Single Realisation Landscape Scale Outcome"), ylab = '', col_vec = c('red', 'blue'), 
                          lty_vec = c(1, 1), lwd_vec = c(3, 3), legend_vec = c('Offset Policy Assessment', 'Landscape Decline'), legend_loc = 'topright')
  
  net_cond = total_parcel_sums - total_counter_sums
 
  if (length(NNL_object$system_NNL) > 0 ){
    x_lab = paste('System NNL at', round(NNL_object$system_NNL), 'years')
  } else {x_lab = 'NNL fail'}
  
  plot((net_cond), type = 'l', main = 'Landscape Condition Relative to cfac', xlab = x_lab, ylab = '', col = 'red', lwd = 3)
  abline(h = 0, lty = 2)

}


find_prog_vector <- function(time_steps, prog_start, prog_end, total_prog_num, sd){
  dev_vec = array(0, time_steps)
  dev_vec[prog_start:prog_end] = split_vector((prog_end - prog_start + 1), total_prog_num, sd, min_width = -1)
  return(dev_vec)
}
  
split_vector <- function(N, M, sd, min_width) {

  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  while (any(vec <= min_width)) {
    negs <- vec <= min_width
    pos  <- vec > min_width
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}


initialise_shape_parcels <- function(global_params){
  parcels = list()
  parcel_num_x = global_params$parcel_num_x
  parcel_num_y = global_params$parcel_num_y
  parcel_vx = split_vector(parcel_num_x, global_params$ecology_size, sd = 5, min_width = 3)
  parcel_vy = split_vector(parcel_num_y, global_params$ecology_size, sd = 5, min_width = 3)
  
  pixel_indexes = 1:(global_params$ecology_size*global_params$ecology_size)
  dim(pixel_indexes) = c(global_params$ecology_size, global_params$ecology_size)
  land_parcels = mcell(pixel_indexes, parcel_vx, parcel_vy) #lit the ecology array into a series of subarrays with dimensions sz_x by sz_y
  land_parcel_num = length(land_parcels$elements) #total number of parcels
  parcel_indexes = 1:land_parcel_num #index all parcels
  dim(parcel_indexes) = c(parcel_num_y, parcel_num_x) #arrange indicies into array with dimensions of land parcels
  region_vx = split_vector(global_params$region_num_x, parcel_num_x, 1, min_width = 3) 
  region_vy = split_vector(global_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  regions = mcell(parcel_indexes, region_vx, region_vy)
  
  region_num = length(regions$elements)
  parcels$parcel_indexes = parcel_indexes
  parcels$land_parcel_num = land_parcel_num
  parcels$land_parcels = land_parcels$elements
  parcels$land_parcel_dims = land_parcels$dims
  parcels$regions = regions$elements
  parcels$region_dims = regions$dims
  parcels$region_num = region_num
  parcels$parcel_vx = parcel_vx
  parcels$parcel_vy = parcel_vy
  
  return(parcels)
}


initialise_index_object <- function(parcels, global_params){
  index_object = list()
  index_object$ind_available = parcels$regions
  index_object$developments = vector()
  index_object$offsets = vector()
  index_object$banked_offset_pool = vector()
  index_object$parcel_sets = list()
  index_object$parcel_num_remaining = vector()
  index_object$parcel_set_count = 0
  index_object$break_flag = FALSE
  return(index_object)
}


initialise_ecology_slice <- function(global_params, land_parcels){
  
  land_parcel_num = length(land_parcels)
  initial_ecology = matrix(1,global_params$ecology_size,global_params$ecology_size)
  
  for (parcel_ind in seq_len(land_parcel_num)){
    initial_parcel_value = global_params$min_initial_eco_val + (global_params$max_initial_eco_val - global_params$min_initial_eco_val - global_params$initial_eco_noise)*runif(1) 
    current_parcel = select_land_parcel(land_parcels, parcel_ind)
    initial_ecology[current_parcel] = initial_ecology[current_parcel]*initial_parcel_value
  }
  initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size),global_params$ecology_size,global_params$ecology_size)
  return(initial_ecology)
}




initialise_ecology <- function(global_params, land_parcels){
  initial_ecology = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$eco_dims))
  for (eco_ind in seq_len(global_params$eco_dims)){
    initial_ecology[, , eco_ind]  = initialise_ecology_slice(global_params, land_parcels)
  }
  return(initial_ecology)
}
  



mcell <- function(x, vx, vy){
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  B = vector('list', rows*cols)
  colStart = 0
  for (i in seq_len(cols)){
    rowStart = 0
    for (j in seq_len(rows)){
      B[[a]] = x[rowStart+(1:rowsizes[j]), colStart+(1:colsizes[i])]
      rowStart = rowStart + rowsizes[j]
      a = a + 1
    }
    colStart = colStart + colsizes[i]
  }
  
  parcel = list()
  parcel$dims = c(length(vy), length(vx))
  parcel$elements = B
  return(parcel)
  
}  

ind2sub <- function(rows, ind){
  rw = ((ind-1) %% rows) + 1 
  cl = floor((ind-1) / rows) + 1
  loc = c(rw, cl)
  return(loc)
}


project_ecology <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_horizon, time_fill){
  if (time_fill == 'all'){
    #time_vec = seq_len(time_horizon)
    time_vec = 0:time_horizon
  } else {time_vec = time_horizon}
  t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
  eco_projected = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_vec - t_sh)))
  return(eco_projected)
}





build_cfacs_by_year <- function(global_params, decline_rates, land_parcels, initial_ecology){
  current_ecology = initial_ecology
  cfacs = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
  cfacs[, , 1] = initial_ecology
  parcel_num = length(land_parcels)
  
  for (yr in seq(global_params$time_steps)){
    for (parcel_ind in seq_len(parcel_num)){
      current_parcel = select_land_parcel(land_parcels, parcel_ind)
      current_dec_rate = decline_rates[parcel_ind]      
      updated_parcel = sapply(current_ecology[current_parcel], project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, decline_rate = current_dec_rate, time_horizon = 1, time_fill = 'none')
      current_ecology[current_parcel] = updated_parcel 
    }
    if (global_params$blur == TRUE){
      current_ecology = Blur_2D(current_ecology, 0.5, 0.5)
    } 
    cfacs[, , yr] = current_ecology
  }
  
  return(cfacs)
}


#predict_parcel_traj(current_parcel_ecology, current_parcel_ind, parcel_traj_type = 'protect', global_params, decline_rates, time_horizon = time_horizons[parcel_count_ind])


predict_parcel_traj <- function(current_parcel_ecology, current_parcel_ind, parcel_traj_type, global_params, decline_rates, time_horizon){
  
  eco_dims = global_params$eco_dims
  projected_ecology = vector('list', eco_dims)

  if (time_horizon >= 0){
    for (eco_ind in seq_len(eco_dims)){
      
      current_slice = current_parcel_ecology[, , eco_ind]
      
      if (parcel_traj_type == 'protect'){
        dec_rate = find_decline_rate(decline_rates, current_parcel_ind, eco_ind)
      } else if (parcel_traj_type == 'maintain'){
        dec_rate = 1e-10
      } else if (parcel_traj_type == 'restore'){
        dec_rate = global_params$restoration_rate
      }
      
      current_predicted_ecology = apply(current_slice, MARGIN = c(1, 2), FUN = project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, dec_rate, time_horizon, time_fill = 'all')
      
    if (length(dim(current_predicted_ecology)) > 2){
        current_predicted_ecology = aperm(current_predicted_ecology, c(2, 3, 1))
    }  else {
      dim(current_predicted_ecology) = c(dim(current_predicted_ecology), 1)
    }
        
      
      projected_ecology[[eco_ind]] = current_predicted_ecology
      
    }
  }
  
  return(projected_ecology)
  
}



# current_ecology = initial_ecology
# decline_rates = decline_rates_initial
# land_parcels = parcels$land_parcels
# parcel_indexes = 1:(parcels$land_parcel_num)
# time_horizons = global_params$time_steps


# 
# current_cfacs = build_cfacs_list(global_params, decline_rates, parcel_ecologies = (current_sets_object$parcel_ecologies), 
#                                  parcel_indexes = current_sets_object$parcel_indexes, time_horizons)

ecology_to_parcels <- function(current_ecology, land_parcels){
  parcel_num = length(land_parcels)
  parcel_ecologies <- vector('list', parcel_num)
  for (parcel_ind in 1:parcel_num){
    current_parcel = land_parcels[[parcel_ind]]
    parcel_ecologies[[parcel_ind]] = extract_3D_parcel(current_parcel, current_ecology)
  }
  return(parcel_ecologies)
}


build_cfacs_list <- function(global_params, decline_rates, parcel_ecologies, parcel_indexes, time_horizons){
  
  if (class(parcel_indexes) == 'list'){
    parcel_indexes = unlist(parcel_indexes)
  }
  
  if (class(time_horizons) == 'list'){
    time_horizons = unlist(time_horizons)
  }
  
  parcel_num = length(parcel_indexes)
  cfacs  = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ecology = parcel_ecologies[[parcel_count_ind]]
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    cfacs[[parcel_count_ind]] = predict_parcel_traj(current_parcel_ecology, current_parcel_ind, parcel_traj_type = 'protect', global_params, decline_rates, time_horizon = time_horizons[parcel_count_ind])
  }
  return(cfacs)
}


build_cfacs_by_parcel_multi <- function(global_params, decline_rates, parcel_indexes, land_parcels, current_ecology, time_horizon){
  
  eco_dims = dim(current_ecology)[3]
  parcel_num = length(parcel_indexes)
  cfacs = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    current_cfac = vector('list', eco_dims)
    
    for (eco_ind in seq_len(eco_dims)){
      current_ecology_slice = current_ecology[, , eco_ind]
      current_dec_rate = find_decline_rate(decline_rates, current_parcel_ind, eco_ind)
      current_parcel_ecology = current_ecology_slice[current_parcel]
      dim(current_parcel_ecology) = dim(current_parcel)
      current_cfac[[eco_ind]] = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = 'protect', global_params, current_dec_rate, time_horizon)
    }
    
    cfacs[[parcel_count_ind]] = current_cfac
  }
  
  return(cfacs)
  
}





initialise_cfacs_multi <- function(global_params, region_params, land_parcels, initial_ecology, decline_rates){
  cfacs = vector('list', global_params$eco_dims)
  ecology_size = global_params$ecology_size
  for (eco_ind in seq_len(global_params$eco_dims)){
    cfacs[[eco_ind]] = build_cfacs_by_year(global_params, decline_rates[, , eco_ind] , land_parcels, initial_ecology[, , eco_ind] )
  }
  return(cfacs)
}



# initialise trajectories as a list containing N 3 dimensional arrays where N is the number of ecologiecal dimensions
initialise_trajectories <- function(eco_dims, ecology_size, time_steps, initial_ecology){
  trajectories = vector('list', eco_dims)
  for (eco_ind in seq_len(eco_dims)){
    trajectories[[eco_ind]] = array(0, c(ecology_size, ecology_size, time_steps))
  }
  
  return(trajectories)
}







build_decline_rates_multi <- function(parcels, condition_change, mean_decline_rate, decline_rate_std, eco_dims){
  regions = parcels$regions
  region_num = length(regions)
  if (condition_change == 'decline'){
    mean_decline_rate = -mean_decline_rate
  }
#  region_dec_rates = vector('list', region_num)
  decline_rates = array(0, c(dim(parcels$parcel_indexes), eco_dims))
  for (eco_ind in seq_len(eco_dims)){
    current_decline_rates = array(0, dim(parcels$parcel_indexes))
    for (region_ind in seq_len(region_num)){ 
      current_region = regions[[region_ind]]
      current_parcel_num = length(current_region)    
      decline_params = c(length(current_region), mean_decline_rate, decline_rate_std) #params$decline_rate_std[region_ind])
      region_decline_rates = matrix(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), ncol = ncol(current_region))
      #    region_dec_rates[[region_ind]] = current_decline_rates
      current_decline_rates[current_region] = region_decline_rates
    }
    decline_rates[, , eco_ind]  = current_decline_rates
  }
  
  return(decline_rates)
}



select_rand_index <- function(ind_available, parcel_num){
  parcel_indexes = ind_available[sample(1:length(ind_available), parcel_num)]
  return(parcel_indexes)
}


# matched_parcel_set_object <- match_parcel_set(offset_pool_object, global_params, ind_available = index_object$ind_available[[region_ind]], 
#                                               current_ecology, decline_rates_initial, parcels$land_parcels, yr, time_horizon)
#ind_available = index_object$ind_available[[region_ind]]




match_parcel_set <- function(offset_pool_object, global_params, ind_available, current_ecology, decline_rates_initial, land_parcels, yr, time_horizon, net_development_val){
  
  current_pool_vals = unlist(offset_pool_object$parcel_vals_used)
  current_pool_indexes = offset_pool_object$parcel_indexes
  parcel_num_remaining = length(ind_available)
  current_match_pool = ind_available
  parcel_for_parcel = global_params$offset_parcel_for_parcel
  
  match_flag = FALSE
  
  while( (match_flag == FALSE) & length(current_match_pool > 0) ){
    
    current_test_index = select_rand_index(current_match_pool, parcel_num = 1)
    
    dev_pool_object <- record_current_parcel_set(land_parcels = parcels$land_parcels, current_ecology, current_test_index, parcel_num_remaining, yr)
    
    dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, pool_type = 'devs', calc_type = global_params$dev_calc_type, cfacs_flag = global_params$dev_cfacs_flag, 
                                           adjust_cfacs_flag = global_params$adjust_cfacs_flag, cfac_type = global_params$cfac_type_in_dev_calc, time_horizon_type = 'future',
                                           global_params, region_params, decline_rates_initial, time_horizon, yr)
    
    vals_to_match = unlist(dev_pool_object$parcel_vals_used)
    
    if (global_params$match_type == 'parcel_set'){
      
      if (global_params$use_offset_bank == FALSE){
        test_ind = list_intersect(current_pool_indexes, current_test_index)
        test_ind = test_ind$match_ind
        pool_to_use = current_pool_indexes[-test_ind]
        vals_to_use = current_pool_vals[-test_ind]
      } else {
        pool_to_use = current_pool_indexes
        vals_to_use = current_pool_vals
      }
      match_object <- select_from_pool(current_pool = pool_to_use, vals_to_use = vals_to_use, offset_multiplier = global_params$offset_multiplier, 
                                       vals_to_match = vals_to_match, parcel_for_parcel, global_params$eco_dims, yr)
      
      match_flag = match_object$match_flag
    
    } else if (global_params$match_type == 'banked'){
      
      pooled_offset_val = sum(current_pool_vals) - net_development_val
      match_flag = vals_to_match < pooled_offset_val
      
    }
    
    if (match_flag == FALSE){
      ind_to_reject = list_intersect(current_match_pool, current_test_index)
      current_match_pool = current_match_pool[-ind_to_reject$match_ind] 
    }
    
  }
  
  matched_parcel_set = list()
  matched_parcel_set$match_flag = match_flag
  
  if (match_flag == TRUE){
    matched_parcel_set$development_object = dev_pool_object
    
    if (global_params$match_type == 'parcel_set'){
      matched_parcel_set$offset_object = select_current_subset(offset_pool_object, match_object$match_indexes)
    } else {
      offset_object = list()
      
    }
  } 
  
  return(matched_parcel_set)
  
}


# 
# match_parcel_set <- function(offset_pool_object, global_params, ind_available, current_ecology, decline_rates_initial, land_parcels, yr, time_horizon){
#   
#   current_pool_vals = unlist(offset_pool_object$parcel_vals_used)
#   current_pool_indexes = offset_pool_object$parcel_indexes
#   parcel_num_remaining = length(ind_available)
#   current_match_pool = ind_available
#   parcel_for_parcel = global_params$offset_parcel_for_parcel
#   
#   match_flag = FALSE
#   
#   while( (match_flag == FALSE) & length(current_match_pool > 0) ){
#     
#     current_test_index = select_rand_index(current_match_pool, parcel_num = 1)
# 
#     dev_pool_object <- record_current_parcel_set(land_parcels = parcels$land_parcels, current_ecology, current_test_index, parcel_num_remaining, yr)
#     
#     dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, pool_type = 'devs', calc_type = global_params$dev_calc_type, cfacs_flag = global_params$dev_cfacs_flag, 
#                                            adjust_cfacs_flag = global_params$adjust_cfacs_flag, cfac_type = global_params$cfac_type_in_dev_calc, time_horizon_type = 'future',
#                                               global_params, region_params, decline_rates_initial, time_horizon, yr)
#     
#     vals_to_match = unlist(dev_pool_object$parcel_vals_used)
#     
#     if (global_params$use_offset_bank == FALSE){
#       test_ind = list_intersect(current_pool_indexes, current_test_index)
#       test_ind = test_ind$match_ind
#       pool_to_use = current_pool_indexes[-test_ind]
#       vals_to_use = current_pool_vals[-test_ind]
#     } else {
#       pool_to_use = current_pool_indexes
#       vals_to_use = current_pool_vals
#     }
#     
#     match_object <- select_from_pool(current_pool = pool_to_use, vals_to_use = vals_to_use, offset_multiplier = global_params$offset_multiplier, 
#                                      vals_to_match = vals_to_match, parcel_for_parcel, global_params$eco_dims, yr)
#     
#     match_flag = match_object$match_flag
#     
#     if (match_flag == FALSE){
#       ind_to_reject = list_intersect(current_match_pool, current_test_index)
#       current_match_pool = current_match_pool[-ind_to_reject$match_ind] 
#     }
#     
#   }
#   
#   matched_parcel_set = list()
#   matched_parcel_set$match_flag = match_object$match_flag
#   
#   if (match_flag == TRUE){
#     matched_parcel_set$development_object = dev_pool_object
#     matched_parcel_set$offset_object = select_current_subset(offset_pool_object, match_object$match_indexes)
#   } 
#   
#   return(matched_parcel_set)
#   
# }


# select_dev_index_from_pool <- function(ind_available){
#   dev_pool = cfac_list(calc_type = global_params$dev_calc_type, cfac_type = global_params$cfac_type_in_dev_calc, 
#                               current_pool, decline_rates, global_params, region_params, parcels$land_parcels, current_ecology, time_horizon, yr)
#   dev_object <- select_from_pool(offset_pool_object, global_params$offset_multiplier, development_object$parcel_vals_used, global_params$offset_parcel_for_parcel, global_params$eco_dims, yr)   
#   
#   index_object <- update_ind_available(update_type = 'offset', index_object, offset_object$parcel_indexes, region_ind)              
#   decline_rates <- update_decline_rates(decline_rates, global_params$restoration_rate, decline_rate_type = 'offset', offset_action_type = global_params$offset_action_type, offset_object$parcel_indexes, global_params$offset_dims)
#   
# }


# select_development <- function(banked_offset_pool, global_params, ind_available, offset_multiplier, current_ecology, decline_rates, land_parcels, offset_dims, yr, time_horizon){
#   
#   if (global_params$use_offset_bank == TRUE){
#     current_pool = ind_available
#   } else {
#     current_pool = select_rand_dev_index(ind_available, parcel_num = 1)
#   } 
#   
#   dev_pool = evaluate_parcel_pool(calc_type = global_params$dev_calc_type, cfac_type = global_params$cfac_type_in_dev_calc, current_pool, 
#                               current_parcel_num_remaining = length(ind_available), decline_rates, global_params, 
#                               parcels$land_parcels, current_ecology, time_horizon, yr)
#   
#   if (global_params$use_offset_bank == TRUE){
#     development_object <- select_from_pool(dev_pool, offset_multiplier, vals_to_match = banked_offset_vals_to_use, global_params$offset_parcel_for_parcel, global_params$eco_dims, yr) 
#   } else {
#     development_object <- record_parcel_info((parcel_sums_at_offset = dev_pool$parcel_sums_at_offset), parcel_indexes = current_pool, 
#                                              (parcel_eco_vals = dev_pool$parcel_eco_vals), (parcel_vals_used = dev_pool$parcel_vals_pool), yr)
#   }
#   
#   return(development_object)
#   
# }
  




# predict_type = 'restore' 
# parcel_eco_vals = parcel_sums_object$parcel_eco_vals 
# parcel_indexes = current_pool 
# restoration_rate = global_params$restoration_rate
# min_eco_val = global_params$min_eco_val
# max_eco_val = global_params$max_eco_val


predict_parcel_vals_multi <- function(predict_type, parcel_eco_vals, parcel_indexes, decline_rates, restoration_rate, min_eco_val, max_eco_val, eco_dims, time_horizons){
  
  if (class(time_horizons) == 'list'){
    time_horizons = unlist(time_horizons)
  }
  
  parcel_num = length(parcel_indexes)
  predicted_parcel_vals = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    current_predicted_parcel_vals = array(0, c(1, eco_dims))
    for (eco_ind in seq_len(eco_dims)){
      current_decline_rates = decline_rates[, , eco_ind] 
      
      if (predict_type == 'protect'){
        decline_rate = current_decline_rates[current_parcel_ind]      
      } else if (predict_type == 'restore'){
        decline_rate = restoration_rate
      }
      predicted_parcel = sapply(parcel_eco_vals[[parcel_count_ind]][, , eco_ind], project_ecology, min_eco_val, max_eco_val, decline_rate, time_horizons[parcel_count_ind], time_fill = 'none')
      current_predicted_parcel_vals[1, eco_ind] = sum(predicted_parcel)
    }
    predicted_parcel_vals[[parcel_count_ind]] = current_predicted_parcel_vals
    
  }
  return(predicted_parcel_vals)
  
}


record_parcel_info <- function(parcel_sums_at_offset, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr, parcel_num_remaining){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = yr
  parcel_set_object$parcel_ecologies = parcel_eco_vals
  parcel_set_object$parcel_sums_at_offset = parcel_sums_at_offset
  parcel_set_object$parcel_indexes = parcel_indexes
  parcel_set_object$parcel_vals_used = parcel_vals_used
  parcel_set_object$parcel_num_remaining = parcel_num_remaining
#   if (record_type == 'offset'){
#     parcel_set_object$adjusted_cfac_trajs = adjusted_counter_trajs
#   }
  return(parcel_set_object)
  
}



evaluate_parcel_vals <- function(calc_type, parcel_sums_at_offset, restoration_vals, cfac_vals){

  if (calc_type == 'current_condition'){
    parcel_vals_pool = parcel_sums_at_offset
  } else if (calc_type == 'restoration_gains'){
    parcel_vals_pool = list_subtract(restoration_vals, parcel_sums_at_offset)
  } else if (calc_type == 'restoration_from_cfac'){
    parcel_vals_pool = list_subtract(restoration_vals, cfac_vals)
  } else if (calc_type == 'restoration_condition_value'){
    parcel_vals_pool = restoration_vals
  } else if (calc_type == 'avoided_degredation'){
    parcel_vals_pool = list_subtract(parcel_sums_at_offset, cfac_vals)
  } else if (calc_type == 'future_condition'){
    parcel_vals_pool = cfac_vals 
  } else{
    parcel_vals_pool = list()
  }
  return(parcel_vals_pool)
}


list_subtract <- function(list_a, list_b){
  list_c = mapply('-', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}

list_add <- function(list_a, list_b){
  list_c = mapply('+', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}

# 
# calc_type = global_params$offset_calc_type
# cfac_type = global_params$cfac_type_in_offset_calc
# current_pool = index_object$ind_available[[region_ind]]
# parcel_num_remaining = index_object$parcel_num_remaining
# decline_rates = decline_rates_initial



# evaluate_parcel_pool(calc_type = global_params$offset_calc_type, cfac_type = global_params$cfac_type_in_offset_calc, 
#                      current_pool = index_object$ind_available[[region_ind]], parcel_num_remaining = index_object$parcel_num_remaining, 
#                      decline_rates = decline_rates_initial, global_params, land_parcels, current_ecology, time_horizon, yr)


record_current_parcel_set <- function(land_parcels, current_ecology, current_pool, parcel_num_remaining, yr){
  
  parcel_sums_object = find_current_parcel_sums(land_parcels, current_ecology, current_pool, global_params$eco_dims)
  parcel_set_object = list()
  parcel_set_object$offset_yrs = rep(list(yr), length(current_pool))
  
  parcel_set_object$parcel_ecologies = parcel_sums_object$parcel_ecologies
  parcel_set_object$parcel_sums_at_offset = parcel_sums_object$parcel_sums
  parcel_set_object$parcel_indexes = current_pool
  parcel_set_object$parcel_num_remaining = rep(list(parcel_num_remaining), length(current_pool))
  
  return(parcel_set_object)
  
}



# offset_pool_object <- evaluate_parcel_pool(calc_type = global_params$offset_calc_type, cfac_type = global_params$cfac_type_in_offset_calc, 
#                                            current_pool = index_object$ind_available[[region_ind]], parcel_num_remaining = index_object$parcel_num_remaining, 
#                                            decline_rates = decline_rates_initial, global_params, land_parcels, current_ecology, time_horizons, yr)



# calc_type = global_params$offset_calc_type
# cfac_type = global_params$cfac_type_in_offset_calc
# current_pool = index_object$ind_available[[region_ind]]
# parcel_num_remaining = index_object$parcel_num_remaining
# decline_rates = decline_rates_initial

evaluate_parcel_pool <- function(calc_type, cfac_type, current_pool, parcel_num_remaining, decline_rates, global_params, 
                                 land_parcels, current_ecology, time_horizons, yr){
  
  parcel_num = length(current_pool)
  parcel_pool_object <- record_current_parcel_set(land_parcels, current_ecology, current_pool, parcel_num_remaining, yr)
  
  restoration_flag = ( (calc_type == 'restoration_gains') || (calc_type == 'restoration_from_cfac') || (calc_type == 'restoration_condition_value'))
  if (restoration_flag == TRUE){
    restoration_vals = predict_parcel_vals_multi(predict_type = 'restore', parcel_pool_object$parcel_ecologies, parcel_indexes = current_pool, decline_rates, global_params$restoration_rate, 
                                                 global_params$min_eco_val, global_params$max_eco_val, eco_dims = global_params$eco_dims, time_horizons)
  } else{
    restoration_vals = list()
  }
  
  if ((calc_type == 'avoided_degredation') || (calc_type == 'restoration_from_cfac') || (calc_type == 'future_condition') ){
    cfac_flag = TRUE
  } else {cfac_flag = FALSE
  }
  
  parcel_pool$cfac_flag = cfac_flag
  
  if (cfac_flag == TRUE){
    cfac_trajs = list()
    cfacs_standard = build_cfacs_list(global_params, decline_rates_initial, parcel_ecologies = parcel_pool_object$parcel_ecologies, 
                                      parcel_indexes = current_pool, time_horizons)
    cfac_trajs$standard = sum_trajectories_as_list(cfacs_standard, eco_dims = global_params$eco_dims)
    
    if ( global_params$adjust_cfacs_flag == TRUE){
      cfacs_adjusted = adjust_cfacs(current_cfacs = cfacs_standard, current_parcel_ecologies = parcel_pool_object$parcel_ecologies, adjusted_cfac_type = cfac_type, 
                                    global_params, parcel_num_remaining = parcel_pool_object$parcel_num_remaining, decline_rates, 
                                    parcel_indexes = current_pool, time_horizons = time_horizons, offset_yrs = parcel_pool_object$offset_yrs)
      
      cfac_trajs$adjusted = sum_trajectories_as_list(cfacs_adjusted, eco_dims = global_params$eco_dims)
      cfac_vals = lapply(cfac_trajs$adjusted, tail, 1) 
      
    } else {
      cfac_vals = last_element_in_list(cfac_trajs$standard) 
    }
    
    parcel_pool$cfac_trajs = cfac_trajs
    
  } else {
    cfac_vals = list()
  }
  
  parcel_vals_pool <- evaluate_parcel_vals(calc_type, parcel_pool_object$parcel_sums_at_offset, restoration_vals, cfac_vals)
  return(parcel_pool)
  
}                 


last_element_in_list <- function(list_a){
  last_elements = lapply(list_a, tail, 1)
  return(last_elements)
}
# select_offset_index <- function(offset_pool_object, offset_multiplier, development_vals_used){
#   outs = list()
#   parcel_vals_pool = offset_pool_object$parcel_vals_pool
#   current_offset_pool = offset_pool_object$current_parcel_pool
#   dev_vals = offset_multiplier*matrix(rep(development_vals_used, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
#   err = sqrt(rowSums( (parcel_vals_pool - dev_vals)^2 ) )
#   best_ind = which(err == min(err))
#   parcel_indexes = current_offset_pool[best_ind]
#   parcel_vals_used = parcel_vals_pool[best_ind, ]
#   
#   
#   parcel_num = length(parcel_indexes)
#   outs$parcel_eco_vals = vector('list', parcel_num)
#   outs$parcel_sums = vector('list', parcel_num)
#   outs$parcel_indexes = array(0, parcel_num)
#   outs$parcel_vals_used = vector('list', parcel_num)
#   
#   for (parcel_ind in seq_len(parcel_num)){
#     outs$parcel_eco_vals[[parcel_ind]] = offset_pool_object$parcel_eco_vals[[best_ind]]
#     outs$parcel_sums[[parcel_ind]] = offset_pool_object$parcel_sums_at_offset[best_ind, ]
#     outs$parcel_vals_used[parcel_ind, ] = parcel_vals_used
#     outs$parcel_indexes[parcel_ind] = parcel_indexes
#   }
#  
#   return(outs)
# }


rowProds <- function(X){ t(t(apply(X,1,FUN="prod"))) }

test_cond <- function(vals_to_match, parcel_vals_pool, development_vals_used, match_array){
  thresh_array = matrix(rep(0.10*vals_to_match, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
  cond = (parcel_vals_pool - match_array) < thresh_array
  cond = rowProds(cond)
  return(cond)
}












adjust_cfac <- function(cfacs, region_params, global_params, parcel_indexes, parcel_num_remaining, time_horizon, yr){
  
  parcel_num = length(parcel_indexes)
  eco_dims = global_params$eco_dims
  rec_list = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    rec_list[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  weighted_offset_projections = rec_list
  include_clearing = rec_list
  include_clearing_offsets = rec_list
  
  rm(rec_list)
  
  dev_prob <- find_dev_probability(global_params$dev_vec, yr, time_horizon, parcel_num_remaining)
  
  if (global_params$cfac_type_in_offset_calc == 'include_clearing_offsets'){
    offset_prob = dev_prob
  } else {offset_prob = 0}
  
  counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      current_cfac = cfacs[[parcel_count_ind]][[eco_ind]]
      projected_dims = dim(current_cfac)

      counter_prob_array = rep(counter_probs, projected_dims[1]*projected_dims[2])
      dim(counter_prob_array) = c(length(counter_probs), c(projected_dims[2], projected_dims[1]))
      counter_prob_array = aperm(counter_prob_array, c(3, 2, 1))
      weighted_counters = counter_prob_array*current_cfac
      include_clearing[[parcel_count_ind]][[eco_ind]] = weighted_counters
      
      if (global_params$cfac_type_in_offset_calc == 'include_clearing_offsets'){
        current_weighted_offset_projections = find_summed_offset_projections(projected_dims, current_cfac, offset_prob, global_params, time_horizon)
        weighted_offset_projections[[parcel_count_ind]][[eco_ind]] = current_weighted_offset_projections
        include_clearing_offsets[[parcel_count_ind]][[eco_ind]] = weighted_counters + current_weighted_offset_projections
      } 
    } 
  }
  
  adjusted_counters_object = list()
  adjusted_counters_object$include_clearing = include_clearing
  adjusted_counters_object$include_clearing_offsets = include_clearing_offsets
  
  return(adjusted_counters_object)
  
}



#dev_probs <- find_dev_probability(global_dev_vec = global_params$dev_vec, offset_yrs, time_horizons, parcel_num = length(parcel_indexes), parcel_num_remaining)

find_dev_probability <- function(global_dev_vec, offset_yrs, time_horizons, parcel_num, parcel_num_remaining){
  
  dev_probs = list()
  parcel_num_remaining = unlist(parcel_num_remaining)
  offset_yrs = unlist(offset_yrs)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_count_ind]
    offset_yr = offset_yrs[parcel_count_ind]
    dev_vec = global_dev_vec[offset_yr:length(global_dev_vec)]
    
    if (length(dev_vec) < (time_horizon + 1)){
      dev_vec = c(dev_vec, array(0, ((time_horizon + 1) - length(dev_vec))))
    }
    
    dev_vec = dev_vec[1:(time_horizon + 1)]
    dev_probs[[parcel_count_ind]] = dev_vec/parcel_num_remaining[parcel_count_ind]

  }
  return(dev_probs)
}





# development_vals_used = development_object$parcel_vals_used
# offset_multiplier = global_params$offset_multiplier
# offset_parcel_for_parcel = region_params[[region_ind]]$offset_parcel_for_parcel
# eco_dims = global_params$eco_dims


euclidean_norm_match <- function(parcel_vals_pool, vals_to_match){
  parcel_vals_pool = t(t(parcel_vals_pool))
  match_array = matrix(rep(vals_to_match, length(parcel_vals_pool)), ncol = length(vals_to_match), byrow = TRUE)
  
  err = sqrt(rowSums( (parcel_vals_pool - match_array)^2 ) )
  match_ind = which(err == min(err))
  match_vals = parcel_vals_pool[match_ind, ]
  
  match_object = list()
  match_object$match_vals = match_vals
  match_object$match_ind = match_ind
  return(match_object)
}
  





false_match <- function(){
  match_object = list()
  match_object$match_flag = FALSE
  return(match_object)
}



# select_from_pool(current_pool = pool_to_use, vals_to_use = vals_to_use, offset_multiplier = global_params$offset_multiplier, 
#                  vals_to_match = vals_to_match, parcel_for_parcel, eco_dims = global_params$eco_dims, yr)

# current_pool = pool_to_use
# vals_to_use = vals_to_use
# offset_multiplier = global_params$offset_multiplier
# eco_dims = global_params$eco_dims
# offset_parcel_for_parcel = global_params$offset_parcel_for_parcel

select_from_pool <- function(current_pool, vals_to_use, offset_multiplier, vals_to_match, offset_parcel_for_parcel, eco_dims, yr){
  
  if (class(current_pool) == 'list'){
    current_pool = unlist(current_pool)
  }
  
  if (class(vals_to_use) == 'list'){
    vals_to_use = unlist(vals_to_use)
  } 
  
  thresh = array(50, length(vals_to_match))
  
  vals_to_match = offset_multiplier*vals_to_match
  
  if (offset_parcel_for_parcel == TRUE){
    match_array = matrix(rep(vals_to_match, length(vals_to_use), ncol = length(vals_to_match), byrow = TRUE))
    thresh = matrix(rep(thresh, length(vals_to_use), ncol = length(thresh), byrow = TRUE))
    inds_to_use = (match_array - vals_to_use) < thresh
    vals_to_use = vals_to_use[inds_to_use]
    current_pool = current_pool[inds_to_use]
    
    if (all(inds_to_use == FALSE)){
      match_object <- false_match()
      return(match_object)
    } 
  } else if(offset_parcel_for_parcel == FALSE){
    if (vals_to_match - sum(vals_to_use) > thresh){ 
      match_object <- false_match()
      return(match_object)
    } 
    match_vals = list()
    match_indexes = list()
  } 
  
  parcel_vals_pool = vals_to_use
  
  match_flag = FALSE
  
  while(match_flag == FALSE){
    if (length(current_pool) == 0){
      break
    }
    euclidean_match = euclidean_norm_match(parcel_vals_pool, vals_to_match)
    current_match_val = euclidean_match$match_vals
    current_match_index = current_pool[euclidean_match$match_ind]
    vals_to_match = vals_to_match - current_match_val
    
    if (offset_parcel_for_parcel == FALSE){
      ind_to_remove = list_intersect(current_pool, current_match_index)
      current_pool = current_pool[-ind_to_remove$match_ind]
      parcel_vals_pool = parcel_vals_pool[-ind_to_remove$match_ind]
      match_vals = append(match_vals, current_match_val)
      match_indexes = append(match_indexes, current_match_index)
    }
    match_flag = all(vals_to_match < thresh)
  }
  
  match_object = list()
  match_object$match_indexes = match_indexes
  match_object$match_vals = match_vals
  match_object$match_flag = match_flag
  
  return(match_object)
}






# select_from_pool <- function(current_pool, vals_to_use, offset_multiplier, vals_to_match, offset_parcel_for_parcel, eco_dims, yr){
#   
#   if (class(current_pool) == 'list'){
#     current_pool = simplify2array(current_pool)
#   }
#   
#   thresh = array(50, length(vals_to_match))
#   
#   if (offset_parcel_for_parcel == TRUE){
#     match_array = matrix(rep(vals_to_match, length(vals_to_use), ncol = length(vals_to_match), byrow = TRUE))
#     thresh_array = matrix(rep(thresh, length(vals_to_use), ncol = length(thresh), byrow = TRUE))
#     inds_to_use = (vals_to_use - match_array) > -thresh_array
#     vals_to_use = vals_to_use[inds_to_use]
#     current_pool = current_pool[inds_to_use]
#     
#     if (length(inds_to_use) == 0){
#       match_object <- false_match()
#       return(match_object)
#     }
#   } else {
#     if (sum(vals_to_use) - vals_to_match < thresh){
#       match_object <- false_match()
#       return(match_object)
#     } 
#   }
#  
#   match_indexes = vector()
#   match_vals = vector()
#   
#   parcel_vals_pool = vals_to_use
#   vals_to_match = offset_multiplier*vals_to_match
# 
#   match_flag = all(vals_to_match < thresh)
#   
#   while(match_flag == FALSE){
#     if (length(current_pool) == 0){
#       match_object <- false_match()
#       return(match_object)
#     }
#     euclidean_match = euclidean_norm_match(parcel_vals_pool, vals_to_match)
#     match_vals = c(match_vals, euclidean_match$match_vals)
#     match_indexes = c(match_indexes, current_pool[euclidean_match$match_ind])
#     
#     vals_to_match = vals_to_match - euclidean_match$match_vals
#     match_flag = all(vals_to_match < thresh)
#     
#     if (match_flag == FALSE){
#       ind_to_remove = list_intersect(current_pool, euclidean_match$index_used)
#       ind_to_remove = ind_to_remove$match_ind
#       current_pool = current_pool[-ind_to_remove]
#       parcel_vals_pool = parcel_vals_pool[-ind_to_remove]
#     }
#     else {
#       break
#     }
#     
#   }
#   
#   match_object = list()
#   match_object$match_indexes = match_indexes
#   match_object$match_vals = match_vals
#   match_object$match_flag = match_flag
#   
#   return(match_object)
#   
# }

update_selected_object <- function(match_object, current_pool, pool_object){
  
  match_indexes = match_object$match_indexes
  parcel_num = length(match_indexes)
  parcel_eco_vals = vector('list', parcel_num)
  parcel_sums_at_offset = vector('list', parcel_num)
  parcel_vals_used = vector('list', parcel_num)
  
  pool_used = list_intersect(current_pool, match_indexes)
  pool_used = pool_used$match_val
  
  for (parcel_count in seq_len(parcel_num)){
    matched_parcel_ind = pool_used[parcel_count]
    parcel_eco_vals[[parcel_count]] = pool_object$parcel_eco_vals[[matched_parcel_ind]]
    parcel_sums_at_offset[[parcel_count]] = pool_object$parcel_sums_at_offset[matched_parcel_ind, ]
    parcel_vals_used[[parcel_count]] = parcel_vals_pool[matched_parcel_ind, ]
    
  }
  
  selected_parcel_object = list()
  selected_parcel_object <- record_parcel_info(parcel_sums_at_offset, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr)
  return(selected_parcel_object)
}



# 
# select_from_pool <- function(pool_to_use, vals_to_use, offset_multiplier, vals_to_match, offset_parcel_for_parcel, eco_dims, yr){
#   
#   match_indexes = vector()
#   parcel_vals_pool = vals_to_use
#   current_pool = pool_to_use
#   vals_to_match = offset_multiplier*vals_to_match
#   
#   match_ind_available = 1:length(current_pool)
#   match_array = matrix(rep(vals_to_match, length(match_ind_available)), ncol = length(vals_to_match), byrow = TRUE)
#   #cond = test_cond(vals_to_match, t(t(parcel_vals_pool[match_ind_available, ])), vals_to_match, match_array)
#   
#   cond = all(vals_to_match > 0)
#     
#   parcel_vals_pool = t(t(parcel_vals_pool))
#   
#   while(any(cond > 0)){
#     err = sqrt(rowSums( (parcel_vals_pool[match_ind_available, ] - match_array)^2 ) )
#     match_ind = which(err == min(err))
#     index_used = match_ind_available[match_ind]
#     match_indexes = c(match_indexes, index_used)
#     parcel_vals_used = parcel_vals_pool[index_used, ]
#     
#     if ( offset_parcel_for_parcel == TRUE)
#       {
#       break
#     }
#     match_ind_available = match_ind_available[-match_ind]
#     vals_to_match = vals_to_match - parcel_vals_used
#     match_array = matrix(rep(vals_to_match, length(match_ind_available)), ncol = length(vals_to_match), byrow = TRUE)
#     
#     cond = all(vals_to_match > 0)
#     
#   }
#   
#   parcel_num = length(match_indexes)
#   parcel_indexes = current_pool[match_indexes]
#   parcel_eco_vals = vector('list', parcel_num)
#   
#   current_parcel_sums = array(0, c(parcel_num, eco_dims))
#   parcel_vals_used = array(0, c(parcel_num, length(vals_to_match)))
#   
#   for (parcel_count in seq_len(parcel_num)){
#     match_index = match_indexes[parcel_count]
#     parcel_eco_vals[[parcel_count]] = pool_object$parcel_eco_vals[[match_index]]
#     current_parcel_sums[parcel_count, ] = pool_object$current_parcel_sums[match_index, ]
#     parcel_vals_used[parcel_count, ] = parcel_vals_pool[match_index, ]
#   }
#   
#   selected_parcel_object = list()
#   selected_parcel_object <- record_parcel_info(current_parcel_sums, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr)
#   
#   return(selected_parcel_object)
#   
# }



write_null_offset_object <- function(){
  offset_object = list()
  offset_object$parcel_indexes = list()
  offset_object$parcel_sums_at_offset = list()
  offset_object$parcel_vals_used = list()
  return(offset_object)
}


write_development <- function(development_object, current_ecology){
  
  current_parcel = development_object$current_parcels
  current_ecology[current_parcel] = 0
  
  return(current_ecology)
  
}




find_current_parcel_sums <- function(land_parcels, current_ecology, parcel_indexes, eco_dims){
  
  parcel_sums_object = list()
  parcel_ecologies = vector('list', length(parcel_indexes))
  parcel_sums = vector('list', length(parcel_indexes))
  
  if (class(parcel_indexes) == 'list'){
    parcel_indexes = unlist(parcel_indexes)
  }
  
  for (parcel_count_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_count_ind] 
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    current_parcel_ecologies = extract_3D_parcel(current_parcel, current_ecology)
    parcel_sums[[parcel_count_ind]] = apply(current_parcel_ecologies, 3, sum)
    parcel_ecologies[[parcel_count_ind]] = current_parcel_ecologies
  }
  
  parcel_sums_object$parcel_ecologies = parcel_ecologies
  parcel_sums_object$parcel_sums = parcel_sums
  return(parcel_sums_object)
  
}





write_parcel_sets <- function(parcel_set_object, yr){
  parcel_set = list()
  parcel_set$offset_yrs = yr
  parcel_set$parcel_set_object = parcel_set_object
  return(parcel_set)
}


#update_ind_available(update_type = 'offset', index_object, current_banked_offset_pool, region_ind) 

update_ind_available <- function(update_type, index_object, parcel_indexes, region_ind){
  
  ind_available = index_object$ind_available[[region_ind]]
  ind_available = setdiff(ind_available, parcel_indexes) #remove development parcel from available list   
  
  if ( length(ind_available) < 0 ){   
    break_flag = TRUE
  } else {break_flag = FALSE}
  
  index_object$ind_available[[region_ind]] = ind_available
  
  index_object$break_flag = break_flag
  
  if (update_type == 'development'){
    index_object$developments = c(index_object$developments, parcel_indexes)
    
  } else if (update_type == 'offset'){
    index_object$offsets = c(index_object$offsets, parcel_indexes)
    
  }
  return(index_object)
  
}

update_parcel_num_remaining <- function(index_object, region_ind){

  index_object$parcel_num_remaining = length(index_object$ind_available[[region_ind]])
  
  return(index_object)
}



update_decline_rates <- function(decline_rates, restoration_rate, decline_rate_type, parcel_indexes){

  restoration_rate = global_params$restoration_rate
  offset_action_type = global_params$offset_action_type
  offset_dims = global_params$offset_dims 
  
  if (class(parcel_indexes) == 'list'){
    parcel_indexes = unlist(parcel_indexes)
  }
  
  for (parcel_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_ind]
    loc = ind2sub(dim(decline_rates)[1], current_parcel_ind)
    if (decline_rate_type == 'development'){
      decline_rates[loc[1], loc[2], ] = 0
    } else if (decline_rate_type == 'offset'){
      
      if (offset_action_type == 'protect'){
        offset_rate = decline_rates[loc[1], loc[2], ]  
      } else if (offset_action_type == 'maintain'){
        offset_rate = 1  
      } else if (offset_action_type == 'restore'){
        offset_rate = restoration_rate
      }
      
      if (offset_dims == 1){
        decline_rates[loc[1], loc[2], 1] = offset_rate[1]
      } else {
        decline_rates[loc[1], loc[2], ] = offset_rate
      }
    }
  }
  
  return(decline_rates)
  
}


find_current_dev_nums <- function(dev_vec, region_num, yr){

  dev_nums = array(0, region_num)
  for (region_ind in seq_len(region_num)){
    current_dev_num = dev_vec[yr]
    dev_nums[region_ind] = current_dev_num
  }
  return(dev_nums)
  
}


list_intersect <- function(list_a, list_b){
  if ( (length(list_a) == 0) || (length(list_b) == 0)){
    print('empty list match')
    return()
  }
  vec_a <- unlist(list_a)
  vec_b <- unlist(list_b)
  match_ind <- which(vec_a %in% vec_b)
  match_val <- vec_a[match_ind]
  
  list_match = list()
  list_match$match_ind = match_ind
  list_match$match_val = match_val
  return(list_match)
}


# a = offset_pool_object[[names(offset_pool_object)[1]]]
# 
# setNames(as.list(c(1, 2)), c('foo', 'bar'))
# x <- letters[1:2]
# y <- 1:2
# mapply(function(x, y) {y}, x, y, SIMPLIFY = FALSE, USE.NAMES = TRUE)

# y = offset_pool_object
# x = current_banked_subset
# mapply(function(x, y) {y[x]}, x, y, SIMPLIFY = FALSE, USE.NAMES = TRUE)
# 
# 
# pool_names = names(offset_pool_object)
# lapply(seq_along(pool_names) offset_pool_object[[names(offset_pool_object)[1]]])
# 
# lapply(seq_along(dev_weights), function(i) 

select_current_subset <- function(pool_object, current_pool){
  current_banked_subset =  list_intersect(pool_object$parcel_indexes, current_pool)
  current_banked_subset = current_banked_subset$match_ind
  
  subset_pool_object = list()
  subset_pool_object$parcel_indexes <- pool_object$parcel_indexes[current_banked_subset]
  
  parcel_count = length(current_banked_subset)
  subset_pool_object$parcel_count = parcel_count
  subset_pool_object$offset_yrs = pool_object$offset_yrs[current_banked_subset]
  subset_pool_object$parcel_ecologies = pool_object$parcel_ecologies[current_banked_subset]
  subset_pool_object$parcel_sums_at_offset = pool_object$parcel_sums_at_offset[current_banked_subset]
  subset_pool_object$parcel_num_remaining = pool_object$parcel_num_remaining[current_banked_subset]
  return(subset_pool_object)
}




# assess_current_offset_pool(banked_offsets_object, time_horizon_type, region_ind, global_params, region_params, 
#                            current_ecology, decline_rates = decline_rates_initial, (land_parcels = parcels$land_parcels), index_object, time_horizon, yr)



# time_horizons = generate_time_horizons(time_horizon_type = 'current', yr = global_params$time_steps, offset_yrs = unlist(current_model_outputs$offset_yrs), 
#                                        time_horizon = vector(), parcel_count = length(unlist(current_model_outputs$offset_yrs)))


generate_time_horizons <- function(use_offset_bank, offset_time_horizon_type, yr, offset_yrs, time_horizon, parcel_count){
  if (use_offset_bank == TRUE){
    time_horizons = rep(yr, parcel_count)
    time_horizons = time_horizons - offset_yrs
    if (offset_time_horizon_type == 'future'){
      time_horizons = time_horizons + time_horizon
    }
    
  } else{
    time_horizons = rep(time_horizon, parcel_count)
  }
  return(time_horizons)
}


# 
# 
# pool_type = offset_pool_type
# calc_type = global_params$offset_calc_type
# cfacs_flag = global_params$offset_cfacs_flag 
# adjust_cfacs_flag = global_params$adjust_cfacs_flag
# 
# 
# 

# assess_current_pool(pool_type = 'devs', calc_type = global_params$dev_calc_type, cfacs_flag = global_params$dev_cfacs_flag, 
#                     adjust_cfacs_flag = global_params$adjust_cfacs_flag, cfac_type = global_params$cfac_type_in_dev_calc, banked_offsets_object = list(), time_horizon_type = 'future', region_ind, 
#                     global_params, region_params, current_ecology, decline_rates_initial, (land_parcels = parcels$land_parcels), 
#                     current_pool = current_test_index, time_horizon, yr)


prepare_offset_bank <- function(banked_offsets_object, current_offset_pool, restoration_flag, land_parcels, current_ecology, eco_dims){
  
  pool_object <- select_current_subset(banked_offsets_object, current_pool = current_offset_pool)
  if (global_params$offset_restoration_flag == TRUE){
    current_sums_object <- find_current_parcel_sums(land_parcels, current_ecology, parcel_indexes = current_offset_pool, global_params$eco_dims)
    pool_object$restoration_vals <- current_sums_object$parcel_sums
  }
  
  return(pool_object)
}


# pool_object <- record_current_parcel_set(land_parcels = parcels$land_parcels, current_ecology, current_pool, parcel_num_remaining = index_object$ind_available[[region_ind]], yr)



# assess_current_pool <- function(current_pool, index_object, banked_offsets_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, cfac_type, time_horizon_type, 
#                                 region_ind, global_params, region_params, current_ecology, decline_rates_initial, land_parcels, time_horizon, yr){
#   
#   if ( pool_type == 'offset_bank'){
#     pool_object <- select_current_subset(banked_offsets_object, banked_offset_pool = current_pool)
#     if (global_params$offset_restoration_flag == TRUE){
#       current_sums_object <- find_current_parcel_sums(land_parcels, current_ecology, current_pool, global_params$eco_dims)
#       pool_object$restoration_vals <- current_sums_object$parcel_sums
#     } 
#     
#   } else {
#     parcel_num_remaining = index_object$ind_available[[region_ind]]
#     pool_object <- record_current_parcel_set(land_parcels = parcels$land_parcels, current_ecology, current_pool, parcel_num_remaining, yr)
#   }



# assess_current_pool(pool_object = offset_pool_object, pool_type = offset_pool_type, calc_type = global_params$offset_calc_type, cfacs_flag = global_params$offset_cfacs_flag, 
#                     adjust_cfacs_flag = global_params$adjust_cfacs_flag, cfac_type = global_params$cfac_type_in_offset_calc, time_horizon_type = offset_time_horizon_type,
#                     global_params, region_params, decline_rates_initial, time_horizon, yr)
# 
# 

# pool_object = offset_pool_object
# pool_type = offset_pool_type 
# calc_type = global_params$offset_calc_type 
# cfacs_flag = global_params$offset_cfacs_flag 
# adjust_cfacs_flag = global_params$adjust_cfacs_flag
# cfac_type = global_params$cfac_type_in_offset_calc 
# time_horizon_type = offset_time_horizon_type



assess_current_pool <- function(pool_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, cfac_type, time_horizon_type, 
                                global_params, region_params, decline_rates_initial, time_horizon, yr){
  current_pool = pool_object$parcel_indexes
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  
  time_horizons <- generate_time_horizons(global_params$use_offset_bank, global_params$offset_time_horizon_type, yr, offset_yrs, time_horizon, parcel_count)
    
  if (pool_type == 'offsets'){
    if (global_params$offset_restoration_flag == TRUE){
      pool_object$restoration_vals = predict_parcel_vals_multi(predict_type = 'restore', pool_object$parcel_ecologies, parcel_indexes = current_pool, decline_rates_initial, 
                                                               global_params$restoration_rate, global_params$min_eco_val, global_params$max_eco_val, eco_dims = global_params$eco_dims, time_horizons)
    } else {
      pool_object$restoration_vals = list()
    }
  } else if (pool_type == 'devs'){
    pool_object$restoration_vals = list()
  }
  
  if (cfacs_flag == TRUE){
    
    cfacs_object = calc_cfacs(parcel_indexes = pool_object$parcel_indexes, parcel_ecologies = pool_object$parcel_ecologies, 
                              parcel_num_remaining = pool_object$parcel_num_remaining, global_params, decline_rates_initial, 
                             time_horizons, offset_yrs, cfac_type)
    
    if (adjust_cfacs_flag == TRUE){
      cfac_trajs = sum_trajectories_as_list(cfacs_object$adjusted_cfacs, eco_dims = global_params$eco_dims)
    } else {
      cfac_trajs = sum_trajectories_as_list(cfacs_object$cfacs, eco_dims = global_params$eco_dims)
    }
    
    pool_object$cfac_vals = lapply(seq_along(cfac_trajs), function(i) tail(cfac_trajs[[i]][[1]], 1))
  }
  
  pool_object$parcel_vals_used = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, pool_object$restoration_vals, pool_object$cfac_vals)
  return(pool_object)
}


# find_parcel_set <- function(offset_pool_object, region_ind, global_params, region_params, current_ecology, decline_rates, parcels, index_object, time_horizon, yr){
#   
#   match_object <- match_parcel_set(offset_pool_object, global_params, ind_available = index_object$ind_available[[region_ind]], 
#                                            current_ecology, decline_rates, parcels$land_parcels, yr, time_horizon)
#                                            
# 
#   if (match_object$match_flag == TRUE){
#     
#     matched_parcel_set$development_object = development_object
#   } else {
#     matched_parcel_set$development_object = list()
#   }
#   
#   if (match_object$match_flag == TRUE){
#     development_object <- match_object$development_object
#     offset_object <- match_object$offset_object
#     
#     index_object = update_ind_available(update_type = 'development', index_object, development_object$parcel_indexes, region_ind)
#     decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'development', development_object$parcel_indexes)
#     if (global_params$use_offset_bank == TRUE){
#       banked_offset_pool = index_object$banked_offset_pool
#       banked_offset_inds_used = which(banked_offset_pool == offset_object$parcel_indexes)
#       index_object$banked_offset_pool = index_object$banked_offset_pool[-banked_offset_inds_used]
#     } else {
#       index_object = update_ind_available(update_type = 'offset', index_object, match_object$parcel_indexes, region_ind)
#       decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', offset_object$parcel_indexes)
#     }
#     
#   }
#   
#   parcel_set_object = list()
#   parcel_set_object$index_object = index_object
#   parcel_set_object$decline_rates = decline_rates
#   parcel_set_object$offset_object = offset_object
#   parcel_set_object$development_object = development_object
# 
#   return(parcel_set_object)
#   
# } 
# 





                      
# find_parcel_set <- function(banked_offset_vals_to_use, region_ind, global_params, region_params, current_ecology, decline_rates, parcels, index_object, time_horizon, yr){
#   
#   index_object$parcel_set_count = index_object$parcel_set_count + 1
#   index_object <- update_parcel_num_remaining(index_object, region_ind)
# 
#   development_object <- select_development(banked_offset_vals_to_use, global_params, ind_available = index_object$ind_available[[region_ind]], 
#                                            offset_multiplier = global_params$offset_multiplier, current_ecology, decline_rates,
#                                            parcels$land_parcels, global_params$offset_dims, yr, time_horizon) 
#   
#   index_object <- update_ind_available(update_type = 'development', index_object, development_object$parcel_indexes, region_ind)
#   decline_rates <- update_decline_rates(decline_rates, global_params$restoration_rate, decline_rate_type = 'development', 
#                                         offset_action_type = global_params$offset_action_type, development_object$parcel_indexes, global_params$offset_dims)
#   
#   if (global_params$use_offset_bank == FALSE){
#                   
#     current_pool = index_object$ind_available[[region_ind]]                        
#     offset_pool_object <- evaluate_parcel_pool(calc_type = global_params$offset_calc_type, cfac_type = global_params$cfac_type_in_offset_calc, 
#                                            current_pool, current_parcel_num_remaining = length(current_pool), decline_rates, 
#                                            global_params, parcels$land_parcels, current_ecology, time_horizon, yr)
#     offset_object <- select_from_pool(offset_pool_object, global_params$offset_multiplier, development_object$parcel_vals_used, global_params$offset_parcel_for_parcel, global_params$eco_dims, yr)   
#     
#     index_object <- update_ind_available(update_type = 'offset', index_object, offset_object$parcel_indexes, region_ind)              
#     decline_rates <- update_decline_rates(decline_rates, global_params$restoration_rate, decline_rate_type = 'offset', offset_action_type = global_params$offset_action_type, offset_object$parcel_indexes, global_params$offset_dims)
#     
#   } else {offset_object <- write_null_offset_object() }
#   
#   parcel_set_object = list()
#   parcel_set_object$index_object = index_object
#   parcel_set_object$decline_rates = decline_rates
#   parcel_set_object$offset_object = offset_object
#   parcel_set_object$development_object = development_object
# 
#   return(parcel_set_object)
#   
# } 





write_current_parcel_set <- function(parcel_set_object, current_parcel_set_object, parcel_set_count){
  parcel_set_object$offset_yrs[[parcel_set_count]] = current_parcel_set_object$offset_yrs
  parcel_set_object$parcel_ecologies[[parcel_set_count]] = current_parcel_set_object$parcel_ecologies
  parcel_set_object$parcel_sums[[parcel_set_count]] = current_parcel_set_object$parcel_sums
  parcel_set_object$parcel_indexes[[parcel_set_count]] = current_parcel_set_object$parcel_indexes
  parcel_set_object$parcel_vals_used[[parcel_set_count]] = current_parcel_set_object$parcel_vals_used
  parcel_set_object$parcel_num_remaining[[parcel_set_count]] = current_parcel_set_object$parcel_num_remaining
  parcel_set_object$parcel_set_count = parcel_set_count
  return(parcel_set_object)
}

initialise_parcel_set_object <- function(dev_num){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = vector('list', dev_num)
  parcel_set_object$parcel_ecologies = vector('list', dev_num)
  parcel_set_object$parcel_sums = vector('list', dev_num)
  parcel_set_object$parcel_indexes = vector('list', dev_num)
  parcel_set_object$parcel_vals_used = vector('list', dev_num)
  return(parcel_set_object)
  
}

select_banked_offset_indexes <- function(offset_bank_num, ind_available){
  
  parcels_to_bank = sample(ind_available, offset_bank_num)
  return(parcels_to_bank)
}





assess_banked_offset_vals <- function(offset_pool_object, global_params, yr, current_ecology, land_parcels, time_horizons){
  
  calc_type = global_params$offset_calc_type
  cfacs_flag = global_params$offset_cfacs_flag
  adjust_cfacs_flag = global_params$adjust_cfacs_flag
  
  current_banked_group = offset_pool_object$parcel_indexes
  
  current_banked_offset_num = length(current_banked_group)
  
  offset_vals_object <- find_current_parcel_sums(land_parcels, current_ecology, current_banked_group, global_params$eco_dims)
  
  banked_offset_vals = offset_vals_object$parcel_sums
  
  banked_offset_vals_used = vector('list', current_banked_offset_num)
  
  for (banked_offset_ind in seq_len(current_banked_offset_num)){
    
    time_horizon = time_horizons[banked_offset_ind]
    offset_yr = offset_pool_object$offset_yrs[[banked_offset_ind]]
    
    parcel_sums_at_offset = offset_pool_object$parcel_sums_at_offset[[banked_offset_ind]]
    restoration_vals = banked_offset_vals[banked_offset_ind]
    
    if (cfacs_flag == TRUE){
      if (adjust_cfacs_flag == TRUE){
        counter_trajs = offset_pool_object$cfac_trajs$adjusted[[banked_offset_ind]]
      } else{
        counter_trajs = offset_pool_object$cfac_trajs$standard[[banked_offset_ind]]
      }
      cfac_vals = last_element_in_list(counter_trajs)
    } else {
      cfac_vals = vector()
    }
    
    current_offset_parcel_vals = evaluate_parcel_vals(calc_type, parcel_sums_at_offset, restoration_vals, cfac_vals)
    
    banked_offset_vals_used[[banked_offset_ind]] = unlist(current_offset_parcel_vals)
  }
  
  return(banked_offset_vals_used)
  
}





# 
# 
# assess_banked_offset_vals <- function(banked_offsets_object, global_params, yr, current_ecology, land_parcels){
#   
#   calc_type = global_params$offset_calc_type
#   cfacs_flag = global_params$offset_cfacs_flag
#   adjust_cfacs_flag = global_params$adjust_cfacs_flag
#   
#   current_banked_group = banked_offsets_object$parcel_indexes
#   
#   current_banked_offset_num = length(current_banked_group)
# 
#   offset_vals_object <- find_current_parcel_sums(land_parcels, current_ecology, current_banked_group)
#   
#   banked_offset_vals = offset_vals_object$parcel_sums
#   
#   banked_offset_vals_used = vector('list', current_banked_offset_num)
#   
#   for (banked_offset_ind in seq_len(current_banked_offset_num)){
#     offset_yr = banked_offsets_object$offset_yrs[[banked_offset_ind]]
#     time_elapsed = yr - offset_yr + 1
#     
#     parcel_sums_at_offset = banked_offsets_object$parcel_sums_at_offset[[banked_offset_ind]]
#     restoration_vals = banked_offset_vals[banked_offset_ind]
#     
#     if (cfacs_flag == TRUE){
#       if (adjust_cfacs_flag == TRUE){
#         counter_trajs = banked_offsets_object$cfac_trajs$adjusted[[banked_offset_ind]]
#       } else{
#         counter_trajs = banked_offsets_object$cfac_trajs$standard[[banked_offset_ind]]
#       }
#       cfac_vals = counter_trajs[time_elapsed, ]
#     } else {
#       cfac_vals = vector()
#     }
#     
#     current_offset_parcel_vals = evaluate_parcel_vals(calc_type, parcel_sums_at_offset, restoration_vals, cfac_vals)
#                                                       
#     banked_offset_vals_used[[banked_offset_ind]] = unlist(current_offset_parcel_vals)
#   }
#   
#   return(banked_offset_vals_used)
#   
# }


initialise_banked_offsets_object <- function(global_params){
  banked_offsets_object = list()
  
  if (global_params$use_offset_bank == TRUE){
    
    offset_bank_num = global_params$offset_bank_num
    cfacs_flag = global_params$offset_cfacs_flag
    
    
    banked_offsets_object$bank_num = 0
    banked_offsets_object$cfacs_flag = cfacs_flag
    rec_list = vector('list', offset_bank_num)
    banked_offsets_object$parcel_indexes = rec_list
    banked_offsets_object$parcel_num_remaining = rec_list
    banked_offsets_object$offset_yrs = rec_list
    banked_offsets_object$parcel_ecologies = rec_list
    banked_offsets_object$parcel_sums_at_offset = rec_list
    banked_offsets_object$cfac_trajs$standard = rec_list
    banked_offsets_object$cfac_trajs$adjusted = rec_list
    banked_offsets_object$parcel_vals_used = rec_list
  }
  return(banked_offsets_object)
}



update_banked_offsets <- function(banked_offsets_object, current_banked_offset){
  
  current_banked_offset_num = banked_offsets_object$bank_num
  
  for (parcel_set_ind in 1:length(current_banked_offset$parcel_indexes)){
    
    banked_offsets_object$offset_yrs[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$offset_yrs[[parcel_set_ind]]
    
    banked_offsets_object$parcel_ecologies[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_ecologies[[parcel_set_ind]]
    
    banked_offsets_object$parcel_sums_at_offset[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_sums_at_offset[[parcel_set_ind]]
    
    banked_offsets_object$parcel_num_remaining[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_num_remaining[[parcel_set_ind]]
    
    banked_offsets_object$parcel_indexes[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_indexes[[parcel_set_ind]]
    
  }
  
  banked_offsets_object$bank_num = banked_offsets_object$bank_num + length(current_banked_offset$parcel_indexes)
  
  return(banked_offsets_object)
}




# update_banked_offsets <- function(banked_offsets_object, current_banked_offset, adjust_cfacs_flag){
#   
#   current_banked_offset_num = banked_offsets_object$bank_num
#   
#   for (parcel_set_ind in 1:length(current_banked_offset$parcel_indexes)){
#     
#     banked_offsets_object$offset_yrs[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$offset_yrs
#     banked_offsets_object$parcel_ecologies[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_ecologies[[parcel_set_ind]]
#     
#     banked_offsets_object$parcel_sums_at_offset[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_sums_at_offset[[parcel_set_ind]]
#     
#     if (current_banked_offset$cfac_flag == TRUE){
#       banked_offsets_object$cfac_trajs$standard[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$cfac_trajs$standard[[parcel_set_ind]]
#       if (adjust_cfacs_flag == TRUE){
#         banked_offsets_object$cfac_trajs$adjusted[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$cfac_trajs$adjusted[[parcel_set_ind]]
#       }
#     }
#     
#     banked_offsets_object$parcel_indexes[[current_banked_offset_num + parcel_set_ind]] = current_banked_offset$parcel_indexes[[parcel_set_ind]]
# 
#   }
#   
#   banked_offsets_object$bank_num = banked_offsets_object$bank_num + length(current_banked_offset$parcel_indexes)
#   
#   return(banked_offsets_object)
# }



update_banked_offset_pool <- function(index_object, current_banked_offset_pool){
  index_object$banked_offset_pool = c(index_object$banked_offset_pool, current_banked_offset_pool)
  return(index_object)
}



assess_time_horizon <- function(use_offset_time_horizon, offset_time_horizon, time_steps, yr){
  if (use_offset_time_horizon == TRUE){
    time_horizon = offset_time_horizon
  } else {
    time_horizon = time_steps - yr
  }
  
#   if (assess_type == 'current'){
#     time_horizons = rep(yr, parcel_count)
#     time_horizons = time_horizons - offset_yrs
#   } else if (assess_type == 'future'){
#     time_horizons = rep(time_horizon, parcel_count)
#   }
  
  return(time_horizon)
}


# 
# 
# run_matched_parcel_set_routine <- function(match_object, offset_pool_object, index_object, region_ind, global_params, decline_rates){
#   
#   development_object <- match_object$matched_object
#   
#   matched_offset_parcel_inds = which(offset_pool_object$parcel_indexes == match_object$match_pool_indexes)
#   
#   offset_object = record_parcel_info(offset_pool_object$parcel_sums_at_offset[matched_offset_parcel_inds], 
#                                      offset_pool_object$parcel_indexes[matched_offset_parcel_inds], 
#                                      offset_pool_object$parcel_ecologies[matched_offset_parcel_inds], 
#                                      offset_pool_object$parcel_vals_used[matched_offset_parcel_inds], 
#                                      offset_pool_object$offset_yrs[matched_offset_parcel_inds])
# 
#  
#   matched_parcel_set_object$offset_object = offset_object
#   matched_parcel_set_object$development_object = development_object
#   return(matched_parcel_set_object)
# }


 # run the model, select and record parcel sets, calculate landscape condition time series



run_system <- function(trajectories, offsets_object, developments_object, banked_offsets_object, global_params, region_params, 
                       current_ecology, decline_rates_initial, parcels, index_object){ # main engine for code - returns all development/offset parcel sets, land parcel trajectories etc.
  
  yr = 1
  offset_bank_num = global_params$banked_offset_vec[yr]
  current_ecology = initial_ecology
  decline_rates = decline_rates_initial
  perform_offsets = TRUE
  region_ind = 1
  current_dev_nums <- find_current_dev_nums(region_params, global_params$region_num, yr)
  current_develop_num = current_dev_nums[region_ind]
  land_parcels = parcels$land_parcels
  time_horizon = 20
  
  trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
  offsets_object <- initialise_parcel_set_object(global_params$total_dev_num)   #initialise offsets object to store all offsets
  developments_object <- initialise_parcel_set_object(global_params$total_dev_num) #initialise developments object to store all offsets
  banked_offsets_object <- initialise_banked_offsets_object(global_params)
  index_object <- initialise_index_object(parcels, global_params)
  
  
  
  
  decline_rates = decline_rates_initial
  
  for (yr in seq_len(global_params$time_steps)){
    
    current_dev_nums <- find_current_dev_nums(global_params$dev_vec, global_params$region_num, yr) #developments to perform in current year per region
    time_horizon <- assess_time_horizon(global_params$use_offset_time_horizon, global_params$offset_time_horizon, global_params$time_steps, yr)
    
    for (region_ind in seq_len(parcels$region_num)){ 
      
      if (global_params$use_offset_bank == TRUE){
        
        offset_bank_num = global_params$banked_offset_vec[yr]
        
        if (offset_bank_num > 0){
          index_object <- update_parcel_num_remaining(index_object, region_ind)
          
          total_current_pool = index_object$ind_available[[region_ind]]
          current_banked_offset_pool <- select_banked_offset_indexes(offset_bank_num, total_current_pool)
          
          index_object <- update_banked_offset_pool(index_object, current_banked_offset_pool)
          index_object <- update_ind_available(update_type = 'offset', index_object, current_banked_offset_pool, region_ind) 
          
          current_banked_offset <- record_current_parcel_set(land_parcels = parcels$land_parcels, current_ecology, current_pool = current_banked_offset_pool, 
                                                             parcel_num_remaining = index_object$parcel_num_remaining, yr)
          
          banked_offsets_object <- update_banked_offsets(banked_offsets_object, current_banked_offset)
          decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', current_banked_offset_pool)
          
        }
      }
      
      current_develop_num = current_dev_nums[region_ind]
      
      if (current_develop_num > 0){
        
        for (parcel_set_count_index in seq_len(current_develop_num)){
          
          if (global_params$use_offset_bank == TRUE){
            current_offset_pool = index_object$banked_offset_pool
            
            if (length(current_offset_pool) == 0){
              break
            }
            
            offset_pool_object <- prepare_offset_bank(banked_offsets_object, current_offset_pool, restoration_flag = global_params$offset_restoration_flag, 
                                                      land_parcels, current_ecology, eco_dims = global_params$eco_dims)
            
            offset_pool_type = 'offset_bank'
            
          } else {
            current_offset_pool = index_object$ind_available[[region_ind]]
            parcel_num_remaining = length(current_offset_pool)
            offset_pool_object <- record_current_parcel_set(land_parcels = parcels$land_parcels, current_ecology, current_offset_pool, parcel_num_remaining, yr)
            offset_pool_type = 'offsets'
          }

          offset_pool_object <- assess_current_pool(pool_object = offset_pool_object, pool_type = offset_pool_type, calc_type = global_params$offset_calc_type, cfacs_flag = global_params$offset_cfacs_flag, 
                                                    adjust_cfacs_flag = global_params$adjust_cfacs_flag, cfac_type = global_params$cfac_type_in_offset_calc, time_horizon_type = global_params$offset_time_horizon_type,
                                                    global_params, region_params, decline_rates_initial, time_horizon, yr)
          
          matched_parcel_set_object <- match_parcel_set(offset_pool_object, global_params, ind_available = index_object$ind_available[[region_ind]], 
                                                        current_ecology, decline_rates_initial, parcels$land_parcels, yr, time_horizon, 
                                                        net_development_val = sum(unlist(developments_object$parcel_vals_used)))
          
          #       print(c(sum(unlist(offset_pool_object$parcel_vals_used)), sum(unlist(developments_object$parcel_vals_used))))
          
          if (matched_parcel_set_object$match_flag == TRUE){
            
            index_object$parcel_set_count = index_object$parcel_set_count + 1
            
            current_development_object <- matched_parcel_set_object$development_object
            current_dev_indexes = current_development_object$parcel_indexes
            index_object = update_ind_available(update_type = 'development', index_object, current_dev_indexes, region_ind)
            decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'development', current_dev_indexes)
            developments_object <- write_current_parcel_set(developments_object, current_development_object, index_object$parcel_set_count)  #record development info for current parcel set
            
            if (global_params$match_type == 'parcel_set'){ #record offset info for current parcel set
              current_offset_object <- matched_parcel_set_object$offset_object
              current_offset_indexes = current_offset_object$parcel_indexes
              
              if (global_params$use_offset_bank == TRUE){
                
                banked_offset_inds_used = list_intersect(index_object$banked_offset_pool, current_offset_indexes)
                banked_offset_inds_used = banked_offset_inds_used$match_ind
                index_object$banked_offset_pool = index_object$banked_offset_pool[-banked_offset_inds_used]
                
              } else {
                index_object = update_ind_available(update_type = 'offset', index_object, current_offset_indexes, region_ind)
                decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', current_offset_indexes)
              }
              offsets_object <- write_current_parcel_set(offsets_object, current_offset_object, index_object$parcel_set_count)
              
            }
            
          } 
          
        } 
        
      }
      
      for (eco_ind in seq_len(global_params$eco_dims)){
        trajectories[[eco_ind]][, , yr] = current_ecology[, , eco_ind] # record current ecology in trajectories list for each eco dimension
      }
      
      current_ecology <- project_current_system(current_ecology, parcels$land_parcels, decline_rates, global_params$min_eco_val, 
                                                global_params$max_eco_val, time_horizon = 1, global_params$eco_dims)     # update ecology for subsequent time step using current decline rates
    }
    
    
  }
  
  if (global_params$match_type == 'banked'){
    offsets_object <- write_current_parcel_set(offsets_object, banked_offsets_object, parcel_set_count = 1)
  }
  

  outputs = list()
  #outputs$banked_offsets_object = banked_offsets_object
  outputs$offsets = offsets_object
  outputs$developments = developments_object
  outputs$trajectories = trajectories
  outputs$decline_rates = decline_rates
  outputs$parcel_num_remaining = index_object$parcel_num_remaining
  outputs$index_object = index_object
  return(outputs)

 } 




# 
# record_matched_parcel_set <- function(match_object, offset_pool_object){
# 
#   development_object <- match_object$matched_object
#   
#   matched_offset_parcel_inds = which(offset_pool_object$parcel_indexes == match_object$match_pool_indexes)
#   
#   offset_object = record_parcel_info(offset_pool_object$parcel_sums_at_offset[matched_offset_parcel_inds], 
#                                      offset_pool_object$parcel_indexes[matched_offset_parcel_inds], 
#                                      offset_pool_object$parcel_ecologies[matched_offset_parcel_inds], 
#                                      offset_pool_object$parcel_vals_used[matched_offset_parcel_inds], 
#                                      offset_pool_object$offset_yrs[matched_offset_parcel_inds])
#   matched_parcel_set = list()
#   matched_parcel_set$development_object = development_object
#   matched_parcel_set$offset_object = offset_object
#   return(matched_parcel_set)
# 
# } 


update_index_object <- function(index_object, current_dev_indexes, current_offset_indexes, region_ind, use_offset_bank){
  
  index_object = update_ind_available(update_type = 'development', index_object, current_dev_indexes, region_ind)
   
  if (global_params$use_offset_bank == TRUE){
    banked_offset_inds_used = list_intersect(index_object$banked_offset_pool, current_offset_indexes)
    index_object$banked_offset_pool = index_object$banked_offset_pool[-banked_offset_inds_used$match_ind]
  } else {
    index_object = update_ind_available(update_type = 'offset', index_object, current_offset_indexes, region_ind)
  }
  
  return(index_object)
  
}


# run_system <- function(global_params, region_params, current_ecology, decline_rates, parcels, index_object){ # main engine for code - returns all development/offset parcel sets, land parcel trajectories etc.
#   
#   trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
#   net_offsets_object <- initialise_parcel_set_object(global_params$total_dev_num)   #initialise offsets object to store all offsets
#   net_developments_object <- initialise_parcel_set_object(global_params$total_dev_num) #initialise developments object to store all offsets
#   banked_offsets_object <- initialise_banked_offsets_object(global_params$offset_bank_num)
#   banked_offset_vals <- vector('list', global_params$time_steps)
#   #main time loop   
#   for (yr in seq_len(global_params$time_steps)){   
#     current_dev_nums <- find_current_dev_nums(global_params$dev_vec, global_params$region_num, yr) #developments to perform in current year per region
#     
#     if (global_params$use_offset_time_horizon == TRUE){
#       time_horizon = global_params$offset_time_horizon
#     } else {
#       time_horizon = global_params$time_steps - yr
#     }
#     
#     for (region_ind in seq_len(parcels$region_num)){ 
#       current_develop_num = current_dev_nums[region_ind]
# 
#       if (global_params$use_offset_bank == TRUE){
#         current_offset_bank_num = global_params$offset_vec[yr]
#         
#         if (current_offset_bank_num > 0){
#           
#           total_current_pool = index_object$ind_available[[region_ind]]
#           current_pool <- select_banked_offset_indexes(offset_bank_num = global_params$offset_vec[yr], total_current_pool)
#           
#           current_banked_offset_pool <- evaluate_parcel_pool(calc_type = global_params$offset_calc_type, cfac_type = global_params$cfac_type_in_offset_calc, 
#                                                              current_pool, current_parcel_num_remaining = length(total_current_pool), decline_rates, 
#                                                              global_params, land_parcels, current_ecology, 
#                                                              (time_horizon = global_params$time_steps - yr), yr)
#           
#           banked_offsets_object <- record_banked_offsets(banked_offsets_object, current_banked_offset_pool, global_params$adjust_cfacs_flag)
#           
#           index_object <- update_ind_available(update_type = 'offset', index_object, current_pool, region_ind) 
#           decline_rates <- update_decline_rates(decline_rates, global_params$restoration_rate, decline_rate_type = 'offset', 
#                                                 offset_action_type = global_params$offset_action_type, current_pool, global_params$offset_dims)
#         
#         }
#         
#         if ( length(banked_offsets_object$parcel_indexes) > 0 ){
#           banked_offset_vals[[yr]] <- assess_current_banked_offset_vals(banked_offsets_object, calc_type = global_params$offset_calc_type, yr, current_ecology, 
#                                                                 land_parcels, global_params$adjust_cfacs_flag)
#         } else banked_offset_vals[[yr]] = vector()
#         
#       } 
#       
#       if (current_develop_num > 0){
#         
#         for (dev_index in seq_len(current_develop_num)){
#           
#           parcel_set_object <- find_parcel_set(banked_offset_vals_to_use = sum(banked_offset_vals[[yr]]), region_ind, global_params, 
#                                                 region_params, current_ecology, decline_rates, parcels, index_object, time_horizon, yr) # find the development/offset parcel sets for each proposed development
#           decline_rates <- parcel_set_object$decline_rates   # update decline rates
#           index_object <- parcel_set_object$index_object     # update index obeject containing all available parcels
#           net_offsets_object <- write_current_parcel_set(net_offsets_object, parcel_set_object$offset_object, index_object$parcel_set_count)  #record offset info for current parcel set
#           net_developments_object <- write_current_parcel_set(net_developments_object, parcel_set_object$development_object, index_object$parcel_set_count)  #record development info for current parcel set
#           
#         }
#       }
#       
#     }
#     
#     for (eco_ind in seq_len(global_params$eco_dims)){
#       trajectories[[eco_ind]][, , yr] = current_ecology[, , eco_ind] # record current ecology in trajectories list for each eco dimension
#     }
#     current_ecology <- project_current_system(current_ecology, parcels$land_parcels, decline_rates, global_params$min_eco_val, 
#                                               global_params$max_eco_val, time_horizon = 1, global_params$eco_dims)     # update ecology for subsequent time step using current decline rates
#     
#     print(yr)
#   }
#   
#   
#   if (global_params$record_parcel_sets == TRUE){
#     outputs = list()
#     outputs$banked_offsets_object = banked_offsets_object
#     outputs$banked_offset_vals = banked_offset_vals
#     outputs$offset_list = index_object$offsets
#     outputs$development_list = index_object$developments
#     outputs$offsets = net_offsets_object
#     outputs$developments = net_developments_object
#     outputs$trajectories = trajectories
#     outputs$decline_rates = decline_rates
#     outputs$parcel_num_remaining = index_object$parcel_num_remaining
#     return(outputs)
#   } else{
#     return(trajectories)
#   }
#   
# }


select_land_parcel <- function(land_parcels, current_parcel_ind){
  selected_land_parcel = land_parcels[[current_parcel_ind]]
  return(selected_land_parcel)
}


# update land parcels according to development/offset action (maintain/restore) 

project_current_system <- function(current_ecology, land_parcels, decline_rates, min_eco_val, max_eco_val, time_horizon, eco_dims){
  
  parcel_num = length(land_parcels)
  
  for (eco_ind in seq_len(eco_dims)){
    current_ecology_slice = current_ecology[, , eco_ind] #select current ecological dimension to work on
    current_decline_rates = decline_rates[,  , eco_ind] #select current decline rates to work on
    for (parcel_ind in seq_len(parcel_num)){  
      current_parcel = select_land_parcel(land_parcels, parcel_ind)   #select array indexes that correspond to current land parcel
      current_parcel_ecology = current_ecology[current_parcel]
      decline_rate = current_decline_rates[parcel_ind] #select corresponding decline rate
      if (decline_rate == 0){
        updated_parcel_ecology = array(0, length(current_parcel_ecology))   # if the parcel is to be developed (i.e. decline rate = 0), set parcel value to zeros
      } else if (decline_rate == 1){
        updated_parcel_ecology = current_parcel_ecology      # if the parcel is to be maintained (i.e. decline rate = 1), set parcel values to current values
      } else {updated_parcel_ecology = sapply(current_parcel_ecology, project_ecology, min_eco_val = min_eco_val, 
                                              max_eco_val = max_eco_val, decline_rate = decline_rate, time_horizon = time_horizon, time_fill = 'none')}  # update ecology according to ecological curve in project_ecology function (currently logistic) - curve parameters are contained in decline_rates array

      current_ecology_slice[current_parcel] = updated_parcel_ecology 
    }
    current_ecology[, , eco_ind]  = current_ecology_slice
  }
  return(current_ecology) 
}
  

plot_outs <- function(...){
  
  dots = list(...)
  plot_params = dots[[length(dots)]]
  
  plot_num = length(dots) - 1
  sub_plot_num = plot_params[1]*plot_params[2]
  A = 1:sub_plot_num
  dim(A) = c(plot_params[1], plot_params[2])
  layout(A)
  ymaxs = array(0, plot_num)
  ymins = array(0, plot_num)
  for (s in seq_len((length(dots) - 1))){
    ymaxs[s] = max(dots[[s]])
    ymins[s] = min(dots[[s]])
  }
  ymax = max(ymaxs)
  ymin = min(ymins)
  for (s in seq_len(sub_plot_num)){
    plot(dots[[1]][, s], type = 'l', col = 'red', ylim = c(ymin, ymax))
    if (plot_num > 1){
      for (t in 2:plot_num){
        lines(dots[[t]][, s],  ylim = c(ymin, ymax))
      }
    } 
  }
}



build_trajectories_as_list <- function(traj_list, parcel_pool, eco_dims){
  traj_num = length(parcel_pool)
  trajectories_list = vector('list', traj_num)
  for (traj_ind in 1:traj_num){
    parcel_pool_ind = parcel_pool[traj_ind]
    for (eco_ind in 1:eco_dims){
      trajectories_list[[traj_ind]][, eco_ind] = apply(traj_list[[parcel_pool_ind]][[eco_ind]], MARGIN = 3, sum)
    }
  }
  return(trajectories_list)
}


# extract_parcel_set_trajs <- function(traj_list, parcel_set_indexes){
#   
#   if (class(parcel_set_indexes) == 'list'){
#     parcel_set_indexes = unlist(parcel_set_indexes)
#   }
#   parcel_set_num = length(parcel_set_indexes)
#   
#   parcel_set_traj_list = vector('list', parcel_set_num)
#   for (parcel_set_ind in seq_len(parcel_set_num)){
#     current_parcel_indexes = parcel_set_indexes[[parcel_set_ind]]
#     parcel_num = length(current_parcel_indexes)
#     current_parcel_set_traj = vector('list', parcel_num)
#     for (parcel_count_ind in seq_len(parcel_num)){
#       parcel_ind = current_parcel_indexes[parcel_count_ind]
#       current_parcel_set_traj[[parcel_count_ind]] = traj_list[[parcel_ind]]
#     }
#     parcel_set_traj_list[[parcel_set_ind]] = current_parcel_set_traj
#   }
#   return(parcel_set_traj_list)
#   
# }





build_traj_list <- function(trajectories, land_parcels, parcel_indexes, eco_dims){
  
  parcel_num = length(parcel_indexes)
  traj_list = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_traj = vector('list', eco_dims)
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind = parcel_indexes[parcel_count_ind])
    
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_traj[[eco_ind]] = extract_3D_parcel(current_parcel, trajectories[[eco_ind]][, , ])
    }
    
    traj_list[[parcel_count_ind]] = current_parcel_traj
  }
  
  return(traj_list)
}
  




#offset_parcel_indexes = (outs$model_outputs$offset_list)
#dev_parcel_indexes = (outs$model_outputs$development_list)

# sum_program_parcels <- function(traj_list, eco_dims, offset_parcel_indexes, dev_parcel_indexes, time_horizon){
#   summed_program_parcels = list()
#   offset_trajs = sum_parcel_trajectories(traj_list, eco_dims, parcel_indexes = offset_parcel_indexes, time_horizon) 
#   dev_trajs = sum_parcel_trajectories(traj_list, eco_dims, parcel_indexes = dev_parcel_indexes, time_horizon)
#   net_trajs = offset_trajs + dev_trajs
#   
#   summed_program_parcels$offset_trajs = sum_cols_multi(offset_trajs)
#   summed_program_parcels$dev_trajs = sum_cols_multi(dev_trajs)
#   summed_program_parcels$net_trajs = sum_cols_multi(net_trajs)
#   return(summed_program_parcels)
# }

sum_program_cfacs <- function(parcel_cfac_trajs, parcel_indexes){
  program_cfacs = parcel_cfac_trajs[, parcel_indexes, ]
  program_cfac_sum = sum_cols_multi(program_cfacs)
  return(program_cfac_sum)
}





# collated_offsets = collated_parcel_sets$offsets
# collated_developments = collated_parcel_sets$developments
# parcel_set_num = length(parcel_sets_object$offsets)


# parcel_sets_object = outs$parcel_sets_object
# time_steps = global_params$time_steps
# eco_dims = global_params$eco_dims
# current_assessed_object = parcel_sets_object$offsets
# parcel_set_ind = 1
# eco_ind = 1

# collate_parcel_sets <- function(parcel_sets_object, global_params, eco_dims){
#   collated_parcel_sets = list()
#   collated_parcel_sets$offsets = collate_parcel_sets_object(parcel_sets_object$offsets, global_params, eco_dims)
#   collated_parcel_sets$developments = collate_parcel_sets_object(parcel_sets_object$developments, global_params, eco_dims)
#   collated_parcel_sets$NNL_object = assess_NNL(collated_parcel_sets$offsets, collated_parcel_sets$developments, eco_dims, parcel_set_num = length(parcel_sets_object$offsets))
#   return(collated_parcel_sets)
# }



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










find_decline_rate <- function(decline_rates, current_parcel_ind, eco_ind){
  loc = ind2sub(dim(decline_rates)[1], current_parcel_ind)
  current_dec_rate = decline_rates[loc[1], loc[2], eco_ind]
  return(current_dec_rate)
}



# 
# current_cfacs = calc_cfacs(parcel_indexes = parcel_set_indexes, parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE),
#                            parcel_num_remaining = current_model_outputs$parcel_num_remaining, global_params, decline_rates_initial, time_horizons, 
#                            offset_yrs = current_model_outputs$offset_yrs, cfac_type)
# 
# parcel_indexes = parcel_set_indexes
# parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE)
# parcel_num_remaining = current_model_outputs$parcel_num_remaining
# offset_yrs = current_model_outputs$offset_yrs

calc_cfacs <- function(parcel_indexes, parcel_ecologies, parcel_num_remaining, global_params, decline_rates_initial, time_horizons, offset_yrs, cfac_type){
  
  cfacs_object = list()
  
  parcel_count = length(parcel_indexes)
  cfacs = vector('list', parcel_count)
  cfacs_include_clearing = vector('list', parcel_count)
  cfacs_include_clearing_offsets = vector('list', parcel_count)
  
  current_cfacs = build_cfacs_list(global_params, decline_rates_initial, parcel_ecologies, parcel_indexes, time_horizons)
  
  if (global_params$adjust_cfacs_flag == TRUE){
    adjusted_cfacs = adjust_cfacs(current_cfacs, current_parcel_ecologies = parcel_ecologies, adjusted_cfac_type = cfac_type, global_params, 
                                  parcel_num_remaining, decline_rates_initial, parcel_indexes, time_horizons, offset_yrs)
    cfacs_object$adjusted_cfacs = adjusted_cfacs
  }
  
  cfacs_object$cfacs = current_cfacs

  return(cfacs_object)
  
}  




# 
# calc_cfacs <- function(current_sets_object, global_params, decline_rates, parcel_num_remaining, yr){
#   
#   parcel_count = length(unlist(current_sets_object$parcel_indexes))
#   
#   cfacs = vector('list', parcel_count)
#   cfacs_include_clearing = vector('list', parcel_count)
#   cfacs_include_clearing_offsets = vector('list', parcel_count)
#   
#   offset_yrs = unlist(current_sets_object$offset_yrs)
#   
#   time_horizons = rep(yr, parcel_count)
#   time_horizons = time_horizons - offset_yrs
#   
#   current_cfacs = build_cfacs_list(global_params, decline_rates, (parcel_ecologies = current_sets_object$parcel_ecologies[[parcel_set_ind]]), current_parcel_index, time_horizons)
#   
#   
#   for (parcel_count_ind in seq_len(parcel_count)){
#     offset_yr = yrs[parcel_set_ind]
#     time_horizon = yr - offset_yr
#     current_parcel_index = parcel_indexes[parcel_set_ind]
#     
#     current_cfacs = build_cfacs_list(global_params, decline_rates, (parcel_ecologies = current_sets_object$parcel_ecologies[[parcel_set_ind]]), current_parcel_index, time_horizons)
#     
#     cfacs[[parcel_set_ind]] = current_cfacs
#     if (global_params$adjust_counters_post_facto == TRUE){
#       cfacs_include_clearing[[parcel_set_ind]] = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing', global_params, 
#                                                               parcel_num_remaining[[parcel_set_ind]], decline_rates, parcel_indexes, time_horizon, yr)
#       cfacs_include_clearing_offsets[[parcel_set_ind]] = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing_offsets', global_params, 
#                                                                       parcel_num_remaining[[parcel_set_ind]], decline_rates, parcel_indexes, time_horizon, yr)
#     }
#   }
#   
#   cfacs_object = list()
#   cfacs_object$cfacs = cfacs
#   cfacs_object$cfacs_include_clearing = cfacs_include_clearing
#   cfacs_object$cfacs_include_clearing_offsets = cfacs_include_clearing_offsets
#   return(cfacs_object)
# }  
# 


# cfacs_include_clearing = adjust_cfacs(current_cfacs, current_parcel_ecologies = (current_sets_object$parcel_ecologies), adjusted_cfac_type = 'include_clearing', global_params, 
#                                       parcel_num_remaining = current_sets_object$parcel_num_remaining, decline_rates, parcel_indexes, time_horizons, offset_yrs)


# cfacs_adjusted = adjust_cfacs(cfacs_standard, current_parcel_ecologies, adjusted_cfac_type = cfac_type, global_params, 
#                               parcel_num_remaining, decline_rates, parcel_indexes, time_horizon, yr)


# current_cfacs = cfacs_standard
# adjusted_cfac_type = cfac_type
# decline_rates = decline_rates_initial
# time_horizons = time_horizon
# 
# cfacs_adjusted = adjust_cfacs(cfacs_standard, current_parcel_ecologies, adjusted_cfac_type = cfac_type, global_params, 
#                               parcel_num_remaining, decline_rates, parcel_indexes, time_horizon, yr)
# 
# adjust_cfacs(current_cfacs, current_parcel_ecologies = (current_sets_object$parcel_ecologies), adjusted_cfac_type = cfac_type, global_params, 
#                               parcel_num_remaining = current_sets_object$parcel_num_remaining, decline_rates_initial, parcel_indexes, time_horizons, offset_yrs)
# 

# current_cfacs = cfacs_standard 
# current_parcel_ecologies = parcel_pool_object$parcel_ecologies
# adjusted_cfac_type = cfac_type
# parcel_num_remaining = parcel_pool_object$parcel_num_remaining
# parcel_indexes = current_pool
# offset_yrs = parcel_pool_object$offset_yrs

adjust_cfacs <- function(current_cfacs, current_parcel_ecologies, adjusted_cfac_type, global_params, parcel_num_remaining, decline_rates, parcel_indexes, 
                         time_horizons, offset_yrs){
  
  if (class(time_horizons) == 'list'){
    time_horizons = unlist(time_horizons)
  }
  dev_probs <- find_dev_probability(global_dev_vec = global_params$dev_vec, offset_yrs, time_horizons, parcel_num = length(parcel_indexes), parcel_num_remaining)
  
  if (adjusted_cfac_type == 'include_clearing'){
    dev_weights <- lapply(dev_probs, cumsum)
    counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - dev_weights[[i]])
    adjusted_cfacs = weight_counters(current_cfacs, global_params, counter_weights)
    
  } else if (adjusted_cfac_type == 'include_clearing_offsets'){
    
    dev_weights <- lapply(dev_probs, cumsum)
    
    offset_probs <- dev_probs
    offset_weights <- lapply(offset_probs, cumsum)
    
    counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - (dev_weights[[i]] + offset_weights[[i]]))
    weighted_counters = weight_counters(current_cfacs, global_params, counter_weights)
    
    summed_offset_projections = sum_offset_projections(current_parcel_ecologies, offset_probs, global_params, decline_rates, parcel_indexes, time_horizons)
    
    adjusted_cfacs = sum_clearing_offsets(weighted_counters, summed_offset_projections)
  }
  
  return(adjusted_cfacs)
  
}


# adjust_cfacs <- function(current_cfacs, adjusted_cfac_type, global_params, parcel_num_remaining, decline_rates, parcel_indexes, time_horizons, offset_yrs){
#   
#   dev_probs <- find_dev_probability(global_dev_vec = global_params$dev_vec, offset_yrs, time_horizons, parcel_num_remaining)
#   
#   a <- lapply(dev_probs, cumsum)
#   a <- lapply(seq_along(a), function(i) 1 - a[[i]])
#   
#   if (adjusted_cfac_type == 'include_clearing'){
#     counter_probs <- 1 - cumsum(dev_prob)
#     adjusted_cfacs = weight_counters(current_cfacs, global_params, counter_probs, time_horizon)
#   } else if (adjusted_cfac_type == 'include_clearing_offsets'){
#     offset_prob = dev_prob
#     counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
#     weighted_counters = weight_counters(current_cfacs, global_params, counter_probs, time_horizon)
#     summed_offset_projections = sum_offset_projections(current_cfacs, offset_prob, global_params, decline_rates, parcel_indexes, time_horizon)
#     adjusted_cfacs = sum_clearing_offsets(weighted_counters, summed_offset_projections)
#   }
#   
#   return(adjusted_cfacs)
#   
# }


find_weight_array <- function(current_counter_weight, projected_dims){
  counter_weight_array = rep(current_counter_weight, projected_dims[1]*projected_dims[2])
  dim(counter_weight_array) = c(length(current_counter_weight), c(projected_dims[2], projected_dims[1]))
  counter_weight_array = aperm(counter_weight_array, c(3, 2, 1))
  return(counter_weight_array)
}



weight_counters <- function(current_cfacs, global_params, counter_weights){
  
  parcel_num = length(current_cfacs)
  eco_dims = global_params$eco_dims
  weighted_counters = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    weighted_counters[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    current_counter_weight = counter_weights[[parcel_count_ind]]
    
    for (eco_ind in seq_len(eco_dims)){
      
      current_cfac = current_cfacs[[parcel_count_ind]][[eco_ind]]
      projected_dims = dim(current_cfac)
      counter_weight_array = find_weight_array(current_counter_weight, projected_dims)
      weighted_counters[[parcel_count_ind]][[eco_ind]] = counter_weight_array*current_cfac
      
    } 
    
  }
  
  return(weighted_counters)
  
}



sum_offset_projections <- function(current_parcel_ecologies, offset_probs, global_params, decline_rates, parcel_indexes, time_horizons){
  
  if (class(parcel_indexes) == 'list'){
    parcel_indexes = unlist(parcel_indexes)
  }
  parcel_num = length(parcel_indexes)
  eco_dims = global_params$eco_dims
  summed_offset_projections = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_count_ind]
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    current_offset_prob = offset_probs[[parcel_count_ind]]
    
    offset_proj_yrs = which(current_offset_prob > 0)
    offset_proj_num = length(offset_proj_yrs)
    
    current_parcel_ecology = current_parcel_ecologies[[parcel_count_ind]]
    
    current_summed_offset_projections = vector('list', eco_dims)
    for (eco_ind in seq_len(eco_dims)){
      projected_dims = c(dim(current_parcel_ecology)[1:2], length(current_offset_prob))
      current_summed_offset_projections[[eco_ind]] = array(0, projected_dims)
    }
    
    for (proj_ind in seq_len(offset_proj_num)){
      proj_yr = offset_proj_yrs[proj_ind]
      current_time_horizon = time_horizon - proj_yr + 1
      current_offset_proj = predict_parcel_traj(current_parcel_ecology, current_parcel_ind, parcel_traj_type = global_params$offset_action_type, 
                                                global_params, decline_rates, time_horizon = current_time_horizon)
      current_projected_dims = c(dim(current_parcel_ecology)[1:2], current_time_horizon + 1)
      #offset_weight_array = array(current_offset_prob[proj_yr], current_projected_dims)
      
      for (eco_ind in seq_len(eco_dims)){
        current_summed_offset_projections[[eco_ind]][, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[[eco_ind]][, , proj_yr:(time_horizon + 1)] + 
          as.vector(current_offset_proj[[eco_ind]]*current_offset_prob[proj_yr])
      }
      
    }
    
    summed_offset_projections[[parcel_count_ind]] = current_summed_offset_projections
    
  }
  
  return(summed_offset_projections)
}



# sum_offset_projections <- function(current_cfacs, offset_probs, global_params, decline_rates, parcel_indexes, time_horizon){
#   
#   parcel_num = length(current_cfacs)
#   eco_dims = global_params$eco_dims
#   summed_offset_projections = vector('list', parcel_num)
#   
#   for (parcel_count_ind in 1:parcel_num){
#     summed_offset_projections[[parcel_count_ind]] = vector('list', eco_dims)
#   }
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
# 
#     for (eco_ind in seq_len(eco_dims)){
#       current_dec_rate = find_decline_rate(decline_rates, parcel_indexes[parcel_count_ind], eco_ind)
#       current_cfac = current_cfacs[[parcel_count_ind]][[eco_ind]]
#       projected_dims = dim(current_cfac)
#       
#       current_summed_offset_projections = array(0, projected_dims)
#       
#       for (proj_yr in which(offset_prob > 0)){
#         
#         current_parcel_ecology = current_cfac[, , proj_yr]
#         current_offset_prob = offset_prob[proj_yr]
#         current_offset_projection = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = global_params$offset_action_type, global_params, current_dec_rate, time_horizon = (time_horizon - proj_yr + 1))
#         current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] 
#                                                                                   + current_offset_prob*current_offset_projection$projected
#       } 
#       summed_offset_projections[[parcel_count_ind]][[eco_ind]] = current_summed_offset_projections
#     } 
#     
#   }
#   
#   return(summed_offset_projections)
# }


sum_clearing_offsets <- function(cfacs_include_clearing, summed_offset_projections, eco_dims){
  parcel_num = length(cfacs_include_clearing)
  eco_dims = global_params$eco_dims
  cfacs_include_clearing_offsets = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    cfacs_include_clearing_offsets[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  for (parcel_count_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(eco_dims)){
      cfacs_include_clearing_offsets[[parcel_count_ind]][[eco_ind]] = summed_offset_projections[[parcel_count_ind]][[eco_ind]] + cfacs_include_clearing[[parcel_count_ind]][[eco_ind]]
    }
  }
  return(cfacs_include_clearing_offsets)
}









plot_parcel_set_parcels <- function(current_set_object){
  
  
  parcel_num = length(current_set_object$parcel_indexes)
  sub_plots = 1:(parcel_num*global_params$eco_dims)
  dim(sub_plots) = c(global_params$eco_dims, parcel_num)
  layout(sub_plots)
  
  rest_gains = current_set_object$rest_gains
  degs = current_set_object$avoided_degs
  lim_vec = c(rest_gains, degs, (rest_gains + degs)) 
  mx = max(lim_vec)
  mn = min(lim_vec)
  
  for (parcel_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(global_params$eco_dims)){
    plot(rest_gains[, eco_ind, parcel_ind], type = 'l', ylim = c(mn, mx), main = paste('parcel index =', current_set_object$parcel_indexes[parcel_ind]), ylab = '', xlab = paste('dim = ', eco_ind))
    lines(degs[, eco_ind, parcel_ind], col = 'blue')
    lines(rest_gains[, eco_ind, parcel_ind] + degs[, eco_ind, parcel_ind], col = 'red')
    }
  }
  
}


#collated_parcel_sets_object = realisations[[1]]$collated_parcel_sets_object 
# parcel_trajs = outs$parcel_trajs
# assessed_set_index = 5


plot_sample_parcel_sets <- function(collated_parcel_sets_object, plot_num, global_params){
  
  parcel_set_indexes = sample(global_params$total_dev_num, plot_num)
  setup_sub_plots(nx = 3, ny = 3, x_tit = TRUE)
  for (parcel_set_index in parcel_set_indexes){
    plot_parcel_set_from_collated_object(collated_parcel_sets_object, parcel_set_index, time_horizon = global_params$time_steps, global_params$eco_dims, 
                                         headings = c('Parcel Set Developments', 'Parcel Set Offsets', 'Parcel Set Outcome'))
  }
}




setup_sub_plots <- function(nx, ny, x_tit){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  if (x_tit == TRUE){
    x_space = 5
  } else {x_space = 2}
  
  par(mar = c(x_space, 1, 1, 0), oma = c(2, 4, 2.5, 0.5))

  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
}





plot_parcel_set_from_collated_object <- function(collated_parcel_sets_object, parcel_set_indexes, time_horizon, eco_dims, headings){
  
  collated_offsets = collated_parcel_sets_object$offsets
  collated_developments = collated_parcel_sets_object$devs
  collated_NNL = collated_parcel_sets_object$NNL_object
  
  NNL_yrs = collated_NNL$NNL_yrs[parcel_set_indexes]
  success_NNLs = which(NNL_yrs > 0)
  if (length(success_NNLs) > 0){
    success_NNL_yrs = NNL_yrs[success_NNLs]
    x_labs = c('', '', paste('NNL at', round(mean(success_NNL_yrs), 1), 'years'))
  } else {x_labs = c('', '', 'NNL fail')
  }
  
  plot_list = vector('list', 3)
  plot_list[[1]] = sum_parcel_sets(collated_developments$rest_gains, collated_developments$avoided_degs, collated_NNL$net_dev_gains, parcel_set_indexes, time_horizon, eco_dims)
  plot_list[[2]] = sum_parcel_sets(collated_offsets$rest_gains, collated_offsets$avoided_degs, collated_NNL$net_offset_gains, parcel_set_indexes, time_horizon, eco_dims)
  net_array = array(0, c(time_horizon, 3, eco_dims))
  net_array[, 1, ] = plot_list[[1]][, 3, ]
  net_array[, 2, ] = plot_list[[2]][, 3, ]
  net_array[, 3, ] = plot_list[[1]][, 3, ] + plot_list[[2]][, 3, ]
  plot_list[[3]] = net_array
  
  lim_vec = abind(plot_list[[1]][, , 1], plot_list[[2]][, , 1], plot_list[[3]][, , 1])
  mx = max(lim_vec)
  mn = min(lim_vec)
  overlay_plots_as_vector(plot_list[[1]][, , 1], yticks = 'y', axis_lab = TRUE, x_lab = x_labs[1], ylims = c(mn, mx), (heading = headings[1]), ylab = '', col_vec = c('red', 'red', 'red'), lty_vec = c(1, 2, 1), 
                lwd_vec = c(1, 1, 3), legend_vec = c('Parcel Set Restoration Gains', 'Parcel Set Avoided Degredation', 'Net Losses'), legend_loc = 'topleft')
  overlay_plots_as_vector(plot_list[[2]][, , 1], yticks = 'n', axis_lab = TRUE, x_lab = x_labs[2], ylims = c(mn, mx), (heading = headings[2]), ylab = '', col_vec = c('blue', 'blue', 'blue'), lty_vec = c(1, 2, 1), 
                lwd_vec = c(1, 1, 3), legend_vec = c('Parcel Set Restoration Gains', 'Parcel Set Avoided Degredation', 'Parcel Set Gains'), legend_loc = 'topleft')
  overlay_plots_as_vector(plot_list[[3]][, , 1], yticks = 'n', axis_lab = TRUE, x_lab = x_labs[3], ylims = c(mn, mx), (heading = headings[3]), ylab = '', col_vec = c('red', 'blue', 'black'), lty_vec = c(1, 1, 1), 
                lwd_vec = c(1, 1, 3), legend_vec = c('Development Losses', 'Offset Gains', 'Net Outcome'), legend_loc = 'topleft')
  
}




sum_parcel_sets_as_list <- function(rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon){
  summed_parcel_sets = vector('list', 3)
  summed_parcel_sets$rest_gains = sum_cols_multi(rest_gains[, parcel_set_indexes, ])
  summed_parcel_sets$avoided_degs = sum_cols_multi(avoided_degs[, parcel_set_indexes, ])
  summed_parcel_sets$net_gains = sum_cols_multi(net_gains[, parcel_set_indexes, ])
  return(summed_parcel_sets)
}



sum_parcel_sets <- function(rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon, eco_dims){
  summed_parcel_sets = array(0, c(time_horizon, 3, eco_dims))
  for (eco_ind in 1:eco_dims){
    summed_parcel_sets[, 1, ] = sum_cols_multi(rest_gains[, parcel_set_indexes, ])
    summed_parcel_sets[, 2, ] = sum_cols_multi(avoided_degs[, parcel_set_indexes, ])
    summed_parcel_sets[, 3, ] = sum_cols_multi(net_gains[, parcel_set_indexes, ])
  }
  return(summed_parcel_sets)
}

sum_cols_multi <- function(arr_to_use){
  
  if (length(dim(arr_to_use)) <= 1){
    
    arr_out = t(t(arr_to_use))
  } else if (length(dim(arr_to_use)) == 2){
    arr_out = apply(arr_to_use, MARGIN = 1, sum)
    arr_out = t(t(arr_out))
  } else if (length(dim(arr_to_use)) == 3){
    arr_out = apply(arr_to_use, MARGIN = c(1, 3), sum)
    dim(arr_out) = c(dim(arr_out), 1)
    arr_out = aperm(arr_out, c(1, 3, 2))
  }
  return(arr_out)
}






# find_parcel_traj_by_list <- function(trajectories_list, time_horizon, parcel_indexes){
#   
#   parcel_num = length(parcel_indexes)
#   parcel_trajs = array(0, c(time_horizon, parcel_num))
#   eco_ind = 1
#   for (parcel_count_ind in seq_len(parcel_num)){
#     parcel_ind = parcel_indexes[parcel_count_ind]
#     parcel_trajs[, parcel_count_ind] =  apply(trajectories_list[[parcel_ind]][[eco_ind]], MARGIN=3, sum)
#   }
#   
#   return(parcel_trajs)
# }





plot_net_parcel_sets <- function(collated_object, eco_dims, parcel_set_list){
  
  collated_offsets = collated_object$offsets
  collated_developments = collated_object$developments
  
  eco_ind = 1
  net_rest_gains = apply(collated_offsets$rest_gains[, , eco_ind], MARGIN = 1, sum)
  net_avoided_degs = apply(collated_object$avoided_degs[, , parcel_set_list], MARGIN = c(1, 2), sum)
  net_trajs = apply(collated_object$parcel_set_trajs[, , parcel_set_list], MARGIN = c(1, 2), sum)
  net_counters = apply(collated_object$parcel_set_counters[, , parcel_set_list], MARGIN = c(1, 2), sum)
  
  plot_list = cbind(net_rest_gains, net_avoided_degs, net_trajs, net_counters)
  mx = max(plot_list)
  mn = min(plot_list)
  
  for (eco_ind in seq_len(eco_dims)){ 
    plot(net_rest_gains[, eco_ind] , ylim = c(mn, mx), type = 'l', xlab = paste('dim = ', eco_ind), ylab = '', col = 'black')
    lines(net_avoided_degs[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'blue')
    lines(net_trajs[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'red')
    lines(net_counters[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'blue', lty = 2)
  }
}






sum_current_gains_degs <- function(object_to_collate, parcel_set_num, eco_dims){
  
  collate_dims = c(dim(object_to_collate)[1], parcel_set_num, eco_dims)
  collate_array = array(0, collate_dims)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    parcel_set_to_assess = current_assessed_object[[parcel_set_ind]]
    for (eco_ind in seq_len(eco_dims)){
      collate_array = apply(object_to_collate, MARGIN = 1, sum)
    }
  }
  
}




two_plot <- function(plot_a, plot_b, colours){
  mx = max(c(plot_a, plot_b))
  mn = min(c(plot_a, plot_b))
  plot(plot_a, type = 'l', col = colours[1], ylim = c(mn, mx))
  lines(plot_b, col = colours[2], ylim = c(mn, mx))
}


extract_3D_parcel <- function(current_parcel, trajectories){
  loc_1 = ind2sub(dim(trajectories)[1], current_parcel[1])
  loc_2 = ind2sub(dim(trajectories)[1], current_parcel[length(current_parcel)])
  parcel_sz = c((loc_2[1] - loc_1[1] + 1), (loc_2[2] - loc_1[2] + 1), dim(trajectories)[3])
  parcel_3D = array(0, parcel_sz)
  parcel_3D[, ,] = trajectories[loc_1[1]:loc_2[1], loc_1[2]:loc_2[2], ]
  return(parcel_3D)
}

insert_parcel_trajectory <- function(trajectories, current_parcel, parcel_3D){
  loc_1 = ind2sub(dim(trajectories)[1], current_parcel[1])
  loc_2 = ind2sub(dim(trajectories)[1], current_parcel[length(current_parcel)])
  parcel_sz = c((loc_2[1] - loc_1[1] + 1), (loc_2[2] - loc_1[2] + 1), dim(trajectories)[3])
  trajectories[loc_1[1]:loc_2[1], loc_1[2]:loc_2[2], ] = parcel_3D 
  return(trajectories)
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



#wrap realisations up into usable form for plotting

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



# 
# 
# collate_realisations <- function(realisations, parcel_set_indexes, time_horizon, eco_dims){
#   
#   collated_program_cfacs <- collate_program_cfac_sum(realisations, time_horizon, eco_dims)    #group individual net realisation counterfactual values
#   
#   outs = list()
#   summed_dev_realisations = list()
#   
#   realisation_num = length(realisations)
#   collate_array = array(0, c(time_horizon, realisation_num, eco_dims))
#   summed_dev_realisations$net_gains = collate_array
#   summed_dev_realisations$avoided_degs = collate_array
#   summed_dev_realisations$rest_gains = collate_array
#   net_realisations = collate_array
#   net_parcel_set_realisations = collate_array
#   
#   summed_offset_realisations = summed_dev_realisations
#   
#   summed_program_realisations = list()
#   summed_program_realisations$net_program_value = collate_array
#   summed_program_realisations$net_offset_value = collate_array
#   summed_program_realisations$net_development_value = collate_array
#   
#   program_cfac_realisations = collate_array
#   
#   rm(collate_array)
#   
#   
#   
#   
#   system_NNLs = array(0, c(realisation_num, eco_dims))
#   collate_array = array(0, c(length(parcel_set_indexes), realisation_num))
#   ALL_parcel_set_NNLs = collate_array
#   ALL_offset_parcel_sums_at_offset = collate_array
#   rm(collate_array)
#   
#   eco_ind = 1
#   
#   for (realisation_ind in seq_len(realisation_num)){
#     current_realisation = realisations[[realisation_ind]]
#     collated_object = current_realisation$collated_parcel_sets_object
#     
#     current_summed_devs = sum_parcel_sets_as_list(collated_object$devs$rest_gains, collated_object$devs$avoided_degs, collated_object$NNL_object$net_dev_gains, parcel_set_indexes, time_horizon)
#     current_summed_offsets = sum_parcel_sets_as_list(collated_object$offsets$rest_gains, collated_object$offsets$avoided_degs, collated_object$NNL_object$net_offset_gains, parcel_set_indexes, time_horizon)
#     summed_dev_realisations = write_realisation_sums(summed_dev_realisations, realisation_ind, current_summed_devs)
#     summed_offset_realisations = write_realisation_sums(summed_offset_realisations, realisation_ind, current_summed_offsets)
#     summed_program_realisations = write_summed_programs(summed_program_realisations, realisation_ind, collated_object$summed_program_parcels)
#     
#     net_parcel_set_realisations[, realisation_ind, ] = sum_cols_multi(collated_object$NNL_object$net_gains[, parcel_set_indexes, ])
#     net_realisations[, realisation_ind, ] = apply(current_realisation$model_outputs$trajectories[[eco_ind]], MARGIN = 3, sum)
#     if (length(collated_object$NNL_object$system_NNL) > 0){
#       system_NNLs[realisation_ind, ] = collated_object$NNL_object$system_NNL
#     }
#     ALL_parcel_set_NNLs[, realisation_ind] = collated_object$NNL_object$NNL_yrs
#     ALL_offset_parcel_sums_at_offset[, realisation_ind] = collated_object$offsets$parcel_sums_at_offset
#   }
#   
#   system_NNL_fails = which(system_NNLs == 0)
#   
#   outs$collated_net_cfacs = collated_net_cfacs
#   outs$system_NNL_fails = system_NNL_fails
#   outs$ALL_offset_parcel_sums_at_offset = ALL_offset_parcel_sums_at_offset
#   outs$ALL_parcel_set_NNLs = ALL_parcel_set_NNLs
#   outs$system_NNLs = system_NNLs
#   outs$summed_dev_realisations = summed_dev_realisations
#   outs$summed_offset_realisations = summed_offset_realisations
#   outs$net_parcel_set_realisations = net_parcel_set_realisations
#   outs$net_realisations = net_realisations
#   outs$summed_program_realisations = summed_program_realisations
#   return(outs)
#   
# }

# collate_parcel_realisations_multi <- function(record_parcel_sets, realisations, parcel_ind, land_parcels, global_params, eco_dims){
#   realisation_num = length(realisations)
#   current_parcel = select_land_parcel(land_parcels, parcel_ind)
#   
#   parcel_realisations = list()
#   parcel_realisations$parcel_3D = vector('list', realisation_num)
#   parcel_realisations$sums = array(0, c(global_params$time_steps, realisation_num, eco_dims))
#   
#   for (realisation_ind in seq_len(realisation_num)){
#     for (eco_ind in seq_len(eco_dims)){
#       if (record_parcel_sets == FALSE){
#         current_realisation = realisations[[realisation_ind]]
#       } else current_realisation = (realisations[[realisation_ind]]$trajectories)
#     
#       current_parcel_3D = extract_3D_parcel(current_parcel, current_realisation[[eco_ind]])
#       parcel_realisations$parcel_3D[[realisation_ind]][[eco_ind]] = current_parcel_3D
#       parcel_realisations$sums[, realisation_ind, eco_ind] = apply(current_parcel_3D, 3, sum)
#     }
#   }
#   return(parcel_realisations)
# }





tally_devs_offsets <- function(realisations, parcels, global_params){
  
  tally_object = list()
  realisation_num = length(realisations)
  parcel_num = length(parcels$land_parcels)
  offset_tally = array(0, c(global_params$time_steps, parcel_num))
  development_tally = array(0, c(global_params$time_steps, parcel_num))
  
  for (realisation_ind in seq_len(realisation_num)){
    current_realisation = realisations[[realisation_ind]]
    current_dev_num = length(current_realisation$development_list)
    for (parcel_set_ind in seq_len(current_dev_num)){
      yr = current_realisation$developments[[parcel_set_ind]]$offset_yrs
      current_offset_inds = current_realisation$offsets[[parcel_set_ind]]$parcel_indexes
      current_dev_inds = current_realisation$developments[[parcel_set_ind]]$parcel_indexes
      offset_tally[yr, current_offset_inds] = offset_tally[yr, current_offset_inds] + 1
      development_tally[yr, current_dev_inds] = development_tally[yr, current_dev_inds] + 1
    }
  }
  
  tally_object$offset_tally = offset_tally
  tally_object$development_tally = development_tally
  
  return(tally_object)
  
}
  






sum_parcels_multi <- function(trajectories, parcel_indexes_to_use, land_parcels, global_params, eco_dims){
  
  parcel_num = length(parcel_indexes_to_use)
  parcel_sums = array(0, c(global_params$time_steps, parcel_num, eco_dims))
  
  for (parcel_ind in seq_len(parcel_num)){   
    current_parcel_ind = parcel_indexes_to_use[parcel_ind]
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_trajectory = extract_3D_parcel(current_parcel, trajectories[[eco_ind]])
      parcel_sums[, parcel_ind, eco_ind] = apply(current_parcel_trajectory, 3, sum)
    }  
  }
  
  return(parcel_sums)  
}

sum_regions <- function(parcel_sums, parcel_index_list, regions, global_params){
  region_num = length(regions)
  region_sums = array(0, c(global_params$time_steps, region_num))
  parcel_num = length(parcel_index_list)
  
  for (parcel_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_index_list[parcel_ind]
    region_ind = find_region(current_parcel_ind, regions)
    region_sums[, region_ind] = region_sums[, region_ind] + parcel_sums[, parcel_ind]
  }
  
  return(region_sums)
}

find_region <- function(parcel, regions){
  
  for (region_ind in seq_len(length(regions))){
    if (is.element(parcel, regions[[region_ind]])){
      return(region_ind)
    }
  }
}


true_sums <- function(parcel_indexes, object_sums, counter_sums){
  true_sums_object = list()
  true_sums_object$parcel_sums = object_sums$parcel_sums - counter_sums$parcel_sums[, parcel_indexes]
  true_sums_object$net_sums = object_sums$net_sums - counter_sums$net_sums
  return(true_sums_object)
}


find_sums <- function(trajectories, parcels_inds, parcels, global_params){
  object = list()
  object$parcel_sums = sum_parcels(trajectories, parcel_indexes, parcels$land_parcels, time_steps)
  object$region_sums = sum_regions(object$parcel_sums, parcel_indexes, parcels$regions, time_steps)
  object$net_sums = rowSums(object$region_sums)
  return(object)
}



overlay_plots_as_list <- function(plot_list, yticks, axis_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (yticks == 'y'){
    plot(plot_list[[1]], axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  } else if ((yticks == 'n')){
    plot(plot_list[[2]], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  }
  for (plot_ind in 2:length(plot_list)){
    lines(plot_list[[plot_ind]],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
  }
  abline(h = 0, lty = 2)
  legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
}

overlay_plots_as_vector <- function(plot_array, yticks, axis_lab, x_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (yticks == 'y'){
    plot(plot_array[, 1], axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  } else if ((yticks == 'n')){
    plot(plot_array[, 1], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  }
  for (plot_ind in 2:dim(plot_array)[2]){
    lines(plot_array[, plot_ind],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
  }
  legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
  abline(h = 0, lty = 2)
  
}



cartesian_mesh <- function(N, M){
  mesh = list()
  xx = seq(-floor(M/2), (ceiling(M/2) - 1))
  yy = seq(-floor(N/2), (ceiling(N/2) - 1))
  x = matrix(rep(xx,each=N),nrow=N);
  y = matrix(rep(yy,M),nrow=N)
  mesh$x = x
  mesh$y = y
  return(mesh)
#  m=length(x); n=length(y);
#  X=matrix(rep(x,each=n),nrow=n);
#  Y=matrix(rep(y,m),nrow=n)
}
  
gauss <- function(x, y, sig_x, sig_y){
  g = exp(-x^2/(sig_x^2)) * exp(-y^2/(sig_y^2))
  return(g)
}


fftshift <- function(fft_array){
  numDims = 2
  idx <- vector('list', numDims)
  for (k in seq_len(numDims)){
    m = dim(fft_array)[k];
    p = ceiling(m/2);
    idx[[k]] = c((p+1):m, (1:p));
  }
  fft_array = fft_array[idx[[1]], ]
  fft_array = fft_array[, idx[[2]]]
  return(fft_array)
}


Blur_2D <- function(A, sig_x, sig_y){
  dims = dim(A)
  M = dims[1]
  N = dims[2]
  mesh = cartesian_mesh(M, N)
  knl = gauss(mesh$x, mesh$y, sig_x, sig_y)
  knl = knl / sum(knl)
  convolve = fftshift( fft( fftshift(knl), inverse = TRUE )) * fftshift( fft( fftshift(A), inverse = TRUE ) )
  A = (fftshift( fft( fftshift( convolve ) ) ))/(M*N)
  A = Re(A)
  return(A)
}






# plot_parcel_set_from_collated_object <- function(collated_object, parcel_set_indexes, global_params, eco_dims){
# 
#   collated_offsets = collated_object$offsets
#   collated_developments = collated_object$developments
#   
#   plot_list = vector('list', 3)
#   plot_list[[1]] = sum_parcel_sets(collated_developments, parcel_set_indexes, global_params, eco_dims)
#   plot_list[[2]] = sum_parcel_sets(collated_offsets, parcel_set_indexes, global_params, eco_dims)
#   net_array = array(0, c(global_params$time_steps, 3, eco_dims))
#   net_array[, 1, ] = plot_list[[1]][, 3, ]
#   net_array[, 2, ] = plot_list[[2]][, 3, ]
#   net_array[, 3, ] = plot_list[[1]][, 3, ] + plot_list[[2]][, 3, ]
#   plot_list[[3]] = net_array
#     
#   lim_vec = abind(plot_list[[1]][, , 1], plot_list[[2]][, , 1], plot_list[[3]][, , 1])
#   mx = max(lim_vec)
#   mn = min(lim_vec)
#   overlay_plots_as_vector(plot_list[[1]][, , 1], yticks = 'y', axis_lab = TRUE, ylims = c(mn, mx), (heading = "developments"), ylab = '', col_vec = c('red', 'red', 'red'), lty_vec = c(1, 2, 1), 
#                 lwd_vec = c(1, 1, 3), legend_vec = c('Net Restoration Gains', 'Net Avoided Degredation', 'Net Losses'), legend_loc = 'topleft')
#   overlay_plots_as_vector(plot_list[[2]][, , 1], yticks = 'n', axis_lab = TRUE, ylims = c(mn, mx), (heading = "offsets"), ylab = '', col_vec = c('blue', 'blue', 'blue'), lty_vec = c(1, 2, 1), 
#                 lwd_vec = c(1, 1, 3), legend_vec = c('Net Restoration Gains', 'Net Avoided Degredation', 'Net Gains'), legend_loc = 'topleft')
#   overlay_plots_as_vector(plot_list[[3]][, , 1], yticks = 'n', axis_lab = TRUE, ylims = c(mn, mx), (heading = "net outcome"), ylab = '', col_vec = c('red', 'blue', 'black'), lty_vec = c(1, 1, 1), 
#                 lwd_vec = c(1, 1, 3), legend_vec = c('Net Losses', 'Net Gains', 'Net Outcome'), legend_loc = 'topleft')
#   
# }





# calc_rel_intial <- function(trajectory_type, global_params, parcel_trajectory, yr, parcel_ecology){
#   rel_arr = array(0, c(dim(parcel_trajectory)[1:2], global_params$time_steps))
#   rel_arr[, , yr:global_params$time_steps] = parcel_trajectory - as.vector(parcel_ecology)
#   if (trajectory_type == 'cfac'){
#     rel_arr = -rel_arr
#   }
#   rel_arr = apply(rel_arr, 3, sum)
# }
# 







# collate_parcel_set_element <- function(global_params, region_params, current_parcel_set, current_cfac_set, adjusted_cfac_set, land_parcels, trajectories, decline_rates){
#   
#   parcel_set_element = list()
#   
#   yr = current_parcel_set$offset_yrs
#   parcel_indexes = current_parcel_set$parcel_indexes
#   parcel_ecologies_list = current_parcel_set$parcel_ecologies
#   parcel_num = length(parcel_indexes)
#   
#   collate_array = array(0, c(global_params$time_steps, parcel_num, global_params$eco_dims))
#   avoided_degs = collate_array
#   avoided_degs_include_clearing = collate_array
#   avoided_degs_include_clearing_offsets = collate_array
#   rest_gains = collate_array
#   
#   parcel_sums_at_offset = array(0, c(parcel_num, global_params$eco_dims))
#   
#   time_horizon = global_params$time_steps - yr 
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     current_parcel = select_land_parcel(land_parcels, current_parcel_ind = parcel_indexes[parcel_count_ind])
#     
#     for (eco_ind in seq_len(global_params$eco_dims)){
#       current_parcel_trajectory = extract_3D_parcel(current_parcel, trajectories[[eco_ind]][, , yr:global_params$time_steps])
#       parcel_ecology = parcel_ecologies_list[[parcel_count_ind]][, , eco_ind] 
#       rest_gains[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'restoration', global_params, current_parcel_trajectory, yr, parcel_ecology)
#       avoided_degs[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'cfac', global_params, current_cfac_set[[parcel_count_ind]][[eco_ind]], yr, parcel_ecology)
#       if (global_params$adjust_counters_post_facto == TRUE){
#         avoided_degs_include_clearing[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'cfac', global_params, adjusted_cfac_set$include_clearing[[parcel_count_ind]][[eco_ind]], yr, parcel_ecology)
#         avoided_degs_include_clearing_offsets[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'cfac', global_params, adjusted_cfac_set$include_clearing_offsets[[parcel_count_ind]][[eco_ind]], yr, parcel_ecology)
#       }
#       parcel_sums_at_offset[parcel_count_ind, eco_ind] = sum(parcel_ecology)
#     }
#   }
#   
#   parcel_set_element$offset_yrs = yr
#   parcel_set_element$rest_gains = rest_gains
#   parcel_set_element$avoided_degs = avoided_degs
#   parcel_set_element$parcel_indexes = parcel_indexes
#   parcel_set_element$parcel_vals_used = current_parcel_set$parcel_vals_used
#   parcel_set_element$parcel_sums_at_offset = parcel_sums_at_offset
#   parcel_set_element$avoided_degs_include_clearing = avoided_degs_include_clearing
#   parcel_set_element$avoided_degs_include_clearing_offsets = avoided_degs_include_clearing_offsets
#   return(parcel_set_element)
#   
# }


# 
# 
# outputs = outs$model_outputs
# land_parcels = parcels$land_parcels
# trajectories = outputs$trajectories
# time_steps = global_params$time_steps
# parcel_set_ind = 1
# current_sets_object = outputs$offsets
# current_parcel_set = current_sets_object[[parcel_set_ind]]
# decline_rates = decline_rates_initial
# parcel_count_ind = 1
# parcel_sets_cfacs = outs$parcel_sets_cfacs
# current_cfac_sets_object = parcel_sets_cfacs$offsets

# 
# group_parcel_sets <- function(outputs, parcel_sets_cfacs, land_parcels, global_params, decline_rates){
#   parcel_sets_object = list()
#   
#   parcel_sets_object$offsets = find_parcel_sets(outputs$offsets, parcel_sets_cfacs$offsets, land_parcels, outputs$trajectories, global_params, decline_rates)
#   parcel_sets_object$developments = find_parcel_sets(outputs$developments, parcel_sets_cfacs$developments, land_parcels, outputs$trajectories, global_params, decline_rates)
#   return(parcel_sets_object)
# }
# 
# 
# find_parcel_sets <- function(current_sets_object, current_cfac_sets_object, land_parcels, trajectories, global_params, decline_rates){
#   
#   parcel_set_num = length(current_sets_object)
#   parcel_set_object = vector('list', parcel_set_num)
#   
#   for (parcel_set_ind in seq_len(parcel_set_num)){
#     current_parcel_set = current_sets_object[[parcel_set_ind]]
#     current_cfac_set = current_cfac_sets_object$cfacs[[parcel_set_ind]]
#     if (global_params$adjust_counters_post_facto == TRUE){
#       adjusted_cfac_set = current_cfac_sets_object$adjusted_cfacs[[parcel_set_ind]]
#     }
#     parcel_set_object[[parcel_set_ind]] = collate_parcel_set_element(global_params, region_params, current_parcel_set, current_cfac_set, adjusted_cfac_set, land_parcels, trajectories, decline_rates)
#   }
#   
#   return(parcel_set_object)
#   
# }  






# eco_shift <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_horizon){
#   
#   t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
#   eco_shift = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_horizon - t_sh)))
#   return(eco_shift)
# }




# 
# initialise_ecology_by_parcel <- function(global_params, parcels){
#   land_parcels = parcels$land_parcels
#   land_parcel_num = parcels$land_parcel_num
#   initial_ecology = vector('list', land_parcel_num)
#   sc = global_params$max_initial_eco_vals - global_params$min_initial_eco_vals - global_params$initial_eco_noise
#   
#   min_parcel_vals = runif(2, min = global_params$min_eco_val, max = (global_params$max_eco_val - global_params$initial_eco_noise))
#   for (parcel_ind in seq_len(land_parcel_num)){
#     current_parcel = land_parcels[[parcel_ind]]
#     parcel_length = length(current_parcel)
#     current_eco_array = global_params$initial_eco_noise*array(runif(parcel_length*eco_dims), c(dim(current_parcel), eco_dims))
#     sc_array = rep(global_params$min_initial_eco_vals, parcel_length)
#     dim(sc_array) = c(eco_dims, parcel_length, 1)
#     sc_array = aperm(sc_array, c(3, 2, 1))
#     dim(sc_array) = dim(current_eco_array)
#     
#     for (eco_ind in 1:eco_dims){
#       current_eco_slice = current_eco_array[, , eco_dim] * rep(global_params$min_initial_eco_vals[eco_ind], length(current_parcel))
#        rep(global_params$min_initial_eco_vals[eco_ind], length(current_parcel))
#   #  min_parcel_vals = global_params$min_initial_eco_val[eco_ind] + runif(land_parcel_num)*(global_params$max_initial_eco_val[eco_ind] - global_params$min_initial_eco_val[eco_ind] - global_params$initial_eco_noise[eco_ind])
#     
#       
#   #    initial_parcel_ecology = min_parcel_vals[parcel_ind] + global_params$initial_eco_spread*runif(length(current_parcel))
#   #    dim(initial_parcel_ecology) = dim(current_parcel)
#   #    initial_ecology[[parcel_ind]][, , eco_ind] = initial_parcel_ecology
#   }
#  # initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size),global_params$ecology_size,global_params$ecology_size)
#   return(initial_ecology)
# }




# build_cfacs_by_parcel <- function(global_params, decline_rates, land_parcels, current_ecology){
#   
#   cfacs = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
#   parcel_num = length(land_parcels)
#   
#   for (parcel_ind in seq_len(parcel_num)){
#     
#     current_parcel = land_parcels[[parcel_ind]]
#     current_dec_rate = decline_rates[parcel_ind]  
#     current_parcel_ecology = initial_ecology[current_parcel]
#     dim(current_parcel_ecology) = dim(current_parcel)
#     projected_ecology = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = current_dec_rate, (time_horizon = global_params$time_steps - 1))
#     cfacs = insert_parcel_trajectory(cfacs, current_parcel, projected_ecology)
#     
#   }
#   
#   return(cfacs)
# }



# build_decline_rates <- function(parcels, region_params){
#   regions = parcels$regions
#   region_num = length(regions)
# #  region_dec_rates = vector('list', region_num)
#   decline_rates = array(0, dim(parcels$parcel_indexes))
#   
#   for (region_ind in seq_len(region_num)){ 
#     current_region = regions[[region_ind]]
#     current_parcel_num = length(current_region)    
#     decline_params = c(length(current_region), region_params[[region_ind]]$mean_decline_rate, region_params[[region_ind]]$decline_rate_std) #params$decline_rate_std[region_ind])
#     current_decline_rates = matrix(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), ncol = ncol(current_region))
# #    dec_rates[[region_ind]] = current_decline_rates
#     decline_rates[current_region] = current_decline_rates
#   }
#   
#   return(decline_rates)
# }




# adjust_counters <-function(cfacs, region_params, global_params, parcel_num, time_horizon, yr){
#   
#   eco_dims = global_params$eco_dims
#   rec_list = vector('list', parcel_num)
#   
#   for (parcel_count_ind in 1:parcel_num){
#     rec_list[[parcel_count_ind]] = vector('list', eco_dims)
#   }
#   
#   summed_offset_projections = rec_list
#   adjusted_counters = rec_list
#   
#   dev_vec = global_params$dev_vec
#   dev_prob <- find_dev_probability(global_params$dev_vec, yr, time_horizon, parcel_num)
#   
#   if (global_params$cfac_type_in_offset_calc == 'include_clearing_offsets'){
#     offset_prob = dev_prob
#   } else {offset_prob = 0}
#   
#   counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
#   eco_ind = 1
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     
#     current_cfac = cfacs[[parcel_count_ind]][[eco_ind]]
#     projected_dims = dim(current_cfac)
#     
#     if (cfac_type == 'include_clearing_offsets'){
#       current_summed_offset_projections = array(0, projected_dims)
#       
#       for (proj_yr in which(dev_vec > 0)){
#         
#         current_parcel_ecology = current_cfac[, , proj_yr]
#         current_offset_prob = offset_prob[proj_yr]
#         current_offset_projection = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = global_params$restoration_rate, time_horizon = (time_horizon - proj_yr))
#         current_summed_offset_projections[, , proj_yr:time_horizon] = current_summed_offset_projections[, , proj_yr:time_horizon] + current_offset_prob*current_offset_projection
#         
#       } 
#       
#     } else {
#       current_summed_offset_projections = 0
#     }
#     
#     summed_offset_projections[[parcel_count_ind]][[eco_ind]] = current_summed_offset_projections
#     
#     c_probs = rep(counter_probs, projected_dims[1]*projected_dims[2])
#     dim(c_probs) = c(length(counter_probs), c(projected_dims[2], projected_dims[1]))
#     c_probs = aperm(c_probs, c(3, 2, 1))
#     adjusted_counters[[parcel_count_ind]][[eco_ind]] = c_probs*current_cfac + current_summed_offset_projections
#     
#   }
#   
#   return(adjusted_counters)
#   
# }


#parcel_num_remaining = (current_parcel_set$parcel_num_remaining)








# find_summed_offset_projections <- function(projected_dims, current_cfac, offset_prob, global_params, time_horizon){
# 
#     current_summed_offset_projections = array(0, projected_dims)
#     
#     for (proj_yr in which(offset_prob > 0)){
#       
#       current_parcel_ecology = current_cfac[, , proj_yr]
#       current_offset_prob = offset_prob[proj_yr]
#       current_offset_projection = predict_parcel_traj(current_parcel_ecology, parcel_traj_type, global_params, current_dec_rate, time_horizon = (time_horizon - proj_yr + 1))
#       current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] + current_offset_prob*current_offset_projection
#     } 
#     return(current_summed_offset_projections)
#     
# }




# write_offset <- function(offset_object, current_ecology, min_eco_val, max_eco_val, ecology_size, restoration_rate, yr){
#   current_parcels = offset_object$current_parcels
#   parcel_num = length(offset_object$current_parcels)
#   for (parcel_ind in seq_len(parcel_num)){
#     current_parcel = current_parcels[[parcel_ind]]
#     updated_parcel = sapply(current_ecology[current_parcel], project_ecology, min_eco_val = min_eco_val, max_eco_val = max_eco_val, decline_rate = restoration_rate, time_horizon = 1, time_fill = 'none')
#     current_ecology[current_parcel] = updated_parcel 
#   }
#   return(trajectories)
# }



# find_parcel_traj_from_trajectories <- function(land_parcels, parcel_ind, trajectories){
#   time_horizon = dim(trajectories)[3]
#   current_parcel = select_land_parcel(land_parcels, parcel_ind)
#   parcel_traj = array(0, time_horizon)
#   
#   for (yr in seq_len(time_horizon)){ #determine net regional offsets, net regional development_losses
#     current_slice = trajectories[ , , yr]
#     parcel_traj[yr] = sum(current_slice[current_parcel])
#   }
#   return(parcel_traj)
# }


# land_parcels = parcels$land_parcels
# parcel_indexes = 1:parcels$land_parcel_num
# time_steps = global_params$time_steps
# trajectories = outputs$trajectories[[1]]



# find_parcel_trajectories <- function(land_parcels, parcel_indexes, trajectories){
#   time_horizon = dim(trajectories)[3]
#   parcel_trajs = array(0, c(time_horizon, length(parcel_indexes)))
#   
#   for (parcel_ind in seq_len(length(parcel_indexes))){
#     current_ind = parcel_indexes[parcel_ind]
#     parcel_trajs[, parcel_ind] = find_parcel_traj_from_trajectories(land_parcels, current_ind, trajectories)
#   }
#   
#   if (length(parcel_indexes) == 1){
#     dim(parcel_trajs) = c(length(parcel_trajs), 1)
#   }
#   
#   return(parcel_trajs)
# }




# calc_counters_post_facto <- function(current_sets_object, global_params, decline_rates, parcel_num_remaining){
#   
#   parcel_set_num = length(current_sets_object)
#   adjusted_cfacs = vector('list', parcel_set_num)
#   cfacs = vector('list', parcel_set_num)
#   
#   for (parcel_set_ind in seq_len(parcel_set_num)){
#     current_parcel_set = current_sets_object[[parcel_set_ind]]
#     yr = current_parcel_set$offset_yrs
#     time_horizon = global_params$time_steps - yr
#     parcel_indexes = current_parcel_set$parcel_indexes
#     current_cfacs = build_cfacs_list(global_params, decline_rates, (parcel_ecologies_list = current_parcel_set$parcel_ecologies), parcel_indexes, time_horizons)
#     cfacs[[parcel_set_ind]] = current_cfacs
#     if (global_params$adjust_counters_post_facto == TRUE){
#       current_adjusted_cfac = list()
#       current_adjusted_cfac$include_clearing = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing', global_params, 
#                                                             parcel_num_remaining[parcel_set_ind], decline_rates, parcel_indexes, time_horizon, yr)
#       current_adjusted_cfac$include_clearing_offsets = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing_offsets', global_params, 
#                                                                     parcel_num_remaining[parcel_set_ind], decline_rates, parcel_indexes, time_horizon, yr)
#       adjusted_cfacs[[parcel_set_ind]] = current_adjusted_cfac
#     }
#   }
#   
#   counters_object = list()
#   counters_object$cfacs = cfacs
#   counters_object$adjusted_cfacs = adjusted_cfacs
#   return(counters_object)
# }  






#   
#   
# 
# plot_parcel_sums <- function(plot_type, assess_type, assess_object, parcel_set_num){
#   
#   if (plot_type == 'offsets'){
#     plot_object = assess_object$offsets
#   } else if(plot_type == 'developments') {
#     plot_object = assess_object$developments
#   }
#   
#   
#   if (assess_type == 'set'){
#     plot_1a = plot_object$net_parcel_set[, parcel_set_num]
#     plot_1b = plot_object$net_counter_set[, parcel_set_num]
#     plot_1c = plot_object$parcel_sums_at_offset_set[parcel_set_num]*array(1, length(plot_1a))
#     plot_2a = plot_object$rest_gains_set[, parcel_set_num]
#     plot_2b = plot_object$avoided_degs_set[, parcel_set_num]
# 
#   } else if(assess_type == 'individual'){
#     plot_1a = plot_object$net_parcel[, parcel_set_num]
#     plot_1b = plot_object$net_counter[, parcel_set_num]
#     plot_1c = plot_object$parcel_sums_at_offset[parcel_set_num]*array(1, length(plot_1a))
#     plot_2a = plot_object$rest_gains[, parcel_set_num]
#     plot_2b = plot_object$avoided_degs[, parcel_set_num]
#   }
#  
#   mx = max(cbind(plot_1a, plot_1b))
#   mn = min(cbind(plot_1a, plot_1b))
#   plot(plot_1a, ylim = c(mn, mx), type = 'l', xlab="time", ylab="trajectory", col = 'red')
#   lines(plot_1b, ylim = c(mn, mx), col = 'blue')
#   lines(plot_1c, ylim = c(mn, mx), lty = 2)
#   
#   
#   
#   plot_2c = plot_2a + plot_2b
#   mx = max(cbind(plot_2a, plot_2b, plot_2c))
#   mn = min(cbind(plot_2a, plot_2b, plot_2c))
#   plot(plot_2a, ylim = c(mn, mx), type = 'l', xlab="time", ylab="split")
#   lines(plot_2b, ylim = c(mn, mx), col = 'blue')
#   lines(plot_2c, ylim = c(mn, mx), col = 'red')
#   
# }


# 
# 
# plot_parcel_sets <- function(assess_object, assess_type, parcel_set_num){
#  
#   
#   sub_plots = 1:6
#   dim(sub_plots) = c(2, 3)
#   layout(sub_plots)
#   plot_parcel_sums(plot_type = 'developments', assess_type, assess_object, parcel_set_num)
#   plot_parcel_sums(plot_type = 'offsets', assess_type, assess_object, parcel_set_num)
#   
#   plot_a = assess_object$developments$rest_gains_set[, parcel_set_num] + assess_object$developments$avoided_degs_set[, parcel_set_num]
#   plot_b = assess_object$offsets$rest_gains_set[, parcel_set_num] + assess_object$offsets$avoided_degs_set[, parcel_set_num]
#   two_plot(plot_a, plot_b, cols = c('red', 'black'))
#   lines((plot_a + plot_b), col = 'blue')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
# }
# 
# 
# 
# plot_net_parcel_sets <- function(assess_object){
#   
#   sub_plots = 1:3
#   dim(sub_plots) = c(1, 3)
#   layout(sub_plots)
#   
#   plot_a = rowSums(assess_object$developments$rest_gains_set) 
#   plot_b = rowSums(assess_object$developments$avoided_degs_set)
#   plot_c = rowSums(assess_object$offsets$rest_gains_set) 
#   plot_d = rowSums(assess_object$offsets$avoided_degs_set)
#   
#   two_plot(plot_a, plot_b, cols = c('blue', 'black'))
#   lines((plot_a + plot_b), col = 'red')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
#   two_plot(plot_c, plot_d, cols = c('blue', 'black'))
#   lines((plot_c + plot_d), col = 'red')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
# 
#   two_plot(plot_a + plot_b, plot_c + plot_d, cols = c('blue', 'black'))
#   lines((plot_a + plot_b) + (plot_c + plot_d), col = 'red')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
# }

