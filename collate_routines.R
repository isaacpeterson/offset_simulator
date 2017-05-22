prepare_realisations <- function(realisations){   #remove unsuccessful offset programs
  offset_success_flag = unlist(lapply(seq_along(realisations), function(i) realisations[[i]]$offset_success_flag))
  success_inds = which(offset_success_flag == TRUE)
  realisations <- lapply(success_inds, function(i) realisations[[i]])
  return(realisations)
}

 
# global_params = sim_group$global_params 
# program_params = sim_group$program_params_to_use 
# use_cfac_type_in_sim = TRUE 
# decline_rates_initial = sim_group$decline_rates_initial 
# parcels = sim_group$parcels 
# initial_ecology = sim_group$initial_ecology

collate_realisations <- function(realisations, global_params, program_params, use_cfac_type_in_sim, decline_rates_initial, parcels, initial_ecology){
  
  parcel_nums_used = assess_parcels_used(realisations)
  
  collated_realisations = list()
  
  realisation_num = length(realisations)
  
 
  collated_offsets <- run_collate_routine(realisations,
                                          initial_ecology, 
                                          parcels, 
                                          realisation_num = length(realisations),
                                          collate_type = 'offsets', 
                                          global_params, 
                                          program_params, 
                                          use_cfac_type_in_sim, 
                                          decline_rates_initial)
  
  collated_devs = run_collate_routine(realisations, 
                                      initial_ecology, 
                                      parcels, 
                                      realisation_num = length(realisations), 
                                      collate_type = 'devs', 
                                      global_params, 
                                      program_params, 
                                      use_cfac_type_in_sim, 
                                      decline_rates_initial)
  
  collated_dev_credit = run_collate_routine(realisations, 
                                            initial_ecology, 
                                            parcels, 
                                            realisation_num = length(realisations), 
                                            collate_type = 'dev_credit', 
                                            global_params, 
                                            program_params, 
                                            use_cfac_type_in_sim, 
                                            decline_rates_initial)
  
  collated_offset_bank = run_collate_routine(realisations,
                                             initial_ecology,  
                                             parcels, 
                                             realisation_num = length(realisations), 
                                             collate_type = 'offset_bank', 
                                             global_params, 
                                             program_params, 
                                             use_cfac_type_in_sim, 
                                             decline_rates_initial)
  
  collated_illegal_clearing = run_collate_routine(realisations, 
                                                  initial_ecology, 
                                                  parcels, 
                                                  realisation_num = length(realisations), 
                                                  collate_type = 'illegal_clearing', 
                                                  global_params, 
                                                  program_params, 
                                                  use_cfac_type_in_sim, 
                                                  decline_rates_initial)
  
  parcel_set_outcomes <- sum_nested_lists(list(collated_offsets$collated_parcel_sets$site_nets, collated_devs$collated_parcel_sets$site_nets))
  
  if (length(parcel_set_outcomes) > 0){
    parcel_set_NNL = assess_parcel_set_NNL(net_outcomes = parcel_set_outcomes, 
                                         offset_yrs = collated_offsets$offset_yrs, 
                                         parcel_set_nums = collated_offsets$parcel_set_nums, 
                                         time_steps = global_params$time_steps, 
                                         realisation_num,  
                                         eco_dims = global_params$eco_dims, 
                                         offset_time_horizon = global_params$offset_time_horizon)
  } else{
    parcel_set_NNL = list()
  }
 
  gains_degs_object = list()
  gains_degs_object$site_offset_gains <- sum_gains_degs(current_collated_reals = collated_offsets$collated_parcel_sets, 
                                     time_steps = global_params$time_steps, 
                                     eco_dims = global_params$eco_dims)
  gains_degs_object$offset_bank_gains <- sum_gains_degs(current_collated_reals = collated_offset_bank$collated_parcel_sets, 
                                                        time_steps = global_params$time_steps, 
                                                        eco_dims = global_params$eco_dims)  
  gains_degs_object$site_dev_losses <- sum_gains_degs(current_collated_reals = collated_devs$collated_parcel_sets, 
                                   time_steps = global_params$time_steps, 
                                   eco_dims = global_params$eco_dims)  
  
  gains_degs_object$dev_credit_losses <- sum_gains_degs(current_collated_reals = collated_dev_credit$collated_parcel_sets, 
                                          time_steps = global_params$time_steps, 
                                          eco_dims = global_params$eco_dims) 
  
  
  gains_degs_object$net_offset_gains <- sum_net_gains_degs(list(gains_degs_object$site_offset_gains,
                                                                  gains_degs_object$offset_bank_gains))
  gains_degs_object$net_dev_losses <- sum_net_gains_degs(list(gains_degs_object$site_dev_losses, 
                                                           gains_degs_object$dev_credit_losses))
  
  gains_degs_object$net_program_outcomes <- sum_nested_lists(list(gains_degs_object$net_offset_gains, gains_degs_object$net_dev_losses))
  
  gains_degs_object$net_illegal_clearing <- sum_gains_degs(current_collated_reals = collated_illegal_clearing$collated_parcel_sets, 
                                                           time_steps = global_params$time_steps, 
                                                           eco_dims = global_params$eco_dims) 
    
  
  if (length(gains_degs_object$net_program_outcomes) > 0){
    program_NNL <- assess_system_NNL(gains_degs_object$net_program_outcomes, 
                                   realisation_num, 
                                   time_steps = global_params$time_steps, 
                                   offset_time_horizon = global_params$offset_time_horizon, 
                                   eco_dims = global_params$eco_dims)
  } else {
    program_NNL = list()
  }
  net_landscape = sum_landscape(realisations, 
                                eco_dims = global_params$eco_dims, 
                                parcel_indexes = 1:length(parcels$land_parcels), 
                                time_horizon = global_params$time_steps)
  
  program_sums <- collate_program(traj_type = 'trajectory', 
                                  realisations,
                                  trajectory_object = realisations,
                                  time_steps = global_params$time_steps, 
                                  eco_dims = global_params$eco_dims)    #group individual net realisation counterfactual values
  
  cfacs_object <- collate_all_cfacs(use_cfac_type_in_sim, 
                                    initial_ecology, 
                                    decline_rates_initial, 
                                    global_params, 
                                    collated_offsets, 
                                    collated_devs,
                                    collated_dev_credit, 
                                    collated_offset_bank, 
                                    collated_illegal_clearing,
                                    eco_dims = global_params$eco_dims)
  
  net_landscape_cfac_sum = build_landscape_cfacs(initial_ecology, decline_rates_initial, global_params, parcel_num, program_params[[1]])
  
  landscape_rel_to_counter <- subtract_nested_lists(net_landscape, rep(list(net_landscape_cfac_sum), realisation_num))
  
  system_NNL <- assess_system_NNL(landscape_rel_to_counter, 
                                  realisation_num, 
                                  time_steps = global_params$time_steps, 
                                  offset_time_horizon = global_params$offset_time_horizon, 
                                  eco_dims = global_params$eco_dims)
  
  net_program_loss = assess_landscape_loss(cfacs_object$program_cfac_sum, 
                                           program_NNL$NNL_yrs, 
                                           realisation_num, 
                                           eco_dims = global_params$eco_dims, 
                                           time_steps = global_params$time_steps)
  
  landscape_loss = assess_landscape_loss(net_landscape, 
                                         system_NNL$NNL_yrs, 
                                         realisation_num, 
                                         eco_dims = global_params$eco_dims, 
                                         time_steps = global_params$time_steps)

  collated_realisations$parcel_nums_used = parcel_nums_used
  collated_realisations$parcel_set_outcomes = parcel_set_outcomes
  collated_realisations$landscape_loss = landscape_loss
  collated_realisations$net_program_loss = net_program_loss
  collated_realisations$landscape_rel_to_counter = landscape_rel_to_counter
  collated_realisations$program_cfac_sums = cfacs_object$program_cfac_sum
  collated_realisations$program_cfac_sum_rel_initial = cfacs_object$program_cfac_sum_rel_initial
  collated_realisations$net_cfac_sum = cfacs_object$net_cfac_decline_sum
  collated_realisations$program_NNL = program_NNL
  collated_realisations$system_NNL = system_NNL
  collated_realisations$cfac_trajs = cfacs_object$decline_cfac_trajs
  collated_realisations$parcel_set_NNL = parcel_set_NNL
  collated_realisations$program_sums = program_sums
  collated_realisations$realisation_num = realisation_num
  collated_realisations$collated_offsets = collated_offsets
  collated_realisations$collated_devs = collated_devs
  collated_realisations$collated_illegal_clearing = collated_illegal_clearing
  collated_realisations$collated_dev_credit = collated_dev_credit
  collated_realisations$collated_offset_bank= collated_offset_bank
  
  collated_realisations$site_offset_gains = gains_degs_object$site_offset_gains
  collated_realisations$site_dev_losses = gains_degs_object$site_dev_losses
  collated_realisations$offset_bank_gains = gains_degs_object$offset_bank_gains
  collated_realisations$net_illegal_clearing = gains_degs_object$net_illegal_clearing
  collated_realisations$dev_credit_losses = gains_degs_object$dev_credit_losses
  collated_realisations$net_offset_gains = gains_degs_object$net_offset_gains
  collated_realisations$net_dev_losses = gains_degs_object$net_dev_losses
  collated_realisations$net_program_outcomes = gains_degs_object$net_program_outcomes
  
  collated_realisations$net_landscape = net_landscape
  
  return(collated_realisations)
  
}



place_cfac_traj_in_zero_array <- function(cfac_list, eco_dims, offset_yrs){
  output_list <- lapply(seq(cfac_list), function(i) lapply(seq(eco_dims), function(j) c(array(0, (offset_yrs[[i]] - 1)), cfac_list[[i]][[j]] ) ) )
  return(cfac_list)
}


merge_lists <- function(list_a, list_b, offset_yrs, eco_dims){
  output_list <- lapply(seq(list_a), function(i) lapply(seq(eco_dims), function(j) merge_vectors(list_a[[i]][[j]], offset_yrs[[i]], list_b[[i]][[j]]) ) ) 
  return(output_list)
}


merge_vectors <- function(vec_a, start_ind, vec_b){
  vec_a[start_ind:(start_ind + length(vec_b) - 1)] = vec_b
  return(vec_a)
}



sum_net_gains_degs <- function(gains_degs_object){
  gains_degs_inds_to_sum = which(unlist(lapply(seq_along(gains_degs_object), function(i) length(gains_degs_object[[i]]))) >0)
  gains_degs_to_sum = gains_degs_object[gains_degs_inds_to_sum]
  
  if (length(gains_degs_to_sum) > 0){
    net_gains_degs <- sum_nested_lists( lapply(seq_along(gains_degs_to_sum), function(i)  gains_degs_to_sum[[i]]$net_outcome))
  } else {
    net_gains_degs = list()
  }
  return(net_gains_degs)
}

# collated_offset_cfacs = collated_offsets$cfac_trajs 
# collated_dev_cfacs = collated_devs$cfac_trajs
# eco_dims = global_params$eco_dims

# (use_cfac_type_in_sim, 
# initial_ecology, 
# decline_rates_initial, 
# global_params, 
# collated_offsets, 
# collated_devs,
# collated_dev_credit, 
# collated_offset_bank, 
# collated_illegal_clearing,
# eco_dims = global_params$eco_dims)


collate_all_cfacs <- function(use_cfac_type_in_sim, initial_ecology, decline_rates_initial, global_params, collated_offsets, 
                              collated_devs, collated_dev_credit,  collated_offset_bank,  collated_illegal_clearing, eco_dims){
  
  time_horizons <- generate_time_horizons(time_horizon_type = 'offsets', 
                                          project_type = 'future', 
                                          yr = 1, 
                                          offset_yrs = 1, 
                                          time_horizon = (global_params$time_steps - 1), 
                                          parcel_count = length(initial_ecology) )
  
  cfacs_decline <- calc_parcel_trajs(current_parcel_ecologies = initial_ecology, 
                                     action_type = 'protect', 
                                     current_decline_rates = decline_rates_initial, 
                                     time_horizons, 
                                     global_params,
                                     vector(),
                                     time_fill = TRUE)
  
  decline_cfac_trajs = sum_trajectories(cfacs_decline, eco_dims = global_params$eco_dims)
  
  offset_cfacs <- merge_cfacs(realisations, collate_type = 'offsets', global_params, use_cfac_type_in_sim, collated_offsets$cfac_trajs, decline_cfac_trajs, eco_dims)
  dev_cfacs <- merge_cfacs(realisations, collate_type = 'devs', global_params, use_cfac_type_in_sim, collated_devs$cfac_trajs, decline_cfac_trajs, eco_dims)
  dev_credit_cfacs <- merge_cfacs(realisations, collate_type = 'dev_credit', global_params, use_cfac_type_in_sim, collated_dev_credit$cfac_trajs, decline_cfac_trajs, eco_dims)
  offset_bank_cfacs <- merge_cfacs(realisations, collate_type = 'offset_bank', global_params, use_cfac_type_in_sim, collated_offset_bank$cfac_trajs, decline_cfac_trajs, eco_dims)
  
  illegal_cfacs <- merge_cfacs(realisations, collate_type = 'illegal_clearing',global_params,  use_cfac_type_in_sim, collated_illegal_clearing$cfac_trajs, decline_cfac_trajs, eco_dims)
  
  offset_cfac_sum <- sum_cfacs(offset_cfacs)
  dev_cfac_sum <- sum_cfacs(dev_cfacs)
  dev_credit_cfac_sum <- sum_cfacs(dev_credit_cfacs)
  offset_bank_cfac_sum <- sum_cfacs(offset_bank_cfacs)
  illegal_cfac_sum <- sum_cfacs(illegal_cfacs)
  
  program_cfac_sum <- sum_nested_lists(list(offset_cfac_sum, dev_cfac_sum, dev_credit_cfac_sum, offset_bank_cfac_sum))
  program_cfac_sum_rel_initial <- shift_nested_list(list_in = program_cfac_sum, eco_dims, shift_type = 'mean')
  net_cfac_decline_sum <- nested_list_sum(decline_cfac_trajs)
  
  cfacs_object = list()
  cfacs_object$program_cfac_sum <- program_cfac_sum
  cfacs_object$program_cfac_sum_rel_initial = program_cfac_sum_rel_initial
  cfacs_object$decline_cfac_trajs <- decline_cfac_trajs
  cfacs_object$net_cfac_decline_sum <- net_cfac_decline_sum
  cfacs_object$net_illegal_cfac_sum <- illegal_cfac_sum
  
  return(cfacs_object)
}


build_landscape_cfacs <- function(initial_ecology, decline_rates_initial, global_params, parcel_num, current_program_params){
  
  time_horizons <- generate_time_horizons(time_horizon_type = 'offsets', 
                                          project_type = 'future', 
                                          yr = 1, 
                                          offset_yrs = 1, 
                                          time_horizon = (global_params$time_steps - 1), 
                                          parcel_count = length(initial_ecology) )
  
  current_cfacs = calc_cfacs(parcel_ecologies = initial_ecology, 
                             parcel_num_remaining = parcel_num,
                             global_params, 
                             current_program_params,
                             decline_rates_initial, 
                             time_horizons, 
                             offset_yrs = rep(1, length(initial_ecology)), 
                             FALSE,
                             FALSE,
                             TRUE,  
                             TRUE)
  net_landscape_cfac = nested_list_sum(current_cfacs$cfac_trajs)
  return(net_landscape_cfac)
}



sum_cfacs <- function(cfac_list){
  if (length(unlist(cfac_list)) > 0){
    cfac_sum <- lapply(seq(cfac_list), function(i) nested_list_sum(cfac_list[[i]]))
  } else {
    cfac_sum = list()
  }
  return(cfac_sum)
}
  
sum_collated_cfacs <- function(merged_cfacs){
  collate_list = vector('list', realisation_num)
  for (realisation_ind in seq(realisation_num)){
    collate_list[[realisation_iond]] <- nested_list_sum(merged_offset_cfacs[[realisation_ind]])
  }
  return(collate_list)
}



# collate_type = 'illegal_clearing'
# collated_cfac_object = collated_illegal_cfacs


merge_cfacs <- function(realisations, collate_type, global_params, use_cfac_type_in_sim, cfac_trajs, decline_cfac_trajs, eco_dims){
  realisation_num = length(realisations)
  collate_list = vector('list', realisation_num) 
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]
    
    parcel_indexes <- select_parcel_set_indexes(model_outputs, collate_type)
    offset_yrs <- select_offset_yrs(model_outputs, collate_type)
    if (use_cfac_type_in_sim == TRUE){
      if (length(parcel_indexes) > 0){
      collate_list[[realisation_ind]] <- merge_lists(decline_cfac_trajs[unlist(parcel_indexes)], cfac_trajs[[realisation_ind]], offset_yrs, eco_dims)
      } else {
        collate_list[[realisation_ind]] = list(list_of_zeros(list_dims = eco_dims, array_dims = global_params$time_steps))
      }
    } else {
      collate_list[[realisation_ind]] <- decline_cfac_trajs[unlist(parcel_indexes)]
    }
  }
  return(collate_list)
}


select_offset_yrs <- function(model_outputs, collate_type){
  if (collate_type == 'offsets'){
    offset_yrs = model_outputs$offsets_object$offset_yrs
  } else if (collate_type == 'devs'){
    offset_yrs = model_outputs$dev_object$offset_yrs
  } else if (collate_type == 'illegal_clearing'){
    offset_yrs = model_outputs$illegal_clearing$offset_yrs
  } else if (collate_type == 'dev_credit'){
    offset_yrs = model_outputs$dev_credit_object$offset_yrs
  } else if (collate_type == 'offset_bank'){
    offset_yrs = model_outputs$offset_bank$offset_yrs
  } 
  return(offset_yrs)
}



assess_system_NNL <- function(net_program_outcomes, realisation_num, time_steps, offset_time_horizon, eco_dims){
  system_NNL <- list()
  NNL_yrs <- find_NNL(net_program_outcomes, assess_type = 'system', eco_dims, assess_num = realisation_num, time_steps, offset_yrs = vector(), offset_time_horizon)
  system_NNL$NNL_yrs <- NNL_yrs
  system_NNL$NNL_success = assess_NNL(system_NNL$NNL_yrs, assess_type = 'success', eco_dims)
  system_NNL$NNL_mean = assess_NNL(system_NNL$NNL_yrs, assess_type = 'mean', eco_dims)
  return(system_NNL)
}



# net_outcomes = parcel_set_outcomes 
# offset_yrs = collated_offsets$offset_yrs 
# parcel_set_nums = collated_offsets$parcel_set_nums 
# time_steps = global_params$time_steps
# realisation_num  
# eco_dims = global_params$eco_dims 
# offset_time_horizon = global_params$offset_time_horizon


assess_parcel_set_NNL <- function(net_outcomes, offset_yrs, parcel_set_nums, time_steps, realisation_num,  eco_dims, offset_time_horizon){
  parcel_set_NNL <- list()
  parcel_set_NNL$NNL_yrs <- find_parcel_set_NNL_yrs(net_outcomes, offset_yrs, parcel_set_nums, time_steps, realisation_num,  eco_dims, offset_time_horizon)
  parcel_set_NNL$NNL_success = assess_NNL(unlist(parcel_set_NNL$NNL_yrs, recursive = FALSE), assess_type = 'success', eco_dims)
  parcel_set_NNL$NNL_mean = assess_NNL(unlist(parcel_set_NNL$NNL_yrs, recursive = FALSE), assess_type = 'mean', eco_dims)
  return(parcel_set_NNL)
}



# assess_rel_to_counter <- function(net_landscape, net_cfac_sum, assess_type, realisation_num, eco_dims){
#   if (assess_type == 'landscape'){
#     net_cfac_sum = rep(list(net_cfac_sum), realisation_num)
#   }
#   landscape_outcome <- lapply(seq_len(realisation_num), function(i) mapply('-', net_landscape[[i]], net_cfac_sum[[i]], SIMPLIFY = FALSE))
#   return(landscape_outcome)
# }
#   

sum_landscape <- function(realisations, eco_dims, parcel_indexes, time_horizon){
  realisation_num = length(realisations)
  
  net_landscape = vector('list', realisation_num)
  
  for (realisation_ind in 1:realisation_num){
    current_landscape_trajs = sum_trajectories(realisations[[realisation_ind]]$trajectories, eco_dims)
    net_landscape[[realisation_ind]] = nested_list_sum(current_landscape_trajs)
  }
  return(net_landscape)
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

assess_net_gains <- function(net_offset_outcome, net_dev_outcome){
  net_gains = sum_nested_lists(list(net_offset_outcome, net_dev_outcome))
  return(net_gains)
}


find_parcel_set_NNL_yrs <- function(net_outcomes, offset_yrs, parcel_set_nums, time_steps, realisation_num, eco_dims, offset_time_horizon){
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



assess_parcels_used <- function(realisations){
  dev_nums = unlist(lapply(seq_along(realisations), function(i) length(realisations[[i]]$dev_object$parcel_indexes)))
  offset_nums = unlist(lapply(seq_along(realisations), function(i) length(realisations[[i]]$offsets_object$parcel_indexes)))
  program_nums = list()
  program_nums$mean_dev_num = round(mean(dev_nums))
  program_nums$mean_offset_num = round(mean(offset_nums))
  return(program_nums)
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


select_object_to_collate <- function(current_realisation, collate_type, initial_ecology){
  if (collate_type == 'offsets'){
    current_model_outputs = current_realisation$offsets_object
    current_model_outputs$parcel_indexes = current_realisation$index_object$offsets
  } else if (collate_type == 'devs'){
    current_model_outputs = current_realisation$dev_object
    current_model_outputs$parcel_indexes = current_realisation$index_object$developments
  } else if (collate_type == 'illegal_clearing'){
    current_model_outputs = current_realisation$illegal_clearing
    current_model_outputs$parcel_indexes = current_realisation$index_object$illegal_clearing
  } else if (collate_type == 'dev_credit'){
    current_model_outputs = current_realisation$dev_credit_object
    current_model_outputs$parcel_indexes = current_realisation$index_object$dev_credit
  } else if (collate_type == 'offset_bank'){
    current_model_outputs = current_realisation$offset_bank
    current_model_outputs$parcel_indexes = current_realisation$index_object$offset_bank
  } else if (collate_type == 'all'){
    current_model_outputs = list()
    current_model_outputs$parcel_indexes = unlist(current_realisation$index_object$landscape_inds)
    parcel_num = length(current_model_outputs$parcel_indexes)
    current_model_outputs$parcel_ecologies = initial_ecology[current_model_outputs$parcel_indexes]
    current_model_outputs$offset_yrs = rep(list(1), parcel_num)
    current_model_outputs$parcel_num_remaining = rep(list(parcel_num), parcel_num)
  }

  indexes_to_use = unlist(current_model_outputs$parcel_indexes)
  
  if (length(indexes_to_use) > 0){
    current_model_outputs$trajectories = current_realisation$trajectories[indexes_to_use]
  }
  
  return(current_model_outputs)
}


select_parcel_set_indexes <- function(model_outputs, collate_type){
  if (collate_type == 'offsets'){
    parcel_set_indexes = model_outputs$index_object$offsets
  } else if (collate_type == 'devs'){
    parcel_set_indexes = model_outputs$index_object$developments
  } else if (collate_type == 'illegal_clearing'){
    parcel_set_indexes = model_outputs$index_object$illegal_clearing
  } else if (collate_type == 'dev_credit'){
    parcel_set_indexes = model_outputs$dev_credit_object$parcel_indexes
  } else if (collate_type == 'offset_bank'){
    parcel_set_indexes = model_outputs$offset_bank$parcel_indexes
  }  else if (collate_type == 'all'){
    parcel_set_indexes = model_outputs$index_object$landscape_inds
  }
  return(parcel_set_indexes)
}


select_cfac_type <- function(collate_type, use_cfac_type_in_sim, current_program_params){
  cfac_params = list()
  
  if (use_cfac_type_in_sim == FALSE){
    include_illegal_clearing = FALSE
    include_potential_developments = FALSE
    include_potential_offsets = FALSE
    adjust_cfacs_flag = FALSE
  } else {
    
    if ((collate_type == 'devs') | (collate_type == 'dev_credit')){
      include_illegal_clearing = current_program_params$include_illegal_clearing_in_dev_calc
      include_potential_developments = current_program_params$include_potential_developments_in_dev_calc
      include_potential_offsets = current_program_params$include_potential_offsets_in_dev_calc
      adjust_cfacs_flag = current_program_params$adjust_dev_cfacs_flag
    } else {
      include_illegal_clearing = current_program_params$include_illegal_clearing_in_offset_calc
      include_potential_developments = current_program_params$include_potential_developments_in_offset_calc
      include_potential_offsets = current_program_params$include_potential_offsets_in_offset_calc
      adjust_cfacs_flag = current_program_params$adjust_offset_cfacs_flag
    }
  }
  cfac_params$include_illegal_clearing = include_illegal_clearing
  cfac_params$include_potential_developments = include_potential_developments
  cfac_params$include_potential_offsets = include_potential_offsets
  cfac_params$adjust_cfacs_flag = adjust_cfacs_flag
  return(cfac_params)
}




# realisation_num = length(realisations) 
# collate_type = 'illegal_clearing' 



collate_cfacs <- function(current_model_outputs, parcels, collate_type, global_params, program_params, use_cfac_type_in_sim, decline_rates_initial){
  
  current_indexes = current_model_outputs$parcel_indexes
  
  for (region_ind in seq_len(global_params$region_num)){
    
    indexes_to_use = which(unlist(current_indexes) %in% parcels$regions[[region_ind]])
    if (length(indexes_to_use) > 0){
      current_program_params = program_params[[region_ind]]
      cfac_params <- select_cfac_type(collate_type, use_cfac_type_in_sim, current_program_params)
      
      current_model_outputs <- select_subset(current_model_outputs, subset_pool = indexes_to_use)
      offset_yrs = unlist(current_model_outputs$offset_yrs)
      
      time_horizons = generate_time_horizons(time_horizon_type = 'offset_bank', 
                                             project_type = 'current', 
                                             yr = global_params$time_steps, 
                                             offset_yrs, 
                                             time_horizon = (global_params$time_steps - 1), 
                                             parcel_count = length(unlist(current_model_outputs$offset_yrs)))
      
      current_cfacs = calc_cfacs(parcel_ecologies = current_model_outputs$parcel_ecologies, 
                                 parcel_num_remaining = current_model_outputs$parcel_num_remaining, 
                                 global_params, 
                                 current_program_params,
                                 decline_rates_initial[unlist(current_indexes)], 
                                 time_horizons, 
                                 offset_yrs, 
                                 cfac_params$include_potential_developments,
                                 cfac_params$include_potential_offsets,
                                 cfac_params$include_illegal_clearing,  
                                 cfac_params$adjust_cfacs_flag)
    } else {
      current_cfacs = list()
    }
    
  }
  
  return(current_cfacs)
}




# realisation_num = length(realisations) 
# collate_type = 'illegal_clearing' 


run_collate_routine <- function(realisations, initial_ecology, parcels, realisation_num, collate_type, global_params, program_params, use_cfac_type_in_sim, decline_rates_initial){
  collated_object = list()
  collated_object = collate_gains_degs(realisations, initial_ecology, parcels, realisation_num, collate_type, global_params, program_params, use_cfac_type_in_sim, decline_rates_initial)
  if (length(collated_object) > 0){
    collated_object$collated_parcel_sets = group_gains_degs(collated_object, realisation_num, global_params$time_steps, global_params$eco_dims)
  } else {
    collated_object$collated_parcel_sets = list()
  }
  return(collated_object)
}


collate_gains_degs <- function(realisations, initial_ecology, parcels, realisation_num, collate_type, global_params, program_params, use_cfac_type_in_sim, decline_rates_initial){ 
  
  if (collate_type == 'dev_credit'){
    assessment_test = lapply(seq_along(realisations), function(i) length(unlist(realisations[[i]]$dev_credit_object$parcel_indexes)))
  } else if (collate_type == 'devs'){
    assessment_test = lapply(seq_along(realisations), function(i) length(unlist(realisations[[i]]$dev_object$parcel_indexes)))
  } else if (collate_type == 'offsets'){
    assessment_test = lapply(seq_along(realisations), function(i) length(unlist(realisations[[i]]$offsets_object$parcel_indexes)))
  } else if (collate_type == 'offset_bank'){
    assessment_test = lapply(seq_along(realisations), function(i) length(unlist(realisations[[i]]$offset_bank$parcel_indexes)))
  } else if (collate_type == 'illegal_clearing'){
    assessment_test = lapply(seq_along(realisations), function(i) length(unlist(realisations[[i]]$illegal_clearing$parcel_indexes)))
  }
  
  if (any(unlist(assessment_test) == 0)){
    collated_gains_degs = list()
    return(collated_gains_degs)
  }

  collated_gains_degs = vector('list', realisation_num)
  offset_yrs = vector('list', realisation_num)
  parcel_set_nums = vector('list', realisation_num)
  parcel_sums_at_offset = vector('list', realisation_num)
  trajectories = vector('list', realisation_num)
  parcel_set_indexes = vector('list', realisation_num)
  cfac_trajs = vector('list', realisation_num)
  
  for (realisation_ind in seq_len(realisation_num)){

    current_realisation = realisations[[realisation_ind]]
    current_model_outputs = select_object_to_collate(current_realisation, collate_type, initial_ecology)
    
    current_cfacs = collate_cfacs(current_model_outputs, parcels, collate_type, global_params, program_params, use_cfac_type_in_sim, decline_rates_initial)
    
    if (length(current_model_outputs$parcel_indexes) > 0){
      collated_gains_degs[[realisation_ind]] <- assess_gains_degs(current_model_outputs, 
                                                      collate_type, 
                                                      program_params,
                                                      cfac_list = current_cfacs$cfacs, 
                                                      global_params$eco_dims, 
                                                      time_steps = global_params$time_steps)
    } else {
      collated_gains_degs = list()
    }

    offset_yrs[[realisation_ind]] = current_model_outputs$offset_yrs
    parcel_sums_at_offset[[realisation_ind]] = current_model_outputs$parcel_sums_at_offset
    parcel_set_indexes[[realisation_ind]] = current_model_outputs$parcel_indexes
    trajectories[[realisation_ind]] = sum_trajectories(traj_list = current_model_outputs$trajectories, eco_dims = global_params$eco_dims)
    cfac_trajs[[realisation_ind]] = current_cfacs$cfac_trajs 
  }
  

  collated_gains_degs_object = list()
  collated_gains_degs_object$collated_gains_degs = collated_gains_degs
  collated_gains_degs_object$parcel_set_nums = lapply(seq_along(parcel_set_indexes), function(i) length(parcel_set_indexes[[i]]) )
  collated_gains_degs_object$offset_yrs = offset_yrs
  collated_gains_degs_object$parcel_sums_at_offset = parcel_sums_at_offset
  collated_gains_degs_object$cfac_trajs = cfac_trajs
  collated_gains_degs_object$trajectories = trajectories
  collated_gains_degs_object$parcel_set_indexes = parcel_set_indexes
  
  return(collated_gains_degs_object)
  
}



group_gains_degs <- function(collated_gains_degs_object, realisation_num, time_steps, eco_dims){ 
  
  collated_parcel_sets = vector('list', 5)

  for (realisation_ind in seq_len(realisation_num)){
    current_gains_degs_object = collated_gains_degs_object$collated_gains_degs[[realisation_ind]]
    parcel_set_indexes = collated_gains_degs_object$parcel_set_indexes[[realisation_ind]]
    grouped_gains_degs_object <- lapply(seq_along(current_gains_degs_object), function (i) arrange_to_parcel_set_list(current_gains_degs_object[[i]], parcel_set_indexes))
    grouped_gains_degs_object <- lapply(seq_along(grouped_gains_degs_object), function (i) combine_to_parcel_set_array(grouped_gains_degs_object[[i]], time_steps, eco_dims))
    collated_parcel_sets <- lapply(seq_along(grouped_gains_degs_object), function(i) append(collated_parcel_sets[[i]], grouped_gains_degs_object[i]))
    
  }
  
  names(collated_parcel_sets) = names(current_gains_degs_object)

  return(collated_parcel_sets)
  
}




# traj_type = 'trajectory'
# trajectory_object = realisations
collate_program <- function(traj_type, realisations, trajectory_object, time_steps, eco_dims){
  collated_program = list()
  collated_program$offsets <- sum_parcel_sets(traj_type, collate_type = 'offsets', sum_type = 'net', realisations, trajectory_object, time_steps, eco_dims)
  collated_program$devs <- sum_parcel_sets(traj_type, collate_type = 'devs', sum_type = 'net', realisations, trajectory_object, time_steps, eco_dims)
  collated_program$dev_credit <- sum_parcel_sets(traj_type, collate_type = 'dev_credit', sum_type = 'net', realisations, trajectory_object, time_steps, eco_dims)
  collated_program$offset_bank <- sum_parcel_sets(traj_type, collate_type = 'offset_bank', sum_type = 'net', realisations, trajectory_object, time_steps, eco_dims)
  collated_program$net_offsets <- sum_nested_lists(list(collated_program$offsets, collated_program$offset_bank))
  collated_program$net_devs <- sum_nested_lists(list(collated_program$devs, collated_program$dev_credit))
  collated_program$net <- sum_nested_lists(list(collated_program$net_offsets, collated_program$net_devs))
  collated_program$outcome_rel_initial <- shift_nested_list(list_in = collated_program$net, eco_dims, shift_type = 'mean')
  return(collated_program)
}


shift_nested_list <- function(list_in, eco_dims, shift_type){
  if (shift_type == 'mean'){
    mean_initial = mean(unlist(lapply(seq_along(list_in), function(i) (lapply( seq(eco_dims), function(j) (list_in[[i]][[j]][1]))))))
  } else{
    mean_initial = 0
  }
  list_out <- lapply(seq_along(list_in), function(i) (lapply( seq(eco_dims), function(j) (list_in[[i]][[j]] + rep(mean_initial - list_in[[i]][[j]][1], length(list_in[[i]][[j]]) )))))

  return(list_out)
}


sum_parcel_sets <- function(traj_type, collate_type, sum_type, realisations, trajectory_object, time_steps, eco_dims){
  
  realisation_num = length(trajectory_object)
  collate_list = vector('list', realisation_num) 
  
  for (realisation_ind in seq_len(realisation_num)){
    model_outputs = realisations[[realisation_ind]]
    if (traj_type == 'cfac'){
      trajectories = trajectory_object[[realisation_ind]]
    } else {
      trajectories = model_outputs$trajectories
    }
    
    if (collate_type == 'offsets'){
      parcel_indexes = model_outputs$index_object$offsets
    } else if (collate_type == 'devs'){
      parcel_indexes = model_outputs$index_object$developments
    } else if (collate_type == 'dev_credit'){
      parcel_indexes = model_outputs$dev_credit_object$parcel_indexes
    } else if (collate_type == 'offset_bank'){
      parcel_indexes = model_outputs$offset_bank$parcel_indexes
    } else if (collate_type == 'illegal_clearing'){
      parcel_indexes = model_outputs$index_object$illegal_clearing
    }
    
    parcel_set_num <- length(parcel_indexes) 
    
    parcel_trajs = sum_trajectories(trajectories[unlist(parcel_indexes)], eco_dims) 
    parcel_set_trajs = combine_to_parcel_set(parcel_trajs, parcel_indexes, parcel_set_num, time_steps, eco_dims)
    
    if (sum_type == 'net'){
      collate_list[[realisation_ind]] = lapply(seq(eco_dims), function(i) apply(parcel_set_trajs[[i]], MARGIN = 1, 'sum'))
    } else {
      collate_list[[realisation_ind]] = parcel_set_trajs
    }
    
  }
  
  return(collate_list)
}



sum_program_cfacs <- function(parcel_cfac_trajs, parcel_indexes){
  program_cfacs = parcel_cfac_trajs[, parcel_indexes, ]
  program_cfac_sum = sum_cols_multi(program_cfacs)
  return(program_cfac_sum)
}


# list_to_collate = current_gains_degs_object[[1]]
# parcel_indexes = parcel_set_indexes



arrange_to_parcel_set_list <- function(list_to_collate, parcel_set_indexes){
  parcel_set_num = length(parcel_set_indexes)
  collate_list = vector('list', length(parcel_set_num))
  parcel_inds_to_collate = unlist(parcel_set_indexes)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    match_inds <- list_intersect(parcel_inds_to_collate, parcel_set_indexes[[parcel_set_ind]])
    match_inds <- match_inds$match_ind
    collate_list[[parcel_set_ind]] = list_to_collate[match_inds]
  }
  
  return(collate_list)
}



combine_to_parcel_set_array <- function(list_to_collate, time_steps, eco_dims){
  parcel_set_num = length(list_to_collate)
  collate_list = list_of_zeros(list_dims = eco_dims, array_dims = c(time_steps, parcel_set_num))
   for (parcel_set_ind in seq_along(list_to_collate)){
     current_list = list_to_collate[[parcel_set_ind]]
      for (eco_ind in seq(eco_dims)){
        collate_list[[eco_ind]][, parcel_set_ind] = Reduce('+', lapply(seq_along(current_list), function(i) current_list[[i]][[eco_ind]]))
      }
  }
  return(collate_list)
}



combine_to_parcel_set <- function(list_to_collate, parcel_indexes, parcel_set_num, time_steps, eco_dims){
  
  collate_list = list_of_zeros(list_dims = eco_dims, array_dims = c(time_steps, parcel_set_num))
  
  parcel_inds_to_collate = unlist(parcel_indexes)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    match_inds <- list_intersect(parcel_inds_to_collate, parcel_indexes[[parcel_set_ind]])
    match_inds <- match_inds$match_ind
    current_list = list_to_collate[match_inds]
    if (length(match_inds) > 0){
      for (eco_ind in seq(eco_dims)){
        collate_list[[eco_ind]][, parcel_set_ind] = Reduce('+', lapply(seq_along(current_list), function(i) current_list[[i]][[eco_ind]]))
      }
    }
  }
  
  return(collate_list)
}



# trajectories = current_model_outputs$trajectories 
# cfac_list = current_cfacs$cfacs
# parcel_num = length(indexes_to_use) 
# eco_dims= global_params$eco_dims 
# time_steps = global_params$time_steps

assess_gains_degs <- function(current_model_outputs, collate_type, program_params, cfac_list, eco_dims, time_steps){
  
  parcel_num = length(unlist(current_model_outputs$parcel_indexes))
  trajectories = current_model_outputs$trajectories
  collate_array = vector('list', parcel_num) 
  for (parcel_count_ind in 1:parcel_num){
    collate_array[[parcel_count_ind]] = list_of_zeros(list_dims = eco_dims, array_dims = time_steps)
  }
  rest_gains <- collate_array
  avoided_degs <- collate_array      # initialise array to store rest. gains / avoided degs
  losses <- collate_array
  nets <- collate_array
  
  offset_yrs = current_model_outputs$offset_yrs                         # vector containing year offset was initialised
  
  if (class(offset_yrs) == 'list'){
    offset_yrs = unlist(offset_yrs)
  }
  
  #parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE)        #remove lowest list level 
  parcel_ecologies = current_model_outputs$parcel_ecologies
  
  for (parcel_count_ind in seq_len(parcel_num)){                      #cycle through all parcels to collate
    current_yr = offset_yrs[parcel_count_ind]                         #select current offset year 
    
    for (eco_ind in seq_len(eco_dims)){                 # cycle through ecological dimensions
      
      current_parcel_ecology = parcel_ecologies[[parcel_count_ind]][[eco_ind]]
      current_parcel_traj = trajectories[[parcel_count_ind]][[eco_ind]][current_yr:time_steps, ]
      current_parcel_cfac = cfac_list[[parcel_count_ind]][[eco_ind]]
      current_gains_losses <- assess_gains_losses(current_parcel_traj, current_parcel_cfac)
      current_gains_degs <- separate_gains_degs(current_parcel_traj, 
                                                collate_type, 
                                                current_parcel_cfac, 
                                                current_parcel_ecology,
                                                loss_inds = current_gains_losses$loss_inds,
                                                current_time_horizon = (time_steps - current_yr + 1) )
      
      rest_gains[[parcel_count_ind]][[eco_ind]][current_yr:time_steps] = current_gains_degs$rest_gains
      avoided_degs[[parcel_count_ind]][[eco_ind]][current_yr:time_steps] = current_gains_degs$avoided_degs
      losses[[parcel_count_ind]][[eco_ind]][current_yr:time_steps] = current_gains_losses$losses
      nets[[parcel_count_ind]][[eco_ind]][current_yr:time_steps] = current_gains_losses$nets
    }
    
  }
  
  if ((collate_type == 'offsets') | (collate_type == 'offset_bank')){
    if (program_params[[1]]$offset_calc_type == 'restoration_gains'){
      site_nets = rest_gains
    } else if (program_params[[1]]$offset_calc_type == 'avoided_degs'){
      site_nets = avoided_degs
    } else if (program_params[[1]]$offset_calc_type == 'net_gains'){
      site_nets = nets
    }
  } else if ( (collate_type == 'devs') | (collate_type == 'illegal_clearing') | (collate_type == 'dev_credit')){
    if (program_params[[1]]$dev_calc_type == 'future_condition'){
      site_nets = nets
    } else if (program_params[[1]]$dev_calc_type == 'current_condition'){
      site_nets = rest_gains
    }
    
  }
  
  collated_object = list()
  collated_object$rest_gains = rest_gains
  collated_object$avoided_degs = avoided_degs
  collated_object$losses = losses
  collated_object$nets = nets
  collated_object$site_nets = site_nets
  return(collated_object)
}




threshold_array <- function(arr_in, thresh_level){
  thresh_array = rep(thresh_level, length(arr_in))
  dim(thresh_array) = dim(arr_in)
  arr_out = arr_in * (abs(arr_in) > thresh_array)
  return(arr_out)
}


# current_time_horizon = (time_steps - current_yr + 1)

assess_gains_losses <- function(current_parcel_traj, current_parcel_cfac){
  nets = apply(current_parcel_traj - current_parcel_cfac, 1, 'sum')
  losses = array(0, length(nets))
  loss_inds = nets < 0
  losses[loss_inds] = nets[loss_inds]
  
  gain_loss_object = list()
  gain_loss_object$loss_inds = loss_inds
  gain_loss_object$losses = threshold_array(losses, thresh_level= 1e-5)
  gain_loss_object$nets = threshold_array(nets, thresh_level = 1e-5)
  return(gain_loss_object)
}

# loss_inds = current_gains_losses$loss_inds
# current_time_horizon = (time_steps - current_yr + 1)

separate_gains_degs <- function(current_parcel_traj, collate_type, current_parcel_cfac, current_parcel_ecology, loss_inds, current_time_horizon){
  
  gains_degs_object = list()
  current_eco_array = matrix(rep(current_parcel_ecology, current_time_horizon), nrow = current_time_horizon, byrow = TRUE)
  rest_gains = apply(current_parcel_traj - current_eco_array, 1, 'sum')
  avoided_degs = apply(current_eco_array - current_parcel_cfac, 1, 'sum')
  
  if ((collate_type == 'devs') | (collate_type == 'dev_credit') | (collate_type == 'illegal_clearing')){
   
    gains_degs_object$rest_gains = rest_gains
    gains_degs_object$avoided_degs = avoided_degs
    
  } else if ((collate_type == 'offsets') | (collate_type == 'offset_bank')){
    rest_gains[loss_inds] = 0
    avoided_degs[loss_inds] = 0
    
    pure_rest = (rest_gains >= 0) & (avoided_degs < 0)
    pure_degs = (rest_gains < 0)
    
    rest_gains[pure_rest] = rest_gains[pure_rest] + avoided_degs[pure_rest]
    avoided_degs[pure_rest] = 0
    
    avoided_degs[pure_degs] = avoided_degs[pure_degs] + rest_gains[pure_degs]
    rest_gains[pure_degs] = 0
    
    gains_degs_object$rest_gains = threshold_array(rest_gains, 1e-5)
    gains_degs_object$avoided_degs = threshold_array(avoided_degs, 1e-5)
  }

  return(gains_degs_object)
  
}




      

sum_gains_degs <- function(current_collated_reals, time_steps, eco_dims){
  
  summed_realisations = list()
  if (length(current_collated_reals)>0){
    summed_realisations$rest_gains = sum_collated_realisations(current_collated_reals$rest_gains, time_steps, eco_dims)
    summed_realisations$avoided_degs = sum_collated_realisations(current_collated_reals$avoided_degs, time_steps, eco_dims)
    summed_realisations$losses = sum_collated_realisations(current_collated_reals$losses, time_steps, eco_dims)
    summed_realisations$net_outcome <- sum_collated_realisations(current_collated_reals$nets, time_steps, eco_dims)
  }
  return(summed_realisations)
  
}





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





collate_net_realisation_outcome <- function(summed_realisations, eco_dims){
  net_outcome = list()
  net_outcome$standard = sum_nested_lists(list(summed_realisations$rest_gains, summed_realisations$avoided_degs))

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




