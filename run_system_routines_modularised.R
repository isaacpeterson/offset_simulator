run_offsets_simulation <- function(sim_group){ # run the model and return outputs
  
  if (global_params$set_seed == TRUE){
    set.seed(123)
  }
  global_object = intialise_global_object(sim_group$global_params, sim_group$initial_ecology, sim_group$decline_rates_initial, sim_group$parcels)
  global_object <- run_system(global_object, sim_group$global_params, sim_group$program_params_to_use, sim_group$decline_rates_initial, sim_group$parcels)  
  
  return(global_object)
}  

# for (s in 1:10){
#   global_object = intialise_global_object(sim_group$global_params, sim_group$initial_ecology, sim_group$decline_rates_initial, sim_group$parcels)
#   global_object <- run_system(global_object, sim_group$global_params, sim_group$program_params_to_use, sim_group$decline_rates_initial, sim_group$parcels)  
#   
# }

intialise_global_object <- function(global_params, initial_ecology, decline_rates_initial, parcels){
  global_object = list()
  global_object$dev_credit = array(0, global_params$eco_dims)
  global_object$recorded_dev_credit = vector()
  global_object$offsets_object <- initialise_parcel_set_object() 
  global_object$dev_object <- initialise_parcel_set_object() 
  global_object$illegal_clearing <- initialise_parcel_set_object()
  global_object$index_object <- initialise_index_object(parcels, initial_ecology, global_params)
  global_object$dev_credit_object <- initialise_parcel_set_object() 
  global_object$offset_bank <- initialise_parcel_set_object()
  global_object$decline_rates <- decline_rates_initial
  global_object$offset_pool_object <- list()
  global_object$trajectories <- initialise_trajectories(global_params$eco_dims, land_parcels = parcels$land_parcels, global_params$time_steps)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
  global_object$initial_ecology = initial_ecology
  global_object$current_ecology = initial_ecology
  global_object$credit_match_object$match_flag = FALSE
  global_object$match_object$match_flag = FALSE
  return(global_object)
}




get_sim_characteristics <- function(program_params, realisation_num){
  
  sim_characteristics = vector()
  sim_characteristics = paste(sim_characteristics, program_params[[1]]$offset_calc_type, '_', sep = '', collapse = '')
  sim_characteristics = paste(sim_characteristics, 'offset_bank_', program_params[[1]]$use_offset_bank, '_', sep = '', collapse = '')
  if ((program_params[[1]]$use_offset_time_horizon == TRUE) & (program_params[[1]]$use_offset_bank == FALSE)){                                   
    sim_characteristics = paste(sim_characteristics, 'time_horizon_', program_params[[1]]$offset_time_horizon, sep = '', collapse = '')
  }
  sim_characteristics = paste(sim_characteristics, '_include_illegal_clearing_', program_params[[1]]$include_illegal_clearing_in_offset_calc, sep = '', collapse = '')
  
  sim_characteristics = paste(sim_characteristics, '_realisations_', realisation_num, sep = '', collapse = '')
  #   sim_characteristics = paste(program_params[[1]]$offset_calc_type, '_', program_params[[1]]$dev_calc_type, '_', program_params[[1]]$cfac_type_in_offset_calc,  '_cfac_offset_bank_', 
  #                                program_params[[1]]$use_offset_bank, '_', sep = '', collapse = '')
  #   
  #   if (program_params[[1]]$use_offset_bank == TRUE){                                   
  #     sim_characteristics = paste(sim_characteristics, program_params[[1]]$offset_bank_start, '_', program_params[[1]]$offset_bank_end, '_', 
  #                                  program_params[[1]]$offset_bank_num, '_', program_params[[1]]$match_type, sep = '', collapse = '')
  #   }
  #   
  #   sim_characteristics = paste(sim_characteristics, '_', program_params[[1]]$offset_action_type, '_', sep = '', collapse = '')
  #   if (program_params[[1]]$offset_action_type == 'restore'){
  #     sim_characteristics = paste(sim_characteristics, program_params[[1]]$restoration_rate, '_', sep = '', collapse = '')
  #   }
  #   
  #   if (program_params[[1]]$use_offset_time_horizon == TRUE){                                   
  #     sim_characteristics = paste(sim_characteristics, '_time_horizon_', program_params[[1]]$offset_time_horizon, sep = '', collapse = '')
  #   }
  
  
  #  sim_characteristics = paste(sim_characteristics, '_offsets_potential_developments_', program_params[[1]]$include_potential_developments_in_offset_calc, sep = '', collapse = '')
  
  #  sim_characteristics = paste(sim_characteristics, '_offsets_potential_offsets_', program_params[[1]]$include_potential_offsets_in_offset_calc, sep = '', collapse = '')
  
  #  sim_characteristics = paste(sim_characteristics, '_devs_illegal_clearing_', program_params[[1]]$include_illegal_clearing_in_dev_calc, sep = '', collapse = '')
  
  # sim_characteristics = paste(sim_characteristics, '_devs_potential_developments_', program_params[[1]]$include_potential_developments_in_dev_calc, sep = '', collapse = '')
  
  #  sim_characteristics = paste(sim_characteristics, '_devs_potential_offsets_', program_params[[1]]$include_potential_offsets_in_dev_calc, sep = '', collapse = '')
  
  
  return(sim_characteristics)
}




parcel_set_list_names <- function(){
  list_names = c("parcel_indexes", "parcel_num_remaining", "offset_yrs", "parcel_ecologies", "parcel_sums_at_offset", "cfac_trajs", "parcel_vals_used", 
                 "restoration_vals", "cfac_vals", "region_ind")
  return(list_names)
}


run_system <- function(global_object, global_params, program_params_to_use, decline_rates_initial, parcels){ # main engine for code - returns all development/offset parcel sets, land parcel trajectories etc.

  for (yr in seq_len(global_params$time_steps)){          #run through main time loop
  #for (yr in seq_len(15)){
    
    for (region_ind in seq_len(parcels$region_num)){            #cycle through each region

      current_program_params = program_params_to_use[[region_ind]]
      time_horizon <- current_program_params$offset_time_horizon
      offset_bank_num = unlist(current_program_params$banked_offset_vec[yr])   # how many offsets to be added in current year
      
      if (current_program_params$use_offset_bank == TRUE){
        if (offset_bank_num > 0){ 
          global_object <- add_to_bank(global_object, offset_bank_num, current_program_params,  yr,  global_object$current_ecology,  region_ind, global_params)
        }
      }
      
      if (current_program_params$intervention_vec[yr] > 0){
        offset_index_pool <- select_offset_index_pool(index_object = global_object$index_object, current_program_params, region_ind)
        
        if (offset_index_pool$empty_pool_flag == TRUE){ #if nothing in offset pool do not attempt to develop
          break
        }
        
        global_object$offset_pool_object <- prepare_offset_pool(current_offset_pool = offset_index_pool$pool, 
                                                                current_program_params, 
                                                                index_object = global_object$index_object, 
                                                                offset_bank = global_object$offset_bank, 
                                                                decline_rates = global_object$decline_rates,
                                                                decline_rates_initial,
                                                                region_ind, 
                                                                parcels, 
                                                                current_ecology = global_object$current_ecology, 
                                                                global_params, 
                                                                time_horizon, 
                                                                yr)
      }
      
      for (current_dev_index in seq_len(current_program_params$intervention_vec[yr])){   # cycle through number of developments and associated offsets
        
        global_object$dev_credit_to_use = assess_credit(global_object, current_program_params)
        
        if (current_program_params$use_dev_credit == TRUE){
          
          global_object$dev_credit_to_use = assess_credit(global_object, current_program_params)
          
          global_object$credit_match_object = develop_from_credit(global_object$current_ecology, 
                                             global_object$dev_credit_to_use, 
                                             global_params,
                                             current_program_params,
                                             intervention_vec = current_program_params$intervention_vec, 
                                             dev_ind_available = global_object$index_object$ind_available[[region_ind]], 
                                             global_object$decline_rates, 
                                             global_object$land_parcels,
                                             region_ind,
                                             yr, 
                                             time_horizon)
      
          if (global_object$credit_match_object$match_flag == TRUE){
            #stopifnot(global_object$credit_match_object$match_flag == FALSE)
            global_object$dev_credit = global_object$credit_match_object$dev_credit
            print(paste('credit flag ', global_object$credit_match_object$match_vals))
            global_object <- perform_clearing_routine(global_object, 
                                                      clearing_type = 'develop_from_credit', 
                                                      current_development_object = global_object$credit_match_object$development_object,
                                                      region_ind, 
                                                      global_params)
          }
          
        } 
        
        if ( (global_object$credit_match_object$match_flag == FALSE && current_program_params$use_parcel_sets == TRUE)){  #if insufficient credits accumulated, perform offset parcel set match
          print('credit fail flag')
          print(global_object$dev_credit_to_use)
          global_object$match_object <- match_parcel_set(offset_pool_object = global_object$offset_pool_object, 
                                           global_object$dev_credit_to_use, 
                                           global_params, 
                                           current_program_params, 
                                           intervention_vec = current_program_params$intervention_vec, 
                                           ind_available = global_object$index_object$ind_available[[region_ind]], 
                                           global_object$current_ecology, 
                                           decline_rates = global_object$decline_rates, 
                                           parcels$land_parcels, 
                                           yr, 
                                           time_horizon, 
                                           region_ind)  #perform the matching routine - i.e. find a matching development/offset set.
          
          if (global_object$match_object$match_flag == TRUE){
            
            print('match flag')
            global_object$recorded_dev_credit = append(global_object$recorded_dev_credit, global_object$match_object$dev_credit)
            global_object$dev_credit = global_object$match_object$dev_credit
            global_object <- perform_offset_routine(global_object, global_object$match_object, current_program_params, region_ind, global_params)
            global_object <- perform_clearing_routine(global_object, 
                                                      clearing_type = 'development', 
                                                      current_development_object = global_object$match_object$current_development_object, 
                                                      region_ind, 
                                                      global_params)
            
          } 
        } 
        
        if ( (global_object$credit_match_object$match_flag == FALSE) & (global_object$match_object$match_flag == FALSE) ){
          print('match break flag')
          break
        }
        
      }
      
      if (global_params$perform_illegal_clearing == TRUE){
        global_object <- perform_illegal_clearing(global_object, global_object$current_ecology, land_parcel_num = parcels$land_parcel_num, 
                                                  yr, region_ind, current_program_params, global_params, decline_rates_initial, time_horizon)
      }
      
    }
    
    if ( (length(unlist(global_object$offsets_object$parcel_indexes)) > 0) & 
         (global_params$limit_offset_restoration == TRUE) & 
         current_program_params$offset_action_type == 'restore'){
      
      
     assessed_parcel_sets_object <- assess_parcel_sets(global_object$current_ecology, 
                                              global_object$offsets_object, 
                                              offset_indexes = global_object$index_object$offsets,
                                              global_params, 
                                              current_program_params,
                                              decline_rates_initial,
                                              time_horizon, 
                                              yr)
      
      global_object$decline_rates <- update_decline_rates(global_object$decline_rates, 
                                                          restoration_rate = current_program_params$restoration_rate, 
                                                          dims_to_use = global_params$dims_to_use, 
                                                          eco_dims = global_params$eco_dims, 
                                                          decline_rate_type = 'offset', 
                                                          offset_action_type = 'maintain', 
                                                          parcel_indexes = assessed_parcel_sets_object$site_success_inds)
    }
    
    global_object$current_ecology <- kill_development_ecology(global_object$current_ecology, global_object$decline_rates, global_params$eco_dims)
    
    global_object$trajectories <- update_trajectories(global_object$trajectories, global_params$eco_dims, global_object$current_ecology, yr)
    
    global_object$current_ecology <- project_current_system_multi(global_object$current_ecology, 
                                                                  decline_rates = global_object$decline_rates,
                                                                  min_eco_val = global_params$min_eco_val, 
                                                                  max_eco_val = global_params$max_eco_val,
                                                                  max_restoration_eco_val = global_params$max_eco_val, 
                                                                  time_horizon = 1, 
                                                                  eco_dims = global_params$eco_dims)     # update ecology for subsequent time step using current decline rates
    print(yr)
    
  }
  
  #   for (region_ind in seq_len(global_params$region_num)){
  #     if (program_params[[region_ind]]$offset_bank_type == 'credit'){
  #       offsets_object <- append_current_object(offsets_object, offset_bank, parcel_set_count = 1)           # if using o
  #       dev_object <- dev_credit_object
  #     }
  #   }
  
  return(global_object)
  
} 


remove_parcel_from_current_pool <- function(offset_pool_object, current_parcel_indexes){
  subset_pool_to_remove <- list_intersect(offset_pool_object$parcel_indexes, current_parcel_indexes)
  subset_pool_to_use <- seq_along(offset_pool_object$parcel_indexes)[-subset_pool_to_remove$match_ind]
  offset_pool_object <- select_subset(offset_pool_object, subset_pool_to_use)
  return(offset_pool_object)
}
# remove_dev_parcel_from_current_pool <- function(pool_object, dev_ind){
#   ind_to_remove <- which(pool_object$parcel_indexes %in% dev_ind)
#   updated_pool_object <- lapply(seq_along(pool_object), function(i) pool_object[[i]][-ind_to_remove])
#   names(updated_pool_object) <- names(pool_object)
#   return(updated_pool_object)
# }


select_inds_to_clear <- function(index_object, current_program_params, land_parcel_num){
  
  parcel_inds <- seq_len(land_parcel_num)                                                              #create vector of all land parcel indicies
  inds_to_remove <- c(unlist(index_object$illegal_clearing), unlist(index_object$developments), unlist(index_object$offsets), unlist(index_object$banked_offset_pool))             #determine which indicies have already been cleared
  
  if (length(inds_to_remove) > 0){
    parcel_inds <- parcel_inds[-inds_to_remove]                                                #remove developed indicies
  }
  
  clearing_thresh <- rep(current_program_params$illegal_clearing_prob, length(parcel_inds))
  discrim <- runif(length(clearing_thresh)) < clearing_thresh                               #sample over uniform random vector, indicies less than the threshold level are selected for illegal clearing
  inds_to_clear <- parcel_inds[discrim]
  return(inds_to_clear)
}



perform_illegal_clearing <- function(global_object, current_ecology, land_parcel_num, yr, region_ind, current_program_params, 
                                     global_params, decline_rates_initial, time_horizon){
  
  parcel_num_remaining = length(global_object$index_object$ind_available[[region_ind]])
  decline_rates <- global_object$decline_rates
  inds_to_clear <- select_inds_to_clear(index_object = global_object$index_object, current_program_params, land_parcel_num = parcels$land_parcel_num)
  if (length(inds_to_clear) > 0){
    dev_pool_object <- record_current_parcel_set(current_ecology[inds_to_clear], 
                                                 inds_to_clear, 
                                                 parcel_num_remaining, 
                                                 yr, 
                                                 region_ind) #record potential current development parcel attributes
    
    dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, 
                                           pool_type = 'devs', 
                                           calc_type = current_program_params$dev_calc_type, 
                                           cfacs_flag = current_program_params$dev_cfacs_flag, 
                                           adjust_cfacs_flag = current_program_params$adjust_dev_cfacs_flag, 
                                           offset_restoration_flag = current_program_params$offset_restoration_flag,
                                           include_potential_developments = current_program_params$include_potential_developments_in_dev_calc,
                                           include_potential_offsets = current_program_params$include_potential_offsets_in_dev_calc,
                                           include_illegal_clearing = current_program_params$include_illegal_clearing_in_dev_calc,
                                           time_horizon_type = 'future',
                                           global_params, 
                                           current_program_params,
                                           decline_rates, 
                                           time_horizon, 
                                           yr)  #determine future development parcel attributes
    
    global_object <- perform_clearing_routine(global_object, clearing_type = 'illegal', current_development_object = dev_pool_object, region_ind, global_params) 
  }
  return(global_object)
}

perform_offset_routine <- function(global_object, match_object, current_program_params, region_ind, global_params){
  
  index_object = global_object$index_object
  decline_rates = global_object$decline_rates
  offsets_object = global_object$offsets_object
  
  current_offset_object <- match_object$offset_object
  current_offset_indexes = current_offset_object$parcel_indexes
  
  if (current_program_params$use_offset_bank == TRUE){
    
    banked_offset_inds_used = list_intersect(index_object$banked_offset_pool[[region_ind]], current_offset_indexes)         # determine parcels used in matching routine and remove from available pool
    index_object$banked_offset_pool[[region_ind]] = index_object$banked_offset_pool[[region_ind]][-banked_offset_inds_used$match_ind]
    
  } else {
    index_object <- update_ind_available(index_object, current_offset_indexes, region_ind)         # determine parcels used in matching routine and remove from available pool
    decline_rates <- update_decline_rates(decline_rates, 
                                          restoration_rate_params = global_params$restoration_rate_params, 
                                          spread_restoration_rate = global_params$spread_restoration_rate,
                                          dims_to_use = global_params$dims_to_use, 
                                          eco_dims = global_params$eco_dims, 
                                          decline_rate_type = 'offset', 
                                          offset_action_type = current_program_params$offset_action_type, 
                                          current_offset_indexes) # set elements in decline rates array corresponding to offsets to restoration rates
  }
  

#   subset_pool_to_remove <- list_intersect(offset_pool_object$parcel_indexes, current_offset_indexes)
#   subset_pool_to_use <- seq_along(offset_pool_object$parcel_indexes)[-subset_pool_to_remove$match_ind]
#   offset_pool_object <- select_subset(offset_pool_object, subset_pool_to_use)
  
  index_object <- record_parcel_index(update_type = 'offset', index_object, parcel_indexes = current_offset_indexes, region_ind)
  offsets_object <- append_current_object(offsets_object, 
                                          current_offset_object, 
                                          append_type = 'as_list')      #record current offset parcels in offsets object containing all offsets info
  
  
  global_object$offset_pool_object <- remove_parcel_from_current_pool(global_object$offset_pool_object, current_parcel_indexes = current_offset_indexes)
  global_object$index_object <- index_object
  global_object$offsets_object <- offsets_object
  global_object$decline_rates <- decline_rates
  return(global_object)
}


# clearing_type = 'development' 
# current_development_object = match_object$current_development_object 

perform_clearing_routine <- function(global_object, clearing_type, current_development_object, region_ind, global_params){
  
  index_object = global_object$index_object
  decline_rates = global_object$decline_rates
  current_dev_indexes <- current_development_object$parcel_indexes
  index_object <- update_ind_available(index_object, current_dev_indexes, region_ind)                 #remove development parcels from available pool
  index_object <- record_parcel_index(update_type = clearing_type, 
                                      index_object, 
                                      parcel_indexes = current_dev_indexes, 
                                      region_ind)
  
  if (clearing_type == 'development'){
    global_object$dev_object <- append_current_object(global_object$dev_object, 
                                                      current_development_object, 
                                                      append_type = 'as_list')
  } else if (clearing_type == 'develop_from_credit'){
    global_object$dev_credit_object <- append_current_object(global_object$dev_credit_object,
                                                            current_development_object,   
                                                            append_type = 'as_list')
  } else if (clearing_type == 'illegal'){
    global_object$illegal_clearing <- append_current_object(global_object$illegal_clearing, 
                                                            current_development_object, 
                                                            append_type = 'as_list')
  }  
  
  decline_rates <- update_decline_rates(decline_rates, 
                                        restoration_rate_params = vector(),
                                        spread_restoration_rate = vector(),
                                        dims_to_use = global_params$dims_to_use, 
                                        eco_dims = global_params$eco_dims, 
                                        decline_rate_type = 'development', 
                                        offset_action_type = vector(), 
                                        current_dev_indexes)     # set elements corresponding to developed parcels in decline rates array to zero. decline_rates <- current_dev_index_object$decline_rates
  
  if (length(current_dev_indexes) > 0){
    global_object$offset_pool_object <- remove_parcel_from_current_pool(global_object$offset_pool_object, current_parcel_indexes = current_dev_indexes)
  }
  global_object$index_object <- index_object
  global_object$decline_rates <- decline_rates
  
  return(global_object)
}  


assess_credit <- function(global_object, current_program_params){
  
  if (current_program_params$use_parcel_sets == FALSE){
    offset_credit = nested_list_sum(global_object$offset_pool_object$parcel_vals_used)
    dev_sum = nested_list_sum(global_object$dev_credit_object$parcel_vals_used)
    if (length(dev_sum) > 0){
      dev_credit = unlist(subtract_nested_lists(offset_credit, dev_sum))
    } else{
      dev_credit = offset_credit
    }
  } else{
    dev_credit = global_object$dev_credit
  }
  
  return(dev_credit)
}



# perform_development_from_credit_routine <- function(global_object, current_program_params, current_ecology, dev_credit, region_ind, current_offset_pool, land_parcels,
#                                                     yr, time_horizon, global_params){
#   
#   dev_credit_object = global_object$dev_credit_object
#   index_object = global_object$index_object
#   decline_rates = global_object$decline_rates 
#   
#   if (current_program_params$use_offset_bank == TRUE){
#     if (length(global_object$dev_object$parcel_vals_used) > 0){
#       dev_credit = subtract_nested_lists(nested_list_sum(global_object$offset_pool_object$parcel_vals_used), nested_list_sum(global_object$dev_object$parcel_vals_used))
#     } else {
#       dev_credit = nested_list_sum(global_object$offset_pool_object$parcel_vals_used)
#     }
#   } 
#   
#   match_object = develop_from_credit(current_ecology, 
#                                      dev_credit, 
#                                      global_params,
#                                      current_program_params,
#                                      intervention_vec = current_program_params$intervention_vec, 
#                                      dev_ind_available = global_object$index_object$ind_available[[region_ind]], 
#                                      decline_rates, 
#                                      land_parcels, 
#                                      yr, 
#                                      time_horizon)
#   
#   current_development_object = match_object$development_object
#   if (match_object$match_flag == TRUE){
#     dev_credit_object = append_current_object(dev_credit_object, 
#                                                          current_development_object, 
#                                                          append_type = 'as_list')
#     
#     index_object <- update_ind_available(index_object, match_object$development_object$parcel_indexes, region_ind)                 #remove development parcels from available pool
#     index_object <- record_parcel_index(update_type = 'development', index_object, parcel_indexes = match_object$development_object$parcel_indexes, region_ind)
#     decline_rates <- update_decline_rates(decline_rates, 
#                                           restoration_rate_params = global_params$restoration_rate_params, 
#                                           dims_to_use = global_params$dims_to_use, 
#                                           eco_dims = global_params$eco_dims, 
#                                           decline_rate_type = 'development', 
#                                           offset_action_type = vector(), 
#                                           match_object$development_object$parcel_indexes)     # set elements corresponding to developed parcels in decline rates array to zero.
#     
#    global_object$dev_credit_object = dev_credit_object
#    global_object$index_object <- index_object
#    global_object$decline_rates <- decline_rates
#    return(global_object)
#   } else {
#     return(global_object)
#   }
# }


# update_development_index <- function(current_dev_indexes, index_object, region_ind, decline_rates, current_program_params, global_params){
#   
#   index_object <- update_ind_available(index_object, current_dev_indexes, region_ind)                 #remove development parcels from available pool
#   index_object <- record_parcel_index(update_type = 'development', index_object, parcel_indexes = current_dev_indexes, region_ind)
#   decline_rates <- update_decline_rates(decline_rates, 
#                                         restoration_rate_params = global_params$restoration_rate_params, 
#                                         dims_to_use = global_params$dims_to_use, 
#                                         eco_dims = global_params$eco_dims, 
#                                         decline_rate_type = 'development', 
#                                         offset_action_type = vector(), 
#                                         current_dev_indexes)     # set elements corresponding to developed parcels in decline rates array to zero.
#   updated_dev_index_object = list()
#   updated_dev_index_object$decline_rates <- decline_rates
#   updated_dev_index_object$index_object <- index_object
#   return(updated_dev_index_object)
# }

select_offset_index_pool <- function(index_object, current_program_params, region_ind){
  if (current_program_params$use_offset_bank == TRUE){
    
    if (current_program_params$offset_region == 'development'){
      current_pool = index_object$banked_offset_pool[[region_ind]]
    } else if (current_program_params$offset_region == 'all'){
      current_pool = unlist(index_object$banked_offset_pool)
    }
    
  }  else if (current_program_params$use_offset_bank == FALSE){
    if (current_program_params$offset_region == 'development'){
      current_pool = index_object$ind_available[[region_ind]]
    } else if (current_program_params$offset_region == 'all'){
      current_pool = unlist(index_object$ind_available)
    }
  }
  
  if (length(current_pool) == 0){ #break out when no parcels are left in banking pool
    index_object$empty_pool_flag = TRUE
  } else {
    index_object$empty_pool_flag = FALSE
    index_object$pool = current_pool
  }
  return(index_object)
  
}



prepare_offset_pool <- function(current_offset_pool, current_program_params, index_object, offset_bank, decline_rates, 
                                decline_rates_initial, region_ind, parcels, current_ecology, global_params, time_horizon, yr){
  
  if (current_program_params$use_offset_bank == TRUE){
    subset_pool = which(unlist(offset_bank$parcel_indexes) %in% parcels$regions[[region_ind]])
    current_offset_bank <- select_subset(offset_bank, subset_pool)
    
    offset_pool_object <- prepare_offset_bank(current_offset_bank, #current_offset_bank = offset_bank[[region_ind]]
                                              current_offset_pool,
                                              offset_restoration_flag = current_program_params$offset_restoration_flag, 
                                              parcels$land_parcels, 
                                              current_ecology, 
                                              eco_dims = global_params$eco_dims)   #arrange current banked offset data into form to use in parcel set determination
    offset_pool_type = 'offset_bank'
    
  } else {
    
    parcel_num_remaining = length(current_offset_pool)
    
    offset_pool_object <- record_current_parcel_set(current_ecology[current_offset_pool], 
                                                    current_offset_pool, 
                                                    parcel_num_remaining, 
                                                    yr, 
                                                    region_ind)   #arrange available parcel pool into form to use in parcel set determination
    offset_pool_type = 'offsets'
  }
  
  offset_pool_object <- assess_current_pool(pool_object = offset_pool_object, 
                                            pool_type = offset_pool_type, 
                                            calc_type = current_program_params$offset_calc_type, 
                                            cfacs_flag = current_program_params$offset_cfacs_flag, 
                                            adjust_cfacs_flag = current_program_params$adjust_offset_cfacs_flag, 
                                            offset_restoration_flag = current_program_params$offset_restoration_flag,
                                            include_potential_developments = current_program_params$include_potential_developments_in_offset_calc,
                                            include_potential_offsets = current_program_params$include_potential_offsets_in_offset_calc,
                                            include_illegal_clearing = current_program_params$include_illegal_clearing_in_offset_calc,
                                            time_horizon_type = current_program_params$offset_time_horizon_type,
                                            global_params,
                                            current_program_params, 
                                            decline_rates,
                                            time_horizon, 
                                            yr)      #determine available parcel values, depending on what particular offset policy is in use using counterfactuals etc.
  
  return(offset_pool_object)
}



add_to_bank <- function(global_object, offset_bank_num, current_program_params, yr, current_ecology, region_ind, global_params){
  
  if (offset_bank_num == 0){
    return(global_object)
  }
  
  index_object = global_object$index_object 
  offset_bank = global_object$offset_bank 
  decline_rates = global_object$decline_rates 
  total_current_pool = index_object$ind_available[[region_ind]]             # determine parcel indexes currently available
  index_object$parcel_num_remaining = length(total_current_pool)            # record how many parcels remaining
  current_banked_offset_pool <- select_banked_offset_indexes(offset_bank_num, total_current_pool)   # select current number of offset parcels from current available pool to add to banked offset pool
  
  current_banked_offset <- record_current_parcel_set(current_ecology[current_banked_offset_pool], 
                                                     current_pool = current_banked_offset_pool, 
                                                     parcel_num_remaining = index_object$parcel_num_remaining, 
                                                     yr, 
                                                     region_ind)   # arrange current parcel data  
  
  offset_bank = append_current_object(offset_bank, current_banked_offset, append_type = 'as_list')
  
  index_object <- update_banked_offset_pool(index_object,  # add new offset parcels to banked offset pool and remove those parcels from available pool
                                            current_banked_offset_pool, 
                                            region_ind)  
  
  decline_rates <- update_decline_rates(decline_rates,     # set decline rate parameters to offset
                                        restoration_rate_params = global_params$restoration_rate_params,
                                        spread_restoration_rate = global_params$spread_restoration_rate,
                                        dims_to_use = global_params$dims_to_use, 
                                        eco_dims = global_params$eco_dims, 
                                        decline_rate_type = 'offset', 
                                        offset_action_type = current_program_params$offset_action_type, 
                                        current_banked_offset_pool) 
  
  global_object$offset_bank = offset_bank
  global_object$index_object = index_object
  global_object$decline_rates = decline_rates
  
  return(global_object)
}


update_trajectories <- function(trajectories, eco_dims, current_ecology, yr){
  for (parcel_count_ind in seq_len(length(trajectories))){
    for (eco_ind in seq_len(eco_dims)){
      trajectories[[parcel_count_ind]][[eco_ind]][yr, ] = current_ecology[[parcel_count_ind]][[eco_ind]] # record current ecology in trajectories list for each eco dimension
    }
  }
  return(trajectories)
}

# offsets_object_to_assess = vector('list', length(offsets_object))
#   for (name_ind in seq_along(offsets_object)){
#     offsets_object_to_assess[[name_ind]] = unlist(offsets_object[[name_ind]], recursive = F)
#   }
# names(offsets_object_to_assess) = names(offsets_object)
# }
# 
# offsets_object_to_assess[[name_ind]] = unlist(offsets_object[[name_ind]], recursive = F)




find_region <- function(parcel_index, regions){
  region_match <- lapply(seq_along(regions), function(i) which(parcel_index %in% regions[[i]]))
  return(region_match)
}

# 
# offsets_object = global_object$offsets_object 
# dev_object = global_object$dev_object 
# decline_rates = global_object$decline_rates
# offset_indexes = global_object$index_object$offsets
# current_ecology = global_object$current_ecology

assess_parcel_sets <- function(current_ecology, offsets_object, offset_indexes, global_params, current_program_params, decline_rates_initial, time_horizon, yr){
  
  assessed_parcel_sets_object <- list()
  eco_dims = global_params$eco_dims
  parcel_set_count = length(offset_indexes)
  metric <- vector('list', parcel_set_count)
  
  for (parcel_set_ind in seq_len(parcel_set_count)){
    current_parcel_indexes <- unlist(offset_indexes[[parcel_set_ind]])
    current_parcel_set <- which(unlist(offsets_object$parcel_indexes) %in% current_parcel_indexes)
    current_offset_object <- lapply(seq_along(offsets_object), function(i) offsets_object[[i]][current_parcel_set])
    names(current_offset_object) <- names(offsets_object)
    
    parcel_vals_achieved <- assess_current_gain_pool(current_ecology, 
                                                     pool_object = current_offset_object, 
                                                     pool_type = "offset_bank", 
                                                     calc_type = current_program_params$offset_calc_type, 
                                                     cfacs_flag = current_program_params$offset_cfacs_flag, 
                                                     adjust_cfacs_flag = current_program_params$adjust_offset_cfacs_flag, 
                                                     include_potential_developments = current_program_params$include_potential_developments_in_offset_calc,
                                                     include_potential_offsets = current_program_params$include_potential_offsets_in_offset_calc,
                                                     include_illegal_clearing = current_program_params$include_illegal_clearing_in_offset_calc,
                                                     time_horizon_type = current_program_params$offset_time_horizon_type,
                                                     global_params, 
                                                     current_program_params, 
                                                     decline_rates_initial, 
                                                     time_horizon, 
                                                     yr)      
    
    for (eco_ind in seq_len(eco_dims)){
      metric[[parcel_set_ind]] = nested_list_sum(subtract_nested_lists(parcel_vals_achieved, current_offset_object$parcel_vals_used))
    }
  }
  
  parcel_set_success_inds <- unlist(lapply(seq_along(metric), function(i) (all(unlist(metric[[i]][global_params$dims_to_use]) > 0))))
  
  site_success_inds = which(unlist(offsets_object$parcel_indexes) %in% unlist(offset_indexes[parcel_set_success_inds]))
  assessed_parcel_sets_object$site_success_inds <- offsets_object$parcel_indexes[site_success_inds]
  assessed_parcel_sets_object$metric <- metric
  
  return(assessed_parcel_sets_object)
}


# pool_object = current_offset_object 
# pool_type = "offset_bank" 
# calc_type = current_program_params$offset_calc_type 
# cfacs_flag = current_program_params$offset_cfacs_flag 
# adjust_cfacs_flag = current_program_params$adjust_offset_cfacs_flag 
# include_potential_developments = current_program_params$include_potential_developments_in_offset_calc
# include_potential_offsets = current_program_params$include_potential_offsets_in_offset_calc
# include_illegal_clearing = current_program_params$include_illegal_clearing_in_offset_calc
# offset_restoration_flag = current_program_params$offset_restoration_flag
# time_horizon_type = current_program_params$offset_time_horizon_type



assess_current_gain_pool <- function(current_ecology, pool_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, 
                                     include_potential_developments,include_potential_offsets,include_illegal_clearing,
                                     time_horizon_type, global_params, current_program_params, decline_rates_initial, time_horizon, yr){
  
  current_pool = unlist(pool_object$parcel_indexes)
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  
  time_horizons <- generate_time_horizons(time_horizon_type = pool_type, project_type = 'current', yr, offset_yrs, time_horizon, parcel_count)
  
  if (cfacs_flag == TRUE){
    
    cfacs_object = calc_cfacs(parcel_ecologies = pool_object$parcel_ecologies, 
                              parcel_num_remaining = pool_object$parcel_num_remaining, 
                              global_params, 
                              current_program_params, 
                              decline_rates = decline_rates_initial[current_pool], 
                              time_horizons, 
                              offset_yrs, 
                              include_potential_developments,
                              include_potential_offsets,
                              include_illegal_clearing, 
                              adjust_cfacs_flag)
    
    cfac_vals = nested_list_tail(cfacs_object$cfac_trajs)
    
  }
  restoration_vals = current_ecology[current_pool]
  restoration_vals = lapply(seq_along(restoration_vals), function(i) lapply(seq_along(restoration_vals[[i]]), function(j) sum(restoration_vals[[i]][[j]] )))
  
  parcel_vals_achieved = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, restoration_vals, cfac_vals)
  
  return(parcel_vals_achieved)
}




simplify_list_to_array <- function(collated_list){
  collated_array = simplify2array(collated_list)
  collated_dims = dim(collated_array)
  dim(collated_array) = c(collated_dims[1], collated_dims[4], 1)
  return(collated_array)
}




generate_intervention_vec <- function(time_steps, prog_start, prog_end, total_prog_num, sd){
  intervention_vec = array(0, time_steps)
  intervention_vec[prog_start:prog_end] = split_vector((prog_end - prog_start + 1), total_prog_num, sd, min_width = -1)
  return(intervention_vec)
}

split_vector <- function(N, M, sd, min_width) {               # make a vector of length N where the elements sum to M and with values normally distributed about M/N with std dev "sd"
  
  vec <- rnorm(N, M/N, sd)                                    # select vector from normal distribution
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


initialise_parcels_from_data <- function(filename){
  
  img = read.pnm(file = filename, cellres = 1)
  img_array = img@grey
  land_index_vals = unique(as.vector(img_array))
  landscape_dims = dim(img_array)
  land_parcels <- lapply(seq_along(land_index_vals), function(i) which(img_array == land_index_vals[i]))
  regions = list()
  regions[[1]] = seq_len(length(land_parcels))
  region_num = length(regions)
  parcels = list()
  parcels$landscape_dims = landscape_dims
  parcels$parcel_indexes = seq_along(land_parcels)
  parcels$land_parcel_num = length(land_parcels)
  parcels$land_parcels = land_parcels
  parcels$regions = regions
  parcels$region_num = region_num
  parcels$img_array = img_array
  return(parcels)
}

initialise_ecology_from_data <- function(filename, land_parcels, eco_dims){
  img = read.pnm(file = filename, cellres = 1)
  landscape = list()
  img_to_use = img@grey
  zero_inds = which(img_to_use == 0)
  img_to_use[zero_inds] = runif(length(zero_inds), 0, 1e-10)
  landscape[[1]] = 100*img_to_use
  initial_ecology <- split_landscape_to_land_parcels(landscape, land_parcels, eco_dims)
  return(initial_ecology)
}


initialise_shape_parcels <- function(global_params){
  parcels = list()
  parcels$landscape_dims = c(global_params$ecology_size, global_params$ecology_size)
  parcel_num_x = global_params$parcel_num_x   #length in parcels of array in x 
  parcel_num_y = global_params$parcel_num_y #length in parcels of array in y 
  parcel_vx = split_vector(parcel_num_x, global_params$ecology_size, sd = 5, min_width = 3) # make normally distributed vector that sums to ecology size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, global_params$ecology_size, sd = 5, min_width = 3) # as above for y
  
  pixel_indexes = 1:(global_params$ecology_size*global_params$ecology_size)     #index all elements of ecology array
  dim(pixel_indexes) = c(global_params$ecology_size, global_params$ecology_size)  # arrange ecology array index vector into array of landscape dimensions 
  land_parcels = mcell(pixel_indexes, parcel_vx, parcel_vy) #split the ecology array into a series of subarrays with dimensions sz_x by sz_y
  land_parcel_num = length(land_parcels$elements) #total number of parcels
  parcel_indexes = 1:land_parcel_num #index all parcels
  dim(parcel_indexes) = c(parcel_num_y, parcel_num_x) #arrange indicies into array with dimensions of land parcels
  region_vx = split_vector(global_params$region_num_x, parcel_num_x, 1, min_width = 3) # perform similar operation used to split array into smallest elements, but this time for land parcels, arranging into regions
  region_vy = split_vector(global_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  regions = mcell(parcel_indexes, region_vx, region_vy)   # split land parcel indexes into regions
  
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


initialise_index_object <- function(parcels, initial_ecology, global_params){
  index_object = list()
  index_object$ind_available = vector('list', parcels$region_num)
  index_object$developments = list()
  index_object$offsets = list()
  index_object$illegal_clearing = list()
  index_object$dev_credit = list()
  index_object$banked_offset_pool = vector('list', parcels$region_num)
  index_object$parcel_num_remaining = vector()
  index_object$break_flag = FALSE
  
  ind_available = parcels$regions
  
  parcel_lengths <- unlist(lapply(seq_along(parcels$land_parcels), function(i) length(parcels$land_parcels[[i]])))
  smalls_to_screen = which(parcel_lengths < global_params$parcel_screen_size)
  initial_parcel_sums = unlist(lapply(seq_along(initial_ecology), function(i) sum(initial_ecology[[i]][[1]])))
  zeros_to_screen = which(initial_parcel_sums < 1e-5)
  
  if (global_params$screen_parcels == TRUE){
    parcel_dist = quantile(initial_parcel_sums[-c(zeros_to_screen, smalls_to_screen)], probs = c(0.05, 0.95))
    parcels_to_screen = c(which(initial_parcel_sums < parcel_dist[1]), which(initial_parcel_sums > parcel_dist[2]))
  } else {
    parcels_to_screen = zeros_to_screen
  }
  
  parcels_to_screen = unique(c(parcels_to_screen, smalls_to_screen))
  
  for (region_ind in seq_len(parcels$region_num)){
    current_region = ind_available[[region_ind]]
    inds_to_remove = which(current_region %in% parcels_to_screen)
    if (length(inds_to_remove) > 0){
      ind_available[[region_ind]] = ind_available[[region_ind]][-inds_to_remove]
    }
  }
  index_object$ind_available = ind_available
  index_object$landscape_inds = ind_available
  
  return(index_object)
  
}




list_of_zeros <- function(list_dims, array_dims){
  list_object = vector('list', list_dims)
  for (list_ind in seq_len(list_dims)){
    list_object[[list_ind]] = array(0, array_dims)
  }
  return(list_object)
}





mcell <- function(x, vx, vy){       #used to break up array into samller set of sub arrays defined by vx and vy that fit together to give input array
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  B = vector('list', rows*cols)   # make an array composed of lists with dimenisons that define the land parcels/regions. The list format allows arrays of different sizes to be stored
  colStart = 0
  for (i in seq_len(cols)){       # run down through the columns of input array 
    rowStart = 0
    for (j in seq_len(rows)){ #group elements of input array into sub arrays and assign to B
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



ind2sub <- function(rows, ind){       # give an array with N rows, return location of array element "ind" in loc = [x, y] format
  rw = ((ind-1) %% rows) + 1          # identify row of current element using mod format
  cl = floor((ind-1) / rows) + 1      
  loc = c(rw, cl)
  return(loc)
}


project_ecology <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_horizon, time_fill){
  
  if (time_fill == TRUE){
    time_vec = 0:time_horizon
  } else {time_vec = time_horizon}
  if ( (decline_rate != 0) & (parcel_vals > min_eco_val)){
    t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
    eco_projected = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_vec - t_sh)))
  } else {
    eco_projected = rep(parcel_vals, length(time_vec))
  }
  
  return(eco_projected)
}



predict_parcel_traj <- function(current_parcel_ecology, action_type, min_eco_val, max_eco_val, eco_dims, restoration_rate_params, current_dec_rate, time_horizon, time_fill){
  
  projected_ecology = vector('list', eco_dims)
  
  if (action_type == 'maintain'){
    current_dec_rate = rep(list(1e-10), eco_dims)
  } else if (action_type == 'restore'){
    current_dec_rate = rep(list(restoration_rate_params[1]), eco_dims)
  }
  
  for (eco_ind in seq_len(eco_dims)){
    current_ecology_slice = current_parcel_ecology[[eco_ind]]
    
    current_predicted_ecology = sapply(current_ecology_slice, project_ecology, min_eco_val, max_eco_val, 
                                       decline_rate = current_dec_rate[[eco_ind]], time_horizon = time_horizon, time_fill)  # update ecology according to ecological curve in project_ecology function (currently logistic) - curve parameters are contained in decline_rates array
    if (time_horizon == 0){
      dim(current_predicted_ecology) = c(1, length(current_predicted_ecology))
    }
    projected_ecology[[eco_ind]] = current_predicted_ecology
  }
  return(projected_ecology)
  
}


ecology_to_parcels <- function(current_ecology, land_parcels){
  parcel_num = length(land_parcels)
  parcel_ecologies <- vector('list', parcel_num)
  for (parcel_ind in 1:parcel_num){
    current_parcel = land_parcels[[parcel_ind]]
    parcel_ecologies[[parcel_ind]] = extract_parcel(current_parcel, current_ecology)
  }
  return(parcel_ecologies)
}


calc_parcel_trajs <- function(current_parcel_ecologies, action_type, current_decline_rates, current_time_horizons, global_params, restoration_rate_params, time_fill){
  parcel_num = length(current_parcel_ecologies)
  parcel_trajs <- generate_nested_list(outer_dim = parcel_num, inner_dim = global_params$eco_dims)
  
  if (length(current_decline_rates) != parcel_num){
    print('length error')
  }
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ecology = current_parcel_ecologies[[parcel_count_ind]]
    parcel_trajs[[parcel_count_ind]] = predict_parcel_traj(current_parcel_ecology, 
                                                           action_type, 
                                                           global_params$min_eco_val, 
                                                           global_params$max_eco_val,
                                                           eco_dims = global_params$eco_dims,
                                                           restoration_rate_params,
                                                           current_dec_rate = current_decline_rates[[parcel_count_ind]], 
                                                           time_horizon = unlist(current_time_horizons[parcel_count_ind]), 
                                                           time_fill)
  }
  
  return(parcel_trajs)
}


initialise_trajectories <- function(eco_dims, land_parcels, time_steps){
  parcel_num =  length(land_parcels)
  trajectories = generate_nested_list(outer_dim = parcel_num, inner_dim = eco_dims)
  for (parcel_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(eco_dims)){
      parcel_dims = length(land_parcels[[parcel_ind]])
      trajectories[[parcel_ind]][[eco_ind]] = array(0, c(time_steps, parcel_dims))
    }
  }
  return(trajectories)
}



nested_list_sum <- function(nested_list){
  
  nested_dims = length(nested_list[[1]])
  summed_list = vector('list', nested_dims)
  
  for (nested_ind in seq_len(nested_dims)){
    summed_list[[nested_ind]] <- Reduce('+', lapply(seq_along(nested_list), function(i) nested_list[[i]][[nested_ind]]))
  }
  
  return(summed_list)
}



generate_nested_list <- function(outer_dim, inner_dim){
  if (outer_dim > 0){
    nested_list <- vector('list', outer_dim)
  } else {
    nested_list = list()
  }
  for (outer_ind in seq_len(outer_dim)){
    nested_list[[outer_ind]] <- vector('list', inner_dim)
  }
  return(nested_list)
}


initialise_ecology <- function(global_params, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
  
  parcel_num = length(land_parcels)
  eco_dims = global_params$eco_dims
  initial_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = global_params$eco_dims)
  
  for (parcel_ind in seq_len(parcel_num)){  
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (eco_ind in seq_len(eco_dims)){
      mean_eco_val = global_params$min_initial_eco_val + (global_params$max_initial_eco_val - global_params$min_initial_eco_val - global_params$initial_eco_noise)*runif(1)
      initial_ecology[[parcel_ind]][[eco_ind]] =  array(mean_eco_val, parcel_dims) + global_params$initial_eco_noise*array(runif(length(current_parcel)), c(parcel_dims))
    }
    
  }
  
  return(initial_ecology)
}


# sample_decline_rate = global_params$sample_decline_rate
# mean_decline_rates = global_params$mean_decline_rates
# decline_rate_std = 1e-4
initialise_decline_rates <- function(parcels, sample_decline_rate, mean_decline_rates, decline_rate_std, eco_dims){
  
  regions = parcels$regions
  land_parcels = parcels$land_parcels
  parcel_num = length(land_parcels)
  decline_rates = generate_nested_list(parcel_num, eco_dims)
  
  for (eco_ind in seq_len(eco_dims)){
    for (region_ind in seq_len(length(regions))){
      current_region = regions[[region_ind]]
      current_parcel_num = length(current_region)    
      decline_params = c(length(current_region), mean_decline_rates[region_ind, eco_ind], decline_rate_std)
      
      if (sample_decline_rate == TRUE){
        region_decline_rates = array(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), length(current_region))
      } else {
        region_decline_rates = array(rep(mean_decline_rates[region_ind, eco_ind], length(current_region), length(current_region)))
      }
      
      for (region_parcel_count_ind in seq_len(length(current_region))){
        parcel_ind = current_region[region_parcel_count_ind]
        decline_rates[[parcel_ind]][[eco_ind]] = region_decline_rates[region_parcel_count_ind]
      }
    }
  }
  
  return(decline_rates)
  
}



select_rand_index <- function(ind_available, parcel_num){
  parcel_indexes = ind_available[sample(1:length(ind_available), parcel_num)]
  return(parcel_indexes)
}


# offset_pool_object = global_object$offset_pool_object 
# intervention_vec = current_program_params$intervention_vec 
# ind_available = global_object$index_object$ind_available[[region_ind]] 
# dev_credit = global_object$dev_credit
# decline_rates = global_object$decline_rates 
# land_parcels = parcels$land_parcels 
# current_ecology = global_object$current_ecology




match_parcel_set <- function(offset_pool_object, dev_credit, global_params, current_program_params, 
                             intervention_vec, ind_available, current_ecology, decline_rates, land_parcels, yr, time_horizon, region_ind){
  
  match_object = list()
  match_object$match_flag = FALSE
  
  current_pool_vals = offset_pool_object$parcel_vals_used
  current_pool_indexes = offset_pool_object$parcel_indexes
  parcel_num_remaining = length(ind_available)
  current_match_pool = ind_available
  
  
  if (parcel_num_remaining == 0){ # exit if no parcels remain
    return(match_object)
  }
  
  dev_pool_object <- record_current_parcel_set(current_ecology[current_match_pool], 
                                               current_match_pool, 
                                               parcel_num_remaining, 
                                               yr, 
                                               region_ind) #record potential current development parcel attributes
  
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, 
                                         pool_type = 'devs', 
                                         calc_type = current_program_params$dev_calc_type, 
                                         cfacs_flag = current_program_params$dev_cfacs_flag, 
                                         adjust_cfacs_flag = current_program_params$adjust_dev_cfacs_flag, 
                                         offset_restoration_flag = current_program_params$offset_restoration_flag,
                                         include_potential_developments = current_program_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = current_program_params$include_potential_offsets_in_dev_calc,
                                         include_illegal_clearing = current_program_params$include_illegal_clearing_in_dev_calc,
                                         time_horizon_type = 'future',
                                         global_params, 
                                         current_program_params,
                                         decline_rates, 
                                         time_horizon, 
                                         yr)  #determine future development parcel attributes
  
  current_match_vals_pool = dev_pool_object$parcel_vals_used
  
  while( (match_object$match_flag == FALSE) & length(current_match_pool > 1) ){   #any potential parcel set match requires a minimum of two sites
    
    if (current_program_params$development_selection_type == 'random'){
      sample_ind = sample(x = (1:length(current_match_pool)), size = 1)
      current_test_index = current_match_pool[sample_ind]
    }
    
    vals_to_match = current_match_vals_pool[[sample_ind]]
    
    if (current_program_params$use_offset_bank == FALSE){
      dev_ind = list_intersect(current_pool_indexes, current_test_index) #find and remove index that corresponds to potiential development index
      match_pool_to_use = current_pool_indexes[-dev_ind$match_ind]       
      vals_to_use = current_pool_vals[-dev_ind$match_ind]
    } else {
      match_pool_to_use = current_pool_indexes  #if performing offset banking use any of the available banked offset pool
      vals_to_use = current_pool_vals
    }
    
    match_object <- select_from_pool(match_type = 'offset', 
                                     match_procedure = 'euclidean', 
                                     current_pool = match_pool_to_use, 
                                     vals_to_use, 
                                     dev_credit, 
                                     use_parcel_set_dev_credit = current_program_params$use_parcel_set_dev_credit, 
                                     offset_multiplier = current_program_params$offset_multiplier, 
                                     match_threshold = global_params$match_threshold, 
                                     vals_to_match_initial = vals_to_match, 
                                     current_program_params$offset_parcel_for_parcel, 
                                     dims_to_use = global_params$dims_to_use,
                                     max_offset_parcel_num = global_params$max_offset_parcel_num,
                                     yr) #perform matching routine
    
    if (match_object$match_flag == FALSE){
      
      inds_to_keep = which(lapply(seq_along(dev_pool_object$parcel_vals_used), 
                    function(i) all(unlist(subtract_nested_lists(dev_pool_object$parcel_vals_used[[i]], vals_to_match)) < 0) ) == TRUE)
      current_match_pool = dev_pool_object$parcel_indexes[inds_to_keep]     #remove current potential development from potential pool
      current_match_vals_pool = dev_pool_object$parcel_vals_used[inds_to_keep]
      print(length(current_match_pool))
      print(unlist(vals_to_match))
    }
  }
  
  if (match_object$match_flag == TRUE){
    dev_match_index = which(unlist(dev_pool_object$parcel_indexes) == current_test_index)
    match_object$current_development_object = select_subset(dev_pool_object, unlist(dev_match_index))
    subset_pool =  list_intersect(offset_pool_object$parcel_indexes, match_object$match_indexes)
    offset_object <- select_subset(pool_object = offset_pool_object, subset_pool = subset_pool$match_ind)
    match_object$offset_object = offset_object
  } else if (match_object$match_flag == FALSE){
    match_object$offset_object = list()
    # match_object$dev_credit = global_object$dev_credit
  } 
  
  return(match_object)
  
}


#     dev_pool_object <- record_current_parcel_set(current_ecology[current_test_index], 
#                                                  current_test_index, 
#                                                  parcel_num_remaining, 
#                                                  yr, 
#                                                  region_ind) #record potential current development parcel attributes

#     dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, 
#                                            pool_type = 'devs', 
#                                            calc_type = current_program_params$dev_calc_type, 
#                                            cfacs_flag = current_program_params$dev_cfacs_flag, 
#                                            adjust_cfacs_flag = current_program_params$adjust_dev_cfacs_flag, 
#                                            offset_restoration_flag = current_program_params$offset_restoration_flag,
#                                            include_potential_developments = current_program_params$include_potential_developments_in_dev_calc,
#                                            include_potential_offsets = current_program_params$include_potential_offsets_in_dev_calc,
#                                            include_illegal_clearing = current_program_params$include_illegal_clearing_in_dev_calc,
#                                            time_horizon_type = 'future',
#                                            global_params, 
#                                            current_program_params,
#                                            decline_rates, 
#                                            time_horizon, 
#                                            yr)  #determine future development parcel attributes





# intervention_vec = current_program_params$intervention_vec 
# dev_ind_available = global_object$index_object$ind_available[[region_ind]] 


# current_ecology = global_object$current_ecology 
# dev_credit = global_object$dev_credit_to_use 
# intervention_vec = current_program_params$intervention_vec 
# dev_ind_available = global_object$index_object$ind_available[[region_ind]] 
# decline_rates = global_object$decline_rates
# land_parcels = global_object$land_parcels


develop_from_credit <- function(current_ecology, dev_credit, global_params, current_program_params, intervention_vec, dev_ind_available, decline_rates, land_parcels, region_ind, yr, time_horizon){
  
  parcel_num_remaining = length(dev_ind_available)
  
  dev_pool_object <- record_current_parcel_set(current_ecology[dev_ind_available],  dev_ind_available,  parcel_num_remaining,  yr,  region_ind)
  
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, 
                                         pool_type = 'devs', 
                                         calc_type = current_program_params$dev_calc_type, 
                                         cfacs_flag = current_program_params$dev_cfacs_flag, 
                                         adjust_cfacs_flag = current_program_params$adjust_dev_cfacs_flag, 
                                         offset_restoration_flag = current_program_params$offset_restoration_flag,
                                         include_potential_developments = current_program_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = current_program_params$include_potential_offsets_in_dev_calc,
                                         include_illegal_clearing = current_program_params$include_illegal_clearing_in_dev_calc,
                                         time_horizon_type = 'future',
                                         global_params, 
                                         current_program_params,
                                         decline_rates,
                                         time_horizon, 
                                         yr)
  
  if (length(unlist(dev_pool_object$parcel_indexes) > 0)){
    match_object <- select_from_pool(match_type = 'development', 
                                   match_procedure = 'random', 
                                   current_pool = dev_pool_object$parcel_indexes, 
                                   vals_to_use = dev_pool_object$parcel_vals_used, 
                                   dev_credit, 
                                   use_parcel_set_dev_credit = FALSE, 
                                   offset_multiplier = current_program_params$offset_multiplier, 
                                   match_threshold = global_params$match_threshold, 
                                   vals_to_match_initial = dev_credit, 
                                   parcel_for_parcel = TRUE, 
                                   global_params$dims_to_use, 
                                   max_offset_parcel_num = global_params$max_offset_parcel_num, 
                                   yr)
  
  } else{
    match_object = list()
    match_object$match_flag = FALSE
  }
  
  if (match_object$match_flag == TRUE){
    subset_pool =  list_intersect(dev_pool_object$parcel_indexes, match_object$match_indexes)
    match_object$development_object = select_subset(dev_pool_object, subset_pool = subset_pool$match_ind)
  } else{
    match_object$development_object = list()
    match_object$dev_credit = dev_credit
  }
  
  return(match_object)
  
}




evaluate_parcel_vals <- function(calc_type, parcel_sums_at_offset, restoration_vals, cfac_vals){
  
  if (calc_type == 'current_condition'){
    parcel_vals_pool = parcel_sums_at_offset
  } else if (calc_type == 'restoration_gains'){
    parcel_vals_pool = subtract_nested_lists(restoration_vals, parcel_sums_at_offset)
  } else if (calc_type == 'net_gains'){
    parcel_vals_pool = subtract_nested_lists(restoration_vals, cfac_vals)
  } else if (calc_type == 'restoration_condition_value'){
    parcel_vals_pool = restoration_vals
  } else if (calc_type == 'avoided_degs'){
    parcel_vals_pool = subtract_nested_lists(parcel_sums_at_offset, cfac_vals)
  } else if ((calc_type == 'future_condition') || (calc_type == 'protected_condition')){
    parcel_vals_pool = cfac_vals 
  } else{
    parcel_vals_pool = list()
  }
  return(parcel_vals_pool)
}


subtract_nested_lists <- function(list_a, list_b){
  list_c = lapply( seq_along(list_a), function(i)  mapply('-', list_a[[i]], list_b[[i]], SIMPLIFY = FALSE))
  return(list_c)
}




sum_nested_lists <- function(list_to_sum){
  
  summed_list = list_to_sum[[1]]
  if (length(list_to_sum) > 1){
    for (list_ind in 2:length(list_to_sum)){
      current_list = list_to_sum[[list_ind]]
      summed_list = lapply(seq_along(summed_list), function(i) mapply('+', summed_list[[i]], current_list[[i]], SIMPLIFY = FALSE))
    }
  }
  
  return(summed_list)
}


subtract_lists <- function(list_a, list_b){
  list_c = mapply('-', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}

sum_lists <- function(list_a, list_b){
  if ( (length(list_a) == length(list_b)) & (length(list_a) >0)){
    list_c = mapply('+', list_a, list_b, SIMPLIFY = FALSE)
  } else if (length(list_a) == 0){
    if (length(list_b) == 0){
      list_c = list()
    } else{
      list_c = list_b
    }
  } else if (length(list_b) == 0){
    if (length(list_a) == 0){
      list_c = list()
    } else{
      list_c = list_a
    }
  }
  return(list_c)
}


record_current_parcel_set <- function(current_ecologies, current_pool, parcel_num_remaining, yr, region_ind){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = rep(list(yr), length(current_pool))
  parcel_set_object$parcel_ecologies = current_ecologies
  parcel_set_object$parcel_sums_at_offset = find_current_parcel_sums(current_ecologies, global_params$eco_dims)
  parcel_set_object$parcel_indexes = as.list(current_pool)
  parcel_set_object$parcel_num_remaining = rep(list(parcel_num_remaining), length(current_pool))
  parcel_set_object$region_ind = rep(list(region_ind), length(current_pool))
  
  return(parcel_set_object)
  
}




nested_list_tail <- function(list_a){
  last_elements <- lapply(seq_along(list_a), function(i) lapply(seq_along(list_a[[i]]), function(j) tail(list_a[[i]][[j]], 1) ))
  return(last_elements)
}



rowProds <- function(X){ t(t(apply(X,1,FUN="prod"))) }

test_cond <- function(vals_to_match, parcel_vals_pool, development_vals_used, match_array){
  thresh_array = matrix(rep(0.10*vals_to_match, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
  cond = (parcel_vals_pool - match_array) < thresh_array
  cond = rowProds(cond)
  return(cond)
}





find_intervention_probability <- function(intervention_vec, offset_yrs, calc_type, offset_intervention_scale, time_horizons, parcel_num, parcel_num_remaining, time_steps){
  
  intervention_probs = vector('list', parcel_num)
  parcel_num_remaining = unlist(parcel_num_remaining)
  offset_yrs = unlist(offset_yrs)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_count_ind]
    offset_yr = offset_yrs[parcel_count_ind]
    current_prob = array(0, (time_horizon + 1))
    
    current_vec = intervention_vec[offset_yr:time_steps]
    
    if (length(current_vec) < (time_horizon + 1)){
      current_vec = c(current_vec, array(0, ((time_horizon + 1) - length(current_vec))))
    }
    
    current_vec = current_vec[1:(time_horizon + 1)]
    current_parcel_num_remaining = parcel_num_remaining[parcel_count_ind]
    if (current_parcel_num_remaining > 0) {
      current_prob = current_prob + current_vec/current_parcel_num_remaining
    } 
    
    if (calc_type == 'offset'){
      current_prob = offset_intervention_scale*current_prob
    }
    intervention_probs[[parcel_count_ind]] = current_prob
    
  }
  return(intervention_probs)
}




euclidean_norm_match <- function(parcel_vals_pool, vals_to_match){
  
  vals_to_test = matrix(unlist(parcel_vals_pool), nrow = length(parcel_vals_pool), byrow=TRUE)
  match_array = matrix(rep(vals_to_match, nrow(vals_to_test)), ncol = length(vals_to_match), byrow = TRUE)
  
  err = sqrt(rowSums( (vals_to_test - match_array)^2 ) )
  match_ind = which(err == min(err))
  
  if (length(match_ind) > 1){
    match_ind = sample(match_ind, 1)
    print('multiple match flag')
  }
  
  match_vals = parcel_vals_pool[match_ind]
  
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




select_pool_to_match <- function(dims_to_use, ndims, thresh, pool_num, vals_to_use, vals_to_match, match_threshold, 
                                 current_pool, use_parcel_set_dev_credit, dev_credit, parcel_for_parcel, match_type){
  
  pool_object = list()
  
  if (length(unlist(vals_to_use)) > 0){
    vals_to_test = matrix(unlist(vals_to_use), nrow = pool_num, byrow=TRUE)
  } else {
    pool_object$break_flag = TRUE
    return(pool_object)
  }
  
  vals_to_test <- select_cols(vals_to_test, dims_to_use)
  zero_inds <- which(apply(vals_to_test, MARGIN = 1, sum) < 1e-5)  
  
  if (length(zero_inds) > 0){
    vals_to_use <- vals_to_use[-zero_inds]
    current_pool = current_pool[-zero_inds]
  }
  
  vals_to_use <- lapply(seq_along(vals_to_use), function(i) vals_to_use[[i]][dims_to_use])
  
  if ( (match_type == 'offset') & (use_parcel_set_dev_credit == TRUE) ){
    vals_to_match = vals_to_match - dev_credit
  }
  
  if (parcel_for_parcel == TRUE){
    match_array = matrix(rep(vals_to_match, pool_num), ncol = ndims, byrow = TRUE)
    thresh_array = matrix(rep(thresh, pool_num), ncol = ndims, byrow = TRUE)
  } else if (parcel_for_parcel == FALSE){
    vals_to_test = apply(vals_to_test, MARGIN = 2, 'sum')
    vals_to_test = matrix(vals_to_test, ncol = ndims, byrow = TRUE) 
    match_array = matrix(vals_to_match, ncol = ndims, byrow = TRUE)
    thresh_array = matrix(thresh, ncol = ndims, byrow = TRUE)
  }
  
  if (match_type == 'offset'){
    test_array = (match_array - vals_to_test) < thresh_array
  } else if (match_type == 'development'){
    test_array = (match_array - vals_to_test) > thresh_array
  }
  
  inds_to_use = which(apply(test_array, MARGIN = 1, prod) > 0) # test if all dimensions pass threshold test and are non-zero
  
  if (all(inds_to_use == FALSE)){
    pool_object$break_flag = TRUE
    
  } else {
    
    if (parcel_for_parcel == TRUE){
      vals_to_use <- vals_to_use[inds_to_use]
      current_pool <- current_pool[inds_to_use]
    }
    
    pool_object$break_flag = FALSE
    pool_object$vals_to_use = vals_to_use
    pool_object$current_pool = current_pool
  } 
  
  return(pool_object)
}


# 
# match_type = 'offset' 
# match_procedure = 'euclidean' 
# current_pool = dev_pool_object$parcel_indexes 
# vals_to_use = dev_pool_object$parcel_vals_used 
# dev_credit = global_object$dev_credit_to_use
# use_parcel_set_dev_credit = FALSE 
# offset_multiplier = current_program_params$offset_multiplier 
# match_threshold = global_params$match_threshold 
# vals_to_match_initial = vals_to_match
# parcel_for_parcel = TRUE 
# global_params$eco_dims 
# max_offset_parcel_num = global_params$max_offset_parcel_num 
# dims_to_use = global_params$dims_to_use

# match_type = 'development' 
# match_procedure = 'random' 
# current_pool = dev_pool_object$parcel_indexes 
# vals_to_use = dev_pool_object$parcel_vals_used 
# dev_credit 
# use_parcel_set_dev_credit = FALSE 
# offset_multiplier = current_program_params$offset_multiplier 
# match_threshold = global_params$match_threshold
# vals_to_match_initial = dev_credit 
# parcel_for_parcel = TRUE 
# dims_to_use = global_params$eco_dims
# max_offset_parcel_num = global_params$max_offset_parcel_num 




select_from_pool <- function(match_type, match_procedure, current_pool, vals_to_use, dev_credit, use_parcel_set_dev_credit, 
                             offset_multiplier, match_threshold, vals_to_match_initial, parcel_for_parcel, dims_to_use, max_offset_parcel_num,yr){
  
  ndims = length(dims_to_use)
  thresh = array(match_threshold, ndims)         #create an array of threshold values with length equal to the dimensions to match to
  pool_num = length(current_pool)
  
  vals_to_match = offset_multiplier*unlist(vals_to_match_initial[dims_to_use])
  
  pool_object <- select_pool_to_match(dims_to_use, ndims, thresh, pool_num, vals_to_use, vals_to_match, match_threshold, 
                                      current_pool, use_parcel_set_dev_credit, dev_credit, parcel_for_parcel, match_type)
  
  if (pool_object$break_flag == TRUE){
    match_object = list()
    match_object$match_flag = FALSE
    return(match_object)
  } 
  
  parcel_vals_pool = pool_object$vals_to_use
  current_pool = pool_object$current_pool
  match_flag = FALSE
  match_vals = list()
  match_indexes = list()
  
  while(match_flag == FALSE){
    
    if (length(current_pool) == 0){
      break
    }
    
    if (match_procedure == 'euclidean'){
      match_params = euclidean_norm_match(parcel_vals_pool, vals_to_match)
    } else if (match_procedure == 'random'){
      match_params = list()
      match_params$match_ind = sample(length(current_pool), 1)
      match_params$match_vals = parcel_vals_pool[match_params$match_ind]
    }
    
    current_match_val = unlist(match_params$match_vals)
    current_match_index = current_pool[match_params$match_ind]
    vals_to_match = vals_to_match - current_match_val
    
    if (parcel_for_parcel == FALSE){
      
      ind_to_remove = list_intersect(current_pool, current_match_index)
      current_pool = current_pool[-ind_to_remove$match_ind]
      parcel_vals_pool = parcel_vals_pool[-ind_to_remove$match_ind]
      match_vals = append(match_vals, current_match_val)
      match_indexes = append(match_indexes, current_match_index)
      if (length(unlist(match_indexes)) > max_offset_parcel_num){
        match_flag = FALSE
        break
      }
    } else {
      match_indexes = list(current_match_index)
      match_vals = list(current_match_val)
    }
    
    if (match_type == 'offset'){
      match_flag = all(vals_to_match < thresh)
    } else if (match_type == 'development'){
      match_flag = all(vals_to_match > thresh)
    } 
  }
  
  dev_credit = vals_to_match
  if (match_type == 'offset'){
    dev_credit = -dev_credit # switch sign for any additional credit from offset 
  }
  
  match_object = list()
  match_object$match_indexes = match_indexes
  match_object$match_vals = match_vals
  match_object$match_flag = match_flag
  #match_object$dev_credit = -vals_to_match * (abs(vals_to_match) > match_threshold)
  match_object$dev_credit = dev_credit 
  match_object$vals_to_match = vals_to_match_initial
  
  return(match_object)
  
}





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




find_current_parcel_sums <- function(current_ecologies, eco_dims){
  parcel_num = length(current_ecologies)
  parcel_sums = vector('list', parcel_num)
  for (parcel_count_ind in seq_len(parcel_num)){
    parcel_sums[[parcel_count_ind]] = lapply(seq_along(current_ecologies[[parcel_count_ind]]), 
                                             function(i) sum(current_ecologies[[parcel_count_ind]][[i]]) )
  }
  return(parcel_sums)
}



split_ecology <- function(current_ecology, land_parcels, parcel_indexes, eco_dims){
  parcel_indexes = unlist(parcel_indexes)
  parcel_ecologies = vector('list', length(parcel_indexes))
  
  for (parcel_count_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_count_ind] 
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    current_parcel_ecology = extract_parcel(current_parcel, current_ecology)
    parcel_ecologies[[parcel_count_ind]] = current_parcel_ecology
  }
  return(parcel_ecologies)
}

sum_ecologies <- function(parcel_ecologies){
  parcel_sums <- lapply(seq_along(parcel_ecologies), function(i) sum(parcel_ecologies[[i]] ))
}


extract_parcel <- function(current_parcel, current_ecology){
  current_parcel_ecology = current_ecology[current_parcel]
  dim(current_parcel_ecology) = dim(current_parcel) 
  return(current_parcel_ecology)
}


write_parcel_sets <- function(parcel_set_object, yr){
  parcel_set = list()
  parcel_set$offset_yrs = yr
  parcel_set$parcel_set_object = parcel_set_object
  return(parcel_set)
}



update_ind_available <- function(index_object, parcel_indexes, region_ind){
  index_object$ind_available[[region_ind]] = setdiff(index_object$ind_available[[region_ind]], parcel_indexes) #remove parcel from available list   
  return(index_object)
}


record_parcel_index <- function(update_type, index_object, parcel_indexes, region_ind){
  
  if ( length(index_object$ind_available[[region_ind]]) == 0 ){
    index_object$break_flag = TRUE
  } else {index_object$break_flag = FALSE}
  
  if (update_type == 'development'){
    index_object$developments = append(index_object$developments, list(parcel_indexes))
    
  } else if (update_type == 'offset'){
    index_object$offsets = append(index_object$offsets, list(parcel_indexes))
    
  } else if (update_type == 'illegal'){
    index_object$illegal_clearing = append(index_object$illegal_clearing, list(parcel_indexes))
    
  } else if (update_type == 'develop_from_credit'){
    index_object$dev_credit = append(index_object$dev_credit, list(parcel_indexes))
  }
  return(index_object)
}


update_decline_rates <- function(decline_rates, restoration_rate_params, spread_restoration_rate, dims_to_use, eco_dims, decline_rate_type, offset_action_type, parcel_indexes){
  
  parcel_indexes = unlist(parcel_indexes)
  
  for (current_parcel_ind in unlist(parcel_indexes)){
    
    if (decline_rate_type == 'development'){
      decline_rates[[current_parcel_ind]] <- rep(list(0), eco_dims)
    } else if (decline_rate_type == 'offset'){
      
      if (offset_action_type == 'maintain'){
        decline_rates[[current_parcel_ind]][dims_to_use] = rep(list(1), length(dims_to_use))
      } else if (offset_action_type == 'restore'){
        if (spread_restoration_rate == TRUE){
          decline_rates[[current_parcel_ind]][dims_to_use] = as.list(rnorm(length(dims_to_use), mean = restoration_rate_params[1], sd = restoration_rate_params[2]))
        } else{
          decline_rates[[current_parcel_ind]][dims_to_use] = rep(list(restoration_rate_params[1]), length(dims_to_use))
        } 
        
      } else if (offset_action_type == 'protect'){
        
      }
      
    }
  }
  
  return(decline_rates)
  
}




list_intersect <- function(list_a, list_b){
  list_match = list()
  vec_a <- unlist(list_a)
  vec_b <- unlist(list_b)
  
  if ( (length(vec_a) == 0) || (length(vec_b) == 0)){
    return(list_match)
  }
  
  match_ind <- which(vec_a %in% vec_b)
  match_val <- vec_a[match_ind]
  
  
  list_match$match_ind = match_ind
  list_match$match_val = match_val
  return(list_match)
}


select_subset <- function(pool_object, subset_pool){
  object_subset = lapply(seq_along(pool_object), function(i) pool_object[[i]][subset_pool])
  names(object_subset) <- names(pool_object)
  return(object_subset)
}


generate_time_horizons <- function(time_horizon_type, project_type, yr, offset_yrs, time_horizon, parcel_count){
  
  if (time_horizon_type == 'offset_bank'){
    
    time_horizons = rep(yr, parcel_count) - offset_yrs
    if (project_type == 'future'){
      time_horizons = time_horizons + time_horizon
    }
    
  } else {
    if (project_type == 'future'){
      time_horizons = rep(time_horizon, parcel_count)
    } else if (project_type == 'current'){
      time_horizons = rep(0, parcel_count)
    }
  }
  return(time_horizons)
}


prepare_offset_bank <- function(current_offset_bank, current_offset_pool, offset_restoration_flag, land_parcels, current_ecologies, eco_dims){
  subset_pool =  list_intersect(current_offset_bank$parcel_indexes, current_offset_pool)
  pool_object <- select_subset(pool_object = current_offset_bank, subset_pool = subset_pool$match_ind)
  if (offset_restoration_flag == TRUE){
    current_sums_object <- find_current_parcel_sums(current_ecologies, eco_dims)
    pool_object$restoration_vals <- current_sums_object$parcel_sums
  }
  
  return(pool_object)
}


# pool_object = current_offset_object 
# pool_type = "offset_bank" 
# calc_type = current_program_params$offset_calc_type 
# cfacs_flag = current_program_params$offset_cfacs_flag 
# adjust_cfacs_flag = current_program_params$adjust_offset_cfacs_flag 
# include_potential_developments = current_program_params$include_potential_developments_in_offset_calc
# include_potential_offsets = current_program_params$include_potential_offsets_in_offset_calc
# include_illegal_clearing = current_program_params$include_illegal_clearing_in_offset_calc
# offset_restoration_flag = current_program_params$offset_restoration_flag
# time_horizon_type = current_program_params$offset_time_horizon_type


assess_current_pool <- function(pool_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, offset_restoration_flag, 
                                include_potential_developments,include_potential_offsets,include_illegal_clearing,
                                time_horizon_type, global_params, current_program_params, decline_rates, time_horizon, yr){
  
  current_pool = unlist(pool_object$parcel_indexes)
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  
  if ( (pool_type == 'offsets') || (pool_type == 'offset_bank')){
    
    if (pool_type == 'offsets'){
      if (calc_type == 'current_condition'){
        project_type = 'current'
      } else {
        project_type = 'future'
      }
    } else if (pool_type == 'offset_bank'){
      project_type = 'current'
    }
    
    time_horizons <- generate_time_horizons(time_horizon_type = pool_type, project_type, yr, offset_yrs, time_horizon, parcel_count)
    
    if (offset_restoration_flag == TRUE){
      restoration_vals = calc_parcel_trajs(current_parcel_ecologies = pool_object$parcel_ecologies, 
                                           action_type = 'restore', 
                                           current_decline_rates = decline_rates[current_pool], 
                                           time_horizons, 
                                           global_params, 
                                           global_params$restoration_rate_params, 
                                           time_fill = FALSE)
      
      pool_object$restoration_vals = lapply(seq_along(restoration_vals), function(i) lapply(seq_along(restoration_vals[[i]]), function(j) sum(restoration_vals[[i]][[j]] )))
    } else {
      pool_object$restoration_vals = list()
    }
    
  } else if (pool_type == 'devs'){
    if (calc_type == 'current_condition'){
      project_type = 'current'
    } else {
      project_type = 'future'
    }
    time_horizons <- generate_time_horizons(time_horizon_type = pool_type, project_type, yr, offset_yrs, time_horizon, parcel_count)
    pool_object$restoration_vals = list()
  }
  
  if (cfacs_flag == TRUE){
    
    cfacs_object = calc_cfacs(parcel_ecologies = pool_object$parcel_ecologies, 
                              parcel_num_remaining = pool_object$parcel_num_remaining, 
                              global_params, 
                              current_program_params, 
                              decline_rates = decline_rates[current_pool], 
                              time_horizons, 
                              offset_yrs, 
                              include_potential_developments,
                              include_potential_offsets,
                              include_illegal_clearing, 
                              adjust_cfacs_flag)
    
    pool_object$cfac_vals = nested_list_tail(cfacs_object$cfac_trajs)
    
  }
  
  pool_object$parcel_vals_used = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, pool_object$restoration_vals, pool_object$cfac_vals)
  
  return(pool_object)
}


append_current_object <- function(parcel_set_object, current_parcel_set_object, append_type){
  if (append_type == 'as_parcel_set'){
    parcel_set_object <- lapply(seq_along(current_parcel_set_object), function(i) append(parcel_set_object[[i]], list(current_parcel_set_object[[i]])))
  } else if (append_type == 'as_list'){
    parcel_set_object <- lapply(seq_along(current_parcel_set_object), function(i) append(parcel_set_object[[i]], current_parcel_set_object[[i]]))
  }
  names(parcel_set_object) = names(current_parcel_set_object)
  return(parcel_set_object)
}


initialise_parcel_set_object <- function(){
  list_names = parcel_set_list_names()
  parcel_set_object = vector('list', length(list_names))
  names(parcel_set_object) = list_names
  return(parcel_set_object)
  
}


select_banked_offset_indexes <- function(offset_bank_num, ind_available){
  
  parcels_to_bank = sample(ind_available, offset_bank_num)
  return(parcels_to_bank)
}

# initialise_banked_offsets <- function(global_params, program_params, banked_offset_vec){
#   offset_bank = generate_nested_list(outer_dim = global_params$region_num, inner_dim = 1)
#   for (region_ind in seq_len(global_params$region_num)){
#     offset_bank[[region_ind]] <- initialise_parcel_set_object()
#   }
#   return(offset_bank)
# }



update_banked_offset_pool <- function(index_object, current_banked_offset_pool, region_ind){
  index_object$banked_offset_pool[[region_ind]] = append(index_object$banked_offset_pool[[region_ind]], current_banked_offset_pool)
  index_object <- update_ind_available(index_object, 
                                       current_banked_offset_pool, 
                                       region_ind)
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





select_land_parcel <- function(land_parcels, current_parcel_ind){
  selected_land_parcel = land_parcels[[current_parcel_ind]]
  return(selected_land_parcel)
}



# current_ecology = global_object$current_ecology
# decline_rates = global_object$decline_rates
# eco_dims = global_object$eco_dims


kill_development_ecology <- function(current_ecology, decline_rates, eco_dims){
  
  parcel_num = length(current_ecology)
  
  for (parcel_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(eco_dims)){
      current_decline_rate = decline_rates[[parcel_ind]][[eco_ind]] #select current decline rates to work on
      if (current_decline_rate == 0){
        current_ecology[[parcel_ind]][[eco_ind]] = array(0, length(current_ecology[[parcel_ind]][[eco_ind]]))   # if the parcel is to be developed (i.e. decline rate = 0), set parcel value to zeros
      } 
    }
  }
  return(current_ecology) 
}





project_current_system_multi <- function(current_ecology, decline_rates, min_eco_val, max_eco_val, max_restoration_eco_val, time_horizon, eco_dims){
  
  parcel_num = length(current_ecology)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_ecology = current_ecology[[parcel_ind]][[eco_ind]] #select current ecological dimension to work on
      current_decline_rate = decline_rates[[parcel_ind]][[eco_ind]] #select current decline rates to work on
      if (current_decline_rate == 1){
        updated_parcel_ecology = current_parcel_ecology      # if the parcel is to be maintained (i.e. decline rate = 1), set parcel values to current values
      } else {
        updated_parcel_ecology = sapply(current_parcel_ecology, project_ecology, min_eco_val, max_eco_val, decline_rate = current_decline_rate, time_horizon, time_fill = FALSE)
      }
      #dim(updated_parcel_ecology) = dim(current_parcel_ecology)
      current_ecology[[parcel_ind]][[eco_ind]] = updated_parcel_ecology 
    }
  }
  
  return(current_ecology) 
}






build_trajectories_as_list <- function(traj_list, parcel_pool, eco_dims){
  traj_num = length(parcel_pool)
  trajectories_list = generate_nested_list(outer_dim = traj_num, inner_dim = eco_dims)
  for (traj_ind in 1:traj_num){
    parcel_pool_ind = parcel_pool[traj_ind]
    for (eco_ind in 1:eco_dims){
      trajectories_list[[traj_ind]][[eco_ind]] = apply(traj_list[[parcel_pool_ind]][[eco_ind]], MARGIN = 3, sum)
    }
  }
  return(trajectories_list)
}



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




find_decline_rate <- function(decline_rates, current_parcel_ind, eco_ind){
  current_dec_rate = decline_rates[[current_parcel_ind]][[eco_ind]]
  return(current_dec_rate)
}



calc_cfacs <- function(parcel_ecologies, parcel_num_remaining, global_params, current_program_params, decline_rates, time_horizons, offset_yrs, include_potential_developments,include_potential_offsets,include_illegal_clearing, adjust_cfacs_flag){
  
  cfacs_object = list()
  if (length(decline_rates) != length(parcel_ecologies)){
    print('calc cfacs length error')
  }
  
  cfacs_object$cfacs = calc_parcel_trajs(parcel_ecologies, 
                                         action_type = 'protect', 
                                         decline_rates, 
                                         time_horizons, 
                                         global_params, 
                                         global_params$restoration_rate_params, 
                                         time_fill = TRUE)
  
  if (adjust_cfacs_flag == TRUE){
    cfacs_object$cfacs = adjust_cfacs(cfacs_object$cfacs, 
                                      include_potential_developments,
                                      include_potential_offsets,
                                      include_illegal_clearing, 
                                      global_params, 
                                      current_program_params, 
                                      parcel_num_remaining, 
                                      decline_rates, 
                                      time_horizons, 
                                      offset_yrs)
  } 
  cfacs_object$cfac_trajs = sum_trajectories(cfacs_object$cfacs, eco_dims = global_params$eco_dims)
  return(cfacs_object)
  
}  



# current_cfacs = cfacs_object$cfacs


adjust_cfacs <- function(current_cfacs, include_potential_developments,include_potential_offsets, include_illegal_clearing, 
                         global_params, current_program_params, parcel_num_remaining, decline_rates, time_horizons, offset_yrs){
  
  time_horizons = unlist(time_horizons)
  
  weighted_counters_object <- find_weighted_counters(current_cfacs, 
                                                     include_illegal_clearing, 
                                                     include_potential_developments, 
                                                     include_potential_offsets,  
                                                     intervention_vec = current_program_params$intervention_vec, 
                                                     illegal_clearing_prob = current_program_params$illegal_clearing_prob,
                                                     offset_intervention_scale = global_params$max_offset_parcel_num,
                                                     global_params$eco_dims, 
                                                     parcel_num_remaining, 
                                                     parcel_num = length(current_cfacs), 
                                                     time_horizons, 
                                                     offset_yrs, 
                                                     time_steps = global_params$time_steps)
  
  if (length(decline_rates) != length(current_cfacs)){
    print('length error')
  }
  
  if (include_potential_offsets == FALSE){
    adjusted_cfacs = weighted_counters_object$weighted_counters
  } else {
    
    offset_projections <- calc_offset_projections(current_cfacs, 
                                                  weighted_counters_object$offset_intervention_probs, 
                                                  global_params$restoration_rate_params, 
                                                  current_program_params$offset_action_type,
                                                  decline_rates, 
                                                  time_horizons, 
                                                  global_params$eco_dims, 
                                                  global_params$min_eco_val, 
                                                  global_params$max_eco_val)
    
    summed_offset_projections <- sum_offset_projs(offset_projections,
                                                  offset_probs = weighted_counters_object$offset_intervention_probs, 
                                                  global_params$eco_dims, 
                                                  time_horizons)
    
    adjusted_cfacs = sum_clearing_offsets(weighted_counters_object$weighted_counters, 
                                          summed_offset_projections, 
                                          global_params$eco_dims)
  }
  
  return(adjusted_cfacs)
}



remove_neg_probs <- function(weight_list, inds_to_accept){
  weight_list <- lapply(seq_along(weight_list), function(i) weight_list[[i]]*inds_to_accept[[i]])
  return(weight_list)
}


generate_weights <- function(perform_weight, calc_type, offset_intervention_scale, intervention_vec, offset_yrs, time_horizons, 
                             parcel_num, parcel_num_remaining, time_steps, illegal_clearing_prob){
  if (perform_weight == TRUE){
    if (calc_type == 'illegal_clearing'){
      weighted_probs <- lapply(seq_len(parcel_num), function(i) rep(illegal_clearing_prob, (time_horizons[i] + 1)))    #runif(n = (time_horizon + 1), min = 0, max = current_program_params$illegal_clearing_prob)
    } else {
      weighted_probs <- find_intervention_probability(intervention_vec, 
                                                      offset_yrs,
                                                      calc_type, 
                                                      offset_intervention_scale,
                                                      time_horizons, 
                                                      parcel_num, 
                                                      parcel_num_remaining, 
                                                      time_steps)
    } 
  } else {
    weighted_probs <- lapply(seq_len(parcel_num), function(i) rep(0, (time_horizons[i] + 1)))  
  }
  
  weights <- lapply(weighted_probs, cumsum)
  weighted_object = list()
  weighted_object$weighted_probs = weighted_probs
  weighted_object$weights = weights
  return(weighted_object)
}


# current_cfacs 
# include_illegal_clearing = FALSE
# include_potential_developments = FALSE 
# include_potential_offsets = TRUE  
# intervention_vec = dev_vec 
# illegal_clearing_prob = 0
# offset_intervention_scale = 1
# eco_dims = global_params$eco_dims 
# parcel_num_remaining = length(global_params$parcel_indexes) 
# parcel_num = length(current_cfacs) 
# time_horizons 
# offset_yrs = global_params$offset_yr 
# time_steps = global_params$time_steps


# current_cfacs 
# include_illegal_clearing = FALSE 
# include_potential_developments = FALSE 
# include_potential_offsets = TRUE  
# intervention_vec = dev_vec 
# illegal_clearing_prob = 0
# offset_intervention_scale = 1
# eco_dims = global_params$eco_dims
# parcel_num_remaining = global_params$parcel_num_remaining 
# parcel_num = global_params$dev_num 
# offset_yrs = rep(list(global_params$offset_yr), global_params$dev_num)
# time_steps = global_params$time_steps

find_weighted_counters <- function(current_cfacs, include_illegal_clearing, include_potential_developments, include_potential_offsets, 
                                   intervention_vec, illegal_clearing_prob, offset_intervention_scale, eco_dims, parcel_num_remaining, parcel_num, time_horizons, offset_yrs, time_steps){
  
  
  illegal_clearing_weights <- generate_weights(include_illegal_clearing, 
                                               calc_type = 'illegal_clearing',
                                               offset_intervention_scale, 
                                               intervention_vec, 
                                               offset_yrs, 
                                               time_horizons, 
                                               parcel_num, 
                                               parcel_num_remaining, 
                                               time_steps, 
                                               illegal_clearing_prob)
  
  dev_weights <- generate_weights(include_potential_developments, 
                                  calc_type = 'development',
                                  offset_intervention_scale, 
                                  intervention_vec, 
                                  offset_yrs, 
                                  time_horizons, 
                                  parcel_num, 
                                  parcel_num_remaining, 
                                  time_steps, 
                                  illegal_clearing_prob)
  
  offset_weights <- generate_weights(include_potential_offsets, 
                                     calc_type = 'offset',
                                     offset_intervention_scale,
                                     intervention_vec, 
                                     offset_yrs, 
                                     time_horizons, 
                                     parcel_num, 
                                     parcel_num_remaining, 
                                     time_steps, 
                                     illegal_clearing_prob)
  
  counter_weights <- lapply(seq_len(parcel_num), function(i) (1 - (dev_weights$weights[[i]] + offset_weights$weights[[i]] + illegal_clearing_weights$weights[[i]])))
  
  inds_to_accept = lapply(seq_along(counter_weights), function(i) counter_weights[[i]] >= 0)
  offset_intervention_probs <- remove_neg_probs(offset_weights$weighted_probs, inds_to_accept)
  counter_weights <- remove_neg_probs(counter_weights, inds_to_accept) 
  
  weighted_counters_object = list()
  weighted_counters_object$weighted_counters = lapply(seq_along(current_cfacs), function(i) (lapply(seq(eco_dims), function(j) current_cfacs[[i]][[j]]*matrix(rep(counter_weights[[i]], dim(current_cfacs[[i]][[j]])[2]), nrow = dim(current_cfacs[[i]][[j]])[1], byrow = FALSE))))
  
  weighted_counters_object$offset_intervention_probs = offset_intervention_probs
  
  
  
  return(weighted_counters_object)
}







calc_offset_projections <- function(current_cfacs, offset_probs, restoration_rate_params, action_type, decline_rates, time_horizons, eco_dims, min_eco_val, max_eco_val){
  
  if (length(decline_rates) != length(current_cfacs)){
    print('length error')
  }
  parcel_num = length(current_cfacs)
  offset_projections = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_count_ind] + 1
    current_offset_probs = offset_probs[[parcel_count_ind]]
    current_offset_projections = generate_nested_list(outer_dim = eco_dims, inner_dim = time_horizon)
    
    for (eco_ind in seq_len(eco_dims)){
      
      current_cfac = current_cfacs[[parcel_count_ind]][[eco_ind]]
      current_dec_rate = decline_rates[[parcel_count_ind]][[eco_ind]]
      
      for (proj_yr in seq_len(time_horizon)){
        current_offset_projections[[eco_ind]][[proj_yr]] = array(0, dim(current_cfac))
        
        if (current_offset_probs[proj_yr] > 0){
          current_parcel_ecology = list(current_cfac[proj_yr, ])
          
          current_offset_proj = predict_parcel_traj(current_parcel_ecology, 
                                                    action_type, 
                                                    min_eco_val, 
                                                    max_eco_val,
                                                    eco_dims = length(current_parcel_ecology),
                                                    restoration_rate_params, 
                                                    current_dec_rate, 
                                                    (time_horizon - proj_yr), 
                                                    time_fill = TRUE)
          
          current_offset_projections[[eco_ind]][[proj_yr]][proj_yr:time_horizon, ] = current_offset_proj[[1]]
        }
      }
    }
    offset_projections[[parcel_count_ind]] = current_offset_projections
  }
  
  return(offset_projections)
  
}


sum_offset_projs <- function(offset_projections, offset_probs, eco_dims, time_horizons){
  parcel_num = length(offset_projections)
  summed_offset_projections = vector('list', parcel_num)
  for (parcel_count_ind in seq_len(parcel_num)){
    
    summed_offset_projections[[parcel_count_ind]] = vector('list', eco_dims)
    current_offset_prob = offset_probs[[parcel_count_ind]]
    current_offset_prob <- current_offset_prob*(current_offset_prob > 0)
    
    current_offset_proj = offset_projections[[parcel_count_ind]]
    
    for (eco_ind in seq_len(eco_dims)){
      current_offset_projections <- current_offset_proj[[eco_ind]]
      current_offset_projections <- lapply(seq_along(current_offset_projections), function(i) current_offset_projections[[i]]*current_offset_prob[i])
      summed_offset_projections[[parcel_count_ind]][[eco_ind]] = Reduce('+', current_offset_projections)
    }
  }
  
  return(summed_offset_projections)
}






sum_clearing_offsets <- function(cfacs_include_clearing, summed_offset_projections, eco_dims){
  parcel_num = length(cfacs_include_clearing)
  cfacs_include_clearing_offsets = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    cfacs_include_clearing_offsets[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  for (parcel_count_ind in seq_len(parcel_num)){
    if (length(summed_offset_projections[[parcel_count_ind]]) > 0 ){
      for (eco_ind in seq_len(eco_dims)){
        cfacs_include_clearing_offsets[[parcel_count_ind]][[eco_ind]] = summed_offset_projections[[parcel_count_ind]][[eco_ind]] + cfacs_include_clearing[[parcel_count_ind]][[eco_ind]]
      }
    }
  }
  return(cfacs_include_clearing_offsets)
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


select_cols <- function(arr_to_use, col_inds){
  arr_to_use <- arr_to_use[, col_inds]
  arr_to_use <- t(t(arr_to_use))
  return(arr_to_use)
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


combine_land_parcels_to_landscape <- function(current_ecology, land_parcels, landscape_dims, eco_dims){
  landscape = list_of_zeros(list_dims = eco_dims, array_dims = landscape_dims)
  for (eco_ind in seq_len(eco_dims)){
    landscape[[eco_ind]][unlist(land_parcels)] = unlist(current_ecology)
  }
  return(landscape)
}


split_landscape_to_land_parcels <- function(landscape, land_parcels, eco_dims){
  parcel_num = length(land_parcels)
  current_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = eco_dims)
  
  for (parcel_ind in seq_len(parcel_num)){  
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (eco_ind in seq_len(eco_dims)){
      current_ecology[[parcel_ind]][[eco_ind]] = landscape[[eco_ind]][current_parcel]
    }
  }
  return(current_ecology)
  
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
  trajectories[loc_1[1]:loc_2[1], loc_1[2]:loc_2[2], ] = parcel_3D 
  return(trajectories)
}



reshape_trajectories <- function(trajectories, land_parcels, eco_dims){
  for (parcel_ind in seq_along(trajectories)){
    current_parcel_dims = dim(land_parcels[[parcel_ind]])
    for (eco_ind in seq_along(eco_dims)){
      trajectories[[parcel_ind]][[eco_ind]] = reshape_parcel_traj(trajectories[[parcel_ind]][[eco_ind]], current_parcel_dims)
    }
  }
  return(trajectories)
}

reshape_parcel_traj <- function(current_parcel_traj, current_parcel_dims){
  dim(current_parcel_traj) = c(dim(current_parcel_traj), 1)
  current_parcel_traj = aperm(current_parcel_traj, c(3, 2, 1))
  dim(current_parcel_traj) = c(current_parcel_dims, dim(current_parcel_traj)[3]) 
  return(current_parcel_traj)
}


form_net_trajectory <- function(trajectories_list, land_parcels, time_steps, landscape_dims, eco_dims){
  
  net_trajectories = vector('list', eco_dims)
  
  for (eco_ind in seq_len(eco_dims)){
    current_trajectory = array(0, c(time_steps, length(unlist(parcels$land_parcels))))
    for (parcel_ind in seq_along(trajectories_list)){
      current_parcel = land_parcels[[parcel_ind]]
      current_trajectory[, current_parcel] = trajectories_list[[parcel_ind]][[eco_ind]]
    }
    net_trajectories[[eco_ind]] = reshape_parcel_traj(current_trajectory, current_parcel_dims = landscape_dims)
  }
  return(net_trajectories)
}




sum_trajectories <- function(traj_list, eco_dims){
  parcel_num = length(traj_list)
  parcel_traj_list = generate_nested_list(parcel_num, eco_dims)
  for (parcel_count_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      current_traj = traj_list[[parcel_count_ind]][[eco_ind]]
      if (length(dim(current_traj)) > 1){
        current_traj = apply(current_traj, MARGIN=1, sum)
      } else {
        current_traj = sum(current_traj)
      }
      parcel_traj_list[[parcel_count_ind]][[eco_ind]] = current_traj
    }  
  }
  return(parcel_traj_list)
}
