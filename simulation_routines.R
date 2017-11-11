run_offset_simulation_routines <- function(policy_params, run_params, parcels, initial_ecology, decline_rates_initial, 
                                           dev_weights, scenario_ind, realisation_ind){  
  # run simulation with identical realisation instantiation
  if (run_params$set_seed == TRUE){
    set.seed(123)
  }
  
  current_data_dir = write_nested_folder(paste0(run_params$output_folder, 
                                                'scenario_', formatC(scenario_ind, width = 3, format = "d", flag = "0"), 
                                                '/realisation_', formatC(realisation_ind, width = 3, format = "d", flag = "0"), '/'))
  
  # set object used to store simulation outputs 
  simulation_outputs = initialise_output_object(parcels, 
                                                initial_ecology, 
                                                run_params, 
                                                decline_rates_initial, 
                                                run_params$save_realisations)
  
  # run the model and return outputs
  simulation_outputs <- run_simulation(simulation_outputs, 
                                       run_params, 
                                       policy_params, 
                                       parcels, 
                                       decline_rates_initial, 
                                       dev_weights,
                                       current_data_dir)  
  
  # save raw simulation data
  if (run_params$save_simulation_outputs == TRUE){
    saveRDS(simulation_outputs, paste0(current_data_dir, 'realisation_', 
                                       formatC(realisation_ind, width = 3, format = "d", flag = "0"), 
                                       '_outputs.rds'))
  }
  
  for (feature_ind in seq(run_params$feature_num)){
    # select current layer index
    current_feature = run_params$features_to_use_in_simulation[feature_ind]
    # read current feature layer files over time series and arrange into data stack
    current_trajectories = form_data_stack(current_data_dir, 
                                           feature_string = formatC(current_feature, width = 3, format = "d", flag = "0"), 
                                           land_parcels = parcels$land_parcels, 
                                           run_params$time_steps)
    # run series of routines used to calculate gains and losses at multiple scales over current feature layer
    current_collated_realisation = run_collate_routines(simulation_outputs, 
                                                        current_trajectories,
                                                        decline_rates_initial, 
                                                        initial_ecology,  
                                                        current_data_dir, 
                                                        run_params, 
                                                        policy_params,
                                                        realisation_ind, 
                                                        feature_ind)
    
    file_prefix = paste0(run_params$collated_folder, 
                         'collated_scenario_',  formatC(scenario_ind, width = 3, format = "d", flag = "0"), 
                         '_realisation_', formatC(realisation_ind, width = 3, format = "d", flag = "0")) 
    
    saveRDS(current_collated_realisation, paste0(file_prefix, '_feature_', 
                                                 formatC(current_feature, width = 3, format = "d", flag = "0"), '.rds'))
    
    #     if ((run_params$write_offset_layer == TRUE) && (feature_ind == 1)){
    #       write_offset_layer(paste0(file_prefix, '_offset_layer.png'), 
    #                          current_trajectories, 
    #                          parcels, 
    #                          unlist(simulation_outputs$offsets_object$parcel_indexes), 
    #                          color_vec = c('black', 'darkgreen'), 
    #                          run_params)
    #       write_offset_layer(paste0(file_prefix, '_dev_layer.png'),
    #                          current_trajectories, 
    #                          parcels, 
    #                          unlist(simulation_outputs$dev_object$parcel_indexes), 
    #                          color_vec = c('black', 'red'), 
    #                          run_params)
    #       
    #     }
  }
  
  
  # delete current temporary files and folder
  
  if (run_params$save_simulation_outputs == FALSE){
    unlink(current_data_dir, recursive = TRUE)
  }
  
  return(simulation_outputs)
}  



# main engine for code - returns all simulation outputs including developments, offsets etc.
run_simulation <- function(simulation_outputs, run_params, policy_params, parcels, 
                           decline_rates_initial, dev_weights, current_data_dir){  
  
  #run through main time loop
  for (yr in seq_len(run_params$time_steps)){             
    cat('\n t =', yr)
    
    # if running multiple regions with distinct policies cycle through each region
    for (region_ind in seq_len(parcels$region_num)){
      # select current policy - i.e. how gains are calculated                
      current_policy_params = select_current_policy(policy_params, region_ind, parcels$region_num)      
      
      #when running in offset banking select out current set of sites to add to bank
      if (current_policy_params$use_offset_bank == TRUE){          
        simulation_outputs <- perform_banking_routine(simulation_outputs, current_policy_params, yr, region_ind, run_params)
      }
      
      # determine current set of available offset sites and calculate gains structure as detailed in current policy params 
      simulation_outputs$offset_pool_object <- prepare_offset_pool(simulation_outputs,
                                                                   current_policy_params, 
                                                                   region_ind, 
                                                                   run_params, 
                                                                   parcels,
                                                                   decline_rates_initial,
                                                                   yr)
      
      # cycle through number of developments and associated offsets
      
      for (current_dev_index in seq_len(run_params$intervention_vec[yr])){          
        if (current_policy_params$allow_developments_from_credit == TRUE){
          
          if (current_policy_params$use_offset_bank == TRUE){
            simulation_outputs$current_credit = assess_credit(simulation_outputs, current_policy_params)
          }
          # attempt to develop from current available credit
          credit_match_object = develop_from_credit(simulation_outputs$current_ecology, 
                                                    simulation_outputs$current_credit, 
                                                    dev_weights,
                                                    run_params, 
                                                    current_policy_params,
                                                    intervention_vec = run_params$intervention_vec, 
                                                    dev_indexes_to_use = simulation_outputs$index_object$indexes_to_use$devs[[region_ind]], 
                                                    decline_rates_initial, 
                                                    parcels$land_parcels,
                                                    region_ind,
                                                    yr, 
                                                    current_policy_params$offset_time_horizon)
          
          # if development was permitted add current site to current group of developments, set all ecology to zero 
          
          if (credit_match_object$match_flag == TRUE){
            
            simulation_outputs$current_credit = credit_match_object$current_credit
            cat('\n developed site with value', unlist(credit_match_object$development_object$parcel_vals_used), 'from credit')
            cat('\n', unlist(credit_match_object$current_credit), 'remaining\n')
            
            simulation_outputs <- perform_clearing_routine(simulation_outputs, 
                                                           simulation_outputs$index_object, 
                                                           simulation_outputs$decline_rates, 
                                                           current_dev_object = credit_match_object$development_object, 
                                                           clearing_type = 'develop_from_credit', 
                                                           region_ind, 
                                                           run_params)
          }
          
        } else {
          credit_match_object = false_match()
        } 
        
        #if insufficient credits accumulated to allow development attempt development with offset match.
        
        if ( (credit_match_object$match_flag == FALSE && current_policy_params$use_parcel_sets == TRUE)){           
          match_object <- match_parcel_set(offset_pool_object = simulation_outputs$offset_pool_object, 
                                           simulation_outputs$current_credit,
                                           dev_weights, 
                                           run_params,
                                           current_policy_params, 
                                           intervention_vec = run_params$intervention_vec, 
                                           indexes_to_use = simulation_outputs$index_object$indexes_to_use$devs[[region_ind]], 
                                           simulation_outputs$current_ecology, 
                                           decline_rates = simulation_outputs$decline_rates, 
                                           parcels$land_parcels, 
                                           yr, 
                                           current_policy_params$offset_time_horizon, 
                                           region_ind)  #perform the matching routine - i.e. find a matching development/offset set.
          
          # if a match was found record current development and associated offsets and update site parameters.
          
          if (match_object$match_flag == TRUE){
            
            cat('\n matched development site', unlist(match_object$development_object$parcel_indexes), 
                'with offset sites', unlist(match_object$offset_object$parcel_indexes), '\n')
            
            #update available credit 
            simulation_outputs$current_credit = match_object$current_credit
            
            # remove selected offset sites from available pool, save offset site characteristics, update decline rates to implement offset.
            simulation_outputs <- perform_offset_routine(simulation_outputs, 
                                                         index_object = simulation_outputs$index_object, 
                                                         decline_rates = simulation_outputs$decline_rates, 
                                                         current_offset_object = match_object$offset_object, 
                                                         current_offset_indexes = match_object$offset_object$parcel_indexes, 
                                                         match_object, 
                                                         current_policy_params, 
                                                         region_ind, 
                                                         run_params)
            
            # remove selected development sites from available pool, save development site characteristics, 
            # update decline rates to implement development loss.
            simulation_outputs <- perform_clearing_routine(simulation_outputs, 
                                                           simulation_outputs$index_object, 
                                                           simulation_outputs$decline_rates, 
                                                           current_dev_object = match_object$development_object, 
                                                           clearing_type = 'development', 
                                                           region_ind, 
                                                           run_params)
          }  
        } 
      }
      
      #select sites for illegal clearing
      illegally_cleared_object <- perform_illegal_clearing(simulation_outputs$current_ecology, 
                                                           simulation_outputs$index_object,
                                                           yr, 
                                                           region_ind, 
                                                           current_policy_params, 
                                                           run_params, 
                                                           decline_rates_initial, 
                                                           current_policy_params$offset_time_horizon)
      
      # remove selected illegal sites from available pool, save illegal site characteristics, update decline rates to implement loss.
      if (!is.null(illegally_cleared_object)){
        simulation_outputs <- perform_clearing_routine(simulation_outputs, 
                                                       index_object = simulation_outputs$index_object, 
                                                       decline_rates = simulation_outputs$decline_rates, 
                                                       current_dev_object = illegally_cleared_object, 
                                                       clearing_type = 'illegal', 
                                                       region_ind, 
                                                       run_params)
      }
      
    }
    
    if ( (length(unlist(simulation_outputs$offsets_object$parcel_indexes)) > 0) & 
         (run_params$limit_offset_restoration == TRUE) & 
         (current_policy_params$offset_action_type == 'restore')){
      
      # assess whether offsets have achieved their gains as determined in intial offset gains calculation
      assessed_parcel_sets_object <- assess_parcel_sets(simulation_outputs$current_ecology, 
                                                        simulation_outputs$offsets_object, 
                                                        run_params, 
                                                        current_policy_params,
                                                        decline_rates_initial,
                                                        current_policy_params$offset_time_horizon, 
                                                        yr)
      # update ecology change parameters
      simulation_outputs$decline_rates <- update_decline_rates(simulation_outputs$decline_rates, 
                                                               restoration_rate = current_policy_params$restoration_rate, 
                                                               features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc, 
                                                               feature_num = run_params$feature_num, 
                                                               decline_rate_type = 'offset', 
                                                               action_type = 'maintain', 
                                                               parcel_indexes = assessed_parcel_sets_object$site_success_inds)
    }
    
    # implement ecological loss for developed sites (those with decline_rates = 0)
    simulation_outputs$current_ecology <- kill_development_ecology(simulation_outputs$current_ecology, 
                                                                   simulation_outputs$decline_rates, 
                                                                   run_params$feature_num)
    
    for (feature_ind in seq(run_params$feature_num)){
      ecology_to_save = lapply(seq_along(simulation_outputs$current_ecology), function(i) simulation_outputs$current_ecology[[i]][[feature_ind]])
      saveRDS(ecology_to_save, paste0(current_data_dir, 
                                      'feature_', formatC(run_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0"), 
                                      '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
    }
    
    # update ecology for all sites (including those just developed/offset)
    simulation_outputs$current_ecology = project_parcel(simulation_outputs$current_ecology, 
                                                        simulation_outputs$decline_rates, 
                                                        current_time_horizons = rep(list(1), length(simulation_outputs$current_ecology)), 
                                                        run_params, 
                                                        features_to_use_in_offset_calc = seq(run_params$feature_num), 
                                                        time_fill = FALSE)
  }
  
  return(simulation_outputs)
  
} 


# build nested list of ecology change parameters of used to determine how the ecology evolves in the absence of the 
# development and offset intervention. List length is determined by how many sites and list depth is determined by 
# how many features are currently in use

simulate_decline_rates <- function(parcel_num, sample_decline_rate, mean_decline_rates, decline_rate_std, feature_num){
  
  
  if (sample_decline_rate == TRUE){
    # sample change rate from normal distribution
    decline_rates = lapply(seq(parcel_num), function(i) lapply(seq(feature_num), 
                                                               function(j) rnorm(1, mean_decline_rates[[j]], decline_rate_std[[j]])))
  } else { 
    # copy same rate to all sites
    decline_rates = lapply(seq(parcel_num), function(i) lapply(seq(feature_num), 
                                                               function(j) mean_decline_rates[[j]]))
  }
  
  return(decline_rates)
  
}

# for multuiple regions with multiple policies select current policy
select_current_policy <- function(current_policy_params_set, region_ind, region_num){
  if (region_num > 1){
    current_policy_params = current_policy_params_set[[region_ind]]
  } else {
    current_policy_params = current_policy_params_set
  }
  return(current_policy_params)
}


# used after simulation has completed. Takes time series layer files for each feature and stacks 
form_data_stack <- function(current_data_dir, feature_string, land_parcels, time_steps){
  
  current_filenames <- list.files(path = current_data_dir, 
                                  pattern = paste0('feature_', feature_string), all.files = FALSE, 
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  
  data_stack = lapply(seq_along(land_parcels), function(i) array(0, c(time_steps, length(land_parcels[[i]]))))
  
  for (yr in seq(time_steps)){
    current_ecology = readRDS(paste0(current_data_dir, current_filenames[yr]))
    data_stack <- lapply(seq_along(data_stack), function(i) stack_current_yr(data_stack[[i]], current_ecology[[i]], yr))
  }
  
  return(data_stack)
}

stack_current_yr <- function(current_parcel, current_parcel_ecology, yr){
  current_parcel[yr, ] = current_parcel_ecology
  return(current_parcel)
}


# remove site from current offset pool
remove_site_from_pool <- function(offset_pool_object, current_parcel_indexes){
  subset_pool_to_remove <- list_intersect(offset_pool_object$parcel_indexes, current_parcel_indexes)
  if (length(subset_pool_to_remove$match_ind) > 0){
    subset_pool_to_use <- seq_along(offset_pool_object$parcel_indexes)[-subset_pool_to_remove$match_ind]
    offset_pool_object <- select_pool_subset(offset_pool_object, subset_pool_to_use)
  }
  return(offset_pool_object)
}


#sample over uniform random vector, indicies less than the threshold level are selected for illegal clearing
select_sites_to_illegally_clear <- function(index_object, run_params){
  
  parcel_inds = unlist(index_object$indexes_to_use)
  clearing_thresh <- rep(run_params$illegal_clearing_prob, length(parcel_inds))
  discrim <- runif(length(clearing_thresh)) < clearing_thresh                               
  inds_to_clear <- parcel_inds[discrim]
  return(inds_to_clear)
}


perform_illegal_clearing <- function(current_ecology, index_object, yr, region_ind, current_policy_params, 
                                     run_params, decline_rates_initial, time_horizon){
  
  if (run_params$illegal_clearing_prob == 0){ 
    # return null object when illegal clearing is inactive
    return()
  }
  
  inds_to_clear <- select_sites_to_illegally_clear(index_object, run_params)
  
  if (length(inds_to_clear) == 0){ #return null for no sites selected for illegal clearing
    return()
  } else {
    cat('\n illegally cleared sites' , inds_to_clear)
  }
  
  parcel_num_remaining = length(c(unlist(index_object$indexes_to_use$offsets[[region_ind]]), 
                                  unlist(index_object$indexes_to_use$devs[[region_ind]]))) #used for calculation of cfac
  
  # store group of site characteristics in site characteristics object
  illegally_cleared_object <- record_current_parcel_set(current_ecology[inds_to_clear], 
                                                        inds_to_clear, 
                                                        parcel_num_remaining, 
                                                        yr, 
                                                        region_ind) #record potential current development parcel attributes
  
  # record characteristics of illegally cleared site
  illegally_cleared_object <- assess_current_pool(pool_object = illegally_cleared_object, 
                                                  pool_type = 'devs', 
                                                  calc_type = current_policy_params$dev_calc_type, 
                                                  cfacs_flag = current_policy_params$dev_cfacs_flag, 
                                                  adjust_cfacs_flag = current_policy_params$adjust_dev_cfacs_flag, 
                                                  action_type = current_policy_params$offset_action_type,
                                                  include_potential_developments = current_policy_params$include_potential_developments_in_dev_calc,
                                                  include_potential_offsets = current_policy_params$include_potential_offsets_in_dev_calc,
                                                  include_illegal_clearing = current_policy_params$include_illegal_clearing_in_dev_calc,
                                                  time_horizon_type = 'future',
                                                  run_params, 
                                                  current_policy_params,
                                                  decline_rates_initial, 
                                                  time_horizon, 
                                                  yr)  #determine future development parcel attributes
  return(illegally_cleared_object)
}


# series of routines to implement offset
perform_offset_routine <- function(simulation_outputs, index_object, decline_rates, current_offset_object, current_offset_indexes, match_object, 
                                   current_policy_params, region_ind, run_params){
  
  # if running in banking mode remove offset site from available bank  
  if (current_policy_params$use_offset_bank == TRUE){
    banked_offset_pool = simulation_outputs$offset_bank_object$parcel_indexes
    banked_offset_inds_used = list_intersect(banked_offset_pool, current_offset_indexes)         
    
    # determine parcels used in matching routine and remove from available pool
    simulation_outputs$offset_bank_object$parcel_indexes = remove_index(banked_offset_pool, banked_offset_inds_used$match_ind)
    
  } else {
    # determine parcels used in matching routine and remove from available pool
    simulation_outputs$index_object <- update_indexes_to_use(index_object, current_offset_indexes, region_ind)         
    simulation_outputs$decline_rates <- update_decline_rates(decline_rates, 
                                                             restoration_rate_params = run_params$restoration_rate_params, 
                                                             sample_restoration_rate = run_params$sample_restoration_rate,
                                                             features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc, 
                                                             feature_num = run_params$feature_num, 
                                                             decline_rate_type = 'offset', 
                                                             action_type = current_policy_params$offset_action_type, 
                                                             current_offset_indexes) # set elements in decline rates array corresponding to offsets to restoration rates
  }
  
  #record current offset site characteristics
  simulation_outputs$offsets_object <- append_current_group(simulation_outputs$offsets_object, current_offset_object, append_routine = 'standard')      
  
  #remove offset sites from available pool
  simulation_outputs$offset_pool_object <- remove_parcel_from_current_pool(simulation_outputs$offset_pool_object, 
                                                                           current_parcel_indexes = current_offset_indexes)
  return(simulation_outputs)
}

# remove site characteristics from current pool of available sites
remove_parcel_from_current_pool <- function(offset_pool_object, current_parcel_indexes){
  # work out indexes of current sites in pool
  sites_to_remove <- list_intersect(offset_pool_object$parcel_indexes, current_parcel_indexes)
  if (length(sites_to_remove$match_ind) > 0){
    # remove site index from available pool
    subset_pool_to_use <- seq_along(offset_pool_object$parcel_indexes)[-sites_to_remove$match_ind]
    # redefine pool with current used sites removed
    offset_pool_object <- select_pool_subset(offset_pool_object, subset_pool_to_use)
  }
  return(offset_pool_object)
}



# routines to mark and destroy ecology in cleared sites e.g. Development or illegal

perform_clearing_routine <- function(simulation_outputs, index_object, decline_rates, current_dev_object, clearing_type, region_ind, run_params){
  #remove development parcels from available pool 
  simulation_outputs$index_object <- update_indexes_to_use(index_object, current_dev_object$parcel_indexes, region_ind)                  
  if (clearing_type == 'development'){
    #record current development site characteristics
    
    simulation_outputs$dev_object <- append_current_group(simulation_outputs$dev_object, current_dev_object, append_routine = 'standard')
    
  } else if (clearing_type == 'develop_from_credit'){
    #record current development site characteristics
    simulation_outputs$credit_object <- append_current_group(simulation_outputs$credit_object, current_dev_object, append_routine = 'standard')
  } else if (clearing_type == 'illegal'){
    #record current illegally cleared site characteristics
    
    simulation_outputs$illegal_clearing_object <- append_current_group(simulation_outputs$illegal_clearing_object, current_dev_object, append_routine = 'standard')
  }  
  # set elements corresponding to developed parcels in decline rates array to zero
  simulation_outputs$decline_rates <- update_decline_rates(decline_rates, 
                                                           restoration_rate_params = vector(),
                                                           sample_restoration_rate = vector(),
                                                           features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc, 
                                                           feature_num = run_params$feature_num, 
                                                           decline_rate_type = 'development', 
                                                           action_type = vector(), 
                                                           current_dev_object$parcel_indexes)     
  if (length(current_dev_object$parcel_indexes) > 0){
    # if any development was allowed remove developed site from available offset pool
    simulation_outputs$offset_pool_object <- remove_parcel_from_current_pool(simulation_outputs$offset_pool_object, 
                                                                             current_parcel_indexes = current_dev_object$parcel_indexes)
  }
  
  return(simulation_outputs)
}  
# determine current available credit accumulated through offsets for development
assess_credit <- function(simulation_outputs, current_policy_params){
  
  # determine total offset gains
  offset_credit = nested_list_sum(simulation_outputs$offset_pool_object$parcel_vals_used)
  
  dev_list = append(simulation_outputs$credit_object$parcel_vals_used, simulation_outputs$dev_object$parcel_vals_used)
  
  if (length(dev_list) > 0){
    # determine total development losses
    dev_sum = nested_list_sum(dev_list)
    current_credit = subtract_nested_lists(offset_credit, dev_sum)
    print(paste('current credit list ', offset_credit, dev_sum, current_credit))
  } else{
    current_credit = offset_credit
  }
  
  return(current_credit)
}

# determine characteristics of potential offset sites
prepare_offset_pool <- function(simulation_outputs, current_policy_params, region_ind, run_params, 
                                parcels, decline_rates_initial, yr){
  
  # if no developments or banked offsets for the current year return null object
  if (run_params$intervention_vec[yr] ==  0){
    offset_pool_object = list()
    return(offset_pool_object)
  }
  
  # select current set of available offset sites
  current_offset_pool <- simulation_outputs$index_object$indexes_to_use$offsets[[region_ind]]
  
  # if pool is empty return null object and print error
  if (length(current_offset_pool) == 0){ 
    print('empty offset pool flag')
    offset_pool_object = list()
    return(offset_pool_object)
  }
  
  if (current_policy_params$use_offset_bank == TRUE){
    # if running in offset bank mode select sites from current region 
    subset_pool = which(unlist(simulation_outputs$offset_bank_object$parcel_indexes) %in% parcels$regions[[region_ind]])
    
    # find set of offset characteristics that apply to current set of available sites
    offset_pool_object <- select_pool_subset(simulation_outputs$offset_bank_object, subset_pool)
    
    # find set of current cumulative site vals, record as projected val as calculation is from time
    # of original offset to current time for banking
    offset_pool_object$projected_vals <- find_current_parcel_sums(simulation_outputs$current_ecology[unlist(simulation_outputs$offset_bank_object$parcel_indexes[subset_pool])])
    
    offset_pool_type = 'offset_bank'
  } else {
    # when running in standard mode record current offsite site characteristics 
    offset_pool_object <- record_current_parcel_set(simulation_outputs$current_ecology[current_offset_pool], 
                                                    current_offset_pool, 
                                                    parcel_num_remaining = length(current_offset_pool), 
                                                    yr, 
                                                    region_ind)   #arrange available parcel pool into form to use in parcel set determination
    offset_pool_type = 'offsets'
  }
  
  # determine current gains characteristics
  
  offset_pool_object <- assess_current_pool(pool_object = offset_pool_object, 
                                            pool_type = offset_pool_type, 
                                            calc_type = current_policy_params$offset_calc_type, 
                                            cfacs_flag = current_policy_params$offset_cfacs_flag, 
                                            adjust_cfacs_flag = current_policy_params$adjust_offset_cfacs_flag, 
                                            action_type = current_policy_params$offset_action_type,
                                            include_potential_developments = current_policy_params$include_potential_developments_in_offset_calc,
                                            include_potential_offsets = current_policy_params$include_potential_offsets_in_offset_calc,
                                            include_illegal_clearing = current_policy_params$include_illegal_clearing_in_offset_calc,
                                            time_horizon_type = current_policy_params$offset_time_horizon_type,
                                            run_params,
                                            current_policy_params, 
                                            decline_rates_initial,
                                            time_horizon = current_policy_params$offset_time_horizon, 
                                            yr)      #determine available parcel values, depending on what particular offset policy is in use using counterfactuals etc.
  return(offset_pool_object)
}


# routines to perform offset banking
perform_banking_routine <- function(simulation_outputs, current_policy_params, yr, region_ind, run_params){
  
  # how many offsets to be added in current year
  offset_bank_num = unlist(current_policy_params$banked_offset_vec[yr])    
  if (offset_bank_num == 0){
    return(simulation_outputs)
  }
  
  # select current number of offset sites from current available pool to add to banked offset pool
  current_banked_offset_pool <- sample(simulation_outputs$index_object$indexes_to_use$offsets[[region_ind]], offset_bank_num)    
  
  # number of sites to potentially offset
  parcel_num_remaining = length(c(unlist(simulation_outputs$index_object$indexes_to_use$offsets[[region_ind]]), 
                                  unlist(simulation_outputs$index_object$indexes_to_use$devs[[region_ind]])))
  
  
  
  # record current offset pool characteristics
  current_banked_object <- record_current_parcel_set(simulation_outputs$current_ecology[current_banked_offset_pool], 
                                                     current_pool = current_banked_offset_pool, 
                                                     parcel_num_remaining, 
                                                     yr, 
                                                     region_ind)   # arrange current parcel data  
  
  simulation_outputs$offset_bank_object = append_current_group(object_to_append = simulation_outputs$offset_bank_object, 
                                                               current_object = current_banked_object, 
                                                               append_routine = 'banked_offset')
  
  # remove current group of sites from available pool
  simulation_outputs$index_object <- update_indexes_to_use(simulation_outputs$index_object, 
                                                           current_banked_offset_pool, 
                                                           region_ind)
  
  simulation_outputs$decline_rates <- update_decline_rates(simulation_outputs$decline_rates,     # set decline rate parameters to offset
                                                           restoration_rate_params = run_params$restoration_rate_params,
                                                           sample_restoration_rate = run_params$sample_restoration_rate,
                                                           features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc, 
                                                           feature_num = run_params$feature_num, 
                                                           decline_rate_type = 'offset', 
                                                           action_type = current_policy_params$offset_action_type, 
                                                           current_banked_offset_pool) 
  return(simulation_outputs)
}


assess_parcel_sets <- function(current_ecology, offsets_object, run_params, current_policy_params, decline_rates_initial, time_horizon, yr){
  
  assessed_parcel_sets_object <- list()
  feature_num = run_params$feature_num
  
  offset_indexes = offsets_object$parcel_indexes
  parcel_set_count = length((offset_indexes))
  metric <- vector('list', parcel_set_count)
  
  for (parcel_set_ind in seq_len(parcel_set_count)){
    
    current_parcel_indexes <- unlist(offset_indexes[[parcel_set_ind]])
    current_parcel_set <- which(unlist(offsets_object$parcel_indexes) %in% current_parcel_indexes)
    current_offset_object <- lapply(seq_along(offsets_object), function(i) offsets_object[[i]][current_parcel_set])
    
    names(current_offset_object) <- names(offsets_object)
    current_offset_object$parcel_indexes = current_parcel_indexes
    parcel_vals_achieved <- assess_current_gain_pool(current_ecology, 
                                                     pool_object = current_offset_object,
                                                     pool_type = "offset_bank", 
                                                     calc_type = current_policy_params$offset_calc_type, 
                                                     cfacs_flag = current_policy_params$offset_cfacs_flag, 
                                                     adjust_cfacs_flag = current_policy_params$adjust_offset_cfacs_flag, 
                                                     include_potential_developments = current_policy_params$include_potential_developments_in_offset_calc,
                                                     include_potential_offsets = current_policy_params$include_potential_offsets_in_offset_calc,
                                                     include_illegal_clearing = current_policy_params$include_illegal_clearing_in_offset_calc,
                                                     time_horizon_type = current_policy_params$offset_time_horizon_type,
                                                     run_params, 
                                                     current_policy_params, 
                                                     decline_rates_initial, 
                                                     time_horizon, 
                                                     yr)      
    
    for (feature_ind in seq_len(feature_num)){
      metric[[parcel_set_ind]] = nested_list_sum(subtract_nested_lists(parcel_vals_achieved, current_offset_object$parcel_vals_used))
    }
  }
  
  parcel_set_success_inds <- unlist(lapply(seq_along(metric), function(i) (all(unlist(metric[[i]][run_params$features_to_use_in_offset_calc]) > 0))))
  
  site_success_inds = which(unlist(offsets_object$parcel_indexes) %in% unlist(offset_indexes[parcel_set_success_inds]))
  assessed_parcel_sets_object$site_success_inds <- offsets_object$parcel_indexes[site_success_inds]
  assessed_parcel_sets_object$metric <- metric
  
  return(assessed_parcel_sets_object)
}


assess_current_gain_pool <- function(current_ecology, pool_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, 
                                     include_potential_developments,include_potential_offsets,include_illegal_clearing,
                                     time_horizon_type, run_params, current_policy_params, decline_rates_initial, time_horizon, yr){
  
  current_pool = unlist(pool_object$parcel_indexes)
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  
  time_horizons <- generate_time_horizons(project_type = 'current', yr, offset_yrs, time_horizon, parcel_count)
  
  if (cfacs_flag == TRUE){
    
    cfacs_object = calc_cfacs(parcel_ecologies = pool_object$parcel_ecologies, 
                              parcel_num_remaining = pool_object$parcel_num_remaining,
                              run_params,
                              current_policy_params, 
                              decline_rates = decline_rates_initial[current_pool], 
                              time_horizons, 
                              offset_yrs, 
                              include_potential_developments,
                              include_potential_offsets,
                              include_illegal_clearing,
                              adjust_cfacs_flag = current_policy_params$adjust_offset_cfacs_flag,
                              features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc)
    
    cfac_vals = nested_list_tail(cfacs_object$cfacs_to_use)
    
  }
  projected_vals = current_ecology[current_pool]
  projected_vals = lapply(seq_along(projected_vals), function(i) lapply(seq_along(projected_vals[[i]]), function(j) sum(projected_vals[[i]][[j]] )))
  
  parcel_vals_achieved = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, projected_vals, cfac_vals)
  
  return(parcel_vals_achieved)
}


#construct a list of zero arrays with identical dimensions defined by array_dims
list_of_zeros <- function(list_dims, array_dims){
  list_object = vector('list', list_dims)
  for (list_ind in seq_len(list_dims)){
    list_object[[list_ind]] = array(0, array_dims)
  }
  return(list_object)
}




#used to break up array into smaller set of sub arrays defined by vx and vy that fit together to give input array
mcell <- function(x, vx, vy){      
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  # make an array composed of lists with dimenisons that define the land parcels/regions. 
  # The list format allows arrays of different sizes to be stored
  B = vector('list', rows*cols)   
  colStart = 0
  # run down through the columns of input array 
  for (i in seq_len(cols)){       
    rowStart = 0
    for (j in seq_len(rows)){ 
      #group elements of input array into sub arrays and assign to B
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


# define ecology dynamics by logistic curve
logistic_ecology <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_horizon, time_fill){
  
  if (time_fill == TRUE){
    time_vec = 0:time_horizon
  } else {time_vec = time_horizon}
  if ( (decline_rate != 0) & (parcel_vals > min_eco_val)){
    # define logistic curve given logistic parameter decline_rate. 
    t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
    eco_projected = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_vec - t_sh)))
  } else {
    # If decline_rate is zet to zero return same value for all time steps
    eco_projected = rep(parcel_vals, length(time_vec))
  }
  
  return(eco_projected)
}

# calculate the expected ecology under maintenaince, restoration
project_ecology <- function(current_parcel_ecology, min_eco_val, max_eco_val, current_dec_rate, time_horizon, time_fill){
  
  if (current_dec_rate == 1){
    # for maintain ecology copy current ecology to matrix of depth (time_horizon + 1)
    if (time_fill == TRUE){
      # return all ecology states over all time steps
      projected_ecology = matrix(rep(current_parcel_ecology, (time_horizon + 1)), ncol = length(current_parcel_ecology), byrow = TRUE)
    } else {
      # project for single time step
      projected_ecology = current_parcel_ecology
    }
  } else{ 
    # update ecology according to function defined in project_ecology function (in this case logistic_ecology) 
    # function parameters are contained in decline_rates array
    projected_ecology = sapply(current_parcel_ecology, logistic_ecology, min_eco_val, max_eco_val, 
                               decline_rate = current_dec_rate, time_horizon = time_horizon, time_fill)  
  }
  
  if (time_horizon == 0){
    dim(projected_ecology) = c(1, length(projected_ecology))
  }
  
  return(projected_ecology)
  
}


# project sites through all features - returns nested list of arrays where each nested array has length 
# defined by current_time_horizons and depth defined by feature number for all sites

project_parcel <- function(current_parcel_ecologies, current_decline_rates, current_time_horizons, run_params, features_to_use_in_offset_calc, time_fill){
  
  parcel_trajs = lapply(seq_along(current_parcel_ecologies), 
                        function(i) (lapply(features_to_use_in_offset_calc,  
                                            function(j) project_ecology(current_parcel_ecologies[[i]][[j]], 
                                                                        run_params$min_eco_val, 
                                                                        run_params$max_eco_val,
                                                                        current_dec_rate = current_decline_rates[[i]][[j]], 
                                                                        time_horizon = unlist(current_time_horizons[i]), 
                                                                        time_fill))))
  
  return(parcel_trajs)
}



# find sum nested list of depth j with identical structure
nested_list_sum <- function(nested_list){
  
  if (length(nested_list) > 0){
    summed_list <- lapply(seq_along(nested_list[[1]]), 
                          function(j) Reduce('+', lapply(seq_along(nested_list), function(i) nested_list[[i]][[j]])))
    return(summed_list)
  } else {
    return(NULL)
  }
  
}


#build nested list with length outer_dim and depth defined by inner_dim
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


remove_index <- function(object_list, ind_to_remove){
  if (length(ind_to_remove) > 0){
    object_list <- object_list[-ind_to_remove]
  }
  return(object_list)
}

match_parcel_set <- function(offset_pool_object, current_credit, dev_weights, run_params, current_policy_params, 
                             intervention_vec, indexes_to_use, current_ecology, decline_rates_initial, 
                             land_parcels, yr, time_horizon, region_ind){
  match_object = false_match()
  
  current_pool_vals = offset_pool_object$parcel_vals_used
  current_pool_indexes = offset_pool_object$parcel_indexes

  parcel_num_remaining = length(indexes_to_use)
  current_match_pool = indexes_to_use
  
  if ((length(offset_pool_object$parcel_indexes) == 0) | (parcel_num_remaining == 0)){
    return(match_object)
  }
  
  current_pool_vals_array <- matrix(unlist(current_pool_vals), nrow = length(current_pool_vals), byrow=TRUE)
  zero_inds <- which(apply(current_pool_vals_array, MARGIN = 1, sum) == 0)
  
  current_pool_vals = remove_index(current_pool_vals, zero_inds)
  current_pool_indexes = remove_index(current_pool_indexes, zero_inds)
  
  # store group of site characteristics in site characteristics object
  dev_pool_object <- record_current_parcel_set(current_ecology[current_match_pool], 
                                               current_match_pool, 
                                               parcel_num_remaining, 
                                               yr, 
                                               region_ind) #record potential current development parcel attributes
  
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, 
                                         pool_type = 'devs', 
                                         calc_type = current_policy_params$dev_calc_type, 
                                         cfacs_flag = current_policy_params$dev_cfacs_flag, 
                                         adjust_cfacs_flag = current_policy_params$adjust_dev_cfacs_flag, 
                                         action_type = current_policy_params$offset_action_type,
                                         include_potential_developments = current_policy_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = current_policy_params$include_potential_offsets_in_dev_calc,
                                         include_illegal_clearing = current_policy_params$include_illegal_clearing_in_dev_calc,
                                         time_horizon_type = 'future',
                                         run_params, 
                                         current_policy_params,
                                         decline_rates_initial, 
                                         time_horizon, 
                                         yr)  #determine future development parcel attributes
  
  current_match_vals_pool = dev_pool_object$parcel_vals_used
  
  if (sum(unlist(current_match_vals_pool))== 0){
    cat('\nall projected developments are zero - blocking all developments')
    match_object = false_match()
    match_object$offset_object = list()
    match_object$current_credit = current_credit
    return(match_object)
  }
  
  while( (match_object$match_flag == FALSE) & length(current_match_pool > 1) ){   #any potential parcel set match requires a minimum of two sites
    
    if (current_policy_params$development_selection_type == 'random'){
      sample_ind = sample(seq_along(current_match_pool), size = 1)
    } else if (current_policy_params$development_selection_type == 'weighted'){
      
      current_dev_weights = dev_weights[unlist(current_match_pool)]
      current_dev_weights = lapply(seq_along(current_dev_weights), function(i) current_dev_weights[[i]]/sum(unlist(current_dev_weights)))
      sample_ind = sample(seq_along(current_match_pool), size = 1, prob = current_dev_weights)
    }
    
    current_test_index = current_match_pool[sample_ind]
    vals_to_match = current_match_vals_pool[[sample_ind]]
    
    if (current_policy_params$use_offset_bank == FALSE){
      dev_ind = list_intersect(current_pool_indexes, current_test_index) #find and remove index that corresponds to potiential development index
      match_pool_to_use = remove_index(current_pool_indexes, dev_ind$match_ind)
      vals_to_use = remove_index(current_pool_vals, dev_ind$match_ind)
    } else {
      match_pool_to_use = current_pool_indexes  #if performing offset banking use any of the available banked offset pool
      vals_to_use = current_pool_vals
    }
    
    match_object <- select_from_pool(match_type = 'offset', 
                                     match_procedure = 'euclidean', 
                                     current_pool = match_pool_to_use, 
                                     vals_to_use, 
                                     current_credit, 
                                     dev_weights, 
                                     allow_developments_from_credit = current_policy_params$allow_developments_from_credit, 
                                     offset_multiplier = current_policy_params$offset_multiplier, 
                                     match_threshold = run_params$match_threshold, 
                                     vals_to_match_initial = vals_to_match, 
                                     current_policy_params$site_for_site, 
                                     features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc,
                                     max_offset_parcel_num = run_params$max_offset_parcel_num,
                                     yr) #perform matching routine
    
    if (match_object$match_flag == FALSE){
      
      inds_to_keep = which(lapply(seq_along(dev_pool_object$parcel_vals_used), 
                                  function(i) all(unlist(subtract_nested_lists(dev_pool_object$parcel_vals_used[[i]], vals_to_match)) < 0) ) == TRUE)
      current_match_pool = dev_pool_object$parcel_indexes[inds_to_keep]     #remove current potential development from potential pool
      current_match_vals_pool = dev_pool_object$parcel_vals_used[inds_to_keep]
      #       cat('\n', length(current_match_pool))
      #       cat('\n', unlist(vals_to_match))
    }
  }
  
  if (match_object$match_flag == TRUE){
    dev_match_index = which(unlist(dev_pool_object$parcel_indexes) == current_test_index)
    match_object$development_object = select_pool_subset(dev_pool_object, unlist(dev_match_index))
    subset_pool =  list_intersect(offset_pool_object$parcel_indexes, match_object$match_indexes)
    offset_object <- select_pool_subset(pool_object = offset_pool_object, subset_pool = subset_pool$match_ind)
    match_object$offset_object = offset_object
    current_credit[run_params$features_to_use_in_offset_calc] = match_object$current_credit
    match_object$current_credit = current_credit
  } else if (match_object$match_flag == FALSE){
    match_object$offset_object = list()
    match_object$current_credit = current_credit
  } 
  
  return(match_object)
  
}


develop_from_credit <- function(current_ecology, current_credit, dev_weights, run_params, current_policy_params, 
                                intervention_vec, dev_indexes_to_use, decline_rates_initial, land_parcels, region_ind, yr, time_horizon){
  
  parcel_num_remaining = length(dev_indexes_to_use)
  
  # store group of site characteristics in site characteristics object
  dev_pool_object <- record_current_parcel_set(current_ecology[dev_indexes_to_use],  dev_indexes_to_use,  parcel_num_remaining,  yr,  region_ind)
  
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, 
                                         pool_type = 'devs', 
                                         calc_type = current_policy_params$dev_calc_type, 
                                         cfacs_flag = current_policy_params$dev_cfacs_flag, 
                                         adjust_cfacs_flag = current_policy_params$adjust_dev_cfacs_flag, 
                                         action_type = current_policy_params$offset_action_type,
                                         include_potential_developments = current_policy_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = current_policy_params$include_potential_offsets_in_dev_calc,
                                         include_illegal_clearing = current_policy_params$include_illegal_clearing_in_dev_calc,
                                         time_horizon_type = 'future',
                                         run_params, 
                                         current_policy_params,
                                         decline_rates_initial,
                                         time_horizon, 
                                         yr)
  
  if (length(unlist(dev_pool_object$parcel_indexes) > 0)){
    match_object <- select_from_pool(match_type = 'development', 
                                     match_procedure = 'random', 
                                     current_pool = dev_pool_object$parcel_indexes, 
                                     vals_to_use = dev_pool_object$parcel_vals_used, 
                                     current_credit,
                                     dev_weights, 
                                     allow_developments_from_credit = FALSE, 
                                     offset_multiplier = current_policy_params$offset_multiplier, 
                                     match_threshold = run_params$match_threshold, 
                                     vals_to_match_initial = current_credit, 
                                     site_for_site = TRUE, 
                                     run_params$features_to_use_in_offset_calc, 
                                     max_offset_parcel_num = run_params$max_offset_parcel_num, 
                                     yr)
    
  } else{
    match_object = false_match()
  }
  
  if (match_object$match_flag == TRUE){
    subset_pool =  list_intersect(dev_pool_object$parcel_indexes, match_object$match_indexes)
    match_object$development_object = select_pool_subset(dev_pool_object, subset_pool = subset_pool$match_ind)
    current_credit[run_params$features_to_use_in_offset_calc] = match_object$current_credit
    match_object$current_credit = current_credit
  } else{
    match_object$development_object = list()
    match_object$current_credit = current_credit
  }
  
  return(match_object)
  
}


evaluate_parcel_vals <- function(calc_type, current_condition_vals, projected_vals, cfac_vals){
  
  if (calc_type == 'current_condition'){
    projected_vals = current_condition_vals
    cfac_vals = list_of_zeros(length(current_condition_vals), 1)
  } else if (calc_type == 'restoration_gains'){
    cfac_vals = current_condition_vals
  } else if (calc_type == 'restoration_condition_value'){
    cfac_vals = list_of_zeros(length(projected_vals), 1)
  } else if (calc_type == 'avoided_condition_decline'){
    projected_vals = current_condition_vals
  } else if ((calc_type == 'future_condition')){
    projected_vals = cfac_vals
    cfac_vals = list_of_zeros(length(current_condition_vals), 1)
  } 
  parcel_vals_pool = subtract_nested_lists(projected_vals, cfac_vals)
  return(parcel_vals_pool)
}





# evaluate_parcel_vals <- function(calc_type, parcel_sums_at_offset, projected_vals, cfac_vals){
#   
#   
#   if (calc_type == 'current_condition'){
#     parcel_vals_pool = parcel_sums_at_offset
#   } else if (calc_type == 'restoration_gains'){
#     parcel_vals_pool = subtract_nested_lists(projected_vals, parcel_sums_at_offset)
#   } else if (calc_type == 'net_gains'){
#     parcel_vals_pool = subtract_nested_lists(projected_vals, cfac_vals)
#   } else if (calc_type == 'restoration_condition_value'){
#     parcel_vals_pool = projected_vals
#   } else if (calc_type == 'avoided_condition_decline'){
#     parcel_vals_pool = subtract_nested_lists(parcel_sums_at_offset, cfac_vals)
#   } else if ((calc_type == 'future_condition') || (calc_type == 'protected_condition')){
#     parcel_vals_pool = cfac_vals 
#   } else{
#     parcel_vals_pool = list()
#   }
#   return(parcel_vals_pool)
# }


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


# store group of site characteristics in parcel_set_object
record_current_parcel_set <- function(current_ecologies, current_pool, parcel_num_remaining, yr, region_ind){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = rep(list(yr), length(current_pool))
  parcel_set_object$parcel_ecologies = current_ecologies
  parcel_set_object$parcel_sums_at_offset = sum_sites(current_ecologies)
  parcel_set_object$parcel_indexes = as.list(current_pool)
  parcel_set_object$parcel_num_remaining = rep(list(parcel_num_remaining), length(current_pool))
  parcel_set_object$region_ind = rep(list(region_ind), length(current_pool))
  
  return(parcel_set_object)
  
}

# determine last element of all nested list vectors
nested_list_tail <- function(list_a){
  last_elements <- lapply(seq_along(list_a), 
                          function(i) lapply(seq_along(list_a[[i]]), 
                                             function(j) list_a[[i]][[j]][ length(list_a[[i]][[j]]) ] ))
  return(last_elements)
}




find_intervention_probability <- function(intervention_vec, offset_yrs, calc_type, offset_intervention_scale, time_horizons, parcel_num, parcel_num_remaining, time_steps){
  
  intervention_probs = vector('list', parcel_num)
  parcel_num_remaining = unlist(parcel_num_remaining)
  offset_yrs = unlist(offset_yrs)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_ind]
    offset_yr = offset_yrs[parcel_ind]
    current_prob = array(0, (time_horizon + 1))
    
    current_vec = intervention_vec[offset_yr:time_steps]
    
    if (length(current_vec) < (time_horizon + 1)){
      current_vec = c(current_vec, array(0, ((time_horizon + 1) - length(current_vec))))
    }
    
    current_vec = current_vec[1:(time_horizon + 1)]
    current_parcel_num_remaining = parcel_num_remaining[parcel_ind]
    if (current_parcel_num_remaining > 0) {
      current_prob = current_prob + current_vec/current_parcel_num_remaining
    } 
    
    if (calc_type == 'offset'){
      current_prob = offset_intervention_scale*current_prob
    }
    intervention_probs[[parcel_ind]] = current_prob
    
  }
  return(intervention_probs)
}



# test to determine nearest neighbour by Euclidean norm given a pool of potential candidates (parcel_vals_pool) 
# and the criterion to match to (vals_to_match)

euclidean_norm_match <- function(parcel_vals_pool, vals_to_match){
  
  # build matrix composed of summed sites (summed through features)
  vals_to_test = matrix(unlist(parcel_vals_pool), nrow = length(parcel_vals_pool), byrow=TRUE)
  # build matrix composed of copies of summed site to match to (summed through features)
  match_array = matrix(rep(vals_to_match, nrow(vals_to_test)), ncol = length(vals_to_match), byrow = TRUE)
  
  #build euclidean metric to perform euclidean match
  err = sqrt(rowSums( (vals_to_test - match_array)^2 ) )
  #chose site that minimises euclidean metric
  match_ind = which(err == min(err))
  
  if (length(match_ind) > 1){
    #list where the duplicate occurred
    paste0('duplicate match flag on ', parcel_vals_pool[match_ind])
    # chose first site of the multiple match
    match_ind = sample(match_ind, 1) 
  }
  
  match_vals = parcel_vals_pool[match_ind]
  
  match_object = list()
  match_object$match_vals = match_vals
  match_object$match_ind = match_ind
  return(match_object)
}


#return false match object
false_match <- function(){
  match_object = list()
  match_object$match_flag = FALSE
  return(match_object)
}


select_cols <- function(arr_to_use, col_inds){
  arr_to_use <- arr_to_use[, col_inds]
  arr_to_use <- t(t(arr_to_use))
  return(arr_to_use)
}



select_pool_to_match <- function(features_to_use_in_offset_calc, ndims, thresh, pool_num, vals_to_use, vals_to_match, match_threshold, 
                                 current_pool, allow_developments_from_credit, current_credit, site_for_site, match_type){
  
  pool_object = list()
  
  if (length(unlist(vals_to_use)) > 0){
    vals_to_test = matrix(unlist(vals_to_use), nrow = pool_num, byrow=TRUE)
  } else {
    pool_object$break_flag = TRUE
    return(pool_object)
  }
  
  vals_to_test <- select_cols(vals_to_test, features_to_use_in_offset_calc)
  zero_inds <- which(apply(vals_to_test, MARGIN = 1, sum) == 0)  
  
    vals_to_use <- remove_index(vals_to_use, zero_inds)
    current_pool <- remove_index(current_pool, zero_inds)
    vals_to_test <- remove_index(vals_to_test, zero_inds)
    pool_num = length(vals_to_test)
    if (length(current_pool) == 0){
      cat('\nall parcels yield zero assessment')
      pool_object$break_flag = TRUE
      return(pool_object)
    } 
  
  vals_to_use <- lapply(seq_along(vals_to_use), function(i) vals_to_use[[i]][features_to_use_in_offset_calc])
  
  if ( (match_type == 'offset') & (allow_developments_from_credit == TRUE) ){
    vals_to_match = vals_to_match - unlist(current_credit[features_to_use_in_offset_calc])
  }
  
  if (site_for_site == TRUE){
    match_array = matrix(rep(vals_to_match, pool_num), ncol = ndims, byrow = TRUE)
    thresh_array = matrix(rep(thresh, pool_num), ncol = ndims, byrow = TRUE)
  } else {
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
    if (match_type == 'development'){
      cat('\n current credit of', unlist(current_credit), 'is insufficient to allow development with min of ', min(vals_to_test), '\n')
    } else {
      cat('\n insufficient offset gains available to allow development \n')
    }
    pool_object$break_flag = TRUE
    return(pool_object)
  } else {
    
    if (site_for_site == TRUE){
      vals_to_use <- vals_to_use[inds_to_use]
      current_pool <- current_pool[inds_to_use]
    }
    
    pool_object$break_flag = FALSE
    pool_object$vals_to_use = vals_to_use
    pool_object$current_pool = current_pool
    return(pool_object)
  } 
  
  
}


select_from_pool <- function(match_type, match_procedure, current_pool, vals_to_use, current_credit_to_use, dev_weights, allow_developments_from_credit, 
                             offset_multiplier, match_threshold, vals_to_match_initial, site_for_site, features_to_use_in_offset_calc, max_offset_parcel_num, yr){
  
  ndims = length(features_to_use_in_offset_calc)
  thresh = array(match_threshold, ndims)         #create an array of threshold values with length equal to the dimensions to match to
  pool_num = length(current_pool)
  
  vals_to_match = offset_multiplier*unlist(vals_to_match_initial[features_to_use_in_offset_calc])
  
  pool_object <- select_pool_to_match(features_to_use_in_offset_calc, ndims, thresh, pool_num, vals_to_use, vals_to_match, match_threshold, 
                                      current_pool, allow_developments_from_credit, current_credit_to_use, site_for_site, match_type)
  
  if (pool_object$break_flag == TRUE){
    match_object = false_match()
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
    } else if (match_procedure == 'weighted'){
      match_params = list()
      match_params$match_ind = sample(length(current_pool), 1, dev_weights[current_pool])
      match_params$match_vals = parcel_vals_pool[match_params$match_ind]
    }
    
    current_match_val = unlist(match_params$match_vals)
    current_match_index = current_pool[match_params$match_ind]
    vals_to_match = vals_to_match - current_match_val
    
    if (site_for_site == FALSE){
      
      ind_to_remove = list_intersect(current_pool, current_match_index)
      current_pool = remove_index(current_pool, ind_to_remove$match_ind)
      parcel_vals_pool = remove_index(parcel_vals_pool, ind_to_remove$match_ind)
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
  
  current_credit_to_use = vals_to_match
  if (match_type == 'offset'){
    current_credit_to_use = -current_credit_to_use # switch sign for any additional credit from offset 
  }
  
  match_object = list()
  match_object$match_indexes = match_indexes
  match_object$match_vals = match_vals
  match_object$match_flag = match_flag
  #match_object$current_credit_to_use = -vals_to_match * (abs(vals_to_match) > match_threshold)
  match_object$current_credit = as.list(current_credit_to_use)
  match_object$vals_to_match = vals_to_match_initial
  
  return(match_object)
  
}




# determine cumulative value of all sites within parcel ecologies for multiple features

sum_sites <- function(current_ecologies){
  parcel_num = length(current_ecologies)
  parcel_sums = vector('list', parcel_num)
  for (parcel_ind in seq_len(parcel_num)){
    # find sums through all features of each parcel
    parcel_sums[[parcel_ind]] = lapply(seq_along(current_ecologies[[parcel_ind]]), 
                                       function(i) sum(current_ecologies[[parcel_ind]][[i]]) )
  }
  return(parcel_sums)
}


# determine cumulative sum of all sites within parcel ecologies for 1D features
sum_ecologies <- function(parcel_ecologies){
  parcel_sums <- lapply(seq_along(parcel_ecologies), function(i) sum(parcel_ecologies[[i]] ))
}


#remove site from available pool for offsets and developments. This is a two stage process to cover when offsets and developments may not overlap
update_indexes_to_use <- function(index_object, parcel_indexes, region_ind){
  #remove parcel from available list   
  index_object$indexes_to_use$offsets[[region_ind]] = setdiff(index_object$indexes_to_use$offsets[[region_ind]], parcel_indexes)  
  #remove parcel from available list   
  
  index_object$indexes_to_use$devs[[region_ind]] = setdiff(index_object$indexes_to_use$devs[[region_ind]], parcel_indexes) 
  return(index_object)
}


# update ecology change parameters
# routine to label offset and development sites - updates decline_rates with 0 entry for developments, 
# and 1/restoration_parameter for maintain/restoration offset. Protection offset leaves decline_rates unaffected 
update_decline_rates <- function(decline_rates, restoration_rate_params, sample_restoration_rate, features_to_use_in_offset_calc, feature_num, decline_rate_type, action_type, parcel_indexes){
  
  for (current_parcel_ind in unlist(parcel_indexes)){
    
    if (decline_rate_type == 'development'){
      decline_rates[[current_parcel_ind]] <- rep(list(0), feature_num)
    } else if (decline_rate_type == 'offset'){
      
      if (action_type == 'maintain'){
        decline_rates[[current_parcel_ind]][features_to_use_in_offset_calc] = rep(list(1), length(features_to_use_in_offset_calc))
      } else if (action_type == 'restore'){
        if (sample_restoration_rate == TRUE){
          # spread restoration rates about mean
          decline_rates[[current_parcel_ind]][features_to_use_in_offset_calc] = as.list(rnorm(length(features_to_use_in_offset_calc), mean = restoration_rate_params[1], sd = restoration_rate_params[2]))
        } else{
          # use identical restoration rates for each site
          decline_rates[[current_parcel_ind]][features_to_use_in_offset_calc] = rep(list(restoration_rate_params[1]), length(features_to_use_in_offset_calc))
        } 
        
      } 
    }
  }
  
  return(decline_rates)
  
}


#intersection routine for lists with catches on null lists
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


# Determine subset of object (eg development or offset group) containing a nested set of components with the same structure
select_pool_subset <- function(pool_object, subset_pool){
  object_subset = lapply(seq_along(pool_object), function(i) pool_object[[i]][subset_pool])
  names(object_subset) <- names(pool_object)
  return(object_subset)
}


#function to work out vector of time intervals used in gains calculations
generate_time_horizons <- function(project_type, yr, offset_yrs, time_horizon, parcel_count){
  
  if (project_type == 'current'){
    # work out time intervals from time of offset to current year
    time_horizons = rep(yr, parcel_count) - offset_yrs
  } else if (project_type == 'future'){
    # work out time intervals from current year to projected year defined by time_horizon
    time_horizons = rep(time_horizon, parcel_count)
  } else if (project_type == 'zeros'){
    #set up dummy times
    time_horizons = rep(0, parcel_count)
  }
  return(time_horizons)
}



assess_current_pool <- function(pool_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, action_type, 
                                include_potential_developments, include_potential_offsets, include_illegal_clearing,
                                time_horizon_type, run_params, current_policy_params, decline_rates_initial, time_horizon, yr){
  
  current_pool = unlist(pool_object$parcel_indexes)
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  
  current_feature_num = length(run_params$features_to_use_in_offset_calc)
  
  current_condition_vals = lapply(seq_along(pool_object$parcel_sums_at_offset), 
                                  function(i) pool_object$parcel_sums_at_offset[[i]][run_params$features_to_use_in_offset_calc])
  
  if (calc_type == 'current_condition') {
    projected_vals = current_condition_vals
    cfac_vals = list_of_zeros(length(pool_object$parcel_sums_at_offset), 1)
  } else {
    time_horizons <- generate_time_horizons(project_type = 'future', yr, offset_yrs, time_horizon, parcel_count)
    if (cfacs_flag == TRUE){
      cfacs_object = calc_cfacs(parcel_ecologies = pool_object$parcel_ecologies, 
                                parcel_num_remaining = pool_object$parcel_num_remaining, 
                                run_params, 
                                current_policy_params, 
                                decline_rates_initial[current_pool], 
                                time_horizons, 
                                offset_yrs, 
                                include_potential_developments,
                                include_potential_offsets,
                                include_illegal_clearing,
                                adjust_cfacs_flag,
                                features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc)
      cfac_vals = nested_list_tail(cfacs_object$cfacs_to_use)
    } else if (calc_type == 'restoration_gains'){
      cfac_vals = current_condition_vals
    } else {
      cfac_vals = list_of_zeros(length(pool_object$parcel_sums_at_offset), 1)
    }
    
    if (pool_type == 'offsets') {
      if (action_type == 'maintain'){
        current_decline_rates = simulate_decline_rates(length(pool_object$parcel_ecologies), 
                                                       sample_decline_rate = FALSE, 
                                                       mean_decline_rates = rep(list(1), current_feature_num), 
                                                       decline_rate_std = vector(), 
                                                       current_feature_num)
      } else if (action_type == 'restore'){
        current_decline_rates = simulate_decline_rates(length(pool_object$parcel_ecologies), 
                                                       sample_decline_rate = FALSE, 
                                                       mean_decline_rates = rep(list(run_params$restoration_rate_params[1]), current_feature_num), 
                                                       decline_rate_std = rep(list(run_params$restoration_rate_params[2]), current_feature_num), 
                                                       current_feature_num)
      } else if (action_type == 'protect'){
        current_decline_rates = decline_rates_initial[current_pool]
      }
      
      projected_vals = project_parcel(current_parcel_ecologies = pool_object$parcel_ecologies, 
                                      current_decline_rates, 
                                      time_horizons, 
                                      run_params,
                                      features_to_use_in_offset_calc = run_params$features_to_use_in_offset_calc,
                                      time_fill = FALSE)
      
      projected_vals = lapply(seq_along(projected_vals), 
                              function(i) lapply(seq_along(projected_vals[[i]]), function(j) sum(projected_vals[[i]][[j]] )))
    } else if (pool_type == 'devs') {
      projected_vals = cfac_vals
      cfac_vals = list_of_zeros(length(pool_object$parcel_sums_at_offset), 1)
    }
  
  }
  pool_object$parcel_vals_used = subtract_nested_lists(projected_vals, cfac_vals)
  
  return(pool_object)
  
}


# function to append current object characteristics to group 
append_current_group <- function(object_to_append, current_object, append_routine){
  
  if (length(object_to_append) == 0){
    #append null object with same fields to maintain order
    object_to_append = vector('list', length(current_object))
    names(object_to_append) = names(current_object)
  }
  
  if (append_routine == 'banked_offset'){
    appended_object <- append_current_object(object_to_append, 
                                             current_object, 
                                             append_type = 'as_list', 
                                             inds_to_append = seq_along(object_to_append))      #record current offset parcels in offsets object containing all offsets info
  } else {
    #record current offset parcels in offsets object containing all offset characteristics
    # needs a two step process as parcel_indexes are nested and the others are not
    
    appended_object <- append_current_object(object_to_append, 
                                             current_object, 
                                             append_type = 'as_list', 
                                             inds_to_append = which(!(names(current_object) == 'parcel_indexes')))         
    
    appended_object <- append_current_object(appended_object, 
                                             current_object, 
                                             append_type = 'as_group', 
                                             inds_to_append = which(names(current_object) == 'parcel_indexes'))
  }
  return(appended_object)
  
}




append_current_object <- function(parcel_set_object, current_parcel_set_object, append_type, inds_to_append){
  if (append_type == 'as_group'){
    #routine to append by characteristic for nested object
    
    parcel_set_object[inds_to_append] <- lapply(inds_to_append, function(i) append(parcel_set_object[[i]], list(current_parcel_set_object[[i]])))
  } else if (append_type == 'as_list'){
    #routine to append by characteristic for non nested object
    parcel_set_object[inds_to_append] <- lapply(inds_to_append, function(i) append(parcel_set_object[[i]], current_parcel_set_object[[i]]))
  }
  #set names of group to current object name
  names(parcel_set_object) = names(current_parcel_set_object)
  return(parcel_set_object)
}



kill_development_ecology <- function(current_ecology, decline_rates, feature_num){
  
  parcel_num = length(current_ecology)
  
  for (parcel_ind in seq_len(parcel_num)){
    for (feature_ind in seq_len(feature_num)){
      #select all development sites identified by decline rates = 0
      
      current_decline_rate = decline_rates[[parcel_ind]][[feature_ind]]      
      if (current_decline_rate == 0){
        #set all features of development sites to zero
        current_ecology[[parcel_ind]][[feature_ind]] = array(0, length(current_ecology[[parcel_ind]][[feature_ind]]))   
      } 
    }
  }
  return(current_ecology) 
}



calc_cfacs <- function(parcel_ecologies, parcel_num_remaining, run_params, current_policy_params, 
                       decline_rates, time_horizons, offset_yrs, include_potential_developments, include_potential_offsets, include_illegal_clearing, 
                       adjust_cfacs_flag, features_to_use_in_offset_calc){
  
  cfacs_object = list()
  if (length(decline_rates) != length(parcel_ecologies)){
    cat('\ncalc cfacs length error')
  }
  
  cfacs_object$cfacs = project_parcel(parcel_ecologies, 
                                      decline_rates, 
                                      time_horizons, 
                                      run_params,
                                      features_to_use_in_offset_calc,
                                      time_fill = TRUE)
  
  if (adjust_cfacs_flag == TRUE){
    cfacs_object$adjusted_cfacs = adjust_cfacs(cfacs_object$cfacs, 
                                               include_potential_developments,
                                               include_potential_offsets,
                                               include_illegal_clearing,
                                               run_params, 
                                               current_policy_params, 
                                               parcel_num_remaining, 
                                               decline_rates, 
                                               time_horizons, 
                                               offset_yrs)
    
    cfacs_object$cfacs_to_use = sum_trajectories(cfacs_object$adjusted_cfacs)
  } else{
    cfacs_object$cfacs_to_use = sum_trajectories(cfacs_object$cfacs)
  }
  
  return(cfacs_object)
  
}  


adjust_cfacs <- function(current_cfacs, include_potential_developments,include_potential_offsets, include_illegal_clearing, run_params,
                         current_policy_params, parcel_num_remaining, decline_rates, time_horizons, offset_yrs){
  
  time_horizons = unlist(time_horizons)
  
  weighted_counters_object <- find_weighted_counters(current_cfacs, 
                                                     include_illegal_clearing, 
                                                     include_potential_developments, 
                                                     include_potential_offsets,  
                                                     intervention_vec = run_params$intervention_vec, 
                                                     illegal_clearing_prob = run_params$illegal_clearing_prob,
                                                     offset_intervention_scale = run_params$max_offset_parcel_num,
                                                     run_params$feature_num, 
                                                     parcel_num_remaining, 
                                                     parcel_num = length(current_cfacs), 
                                                     time_horizons, 
                                                     offset_yrs, 
                                                     time_steps = run_params$time_steps)
  
  if (length(decline_rates) != length(current_cfacs)){
    cat('\nlength error')
  }
  
  if (include_potential_offsets == FALSE){
    adjusted_cfacs = weighted_counters_object$weighted_counters
  } else {
    
    offset_projections <- calc_offset_projections(current_cfacs, 
                                                  weighted_counters_object$offset_intervention_probs, 
                                                  run_params$restoration_rate_params, 
                                                  current_policy_params$offset_action_type,
                                                  decline_rates, 
                                                  time_horizons, 
                                                  run_params$feature_num, 
                                                  run_params$min_eco_val, 
                                                  run_params$max_eco_val)
    
    summed_offset_projections <- sum_offset_projs(offset_projections,
                                                  offset_probs = weighted_counters_object$offset_intervention_probs, 
                                                  run_params$feature_num, 
                                                  time_horizons)
    
    adjusted_cfacs = sum_clearing_offsets(weighted_counters_object$weighted_counters, 
                                          summed_offset_projections, 
                                          run_params$feature_num)
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
      weighted_probs <- lapply(seq_len(parcel_num), function(i) rep(illegal_clearing_prob, (time_horizons[i] + 1)))    #runif(n = (time_horizon + 1), min = 0, max = run_params$illegal_clearing_prob)
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




find_weighted_counters <- function(current_cfacs, include_illegal_clearing, include_potential_developments, include_potential_offsets, 
                                   intervention_vec, illegal_clearing_prob, offset_intervention_scale, feature_num, parcel_num_remaining, 
                                   parcel_num, time_horizons, offset_yrs, time_steps){
  
  
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
  weighted_counters_object$weighted_counters = lapply(seq_along(current_cfacs), 
                                                      function(i) lapply(seq_along(current_cfacs[[i]]), 
                                                                         function(j) current_cfacs[[i]][[j]]*matrix(rep(counter_weights[[i]], dim(current_cfacs[[i]][[j]])[2]), 
                                                                                                                    nrow = dim(current_cfacs[[i]][[j]])[1], byrow = FALSE)))
  
  weighted_counters_object$offset_intervention_probs = offset_intervention_probs
  
  
  
  return(weighted_counters_object)
}





##########     ERRORS IN THIS CODE   ########

calc_offset_projections <- function(current_cfacs, offset_probs, restoration_rate_params, action_type, decline_rates, time_horizons, feature_num, min_eco_val, max_eco_val){
  
  if (length(decline_rates) != length(current_cfacs)){
    cat('\nlength error')
  }
  parcel_num = length(current_cfacs)
  offset_projections = vector('list', parcel_num)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_ind] + 1
    current_offset_probs = offset_probs[[parcel_ind]]
    current_offset_projections = generate_nested_list(outer_dim = feature_num, inner_dim = time_horizon)
    
    for (feature_ind in seq_len(feature_num)){
      
      current_cfac = current_cfacs[[parcel_ind]][[feature_ind]]
      current_dec_rate = decline_rates[[parcel_ind]][[feature_ind]]
      
      for (proj_yr in seq_len(time_horizon)){
        current_offset_projections[[feature_ind]][[proj_yr]] = array(0, dim(current_cfac))
        
        if (current_offset_probs[proj_yr] > 0){
          current_parcel_ecology = list(current_cfac[proj_yr, ])
          
          current_offset_proj = project_ecology(current_parcel_ecology, 
                                                action_type, 
                                                min_eco_val, 
                                                max_eco_val,
                                                feature_num = length(current_parcel_ecology),
                                                restoration_rate_params, 
                                                current_dec_rate, 
                                                (time_horizon - proj_yr), 
                                                time_fill = TRUE)
          
          current_offset_projections[[feature_ind]][[proj_yr]][proj_yr:time_horizon, ] = current_offset_proj[[1]]  #THIS IS WRONG
        }
      }
    }
    offset_projections[[parcel_ind]] = current_offset_projections
  }
  
  return(offset_projections)
  
}


sum_offset_projs <- function(offset_projections, offset_probs, feature_num, time_horizons){
  parcel_num = length(offset_projections)
  summed_offset_projections = vector('list', parcel_num)
  for (parcel_ind in seq_len(parcel_num)){
    
    summed_offset_projections[[parcel_ind]] = vector('list', feature_num)
    current_offset_prob = offset_probs[[parcel_ind]]
    current_offset_prob <- current_offset_prob*(current_offset_prob > 0)
    
    current_offset_proj = offset_projections[[parcel_ind]]
    
    for (feature_ind in seq_len(feature_num)){
      current_offset_projections <- current_offset_proj[[feature_ind]]
      current_offset_projections <- lapply(seq_along(current_offset_projections), function(i) current_offset_projections[[i]]*current_offset_prob[i])
      summed_offset_projections[[parcel_ind]][[feature_ind]] = Reduce('+', current_offset_projections)
    }
  }
  
  return(summed_offset_projections)
}






sum_clearing_offsets <- function(cfacs_include_clearing, summed_offset_projections, feature_num){
  parcel_num = length(cfacs_include_clearing)
  cfacs_include_clearing_offsets = vector('list', parcel_num)
  
  for (parcel_ind in 1:parcel_num){
    cfacs_include_clearing_offsets[[parcel_ind]] = vector('list', feature_num)
  }
  
  for (parcel_ind in seq_len(parcel_num)){
    if (length(summed_offset_projections[[parcel_ind]]) > 0 ){
      for (feature_ind in seq_len(feature_num)){
        cfacs_include_clearing_offsets[[parcel_ind]][[feature_ind]] = summed_offset_projections[[parcel_ind]][[feature_ind]] + cfacs_include_clearing[[parcel_ind]][[feature_ind]]
      }
    }
  }
  return(cfacs_include_clearing_offsets)
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





# ind2sub <- function(rows, ind){       
#   # give an array with N rows, return location of array element "ind" in loc = [x, y] format
#   rw = ((ind-1) %% rows) + 1 
#   # identify row of current element using mod format
#   cl = floor((ind-1) / rows) + 1      
#   loc = c(rw, cl)
#   return(loc)
# }

# 
# extract_3D_parcel <- function(current_parcel, trajectories){
#   loc_1 = ind2sub(dim(trajectories)[1], current_parcel[1])
#   loc_2 = ind2sub(dim(trajectories)[1], current_parcel[length(current_parcel)])
#   parcel_sz = c((loc_2[1] - loc_1[1] + 1), (loc_2[2] - loc_1[2] + 1), dim(trajectories)[3])
#   parcel_3D = array(0, parcel_sz)
#   parcel_3D[, ,] = trajectories[loc_1[1]:loc_2[1], loc_1[2]:loc_2[2], ]
#   return(parcel_3D)
# }

# rowProds <- function(X){ t(t(apply(X,1,FUN="prod"))) }
# 
# test_cond <- function(vals_to_match, parcel_vals_pool, development_vals_used, match_array){
#   thresh_array = matrix(rep(0.10*vals_to_match, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
#   cond = (parcel_vals_pool - match_array) < thresh_array
#   cond = rowProds(cond)
#   return(cond)
# }


# sum through features for each site to yield single dimensional site state vector for each feature 
sum_trajectories <- function(traj_list, features_to_use_in_offset_calc){
  
  parcel_traj_list = lapply(seq_along(traj_list), function(i) lapply(seq_along(traj_list[[i]]), 
                                                                     function(j) sum_cols_multi(traj_list[[i]][[j]])))
  return(parcel_traj_list)
}


