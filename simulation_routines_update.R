# potential issues with parcel num remaining


# 
select_inds_to_clear <- function(index_object, run_params){
  
  parcel_inds = c(unlist(index_object$indexes_to_use$devs), unlist(index_object$indexes_to_use$offsets))
  clearing_thresh <- rep(run_params$illegal_clearing_prob, length(parcel_inds))
  discrim <- runif(length(clearing_thresh)) < clearing_thresh                               
  
  
  #sample over uniform random vector, indicies less than the threshold level are selected for illegal clearing
  inds_to_clear <- parcel_inds[discrim]
  return(inds_to_clear)
}


perform_illegal_clearing <- function(current_ecology, index_object, yr, region_ind, current_policy_params, 
                                     run_params, decline_rates_initial, time_horizon){
  
  if (run_params$illegal_clearing_prob == 0){ 
    # return null object for illegal clearing off
    return()
  }
  
  inds_to_clear <- select_inds_to_clear(index_object, run_params)
  
  if (length(inds_to_clear) == 0){ 
    #return null for no sites selected for illegal clearing
    return()
  } else {
    cat('\n illegally cleared sites' , inds_to_clear)
  }
  
  parcel_num_remaining = length(c(unlist(index_object$indexes_to_use$offsets[[region_ind]]), 
                                  unlist(index_object$indexes_to_use$devs[[region_ind]]))) #used for calculation of cfac
  
  illegally_cleared_object <- record_current_parcel_set(current_ecology[inds_to_clear], 
                                                        inds_to_clear, 
                                                        parcel_num_remaining, 
                                                        yr, 
                                                        region_ind) #record potential current development parcel attributes
  
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

# sum two lists with identical structure (unless one list is null)
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
  thresh = array(match_threshold, ndims)         
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





# # ##########     ERRORS IN THIS CODE   ########

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

##########     ERRORS IN THIS CODE   ########
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





##########     ERRORS IN THIS CODE   ########
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


