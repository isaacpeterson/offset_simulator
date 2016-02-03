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
  index_object$parcel_sets = list()
  index_object$dev_count = 0
  index_object$break_flag = FALSE
  return(index_object)
}


initialise_ecology_slice <- function(global_params, parcels){
  land_parcels = parcels$land_parcels
  land_parcel_num = parcels$land_parcel_num
  initial_ecology = matrix(1,global_params$ecology_size,global_params$ecology_size)
  
  for (s in seq_len(land_parcel_num)){
    initial_parcel_value = global_params$min_initial_eco_val + (global_params$max_initial_eco_val - global_params$min_initial_eco_val - global_params$initial_eco_noise)*runif(1) 
    inds = parcels$land_parcels[[s]]
    dim(inds) = c(1, length(inds))
    initial_ecology[inds] = initial_ecology[inds]*initial_parcel_value
  }
  initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size),global_params$ecology_size,global_params$ecology_size)
  return(initial_ecology)
}

# initialise_ecology_by_parcel <- function(global_params, parcels){
#   land_parcels = parcels$land_parcels
#   land_parcel_num = parcels$land_parcel_num
#   initial_ecology = vector('list', land_parcel_num)
#   
#   for (parcel_ind in seq_len(land_parcel_num)){
#     current_parcel = land_parcels[[parcel_ind]]
#     initial_parcel_ecology = array(runif(length(current_parcel)*eco_dims), c(dim(current_parcel), eco_dims))
#     rep(global_params$min_initial_eco_vals,length(current_parcel))
#     min_parcel_vals = global_params$min_initial_eco_val[eco_ind] + runif(land_parcel_num)*(global_params$max_initial_eco_val[eco_ind] - global_params$min_initial_eco_val[eco_ind] - global_params$initial_eco_noise[eco_ind])
#     
#     
#       min_parcel_vals = global_params$min_initial_eco_val[eco_ind] + runif(land_parcel_num)*(global_params$max_initial_eco_val[eco_ind] - global_params$min_initial_eco_val[eco_ind] - global_params$initial_eco_noise[eco_ind])
#       
#       
#       initial_parcel_ecology = min_parcel_vals[parcel_ind] + global_params$initial_eco_spread*runif(length(current_parcel))
#       dim(initial_parcel_ecology) = dim(current_parcel)
#       initial_ecology[[parcel_ind]][, , eco_ind] = initial_parcel_ecology
#   }
#  # initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size),global_params$ecology_size,global_params$ecology_size)
#   return(initial_ecology)
# }


initialise_ecology <- function(global_params, parcels){
  initial_ecology = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$eco_dims))
  for (eco_ind in seq_len(global_params$eco_dims)){
    initial_ecology[, , eco_ind]  = initialise_ecology_slice(global_params, parcels)
  }
  return(initial_ecology)
}
  

initialise_offset_object <- function(){
  object = list()
  object$current_parcels = list()
  object$current_parcel_sums = list()
  return(object)
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


eco_shift <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_step){
  
  t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
  eco_shift = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_step - t_sh)))
  return(eco_shift)
}


eco_projected <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_step){
  
  t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
  eco_shift = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(seq_len(time_step) - t_sh)))
  return(eco_shift)
  
}





build_counterfactuals_by_year <- function(global_params, decline_rates, land_parcels, initial_ecology){
  current_ecology = initial_ecology
  counterfactuals = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
  counterfactuals[, , 1] = initial_ecology
  parcel_num = length(land_parcels)
  
  for (yr in seq(global_params$time_steps)){
    for (parcel_ind in seq_len(parcel_num)){
      current_parcel = land_parcels[[parcel_ind]]
      current_dec_rate = decline_rates[parcel_ind]      
      updated_parcel = sapply(current_ecology[current_parcel], eco_shift, min_eco_val = 0, max_eco_val = 100, decline_rate = current_dec_rate, time_step = 1)
      current_ecology[current_parcel] = updated_parcel 
    }
    if (global_params$blur == TRUE){
      current_ecology = Blur_2D(current_ecology, 0.5, 0.5)
    } 
    counterfactuals[, , yr] = current_ecology
  }
  
  return(counterfactuals)
}




predict_parcel_traj <- function(current_ecology, global_params, decline_rate, time_step){
  
  if (time_step == 0){
    ecology_predicted = current_ecology
  } else if (time_step > 0){
    ecology_predicted = array(0, c(dim(current_ecology), (time_step + 1)))
    ecology_predicted[, , 1] = current_ecology
    ecology_projected = apply(current_ecology, MARGIN = c(1, 2), FUN = eco_projected, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, (decline_rate = decline_rate), (time_step = time_step))
    if (length(dim(ecology_projected)) > 2){
      ecology_projected = aperm(ecology_projected, c(2, 3, 1))
    }
    ecology_predicted[, , 2:(time_step + 1)] = ecology_projected
  }
  return(ecology_predicted)
}


# build_counterfactuals_by_parcel <- function(global_params, decline_rates, land_parcels, current_ecology){
#   
#   counterfactuals = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
#   parcel_num = length(land_parcels)
#   
#   for (parcel_ind in seq_len(parcel_num)){
#     
#     current_parcel = land_parcels[[parcel_ind]]
#     current_dec_rate = decline_rates[parcel_ind]  
#     current_parcel_ecology = initial_ecology[current_parcel]
#     dim(current_parcel_ecology) = dim(current_parcel)
#     projected_ecology = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = current_dec_rate, (time_step = global_params$time_steps - 1))
#     counterfactuals = insert_parcel_trajectory(counterfactuals, current_parcel, projected_ecology)
#     
#   }
#   
#   return(counterfactuals)
# }


# current_ecology = initial_ecology
# decline_rates = decline_rates_initial
# land_parcels = parcels$land_parcels
# time_steps = 10


build_counterfactuals_by_parcel_multi <- function(global_params, decline_rates, parcel_indexes, land_parcels, current_ecology, time_steps){
  
  eco_dims = dim(current_ecology)[3]
  counterfactuals = vector('list', eco_dims)
  
  parcel_num = length(parcel_indexes)
  
  for (eco_ind in seq_len(eco_dims)){
    current_ecology_slice = current_ecology[, , eco_ind]
    current_counterfactual = vector('list', parcel_num)
    
    for (parcel_ind in seq_len(parcel_num)){
      current_parcel_ind = parcel_indexes[parcel_ind]
      current_parcel = land_parcels[[current_parcel_ind]]
      loc = ind2sub(dim(decline_rates)[1], current_parcel_ind)
      current_dec_rate = decline_rates[loc[1], loc[2], eco_ind]  
      current_parcel_ecology = current_ecology_slice[current_parcel]
      dim(current_parcel_ecology) = dim(current_parcel)
      current_counterfactual[[parcel_ind]] = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = current_dec_rate, (time_step = (time_steps - 1)))
    }
    counterfactuals[[eco_ind]] = current_counterfactual
  }
  
  return(counterfactuals)
  
}





initialise_counterfactuals_multi <- function(global_params, region_params, land_parcels, initial_ecology, decline_rates){
  counterfactuals = vector('list', global_params$eco_dims)
  time_steps = global_params$time_steps 
  ecology_size = global_params$ecology_size
  for (eco_ind in seq_len(global_params$eco_dims)){
    counterfactuals[[eco_ind]] = build_counterfactuals_by_year(global_params, decline_rates[, , eco_ind] , land_parcels, initial_ecology[, , eco_ind] )
  }
  return(counterfactuals)
}

initialise_trajectories <- function(eco_dims, ecology_size, time_steps, initial_ecology){
  trajectories = vector('list', eco_dims)
  for (eco_ind in seq_len(eco_dims)){
    trajectories[[eco_ind]] = array(0, c(ecology_size, ecology_size, time_steps))
  }
  
  return(trajectories)
}

build_decline_rates <- function(parcels, region_params){
  regions = parcels$regions
  region_num = length(regions)
#  region_dec_rates = vector('list', region_num)
  decline_rates = array(0, dim(parcels$parcel_indexes))
  
  for (region_ind in seq_len(region_num)){ 
    current_region = regions[[region_ind]]
    current_parcel_num = length(current_region)    
    decline_params = c(length(current_region), region_params[[region_ind]]$mean_decline_rate, region_params[[region_ind]]$decline_rate_std) #params$decline_rate_std[region_ind])
    current_decline_rates = matrix(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), ncol = ncol(current_region))
#    dec_rates[[region_ind]] = current_decline_rates
    decline_rates[current_region] = current_decline_rates
  }
  
  return(decline_rates)
}


build_decline_rates_multi <- function(parcels, region_params, eco_dims){
  regions = parcels$regions
  region_num = length(regions)
#  region_dec_rates = vector('list', region_num)
  decline_rates = array(0, c(dim(parcels$parcel_indexes), eco_dims))
  
  for (eco_ind in seq_len(eco_dims)){
    current_decline_rates = array(0, dim(parcels$parcel_indexes))
    for (region_ind in seq_len(region_num)){ 
      current_region = regions[[region_ind]]
      current_parcel_num = length(current_region)    
      decline_params = c(length(current_region), region_params[[region_ind]]$mean_decline_rate, region_params[[region_ind]]$decline_rate_std) #params$decline_rate_std[region_ind])
      region_decline_rates = matrix(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), ncol = ncol(current_region))
      #    region_dec_rates[[region_ind]] = current_decline_rates
      current_decline_rates[current_region] = region_decline_rates
    }
    decline_rates[, , eco_ind]  = current_decline_rates
  }
  
  return(decline_rates)
}

initialise_outputs <- function(counterfactuals, global_params){
  outputs = list()
  outputs$trajectories = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))

  return(outputs)
  
}


select_development_index <- function(ind_available, parcel_num){
  parcel_inds = ind_available[sample(1:length(ind_available), parcel_num)]
  return(parcel_inds)
}





update_development_object <- function(region_params, global_params, region_ind, ind_available, current_ecology, decline_rates, land_parcels, apply_offset_to, yr){
  
  parcel_inds = select_development_index(ind_available, 1)
  
  dev_calc_type = region_params[[region_ind]]$dev_calc_type
  restoration_rate = region_params[[region_ind]]$restoration_rate
  
  parcel_sums_object = find_current_parcel_sums(land_parcels, current_ecology, parcel_inds)
  
  if (apply_offset_to == 'singular'){
    eco_dims = 1
  } else if (apply_offset_to == 'all'){
    eco_dims = global_params$eco_dims
  }
  current_parcel_val = parcel_sums_object$parcel_sums
  
  if (dev_calc_type == 'current condition'){
    parcel_vals_used = current_parcel_val[1:eco_dims]
  } else if (dev_calc_type == 'future condition'){
    parcel_vals_used = predict_parcel_vals_multi(predict_type = 'protect', parcel_sums_object$parcel_eco_vals, parcel_inds, decline_rates, restoration_rate, 
                                                 global_params$min_eco_val, global_params$max_eco_val, eco_dims, time_step = (global_params$time_steps - yr))
  }
  
  development_object = record_parcel_info_multi(parcel_sums_object$parcel_sums, parcel_inds, parcel_sums_object$parcel_eco_vals, parcel_vals_used, yr)
  
  return(development_object)
  
}



predict_parcel_vals_multi <- function(predict_type, parcel_eco_vals, parcel_inds, decline_rates, restoration_rate, min_eco_val, max_eco_val, eco_dims, time_step){
  
  parcel_num = length(parcel_inds)
 
  predicted_parcel_vals = array(0, c(parcel_num, eco_dims))
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_inds[parcel_count_ind]
    
    for (eco_ind in seq_len(eco_dims)){
      current_decline_rates = decline_rates[, , eco_ind] 
      
      if (predict_type == 'protect'){
        decline_rate = current_decline_rates[current_parcel_ind]      
      } else if (predict_type == 'restore'){
        decline_rate = restoration_rate
      }
      predicted_parcel = sapply(parcel_eco_vals[[parcel_count_ind]][, , eco_ind], eco_shift, min_eco_val, max_eco_val, decline_rate, time_step)
      predicted_parcel_vals[parcel_count_ind, eco_ind]  = sum(predicted_parcel)
    }
    
  }
  return(predicted_parcel_vals)
  
}


record_parcel_info_multi <- function(parcel_sums, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr){
  out_object = list()
  out_object$yr = yr
  out_object$parcel_ecologies = parcel_eco_vals
  out_object$parcel_sums = parcel_sums
  out_object$parcel_indexes = parcel_indexes
  out_object$parcel_vals_used = parcel_vals_used
  
  return(out_object)
  
}



# find_projected_offsets <- function(global_params, decline_rates, land_parcels, initial_ecology){
#   current_ecology = initial_ecology
#   counterfactuals = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
#   counterfactuals[, , 1] = initial_ecology
#   parcel_num = length(land_parcels)
#   
#   for (yr in seq(global_params$time_steps)){
#     for (parcel_ind in seq_len(parcel_num)){
#       current_parcel = land_parcels[[parcel_ind]]
#       current_dec_rate = decline_rates[parcel_ind]      
#       updated_parcel = sapply(current_ecology[current_parcel], eco_shift, min_eco_val = 0, max_eco_val = 100, decline_rate = current_dec_rate, time_step = 1)
#       current_ecology[current_parcel] = updated_parcel 
#     }
#     if (global_params$blur == TRUE){
#       current_ecology = Blur_2D(current_ecology, 0.5, 0.5)
#     } 
#     counterfactuals[, , yr] = current_ecology
#   }
#   
#   return(counterfactuals)
# }
# 


    



find_offset_pool <- function(index_object, region_ind, decline_rates, global_params, region_params, land_parcels, current_ecology, time_horizon, yr){
  
  offset_pool_object = list()
  offset_calc_type = region_params[[region_ind]]$offset_calc_type
  current_offset_pool = index_object$ind_available[[region_ind]]
  parcel_sums_object = find_current_parcel_sums(land_parcels, current_ecology, current_offset_pool)
  
  if (global_params$apply_offset_to == 'singular'){
    eco_dims = 1
  } else {
    eco_dims = global_params$eco_dims
  }
  
  current_parcel_sums = parcel_sums_object$parcel_sums[, 1:eco_dims]
  #current_parcel_sums = t(t(current_parcel_sums))
  if ( (offset_calc_type == 'restoration gains') || (offset_calc_type == 'restoration from counterfactual') || (offset_calc_type == 'absolute restoration')){
      restoration_vals = predict_parcel_vals_multi(predict_type = 'restore', parcel_sums_object$parcel_eco_vals, parcel_inds = current_offset_pool, decline_rates, region_params[[region_ind]]$restoration_rate, 
                                                          global_params$min_eco_val, global_params$max_eco_val, eco_dims, time_horizon)
  }
  
  if( (offset_calc_type == 'avoided degredation') || (offset_calc_type == 'restoration from counterfactual') || (offset_calc_type == 'absolute counterfactual') ){
    
    if (global_params$adjust_counterfactual == TRUE){
      
      parcel_indexes = current_offset_pool
      cfacs = build_counterfactuals_by_parcel_multi(global_params, decline_rates_initial, parcel_indexes, land_parcels, current_ecology, (time_steps = time_horizon))
      adjusted_counters = adjust_counterfactual_by_parcel(cfacs, (include_offsets = global_params$include_offsets_in_adjusted_counterfactual), region_params, global_params, parcels, parcel_indexes, time_horizon, yr)
      adjusted_counter_trajs = find_parcel_traj_by_list(adjusted_counters[[1]])
      counterfactual_vals = adjusted_counter_trajs[time_horizon, ]
      counterfactual_vals = t(t(counterfactual_vals))
    } else {
    counterfactual_vals = predict_parcel_vals_multi(predict_type = 'protect', parcel_sums_object$parcel_eco_vals, parcel_inds = current_offset_pool, decline_rates, region_params[[region_ind]]$restoration_rate, 
                                                         global_params$min_eco_val, global_params$max_eco_val, eco_dims, time_horizon)
    }
  }
  
  
  
  if (offset_calc_type == 'current condition'){
    parcel_vals_pool = current_parcel_sums
  } else if (offset_calc_type == 'restoration gains'){
    parcel_vals_pool = restoration_vals - current_parcel_sums
  } else if (offset_calc_type == 'restoration from counterfactual'){
    parcel_vals_pool = restoration_vals - counterfactual_vals
  } else if (offset_calc_type == 'absolute restoration'){
    parcel_vals_pool = restoration_vals
  } else if (offset_calc_type == 'avoided degredation'){
    parcel_vals_pool = current_parcel_sums - counterfactual_vals
  } else if (offset_calc_type == 'absolute counterfactual'){
    parcel_vals_pool = counterfactual_vals 
  } 
  
  offset_pool_object$current_parcel_pool = current_offset_pool
  offset_pool_object$current_parcel_sums = parcel_sums_object$parcel_sums
  offset_pool_object$parcel_vals_pool = parcel_vals_pool
  offset_pool_object$parcel_eco_vals = parcel_sums_object$parcel_eco_vals
  return(offset_pool_object)
  
}                 


select_offset_index <- function(offset_pool_object, offset_multiplier, development_vals_used){
  outs = list()
  parcel_vals_pool = offset_pool_object$parcel_vals_pool
  current_offset_pool = offset_pool_object$current_parcel_pool
  dev_vals = offset_multiplier*matrix(rep(development_vals_used, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
  err = sqrt(rowSums( (parcel_vals_pool - dev_vals)^2 ) )
  best_ind = which(err == min(err))
  parcel_indexes = current_offset_pool[best_ind]
  parcel_vals_used = parcel_vals_pool[best_ind, ]
  
  
  parcel_num = length(parcel_indexes)
  outs$parcel_eco_vals = vector('list', parcel_num)
  outs$parcel_sums = vector('list', parcel_num)
  outs$parcel_indexes = array(0, parcel_num)
  outs$parcel_vals_used = vector('list', parcel_num)
  
  for (parcel_ind in seq_len(parcel_num)){
    outs$parcel_eco_vals[[parcel_ind]] = offset_pool_object$parcel_eco_vals[[best_ind]]
    outs$parcel_sums[[parcel_ind]] = offset_pool_object$current_parcel_sums[best_ind, ]
    outs$parcel_vals_used[parcel_ind, ] = parcel_vals_used
    outs$parcel_indexes[parcel_ind] = parcel_indexes
  }
 
  return(outs)
}


rowProds <- function(X){ t(t(apply(X,1,FUN="prod"))) }

test_cond <- function(vals_to_match, parcel_vals_pool, development_vals_used, match_array){
  thresh_array = matrix(rep(0.10*vals_to_match, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
  cond = (parcel_vals_pool - match_array) < thresh_array
  cond = rowProds(cond)
  return(cond)
}




adjust_counterfactual_by_parcel <-function(cfacs, include_offsets, region_params, global_params, parcels, parcel_indexes, time_horizon, yr){
  
  parcel_num = length(parcel_indexes)
  eco_dims = global_params$eco_dims
  summed_offset_projections = vector('list', eco_dims)
  adjusted_counters = vector('list', eco_dims)
  
  for (eco_ind in 1:eco_dims){
    summed_offset_projections[[eco_ind]] = vector('list', parcel_num)
    adjusted_counters[[eco_ind]] = vector('list', parcel_num)
  }
  
  dev_vec = region_params[[1]]$dev_vec
  dev_vec = dev_vec[yr:length(dev_vec)]
  dev_inds = which(dev_vec > 0)
  
  if (length(dev_vec) < time_horizon){
    dev_vec = c(dev_vec, array(0, (time_horizon - length(dev_vec))))
  }
  
  dev_vec = dev_vec[1:time_horizon]
  
  dev_prob = dev_vec/parcel_num
  
  if (include_offsets == TRUE){
    offset_vec = dev_vec
    offset_prob = offset_vec/parcel_num
  } else {offset_prob = 0}
  
  counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
  eco_ind = 1
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    if (include_offsets == TRUE){
      projected_dims = dim(cfacs[[eco_ind]][[parcel_count_ind]])
      current_summed_offset_projections = array(0, projected_dims)
      
      for (proj_yr in which(dev_vec > 0)){
        
        current_parcel_ecology = cfacs[[eco_ind]][[parcel_count_ind]][, , proj_yr]
        current_offset_prob = offset_prob[proj_yr]
        current_offset_projection = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = region_params[[1]]$restoration_rate, time_step = (time_horizon - proj_yr))
        current_summed_offset_projections[, , proj_yr:time_horizon] = current_summed_offset_projections[, , proj_yr:time_horizon] + current_offset_prob*current_offset_projection
        
      } 
      
    } else {
      current_summed_offset_projections = 0
    }
    
    summed_offset_projections[[eco_ind]][[parcel_count_ind]] = current_summed_offset_projections
    
    c_probs = rep(counter_probs, projected_dims[1]*projected_dims[2])
    dim(c_probs) = c(length(counter_probs), c(projected_dims[2], projected_dims[1]))
    c_probs = aperm(c_probs, c(3, 2, 1))
    adjusted_counters[[eco_ind]][[parcel_count_ind]] = c_probs*cfacs[[eco_ind]][[parcel_count_ind]] + current_summed_offset_projections
    
  }
  
  return(adjusted_counters)
  
}




# 
# decline_rates = decline_rates_initial
# initial_ecology = initial_ecology[, , 1]
# counter_parcel_trajs = find_parcel_trajectories(parcels$land_parcels, 1:(parcels$land_parcel_num), global_params$time_steps, cfacs)


# 
# 
# 
# adjust_counterfactual <-function(cfacs, perform_offsets, region_params, global_params, decline_rates, parcels){
#   
#   dev_vec = region_params[[1]]$dev_nums
#   dev_vec = c(0, dev_vec)
#   dev_vec = dev_vec[1:(length(dev_vec) - 1)]
#   
#   dev_prob = dev_vec/parcel_num
#   offset_vec = dev_vec
#   offset_prob = offset_vec/parcel_num
#   
#   rest_rate = region_params[[1]]$restoration_rate
#   time_steps = global_params$time_steps
#   parcel_num = parcels$land_parcel_num
#   
#   current_parcel = parcels$land_parcels[[1]]
#   parcel_trajs = array(0, c(time_steps, time_steps))
#   
#   if (perform_offsets == TRUE){
#     off_prob = offset_vec/parcel_num
#     
#     summed_offset_projections = array(0, dim(cfacs))
#     
#     for (yr in seq_len(time_steps)){
#       if (dev_vec[yr] > 0){
#         current_ecology = cfacs[, , yr]
#         current_off_prob = offset_prob[yr]
#         current_offset_projection = array(0, dim(cfacs))
#         current_offset_projection[, , yr:time_steps] = predict_parcel_traj(current_ecology, global_params, decline_rate = rest_rate, time_step = (time_steps - yr))
#         summed_offset_projections = summed_offset_projections + current_off_prob*current_offset_projection
#         
#         parcel_trajs[, yr] = find_parcel_trajectory(parcels$land_parcels, 1, time_steps, current_offset_projection)
#         
#       }
#     }
#     
#   } else {
#     summed_offset_projections = 0
#   }
#   
#   counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
#   
#   c_probs = rep(counter_probs, global_params$ecology_size*global_params$ecology_size)
#   dim(c_probs) = c(length(counter_probs), c(global_params$ecology_size, global_params$ecology_size))
#   c_probs = aperm(c_probs, c(3, 2, 1))
#   adjusted_counters = c_probs*cfacs + summed_offset_projections
#   return(adjusted_counters)
#   
# }
# 



select_offset_index_multi <- function(offset_pool_object, offset_multiplier, development_vals_used,  offset_parcel_for_parcel, eco_dims){
  
  outs = list()
  match_indexes = vector()
  parcel_vals_pool = offset_pool_object$parcel_vals_pool
  current_offset_pool = offset_pool_object$current_parcel_pool
  vals_to_match = offset_multiplier*development_vals_used
  
  match_ind_available = 1:length(current_offset_pool)
  match_array = matrix(rep(vals_to_match, length(match_ind_available)), ncol = length(development_vals_used), byrow = TRUE)
  #cond = test_cond(vals_to_match, t(t(parcel_vals_pool[match_ind_available, ])), development_vals_used, match_array)
  
  cond = all(vals_to_match > 0)
    
  while(any(cond > 0))
  {
    err = sqrt(rowSums( (parcel_vals_pool[match_ind_available, ] - match_array)^2 ) )
    match_ind = which(err == min(err))
    index_used = match_ind_available[match_ind]
    match_indexes = c(match_indexes, index_used)
    parcel_vals_used = parcel_vals_pool[index_used, ]
    
    if ( offset_parcel_for_parcel == TRUE)
      {
      break
    }
    match_ind_available = match_ind_available[-match_ind]
    vals_to_match = vals_to_match - parcel_vals_used
    match_array = matrix(rep(vals_to_match, length(match_ind_available)), ncol = length(development_vals_used), byrow = TRUE)
    
    cond = all(vals_to_match > 0)
    
  }
  
  parcel_num = length(match_indexes)
  outs$parcel_indexes = current_offset_pool[match_indexes]
  outs$parcel_eco_vals = vector('list', parcel_num)
  
  outs$parcel_sums = array(0, c(parcel_num, eco_dims))
  outs$parcel_vals_used = array(0, c(parcel_num, length(vals_to_match)))
  
  for (parcel_count in seq_len(parcel_num)){
    match_index = match_indexes[parcel_count]
    outs$parcel_eco_vals[[parcel_count]] = offset_pool_object$parcel_eco_vals[[match_index]]
    outs$parcel_sums[parcel_count, ] = offset_pool_object$current_parcel_sums[match_index, ]
    outs$parcel_vals_used[parcel_count, ] = parcel_vals_pool[match_index, ]
  }
 
  return(outs)
}


#offset_multiplier = 1
#offset_parcel_for_parcel = TRUE
#development_vals_used = development_object$parcel_vals_used

update_offset_object <- function(offset_pool_object, offset_multiplier, land_parcels, development_vals_used, yr,  offset_parcel_for_parcel, eco_dims){      
  selected_offset_object = select_offset_index_multi(offset_pool_object, offset_multiplier, development_vals_used,  offset_parcel_for_parcel, eco_dims)
  
  offset_object = record_parcel_info_multi(selected_offset_object$parcel_sums, selected_offset_object$parcel_indexes, selected_offset_object$parcel_eco_vals,  selected_offset_object$parcel_vals_used, yr)
  return(offset_object)
}


write_null_offset_object <- function(){
  offset_object = list()
  offset_object$parcel_indexes = list()
  offset_object$current_parcel_sums = list()
  offset_object$parcel_vals_used = list()
  return(offset_object)
}


write_development <- function(development_object, current_ecology){
  
  current_parcel = development_object$current_parcels
  current_ecology[current_parcel] = 0
  
  return(current_ecology)
  
}

write_offset <- function(offset_object, current_ecology, min_eco_val, max_eco_val, ecology_size, restoration_rate, yr){
  current_parcels = offset_object$current_parcels
  parcel_num = length(offset_object$current_parcels)
  for (parcel_ind in seq_len(parcel_num)){
    current_parcel = current_parcels[[parcel_ind]]
    updated_parcel = sapply(current_ecology[current_parcel], eco_shift, min_eco_val = 0, max_eco_val = 100, decline_rate = restoration_rate, time_step = 1)
    current_ecology[current_parcel] = updated_parcel 
  }
  return(trajectories)
}


find_current_parcel_sums <- function(land_parcels, current_ecology, parcel_inds){
  
  out_object = list()
  eco_dims = dim(current_ecology)[3]
  parcel_eco_vals = vector('list', length(parcel_inds))
  parcel_sums = array(0, c(length(parcel_inds), eco_dims))
  
  for (parcel_count_ind in seq_len(length(parcel_inds))){
    current_parcel = land_parcels[[parcel_inds[parcel_count_ind]]]
    current_parcel_eco_vals = extract_3D_parcel(current_parcel, current_ecology)
    
    if (eco_dims == 1){
      parcel_sums[parcel_count_ind] = sum(current_parcel_eco_vals)
    } else if (eco_dims > 1){
      parcel_sums[parcel_count_ind, ] = apply(current_parcel_eco_vals, 3, sum)
    }
    parcel_eco_vals[[parcel_count_ind]] = current_parcel_eco_vals
  }
  
  out_object$parcel_eco_vals = parcel_eco_vals
  out_object$parcel_sums = parcel_sums
  return(out_object)
  
}




find_parcel_traj_by_list <- function(trajectories_list){
  
  parcel_num = length(trajectories_list)
  time_steps = dim(trajectories_list[[1]])[3]
  parcel_trajs = array(0, c(time_steps, parcel_num))
  eco_ind = 1
  for (parcel_ind in seq_len(parcel_num)){
    parcel_trajs[, parcel_ind] =  apply(trajectories_list[[parcel_ind]], MARGIN=3, sum)
  }
  
  return(parcel_trajs)
}



find_parcel_traj_from_trajectories <- function(land_parcels, parcel_ind, time_steps, trajectories){

  current_parcel = land_parcels[[parcel_ind]]
  parcel_traj = array(0, time_steps)
  
  for (yr in seq_len(time_steps)){ #determine net regional offsets, net regional development_losses
    current_slice = trajectories[ , , yr]
    parcel_traj[yr] = sum(current_slice[current_parcel])
  }
  return(parcel_traj)
}


# land_parcels = parcels$land_parcels
# parcel_indexes = 1:parcels$land_parcel_num
# time_steps = global_params$time_steps
# trajectories = outputs$trajectories[[1]]

find_parcel_trajectories <- function(land_parcels, parcel_indexes, time_steps, trajectories){

  parcel_trajs = array(0, c(time_steps, length(parcel_indexes)))
  
  for (parcel_ind in seq_len(length(parcel_indexes))){
    current_ind = parcel_indexes[parcel_ind]
    parcel_trajs[, parcel_ind] = find_parcel_traj_from_trajectories(land_parcels, current_ind, time_steps, trajectories)
  }
  
  if (length(parcel_indexes) == 1){
    dim(parcel_trajs) = c(length(parcel_trajs), 1)
  }
  
  return(parcel_trajs)
}




write_parcel_sets <- function(parcel_set_object, yr){
  parcel_set = list()
  parcel_set$yr = yr
  parcel_set$parcel_set_object = parcel_set_object
  return(parcel_set)
}


update_ind_available <- function(update_type, index_object, parcel_indexes, region_ind){
  
  ind_available = index_object$ind_available[[region_ind]]
  ind_available = setdiff(ind_available, parcel_indexes) #remove development parcel from available list   
  if (length(ind_available) < 0 ){   
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


update_decline_rates <- function(decline_rates, decline_rate_type, parcel_indexes, region_params, apply_offset_to, region_ind){
  offset_action_type = region_params[[region_ind]]$offset_action_type 
  restoration_rate = region_params[[region_ind]]$restoration_rate
  
  
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
      
      if (apply_offset_to == 'all'){
        decline_rates[loc[1], loc[2], ] = offset_rate
      } else if (apply_offset_to == 'singular'){
        decline_rates[loc[1], loc[2], 1] = offset_rate[1]
      }
    }
  }
  
  return(decline_rates)
  
}


find_current_dev_nums <- function(region_params, region_num, yr){

  dev_nums = array(0, region_num)
  for (region_ind in seq_len(region_num)){
    current_dev_num = region_params[[region_ind]]$dev_vec[yr]
    dev_nums[region_ind] = current_dev_num
  }
  return(dev_nums)
  
}

                                     
find_developments_offsets <- function(region_ind, global_params, region_params, current_ecology, decline_rates, parcels, index_object, perform_offsets, yr){
  
  if (global_params$use_offset_time_horizon == TRUE){
    time_horizon = global_params$offset_time_horizon
  } else {
    time_horizon = global_params$time_steps - yr
  }
  
  offset_devs_object = list()
  time_remaining = 0:(global_params$time_steps - yr)
  
  development_object <- update_development_object(region_params, global_params, region_ind, index_object$ind_available[[region_ind]], current_ecology, decline_rates, parcels$land_parcels, global_params$apply_offset_to, yr) 
  index_object <- update_ind_available(update_type = 'development', index_object, development_object$parcel_indexes, region_ind)
  decline_rates <- update_decline_rates(decline_rates, decline_rate_type = 'development', development_object$parcel_indexes, region_params, global_params$apply_offset_to, region_ind)
  
  if (perform_offsets == TRUE){
                                          
    offset_pool_object <- find_offset_pool(index_object, region_ind, decline_rates, global_params, region_params, parcels$land_parcels, current_ecology, time_horizon, yr)
    offset_object <- update_offset_object(offset_pool_object, region_params[[region_ind]]$offset_multiplier, parcels$land_parcels, development_object$parcel_vals_used, yr, region_params[[region_ind]]$offset_parcel_for_parcel, global_params$eco_dims)                  
    index_object <- update_ind_available(update_type = 'offset', index_object, offset_object$parcel_indexes, region_ind)              
    decline_rates <- update_decline_rates(decline_rates, decline_rate_type = 'offset', offset_object$parcel_indexes, region_params, global_params$apply_offset_to, region_ind)
    
  } else {offset_object <- write_null_offset_object() }
  
  index_object$dev_count = index_object$dev_count + 1
  
  offset_devs_object$index_object = index_object
  offset_devs_object$decline_rates = decline_rates
  offset_devs_object$offset_object = offset_object
  offset_devs_object$development_object = development_object
  
  return(offset_devs_object)
} 

# 
# yr = 1
# current_ecology = initial_ecology
# decline_rates = decline_rates_initial
# perform_offsets = TRUE
# record_parcel_sets = TRUE
# region_ind = 1
# current_dev_nums <- find_current_dev_nums(region_params, global_params$region_num, yr)
# current_develop_num = current_dev_nums[region_ind]
# 
# land_parcels = parcels$land_parcels
# time_horizon = 20
# 
# trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)
# offsets_object = list()
# developments_object = list()


calc_trajectories_multi <- function(global_params, region_params, current_ecology, decline_rates, parcels, index_object, perform_offsets, record_parcel_sets){
  trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)
  offsets_object = list()
  developments_object = list()
  for (yr in seq_len(global_params$time_steps)){      #main time loop    
   
    current_dev_nums <- find_current_dev_nums(region_params, global_params$region_num, yr)
    for (region_ind in seq_len(parcels$region_num)){
      current_develop_num = current_dev_nums[region_ind]
      
      if (current_develop_num > 0){
        
        for (dev_index in seq_len(current_develop_num)){
          offset_devs_object <- find_developments_offsets(region_ind, global_params, region_params, current_ecology, decline_rates, parcels, index_object, perform_offsets, yr)
          decline_rates <- offset_devs_object$decline_rates
          index_object <- offset_devs_object$index_object
          offsets_object[[index_object$dev_count]] = offset_devs_object$offset_object
          developments_object[[index_object$dev_count]] = offset_devs_object$development_object
          print(yr)
        }
      }
    }
    for (eco_ind in seq_len(global_params$eco_dims)){
      trajectories[[eco_ind]][, , yr] = current_ecology[, , eco_ind] 
    }
    current_ecology <- project_current_system(current_ecology, parcels$land_parcels, decline_rates, global_params$min_eco_val, global_params$max_eco_val, time_step = 1)
  }
  
  if (record_parcel_sets == TRUE){
    outputs = list()
    outputs$offset_list = index_object$offsets
    outputs$development_list = index_object$developments
    outputs$offsets = offsets_object
    outputs$developments = developments_object
    outputs$trajectories = trajectories
    return(outputs)
  } else{
    return(trajectories)
  }
  
}




project_current_system <- function(current_ecology, land_parcels, decline_rates, min_eco_val, max_eco_val, time_step){
  parcel_num = length(land_parcels)
  eco_dims = dim(current_ecology)[3]
  for (eco_ind in seq_len(eco_dims)){
    current_ecology_slice = current_ecology[, , eco_ind] 
    current_decline_rates = decline_rates[,  , eco_ind] 
    for (parcel_ind in seq_len(parcel_num)){  
      current_parcel = land_parcels[[parcel_ind]]
      decline_rate = current_decline_rates[parcel_ind]
      if (decline_rate == 0){
        updated_parcel = 0
      } else if (decline_rate == 1){
        updated_parcel = current_ecology[current_parcel]
      } else (updated_parcel = sapply(current_ecology[current_parcel], eco_projected, min_eco_val, max_eco_val, decline_rate, time_step))
      current_ecology_slice[current_parcel] = updated_parcel 
    }
    current_ecology[, , eco_ind]  = current_ecology_slice
  }
  return(current_ecology) 
}
  
# update_ecology_by_parcel <- function(current_ecology, land_parcels, decline_rates, min_eco_val, max_eco_val, time_step){
#   parcel_num = length(land_parcels)
#   for (parcel_ind in seq_len(parcel_num)){  
#     current_parcel = land_parcels[[parcel_ind]]
#     decline_rate = decline_rates[parcel_ind]
#     if (decline_rate == 0){
#       updated_parcel = 0
#     } else {      
#       updated_parcel = sapply(current_ecology[current_parcel], eco_shift, min_eco_val, max_eco_val, decline_rate, time_step)
#     }
#     current_ecology[current_parcel] = updated_parcel 
#   }
#   return(current_ecology) 
# }





plot_outs <- function(...){
  
  dots = list(...)
  plot_params = dots[[length(dots)]]
  graphics.off()
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




# current_sets_object = outputs$offsets
# land_parcels = parcels$land_parcels
# trajectories = outputs$trajectories[[1]]
# time_steps = global_params$time_steps
# parcel_set_ind = 2
# current_parcel_set = current_sets_object[[parcel_set_ind]]


find_parcel_sets <- function(outputs, land_parcels, time_steps, decline_rates_initial){
  
  parcel_sets_object = list()
  parcel_sets_object$offsets =  parcel_set_offsets_devs(outputs$offsets, parcels$land_parcels, outputs$trajectories, global_params$time_steps, decline_rates_initial)
  parcel_sets_object$developments = parcel_set_offsets_devs(outputs$developments, parcels$land_parcels, outputs$trajectories, global_params$time_steps, decline_rates_initial)
  
  return(parcel_sets_object)
}

parcel_set_offsets_devs <- function(current_sets_object, land_parcels, trajectories, time_steps, decline_rates_initial){
  
  parcel_set_num = length(current_sets_object)
  parcel_set_object = vector('list', parcel_set_num)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    current_parcel_set = current_sets_object[[parcel_set_ind]]
    parcel_set_object[[parcel_set_ind]] = find_parcel_set_element(global_params, region_params, current_parcel_set, parcel_set_ind, land_parcels, trajectories, time_steps, decline_rates_initial)
  }
  
  return(parcel_set_object)
  
}  



find_parcel_set_element <- function(global_params, region_params, current_parcel_set, parcel_set_index, land_parcels, trajectories, time_steps, decline_rates_initial){
  
  yr = current_parcel_set$yr
  parcel_set_element = list()
  parcel_inds = current_parcel_set$parcel_indexes
  initial_parcel_ecologies = current_parcel_set$parcel_ecologies
  
  parcel_num = length(parcel_inds)
  avoided_degredation = array(0, c(time_steps, global_params$eco_dims, parcel_num))
  restoration_gains = array(0, c(time_steps, global_params$eco_dims, parcel_num))
  
  parcel_counter_sum = array(0, c(time_steps, global_params$eco_dims, parcel_num))
  parcel_trajectory_sum = array(0, c(time_steps, global_params$eco_dims, parcel_num))
  initial_sum = array(0, c(parcel_num, global_params$eco_dims))
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    current_parcel_ind = parcel_inds[parcel_count_ind]
    current_parcel = land_parcels[[current_parcel_ind]]
    
    for (eco_ind in seq_len(global_params$eco_dims)){
      
      current_parcel_trajectory = extract_3D_parcel(current_parcel, trajectories[[eco_ind]])
      
      parcel_ecology = initial_parcel_ecologies[[parcel_count_ind]][, , eco_ind] 
      
      loc_1 = ind2sub(dim(decline_rates_initial)[1], current_parcel_ind)
      current_dec_rate = decline_rates_initial[loc_1[1], loc_1[2], eco_ind] 
      parcel_counterfactual = find_parcel_counterfactual(current_parcel_trajectory, parcel_ecology, current_dec_rate, global_params, yr)
      
      restoration_gains[, eco_ind, parcel_count_ind] = calc_rel_intial(trajectory_type = 'trajectory', current_parcel, time_steps, current_parcel_trajectory, yr, parcel_ecology)
      avoided_degredation[, eco_ind, parcel_count_ind] = calc_rel_intial(trajectory_type = 'counterfactual', current_parcel, time_steps, parcel_counterfactual, yr, parcel_ecology)
      
      parcel_trajectory_sum[, eco_ind, parcel_count_ind] = apply(current_parcel_trajectory, 3, sum)
      parcel_counter_sum[, eco_ind, parcel_count_ind] = apply(parcel_counterfactual, 3, sum)
      initial_sum[parcel_count_ind, eco_ind]  = sum(parcel_ecology)
      
    }

  }
  
  parcel_set_element$restoration_gains = restoration_gains
  parcel_set_element$avoided_degredation = avoided_degredation
  parcel_set_element$parcel_trajectory_sum = parcel_trajectory_sum
  parcel_set_element$initial_sum = initial_sum
  parcel_set_element$parcel_counter_sum = parcel_counter_sum
  parcel_set_element$parcel_indexes = parcel_inds
  parcel_set_element$parcel_vals_used = current_parcel_set$parcel_vals_used
  return(parcel_set_element)
  
}



calc_rel_intial <- function(trajectory_type, current_parcel, time_steps, parcel_trajectory, yr, parcel_ecology){
  rel_arr = array(0, c(dim(current_parcel), time_steps))
  rel_arr[, , yr:time_steps] = parcel_trajectory[, , yr:time_steps] - as.vector(parcel_ecology)
  if (trajectory_type == 'counterfactual'){
    rel_arr = -rel_arr
  }
  rel_arr = apply(rel_arr, 3, sum)
}

find_parcel_counterfactual <- function(parcel_trajectory, parcel_ecology, current_dec_rate, global_params, yr){
  parcel_counterfactual = array(0, dim(parcel_trajectory))
  if (yr == 1){
    parcel_counterfactual[, , 1] = parcel_ecology
  } else {
    parcel_counterfactual[, , 1:(yr-1)] = parcel_trajectory[, , 1:(yr - 1)]
  }
  parcel_counterfactual[, , yr:global_params$time_steps] = predict_parcel_trajectory(parcel_ecology, current_dec_rate, global_params$min_eco_val, global_params$max_eco_val, global_params$time_steps - (yr - 1)) 
  return(parcel_counterfactual)                                                 
}


predict_parcel_trajectory <- function(parcel_ecology, current_dec_rate, min_eco_val, max_eco_val, time_steps){
  predicted_parcel_trajectory = array(0, c(dim(parcel_ecology), time_steps))
  predicted_parcel_trajectory[, , 1] = parcel_ecology
  for (current_step in seq_len(time_steps)){
    predicted_parcel_trajectory[, , current_step] = sapply(parcel_ecology, eco_shift, min_eco_val, max_eco_val, current_dec_rate, current_step)
  }
  return(predicted_parcel_trajectory)
  
}






plot_parcel_set_parcels <- function(current_set_object){
  
  graphics.off()
  parcel_num = length(current_set_object$parcel_indexes)
  sub_plots = 1:(parcel_num*global_params$eco_dims)
  dim(sub_plots) = c(global_params$eco_dims, parcel_num)
  layout(sub_plots)
  
  rest_gains = current_set_object$restoration_gains
  degs = current_set_object$avoided_degredation
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


plot_single_net_regional <- function(collated_parcel_sets_object, assessed_set_index, global_params, cfac_parcel_trajs, parcel_trajs){
  graphics.off()
  par(mfrow = c(3, 3))
  par(cex = 0.6)
  par(mar = c(2, 1, 1, 0), oma = c(4, 4, 0.5, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  plot_parcel_set_from_collated_object(collated_parcel_sets_object, assessed_set_indexes = assessed_set_index, global_params$time_steps, global_params$eco_dims)
  plot_parcel_set_from_collated_object(collated_parcel_sets_object, assessed_set_indexes = (1:global_params$total_dev_num), global_params$time_steps, global_params$eco_dims)
  two_plot(sum_cols(cfac_parcel_trajs, 1:dim(cfac_parcel_trajs)[2]), sum_cols(parcel_trajs, 1:dim(parcel_trajs)[2]), colours = c('red', 'black'))
}


setup_sub_plots <- function(nx, ny){
 
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(2, 1, 1, 0), oma = c(4, 4, 0.5, 0.5))
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
}

plot_parcel_set <- function(parcel_sets_object, assess_set_ind){
  
  set_up_sub_plots(1, 3)
  parcel_set_offset = parcel_sets_object$offsets[[assess_set_ind]]
  parcel_set_development = parcel_sets_object$developments[[assess_set_ind]]
  offset_parcel_num = length(parcel_set_offset$parcel_indexes)
  
  plot_list = vector('list', 3)
  d_rest_gains = parcel_traj_sum(parcel_set_development$restoration_gains)
  d_avoided_degs = parcel_traj_sum(parcel_set_development$avoided_degredation)
  d_net = d_rest_gains + d_avoided_degs
  plot_list[[1]] = cbind(d_rest_gains, d_avoided_degs, d_net)
  
  o_rest_gains = parcel_traj_sum(parcel_set_offset$restoration_gains)
  o_avoided_degs = parcel_traj_sum(parcel_set_offset$avoided_degredation)
  o_net = o_rest_gains + o_avoided_degs
  
  plot_list[[2]] = cbind(o_rest_gains, o_avoided_degs, o_net)
  
  plot_list[[3]] = cbind(o_net, d_net, (o_net + d_net))
  lim_vec = cbind(plot_list[[1]], plot_list[[2]], plot_list[[3]])
  
  mx = max(lim_vec)
  mn = min(lim_vec)
  
  overlay_plots(plot_list[[1]], yticks = 'y', axis_lab = TRUE, ylim = c(mn, mx), (heading = "developments"), ylab = '')
  overlay_plots(plot_list[[2]], yticks = 'n', axis_lab = TRUE, ylim = c(mn, mx), (heading = "offsets"), ylab = '')
  overlay_plots(plot_list[[3]], yticks = 'n', axis_lab = TRUE, ylim = c(mn, mx), (heading = "net outcome"), ylab = '')
}

# collated_object = collated_parcel_sets_object
# assessed_set_indexes = 1
# time_steps = global_params$time_steps
# eco_dims = global_params$eco_dims

plot_parcel_set_from_collated_object <- function(collated_object, assessed_set_indexes, time_steps, eco_dims){

  collated_offsets = collated_object$offsets
  collated_developments = collated_object$developments
  
  plot_list = vector('list', 3)
  plot_list[[1]] = build_plot_list(collated_developments, assessed_set_indexes, time_steps, eco_dims)
  plot_list[[2]] = build_plot_list(collated_offsets, assessed_set_indexes, time_steps, eco_dims)
  net_array = array(0, c(time_steps, 3, eco_dims))
  net_array[, 1, ] = plot_list[[1]][, 3, ]
  net_array[, 2, ] = plot_list[[2]][, 3, ]
  net_array[, 3, ] = plot_list[[1]][, 3, ] + plot_list[[2]][, 3, ]
  plot_list[[3]] = net_array
    
  lim_vec = abind(plot_list[[1]][, , 1], plot_list[[2]][, , 1], plot_list[[3]][, , 1])
  mx = max(lim_vec)
  mn = min(lim_vec)

  
  overlay_plots(plot_list[[1]][, , 1], yticks = 'y', axis_lab = TRUE, ylims = c(mn, mx), (heading = "developments"), ylab = '')
  overlay_plots(plot_list[[2]][, , 1], yticks = 'n', axis_lab = TRUE, ylims = c(mn, mx), (heading = "offsets"), ylab = '')
  overlay_plots(plot_list[[3]][, , 1], yticks = 'n', axis_lab = TRUE, ylims = c(mn, mx), (heading = "net outcome"), ylab = '')
  
}


build_plot_list <- function(coll_object, parcel_set_indexes, time_steps, eco_dims){
  plot_list = array(0, c(time_steps, 3, eco_dims))
  for (eco_ind in 1:eco_dims){
    plot_list[, 1, eco_ind] = sum_cols(coll_object$rest_gains[, , eco_ind], parcel_set_indexes)
    plot_list[, 2, eco_ind] = sum_cols(coll_object$avoided_degs[, , eco_ind], parcel_set_indexes)
    plot_list[, 3, eco_ind] = sum_cols(plot_list[, , eco_ind], c(1, 2))
  }
  return(plot_list)
}

sum_cols <- function(arr_in, col_indexes){
  arr_to_use = arr_in[, col_indexes]
  if (length(col_indexes) == 1){
    arr_out = t(t(arr_to_use))
  } else {
    arr_out = apply(arr_to_use, MARGIN = 1, sum)
  }
  return(arr_out)
}

parcel_traj_sum <- function(parcel_3D){
  parcel_traj = apply(parcel_3D, MARGIN=c(1, 2), sum)
  return(parcel_traj)
}


# 
# plot_parcel_set_dimensional <- function(collated_object, assessed_set_ind){
# 
#   assessed_offset = collated_object$assessed_offsets[[assessed_set_ind]]
#   assessed_development = assessed_developments[[assessed_set_ind]]
#   
#   graphics.off()
#   offset_parcel_num = length(assessed_offset$parcel_indexes)
#   sub_plots = 1:(global_params$eco_dims*3)
#   dim(sub_plots) = c(global_params$eco_dims, 3)
#   layout(sub_plots)
#   
#   o_gains = apply(assessed_offset$restoration_gains, MARGIN=c(1, 2), sum)
#   o_degs = apply(assessed_offset$avoided_degredation, MARGIN=c(1, 2), sum)
# 
#   d_gains = assessed_development$restoration_gains
#   d_degs = assessed_development$avoided_degredation
#   
#   lim_vec = c(d_gains, d_degs, (d_gains + d_degs), o_gains, o_degs, (o_gains + o_degs)) 
#   mx = max(lim_vec)
#   mn = min(lim_vec)
#   
#   for (eco_ind in seq_len(global_params$eco_dims)){
#     plot_a = d_gains[, eco_ind, 1]
#     plot_b = d_degs[, eco_ind, 1]
#     plot_c = plot_a + plot_b
#     plot(plot_a, type = 'l', ylim = c(mn, mx), main = "developments", ylab = '', xlab = paste('dim = ', eco_ind))
#     lines(plot_b, col = 'blue', ylim = c(mn, mx))
#     lines(plot_c, col = 'red', ylim = c(mn, mx))
#   }
#   
#   for (eco_ind in seq_len(global_params$eco_dims)){
#     plot_a = o_gains[, eco_ind] 
#     plot_b = o_degs[, eco_ind] 
#     plot_c = plot_a + plot_b
#     plot(plot_a, type = 'l', ylim = c(mn, mx), main= "offsets", ylab = '', xlab = paste('dim = ', eco_ind))
#     lines(plot_b, col = 'blue', ylim = c(mn, mx))
#     lines(plot_a + plot_b, col = 'blueviolet', ylim = c(mn, mx))
#   }
# 
#   for (eco_ind in seq_len(global_params$eco_dims)){
#     plot_a = o_gains[, eco_ind]  + o_degs[, eco_ind] 
#     plot_b = d_gains[, eco_ind, 1] + d_degs[, eco_ind, 1]
#     plot_c = plot_a + plot_b
#     plot(plot_a, type = 'l', col = 'blueviolet', ylim = c(mn, mx), main= "net", ylab = '', xlab = paste('dim = ', eco_ind))
#     lines(plot_b, col = 'red', ylim = c(mn, mx))
#     lines(plot_a + plot_b, col = 'black', ylim = c(mn, mx))
#   }
#   
# }



plot_net_parcel_sets <- function(collated_object, time_steps, eco_dims, parcel_set_list){
  
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


collate_parcel_sets <- function(parcel_sets_object, time_steps, eco_dims){
  collated_parcel_sets = list()
  collated_parcel_sets$offsets = collate_assessed_object(parcel_sets_object$offsets, time_steps, eco_dims)
  collated_parcel_sets$developments = collate_assessed_object(parcel_sets_object$developments, time_steps, eco_dims)
  return(collated_parcel_sets)
}


collate_assessed_object <- function(current_assessed_object, time_steps, eco_dims){
  collated_object = list()
  
  parcel_set_num = length(current_assessed_object)
  collate_array = array(0, c(time_steps, parcel_set_num, eco_dims))
  rest_gains = collate_array
  avoided_degs = collate_array
  parcel_set_trajs = collate_array
  parcel_set_counterfacs = collate_array
  rm(collate_array)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    for (eco_ind in seq_len(eco_dims)){
      rest_gains[, parcel_set_ind, eco_ind] = apply(current_assessed_object[[parcel_set_ind]]$restoration_gains, MARGIN = 1, sum)
      avoided_degs[, parcel_set_ind, eco_ind] = apply(current_assessed_object[[parcel_set_ind]]$avoided_degredation, MARGIN = 1, sum)
      parcel_set_trajs[, parcel_set_ind, eco_ind] = apply(current_assessed_object[[parcel_set_ind]]$parcel_trajectory_sum, MARGIN = 1, sum)
      parcel_set_counterfacs[, parcel_set_ind, eco_ind] = apply(current_assessed_object[[parcel_set_ind]]$parcel_counter_sum, MARGIN = 1, sum)
    }
  }
  collated_object$rest_gains = rest_gains
  collated_object$avoided_degs = avoided_degs
  collated_object$parcel_set_trajs = parcel_set_trajs
  collated_object$parcel_set_counters = parcel_set_counterfacs
  return(collated_object)
}
  

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
#     plot_1c = plot_object$initial_sum_set[parcel_set_num]*array(1, length(plot_1a))
#     plot_2a = plot_object$restoration_gains_set[, parcel_set_num]
#     plot_2b = plot_object$avoided_degredation_set[, parcel_set_num]
# 
#   } else if(assess_type == 'individual'){
#     plot_1a = plot_object$net_parcel[, parcel_set_num]
#     plot_1b = plot_object$net_counter[, parcel_set_num]
#     plot_1c = plot_object$initial_sum[parcel_set_num]*array(1, length(plot_1a))
#     plot_2a = plot_object$restoration_gains[, parcel_set_num]
#     plot_2b = plot_object$avoided_degredation[, parcel_set_num]
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
#   graphics.off()
#   sub_plots = 1:6
#   dim(sub_plots) = c(2, 3)
#   layout(sub_plots)
#   plot_parcel_sums(plot_type = 'developments', assess_type, assess_object, parcel_set_num)
#   plot_parcel_sums(plot_type = 'offsets', assess_type, assess_object, parcel_set_num)
#   
#   plot_a = assess_object$developments$restoration_gains_set[, parcel_set_num] + assess_object$developments$avoided_degredation_set[, parcel_set_num]
#   plot_b = assess_object$offsets$restoration_gains_set[, parcel_set_num] + assess_object$offsets$avoided_degredation_set[, parcel_set_num]
#   two_plot(plot_a, plot_b, cols = c('red', 'black'))
#   lines((plot_a + plot_b), col = 'blue')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
# }
# 
# 
# 
# plot_net_parcel_sets <- function(assess_object){
#   graphics.off()
#   sub_plots = 1:3
#   dim(sub_plots) = c(1, 3)
#   layout(sub_plots)
#   
#   plot_a = rowSums(assess_object$developments$restoration_gains_set) 
#   plot_b = rowSums(assess_object$developments$avoided_degredation_set)
#   plot_c = rowSums(assess_object$offsets$restoration_gains_set) 
#   plot_d = rowSums(assess_object$offsets$avoided_degredation_set)
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



collate_parcel_realisations_multi <- function(record_parcel_sets, realisations, parcel_ind, land_parcels, time_steps, eco_dims){
  realisation_num = length(realisations)
  current_parcel = land_parcels[[parcel_ind]]
  
  parcel_realisations = list()
  parcel_realisations$parcel_3D = vector('list', realisation_num)
  parcel_realisations$sums = array(0, c(time_steps, realisation_num, eco_dims))
  
  for (realisation_ind in seq_len(realisation_num)){
    for (eco_ind in seq_len(eco_dims)){
      if (record_parcel_sets == FALSE){
        current_realisation = realisations[[realisation_ind]]
      } else current_realisation = (realisations[[realisation_ind]]$trajectories)
    
      current_parcel_3D = extract_3D_parcel(current_parcel, current_realisation[[eco_ind]])
      parcel_realisations$parcel_3D[[realisation_ind]][[eco_ind]] = current_parcel_3D
      parcel_realisations$sums[, realisation_ind, eco_ind] = apply(current_parcel_3D, 3, sum)
    }
  }
  return(parcel_realisations)
}





tally_devs_offsets <- function(record_parcel_sets, realisations, parcels, time_steps){
  
  tally_object = list()
  realisation_num = length(realisations)
  parcel_num = length(parcels$land_parcels)
  offset_tally = array(0, c(time_steps, parcel_num))
  development_tally = array(0, c(time_steps, parcel_num))
  
  for (realisation_ind in seq_len(realisation_num))
  {
    current_realisation = realisations[[realisation_ind]]
    current_dev_num = length(current_realisation$development_list)
    for (parcel_set_ind in seq_len(current_dev_num))
    {
      yr = current_realisation$developments[[parcel_set_ind]]$yr
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
  






sum_parcels_multi <- function(trajectories, parcel_inds_to_use, land_parcels, time_steps, eco_dims){
  
  parcel_num = length(parcel_inds_to_use)
  parcel_sums = array(0, c(time_steps, parcel_num, eco_dims))
  
  for (parcel_ind in seq_len(parcel_num)){   
    current_parcel_ind = parcel_inds_to_use[parcel_ind]
    current_parcel = land_parcels[[current_parcel_ind]]
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_trajectory = extract_3D_parcel(current_parcel, trajectories[[eco_ind]])
      parcel_sums[, parcel_ind, eco_ind] = apply(current_parcel_trajectory, 3, sum)
    }  
  }
  
  return(parcel_sums)  
}

sum_regions <- function(parcel_sums, parcel_index_list, regions, time_steps){
  region_num = length(regions)
  region_sums = array(0, c(time_steps, region_num))
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


true_sums <- function(parcel_inds, object_sums, counter_sums){
  true_sums_object = list()
  true_sums_object$parcel_sums = object_sums$parcel_sums - counter_sums$parcel_sums[, parcel_inds]
  true_sums_object$net_sums = object_sums$net_sums - counter_sums$net_sums
  return(true_sums_object)
}


find_sums <- function(trajectories, parcels_inds, parcels, time_steps){
  object = list()
  object$parcel_sums = sum_parcels(trajectories, parcel_inds, parcels$land_parcels, time_steps)
  object$region_sums = sum_regions(object$parcel_sums, parcel_inds, parcels$regions, time_steps)
  object$net_sums = rowSums(object$region_sums)
  return(object)
}



# update_counterfactuals <- function(adjusted_counterfactuals, current_parcel_set, current_develop_num, land_parcels, region, yr){
#   current_parcel_num = length(current_parcel_set)
#   current_prob = current_develop_num/current_parcel_num
#   current_slice = adjusted_counterfactuals[, , yr]
#   for (parcel_ind in current_parcel_set){
#     current_parcel = land_parcels[[parcel_ind]]
#     current_slice[current_parcel] = (1 - current_prob)*current_slice[current_parcel]
#   }
#   for (parcel_ind in setdiff(region, current_parcel_set)){
#     current_parcel = land_parcels[[parcel_ind]]
#     current_slice[current_parcel] = 0
#   }
#   adjusted_counterfactuals[, , yr] = current_slice
#   return(adjusted_counterfactuals)
# }


overlay_plots <- function(plot_array, yticks, axis_lab, ylims, heading, ylab){
  if (yticks == 'y'){
    plot(plot_array[, 1], axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab)
  } else if ((yticks == 'n')){
    plot(plot_array[, 1], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab)
  }
  for (s in 2:dim(plot_array)[2]){
    lines(plot_array[, s],  ylim = ylims)
  }
}

# probability_counterfactuals <- function(adjusted_counterfactuals, current_parcel_set, current_develop_num, land_parcels, region, yr){
#   current_parcel_num = length(current_parcel_set)
#   current_prob = 1 - current_develop_num/current_parcel_num
#   current_slice = adjusted_counterfactuals[, , yr]
#   for (parcel_ind in length(region)){
#     current_parcel = land_parcels[[parcel_ind]]
#     current_slice[current_parcel] = current_prob*current_slice[current_parcel]
#   }
#   adjusted_counterfactuals[, , yr] = current_slice
#   return(adjusted_counterfactuals)
# }


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
