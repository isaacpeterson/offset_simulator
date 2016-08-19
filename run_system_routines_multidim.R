run_offsets_simulation <- function(global_params, initial_ecology, decline_rates_initial, parcels, dev_vec, banked_offset_vec){ # run the model and return outputs
  
  if (global_params$set_seed == TRUE){
    set.seed(123)
  }
  
  trajectories <- initialise_trajectories(global_params$eco_dims, land_parcels = parcels$land_parcels, global_params$time_steps)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
  offsets_object <- initialise_parcel_set_object()   #initialise offsets object to store all offsets
  developments_object <- initialise_developments_object(dev_vec) #initialise developments object to store all offsets
  credited_developments_object <- initialise_parcel_set_object()
  banked_offsets_object <- initialise_banked_offsets_object(global_params, banked_offset_vec)    #initialise banked object to store offsets when using banking
  
  model_outputs <- run_system(trajectories, offsets_object, developments_object, banked_offsets_object, global_params,    # run the model, select and record parcel sets, calculate landscape condition time series
                              current_ecology = initial_ecology, decline_rates = decline_rates_initial, parcels, index_object, credited_developments_object)  
  
  model_outputs$index_object = model_outputs$index_object               #output index object - this is used to track what indexes are available, and have been used in offsets/developments
  model_outputs$offset_success_flag = (length(unlist(model_outputs$developments)) > 0)*(length(unlist(model_outputs$offsets)) > 0)          #used to flag whether the model actually found any offset/development matches
  
  return(model_outputs)
  
}  



#   current_ecology = initial_ecology
#   decline_rates = decline_rates_initial
#   trajectories <- initialise_trajectories(global_params$eco_dims, land_parcels = parcels$land_parcels, global_params$time_steps)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
#   offsets_object <- initialise_parcel_set_object()   #initialise offsets object to store all offsets
#   developments_object <- initialise_developments_object(dev_vec) #initialise developments object to store all offsets
#   
#   banked_offsets_object <- initialise_banked_offsets_object(global_params, banked_offset_vec)
#   index_object <- initialise_index_object(parcels, global_params)
#   
#   credited_developments_object <- initialise_parcel_set_object()

run_system <- function(trajectories, offsets_object, developments_object, banked_offsets_object, global_params, 
                       current_ecology, decline_rates_initial, parcels, index_object, credited_developments_object){ # main engine for code - returns all development/offset parcel sets, land parcel trajectories etc.
  
  dev_credit = 0
  decline_rates = decline_rates_initial
  
  for (yr in seq_len(global_params$time_steps)){          #run through main time loop
    current_dev_nums <- find_current_dev_nums(developments_object$dev_vec, global_params$region_num, yr) #developments to perform in current year per region
    time_horizon <- assess_time_horizon(global_params$use_offset_time_horizon, global_params$offset_time_horizon, global_params$time_steps, yr)   # determine whetehr time horizons to use in calcs are till simulation end or user provided length of time
    
    for (region_ind in seq_len(parcels$region_num)){            #cycle through each region
      
      if (global_params$use_offset_bank == TRUE){               # perform offset banking routine if selected
        
        offset_bank_num = banked_offsets_object$banked_offset_vec[yr]   # how many offsets to be added in current year
        
        if (offset_bank_num > 0){
          
          total_current_pool = index_object$ind_available[[region_ind]]             # determine parcel indexes currently available
          index_object$parcel_num_remaining = length(total_current_pool)            # record how many parcels remaining
          current_banked_offset_pool <- select_banked_offset_indexes(offset_bank_num, total_current_pool)   # select current number of offset parcels from current available pool to add to banked offset pool
          
          current_banked_offset <- record_current_parcel_set(current_ecology[current_banked_offset_pool], current_pool = current_banked_offset_pool, parcel_num_remaining = index_object$parcel_num_remaining, yr)   # arrange current parcel data  
          
          banked_offsets_object <- update_banked_offsets(banked_offsets_object, current_banked_offset)      #record current parcel data into banked offset object
          
          index_object <- update_banked_offset_pool(index_object, current_banked_offset_pool)   # add new offset parcels to banked offset pool
          index_object <- update_ind_available(update_type = 'offset', index_object, current_banked_offset_pool, region_ind) #remove selected offest parcels from avilable pool
          decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', offset_action_type = global_params$offset_action_type, current_banked_offset_pool) # update decline rate for current banked offset parcels 
          
        }
      }
      
      current_develop_num = current_dev_nums[region_ind]        # how many parcels to be developed in current year
      
      for (parcel_set_count_index in seq_len(current_develop_num)){   # cycle through number of developments and associated offsets
        
        if (global_params$use_offset_bank == TRUE){
          current_offset_pool = index_object$banked_offset_pool       # available pool when using offset banking
          
          if (length(current_offset_pool) == 0){break}              #break out when no parcels are left in banking pool
          
          offset_pool_object <- prepare_offset_bank(banked_offsets_object, current_offset_pool, restoration_flag = global_params$offset_restoration_flag, 
                                                    parcels$land_parcels, current_ecology, eco_dims = global_params$eco_dims)   #arrange current banked offset data into form to use in parcel set determination
          offset_pool_type = 'offset_bank'
          
        } else {
          current_offset_pool = index_object$ind_available[[region_ind]]
          parcel_num_remaining = length(current_offset_pool)
          offset_pool_object <- record_current_parcel_set(current_ecology[current_offset_pool], current_offset_pool, parcel_num_remaining, yr)   #arrange available parcel pool into form to use in parcel set determination
          offset_pool_type = 'offsets'
        }
        
        offset_pool_object <- assess_current_pool(pool_object = offset_pool_object, pool_type = offset_pool_type, calc_type = global_params$offset_calc_type, cfacs_flag = global_params$offset_cfacs_flag, 
                                                  adjust_cfacs_flag = global_params$adjust_offset_cfacs_flag, cfac_type = global_params$cfac_type_in_offset_calc, time_horizon_type = global_params$offset_time_horizon_type,
                                                  global_params, dev_vec = developments_object$dev_vec, decline_rates_initial, time_horizon, yr)      #determine available parcel values, depending on what particular offset policy is in use using counterfactuals etc.
        
        if (global_params$use_dev_credit == TRUE){
          
          if (global_params$use_offset_bank == TRUE){
            net_development_val = sum(unlist(developments_object$parcel_vals_used))
            current_pool_vals = unlist(offset_pool_object$parcel_vals_used)
            dev_credit = sum(current_pool_vals) - net_development_val
          } 
          
          current_development_object = develop_from_credit(current_ecology, dev_credit, global_params, dev_vec = developments_object$dev_vec, ind_available = index_object$ind_available[[region_ind]], decline_rates_initial, 
                                                           land_parcels = parcels$land_parcels, yr, time_horizon)
          dev_credit = current_development_object$dev_credit
          
          if (current_development_object$match_flag == TRUE){
            index_object$credited_parcel_set_count = index_object$credited_parcel_set_count + 1
          }
          
        } else {
          current_development_object = list()
          current_development_object$match_flag = FALSE
        }
        
        parcel_set_flag = ((current_development_object$match_flag == FALSE) & (global_params$use_parcel_sets == TRUE))
        
        if (parcel_set_flag == TRUE){  #if insufficient credits accumulated, perform offset parcel set match
          
          if (length(index_object$ind_available[[region_ind]]) > 0){
            match_object <- match_parcel_set(offset_pool_object, dev_credit, global_params, dev_vec = developments_object$dev_vec, ind_available = index_object$ind_available[[region_ind]], 
                                             current_ecology, decline_rates_initial, parcels$land_parcels, yr, time_horizon)  #perform the matching routine - i.e. find a matching development/offset set.
          } else{break} 
          
          current_development_object = match_object$current_development_object
          if (length(unlist(match_object$offset_object)) == 0){
            break
            print('break flag')
          }
          
          if (current_development_object$match_flag == TRUE){                              
            
            index_object$parcel_set_count = index_object$parcel_set_count + 1     
            
            current_offset_object <- match_object$offset_object
            current_offset_indexes = current_offset_object$parcel_indexes
            
            if (global_params$use_offset_bank == TRUE){
              
              banked_offset_inds_used = list_intersect(index_object$banked_offset_pool, current_offset_indexes)         # when using offset banking determine parcels used in matching routine and remove from available pool
              banked_offset_inds_used = banked_offset_inds_used$match_ind
              index_object$banked_offset_pool = index_object$banked_offset_pool[-banked_offset_inds_used]
              
            } else {
              index_object = update_ind_available(update_type = 'offset', index_object, current_offset_indexes, region_ind)         # determine parcels used in matching routine and remove from available pool
              decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', offset_action_type = global_params$offset_action_type, current_offset_indexes) # set elements in decline rates array corresponding to offsets to restoration rates
            }
            offsets_object <- write_current_parcel_set(offsets_object, current_offset_object, index_object$parcel_set_count)      #record current offset parcels in offsets object containing all offsets info
          }
          
        }
        
        if (current_development_object$match_flag == TRUE){
          current_dev_indexes = current_development_object$parcel_indexes
          index_object = update_ind_available(update_type = 'development', index_object, current_dev_indexes, region_ind)                 #remove development parcels from available pool
          decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'development', offset_action_type = vector(), current_dev_indexes)     # set elements corresponding to developed parcels in decline rates array to zero.
          
          if (global_params$use_parcel_sets){
            developments_object <- write_current_parcel_set(developments_object, current_development_object, index_object$parcel_set_count)  # record development info for current parcel set into object containing all development info
          } else {
            credited_developments_object = write_current_parcel_set(credited_developments_object, current_development_object, parcel_set_count = index_object$credited_parcel_set_count)
          }
          dev_credit = current_development_object$dev_credit
        }
        
      }
      
    }
    
    if (length(unlist(offsets_object$parcel_indexes)) > 0){
      assessed_offsets <- assess_offset_gains(offsets_object, global_params, decline_rates_initial, time_horizon, yr)
      decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', offset_action_type = 'maintain', parcel_indexes = assessed_offsets$success_inds)
    }
    trajectories <- update_trajectories(trajectories, global_params$eco_dims, current_ecology, yr)
    current_ecology <- project_current_system_multi(current_ecology, decline_rates, global_params$min_eco_val, global_params$max_eco_val,
                                                    global_params$max_eco_val, time_horizon = 1, global_params$eco_dims)     # update ecology for subsequent time step using current decline rates
    print(yr)
    
  }
  
  
  if (global_params$offset_bank_type == 'credit'){
    offsets_object <- write_current_parcel_set(offsets_object, banked_offsets_object, parcel_set_count = 1)           # if using o
    developments_object <- credited_developments_object
  }
  
  outputs = list()
  outputs$credited_developments_object = credited_developments_object
  outputs$offsets = offsets_object
  outputs$developments = developments_object
  outputs$trajectories = trajectories
  outputs$decline_rates = decline_rates
  outputs$parcel_num_remaining = index_object$parcel_num_remaining
  outputs$index_object = index_object
  return(outputs)
  
} 


update_trajectories <- function(trajectories, eco_dims, current_ecology, yr){
  for (parcel_count_ind in seq_len(length(trajectories))){
    for (eco_ind in seq_len(global_params$eco_dims)){
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


select_from_offset_group <- function(){
  
}

assess_offset_gains <- function(offsets_object, global_params, decline_rates_initial, time_horizon, yr){
  assessed_offsets_object <- list()
  eco_dims = global_params$eco_dims
  parcel_set_count = length(offsets_object$parcel_indexes)
  assessed_offsets <- vector('list', parcel_set_count)
  parcel_sets_to_assess = seq(parcel_set_count)
  
  for (parcel_set_ind in parcel_sets_to_assess){
    current_offset_object <- lapply(seq_along(offsets_object), function(i) offsets_object[[i]][[parcel_set_ind]])
    names(current_offset_object) <- names(offsets_object)
    offset_pool_object <- assess_current_pool(pool_object = current_offset_object, pool_type = "offset_bank", calc_type = global_params$offset_calc_type, cfacs_flag = global_params$offset_cfacs_flag, 
                                              adjust_cfacs_flag = global_params$adjust_offset_cfacs_flag, cfac_type = global_params$cfac_type_in_offset_calc, time_horizon_type = global_params$offset_time_horizon_type,
                                              global_params, dev_vec = developments_object$dev_vec, decline_rates_initial, time_horizon, yr)      #determine available parcel values, depending on what particular offset policy is in use using counterfactuals etc.
    
    for (eco_ind in seq(eco_dims)){
      assessed_offsets[[parcel_set_ind]] = nested_list_sum(subtract_nested_lists(offset_pool_object$parcel_vals_used, current_offset_object$parcel_vals_used))
    }
  }
  
  success_inds <- which(unlist(assessed_offsets) > 0)
  assessed_offsets_object$success_inds <- offsets_object$parcel_indexes[success_inds]
  assessed_offsets_object$assessed_offsets <- assessed_offsets
  return(assessed_offsets_object)
}

  
simplify_list_to_array <- function(collated_list){
  collated_array = simplify2array(collated_list)
  collated_dims = dim(collated_array)
  dim(collated_array) = c(collated_dims[1], collated_dims[4], 1)
  return(collated_array)
}




find_prog_vector <- function(time_steps, prog_start, prog_end, total_prog_num, sd){
  dev_vec = array(0, time_steps)
  dev_vec[prog_start:prog_end] = split_vector((prog_end - prog_start + 1), total_prog_num, sd, min_width = -1)
  return(dev_vec)
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
  landscape[[1]] = 100*img@grey
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


initialise_index_object <- function(parcels, global_params){
  index_object = list()
  index_object$ind_available = vector('list', parcels$region_num)
  index_object$developments = vector()
  index_object$offsets = vector()
  index_object$banked_offset_pool = vector()
  index_object$parcel_sets = list()
  index_object$parcel_num_remaining = vector()
  index_object$parcel_set_count = 0
  index_object$credited_parcel_set_count = 0
  index_object$break_flag = FALSE
  
  index_object$ind_available = parcels$regions
  
  if (global_params$screen_parcels == TRUE){
    parcel_lengths <- unlist(lapply(seq_along(parcels$land_parcels), function(i) length(parcels$land_parcels[[i]])))
    parcels_to_screen <- which(parcel_lengths < global_params$parcel_size_lim)
    
    for (region_ind in seq(parcels$region_num)){
    current_region = index_object$ind_available[[region_ind]]
    inds_to_remove = which(current_region %in% parcels_to_screen)
    if (length(inds_to_remove) > 0){
      index_object$ind_available[[region_ind]] = index_object$ind_available[[region_ind]][-inds_to_remove]
    }
   }
  }
  
  return(index_object)
  
}




list_of_arrays <- function(list_dims, array_dims){
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



# 
# 
# build_cfacs_by_year <- function(global_params, decline_rates, land_parcels, initial_ecology){
#   current_ecology = initial_ecology
#   cfacs = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
#   cfacs[, , 1] = initial_ecology
#   parcel_num = length(land_parcels)
#   
#   for (yr in seq_len(global_params$time_steps)){
#     for (parcel_ind in seq_len(parcel_num)){
#       current_parcel = select_land_parcel(land_parcels, parcel_ind)
#       current_dec_rate = decline_rates[parcel_ind]      
#       updated_parcel = sapply(current_ecology[current_parcel], project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, decline_rate = current_dec_rate, time_horizon = 1, time_fill = FALSE)
#       current_ecology[current_parcel] = updated_parcel 
#     }
#     if (global_params$blur == TRUE){
#       current_ecology = Blur_2D(current_ecology, 0.5, 0.5)
#     } 
#     cfacs[, , yr] = current_ecology
#   }
#   
#   return(cfacs)
# }


# current_offset_proj = predict_parcel_traj(current_parcel_ecology, current_parcel_ind, parcel_traj_type = global_params$offset_action_type, 
#                                           global_params, decline_rates, time_horizon = current_time_horizon)
#predict_parcel_traj(current_parcel_ecology, current_parcel_ind, eco_ind, parcel_traj_type = 'protect', global_params, decline_rates, time_horizon = time_horizons[parcel_count_ind])

#predict_parcel_traj(current_parcel_ecology, parcel_traj_type, global_params, current_dec_rate = decline_rates[[parcel_count_ind]], 
#                                                       time_horizon = unlist(time_horizons[parcel_count_ind]), time_fill)



#predicted_parcel = sapply(parcel_eco_vals[[parcel_count_ind]], project_ecology, min_eco_val, max_eco_val, decline_rate, time_horizons[parcel_count_ind], time_fill = FALSE)




predict_parcel_traj <- function(current_parcel_ecology, parcel_traj_type, global_params, current_dec_rate, time_horizon, time_fill){
  
  eco_dims = length(current_parcel_ecology)
  projected_ecology = vector('list', eco_dims)
  
  if (parcel_traj_type == 'maintain'){
    current_dec_rate = rep(list(1e-10), eco_dims)
  } else if (parcel_traj_type == 'restore'){
    current_dec_rate = rep(list(global_params$restoration_rate), eco_dims)
  }
  
  for (eco_ind in seq_len(eco_dims)){
    current_ecology_slice = current_parcel_ecology[[eco_ind]]
    
    current_predicted_ecology = sapply(current_ecology_slice, project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, 
                                       decline_rate = current_dec_rate[[eco_ind]], time_horizon = time_horizon, time_fill)  # update ecology according to ecological curve in project_ecology function (currently logistic) - curve parameters are contained in decline_rates array
    if (time_horizon == 0){
      dim(current_predicted_ecology) = c(1, length(current_predicted_ecology))
    }
    projected_ecology[[eco_ind]] = current_predicted_ecology
  }
  return(projected_ecology)
  
}



# predict_parcel_traj <- function(current_parcel_ecology, parcel_traj_type, global_params, current_dec_rate, time_horizon, time_fill){
#   
#   eco_dims = global_params$eco_dims
#   projected_ecology = vector('list', eco_dims)
#   
#   if (parcel_traj_type == 'maintain'){
#     current_dec_rate = rep(list(1e-10), eco_dims)
#   } else if (parcel_traj_type == 'restore'){
#     current_dec_rate = rep(list(global_params$restoration_rate), eco_dims)
#   }
#   
#   for (eco_ind in seq_len(eco_dims)){
#     current_ecology_slice = current_parcel_ecology[[eco_ind]]
#     if (time_horizon > 0){
#       current_predicted_ecology = lapply(current_ecology_slice, project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, 
#                                          decline_rate = current_dec_rate[[eco_ind]], time_horizon = time_horizon, time_fill = FALSE)  # update ecology according to ecological curve in project_ecology function (currently logistic) - curve parameters are contained in decline_rates array
#       dim(current_predicted_ecology) = c(dim(current_ecology_slice), 1)
#       current_predicted_ecology = aperm(current_predicted_ecology, c(3, 2, 1))
#       dim(current_predicted_ecology) = c(dim(current_ecology_slice), (time_horizon + 1))
#     } else{
#       current_predicted_ecology = current_ecology_slice
#       dim(current_predicted_ecology) = c(dim(current_ecology_slice), 1)
#     }
#     
#     projected_ecology[[eco_ind]] = current_predicted_ecology
#   }
#   return(projected_ecology)
#   
# }



ecology_to_parcels <- function(current_ecology, land_parcels){
  parcel_num = length(land_parcels)
  parcel_ecologies <- vector('list', parcel_num)
  for (parcel_ind in 1:parcel_num){
    current_parcel = land_parcels[[parcel_ind]]
    parcel_ecologies[[parcel_ind]] = extract_parcel(current_parcel, current_ecology)
  }
  return(parcel_ecologies)
}


# build_cfacs_list <- function(global_params, decline_rates, parcel_ecologies, time_horizons){
#   
#   parcel_num = length(parcel_ecologies)
#   cfacs <- generate_nested_list(outer_dim = parcel_num, inner_dim = eco_dims)
#   for (parcel_count_ind in seq_len(parcel_num)){
#     current_parcel_ecology = parcel_ecologies[[parcel_count_ind]]
#     cfacs[[parcel_count_ind]] = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = 'protect', global_params, current_dec_rate = decline_rates[[parcel_count_ind]], 
#                                                     time_horizon = unlist(time_horizons[parcel_count_ind]), time_fill = TRUE)
#   }
#   
#   return(cfacs)
# }


#calc_parcel_trajs(parcel_ecologies = pool_object$parcel_ecologies, parcel_traj_type = 'restore', decline_rates_initial[current_pool], time_horizons, global_params, time_fill = FALSE)


calc_parcel_trajs <- function(parcel_ecologies, parcel_traj_type, decline_rates, time_horizons, global_params, time_fill){
  parcel_num = length(parcel_ecologies)
  parcel_trajs <- generate_nested_list(outer_dim = parcel_num, inner_dim = global_params$eco_dims)
  
  if (length(decline_rates) != parcel_num){
    print('length error')
  }
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ecology = parcel_ecologies[[parcel_count_ind]]
    parcel_trajs[[parcel_count_ind]] = predict_parcel_traj(current_parcel_ecology, parcel_traj_type, global_params, current_dec_rate = decline_rates[[parcel_count_ind]], 
                                                    time_horizon = unlist(time_horizons[parcel_count_ind]), time_fill)
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





initialise_decline_rates <- function(parcels, mean_decline_rate, decline_rate_std, eco_dims){
  
  regions = parcels$regions
  land_parcels = parcels$land_parcels
  parcel_num = length(land_parcels)
  decline_rates = generate_nested_list(parcel_num, eco_dims)
  
  for (eco_ind in seq_len(eco_dims)){
    for (region_ind in seq_len(length(regions))){
      current_region = regions[[region_ind]]
      current_parcel_num = length(current_region)    
      decline_params = c(length(current_region), mean_decline_rates[[region_ind]][[eco_ind]], decline_rate_std) 
      region_decline_rates = array(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), length(current_region))
      
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



# match_parcel_set(offset_pool_object, global_params, ind_available = index_object$ind_available[[region_ind]], 
#                  current_ecology, decline_rates_initial, parcels$land_parcels, yr, time_horizon, 
#                  net_development_val = sum(unlist(developments_object$parcel_vals_used))) 

# ind_available = index_object$ind_available[[region_ind]]
# net_development_val = sum(unlist(developments_object$parcel_vals_used))


match_parcel_set <- function(offset_pool_object, dev_credit, global_params, dev_vec, ind_available, current_ecology, decline_rates_initial, land_parcels, yr, time_horizon){
  
  current_pool_vals = offset_pool_object$parcel_vals_used
  current_pool_indexes = offset_pool_object$parcel_indexes
  parcel_num_remaining = length(ind_available)
  current_match_pool = ind_available
  
  match_object = list()
  match_object$match_flag = FALSE
  
  while( (match_object$match_flag == FALSE) & length(current_match_pool > 0) ){
    
    current_test_index = select_rand_index(current_match_pool, parcel_num = 1) #select potential parcel to develop
    
    dev_pool_object <- record_current_parcel_set(current_ecology[current_test_index], current_test_index, parcel_num_remaining, yr) #record potential current development parcel attributes
    
    dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, pool_type = 'devs', calc_type = global_params$dev_calc_type, cfacs_flag = global_params$dev_cfacs_flag, 
                                           adjust_cfacs_flag = global_params$adjust_dev_cfacs_flag, cfac_type = global_params$cfac_type_in_dev_calc, time_horizon_type = 'future',
                                           global_params, dev_vec, decline_rates_initial, time_horizon, yr)  #determine future development parcel attributes
    
    vals_to_match = dev_pool_object$parcel_vals_used

    
    if (all(unlist(vals_to_match) < global_params$offset_thresh)){
      match_object$match_flag = FALSE
      match_object$current_development_object = dev_pool_object
      break
    }
    
    if (global_params$use_offset_bank == FALSE){
      test_ind = list_intersect(current_pool_indexes, current_test_index) #find index that corresponds to potiential development index
      test_ind = test_ind$match_ind
      pool_to_use = current_pool_indexes[-test_ind]       #remove current potiential development index from available list
      vals_to_use = current_pool_vals[-test_ind]
    } else {
      pool_to_use = current_pool_indexes  #if performing offset banking use any of the available banked offset pool
      vals_to_use = current_pool_vals
    }
    
    if (length(pool_to_use) > 0){
      match_object <- select_from_pool(match_type = 'offset', match_procedure = 'euclidean', current_pool = pool_to_use, vals_to_use = vals_to_use, dev_credit, use_parcel_set_dev_credit = global_params$use_parcel_set_dev_credit, 
                                       offset_multiplier = global_params$offset_multiplier, match_threshold = global_params$match_threshold, 
                                       vals_to_match_initial = vals_to_match, global_params$offset_parcel_for_parcel, dims_to_use = global_params$offset_dims, yr) #perform matching routine
    } else{
      match_object$match_flag = FALSE
    }
    
    if (match_object$match_flag == FALSE){
      ind_to_reject = list_intersect(current_match_pool, current_test_index)
      current_match_pool = current_match_pool[-ind_to_reject$match_ind]     #remove current potential development from potential pool
    }
    
  }
  
  if (match_object$match_flag == TRUE){
    match_object$current_development_object = dev_pool_object
    if (length(unlist(match_object$match_indexes) >0 )){
      offset_object = select_current_subset(offset_pool_object, match_object$match_indexes)
      offset_object$parcel_vals_used = offset_pool_object$parcel_vals_used[offset_object$subset_pool]
      match_object$offset_object = offset_object
    } else{
      match_object$offset_object = list()
    }
  } else if (match_object$match_flag == FALSE){
    match_object$offset_object = list()
  } 
  
  match_object$current_development_object$dev_credit = match_object$dev_credit
  match_object$current_development_object$match_flag = match_object$match_flag
  
  return(match_object)
  
}




# develop_from_credit(current_ecology, dev_credit, global_params, dev_vec = developments_object$dev_vec, ind_available = index_object$ind_available[[region_ind]], decline_rates_initial, 
#                     land_parcels = parcels$land_parcels, yr, time_horizon)

develop_from_credit <- function(current_ecology, dev_credit, global_params, dev_vec, ind_available, decline_rates_initial, land_parcels, yr, time_horizon){
  
  offset_multiplier = 1/global_params$offset_multiplier #as the development vals are being matched to the credited offset val, yields inverse offset multiplier

  parcel_num_remaining = length(ind_available)
  
  dev_pool_object <- record_current_parcel_set(current_ecology[ind_available], ind_available, parcel_num_remaining, yr)
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object, pool_type = 'devs', calc_type = global_params$dev_calc_type, cfacs_flag = global_params$dev_cfacs_flag, 
                                         adjust_cfacs_flag = global_params$adjust_dev_cfacs_flag, cfac_type = global_params$cfac_type_in_dev_calc, time_horizon_type = 'future',
                                         global_params, dev_vec, decline_rates_initial, time_horizon, yr)

  match_object <- select_from_pool(match_type = 'development', match_procedure = 'random', current_pool = unlist(dev_pool_object$parcel_indexes), vals_to_use = unlist(dev_pool_object$parcel_vals_used), dev_credit, 
                                   use_parcel_set_dev_credit = FALSE, offset_multiplier, match_threshold = global_params$match_threshold, vals_to_match_initial = dev_credit, 
                                   parcel_for_parcel = TRUE, global_params$eco_dims, yr)
  
  
  if (match_object$match_flag == TRUE){
    development_object = select_current_subset(dev_pool_object, match_object$match_indexes)
    development_object$parcel_vals_used = dev_pool_object$parcel_vals_used[development_object$subset_pool]
    development_object$dev_credit = match_object$dev_credit
  } else{
    development_object = list()
    development_object$dev_credit = dev_credit
  }
  development_object$match_flag = match_object$match_flag
  
  return(development_object)
  
}


# 
# 
# 
# predict_parcel_vals_multi <- function(predict_type, parcel_eco_vals, parcel_indexes, decline_rates, restoration_rate, min_eco_val, max_eco_val, eco_dims, time_horizons){
#   
#   if (class(time_horizons) == 'list'){
#     time_horizons = unlist(time_horizons)
#   }
#   
#   parcel_num = length(parcel_indexes)
#   predicted_parcel_vals = vector('list', parcel_num)
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     current_parcel_ind = parcel_indexes[parcel_count_ind]
#     current_predicted_parcel_vals = array(0, c(1, eco_dims))
#     for (eco_ind in seq_len(eco_dims)){
#       current_decline_rates = decline_rates[, , eco_ind] 
#       
#       if (predict_type == 'protect'){
#         decline_rate = current_decline_rates[current_parcel_ind]      
#       } else if (predict_type == 'restore'){
#         decline_rate = restoration_rate
#       }
#       #predicted_parcel = sapply(parcel_eco_vals[[parcel_count_ind]][[eco_ind]], project_ecology, min_eco_val, max_eco_val, decline_rate, time_horizons[parcel_count_ind], time_fill = FALSE)
#       predicted_parcel = sapply(parcel_eco_vals[[parcel_count_ind]], project_ecology, min_eco_val, max_eco_val, decline_rate, time_horizons[parcel_count_ind], time_fill = FALSE)
#       current_predicted_parcel_vals[1, eco_ind] = sum(predicted_parcel)
#     }
#     predicted_parcel_vals[[parcel_count_ind]] = current_predicted_parcel_vals
#     
#   }
#   return(predicted_parcel_vals)
#   
# }

# 
# record_parcel_info <- function(parcel_sums_at_offset, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr, parcel_num_remaining){
#   
#   parcel_set_object = list()
#   parcel_set_object$offset_yrs = yr
#   parcel_set_object$parcel_ecologies = parcel_eco_vals
#   parcel_set_object$parcel_sums_at_offset = parcel_sums_at_offset
#   parcel_set_object$parcel_indexes = parcel_indexes
#   parcel_set_object$parcel_vals_used = parcel_vals_used
#   parcel_set_object$parcel_num_remaining = parcel_num_remaining
#   #   if (record_type == 'offset'){
#   #     parcel_set_object$adjusted_cfac_trajs = adjusted_counter_trajs
#   #   }
#   return(parcel_set_object)
#   
# }



# evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, pool_object$restoration_vals, pool_object$cfac_vals)
# 
# parcel_sums_at_offset = pool_object$parcel_sums_at_offset
# restoration_vals = pool_object$restoration_vals
# cfac_vals = pool_object$cfac_vals

evaluate_parcel_vals <- function(calc_type, parcel_sums_at_offset, restoration_vals, cfac_vals){
  
  if (calc_type == 'current_condition'){
    parcel_vals_pool = parcel_sums_at_offset
  } else if (calc_type == 'restoration_gains'){
    parcel_vals_pool = subtract_nested_lists(restoration_vals, parcel_sums_at_offset)
  } else if (calc_type == 'restoration_from_cfac'){
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

sum_nested_lists <- function(list_a, list_b, inner_dim){
  summed_list = lapply(seq_along(list_a), function(i) mapply('+', list_a[[i]], list_b[[i]], SIMPLIFY = FALSE))
  return(summed_list)
}

subtract_lists <- function(list_a, list_b){
  list_c = mapply('-', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}

sum_lists <- function(list_a, list_b){
  list_c = mapply('+', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}


record_current_parcel_set <- function(current_ecologies, current_pool, parcel_num_remaining, yr){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = rep(list(yr), length(current_pool))
  parcel_set_object$parcel_ecologies = current_ecologies
  parcel_set_object$parcel_sums_at_offset = find_current_parcel_sums(current_ecologies, global_params$eco_dims)
  parcel_set_object$parcel_indexes = current_pool
  parcel_set_object$parcel_num_remaining = rep(list(parcel_num_remaining), length(current_pool))
  
  return(parcel_set_object)
  
}


# evaluate_parcel_pool <- function(calc_type, cfac_type, current_pool, parcel_num_remaining, decline_rates, global_params, dev_vec, 
#                                  land_parcels, current_ecology, time_horizons, yr){
#   
#   parcel_num = length(current_pool)
#   parcel_pool_object <- record_current_parcel_set(land_parcels, current_ecology, current_pool, parcel_num_remaining, yr)
#   
#   restoration_flag = ( (calc_type == 'restoration_gains') || (calc_type == 'restoration_from_cfac') || (calc_type == 'restoration_condition_value'))
#   if (restoration_flag == TRUE){
#     restoration_vals = predict_parcel_vals_multi(predict_type = 'restore', parcel_pool_object$parcel_ecologies, parcel_indexes = current_pool, decline_rates, global_params$restoration_rate, 
#                                                  global_params$min_eco_val, global_params$max_eco_val, eco_dims = global_params$eco_dims, time_horizons)
#   } else{
#     restoration_vals = list()
#   }
#   
#   if ((calc_type == 'avoided_degs') || (calc_type == 'restoration_from_cfac') || (calc_type == 'future_condition') ){
#     cfac_flag = TRUE
#   } else {cfac_flag = FALSE
#   }
#   
#   parcel_pool$cfac_flag = cfac_flag
#   
#   if (cfac_flag == TRUE){
#     cfac_trajs = list()
#     cfacs_standard = build_cfacs_list(global_params, decline_rates_initial, parcel_ecologies = parcel_pool_object$parcel_ecologies, 
#                                       parcel_indexes = current_pool, time_horizons)
#     cfac_trajs$standard = sum_trajectories_as_list(cfacs_standard, eco_dims = global_params$eco_dims)
#     
#     if (adjust_cfacs_flag == TRUE){
#       cfacs_adjusted = adjust_cfacs(current_cfacs = cfacs_standard, current_parcel_ecologies = parcel_pool_object$parcel_ecologies, adjusted_cfac_type = cfac_type, 
#                                     global_params, dev_vec, parcel_num_remaining = parcel_pool_object$parcel_num_remaining, decline_rates, 
#                                     parcel_indexes = current_pool, time_horizons = time_horizons, offset_yrs = parcel_pool_object$offset_yrs)
#       
#       cfac_trajs$adjusted = sum_trajectories_as_list(cfacs_adjusted, eco_dims = global_params$eco_dims)
#       cfac_vals = lapply(cfac_trajs$adjusted, tail, 1) 
#       
#     } else {
#       cfac_vals = last_element_in_list(cfac_trajs$standard) 
#     }
#     
#     parcel_pool$cfac_trajs = cfac_trajs
#     
#   } else {
#     cfac_vals = list()
#   }
#   
#   parcel_vals_pool <- evaluate_parcel_vals(calc_type, parcel_pool_object$parcel_sums_at_offset, restoration_vals, cfac_vals)
#   return(parcel_pool)
#   
# }                 


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




adjust_cfac <- function(cfacs, global_params, dev_vec, parcel_indexes, parcel_num_remaining, time_horizon, yr){
  
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
  
  dev_prob <- find_dev_probability(dev_vec, yr, time_horizon, parcel_num_remaining)
  
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



find_dev_probability <- function(total_dev_vec, offset_yrs, time_horizons, parcel_num, parcel_num_remaining){
  
  dev_probs = list()
  parcel_num_remaining = unlist(parcel_num_remaining)
  offset_yrs = unlist(offset_yrs)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_count_ind]
    offset_yr = offset_yrs[parcel_count_ind]
    current_dev_vec = total_dev_vec[offset_yr:length(total_dev_vec)]
    
     
    if (length(current_dev_vec) < (time_horizon + 1)){
      current_dev_vec = c(current_dev_vec, array(0, ((time_horizon + 1) - length(current_dev_vec))))
    }
    
    current_dev_vec = current_dev_vec[1:(time_horizon + 1)]
    dev_probs[[parcel_count_ind]] = current_dev_vec/parcel_num_remaining[parcel_count_ind]
    
  }
  return(dev_probs)
}



euclidean_norm_match <- function(parcel_vals_pool, vals_to_match){
  
  vals_to_test = matrix(unlist(parcel_vals_pool), nrow = length(parcel_vals_pool), byrow=TRUE)
  match_array = matrix(rep(vals_to_match, length(parcel_vals_pool)), ncol = length(vals_to_match), byrow = TRUE)
  
  err = sqrt(rowSums( (vals_to_test - match_array)^2 ) )
  match_ind = which(err == min(err))
  if (length(match_ind > 1)){
    match_ind = sample(match_ind, 1)
  }
  match_vals = parcel_vals_pool[[match_ind]]
  
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


# 
# select_from_pool(current_pool = pool_to_use, vals_to_use = vals_to_use, offset_multiplier = global_params$offset_multiplier, 
#                  vals_to_match = vals_to_match, parcel_for_parcel, global_params$eco_dims, yr)


# match_object <- select_from_pool(match_type = 'offset', match_procedure = 'euclidean', current_pool = pool_to_use, vals_to_use = vals_to_use, dev_credit, use_parcel_set_dev_credit = global_params$use_parcel_set_dev_credit, 
#                                  offset_multiplier = global_params$offset_multiplier, match_threshold = global_params$match_threshold, 
#                                  vals_to_match_initial = vals_to_match, global_params$offset_parcel_for_parcel, global_params$eco_dims, yr) #perform matching routine


# match_type = 'offset'
# match_procedure = 'euclidean'
# current_pool = pool_to_use
# use_parcel_set_dev_credit = global_params$use_parcel_set_dev_credit
# offset_multiplier = global_params$offset_multiplier
# match_threshold = global_params$match_threshold 
# vals_to_match_initial = vals_to_match
# parcel_for_parcel = global_params$offset_parcel_for_parcel
# eco_dims = global_params$eco_dims
# dims_to_use = global_params$offset_dims
# vals_to_use_initial = vals_to_use

select_from_pool <- function(match_type, match_procedure, current_pool, vals_to_use_initial, dev_credit, use_parcel_set_dev_credit, 
                             offset_multiplier, match_threshold, vals_to_match_initial, parcel_for_parcel, dims_to_use, yr){
  
#   current_pool = unlist(current_pool)        #change current pool into array form
#   vals_to_use = unlist(vals_to_use)         
#   
  thresh = array(match_threshold, dims_to_use)         #create an array of threshold values with length equal to the dimensions to match to
  pool_num = length(current_pool)
  vals_to_match = offset_multiplier*unlist(vals_to_match_initial)
  vals_to_test = matrix(unlist(vals_to_use_initial), nrow = pool_num, byrow=TRUE)
  
  if (use_parcel_set_dev_credit == TRUE){
    vals_to_match = vals_to_match - dev_credit
  }
  
  if (parcel_for_parcel == TRUE){
    match_array = matrix(rep(vals_to_match, pool_num), ncol = dims_to_use, byrow = TRUE)
    thresh_array = matrix(rep(thresh, pool_num), ncol = dims_to_use, byrow = TRUE)
    if (match_type == 'offset'){
      test_array = (match_array - vals_to_test) < thresh_array
    } else if (match_type == 'development'){
      test_array = (match_array - vals_to_test) > thresh_array
    }
    
    inds_to_use = which(apply(test_array, MARGIN = 1, prod) > 0) # test if all dimensions pass threshold test
    
    vals_to_use = vals_to_use_initial[inds_to_use]
    current_pool = current_pool[inds_to_use]
    
    if (all(inds_to_use == FALSE)){
      match_object = list()
      match_object$match_flag = FALSE
      return(match_object)
    } 
  } else if (parcel_for_parcel == FALSE){
    vals_to_use = vals_to_use_initial
    net_offset_val = unlist(nested_list_sum(vals_to_use))
    if (all((vals_to_match - net_offset_val) > match_threshold)){ 
      match_object = list()
      match_object$match_flag = FALSE
      return(match_object)
    } 
    match_vals = list()
    match_indexes = list()
  } 
  
  parcel_vals_pool = vals_to_use
  vals_to_test = matrix(unlist(vals_to_use), nrow = length(vals_to_use), byrow=TRUE)
  match_flag = FALSE
  
  while(match_flag == FALSE){
    if (length(current_pool) == 0){
      break
    }
    
    if (match_procedure == 'euclidean'){
      match_params = euclidean_norm_match(parcel_vals_pool, vals_to_match)
    } else if(match_procedure == 'random'){
      match_params = list()
      match_params$match_vals = sample(parcel_vals_pool, 1)
      match_params$match_ind = which(parcel_vals_pool == match_params$match_vals)
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
    } else {
      match_indexes = current_match_index
      match_vals = current_match_val
    }
    if (match_type == 'offset'){
    match_flag = all(vals_to_match < thresh)
    } else if (match_type == 'development'){
      match_flag = all(vals_to_match > thresh)
    } 
  }
  
  match_object = list()
  match_object$match_indexes = match_indexes
  match_object$match_vals = match_vals
  match_object$match_flag = match_flag
  match_object$dev_credit = vals_to_match * (abs(vals_to_match) > match_threshold)
  match_object$vals_to_match = vals_to_match_initial
  
  return(match_object)
  
}




# update_selected_object <- function(match_object, current_pool, pool_object){
#   
#   match_indexes = match_object$match_indexes
#   parcel_num = length(match_indexes)
#   parcel_eco_vals = vector('list', parcel_num)
#   parcel_sums_at_offset = vector('list', parcel_num)
#   parcel_vals_used = vector('list', parcel_num)
#   
#   pool_used = list_intersect(current_pool, match_indexes)
#   pool_used = pool_used$match_val
#   
#   for (parcel_count in seq_len(parcel_num)){
#     matched_parcel_ind = pool_used[parcel_count]
#     parcel_eco_vals[[parcel_count]] = pool_object$parcel_eco_vals[[matched_parcel_ind]]
#     parcel_sums_at_offset[[parcel_count]] = pool_object$parcel_sums_at_offset[matched_parcel_ind, ]
#     parcel_vals_used[[parcel_count]] = parcel_vals_pool[matched_parcel_ind, ]
#     
#   }
#   
#   selected_parcel_object = list()
#   selected_parcel_object <- record_parcel_info(parcel_sums_at_offset, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr)
#   return(selected_parcel_object)
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




find_current_parcel_sums <- function(current_ecologies, eco_dims){
  parcel_num = length(current_ecologies)
  parcel_sums = vector('list', parcel_num)
  for (parcel_count_ind in seq_len(parcel_num)){
      parcel_sums[[parcel_count_ind]] = lapply(seq_along(current_ecologies[[parcel_count_ind]]), 
                                                        function(i) sum(current_ecologies[[parcel_count_ind]][[i]]) )
  }
  return(parcel_sums)
}


# find_current_parcel_sums <- function(land_parcels, current_ecology, parcel_indexes, eco_dims){
#   
#   parcel_sums_object = list()
#   parcel_ecologies = vector('list', length(parcel_indexes))
#   parcel_sums = vector('list', length(parcel_indexes))
#   
#   if (class(parcel_indexes) == 'list'){
#     parcel_indexes = unlist(parcel_indexes)
#   }
#   
#   for (parcel_count_ind in seq_len(length(parcel_indexes))){
#     current_parcel_ind = parcel_indexes[parcel_count_ind] 
#     current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
#     current_parcel_ecology = extract_parcel(current_parcel, current_ecology)
#     parcel_sums[[parcel_count_ind]] = sum(current_parcel_ecology)
#     parcel_ecologies[[parcel_count_ind]] = current_parcel_ecology
#   }
#   
#   parcel_sums_object$parcel_ecologies = parcel_ecologies
#   parcel_sums_object$parcel_sums = parcel_sums
#   return(parcel_sums_object)
#   
# }


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




update_ind_available <- function(update_type, index_object, parcel_indexes, region_ind){
  
  ind_available = index_object$ind_available[[region_ind]]
  ind_available = setdiff(ind_available, parcel_indexes) #remove parcel from available list   
  
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


#decline_rates <- update_decline_rates(decline_rates, global_params, decline_rate_type = 'offset', current_offset_indexes) # set elements in decline rates array corresponding to offsets to restoration rates

update_decline_rates <- function(decline_rates, global_params, decline_rate_type, offset_action_type, parcel_indexes){
  
  restoration_rate = global_params$restoration_rate
  
  offset_dims = global_params$offset_dims 
  eco_dims = global_params$eco_dims
  
  parcel_indexes = unlist(parcel_indexes)
  
  for (parcel_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_ind]
    
    if (decline_rate_type == 'development'){
      decline_rates[[current_parcel_ind]] <- rep(list(0), eco_dims)
    } else if (decline_rate_type == 'offset'){
      
      if (offset_action_type == 'maintain'){
        offset_rate = rep(list(1), eco_dims)  
      } else if (offset_action_type == 'restore'){
        offset_rate = rep(list(restoration_rate), eco_dims)
      } else if (offset_action_type == 'protect'){
        offset_rate = decline_rates[[current_parcel_ind]]
      }
      
      if (offset_dims == 1){
        decline_rates[[current_parcel_ind]][[1]] = offset_rate[[1]]
      } else {
        decline_rates[[current_parcel_ind]] = offset_rate
      }
    }
  }
  
  return(decline_rates)
  
}



find_current_dev_nums <- function(current_dev_vec, region_num, yr){
  
  dev_nums = array(0, region_num)
  for (region_ind in seq_len(region_num)){
    current_dev_num = current_dev_vec[yr]
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




select_current_subset <- function(pool_object, current_pool){
  subset_pool =  list_intersect(pool_object$parcel_indexes, current_pool)
  subset_pool = subset_pool$match_ind
  
  subset_pool_object = list()
  subset_pool_object$parcel_indexes <- pool_object$parcel_indexes[subset_pool]
  
  parcel_count = length(subset_pool)
  subset_pool_object$parcel_count = parcel_count
  subset_pool_object$offset_yrs = pool_object$offset_yrs[subset_pool]
  subset_pool_object$parcel_ecologies = pool_object$parcel_ecologies[subset_pool]
  subset_pool_object$parcel_sums_at_offset = pool_object$parcel_sums_at_offset[subset_pool]
  subset_pool_object$parcel_num_remaining = pool_object$parcel_num_remaining[subset_pool]
  subset_pool_object$subset_pool = subset_pool
  return(subset_pool_object)
  
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

# 
# generate_time_horizons <- function(use_offset_bank, time_horizon_type, yr, offset_yrs, time_horizon, parcel_count){
#   if (use_offset_bank == TRUE){
#     time_horizons = rep(yr, parcel_count)
#     time_horizons = time_horizons - offset_yrs
#     if (time_horizon_type == 'future'){
#       time_horizons = time_horizons + time_horizon
#     }
#     
#   } else{
#     time_horizons = rep(time_horizon, parcel_count)
#   }
#   return(time_horizons)
# }



prepare_offset_bank <- function(banked_offsets_object, current_offset_pool, restoration_flag, land_parcels, current_ecologies, eco_dims){
  
  pool_object <- select_current_subset(banked_offsets_object, current_pool = current_offset_pool)
  if (global_params$offset_restoration_flag == TRUE){
    current_sums_object <- find_current_parcel_sums(current_ecologies, global_params$eco_dims)
    pool_object$restoration_vals <- current_sums_object$parcel_sums
  }
  
  return(pool_object)
}


# assess_current_pool(pool_object = offset_pool_object, pool_type = offset_pool_type, calc_type = global_params$offset_calc_type, cfacs_flag = global_params$offset_cfacs_flag, 
#                     adjust_cfacs_flag = global_params$adjust_offset_cfacs_flag, cfac_type = global_params$cfac_type_in_offset_calc, time_horizon_type = global_params$offset_time_horizon_type,
#                     global_params, dev_vec = developments_object$dev_vec, decline_rates_initial, time_horizon, yr)      #determine available parcel values, depending on what particular offset policy is in use using counterfactuals etc.
# 
# pool_object = offset_pool_object
# pool_type = offset_pool_type
# calc_type = global_params$offset_calc_type
# cfacs_flag = global_params$offset_cfacs_flag
# adjust_cfacs_flag = global_params$adjust_offset_cfacs_flag
# cfac_type = global_params$cfac_type_in_offset_calc
# time_horizon_type = global_params$offset_time_horizon_type

# assess_current_pool(pool_object = dev_pool_object, pool_type = 'devs', calc_type = global_params$dev_calc_type, cfacs_flag = global_params$dev_cfacs_flag, 
#                     adjust_cfacs_flag = global_params$adjust_dev_cfacs_flag, cfac_type = global_params$cfac_type_in_dev_calc, time_horizon_type = 'future',
#                     global_params, dev_vec, decline_rates_initial, time_horizon, yr)  #determine future development parcel attributes

# pool_object = dev_pool_object
# pool_type = 'devs'
# calc_type = global_params$dev_calc_type
# cfacs_flag = global_params$dev_cfacs_flag
# adjust_cfacs_flag = global_params$adjust_dev_cfacs_flag
# cfac_type = global_params$cfac_type_in_dev_calc
# time_horizon_type = global_params$offset_time_horizon_type


assess_current_pool <- function(pool_object, pool_type, calc_type, cfacs_flag, adjust_cfacs_flag, cfac_type, time_horizon_type, 
                                global_params, dev_vec, decline_rates, time_horizon, yr){
  
  current_pool = unlist(pool_object$parcel_indexes)
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  selected_decline_rates = decline_rates[current_pool]
  
  if ( (pool_type == 'offsets') || (pool_type == 'offset_bank')){
    
    if (pool_type == 'offsets'){
      if (global_params$offset_calc_type == 'current_condition'){
        project_type = 'current'
      } else {
        project_type = 'future'
      }
    } else if (pool_type == 'offset_bank'){
      project_type = 'current'
    }
    
    time_horizons <- generate_time_horizons(time_horizon_type = pool_type, project_type, yr, offset_yrs, time_horizon, parcel_count)
    
    if (global_params$offset_restoration_flag == TRUE){
      restoration_vals = calc_parcel_trajs(parcel_ecologies = pool_object$parcel_ecologies, parcel_traj_type = 'restore', selected_decline_rates, time_horizons, global_params, time_fill = FALSE)
      pool_object$restoration_vals = lapply(seq_along(restoration_vals), function(i) lapply(seq_along(restoration_vals[[i]]), function(j) sum(restoration_vals[[i]][[j]] )))
    } else {
      pool_object$restoration_vals = list()
    }
    
  } else if (pool_type == 'devs'){
    if (global_params$dev_calc_type == 'current_condition'){
      project_type = 'current'
    } else {
      project_type = 'future'
    }
    time_horizons <- generate_time_horizons(time_horizon_type = pool_type, project_type, yr, offset_yrs, time_horizon, parcel_count)
    pool_object$restoration_vals = list()
  }
  
  if (cfacs_flag == TRUE){
    
    cfacs_object = calc_cfacs(parcel_ecologies = pool_object$parcel_ecologies, parcel_num_remaining = pool_object$parcel_num_remaining, 
                              global_params, dev_vec, decline_rates = selected_decline_rates, time_horizons, offset_yrs, cfac_type, adjust_cfacs_flag)
                              
    if (adjust_cfacs_flag == TRUE){
      cfac_trajs = sum_trajectories(cfacs_object$adjusted_cfacs, eco_dims = global_params$eco_dims)
    } else {
      cfac_trajs = sum_trajectories(cfacs_object$cfacs, eco_dims = global_params$eco_dims)
    }
    
    pool_object$cfac_vals = nested_list_tail(cfac_trajs)
    
  }
  
  pool_object$parcel_vals_used = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, pool_object$restoration_vals, pool_object$cfac_vals)
  
  return(pool_object)
}


write_current_parcel_set <- function(parcel_set_object, current_parcel_set_object, parcel_set_count){
  
  if (length(current_parcel_set_object) > 0){
    parcel_set_object$offset_yrs[[parcel_set_count]] = current_parcel_set_object$offset_yrs
    parcel_set_object$parcel_ecologies[[parcel_set_count]] = current_parcel_set_object$parcel_ecologies
    parcel_set_object$parcel_sums_at_offset[[parcel_set_count]] = current_parcel_set_object$parcel_sums_at_offset
    parcel_set_object$parcel_indexes[[parcel_set_count]] = current_parcel_set_object$parcel_indexes
    parcel_set_object$parcel_vals_used[[parcel_set_count]] = current_parcel_set_object$parcel_vals_used
    parcel_set_object$parcel_num_remaining[[parcel_set_count]] = current_parcel_set_object$parcel_num_remaining
  }
#   } else {
#     parcel_set_object$offset_yrs[[parcel_set_count]] = list()
#     parcel_set_object$parcel_ecologies[[parcel_set_count]] = list()
#     parcel_set_object$parcel_sums_at_offset[[parcel_set_count]] = list()
#     parcel_set_object$parcel_indexes[[parcel_set_count]] = list()
#     parcel_set_object$parcel_vals_used[[parcel_set_count]] = list()
#     parcel_set_object$parcel_num_remaining[[parcel_set_count]] = list()
#   }
  return(parcel_set_object)
}

  
initialise_parcel_set_object <- function(){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = list()
  parcel_set_object$parcel_ecologies = list()
  parcel_set_object$parcel_sums_at_offset = list()
  parcel_set_object$parcel_indexes = list()
  parcel_set_object$parcel_vals_used = list()
  return(parcel_set_object)
  
}

initialise_developments_object <- function(dev_vec){
  developments_object <- initialise_parcel_set_object()
  developments_object$dev_vec <- dev_vec
  return(developments_object)
}
select_banked_offset_indexes <- function(offset_bank_num, ind_available){
  
  parcels_to_bank = sample(ind_available, offset_bank_num)
  return(parcels_to_bank)
}



initialise_banked_offsets_object <- function(global_params, banked_offset_vec){
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
    banked_offsets_object$banked_offset_vec = banked_offset_vec
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



select_land_parcel <- function(land_parcels, current_parcel_ind){
  selected_land_parcel = land_parcels[[current_parcel_ind]]
  return(selected_land_parcel)
}



project_current_system_multi <- function(current_ecology, decline_rates, min_eco_val, max_eco_val, max_restoration_eco_val, time_horizon, eco_dims){
  
  parcel_num = length(current_ecology)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_ecology = current_ecology[[parcel_ind]][[eco_ind]] #select current ecological dimension to work on
      current_decline_rate = decline_rates[[parcel_ind]][[eco_ind]] #select current decline rates to work on
      if (current_decline_rate == 0){
        updated_parcel_ecology = array(0, length(current_parcel_ecology))   # if the parcel is to be developed (i.e. decline rate = 0), set parcel value to zeros
      } else if (current_decline_rate == 1){
        updated_parcel_ecology = current_parcel_ecology      # if the parcel is to be maintained (i.e. decline rate = 1), set parcel values to current values
      } else { 
        if (current_decline_rate < 0){
          updated_parcel_ecology = sapply(current_parcel_ecology, project_ecology, min_eco_val = min_eco_val, 
                                          max_eco_val = max_eco_val, decline_rate = current_decline_rate, time_horizon = time_horizon, time_fill = FALSE)
        } else if (current_decline_rate > 0){
          updated_parcel_ecology = sapply(current_parcel_ecology, project_ecology, min_eco_val = min_eco_val, 
                                          max_eco_val = max_eco_val, decline_rate = current_decline_rate, time_horizon = time_horizon, time_fill = FALSE)
        }
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

# 
# parcel_indexes = pool_object$parcel_indexes
# parcel_ecologies = pool_object$parcel_ecologies
# parcel_num_remaining = pool_object$parcel_num_remaining


# parcel_indexes = parcel_set_indexes
# parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE) 
# parcel_num_remaining = current_model_outputs$parcel_num_remaining 
# offset_yrs = current_model_outputs$offset_yrs

# calc_cfacs(parcel_indexes = parcel_set_indexes, parcel_ecologies = unlist(current_model_outputs$parcel_ecologies, recursive = FALSE), 
#            parcel_num_remaining = current_model_outputs$parcel_num_remaining, global_params, dev_vec,
#            decline_rates_initial, time_horizons, offset_yrs = current_model_outputs$offset_yrs, cfac_type, adjust_cfacs_flag)



# calc_cfacs(parcel_ecologies = pool_object$parcel_ecologies, parcel_num_remaining = pool_object$parcel_num_remaining, global_params, dev_vec, decline_rates_initial, 
#            time_horizons, offset_yrs, cfac_type, adjust_cfacs_flag)
# 
# 
# parcel_ecologies = pool_object$parcel_ecologies
# parcel_num_remaining = pool_object$parcel_num_remaining
# decline_rates = selected_decline_rates


calc_cfacs <- function(parcel_ecologies, parcel_num_remaining, global_params, dev_vec, decline_rates, time_horizons, offset_yrs, cfac_type, adjust_cfacs_flag){
  
  cfacs_object = list()
  if (length(decline_rates) != length(parcel_ecologies)){
    print('length error')
  }
  
  current_cfacs = calc_parcel_trajs(parcel_ecologies, parcel_traj_type = 'protect', decline_rates, time_horizons, global_params, time_fill = TRUE)
  if (adjust_cfacs_flag == TRUE){
    adjusted_cfacs = adjust_cfacs(current_cfacs, adjusted_cfac_type = cfac_type, global_params, dev_vec, 
                                  parcel_num_remaining, decline_rates, time_horizons, offset_yrs)
    cfacs_object$adjusted_cfacs = adjusted_cfacs
  }
  cfacs_object$cfacs = current_cfacs
  return(cfacs_object)
  
}  



# adjusted_cfac_type ='include_clearing_offsets'
# eco_dims = plot_params$eco_dims 
# parcel_num_remaining = plot_params$parcel_num_remaining
# parcel_num = length(plot_params$parcel_indexes) 
# offset_yrs = plot_params$offset_yr
# parcel_indexes = 1


# adjust_cfacs(current_cfacs, current_parcel_ecologies = parcel_ecologies, adjusted_cfac_type = cfac_type, global_params, dev_vec, 
#              parcel_num_remaining, decline_rates_initial, parcel_indexes, time_horizons, offset_yrs)



# current_parcel_ecologies = parcel_ecologies
# adjusted_cfac_type = cfac_type
          

adjust_cfacs <- function(current_cfacs, adjusted_cfac_type, global_params, dev_vec, parcel_num_remaining, decline_rates, time_horizons, offset_yrs){

  time_horizons = unlist(time_horizons)
  weighted_counters_object <- find_weighted_counters(current_cfacs, adjusted_cfac_type, global_params$eco_dims, dev_vec, parcel_num_remaining, time_horizons, offset_yrs)
  
  if (length(decline_rates) != length(current_cfacs)){
    print('length error')
  }
  
  if (adjusted_cfac_type == 'include_clearing'){
    adjusted_cfacs = weighted_counters_object$weighted_counters
  } else if (adjusted_cfac_type == 'include_clearing_offsets'){
    offset_projections <- calc_offset_projections(current_cfacs, weighted_counters_object$offset_probs, global_params, decline_rates, time_horizons)
    summed_offset_projections <- sum_offset_projs(offset_projections, offset_probs = weighted_counters_object$offset_probs, global_params$eco_dims, time_horizons)
    adjusted_cfacs = sum_clearing_offsets(weighted_counters_object$weighted_counters, summed_offset_projections, global_params$eco_dims)
  }
  
  return(adjusted_cfacs)
}



remove_neg_probs <- function(weight_list, inds_to_accept){
    weight_list <- lapply(seq_along(weight_list), function(i) weight_list[[i]]*inds_to_accept[[i]])
  return(weight_list)
}


#find_weighted_counters(current_cfacs, adjusted_cfac_type, global_params$eco_dims, dev_vec, parcel_num_remaining, parcel_num = length(parcel_indexes), time_horizons, offset_yrs)

find_weighted_counters <- function(current_cfacs, adjusted_cfac_type, eco_dims, dev_vec, parcel_num_remaining, time_horizons, offset_yrs){
  parcel_num = length(current_cfacs)
  dev_probs <- find_dev_probability(total_dev_vec = dev_vec, offset_yrs, time_horizons, parcel_num, parcel_num_remaining)
  
  if (adjusted_cfac_type == 'include_clearing'){
    
    dev_weights <- lapply(dev_probs, cumsum)
    counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - dev_weights[[i]])
    
  } else if (adjusted_cfac_type == 'include_clearing_offsets'){
    
    dev_weights <- lapply(dev_probs, cumsum)
    offset_probs <- dev_probs
    offset_weights <- lapply(offset_probs, cumsum)
    counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - (dev_weights[[i]] + offset_weights[[i]]))

  }
  
  inds_to_accept = lapply(seq_along(counter_weights), function(i) counter_weights[[i]] >= 0)
  dev_probs <- remove_neg_probs(dev_probs, inds_to_accept)
  counter_weights <- remove_neg_probs(counter_weights, inds_to_accept) 
  
  weighted_counters_object = list()
  weighted_counters_object$weighted_counters = weight_counters(current_cfacs, eco_dims, counter_weights)
  weighted_counters_object$dev_probs = dev_probs
  if (adjusted_cfac_type == 'include_clearing_offsets'){
    offset_probs <- remove_neg_probs(offset_probs, inds_to_accept)
    weighted_counters_object$offset_probs = offset_probs
  }
  return(weighted_counters_object)
}




# find_weight_array <- function(current_counter_weight, projected_dims){
#   counter_weight_array = rep(current_counter_weight, projected_dims[1]*projected_dims[2])
#   dim(counter_weight_array) = c(length(current_counter_weight), c(projected_dims[2], projected_dims[1]))
#   counter_weight_array = aperm(counter_weight_array, c(3, 2, 1))
#   return(counter_weight_array)
# }

find_weight_array <- function(current_counter_weight, projected_dims){
  counter_weight_array = matrix(rep(current_counter_weight, projected_dims[2]), nrow = projected_dims[1], byrow = TRUE)
  return(counter_weight_array)
}

weight_counters <- function(current_cfacs, eco_dims, counter_weights){
  
  parcel_num = length(current_cfacs)
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


#offset_probs = weighted_counters_object$offset_probs

calc_offset_projections <- function(current_cfacs, offset_probs, global_params, decline_rates, time_horizons){
  
  if (length(decline_rates) != length(current_cfacs)){
    print('length error')
  }
  parcel_num = length(current_cfacs)
  eco_dims = global_params$eco_dims
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
          
          current_offset_proj = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = global_params$offset_action_type, 
                                                    global_params, current_dec_rate, (time_horizon - proj_yr), time_fill = TRUE)

          current_offset_projections[[eco_ind]][[proj_yr]][proj_yr:time_horizon, ] = current_offset_proj[[1]]
        }
      }
    }
    offset_projections[[parcel_count_ind]] = current_offset_projections
  }
  
  return(offset_projections)
  
}

# 
#sum_offset_projs(offset_projections, offset_probs = weighted_counters_object$offset_probs, global_params$eco_dims, parcel_num = length(parcel_indexes), time_horizons)

# 
# offset_probs = weighted_counters_object$offset_probs
# eco_dims = global_params$eco_dims
# parcel_num = length(parcel_indexes)

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




# calc_offset_projections <- function(current_parcel_ecologies, offset_probs, global_params, decline_rates, parcel_indexes, time_horizons){
#   
#   if (class(parcel_indexes) == 'list'){
#     parcel_indexes = unlist(parcel_indexes)
#   }
#   parcel_num = length(parcel_indexes)
#   eco_dims = global_params$eco_dims
#   offset_projections = vector('list', parcel_num)
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     
#     time_horizon = time_horizons[parcel_count_ind]
#     current_parcel_ind = parcel_indexes[parcel_count_ind]
#     current_offset_prob = offset_probs[[parcel_count_ind]]
#     
#     offset_proj_yrs = which(current_offset_prob > 0)
#     offset_proj_num = length(offset_proj_yrs)
#     
#     current_parcel_ecology = current_parcel_ecologies[[parcel_count_ind]]
#     
#     current_offset_projections = vector('list', offset_proj_num)
#     
#     for (proj_ind in seq_len(offset_proj_num)){
#       proj_yr = offset_proj_yrs[proj_ind]
#       current_time_horizon = time_horizon - proj_yr + 1
#       current_offset_projections[[proj_ind]] = predict_parcel_traj(current_parcel_ecology, current_parcel_ind, parcel_traj_type = global_params$offset_action_type, 
#                                                                    global_params, decline_rates, time_horizon = current_time_horizon)
#     }
#     
#     offset_projections[[parcel_count_ind]] = current_offset_projections
#     
#   }
#   
#   return(offset_projections)
# }
# 



#   
# 
# sum_offset_projs <- function(offset_projections, current_parcel_ecologies, offset_probs, eco_dims, parcel_num, time_horizons){
#   
#   summed_offset_projections = vector('list', parcel_num)
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     
#     time_horizon = time_horizons[parcel_count_ind]
#     current_offset_prob = offset_probs[[parcel_count_ind]]
#     
#     offset_proj_yrs = which(current_offset_prob > 0)
#     offset_proj_num = length(offset_proj_yrs)
#     current_parcel_ecology = current_parcel_ecologies[[parcel_count_ind]]
#     current_summed_offset_projections = vector('list', eco_dims)
#     
#     for (eco_ind in seq_len(eco_dims)){
#       projected_dims = c(dim(current_parcel_ecology)[1:2], length(current_offset_prob))
#       current_summed_offset_projections[[eco_ind]] = array(0, projected_dims)
#     }
#     
#     for (proj_ind in seq_len(offset_proj_num)){
#       proj_yr = offset_proj_yrs[proj_ind]
#       
#       current_offset_proj = offset_projections[[parcel_count_ind]][[offset_proj_num]]
#       
#       for (eco_ind in seq_len(eco_dims)){
#         current_summed_offset_projections[[eco_ind]][, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[[eco_ind]][, , proj_yr:(time_horizon + 1)] + 
#           as.vector(current_offset_proj[[eco_ind]]*current_offset_prob[proj_yr])
#       }
#       
#     }
#     
#     summed_offset_projections[[parcel_count_ind]] = current_summed_offset_projections
#     
#   }
#   
#   return(summed_offset_projections)
# }
# 






# 
# adjust_cfacs <- function(current_cfacs, current_parcel_ecologies, adjusted_cfac_type, global_params, dev_vec, parcel_num_remaining, decline_rates, parcel_indexes, 
#                          time_horizons, offset_yrs){
#   
#   if (class(time_horizons) == 'list'){
#     time_horizons = unlist(time_horizons)
#   }
#   dev_probs <- find_dev_probability(total_dev_vec = dev_vec, offset_yrs, time_horizons, parcel_num = length(parcel_indexes), parcel_num_remaining)
#   
#   if (adjusted_cfac_type == 'include_clearing'){
#     dev_weights <- lapply(dev_probs, cumsum)
#     counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - dev_weights[[i]])
#     weighted_counters = weight_counters(current_cfacs, global_params$eco_dims, counter_weights)
#     adjusted_cfacs = weighted_counters
#     
#   } else if (adjusted_cfac_type == 'include_clearing_offsets'){
#     
#     dev_weights <- lapply(dev_probs, cumsum)
#     
#     offset_probs <- dev_probs
#     offset_weights <- lapply(offset_probs, cumsum)
#     
#     counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - (dev_weights[[i]] + offset_weights[[i]]))
#     weighted_counters = weight_counters(current_cfacs, global_params$eco_dims, counter_weights)
#     
#     offset_projections <- calc_offset_projections(current_parcel_ecologies, offset_probs, global_params, decline_rates, parcel_indexes, time_horizons)
#     summed_offset_projections <- sum_offset_projections(offset_projections, current_parcel_ecologies, offset_probs, eco_dims, parcel_num, time_horizons)
#     
#     adjusted_cfacs = sum_clearing_offsets(weighted_counters, summed_offset_projections, global_params$eco_dims)
#   }
#   
#   return(adjusted_cfacs)
#   
# }



# 
# adjust_cfacs <- function(current_cfacs, current_parcel_ecologies, adjusted_cfac_type, global_params, dev_vec, parcel_num_remaining, decline_rates, parcel_indexes, 
#                          time_horizons, offset_yrs){
#   
#   if (class(time_horizons) == 'list'){
#     time_horizons = unlist(time_horizons)
#   }
#   dev_probs <- find_dev_probability(total_dev_vec = dev_vec, offset_yrs, time_horizons, parcel_num = length(parcel_indexes), parcel_num_remaining)
#   
#   if (adjusted_cfac_type == 'include_clearing'){
#     dev_weights <- lapply(dev_probs, cumsum)
#     counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - dev_weights[[i]])
#     adjusted_cfacs = weight_counters(current_cfacs, global_params$eco_dims, counter_weights)
#     
#   } else if (adjusted_cfac_type == 'include_clearing_offsets'){
#     
#     dev_weights <- lapply(dev_probs, cumsum)
#     
#     offset_probs <- dev_probs
#     offset_weights <- lapply(offset_probs, cumsum)
#     
#     counter_weights <- lapply(seq_along(dev_weights), function(i) 1 - (dev_weights[[i]] + offset_weights[[i]]))
#     weighted_counters = weight_counters(current_cfacs, global_params$eco_dims, counter_weights)
#     
#     summed_offset_projections = sum_offset_projections(current_parcel_ecologies, offset_probs, global_params, decline_rates, parcel_indexes, time_horizons)
#     
#     adjusted_cfacs = sum_clearing_offsets(weighted_counters, summed_offset_projections, global_params$eco_dims)
#   }
#   
#   return(adjusted_cfacs)
#   
# }

# 
# sum_offset_projections <- function(current_parcel_ecologies, offset_probs, global_params, decline_rates, parcel_indexes, time_horizons){
#   
#   if (class(parcel_indexes) == 'list'){
#     parcel_indexes = unlist(parcel_indexes)
#   }
#   parcel_num = length(parcel_indexes)
#   eco_dims = global_params$eco_dims
#   summed_offset_projections = vector('list', parcel_num)
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     
#     time_horizon = time_horizons[parcel_count_ind]
#     current_parcel_ind = parcel_indexes[parcel_count_ind]
#     current_offset_prob = offset_probs[[parcel_count_ind]]
#     
#     offset_proj_yrs = which(current_offset_prob > 0)
#     offset_proj_num = length(offset_proj_yrs)
#     
#     current_parcel_ecology = current_parcel_ecologies[[parcel_count_ind]]
#     
#     current_summed_offset_projections = vector('list', eco_dims)
#     for (eco_ind in seq_len(eco_dims)){
#       projected_dims = c(dim(current_parcel_ecology)[1:2], length(current_offset_prob))
#       current_summed_offset_projections[[eco_ind]] = array(0, projected_dims)
#     }
#     
#     for (proj_ind in seq_len(offset_proj_num)){
#       proj_yr = offset_proj_yrs[proj_ind]
#       current_time_horizon = time_horizon - proj_yr + 1
#       current_offset_proj = predict_parcel_traj(current_parcel_ecology, current_parcel_ind, parcel_traj_type = global_params$offset_action_type, 
#                                                 global_params, decline_rates, time_horizon = current_time_horizon)
#       current_projected_dims = c(dim(current_parcel_ecology)[1:2], current_time_horizon + 1)
#       #offset_weight_array = array(current_offset_prob[proj_yr], current_projected_dims)
#       
#       for (eco_ind in seq_len(eco_dims)){
#         current_summed_offset_projections[[eco_ind]][, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[[eco_ind]][, , proj_yr:(time_horizon + 1)] + 
#           as.vector(current_offset_proj[[eco_ind]]*current_offset_prob[proj_yr])
#       }
#       
#     }
#     
#     summed_offset_projections[[parcel_count_ind]] = current_summed_offset_projections
#     
#   }
#   
#   return(summed_offset_projections)
# }



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
  landscape = list_of_arrays(list_dims = eco_dims, array_dims = landscape_dims)
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

# form_net_trajectory <- function(trajectories_list, land_parcels, global_params, eco_dims){
#   size_x = global_params$ecology_size
#   size_y = global_params$ecology_size
#   net_trajectories = vector('list', global_params$eco_dims)
#   
#   for (eco_ind in seq_len(eco_dims)){
#     current_trajectory = array(0, c(size_y, size_x, global_params$time_steps))
#     for (parcel_ind in seq_along(trajectories_list)){
#       current_parcel = land_parcels[[parcel_ind]]
#       current_trajectory = insert_parcel_trajectory(current_trajectory, current_parcel, trajectories_list[[parcel_ind]][[eco_ind]])
#     }
#     net_trajectories[[eco_ind]] = current_trajectory
#   }
#   return(net_trajectories)
# }

# 
# trajectories_list = realisations[[1]]$trajectories
# land_parcels= parcels$land_parcels 
# time_steps = global_params$time_steps
# landscape_dims = parcels$landscape_dims
# eco_dims = global_params$eco_dims

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




# sum_parcel_trajectories <- function(traj_list, eco_dims, parcel_indexes, time_steps){
#   
#   parcel_num = length(parcel_indexes)
#   parcel_trajs = array(0, c(time_steps, parcel_num, eco_dims))
#   for (parcel_count_ind in seq_len(parcel_num)){
#     parcel_ind = parcel_indexes[parcel_count_ind]
#     for (eco_ind in seq_len(eco_dims)){
#       parcel_trajs[, parcel_count_ind, ] = apply(traj_list[[parcel_ind]][[eco_ind]], MARGIN=3, sum)
#     }  
#   }
#   
#   return(parcel_trajs)
#   
# }


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


# sum_trajectories_as_list <- function(traj_list, eco_dims, parcel_indexes){
#   parcel_indexes = unlist(parcel_indexes)
#   parcel_num = length(parcel_indexes)
#   parcel_traj_list = vector('list', parcel_num)
#   for (parcel_count_ind in seq_len(parcel_num)){
#     
#     for (eco_ind in seq_len(eco_dims)){
#       parcel_traj_list[[parcel_count_ind]][[eco_ind]] = apply(traj_list[[parcel_indexes[parcel_count_ind] ]][[eco_ind]], MARGIN=3, sum)
#     }  
#   }
#   
#   return(parcel_traj_list)
#   
# }












