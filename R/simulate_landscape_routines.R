
simulate_ecology_feature <- function(min_initial_eco_val, max_initial_eco_val, initial_eco_noise, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
  
  eco_scale = (max_initial_eco_val - min_initial_eco_val - initial_eco_noise)
  
  current_ecology = lapply(seq_along(land_parcels), function(i) min_initial_eco_val + eco_scale*runif(1)*array(1, length(land_parcels[[i]] )))
  current_ecology_noise = lapply(seq_along(land_parcels), function(i) initial_eco_noise*array( runif( length( land_parcels[[i]] ) ), length(land_parcels[[i]] )))
  current_ecology = mapply('+', current_ecology, current_ecology_noise, SIMPLIFY = FALSE)
  
  return(current_ecology)
  
}

simulate_ecology <- function(simulated_ecology_params, land_parcels){ 
  
  for (feature_ind in 1:simulated_ecology_params$feature_num){
    current_simulated_ecology <- simulate_ecology_feature(simulated_ecology_params$min_initial_eco_val, 
                                                          simulated_ecology_params$max_initial_eco_val, 
                                                          simulated_ecology_params$initial_eco_noise, 
                                                          land_parcels)
    current_occupation_ratio = simulated_ecology_params$occupation_ratio[[feature_ind]]
    
    if (current_occupation_ratio > 0){
      zero_site_inds = which(runif(length(current_simulated_ecology)) > current_occupation_ratio)
      current_simulated_ecology[zero_site_inds] = lapply(zero_site_inds, function(i) 0*(current_simulated_ecology[[i]]))
    }
    
    current_simulated_ecology <- lapply(seq_along(current_simulated_ecology), function(i) list(current_simulated_ecology[[i]]))
    
    if (feature_ind == 1){
      simulated_ecology <- current_simulated_ecology
    } else {
      simulated_ecology <- lapply(seq_along(land_parcels), function(j) append(simulated_ecology[[j]], current_simulated_ecology[[j]]))
    }
    
  }
  return(simulated_ecology)
}

mcell2 <- function(Arr_in, vx, vy){       #used to break up array into samller set of sub arrays defined by vx and vy that fit together to give input array
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  output_list = vector('list', rows*cols)   # make an array composed of lists with dimenisons that define the land parcels/regions. The list format allows arrays of different sizes to be stored
  colStart = 0
  for (i in seq_len(cols)){       # run down through the columns of input array 
    rowStart = 0
    for (j in seq_len(rows)){ #group elements of input array into sub arrays and assign to output_list
      output_list[[a]] = Arr_in[rowStart+(1:rowsizes[j]), colStart+(1:colsizes[i])]
      rowStart = rowStart + rowsizes[j]
      a = a + 1
    }
    colStart = colStart + colsizes[i]
  }
  
  return(output_list)
  
}  

simulate_planning_units <- function(simulated_ecology_params){
  
  parcel_num_x = simulated_ecology_params$parcel_num_x   #length in parcels of array in x 
  parcel_num_y = simulated_ecology_params$parcel_num_y #length in parcels of array in y 
  parcel_vx = split_vector(parcel_num_x, simulated_ecology_params$ecology_size[2], sd = simulated_ecology_params$site_width_variation_param, min_width = 1) # make normally distributed vector that sums to ecology size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, simulated_ecology_params$ecology_size[1], sd = simulated_ecology_params$site_width_variation_param, min_width = 1) # as above for y
  
  pixel_indexes = 1:(simulated_ecology_params$ecology_size[1]*simulated_ecology_params$ecology_size[2])     #index all elements of ecology array
  dim(pixel_indexes) = c(simulated_ecology_params$ecology_size[1], simulated_ecology_params$ecology_size[2])  # arrange ecology array index vector into array of landscape dimensions 
  parcels = mcell2(pixel_indexes, parcel_vx, parcel_vy) #split the ecology array into a series of subarrays with dimensions sz_x by sz_y
  
  parcel_list = lapply(seq_along(parcels), function(i) array(i, dim(parcels[[i]])))
  parcel_array = array(0, dim(pixel_indexes))
  parcel_array[unlist(parcels)] = unlist(parcel_list)
  
  return(parcel_array)
}




log_proj <- function(parcel_vals, min_eco_val, max_eco_val, current_dec_rate, time_vec){
  
  t_sh = -1/current_dec_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
  
  eco_projected = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-current_dec_rate*(time_vec - t_sh)))
  
  return(eco_projected)
  
}


# simulate_ecological_dynamics(parcel_num = length(objects_to_save$parcels$land_parcels), 
#                              sample_decline_rate = TRUE, 
#                              mean_decline_rates = simulated_ecology_params$mean_decline_rates, 
#                              decline_rate_std = simulated_ecology_params$decline_rate_std)       # set up array of decline rates that are eassociated with each cell

# simulate_dynamics <- function(sample_decline_rate, parcel_num, initial_val, mean_decline_rates, decline_rate_std, min_eco_val, max_eco_val, time_vec){
#   
#   feature_num = length(mean_decline_rates)
#   if (sample_decline_rate == TRUE){
#     # sample change rate from normal distribution
#     decline_rates = lapply(seq(parcel_num), function(i) lapply(seq(feature_num),
#                                                                function(j) rnorm(1, mean_decline_rates[[j]], decline_rate_std[[j]])))
#   } else {
#     # copy same rate to all sites
#     decline_rates = lapply(seq(parcel_num), function(i) lapply(seq(feature_num),
#                                                                function(j) mean_decline_rates[[j]]))
#   }
#   
#   feature_dynamics = lapply(seq_along(decline_rates), 
#                             function(i) lapply(seq_along(decline_rates[[i]]), function(j) log_proj(parcel_vals = initial_val,
#                                                                                            min_eco_val, 
#                                                                                            max_eco_val,  
#                                                                                            current_dec_rate = decline_rates[[i]][[j]], 
#                                                                                            time_vec)))
#   return(feature_dynamics)
# }

construct_simulated_data <- function(simulated_ecology_params, simulation_inputs_folder, simulation_params_folder, backup_simulation_inputs){

  objects_to_save = list()

  objects_to_save$simulated_ecology_params <- simulated_ecology_params
  
  objects_to_save$planning_units_array <- simulate_planning_units(simulated_ecology_params)
  objects_to_save$parcels <- define_planning_units(objects_to_save$planning_units_array)
  objects_to_save$parcel_ecology <- simulate_ecology(simulated_ecology_params, land_parcels = objects_to_save$parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
  
  parcel_num = length(objects_to_save$parcels$land_parcels)
  objects_to_save$dev_probability_list = rep(list(1/parcel_num), parcel_num)
  objects_to_save$offset_probability_list = objects_to_save$dev_probability_list
  
#   objects_to_save$background_dynamics <- simulate_dynamics(sample_decline_rate = simulated_ecology_params$sample_decline_rate,
#                                                         parcel_num, 
#                                                         initial_val = simulated_ecology_params$max_initial_eco_val, 
#                                                         mean_decline_rates = simulated_ecology_params$mean_decline_rate, 
#                                                         decline_rate_std = simulated_ecology_params$decline_rate_std, 
#                                                         min_eco_val = simulated_ecology_params$local_min_eco_val, 
#                                                         max_eco_val = simulated_ecology_params$local_max_eco_val, 
#                                                         time_vec = simulated_ecology_params$simulated_time_vec)
#   
#   objects_to_save$management_dynamics = simulate_dynamics(sample_decline_rate = simulated_ecology_params$sample_decline_rate,
#                                                            parcel_num = parcel_num, 
#                                                            initial_val = simulated_ecology_params$min_initial_eco_val, 
#                                                            mean_decline_rates = simulated_ecology_params$restoration_rate, 
#                                                            decline_rate_std = simulated_ecology_params$decline_rate_std, 
#                                                            min_eco_val = simulated_ecology_params$local_min_eco_val, 
#                                                            max_eco_val = simulated_ecology_params$local_max_eco_val, 
#                                                            time_vec = simulated_ecology_params$simulated_time_vec)
  
#  objects_to_save$management_mode = lapply(seq(parcel_num), function(i) rep(list(0), simulated_ecology_params$feature_num))
    
  save_simulation_inputs(objects_to_save, simulation_inputs_folder)

  if (backup_simulation_inputs == TRUE){
    save_simulation_inputs(objects_to_save, simulation_params_folder)
  }
  
}
