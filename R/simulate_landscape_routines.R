#' @export
simulate_site_feature_elements <- function(site_sample_type, current_condition_class_modes, current_condition_class_set, element_num, initial_site_sd, initial_site_mean_sd, unique_site_vals){

  if (current_condition_class_modes == 0){
    site_elements = array(0, element_num)
    return(site_elements)
  }
  current_condition_class_bounds = current_condition_class_set[[current_condition_class_modes]]
  
  if (length(initial_site_sd) == 0){
    initial_site_sd = (current_condition_class_bounds[2] - current_condition_class_bounds[1]) / 3
  }
  
  if (length(initial_site_mean_sd) == 0){
    initial_site_mean_sd = (current_condition_class_bounds[2] - current_condition_class_bounds[1])
  }
  
  if (site_sample_type == 'trunc_norm'){
    site_mean = rtruncnorm(1, a=current_condition_class_bounds[1], b=current_condition_class_bounds[3], mean=current_condition_class_bounds[2], sd = initial_site_mean_sd)
    if (unique_site_vals == TRUE){
      site_elements = rtruncnorm(element_num, a=current_condition_class_bounds[1], b=current_condition_class_bounds[3], mean=site_mean, sd = initial_site_sd)
    } else{
      site_elements = array(site_mean, element_num)
    }
  } else if (site_sample_type == 'uniform'){
    if (unique_site_vals == TRUE){
      site_elements = current_condition_class_bounds[1] + runif(element_num)*array((current_condition_class_bounds[3] - current_condition_class_bounds[1]), element_num)
    } else {
      site_elements = array(1, element_num)*runif(1)*(current_condition_class_bounds[3] - current_condition_class_bounds[1])
    }
  }

  return(site_elements)
}

simulate_feature_layers <- function(feature_params, site_characteristics, simulation_inputs_folder, condition_class_modes){ 
  
  
  for (feature_ind in 1:feature_params$simulated_feature_num){

    current_condition_class_set = feature_params$initial_condition_class_bounds[[feature_ind]]
    current_condition_mode_set = lapply(seq_along(condition_class_modes), function(i) condition_class_modes[[i]][[feature_ind]])
    current_simulated_feature = lapply(seq_along(site_characteristics$land_parcels), function(i) matrix(array(0, length(site_characteristics$land_parcels[[i]])), nrow = 1))
    for (site_ind in seq_along(site_characteristics$land_parcels)){
      
      for (mode_ind in seq_along(unique(current_condition_mode_set[[site_ind]]))){
        current_element_set = which(current_condition_mode_set[[site_ind]] == unique(current_condition_mode_set[[site_ind]])[mode_ind])
        current_simulated_feature[[site_ind]][current_element_set] = simulate_site_feature_elements(feature_params$site_sample_type, 
                                                                                                    unique(current_condition_mode_set[[site_ind]])[mode_ind],
                                                                                                    current_condition_class_set,
                                                                                                    element_num = length(current_element_set),
                                                                                                    feature_params$initial_site_sd, 
                                                                                                    feature_params$initial_site_mean_sd,
                                                                                                    feature_params$unique_site_vals)
      }
    }

#     current_simulated_feature = lapply(seq_along(site_characteristics$land_parcels), 
#                                        function(i) lapply(seq_along(unique(current_condition_mode_set[[i]])), 
#                                                           function(j) simulate_site_feature_elements(feature_params$site_sample_type, 
#                                                                                                      unique(current_condition_mode_set[[i]])[j],
#                                                                                                      current_condition_class_set,
#                                                                                                      element_num = length(which(current_condition_mode_set[[i]] == unique(current_condition_mode_set[[i]])[j])),
#                                                                                                      feature_params$initial_site_sd, 
#                                                                                                      feature_params$initial_site_mean_sd,
#                                                                                                      feature_params$unique_site_vals)))

    current_occupation_ratio = feature_params$occupation_ratio[[feature_ind]]
    
    if (current_occupation_ratio > 0){
      zero_site_inds = which(runif(length(current_simulated_feature)) > current_occupation_ratio)
      current_simulated_feature[zero_site_inds] = lapply(zero_site_inds, function(i) 0*(current_simulated_feature[[i]]))

    }
    
    current_feature_layer = matrix(data = 0, nrow = site_characteristics$landscape_dims[1], ncol = site_characteristics$landscape_dims[2])
    current_feature_layer[unlist(site_characteristics$land_parcels)] = unlist(current_simulated_feature)
    current_feature_raster = raster(current_feature_layer)
    current_file_name = paste0(simulation_inputs_folder, 'feature_', feature_ind, '.tif')
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
    
  }

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

simulate_planning_units <- function(feature_params){
  
  parcel_num_x = feature_params$parcel_num_x   #length in parcels of array in x 
  parcel_num_y = feature_params$parcel_num_y #length in parcels of array in y 
  parcel_vx = split_vector(parcel_num_x, feature_params$landscape_size[2], sd = feature_params$site_width_variation_param, min_width = 1) # make normally distributed vector that sums to landscape size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, feature_params$landscape_size[1], sd = feature_params$site_width_variation_param, min_width = 1) # as above for y
  
  pixel_indexes = 1:(feature_params$landscape_size[1]*feature_params$landscape_size[2])     #index all elements of landscape array
  dim(pixel_indexes) = c(feature_params$landscape_size[1], feature_params$landscape_size[2])  # arrange landscape array index vector into array of landscape dimensions 
  parcels = mcell2(pixel_indexes, parcel_vx, parcel_vy) #split the landscape array into a series of subarrays with dimensions sz_x by sz_y
  
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


# simulate_ecological_dynamics(parcel_num = length(objects_to_save$site_characteristics$land_parcels), 
#                              sample_decline_rate = TRUE, 
#                              mean_decline_rates = feature_params$mean_decline_rates, 
#                              decline_rate_std = feature_params$decline_rate_std)       # set up array of decline rates that are eassociated with each cell

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

construct_simulated_data <- function(feature_params, simulation_inputs_folder, simulation_params_folder, backup_simulation_inputs){

  objects_to_save = list()

  objects_to_save$planning_units_array <- simulate_planning_units(feature_params)
  objects_to_save$site_characteristics <- define_planning_units(objects_to_save$planning_units_array)
  objects_to_save$dev_probability_list = rep(list(1/objects_to_save$site_characteristics$land_parcel_num), objects_to_save$site_characteristics$land_parcel_num)
  objects_to_save$offset_probability_list = objects_to_save$dev_probability_list

  objects_to_save$condition_class_modes = lapply(seq_along(objects_to_save$site_characteristics$land_parcels), 
                                                 function(i) lapply(seq(feature_params$simulated_feature_num), 
                                                                    function(j) sample(seq_along(feature_params$initial_condition_class_bounds[[j]]),
                                                                                       length(objects_to_save$site_characteristics$land_parcels[[i]]),
                                                                                       replace = TRUE)))

  save_simulation_inputs(objects_to_save, simulation_inputs_folder)
  
  simulate_feature_layers(feature_params, objects_to_save$site_characteristics, simulation_inputs_folder, objects_to_save$condition_class_modes) 
  
}
