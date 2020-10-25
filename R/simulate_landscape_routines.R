construct_simulated_data <- function(feature_params, simulation_inputs_folder, simulation_params_folder, backup_simulation_inputs){
  
  objects_to_save = list()
  if (simulation_inputs_folder == 'default'){
    simulation_inputs_folder = 'simulation_inputs/'
  }
  
  planning_units_raster <- simulate_feature_raster(feature_params$feature_layer_size, feature_params$site_num_characteristics)
  
  current_file_name = paste0(simulation_inputs_folder, 'planning_units.tif')
  print(paste('writing administrative areas raster to', current_file_name))
  writeRaster(planning_units_raster, current_file_name, overwrite = TRUE)
  simulate_feature_layers(feature_params, simulation_inputs_folder) 
  
}

#' @export
simulate_site_feature_elements <- function(site_sample_type, current_condition_class_mode, current_condition_class_characteristics, element_num, initial_site_sd, initial_site_mean_sd){

  if (current_condition_class_mode == 0){
    site_elements = array(0, element_num)
    return(site_elements)
  } else {
    current_condition_class_characteristics = current_condition_class_characteristics[current_condition_class_mode, ]
  }
  
  if (site_sample_type == 'trunc_norm'){
    site_mean = rtruncnorm(1, a=current_condition_class_characteristics$lower_bound, b=current_condition_class_characteristics$upper_bound, mean=(current_condition_class_characteristics$upper_bound - current_condition_class_characteristics$lower_bound), sd = initial_site_mean_sd)

    site_elements = rtruncnorm(element_num, a=current_condition_class_characteristics$lower_bound, b=current_condition_class_characteristics$upper_bound, mean=site_mean, sd = initial_site_sd)

  } else if (site_sample_type == 'uniform'){

      site_elements = current_condition_class_characteristics$lower_bound + runif(element_num)*array((current_condition_class_characteristics$upper_bound - current_condition_class_characteristics$lower_bound), element_num)

  }

  return(site_elements)
}

simulate_feature_layers <- function(feature_params, simulation_inputs_folder){ 
  
  for (feature_ind in 1:feature_params$simulated_feature_num){
    
    feature_ID_raster <- simulate_feature_raster(feature_params$feature_layer_size, feature_params$feature_num_characteristics)

    feature_characteristics <- build_ID_characteristics(feature_ID_raster)
    
    current_condition_class_set = which(!is.na(match(feature_params$condition_class_characteristics$feature, feature_ind)))
    current_condition_class_characteristics = feature_params$condition_class_characteristics[match(feature_ind, feature_params$condition_class_characteristics$feature), ]
    
    condition_class_modes = lapply(seq_along(feature_characteristics$cell_id_groups), 
                                   function(i) rep(sample(seq(nrow(current_condition_class_characteristics)), 1), length(feature_characteristics$cell_id_groups[[i]])))
    
    current_simulated_feature = lapply(seq_along(feature_characteristics$cell_id_groups), function(i) matrix(array(0, length(feature_characteristics$cell_id_groups[[i]])), nrow = 1))
    
    for (site_ind in seq_along(feature_characteristics$cell_id_groups)){
      
      for (mode_ind in seq_along(unique(condition_class_modes[[site_ind]]))){
        
        current_element_set = which(condition_class_modes[[site_ind]] == unique(condition_class_modes[[site_ind]])[mode_ind])
        
        current_simulated_feature[[site_ind]][current_element_set] = simulate_site_feature_elements(feature_params$site_sample_type, 
                                                                                                    unique(condition_class_modes[[site_ind]])[mode_ind],
                                                                                                    current_condition_class_characteristics,
                                                                                                    element_num = length(current_element_set),
                                                                                                    feature_params$initial_site_sd, 
                                                                                                    feature_params$initial_site_mean_sd)
      }
      
    }

    current_occupation_ratio = feature_params$occupation_ratio[[feature_ind]]

    if (current_occupation_ratio > 0){
      
      zero_site_inds = which(runif(length(current_simulated_feature)) > current_occupation_ratio)
      current_simulated_feature[zero_site_inds] = lapply(zero_site_inds, function(i) 0*(current_simulated_feature[[i]]))

    }
    
    current_feature_layer = matrix(data = 0, nrow = feature_characteristics$landscape_dims[1], ncol = feature_characteristics$landscape_dims[2])
    current_feature_layer[unlist(feature_characteristics$cell_id_groups)] = unlist(current_simulated_feature)
    
    current_feature_raster = raster(current_feature_layer)
    current_file_name = paste0(simulation_inputs_folder, 'feature_vals_', 
                               formatC(feature_ind, width = 3, format = "d", flag = "0"), '.tif')
    print(paste('writing simulated raster to', current_file_name))
    writeRaster(current_feature_raster, current_file_name, overwrite = TRUE)
    
    current_file_name = paste0(simulation_inputs_folder, 'feature_IDs_', 
                               formatC(feature_ind, width = 3, format = "d", flag = "0"), '.tif')
    
    feature_ID_raster[which(getValues(current_feature_raster == 0))] = 0
    writeRaster(feature_ID_raster, current_file_name, overwrite = TRUE)
    
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

simulate_feature_raster <- function(feature_layer_size, feature_num_characteristics){

  site_vy = split_vector(feature_num_characteristics[1], feature_layer_size[1], feature_num_characteristics[3], min_width = 1) # as above for y
  site_vx = split_vector(feature_num_characteristics[2], feature_layer_size[2], feature_num_characteristics[3], min_width = 1) # make normally distributed vector that sums to landscape size, composed of n elements where n is the parcel dimension in x
  
  element_inds = seq(feature_layer_size[1]*feature_layer_size[2])     #index all elements of landscape array
  dim(element_inds) = c(feature_layer_size[1], feature_layer_size[2])  # arrange landscape array index vector into array of landscape dimensions 
  parcels = mcell2(element_inds, site_vx, site_vy) #split the landscape array into a series of subarrays with dimensions sz_x by sz_y
  
  site_list = lapply(seq_along(parcels), function(i) array(i, dim(parcels[[i]])))
  feature_raster = array(0, dim(element_inds))
  feature_raster[unlist(parcels)] = unlist(site_list)
  feature_raster = raster(feature_raster)
  return(feature_raster)
}



