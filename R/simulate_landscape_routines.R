
simulate_ecology_feature <- function(min_initial_eco_val, max_initial_eco_val, initial_eco_noise, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
  
  eco_scale = (max_initial_eco_val - min_initial_eco_val - initial_eco_noise)
  
  current_ecology = lapply(seq_along(land_parcels), function(i) min_initial_eco_val + eco_scale*runif(1)*array(1, length(land_parcels[[i]] )))
  current_ecology_noise = lapply(seq_along(land_parcels), function(i) initial_eco_noise*array( runif( length( land_parcels[[i]] ) ), length(land_parcels[[i]] )))
  current_ecology = mapply('+', current_ecology, current_ecology_noise, SIMPLIFY = FALSE)
  
  return(current_ecology)
  
}

simulate_ecology <- function(simulated_ecology_params, land_parcels){ 
  
  for (eco_ind in 1:simulated_ecology_params$feature_num){
    current_simulated_ecology <- simulate_ecology_feature(simulated_ecology_params$min_initial_eco_val, 
                                                          simulated_ecology_params$max_initial_eco_val, 
                                                          simulated_ecology_params$initial_eco_noise, 
                                                          land_parcels)
    current_simulated_ecology <- lapply(seq_along(current_simulated_ecology), function(i) list(current_simulated_ecology[[i]]))
    if (eco_ind == 1){
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

simulate_LGA <- function(simulated_ecology_params){
  
  parcel_num_x = simulated_ecology_params$parcel_num_x   #length in parcels of array in x 
  parcel_num_y = simulated_ecology_params$parcel_num_y #length in parcels of array in y 
  parcel_vx = split_vector(parcel_num_x, simulated_ecology_params$ecology_size[2], sd = 5, min_width = 3) # make normally distributed vector that sums to ecology size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, simulated_ecology_params$ecology_size[1], sd = 5, min_width = 3) # as above for y
  
  pixel_indexes = 1:(simulated_ecology_params$ecology_size[1]*simulated_ecology_params$ecology_size[2])     #index all elements of ecology array
  dim(pixel_indexes) = c(simulated_ecology_params$ecology_size[1], simulated_ecology_params$ecology_size[2])  # arrange ecology array index vector into array of landscape dimensions 
  parcels = mcell2(pixel_indexes, parcel_vx, parcel_vy) #split the ecology array into a series of subarrays with dimensions sz_x by sz_y
  
  parcel_list = lapply(seq_along(parcels), function(i) array(i, dim(parcels[[i]])))
  parcel_array = array(0, dim(pixel_indexes))
  parcel_array[unlist(parcels)] = unlist(parcel_list)
  
  region_vx = split_vector(simulated_ecology_params$region_num_x, parcel_num_x, 1, min_width = 3) # perform similar operation used to split array into smallest elements, but this time for land parcels, arranging into regions
  region_vy = split_vector(simulated_ecology_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  parcel_indexes = seq(length(parcel_list))
  dim(parcel_indexes) = c(region_vy, region_vx)
  
  
  return(parcel_array)
}


construct_simulated_data <- function(simulated_ecology_params, simulation_inputs_folder, simulation_params_folder, backup_simulation_inputs){

  objects_to_save = list()

  objects_to_save$simulated_ecology_params <- simulated_ecology_params
  
  objects_to_save$LGA_array <- simulate_LGA(objects_to_save$simulated_ecology_params)
  objects_to_save$parcels <- LGA_to_parcel_list(objects_to_save$LGA_array)
  objects_to_save$parcel_ecology <- simulate_ecology(objects_to_save$simulated_ecology_params, land_parcels = objects_to_save$parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
  
  objects_to_save$dev_weights = list(1/length(objects_to_save$parcel_ecology), length(objects_to_save$parcel_ecology)) 
  objects_to_save$offset_weights = objects_to_save$dev_weights
  
  save_simulation_inputs(objects_to_save, simulation_inputs_folder)

  if (backup_simulation_inputs == TRUE){
    save_simulation_inputs(objects_to_save, simulation_params_folder)
  }
  
}
