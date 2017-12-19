
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
