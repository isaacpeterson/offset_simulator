initialise_simulated_ecology_params <- function(){
  
  # Construct the static initial landscape 
  
  simulated_ecology_params = list()
  
  #how many feature layers to generate
  simulated_ecology_params$feature_num = 1
  
  # Number of pixels in (y, x) for the feature layes 
  simulated_ecology_params$ecology_size = c(300, 400)
  
  # Numnber of parcels in x (but total size varies)
  simulated_ecology_params$parcel_num_x = 30 
  
  # Numnber of parcels in y (but total size varies)
  simulated_ecology_params$parcel_num_y = 40 
  
  # Minimum allowable initial ecological value of smallest ecological element
  # (pixel) ie min value to sample from
  simulated_ecology_params$min_initial_eco_val = 20
  
  # Max allowable initial ecological value of largest element (pixel) ie max
  # value to sample from
  simulated_ecology_params$max_initial_eco_val = 90
  
  # Mow much initial variation in pixels per land parcel (this is the width of
  # uniform dist) used to add noise to each pixel. Eg if the pixel has a vlaue
  # of 35, a new value will be sampled from between 35-45
  simulated_ecology_params$initial_eco_noise = 10
  
  # Defining multiple regions eg different states where different polcies can apply 
  simulated_ecology_params$region_num_x = 1
  
  # Defining multiple regions eg different states where different rules can apply 
  simulated_ecology_params$region_num_y = 1
  
  return(simulated_ecology_params)
}





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

prepare_simulated_data <- function(run_params){
  simulated_ecology_params <- initialise_simulated_ecology_params()
  LGA_array <- simulate_LGA(simulated_ecology_params)
  parcels <- LGA_to_parcel_list(LGA_array)
  parcel_ecology <- simulate_ecology(simulated_ecology_params, land_parcels = parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
  landscape_ecology = list()
  dev_weights = list() 
  
  save_simulation_inputs(run_params$simulation_inputs_folder, LGA_array, parcels, landscape_ecology,
                         parcel_ecology, dev_weights)
  if (run_params$backup_simulation_inputs == TRUE){
    save_simulation_inputs(run_params$simulation_params_folder, LGA_array, parcels, landscape_ecology,
                           parcel_ecology, dev_weights)
  }
}
