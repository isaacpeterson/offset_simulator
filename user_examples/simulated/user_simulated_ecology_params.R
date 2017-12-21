initialise_simulated_ecology_params <- function(){
  
  # Construct the static initial landscape 
  
  simulated_ecology_params = list()
  
  simulated_ecology_params$include_zeros = FALSE
  
  #how many feature layers to generate
  simulated_ecology_params$feature_num = 1
  
  # Number of pixels in (y, x) for the feature layes 
  simulated_ecology_params$ecology_size = c(300, 500)
  
  # Numnber of parcels in x (but total size varies)
  simulated_ecology_params$parcel_num_x = 50 
  
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