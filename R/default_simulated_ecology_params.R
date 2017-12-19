initialise_default_simulated_ecology_params <- function(){
  
  # Construct the static initial landscape 
  
  default_simulated_ecology_params = list()
  
  #how many feature layers to generate
  default_simulated_ecology_params$feature_num = 1
  
  # Number of pixels in (y, x) for the feature layes 
  default_simulated_ecology_params$ecology_size = c(300, 400)
  
  # Numnber of parcels in x (but total size varies)
  default_simulated_ecology_params$parcel_num_x = 30 
  
  # Numnber of parcels in y (but total size varies)
  default_simulated_ecology_params$parcel_num_y = 40 
  
  # Minimum allowable initial ecological value of smallest ecological element
  # (pixel) ie min value to sample from
  default_simulated_ecology_params$min_initial_eco_val = 20
  
  # Max allowable initial ecological value of largest element (pixel) ie max
  # value to sample from
  default_simulated_ecology_params$max_initial_eco_val = 90
  
  # Mow much initial variation in pixels per land parcel (this is the width of
  # uniform dist) used to add noise to each pixel. Eg if the pixel has a vlaue
  # of 35, a new value will be sampled from between 35-45
  default_simulated_ecology_params$initial_eco_noise = 10
  
  # Defining multiple regions eg different states where different polcies can apply 
  default_simulated_ecology_params$region_num_x = 1
  
  # Defining multiple regions eg different states where different rules can apply 
  default_simulated_ecology_params$region_num_y = 1
  
  return(default_simulated_ecology_params)
}