initialise_common_params <- function(){
  
  global_params = list()
  global_params$time_steps = 50 #number of timesteps simulation will be run
  global_params$total_dev_num = 50
  global_params$dev_start = 10
  global_params$dev_end = global_params$time_steps - 1
  global_params$eco_dims = 1
  global_params$randomise_dev_nums = FALSE
  global_params$display_object = FALSE
  
  global_params$set_seed = FALSE
  global_params$offset_dims = 1
  global_params$match_threshold = 50 # acceptable level above which to accept parcel match
  global_params$ecology_size = 100 #ecology array size (square dimension) to be broken up into regions and land parcels  
  global_params$region_num_x = 1 #numnber of regions in x
  global_params$region_num_y = 1 #numnber of regions in y
  global_params$region_num = global_params$region_num_x*global_params$region_num_y
  global_params$parcel_num_x = 15 #numnber of parcels in x
  global_params$parcel_num_y = 15 #numnber of parcels in x
  global_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  global_params$max_eco_val = 100 #maximum "   "     "           "
  global_params$min_initial_eco_val = 20 #minimum allowable initial ecological value of smallest ecological element (pixel)
  global_params$max_initial_eco_val = 80 #maximum "   "     "           "
  global_params$initial_eco_noise = 10 #how much initial variation in pixels per land parcel 
  global_params$blur = FALSE
  global_params$max_developments = 1 #maximum number of developments per year 
  global_params$develop_every = 1 #how often the policy is implemented
  return(global_params)
  
}
