initialise_common_params <- function(){
  
  global_params = list()
  global_params$time_steps = 50 #number of timesteps simulation will be run
  global_params$total_dev_num = 80
  global_params$dev_start = 5         # the year development starts
  global_params$dev_end = global_params$time_steps - 1 # the year development ends
  global_params$eco_dims = 1 #how many dimensions to simulate
  global_params$dims_to_use = seq_len(global_params$eco_dims)  #what dimensions to consider in claculations e.g. keep many layers running while just optimisaing for layer 1 or 2 or 1:2
  global_params$randomise_dev_nums = FALSE
  global_params$display_object = FALSE
  global_params$offset_thresh = 50
  global_params$parcel_size_lim = 50     #smallest allowable parcel size (in number of elements)
  global_params$screen_parcels = TRUE
  global_params$set_seed = FALSE    # run from randomised initial value (if TRUE this allows the same randomised initial values)
  global_params$match_threshold = 0 # acceptable level above which to accept parcel match
  global_params$ecology_size = 200 #ecology array size (square dimension) to be broken up into regions and land parcels  
  global_params$region_num_x = 1 #numnber of regions in x
  global_params$region_num_y = 1 #numnber of regions in y
  global_params$region_num = global_params$region_num_x*global_params$region_num_y
  global_params$parcel_num_x = 30 #numnber of parcels in x
  global_params$parcel_num_y = 30 #numnber of parcels in x
  global_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  global_params$max_eco_val = 100 #maximum "   "     "           "
  global_params$max_restoration_eco_val = 70
  global_params$min_initial_eco_val = 20 #minimum allowable initial ecological value of smallest ecological element (pixel)
  global_params$max_initial_eco_val = 100 #maximum "   "     "           "
  global_params$initial_eco_noise = 10 #how much initial variation in pixels per land parcel 
  global_params$blur = FALSE

  return(global_params)
  
}



initialise_ecology <- function(global_params, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
  
  parcel_num = length(land_parcels)
  eco_dims = global_params$eco_dims
  initial_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = global_params$eco_dims)
  
  for (parcel_ind in seq_len(parcel_num)){  
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (eco_ind in seq_len(eco_dims)){
      mean_eco_val = global_params$min_initial_eco_val + (global_params$max_initial_eco_val - global_params$min_initial_eco_val - global_params$initial_eco_noise)*runif(1)
      initial_ecology[[parcel_ind]][[eco_ind]] =  array(mean_eco_val, parcel_dims) + global_params$initial_eco_noise*array(runif(length(current_parcel)), c(parcel_dims))
    }
    
  }

  return(initial_ecology)
}



  
# initialise_ecology <- function(global_params, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
#   initial_ecology = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$eco_dims))     #initialise with array of zeros of landscape dimensions, and number of ecological dimension deep
#   
#   for (eco_ind in seq_len(global_params$eco_dims)){     #generate initial ecology for each ecological dimension
#     initial_ecology[, , eco_ind]  = initialise_ecology_slice(global_params, land_parcels)
#   }
#   
#   return(initial_ecology)
# }
# 
# 
# initialise_ecology_slice <- function(global_params, land_parcels){ #generate initial "slice" of ecology to be used for each separate ecological dimension
#   
#   land_parcel_num = length(land_parcels)
#   initial_ecology = matrix(1,global_params$ecology_size,global_params$ecology_size)     #initialise ecology array into unitary array
#   
#   for (parcel_ind in seq_len(land_parcel_num)){   #loop through land parcels assigning the same (randomly distributed) value to all elements within land parcel - with max and min values controlled by user controlled max/min eco_vals
#     initial_parcel_value = global_params$min_initial_eco_val + (global_params$max_initial_eco_val - global_params$min_initial_eco_val - global_params$initial_eco_noise)*runif(1) #determine scaled random multiplier
#     current_parcel = select_land_parcel(land_parcels, parcel_ind)
#     initial_ecology[current_parcel] = initial_ecology[current_parcel]*initial_parcel_value #multiply all elements of current land parcel by scaled random multiplier
#   }
#   initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size), global_params$ecology_size,global_params$ecology_size) #add noise to ecology, yielding land parcels with variation across parcel
#   return(initial_ecology)
# }
