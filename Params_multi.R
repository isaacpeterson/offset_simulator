initialise_global_params <- function(){
  
  global_params = list()
  global_params$eco_dims = 3
  global_params$randomise_dev_nums = FALSE
  global_params$display_object = FALSE
  global_params$time_steps = 100 #number of timesteps simulation will be run
  global_params$ecology_size = 100 #ecology array size (square dimension) to be broken up into regions and land parcels  
  global_params$parcel_size_dist_kind = 'same' #'same' = choose parcels of the same size 
  global_params$region_num_x = 1 #numnber of regions in x
  global_params$region_num_y = 1 #numnber of regions in y
  global_params$region_num = global_params$region_num_x*global_params$region_num_y
  global_params$parcel_num_x = 10 #numnber of parcels in x
  global_params$parcel_num_y = 10 #numnber of parcels in x
  global_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  global_params$max_eco_val = 100 #maximum "   "     "           "
  global_params$min_initial_eco_val = 30 #minimum allowable initial ecological value of smallest ecological element (pixel)
  global_params$max_initial_eco_val = 80 #maximum "   "     "           "
  global_params$initial_eco_noise = 10 #how much initial variation in pixels per land parcel 
  global_params$blur = FALSE
  global_params$max_developments = 1 #maximum number of developments per year 
  global_params$develop_every = 1 #how often the policy is implemented
  return(global_params)
  
}


populate_region_list <- function(parcel_selection_type, offset_calc_type, offset_action_type, apply_offset_to, restoration_rate, offset_multiplier, dev_calc_type, max_region_dev_num, mean_decline_rate){
  region_params = list()
  region_params$restoration_rate = restoration_rate
  region_params$offset_multiplier = offset_multiplier #multiply developed parcel value by this value to determine value to offset
  region_params$parcel_selection_type = parcel_selection_type #'regional' or 'national' - offset into same/any region as developed parcel
  region_params$offset_calc_type = offset_calc_type #'current' ('predicted') - use current (predicted - i.e. the value the parcel would attain if the parcel was offset) value of offset parcel as offset metric
  region_params$offset_action_type = offset_action_type
  region_params$apply_offset_to = apply_offset_to
  region_params$dev_calc_type = dev_calc_type         #'current' - use current value of development parcel to determine offset, 'predicted' - use predicted value (i.e. the value the parcel would attain if the parcel was NOT developed) to determine offset  
  region_params$dev_nums = split_vector(100, 10, sd = 1, min_width = -1)   #number of developments that occur per region per development cycle
  region_params$mean_decline_rate = - mean_decline_rate
  region_params$decline_rate_std = 0.5*abs(mean_decline_rate)
  return(region_params)
}

#offset_calc_type = 'current condition', 'restoration gains', 'avoided degredation', 'restoration from counterfactual'
#dev_calc_type = 'future condition', 'current condition'

initialise_region_params <- function(region_num, eco_dim_num, restoration_rate, mean_decline_rates){
  region_params = vector('list', region_num)
  region_params[[1]] = populate_region_list(parcel_selection_type = 'regional', offset_calc_type = 'restoration from counterfactual', offset_action_type = 'restore', apply_offset_to = 'all',
                                            restoration_rate, offset_multiplier = 1, dev_calc_type = 'future condition', max_region_dev_num = 1, mean_decline_rates[1])
  region_params[[2]] = populate_region_list(parcel_selection_type = 'regional', offset_calc_type = 'restoration from counterfactual', offset_action_type = 'restore', apply_offset_to = 'all',
                                            restoration_rate, offset_multiplier = 1,   dev_calc_type = 'current condition', max_region_dev_num = 1, mean_decline_rates[2])
  
   return(region_params)
}
