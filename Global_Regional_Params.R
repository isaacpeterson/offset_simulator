initialise_global_params <- function(){
  
  global_params = list()
  global_params$eco_dims = 1
  global_params$randomise_dev_nums = FALSE
  global_params$display_object = FALSE
  global_params$offset_time_horizon = 20
  global_params$total_dev_num = 10
  global_params$use_offset_time_horizon = TRUE
  global_params$use_adjusted_counterfactual = FALSE
  global_params$calc_adjusted_counterfactual = TRUE
  global_params$include_offsets_in_adjusted_counterfactual = FALSE
  global_params$apply_offset_to = 'singular'
  global_params$time_steps = 50 #number of timesteps simulation will be run
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
  global_params$dev_vec = find_development_vector(global_params$time_steps, global_params$total_dev_num, sd = 1, min_width = -1)   #number of developments that occur per region per development cycle
  global_params$blur = FALSE
  global_params$max_developments = 1 #maximum number of developments per year 
  global_params$develop_every = 1 #how often the policy is implemented
  global_params$restoration_rate = 0.03
  return(global_params)
  
}


populate_region_list <- function(parcel_selection_type, offset_calc_type, offset_action_type, apply_offset_to,
                                 offset_parcel_for_parcel, offset_multiplier, dev_calc_type, max_region_dev_num, mean_decline_rate){
  region_params = list()
  region_params$offset_multiplier = offset_multiplier #multiply developed parcel value by this value to determine value to offset
  region_params$parcel_selection_type = parcel_selection_type #'regional' or 'national' - offset into same/any region as developed parcel
  region_params$offset_calc_type = offset_calc_type #'current' ('predicted') - use current (predicted - i.e. the value the parcel would attain if the parcel was offset) value of offset parcel as offset metric
  region_params$offset_action_type = offset_action_type
  region_params$apply_offset_to = apply_offset_to
  region_params$offset_parcel_for_parcel = offset_parcel_for_parcel
  region_params$dev_calc_type = dev_calc_type         #'current' - use current value of development parcel to determine offset, 'predicted' - use predicted value (i.e. the value the parcel would attain if the parcel was NOT developed) to determine offset  
  region_params$mean_decline_rate = - mean_decline_rate
  region_params$decline_rate_std = 0.5*abs(mean_decline_rate)
  return(region_params)
}

#offset_calc_type = 'avoided degredation', 'restoration from counterfactual', 'absolute counterfactual', 'restoration gains', 'absolute restoration'
#dev_calc_type = 'future condition', 'current condition'

initialise_region_params <- function(region_num, mean_decline_rates, offset_calc_type){
  region_params = vector('list', region_num)
  region_params[[1]] = populate_region_list(parcel_selection_type = 'regional', offset_calc_type, offset_action_type = 'restore', apply_offset_to = 'all',
                                            offset_parcel_for_parcel = TRUE, offset_multiplier = 1, 
                                            dev_calc_type = 'current condition', max_region_dev_num = 1, mean_decline_rates[1])
  return(region_params)
}
