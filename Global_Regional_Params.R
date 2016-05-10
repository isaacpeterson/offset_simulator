initialise_global_params <- function(match_type, use_offset_bank, offset_time_horizon_type,  offset_calc_type, dev_calc_type, offset_action_type, cfac_type_in_offset_calc, offset_time_horizon){
  
  global_params = list()
  global_params$match_type = match_type
  global_params$offset_time_horizon_type = offset_time_horizon_type
  global_params$offset_parcel_for_parcel = FALSE
  global_params$record_parcel_sets = TRUE
  global_params$use_offset_bank = use_offset_bank
  
  global_params$time_steps = 50 #number of timesteps simulation will be run
  global_params$total_dev_num = 50
  dev_start = 10
  dev_end = global_params$time_steps - 1
  global_params$offset_bank_num = 30
  banked_offset_start = 1
  banked_offset_end = 40
  
  global_params$offset_multiplier = 1
  global_params$eco_dims = 1
  global_params$randomise_dev_nums = FALSE
  global_params$display_object = FALSE
  if ((offset_calc_type == 'current_condition') && (dev_calc_type == 'current_condition')){
    global_params$use_offset_time_horizon = FALSE
  } else {global_params$use_offset_time_horizon = TRUE}
  
  if( (offset_calc_type == 'avoided_degredation') || (offset_calc_type == 'restoration_from_cfac') || (offset_calc_type == 'future_condition') ){
    global_params$offset_cfacs_flag = TRUE
  } else{
    global_params$offset_cfacs_flag = FALSE
  }
  
  if( (offset_calc_type == 'restoration_gains') || (offset_calc_type == 'restoration_from_cfac') || (offset_calc_type == 'restoration_condition_value') ){
    global_params$offset_restoration_flag = TRUE
  } else {
    global_params$offset_restoration_flag = FALSE
  }
  
  if( (dev_calc_type == 'future_condition')){
    global_params$dev_cfacs_flag = TRUE
  } else{
    global_params$dev_cfacs_flag = FALSE
  }
  
  global_params$set_seed = FALSE
  global_params$offset_dims = 1
  
  global_params$ecology_size = 100 #ecology array size (square dimension) to be broken up into regions and land parcels  
  global_params$parcel_size_dist_kind = 'same' #'same' = choose parcels of the same size 
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
  global_params$restoration_rate = 0.03
  
  global_params$dev_vec = find_prog_vector(time_steps = global_params$time_steps, prog_start = dev_start, prog_end = dev_end, 
                                           total_prog_num = global_params$total_dev_num, sd = 1)
  
   if (global_params$use_offset_bank == TRUE){
    global_params$banked_offset_vec = find_prog_vector(time_steps = global_params$time_steps, prog_start = banked_offset_start, prog_end = banked_offset_end, total_prog_num = global_params$offset_bank_num, sd = 2)
  } else {
    global_params$banked_offset_vec = array(0, global_params$time_steps)
  }

  global_params$offset_calc_type = offset_calc_type
  global_params$dev_calc_type = dev_calc_type
  global_params$offset_action_type = offset_action_type
  global_params$cfac_type_in_offset_calc = cfac_type_in_offset_calc
  global_params$cfac_type_in_dev_calc = cfac_type_in_offset_calc
    
  global_params$offset_time_horizon = offset_time_horizon
  global_params$adjust_cfacs_flag = ((cfac_type_in_offset_calc == 'include_clearing') || (cfac_type_in_offset_calc == 'include_clearing_offsets'))
  
  global_params$post_facto_cfac_type = 'include_clearing_offsets'
  global_params$adjust_counters_post_facto = ((global_params$post_facto_cfac_type == 'include_clearing') 
                                              || (global_params$post_facto_cfac_type == 'include_clearing_offsets'))

  
  return(global_params)
  
}


populate_region_list <- function(offset_parcel_selection_type, offset_parcel_for_parcel){
  current_region_params = list()
    current_region_params$offset_parcel_selection_type = offset_parcel_selection_type #'regional' or 'national' - offset into same/any region as developed parcel
  current_region_params$offset_parcel_for_parcel = offset_parcel_for_parcel
  current_region_params$dev_calc_type = dev_calc_type         #'current' - use current value of development parcel to determine offset, 'predicted' - use predicted value (i.e. the value the parcel would attain if the parcel was NOT developed) to determine offset  
  return(current_region_params)
  
}




# initialise_region_params <- function(region_num, offset_parcel_selection_type, mean_decline_rates, offset_calc_type, dev_calc_type, offset_action_type, offset_parcel_for_parcel){
#   region_params = vector('list', region_num)
#   region_params[[1]] = populate_region_list(offset_parcel_selection_type, offset_calc_type, offset_action_type,
#                                             offset_parcel_for_parcel, offset_multiplier = 1, 
#                                             dev_calc_type, mean_decline_rates[1])
#   return(region_params)
# }
