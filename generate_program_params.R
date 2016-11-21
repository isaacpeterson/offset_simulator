
initialise_program_params<- function(){
  program_params = list()
  program_params$offset_bank_type = c('parcel_set') #c('parcel_set', 'credit')       #'parcel_set' or 'credit'
  program_params$offset_region = 'development'
  program_params$use_offset_bank = c(FALSE)
  program_params$development_selection_type = 'random'
  program_params$offset_parcel_for_parcel = c(FALSE)
  program_params$restoration_rate = 0.05
  program_params$offset_time_horizon = c(20)
  program_params$offset_calc_type = c('restoration_from_cfac')  #'current_condition', 'avoided_degs', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
  program_params$dev_calc_type = c('future_condition')                    #'future_condition', 'current_condition' 
  program_params$offset_bank_start = 1
  program_params$offset_bank_end = 10
  program_params$offset_bank_num = 200
  program_params$include_illegal_clearing_in_parcel_value = TRUE
  program_params$include_dev_clearing_in_parcel_value = FALSE
  program_params$illegal_clearing_prob = 1e-3
  program_params$cfac_type_in_offset_calc = 'standard'
  program_params$cfac_type_in_dev_calc = program_params$cfac_type_in_offset_calc        
  program_params$offset_multiplier = 1
  program_params$offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
  program_params$offset_action_type = c('restore')
  program_params$use_parcel_set_dev_credit = FALSE
  program_params$post_facto_cfac_type = 'include_clearing_offsets'
  program_params$dev_start = 1
  program_params$dev_end = 50
  program_params$total_dev_num = 150
  return(program_params)
}


initialise_global_params <- function(){
  
  global_params = list()
  global_params$time_steps = 50 #number of timesteps simulation will be run
  global_params$total_dev_num = 80
  global_params$dev_start = 5
  global_params$dev_end = global_params$time_steps - 1
  global_params$eco_dims = 1
  global_params$offset_dims_to_use = seq_len(global_params$eco_dims)
  global_params$randomise_dev_nums = FALSE
  global_params$display_object = FALSE
  global_params$offset_thresh = 50
  global_params$parcel_size_lim = 50
  global_params$screen_parcels = TRUE
  global_params$set_seed = FALSE
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

generate_program_combs <- function(program_params){
  
  param_inds <- lapply(seq_along(program_params), function(i) 1:length(program_params[[i]]))
  param_combs <- expand.grid(param_inds)
  
  return(param_combs)
  
}


generate_current_program <- function(program_params, current_program_param_inds){
  current_program <- lapply(seq_along(program_params), function(i) program_params[[i]][current_program_param_inds[i]])
  names(current_program) <- names(program_params)
  return(current_program)
}



collate_current_program <- function(current_program, global_params){

  if (current_program$offset_calc_type == 'avoided_degredation'){
    current_program$offset_action_type = 'maintain'
  } else if (current_program$offset_calc_type %in% c('restoration_from_cfac', 'restoration_gains', 'restoration_condition_value')){
    current_program$offset_action_type = 'restore'
  }
  
  current_program$adjust_offset_cfacs_flag = ((current_program$cfac_type_in_offset_calc == 'include_clearing') || (current_program$cfac_type_in_offset_calc == 'include_clearing_offsets'))
  current_program$adjust_dev_cfacs_flag = ((current_program$cfac_type_in_dev_calc == 'include_clearing') || (current_program$cfac_type_in_dev_calc == 'include_clearing_offsets'))
  
  current_program$use_dev_credit = (current_program$use_parcel_set_dev_credit) || (current_program$offset_bank_type == 'credit')
  current_program$use_parcel_sets = (current_program$offset_bank_type == 'parcel_set') || (current_program$use_offset_bank == FALSE)
  current_program$adjust_counters_post_facto = ((current_program$post_facto_cfac_type == 'include_clearing') || (current_program$post_facto_cfac_type == 'include_clearing_offsets'))
  if ((current_program$offset_bank_type == 'parcel_set') || (current_program$use_offset_bank == FALSE)){
    current_program$match_type = 'parcel_set'
  }
  if ((current_program$offset_calc_type == 'current_condition') && (current_program$dev_calc_type == 'current_condition')){
    current_program$use_offset_time_horizon = FALSE
  } else {current_program$use_offset_time_horizon = TRUE}
  
  if( (current_program$offset_calc_type == 'avoided_degs') || (current_program$offset_calc_type == 'restoration_from_cfac') || (current_program$offset_calc_type == 'protected_condition') ){
    current_program$offset_cfacs_flag = TRUE
  } else{
    current_program$offset_cfacs_flag = FALSE
  }
  
  if( (current_program$offset_calc_type == 'restoration_gains') || (current_program$offset_calc_type == 'restoration_from_cfac') || (current_program$offset_calc_type == 'restoration_condition_value') ){
    current_program$offset_restoration_flag = TRUE
  } else {
    current_program$offset_restoration_flag = FALSE
  }
  
  if( (current_program$dev_calc_type == 'future_condition')){
    current_program$dev_cfacs_flag = TRUE
  } else{
    current_program$dev_cfacs_flag = FALSE
  }
  
  if (current_program$use_offset_bank == TRUE){
    current_program$banked_offset_vec = generate_dev_vec(time_steps = global_params$time_steps, prog_start = current_program$offset_bank_start, 
                                                         prog_end = current_program$offset_bank_end, total_prog_num = current_program$offset_bank_num, sd = 2)
  } else {
    current_program$banked_offset_vec = list()
  }
  
  if (current_program$include_illegal_clearing == TRUE){
    current_program$banked_offset_vec = generate_dev_vec(time_steps = global_params$time_steps, prog_start = current_program$offset_bank_start, 
                                                         prog_end = current_program$offset_bank_end, total_prog_num = current_program$offset_bank_num, sd = 2)
  } else {
    current_program$banked_offset_vec = list()
  }
  
  current_program$dev_vec = generate_dev_vec(time_steps = global_params$time_steps, prog_start = current_program$dev_start, prog_end = current_program$dev_end, 
                                            total_prog_num = current_program$total_dev_num, sd = 1)
  
  
  return(current_program)
  
}









initialise_program_params_a <- function(){
  program_params = list()
  program_params$offset_bank_type = c('parcel_set') #c('parcel_set', 'credit')       #'parcel_set' or 'credit'
  program_params$offset_region = 'development'
  program_params$use_offset_bank = c(FALSE)
  program_params$development_selection_type = 'random'
  program_params$offset_parcel_for_parcel = c(FALSE)
  program_params$restoration_rate = 0.05
  program_params$offset_time_horizon = c(20)
  program_params$offset_calc_type = c('restoration_from_cfac')  #'current_condition', 'avoided_degs', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
  program_params$dev_calc_type ='future_condition'                    #'future_condition', 'current_condition' 
  program_params$offset_bank_start = 1
  program_params$offset_bank_end = 10
  program_params$offset_bank_num = 200
  program_params$include_illegal_clearing_in_parcel_value = TRUE
  program_params$include_dev_clearing_in_parcel_value = FALSE
  program_params$illegal_clearing_prob = 1e-3
  program_params$cfac_type_in_offset_calc = 'standard'
  program_params$cfac_type_in_dev_calc = program_params$cfac_type_in_offset_calc        
  program_params$offset_multiplier = 1
  program_params$offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
  program_params$offset_action_type = c('restore')
  program_params$use_parcel_set_dev_credit = FALSE
  program_params$post_facto_cfac_type = 'standard'
  program_params$dev_start = 1
  program_params$dev_end = 50
  program_params$total_dev_num = 400
  return(program_params)
}

initialise_program_params_b <- function(global_params){
  program_params = list()
  program_params$offset_bank_type = c('parcel_set') #c('parcel_set', 'credit')       #'parcel_set' or 'credit'
  program_params$use_offset_bank = c(FALSE)
  program_params$offset_parcel_for_parcel = c(FALSE)
  program_params$restoration_rate = 0.05
  program_params$offset_time_horizon = c(20)
  program_params$offset_region = 'development'
  program_params$offset_calc_type = c('current_condition')  #'current_condition', 'avoided_degs', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
  program_params$dev_calc_type = 'current_condition'                    #'future_condition', 'current_condition' 
  program_params$offset_bank_start = 1
  program_params$offset_bank_end = 10
  program_params$offset_bank_num = 50
  program_params$cfac_type_in_offset_calc = 'standard'
  program_params$cfac_type_in_dev_calc = program_params$cfac_type_in_offset_calc        
  program_params$offset_multiplier = 1
  program_params$offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
  program_params$offset_action_type = c('restore')
  program_params$use_parcel_set_dev_credit = FALSE
  program_params$post_facto_cfac_type = 'include_clearing_offsets'
  program_params$dev_start = 1
  program_params$dev_end = global_params$time_steps
  program_params$total_dev_num = 100
  return(program_params)
}
