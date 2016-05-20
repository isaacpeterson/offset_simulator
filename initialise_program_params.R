initialise_program_params <- function(global_params, offset_multiplier, restoration_rate, use_parcel_set_dev_credit, offset_parcel_for_parcel, offset_bank_type, use_offset_bank, offset_bank_params, offset_time_horizon_type,  offset_calc_type, dev_calc_type, offset_action_type, cfac_type_in_offset_calc, offset_time_horizon){
  global_params$offset_multiplier = offset_multiplier
  global_params$restoration_rate = restoration_rate
  global_params$offset_bank_type = offset_bank_type
  global_params$offset_time_horizon_type = offset_time_horizon_type
  global_params$offset_parcel_for_parcel = offset_parcel_for_parcel
  global_params$use_offset_bank = use_offset_bank
  global_params$offset_calc_type = offset_calc_type
  global_params$dev_calc_type = dev_calc_type
  global_params$offset_action_type = offset_action_type
  global_params$cfac_type_in_offset_calc = cfac_type_in_offset_calc
  global_params$cfac_type_in_dev_calc = cfac_type_in_offset_calc
  global_params$offset_bank_num = offset_bank_params[3]
  global_params$offset_time_horizon = offset_time_horizon
  global_params$adjust_cfacs_flag = ((cfac_type_in_offset_calc == 'include_clearing') || (cfac_type_in_offset_calc == 'include_clearing_offsets'))
  global_params$use_parcel_set_dev_credit = use_parcel_set_dev_credit
  global_params$use_dev_credit = (use_parcel_set_dev_credit) || (offset_bank_type == 'credit')
  global_params$use_parcel_sets = (offset_bank_type == 'parcel_set') || (use_offset_bank == FALSE)
  
  global_params$post_facto_cfac_type = 'include_clearing_offsets'
  global_params$adjust_counters_post_facto = ((global_params$post_facto_cfac_type == 'include_clearing') 
                                              || (global_params$post_facto_cfac_type == 'include_clearing_offsets'))
  if ((offset_bank_type == 'parcel_set') || (use_offset_bank == FALSE)){
    global_params$match_type = 'parcel_set'
  }
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
  
  if (global_params$use_offset_bank == TRUE){

    global_params$banked_offset_vec = find_prog_vector(time_steps = global_params$time_steps, prog_start = offset_bank_params[1], prog_end = offset_bank_params[2], 
                                                       total_prog_num = global_params$offset_bank_num, sd = 2)
  } else {
    global_params$banked_offset_vec = list()
  }
  
  return(global_params)
  
}
