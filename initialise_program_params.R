initialise_program_params <- function(offset_multiplier, restoration_rate, use_parcel_set_dev_credit, offset_parcel_for_parcel, offset_bank_type, use_offset_bank, offset_bank_params, offset_time_horizon_type,  offset_calc_type, dev_calc_type, offset_action_type, cfac_type_in_offset_calc, offset_time_horizon){
  
  program_params$offset_multiplier = offset_multiplier
  program_params$restoration_rate = restoration_rate
  program_params$offset_bank_type = offset_bank_type
  program_params$offset_time_horizon_type = offset_time_horizon_type
  program_params$offset_parcel_for_parcel = offset_parcel_for_parcel
  program_params$use_offset_bank = use_offset_bank
  program_params$offset_calc_type = offset_calc_type
  program_params$dev_calc_type = dev_calc_type
  program_params$offset_action_type = offset_action_type
  program_params$cfac_type_in_offset_calc = cfac_type_in_offset_calc
  program_params$cfac_type_in_dev_calc = cfac_type_in_offset_calc
  program_params$offset_bank_num = offset_bank_params[3]
  program_params$offset_bank_start = offset_bank_params[1]
  program_params$offset_bank_end = offset_bank_params[2]
  
  program_params$offset_time_horizon = offset_time_horizon
  program_params$adjust_cfacs_flag = ((cfac_type_in_offset_calc == 'include_clearing') || (cfac_type_in_offset_calc == 'include_clearing_offsets'))
  program_params$use_parcel_set_dev_credit = use_parcel_set_dev_credit
  program_params$use_dev_credit = (use_parcel_set_dev_credit) || (offset_bank_type == 'credit')
  program_params$use_parcel_sets = (offset_bank_type == 'parcel_set') || (use_offset_bank == FALSE)
  
  program_params$post_facto_cfac_type = 'include_clearing_offsets'
  program_params$adjust_counters_post_facto = ((program_params$post_facto_cfac_type == 'include_clearing') 
                                              || (program_params$post_facto_cfac_type == 'include_clearing_offsets'))
  if ((offset_bank_type == 'parcel_set') || (use_offset_bank == FALSE)){
    program_params$match_type = 'parcel_set'
  }
  if ((offset_calc_type == 'current_condition') && (dev_calc_type == 'current_condition')){
    program_params$use_offset_time_horizon = FALSE
  } else {program_params$use_offset_time_horizon = TRUE}
  
  if( (offset_calc_type == 'avoided_degredation') || (offset_calc_type == 'restoration_from_cfac') || (offset_calc_type == 'future_condition') ){
    program_params$offset_cfacs_flag = TRUE
  } else{
    program_params$offset_cfacs_flag = FALSE
  }
  
  if( (offset_calc_type == 'restoration_gains') || (offset_calc_type == 'restoration_from_cfac') || (offset_calc_type == 'restoration_condition_value') ){
    program_params$offset_restoration_flag = TRUE
  } else {
    program_params$offset_restoration_flag = FALSE
  }
  
  if( (dev_calc_type == 'future_condition')){
    program_params$dev_cfacs_flag = TRUE
  } else{
    program_params$dev_cfacs_flag = FALSE
  }
  
  
  
  return(program_params)
  
}






