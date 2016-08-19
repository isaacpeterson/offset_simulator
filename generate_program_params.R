#KNOWN ISSUES: offset_parcel_for_parcel


initialise_program_param_combs <- function(){
  program_params = list()
  program_params$offset_bank_type = c('parcel_set', 'credit') #c('parcel_set', 'credit')       #'parcel_set' or 'credit'
  program_params$use_offset_bank = c(TRUE, FALSE)
  program_params$offset_parcel_for_parcel = c(TRUE)
  program_params$restoration_rate = c(0.02)
  program_params$offset_time_horizon = 20
  program_params$offset_calc_type = c('restoration_from_cfac', 'restoration_gains', 'avoided_degs') #, 'protected_condition', 'restoration_gains'   #'current_condition', 'avoided_degredation', 'restoration_from_cfac', 'restoration_gains', 'restoration_condition_value'
  program_params$dev_calc_type = c('future_condition')                     #'future_condition', 'current_condition' 
  program_params$offset_bank_start = 1
  program_params$offset_bank_end = 1
  program_params$offset_bank_num = 100
  program_params$cfac_type_in_offset_calc = c('standard') #, 'standard', 'include_clearing', 'include_clearing_offsets')
  program_params$cfac_type_in_dev_calc = program_params$cfac_type_in_offset_calc        
  program_params$offset_multiplier = 1
  program_params$offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
  program_params$offset_action_type = c('protect', 'maintain', 'restore')
  program_params$use_parcel_set_dev_credit = FALSE
  program_params$post_facto_cfac_type = program_params$cfac_type_in_offset_calc
  return(program_params)
}



initialise_program_param_single <- function(){
  program_params = list()
  program_params$offset_bank_type = c('parcel_set') #c('parcel_set', 'credit')       #'parcel_set' or 'credit'
  program_params$use_offset_bank = c(FALSE)
  program_params$offset_parcel_for_parcel = c(TRUE)
  program_params$restoration_rate = 0.05
  program_params$offset_time_horizon = c(20)
  program_params$offset_calc_type = c('current_condition')  #'current_condition', 'avoided_degs', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
  program_params$dev_calc_type ='current_condition'                    #'future_condition', 'current_condition' 
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
  return(program_params)
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

collate_current_program <- function(current_program){

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
  
  return(current_program)
  
}

