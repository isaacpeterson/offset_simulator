
initialise_global_params <- function(){
  
  global_params = list()
  global_params$realisation_num = 10 #how many realisations of system to run in parallel
  global_params$eco_dims = 1 #how many ecological dimensions to use in simulation
  global_params$region_num_x = 1 #number of regions in x
  global_params$region_num_y = 1 #number of regions in y
  global_params$region_num = global_params$region_num_x*global_params$region_num_y
  global_params$mean_decline_rates = -1e-2 #set parameter for rate of decline according to logistic curve
  global_params$time_steps = 50 #number of timesteps simulation will be run
  global_params$max_offset_parcel_num = 2 #how many parcels can be selected to offset a single development
  global_params$dims_to_use = seq_len(global_params$eco_dims) #what dimensions to consider in claculations e.g. keep many layers running while just optimisaing for layer 1 or 2 or 1:2
  global_params$perform_illegal_clearing = TRUE # switch on/off illegal clearing (this can occur in offset regions or in available regions)
  global_params$offset_thresh = 50
  global_params$parcel_size_lim = 50 #set min parcel size limits if using global_params$screen_parcel_size
  global_params$screen_parcel_size = FALSE # do not use parcels with fewer cells than defined by global_params$parcel_size_lim
  global_params$set_seed = FALSE
  global_params$match_threshold = 0 # acceptable level above which to accept parcel match
  global_params$ecology_size = 200 #ecology array size (square dimension) to be broken up into regions and land parcels  
  global_params$parcel_num_x = 30 #numnber of parcels in x
  global_params$parcel_num_y = 30 #numnber of parcels in x
  global_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  global_params$max_eco_val = 100 #maximum "   "     "           "
  global_params$max_restoration_eco_val = 70
  global_params$min_initial_eco_val = 20 #minimum allowable initial ecological value of smallest ecological element (pixel)
  global_params$max_initial_eco_val = 100 #maximum "   "     "           "
  global_params$initial_eco_noise = 10 #how much initial variation in pixels per land parcel 
  global_params$blur = FALSE
  global_params$mean_decline_rates = global_params$mean_decline_rates*array(runif(global_params$region_num*global_params$eco_dims), c(global_params$region_num, global_params$eco_dims))
  return(global_params)
  
}

initialise_program_params<- function(){
  program_params = list()
  program_params$offset_bank_type = c('parcel_set') #c('parcel_set', 'credit')       #'parcel_set' - select discrete land parcels or 'credit' - subtract value from total accumulated gains
  program_params$offset_region = 'development' # force offsets to be in same region as development
  program_params$use_offset_bank = c(FALSE) # FALSE - perform offsets simultaneously with development, TRUE - perform offset banking prior to development according to offset bank parameters 
  program_params$offset_bank_start = 1 #min time for offset banking to initialise
  program_params$offset_bank_end = 10 #mix time for offset banking to finish
  program_params$offset_bank_num = 200 # how many parcels to include in banking scheme
   
  program_params$development_selection_type = 'random' #how the development parcels are selected - 'random' - sample from pool of available indexes
  program_params$offset_parcel_for_parcel = c(TRUE) # TRUE - one-to-one selection of offset parcels for one development, FALSE = many-to-one selection of offset parcels for one development
  program_params$restoration_rate = 0.02 
  program_params$offset_time_horizon = c(15, 30)
  program_params$offset_calc_type = c('restoration_from_cfac')  #'current_condition', 'avoided_degs', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
  program_params$dev_calc_type = c('future_condition')                    #'future_condition', 'current_condition' 
  
  program_params$include_potential_developments_in_offset_calc = c(FALSE)
  program_params$include_potential_offsets_in_offset_calc = c(FALSE)
  program_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  program_params$include_potential_developments_in_dev_calc = program_params$include_potential_developments_in_offset_calc
  program_params$include_potential_offsets_in_dev_calc = program_params$include_potential_offsets_in_offset_calc
  program_params$include_illegal_clearing_in_dev_calc = program_params$include_illegal_clearing_in_offset_calc
  
  program_params$illegal_clearing_prob = 1e-4
  
  program_params$offset_multiplier = 1
  program_params$offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
  program_params$offset_action_type = c('restore')
  program_params$use_parcel_set_dev_credit = FALSE
  program_params$dev_start = 1
  program_params$dev_end = 50
  program_params$total_dev_num = 150
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



collate_current_program <- function(current_program_params, global_params){

  if (current_program_params$offset_calc_type == 'avoided_degredation'){
    current_program_params$offset_action_type = 'maintain'
  } else if (current_program_params$offset_calc_type %in% c('restoration_from_cfac', 'restoration_gains', 'restoration_condition_value')){
    current_program_params$offset_action_type = 'restore'
  }
  
  any(current_program_params$include_potential_developments_in_offset_calc,
  current_program_params$include_potential_offsets_in_offset_calc,
  current_program_params$include_illegal_clearing_in_offset_calc) == TRUE
  
  current_program_params$adjust_offset_cfacs_flag = any(current_program_params$include_potential_developments_in_offset_calc,
                                                        current_program_params$include_potential_offsets_in_offset_calc,
                                                        current_program_params$include_illegal_clearing_in_offset_calc) == TRUE
  current_program_params$adjust_dev_cfacs_flag = any(current_program_params$include_potential_developments_in_dev_calc,
                                                     current_program_params$include_potential_offsets_in_dev_calc,
                                                     current_program_params$include_illegal_clearing_in_dev_calc) == TRUE
  
  current_program_params$use_dev_credit = (current_program_params$use_parcel_set_dev_credit) || (current_program_params$offset_bank_type == 'credit')
  current_program_params$use_parcel_sets = (current_program_params$offset_bank_type == 'parcel_set') || (current_program_params$use_offset_bank == FALSE)
 if ((current_program_params$offset_bank_type == 'parcel_set') || (current_program_params$use_offset_bank == FALSE)){
    current_program_params$match_type = 'parcel_set'
  }
  if ((current_program_params$offset_calc_type == 'current_condition') && (current_program_params$dev_calc_type == 'current_condition')){
    current_program_params$use_offset_time_horizon = FALSE
  } else {current_program_params$use_offset_time_horizon = TRUE}
  
  if( (current_program_params$offset_calc_type == 'avoided_degs') || (current_program_params$offset_calc_type == 'restoration_from_cfac') || (current_program_params$offset_calc_type == 'protected_condition') ){
    current_program_params$offset_cfacs_flag = TRUE
  } else{
    current_program_params$offset_cfacs_flag = FALSE
  }
  
  if( (current_program_params$offset_calc_type == 'restoration_gains') || (current_program_params$offset_calc_type == 'restoration_from_cfac') || (current_program_params$offset_calc_type == 'restoration_condition_value') ){
    current_program_params$offset_restoration_flag = TRUE
  } else {
    current_program_params$offset_restoration_flag = FALSE
  }
  
  if( (current_program_params$dev_calc_type == 'future_condition')){
    current_program_params$dev_cfacs_flag = TRUE
  } else{
    current_program_params$dev_cfacs_flag = FALSE
  }
  
  if (current_program_params$use_offset_bank == TRUE){
    current_program_params$banked_offset_vec = generate_intervention_vec(time_steps = global_params$time_steps, prog_start = current_program_params$offset_bank_start, 
                                                         prog_end = current_program_params$offset_bank_end, total_prog_num = current_program_params$offset_bank_num, sd = 2)
  } else {
    current_program_params$banked_offset_vec = list()
  }
  
  current_program_params$intervention_vec = generate_intervention_vec(time_steps = global_params$time_steps, prog_start = current_program_params$dev_start, prog_end = current_program_params$dev_end, 
                                            total_prog_num = current_program_params$total_dev_num, sd = 1)

  return(current_program_params)
  
}







