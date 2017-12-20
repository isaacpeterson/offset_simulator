initialise_user_global_params <- function(){
  global_params = list()

  # Fix the random seed so we get deterministic results
  global_params$set_seed = TRUE
  
  # Where simulation outputs will be written
  global_params$simulation_folder = paste0('../output/test01out')

  # Fix the output directory (will overwrite existing files) instead of creating unique 
  global_params$unique_simulation_folder = FALSE
  
  # The number of realizations to run
  global_params$realisation_num = 1

  # The total number of parcels that will be developed
  global_params$total_dev_num = 10

  # The time step at which development starts
  global_params$dev_start = 1
  
  # The time at which development ends
  global_params$dev_end = 5

  # Specify how many cores to run on. Default setting here it to use all available
  global_params$number_of_cores = 1 #parallel::detectCores(all.tests = FALSE, logical = TRUE)

  # How long to run the simulaton in years
  global_params$time_steps = 5

  return(global_params)
}



initialise_user_combination_params <- function(){ 
  
  combination_params = list()


  combination_params$offset_action_params = list(c('net_gains', 'restore'))
  

  combination_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 


  combination_params$allow_developments_from_credit = TRUE
  

  combination_params$development_selection_type = 'random'  

  combination_params$use_offset_bank = c(FALSE)


  combination_params$offset_bank_start = 1 

  combination_params$offset_bank_end = 1 


  combination_params$offset_bank_num = 200 

    combination_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')     
  
  combination_params$offset_time_horizon = c(15)
  
  combination_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  combination_params$include_potential_developments_in_offset_calc = c(TRUE)

  combination_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  
  combination_params$include_potential_offsets_in_offset_calc = c(FALSE)
  
  combination_params$dev_counterfactual_adjustment = 'as_offset'
  
  combination_params$offset_multiplier = 1


  return(combination_params)

}

