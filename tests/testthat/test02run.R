initialise_user_global_params <- function(){
  global_params = list()

  # Fix the random seed so we get deterministic results
  global_params$set_seed = TRUE
  
  # Where simulation outputs will be written
  global_params$simulation_folder = paste0('../output/test02out')
  
  # Fix the output directory (will overwrite existing files) instead of creating unique 
  global_params$unique_simulation_folder = FALSE
  
  # The number of realizations to run
  global_params$realisation_num = 2
  
  # The total number of parcels that will be developed
  global_params$total_dev_num = 10
  
  # The time step at which development starts
  global_params$dev_start = 1
  
  # The time at which development ends
  global_params$dev_end = 5
  
  # Specify how many cores to run on. Default setting here it to use all available
  global_params$number_of_cores = 2 #parallel::detectCores(all.tests = FALSE, logical = TRUE)
  
  # How long to run the simulaton in years
  global_params$time_steps = 5
  
  return(global_params)
}


initialise_user_combination_params <- function(){ 
  
  combination_params = list()
  

  combination_params$offset_action_params = list(c('net_gains', 'restore'))
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  # combination_params$dev_calc_type = c('future_condition', 'current_condition')    #'future_condition', 'current_condition' 
  combination_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 
  
  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  combination_params$allow_developments_from_credit = TRUE
  
  # How the development parcels are selected options are 'random' or
  # 'weighted'. Note tha weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  combination_params$development_selection_type = 'random'  
  
  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  combination_params$use_offset_bank = c(FALSE)
  
  # The time at which the offset in the bank offsets are first are implemented and start acurring grains, 
  combination_params$offset_bank_start = 1 
  
  # The time at which no more offsets are added to the bank. The number of
  # offsets per time step is determined as follows: First the mean number
  # number per time step is determined, then sampling is done around this
  # mean number using a normal distribution such that the total number of
  # developments will always equal the total number (Note sd for this
  # distribution is set in the code the currently isn't user settable)
  combination_params$offset_bank_end = 1 
  
  # THe number parcels to include in banking scheme. These are randomly selected.
  combination_params$offset_bank_num = 200 
  
  # Options are 'credit' or 'parcel_set'. 'credit' means there is accumulated
  # gain that is subtracted as parcels are developed. 'parcel_set' one or more
  # parcels in the bank are traded for one development site. If there is left
  # over credit (and allow_developments_from_credit is set to TRUE) then this excess credit is used on subsequent developments
  combination_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')     
  
  # The time horizon in which the offset gains need to equal the devlopment impact
  # combination_params$offset_time_horizon = c(15, 30)
  combination_params$offset_time_horizon = c(5)
  
  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. 
  combination_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  # combination_params$include_illegal_clearing_in_dev_calc = combination_params$include_illegal_clearing_in_offset_calc
  
  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  # combination_params$include_potential_developments_in_offset_calc = c(FALSE, TRUE)
  combination_params$include_potential_developments_in_offset_calc = c(TRUE)
  
  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  # combination_params$include_illegal_clearing_in_offset_calc = c(FALSE, TRUE)
  combination_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  
  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the offset. The decreases the impact of the offset (due to
  # future gains that are avoided)
  combination_params$include_potential_offsets_in_offset_calc = c(FALSE)
  
  combination_params$dev_counterfactual_adjustment = 'as_offset'
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  combination_params$offset_multiplier = 1
  
  
  return(combination_params)
  
}

