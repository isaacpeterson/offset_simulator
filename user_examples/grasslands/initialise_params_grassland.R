initialise_user_global_params <- function(){
  global_params = list()
  
  # Where simulation outputs will be written
  global_params$simulation_folder = getwd()
  
  global_params$use_simulated_data = FALSE
  # The number of realizations to run
  global_params$realisation_num = 1
  
  # The total number of layers to use in the offset calcuation (iterating from the start)
  global_params$features_to_use_in_offset_calc = 1
  
  # what subset of features to use in the simulation
  global_params$features_to_use_in_simulation = 1
  
  global_params$landscape_evolve_type = 'static'
  
  global_params$mean_decline_rates = rep(list(1), length(global_params$features_to_use_in_simulation)) 
  
  #set this parameter to zero to yield no noise
  global_params$decline_rate_std = rep(list(0), length(global_params$features_to_use_in_simulation))
  
  # The total number of parcels that will be developed
  global_params$total_dev_num = 50
  
  # The time step at which development starts
  global_params$dev_start = 1
  
  # The time at which development ends
  global_params$dev_end = 50
  
  # The time step at which development starts
  global_params$dev_start = 1
  
  # The time at which development ends
  global_params$dev_end = 50
  
  # Specify how many cores to run on. Default setting here it to use all available
  global_params$number_of_cores = 1 #detectCores(all.tests = FALSE, logical = TRUE)
  
  # How long to run the simulaton in years
  global_params$time_steps = 50
  
  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  global_params$write_offset_layer = FALSE
  
  global_params$write_movie = TRUE
  # The maxoimum number of parcels can be selected to offset a single development
  
  global_params$max_offset_parcel_num = 20
  
  # Stops the offset from delivering any further gains once it has acheived the gains required
  global_params$limit_offset_restoration = TRUE
  
  # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  global_params$illegal_clearing_prob = 1e-3
  
  # Excludes offset sites by size limitations (in number of elements set by global_params$parcel_screen_size)
  global_params$screen_sites_by_size = TRUE 
  
  # Exclude parcels with less than this number of pixels.
  global_params$site_screen_size = 50 
  
  global_params$screen_offset_site_zeros = TRUE
  
  # The mean and the standard deviation of a normal distribution from which to sample the restoration parameters from
  global_params$restoration_rate_params = c(0.02, 0.005)
  
  
  return(global_params)
}



initialise_user_combination_params <- function(){ 
  
  combination_params = list()
  

  # The Options are 'restoration_gains' - the gains are calcualted relative to
  # the site value at the time of the interventionabove 
  # 'avoided_loss' - the gains are calculated relative to the biodiversity condition
  #                  without the offset in place (the do nothing counterfactual)
  # 'net_gains' - is the sum of the previous 2
  combination_params$offset_action_params = list(c('avoided_loss', 'maintain'))

  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
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
  combination_params$offset_time_horizon = c(15)
  
  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  # combination_params$include_illegal_clearing_in_dev_calc = combination_params$include_illegal_clearing_in_offset_calc
  
  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  combination_params$include_potential_developments_in_offset_calc = c(TRUE)
  
  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  combination_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  
  combination_params$dev_counterfactual_adjustment = 'as_offset'
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  combination_params$offset_multiplier = 1
  
  
  return(combination_params)
  
}

