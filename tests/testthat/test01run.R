initialise_run_params <- function(){
  run_params = list()

  # Fix the random seed so we get deterministic results
  run_params$set_seed = TRUE
  
  # Where simulation outputs will be written
  run_params$simulation_folder = paste0('../output/test01out')

  # Fix the output directory (will overwrite existing files) instead of creating unique 
  run_params$unique_simulation_folder = FALSE
  
  # The number of realizations to run
  run_params$realisation_num = 1

  # The total number of parcels that will be developed
  run_params$total_dev_num = 10

  # The time step at which development starts
  run_params$dev_start = 1
  
  # The time at which development ends
  run_params$dev_end = 5

  # Specify how many cores to run on. Default setting here it to use all available
  run_params$crs = 1 #parallel::detectCores(all.tests = FALSE, logical = TRUE)

  # How long to run the simulaton in years
  run_params$time_steps = 5
  
  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  run_params$write_offset_layer = FALSE
  
  # The maxoimum number of parcels can be selected to offset a single development
  run_params$max_offset_parcel_num = 20

  # Stops the offset from delivering any further gains once it has acheived the gains required
  run_params$limit_offset_restoration = TRUE

  # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  run_params$illegal_clearing_prob = 1e-3

  # Excludes offset sites by size limitations (in number of elements set by run_params$parcel_screen_size)
  run_params$screen_offset_sites_by_size = TRUE 

  # Excludes offset sites by size limitations (in number of elements set by run_params$parcel_screen_size)
  run_params$screen_dev_sites_by_size = TRUE
  
  # Exclude parcels with less than this number of pixels.
  run_params$site_screen_size = 50 

  run_params$screen_offset_site_zeros = TRUE
  
  # The mean and the standard deviation of a normal distribution from which to sample the restoration parameters from
  run_params$restoration_rate_params = c(0.02, 0.005)

  
  return(run_params)
}



initialise_policy_params <- function(){ 
  
  policy_params = list()

# Offset policy types


#   Offset: action is restoration, the counterfactual is ongoing decline
#   Development:  impact is calcualted relative to ongoing decines in
#   the development site.

#     offset_calc_type = 'net_gains'
#     dev_calc_type =  'future_condition'
#     include_potential_developments_in_offset_calc = FALSE
#     include_potential_developments_in_dev_calc = FALSE


#   Offset: action is restoration, the counterfactual is ongoing decline and legal clearing
#   Development:  impact is calcualted relative to ongoing decines in
#   the development site and legal clearing
#     offset_calc_type = 'net_gains'
#     dev_calc_type =  'future_condition'
#     include_potential_developments_in_offset_calc = TRUE
#     include_potential_developments_in_dev_calc = TRUE


#   Offset: action is restoration, the counterfactual is current condition
#   Development: impact is calcualted relative to current condition
#     offset_calc_type = 'restoration_gains'
#     dev_calc_type =  'current_condition'
#     include_potential_developments_in_offset_calc = FALSE
#     include_potential_developments_in_dev_calc = FALSE

#   Offset: action is keeping current condition constant, counterfactual is ongoing decline 
#   Development: impact is calcualted relative to ongoing decines in
#   the development site.
#     offset_calc_type = 'avoided_condition_decline'
#     dev_calc_type =  'future_condition'
#     include_potential_developments_in_offset_calc = FALSE
#     include_potential_developments_in_offset_calc = FALSE

#   Offset: action is keeping current condition constant, counterfactual is ongoing decline and legal clearing
#   Development: impact is calcualted relative to ongoing decines in the development site and legal clearing.
#     offset_calc_type = 'avoided_condition_decline'
#     dev_calc_type =  'future_condition'
#     include_potential_developments_in_offset_calc = TRUE
#     include_potential_developments_in_offset_calc = TRUE

#   CURRENTLY UNAVAILABLE Offset: action is protection only (allowing onging declines), counterfactual is ongoing decline and legal clearing
#   Development: impact is calcualted relative to ongoing decline
#     offset_calc_type = 'avoided_condition_decline'
#     dev_calc_type =  'future_condition'
#     include_potential_developments_in_offset_calc = FALSE
#     include_potential_developments_in_offset_calc = FALSE

# Time Horizon
#   Each of the above can be used to have the gains calcualted over either 15 or 30 years.

# Illegal clearing  
#   Each of the above can be run with and without illegal clearing occurring
#   and including illegal clearing in the offset and dev counterfactuals
#   To turn off illegal clearing set illegal_clearing_prob to zero (in run_params). Otherwise a 
#   prob of illegal clearing of 1e-3 of is often a reasonable setting. Use the following to 
#   specify whether the illegal clearing is used in the offset and development calcs
#       include_illegal_clearing_in_offset_calc
#       include_illegal_clearing_in_dev_calc

  # The Options are 'restoration_gains' - the gains are calcualted relative to
  # the site value at the time of the interventionabove 
  # 'avoided_loss' - the gains are calculated relative to the biodiversity condition
  #                  without the offset in place (the do nothing counterfactual)
  # 'net_gains' - is the sum of the previous 2
  policy_params$offset_action_params = list(c('net_gains', 'restore'))
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  # policy_params$dev_calc_type = c('future_condition', 'current_condition')    #'future_condition', 'current_condition' 
  policy_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 

  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  policy_params$allow_developments_from_credit = TRUE
  
  # How the development parcels are selected options are 'random' or
  # 'weighted'. Note tha weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # intialise_routines.R  (or put the files in simulation_inputs)
  policy_params$development_selection_type = 'random'  

  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  policy_params$use_offset_bank = c(FALSE)

  # The time at which the offset in the bank offsets are first are implemented and start acurring grains, 
  policy_params$offset_bank_start = 1 

  # The time at which no more offsets are added to the bank. The number of
  # offsets per time step is determined as follows: First the mean number
  # number per time step is determined, then sampling is done around this
  # mean number using a normal distribution such that the total number of
  # developments will always equal the total number (Note sd for this
  # distribution is set in the code the currently isn't user settable)
  policy_params$offset_bank_end = 1 

  # THe number parcels to include in banking scheme. These are randomly selected.
  policy_params$offset_bank_num = 200 

  # Options are 'credit' or 'parcel_set'. 'credit' means there is accumulated
  # gain that is subtracted as parcels are developed. 'parcel_set' one or more
  # parcels in the bank are traded for one development site. If there is left
  # over credit (and allow_developments_from_credit is set to TRUE) then this excess credit is used on subsequent developments
  policy_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')     
  
  # The time horizon in which the offset gains need to equal the devlopment impact
  # policy_params$offset_time_horizon = c(15, 30)
  policy_params$offset_time_horizon = c(5)
  
  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. 
  policy_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  # policy_params$include_illegal_clearing_in_dev_calc = policy_params$include_illegal_clearing_in_offset_calc

  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  # policy_params$include_potential_developments_in_offset_calc = c(FALSE, TRUE)
  policy_params$include_potential_developments_in_offset_calc = c(TRUE)

  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  # policy_params$include_illegal_clearing_in_offset_calc = c(FALSE, TRUE)
  policy_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  
  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the offset. The decreases the impact of the offset (due to
  # future gains that are avoided)
  policy_params$include_potential_offsets_in_offset_calc = c(FALSE)
  
  policy_params$dev_counterfactual_adjustment = 'as_offset'
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  policy_params$offset_multiplier = 1


  return(policy_params)

}

