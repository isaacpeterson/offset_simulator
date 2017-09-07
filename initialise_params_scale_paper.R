initialise_run_params <- function(){
  run_params = list()

  # Where simulation outputs will be written
  run_params$simulation_folder = paste0(path.expand('~'), '/offset_data/simulated/')
 
  # The number of realizations to run
  run_params$realisation_num = 4

  # The total number of parcel that will be developed
  run_params$total_dev_num = 250

  # The time step at which development starts
  run_params$dev_start = 1
  
  # The time at which development ends
  run_params$dev_end = 50

# Specify how many cores to run on. Default setting here it to use all available
  run_params$crs = detectCores(all.tests = FALSE, logical = TRUE)

  # hHw long to run the simulaton in years
  run_params$time_steps = 50
  
  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  run_params$write_offset_layer = FALSE
  
  # The maxoimum number of parcels can be selected to offset a single development
  run_params$max_offset_parcel_num = 5
  
  # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  run_params$illegal_clearing_prob = 1e-3

  # Excludes the top and bottom 5% of parcels in terms of their biodiversity values soo keeps values between [0.05, 0.95]
  run_params$screen_parcels_by_size = TRUE 

  # Exclude parcels with less than this number of pixels
  run_params$parcel_screen_size = 20 

  # The mean and the standard deviation of a normal distribution from which to sample the restoration parameters
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
  policy_params$offset_calc_type = c('net_gains', 'restoration_gains', 'avoided_condition_decline')

  # The time horizon in which the offset gains need to equal the devlopment impact
  policy_params$offset_time_horizon = c(15, 30)
  
  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. 
  policy_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. 
  policy_params$include_illegal_clearing_in_dev_calc = policy_params$include_illegal_clearing_in_offset_calc

  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site ie ongling decline if there are landscape declines
  policy_params$dev_calc_type = c('future_condition', 'current_condition' )    #'future_condition', 'current_condition' 
    
  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  policy_params$include_potential_developments_in_offset_calc = c(FALSE)

  # Include future developments in calculating contribution of avoided losses
  # to the impact of the development. This reduces the development impact because
  # projected future value of the site is lower if there is some probability
  # the site may be developed in the future

  return(policy_params)

}

