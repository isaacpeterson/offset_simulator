initialise_run_params <- function(){
  run_params = list()

  # Where simulation outputs will be written
  run_params$simulation_folder = paste0(path.expand('~'), '/offset_data/simulated/')
  # The number of realizations to run
  run_params$realisation_num = 2

  # Specify how many cores to run on. Default setting here it to use all available
  run_params$crs = detectCores(all.tests = FALSE, logical = TRUE)

  # hHw long to run the simulaton in years
  run_params$time_steps = 50
  
  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  run_params$write_offset_layer = FALSE
  
    # The maxoimum number of parcels can be selected to offset a single development
  run_params$max_offset_parcel_num = 5

  # Stops the offset from delivering any further gains once it has acheived the gains required
  run_params$limit_offset_restoration = TRUE

    # The probability per parcel of it being illegaxlly cleared, every parcel gets set to this number - set to zero to turn off
  run_params$illegal_clearing_prob = 1e-3

  # Excludes the top and bottom 5% of parcels in terms of their biodiversity values soo keeps values between [0.05, 0.95]
  run_params$screen_parcels_by_size = TRUE 

  # Exclude parcels with less than this number of pixels.
  run_params$parcel_screen_size = 50 

  # The mean and the standard deviation of a normal distribution fro which to sample the restoration parameters from
  run_params$restoration_rate_params = c(0.02, 0.005)

  return(run_params)
}



initialise_policy_params <- function(){ 

  # Parameters controlling offset policy settings. Note that any of these
  # parameters can take an arbitrary number of values, and the code will then
  # every combination of parameter values over all these variables. For
  # example  x=(a,b); y=(c,d) will then run the model with x=a y=c, x=a y=d,
  # x=b y=c, x=b y=d. Each of these combinations will be designated a
  # 'scenario'. There caution should be used when specifying multiple values
  # as it's easy to create a large number scenarios.


  policy_params = list()

  # The Options are 'restoration_gains' - the gains are calcualted relative to
  # the site value at the time of the interventionabove 
  # 'avoided_condition_decline - the gains are calculated relative to the biodiversity
  # 'condition without the offset in place (the do nothing counterfactual)
  # 'net_gains' - is the sum of the previous 2
  policy_params$offset_calc_type = c('net_gains', 'restoration_gains', 'avoided_condition_decline') 

  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
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
  policy_params$offset_time_horizon = c(15, 30)

  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  policy_params$include_potential_developments_in_offset_calc = c(FALSE)

  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  policy_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the offset. The decreases the impact of the offset (due to
  # future gains that are avoided)
  policy_params$include_potential_offsets_in_offset_calc = c(FALSE)

  # Include future developments in calculating contribution of avoided losses
  # to the impact of the development. This reduces the development impact because
  # projected future value of the site is lower if there is some probability
  # the site may be developed in the future
  policy_params$include_potential_developments_in_dev_calc = c(FALSE)

  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. This reduces the development
  # impact because projected future value of the site is lower if there is
  # some probability the site may be illegally developed in the future
  policy_params$include_illegal_clearing_in_dev_calc = policy_params$include_illegal_clearing_in_offset_calc

  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the development. This increases the impact of the development as
  # future gains are avoided
  policy_params$include_potential_offsets_in_dev_calc = c(FALSE)


  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  policy_params$offset_multiplier = 1

  return(policy_params)

}

