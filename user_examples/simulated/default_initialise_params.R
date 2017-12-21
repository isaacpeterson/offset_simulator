initialise_default_global_params <- function(){
  default_global_params = list()
  
  # to use user simulated landscape params label file, to use default landscape params label as 'default'
  default_global_params$simulated_ecology_user_params_file = 'user_simulated_ecology_params.R' # use 
  
  # set to TRUE to overwrite default params wih user params and FALASE otherwise
  default_global_params$overwrite_default_params = TRUE
  
  # Where simulation outputs will be written
  default_global_params$simulation_folder = paste0('simulated_data/')
 
  default_global_params$use_simulated_data = TRUE
  
  # Fix the output directory (will overwrite existing files) instead of creating unique 
  default_global_params$unique_simulation_folder = TRUE

  # The number of realizations to run
  default_global_params$realisation_num = 1

  # Specify how many cores to run on. Default setting here it to use all available
  default_global_params$number_of_cores = 1

  # hHw long to run the simulaton in years
  default_global_params$time_steps = 50
  
  # The time step at which development starts
  default_global_params$dev_start = 1
  
  # The time at which development ends
  default_global_params$dev_end = 50
  
  # The total number of parcel that will be developed. The number of
  # developments per time step is determined as follows: First the mean number
  # number per time step is determined, then sampling is done around this
  # mean number using a normal distribution such that the total number of
  # developments will always equal the total number (Note sd for this
  # distribution is set in the code the currently isn't user settable)
  default_global_params$total_dev_num = 200
  
  # Whether all of the outputs of the model are kept after a scenario is
  # finished. If false only data required to generate the plots is kept.
  # Setting to fale saves a lot of space
  default_global_params$save_simulation_outputs = FALSE

  default_global_params$overwrite_existing_landscape_data = TRUE
  # Use inputs previously saved in landscape inputs folder to run current simulation
  default_global_params$run_from_saved = FALSE

  # Saves the landscape data use by simulation into the run specific simulation params folder
  default_global_params$backup_simulation_inputs = TRUE

  # Create an animation of the outputs
  default_global_params$write_movie = FALSE

  # Makes a single pdf at the end of the simulation showing the locatons of all offsets and developments
  default_global_params$write_offset_layer = FALSE
  
  # The total number of layers to use in the offset calcuation (iterating from the start)
  default_global_params$features_to_use_in_offset_calc = 1

  # what subset of features to use in the simulation
  default_global_params$features_to_use_in_simulation = 1 
 
  # The maxoimum number of parcels can be selected to offset a single development
  default_global_params$max_offset_parcel_num = 10

  # Sample the restoration rates from a normal distribution to they vary per parcel and per feature
  default_global_params$sample_restoration_rate = FALSE

  # Sample the decline rates from a normal distribution to they vary per parcel and per feature
  default_global_params$sample_decline_rate = FALSE

  # Stops the offset from delivering any further gains once it has acheived the gains required
  default_global_params$limit_offset_restoration = TRUE

    # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  default_global_params$illegal_clearing_prob = 1e-3

  default_global_params$landscape_evolve_type = 'dynamic'
  
  default_global_params$rescale_ecology_to_feature_subset = TRUE
  
  # logistic decline rate means across simulation features. Sample form a normal distribution with this mean and add noise using  default_global_params$decline_rate_std
  default_global_params$mean_decline_rates = rep(list(-1e-2), length(default_global_params$features_to_use_in_simulation)) 
  
  #set this parameter to zero to yield no noise
  default_global_params$decline_rate_std = rep(list(1e-3), length(default_global_params$features_to_use_in_simulation))

  # Lowest value that the logistic decline curve can reach. It will asypotote to this value
  default_global_params$min_eco_val = 0  
  
  # Max value that the logistic decline curve can reach. It will asypotote to this value
  default_global_params$max_eco_val = 100 
  
  # set the random number seed
  default_global_params$set_seed = FALSE
  
  # Acceptable level above which to accept parcel match for offset and
  # development calcs. Positive value means offset gains greater than
  # development losses by this amount are accepted. Negative value means
  # offset value will be accepted that are smaller than dev value by this
  # ammount
  default_global_params$match_threshold = 0 
  
  # NOT CURRENTLY USED (Limit the amount of restoration to this percentage of the total available)
  default_global_params$max_restoration_eco_val = 70

  # The mean and the standard deviation of a normal distribution fro which to sample the restoration parameters from
  default_global_params$restoration_rate_params = c(0.02, 0.005)

  default_global_params$apply_offset_exclusion_layer = TRUE
  default_global_params$offset_exclusion_layer_filename = 'protected_areas.rds'
  default_global_params$apply_development_exclusion_layer = TRUE
  default_global_params$dev_exclusion_layer_filename = 'protected_areas.rds'
  
  return(default_global_params)
}



initialise_default_combination_params <- function(){ 

  # Parameters controlling offset policy settings. Note that any of these
  # parameters can take an arbitrary number of values, and the code will then
  # every combination of parameter values over all these variables. For
  # example  x=(a,b); y=(c,d) will then run the model with x=a y=c, x=a y=d,
  # x=b y=c, x=b y=d. Each of these combinations will be designated a
  # 'scenario'. There caution should be used when specifying multiple values
  # as it's easy to create a large number scenarios.

    default_combination_params = list()

  # The Options are 'restoration_gains' - the gains are calculated relative to
  # the site value at the time of the intervention above  - forces offsets to restore sites
  # 'avoided_condition_decline - the gains are calculated relative to the biodiversity
  # 'condition without the offset in place (the do nothing counterfactual) - forces offsets to maintain present value of sites
  # 'net_gains' - is the sum of the previous 2- forces offsets to restore sites
  # 'current_condition_maintain' is the present condition of the site assuming the site is maintained
  # 'current_condition_protect' is the present condition of the site assuming the site is protected
  # 'protected_condition' is the projected protected value of the site when protected i.e. the counterfactual.
  
  
  default_combination_params$offset_action_params = list(c('net_gains', 'restore'))
  
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  default_combination_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 

  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  default_combination_params$allow_developments_from_credit = TRUE
  
  # How the development parcels are selected options are 'random' or
  # 'weighted'. Note tha weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # initialise_routines.R  (or put the files in simulation_inputs)
  
  default_combination_params$development_selection_type = 'random'  

  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  default_combination_params$use_offset_bank = c(FALSE)

  # The time at which the offset in the bank offsets are first are implemented and start acurring grains, 
  default_combination_params$offset_bank_start = 1 

  # The time at which no more offsets are added to the bank. The number of
  # offsets per time step is determined as follows: First the mean number
  # number per time step is determined, then sampling is done around this
  # mean number using a normal distribution such that the total number of
  # developments will always equal the total number (Note sd for this
  # distribution is set in the code the currently isn't user settable)
  default_combination_params$offset_bank_end = 1 

  # THe number parcels to include in banking scheme. These are randomly selected.
  default_combination_params$offset_bank_num = 200 

  # Options are 'credit' or 'parcel_set'. 'credit' means there is accumulated
  # gain that is subtracted as parcels are developed. 'parcel_set' one or more
  # parcels in the bank are traded for one development site. If there is left
  # over credit (and allow_developments_from_credit is set to TRUE) then this excess credit is used on subsequent developments
  default_combination_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')     
  
  # TRUE - one-to-one selection of offset parcels for one development, FALSE =
  # many-to-one selection of offset parcels for one development
  default_combination_params$site_for_site = c(FALSE)

  # The time horizon in which the offset gains need to equal the devlopment impact
  default_combination_params$offset_time_horizon = c(15)

  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  default_combination_params$include_potential_developments_in_offset_calc = c(FALSE)

  # Include future illegal developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  default_combination_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the offset. The decreases the impact of the offset (due to
  # future gains that are avoided) - UNDER DEVELOPMENT - LEAVE SET TO FALSE
  default_combination_params$include_potential_offsets_in_offset_calc = c(FALSE)

  # include ability to set the counterfactual adjustment (include/exclude illegal clearing, 
  # potential developments, and potential offsets) to be the same as the offset calculations or independent
  # settings are 'as_offset' or 'independent_to_offset'
  
  default_combination_params$dev_counterfactual_adjustment = 'as_offset' 
  
  # Include future developments in calculating contribution of avoided losses
  # to the impact of the development. This reduces the development impact because
  # projected future value of the site is lower if there is some probability
  # the site may be developed in the future
  # default_combination_params$include_potential_developments_in_dev_calc = c(FALSE)

  # Include illegal clearing in the calculating the contribution of avoided
  # losses to the impact of the development. This reduces the development
  # impact because projected future value of the site is lower if there is
  # some probability the site may be illegally developed in the future
  # default_combination_params$include_illegal_clearing_in_dev_calc = default_combination_params$include_illegal_clearing_in_offset_calc

  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the development. This increases the impact of the development as
  # future gains are avoided
  
  # default_combination_params$include_potential_offsets_in_dev_calc = c(FALSE)
  
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  default_combination_params$offset_multiplier = 1

  return(default_combination_params)
}



