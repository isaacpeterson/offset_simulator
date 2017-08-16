initialise_run_params <- function(){
  run_params = list()

  # Where simulation outputs will be written
  run_params$simulation_folder = paste0(path.expand('~'), '/offset_data/simulated/')
 
  run_params$simulate_data = TRUE
  
  # set the random number seed
  run_params$set_seed = FALSE

  # The number of realizations to run
  run_params$realisation_num = 2

  # Specify how many cores to run on. Default setting here it to use all available
  run_params$crs = detectCores(all.tests = FALSE, logical = TRUE)

  # hHw long to run the simulaton in years
  run_params$time_steps = 50
  
  # Whether all of the outputs of the model are kept after a scenario is
  # finished. If false only data required to generate the plots is kept.
  # Setting to fale saves a lot of space
  run_params$save_simulation_outputs = FALSE

  # Use inputs previously saved in landscape inputs folder to run current simulation
  run_params$run_from_saved = FALSE

  # Saves the landscape data use by simulation into the run specific simulations input folder 
  run_params$backup_landscape_data = FALSE

  # Create an animation of the outputs
  run_params$write_movie = FALSE

  # Makes a single pdf at the end of the simulation showing the locatons of all offsets
  run_params$write_offset_layer = FALSE
  
  # The total number of layers to use in the offset calcuation (iterating from the start)
  run_params$features_to_use = 1

  # The total number of features in the simulation
  run_params$feature_num = 1 

  # The maxoimum number of parcels can be selected to offset a single development
  run_params$max_offset_parcel_num = 5

  # Sample the restoration rates from a normal distribution to they vary per parcel and per feature
  run_params$sample_restoration_rate = FALSE

  # Sample the decline rates from a normal distribution to they vary per parcel and per feature
  run_params$sample_decline_rate = FALSE

  # Stops the offset from delivering any further gains once it has acheived the gains required
  run_params$limit_offset_restoration = TRUE

    # The probability per parcel of it being illegally cleared, every parcel gets set to this number - set to zero to turn off
  run_params$illegal_clearing_prob = 1e-3

  # Excludes the top and bottom 5% of parcels in terms of their biodiversity values soo keeps values between [0.05, 0.95]
  run_params$screen_parcels = TRUE 

  # Exclude parcels with less than this number of pixels.
  run_params$parcel_screen_size = 20 

  # Acceptable level above which to accept parcel match for offset and
  # development calcs. Positive value means offset gains greater than
  # development losses by this amount are accepted. Negative value means
  # offset value will be accepted that are smaller than dev value by this
  # ammount
  run_params$match_threshold = 0 
  
  # NOT CURRENTLY USED (Limit the amount of restoration to this percentage of the total available)
  run_params$max_restoration_eco_val = 70

  # The mean and the standard deviation of a normal distribution fro which to sample the restoration parameters from
  run_params$restoration_rate_params = c(0.02, 0.005)

  return(run_params)
}



initialise_policy_params <- function(){ #list of variations in policy
  policy_params = list()
  policy_params$offset_action_type = c('restore')
  policy_params$allow_developments_from_credit = TRUE
  
  policy_params$dev_start = 1
  policy_params$dev_end = 50
  policy_params$total_dev_num = 50
  
  policy_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')       #'parcel_set' - select discrete land parcels or 'credit' - subtract value from total accumulated gains
  policy_params$use_offset_bank = c(FALSE) # FALSE - perform offsets simultaneously with development, TRUE - perform offset banking prior to development according to offset bank parameters 
  policy_params$offset_bank_start = 1 #min time for offset banking to initialise
  policy_params$offset_bank_end = 1 #mix time for offset banking to finish
  policy_params$offset_bank_num = 200 # how many parcels to include in banking scheme
  
  policy_params$development_selection_type = 'random'  #how the development parcels are selected - 'random' or 'weighted'
  policy_params$site_for_site = c(FALSE) # TRUE - one-to-one selection of offset parcels for one development, FALSE = many-to-one selection of offset parcels for one development
  policy_params$offset_time_horizon = c(15)
  policy_params$offset_calc_type = c('net_gains') #'net_gains', 'restoration_gains', 'avoided_degs' 
  policy_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 
  
  policy_params$include_potential_developments_in_offset_calc = c(FALSE)
  policy_params$include_potential_offsets_in_offset_calc = c(FALSE)
  policy_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  policy_params$include_potential_developments_in_dev_calc = policy_params$include_potential_developments_in_offset_calc
  policy_params$include_potential_offsets_in_dev_calc = policy_params$include_potential_offsets_in_offset_calc
  policy_params$include_illegal_clearing_in_dev_calc = policy_params$include_illegal_clearing_in_offset_calc
  
  policy_params$offset_multiplier = 1
  if (policy_params$use_offset_bank == TRUE){
    policy_params$offset_time_horizon_type = 'current'  # 'current' - used for banking only - determine accrued offset gains till current year.
  } else {
    policy_params$offset_time_horizon_type = 'future' #'future' - project from time of development to offset time horizon, or 
  }
  

  return(policy_params)
}




initialise_simulated_ecology_params <- function(){
  
  # Construct the static initial landscape 

  simulated_ecology_params = list()

  # Number of pixels in (y, x) for the feature layes 
  simulated_ecology_params$ecology_size = c(300, 400)

  # Numnber of parcels in x (but total size varies)
  simulated_ecology_params$parcel_num_x = 30 

  # Numnber of parcels in y (but total size varies)
  simulated_ecology_params$parcel_num_y = 40 

  # Minimum allowable initial ecological value of smallest ecological element
  # (pixel) ie min value to sample from
  simulated_ecology_params$min_initial_eco_val = 20

  # Max allowable initial ecological value of largest element (pixel) ie max
  # value to sample from
  simulated_ecology_params$max_initial_eco_val = 90

  # Mow much initial variation in pixels per land parcel (this is the width of
  # uniform dist) used to add noise to each pixel. Eg if the pixel has a vlaue
  # of 35, a new value will be sampled from between 35-45
  simulated_ecology_params$initial_eco_noise = 10

  # Defining multiple regions eg different states where different polcies can apply 
  simulated_ecology_params$region_num_x = 1

  # Defining multiple regions eg different states where different rules can apply 
  simulated_ecology_params$region_num_y = 1

  return(simulated_ecology_params)
}


initialise_ecology_params <- function(run_params){

  # Set up how the landscape changes
  
  ecology_params = list()

  # Set parameter for rate of decline curve according to logistic curve, need
  # to mathc the number of features. Negative value means decline poltive
  # value means improvement
  
  ecology_params$mean_decline_rates = rep(-1e-2, run_params$feature_num) 

  # Sample form a normal distribution with this sd to add noise to the decline rates.
  ecology_params$decline_rate_std = rep(1e-3, run_params$feature_num)

  # Lowest value that the logistic decline curve can reach. It will asypotote to this value
  ecology_params$min_eco_val = 0  

  # Max value that the logistic decline curve can reach. It will asypotote to this value
  ecology_params$max_eco_val = 100 
  
  return(ecology_params)
  
}
