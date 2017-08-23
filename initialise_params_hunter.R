initialise_run_params <- function(){
  run_params = list()
  run_params$overwrite_default_params = TRUE
  run_params$simulation_folder = paste0(path.expand('~'), '/offset_data/hunter/')
  run_params$simulate_data = FALSE
  run_params$realisation_num = 1
  run_params$crs = 4 #detectCores(all.tests = FALSE, logical = TRUE)
  run_params$time_steps = 50
  run_params$save_simulation_outputs = TRUE
  run_params$overwrite_existing_landscape_data = FALSE
  run_params$run_from_saved = TRUE # run from previous data or run from newly generated ecology etc.
  run_params$backup_simulation_inputs = FALSE 
  run_params$write_movie = FALSE            # write outputs to movie
  run_params$write_offset_layer = TRUE     # write layer containing all offset parcels to pdf
  run_params$test_a = 1
  
  run_params$features_to_use_in_offset_calc = 2:4
  run_params$features_to_use_in_simulation = 2:11
  run_params$test_b = 1
  run_params$max_offset_parcel_num = 5 #how many parcels can be selected to offset a single development
  run_params$sample_restoration_rate = FALSE
  run_params$sample_decline_rate = FALSE
  run_params$test_c = 1
  run_params$limit_offset_restoration = TRUE
  run_params$illegal_clearing_prob = 1e-3
  
  run_params$mean_decline_rates = rep(list(-1e-2), length(run_params$features_to_use_in_simulation)) 
  run_params$decline_rate_std = rep(list(1e-3), length(run_params$features_to_use_in_simulation))

  run_params$min_eco_val = 0  
  run_params$max_eco_val = 100 
  
  run_params$screen_parcels = TRUE # do not use parcels outside [0.05, 0.95]
  run_params$parcel_screen_size = 20 # ignore parcels with less than ecology_params$parcel_screen_size elements
  run_params$set_seed = FALSE
  run_params$match_threshold = 0 # acceptable level above which to accept parcel match

  run_params$restoration_rate_params = c(0.02, 0.005)

  return(run_params)
}


initialise_policy_params <- function(){ #list of variations in policy
  policy_params = list()
  policy_params$offset_action_type = c('maintain')
  policy_params$allow_developments_from_credit = TRUE

  policy_params$dev_start = 1
  policy_params$dev_end = 50
  policy_params$total_dev_num = 200
  
  policy_params$use_offset_bank = c(FALSE) # FALSE - perform offsets simultaneously with development, TRUE - perform offset banking prior to development according to offset bank parameters 
  policy_params$offset_bank_type = c('credit') # c('parcel_set', 'credit')       #'parcel_set' - select discrete land parcels or 'credit' - subtract value from total accumulated gains
  policy_params$offset_bank_start = 1 #min time for offset banking to initialise
  policy_params$offset_bank_end = 1 #mix time for offset banking to finish
  policy_params$offset_bank_num = 200 # how many parcels to include in banking scheme
  
  policy_params$development_selection_type = 'random'  #how the development parcels are selected - 'random' or 'weighted'
  
  policy_params$site_for_site = c(FALSE) # TRUE - one-to-one selection of offset parcels for one development, FALSE = many-to-one selection of offset parcels for one development
  policy_params$offset_time_horizon = c(30)
  policy_params$offset_calc_type = c('avoided_loss') #'net_gains', 'restoration_gains', 'avoided_loss' 
  policy_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 
  
  policy_params$include_potential_developments_in_offset_calc = c(TRUE)
  policy_params$include_potential_offsets_in_offset_calc = c(FALSE)
  policy_params$include_illegal_clearing_in_offset_calc = c(TRUE)
  
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

