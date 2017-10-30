initialise_run_params <- function(){
  run_params = list()

  run_params$simulation_folder = paste0(path.expand('~'), '/offset_data/simulated/')

  run_params$realisation_num = 1

  run_params$total_dev_num = 250

  run_params$dev_start = 1

  run_params$dev_end = 50

  run_params$crs = detectCores(all.tests = FALSE, logical = TRUE)

  run_params$time_steps = 50

  run_params$write_offset_layer = FALSE
  
  run_params$max_offset_parcel_num = 1

  run_params$limit_offset_restoration = TRUE

   run_params$illegal_clearing_prob = 0

   run_params$screen_offset_sites_by_size = TRUE 

   run_params$screen_dev_sites_by_size = TRUE
  
 run_params$site_screen_size = 50 

  run_params$screen_offset_site_zeros = TRUE
  
   run_params$illegal_clearing_prob = 0
  # The mean and the standard deviation of a normal distribution from which to sample the restoration parameters from
  run_params$restoration_rate_params = c(0.02, 0.005)
  run_params$features_to_use_in_simulation = 1
  run_params$mean_decline_rates = rep(list(1), length(run_params$features_to_use_in_simulation)) 
  run_params$decline_rate_std = rep(list(0), length(run_params$features_to_use_in_simulation))
  
  return(run_params)
}



initialise_policy_params <- function(){ 



  policy_params = list()

  policy_params$offset_action_params = list(c('current_condition', 'maintain')) #, 'restoration_gains', 'avoided_loss', 'current_condition'

  policy_params$dev_calc_type = c('current_condition')    #'future_condition', 'current_condition' 

  policy_params$allow_developments_from_credit = TRUE
  
  policy_params$development_selection_type = 'random'  

  policy_params$use_offset_bank = c(FALSE)

  policy_params$offset_bank_start = 1 

  policy_params$offset_bank_end = 1 

  policy_params$offset_bank_num = 200 

  policy_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')     
  
  policy_params$offset_time_horizon = c(15)#, 30)

  policy_params$include_potential_developments_in_offset_calc = c(FALSE)

  policy_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  policy_params$include_potential_offsets_in_offset_calc = c(FALSE)
  
  policy_params$dev_counterfactual_adjustment = 'as_offset'

  policy_params$offset_multiplier = 1

  return(policy_params)

}

