initialise_run_params <- function(){
  run_params = list()
  run_params$data_type = 'simulated'

  run_params$crs = detectCores(all.tests = FALSE, logical = TRUE)
  
  run_params$save_realisations = TRUE 
  run_params$save_procedure = 'by_single_realisation' #'realisations_block', 'by_single_realisation', 'by_time_slice'
  
  run_params$collate_realisations = TRUE #TRUE, FALSE to perform collate routines as well as running the simulation or not
  run_params$save_collated_realisations = TRUE
  run_params$plot_impacts = TRUE #if collating realisations switch on to see impacts 
  run_params$run_from_saved = FALSE                                   # run from previous data or run from newly generated ecology etc.
  run_params$save_initial_conditions = FALSE                             # use this to run from simulated data (calculated at the initialisation of the code) or load data (eg from zonation etc)
  run_params$write_movie = FALSE                                      # write evolving ecology to movie
  run_params$show_movie = FALSE                                      # show output in movie form of evolving ecology
  run_params$write_offset_layer = FALSE                                    # write layer containing all offset parcels to pdf
  
  run_params$realisation_num = 5
  run_params$time_steps = 50
  run_params$max_offset_parcel_num = 5 #how many parcels can be selected to offset a single development

  run_params$spread_restoration_rate = FALSE
  run_params$limit_offset_restoration = TRUE
  run_params$perform_illegal_clearing = TRUE # switch on/off illegal clearing (this can occur in offset regions or in available regions)
  run_params$sample_decline_rate = FALSE
  run_params$offset_thresh = 50
  run_params$screen_parcels = TRUE # do not use parcels outside [0.05, 0.95]
  run_params$parcel_screen_size = 20 #ignore parcels with less than ecology_params$parcel_screen_size elements
  run_params$set_seed = FALSE
  run_params$match_threshold = 0 # acceptable level above which to accept parcel match
  run_params$illegal_clearing_prob = 1e-3
   

  return(run_params)
}

initialise_ecology_params <- function(){
  
  ecology_params = list()
  ecology_params$eco_dims = 1 #how many ecological dimensions in simulation
  ecology_params$dims_to_use = seq(ecology_params$eco_dims)
  ecology_params$region_num = 1
  ecology_params$region_num_x = 1
  ecology_params$region_num_y = 1
  ecology_params$mean_decline_rates = rep(-1e-2, ecology_params$eco_dims) #set parameter for rate of decline according to logistic curve
  ecology_params$decline_rate_std = rep(1e-3, ecology_params$eco_dims)
  
  ecology_params$ecology_size = 300 #ecology array size (square dimension) to be broken up into regions and land parcels  
  ecology_params$parcel_num_x = 40 #numnber of parcels in x
  ecology_params$parcel_num_y = 40 #numnber of parcels in x
  ecology_params$min_eco_val = 0  #minimum allowable ecological value of smallest ecological element (pixel)
  ecology_params$max_eco_val = 100 #maximum "   "     "           "
  ecology_params$max_restoration_eco_val = 70
  ecology_params$min_initial_eco_val = 20 #minimum allowable initial ecological value of smallest ecological element (pixel)
  ecology_params$max_initial_eco_val = 90 #maximum "   "     "           "
  ecology_params$initial_eco_noise = 10 #how much initial variation in pixels per land parcel 
  ecology_params$restoration_rate_params = c(0.02, 0.005)
  
  return(ecology_params)
  
}


initialise_policy_params <- function(){ #list of variations in policy
  policy_params = list()
  policy_params$offset_bank_type = c('credit') #c('parcel_set', 'credit')       #'parcel_set' - select discrete land parcels or 'credit' - subtract value from total accumulated gains
  policy_params$offset_region = 'development' # force offsets to be in same region as development
  policy_params$use_offset_bank = c(FALSE) # FALSE - perform offsets simultaneously with development, TRUE - perform offset banking prior to development according to offset bank parameters 
  policy_params$offset_bank_start = 1 #min time for offset banking to initialise
  policy_params$offset_bank_end = 1 #mix time for offset banking to finish
  policy_params$offset_bank_num = 700 # how many parcels to include in banking scheme
  
  policy_params$development_selection_type = 'random'  #how the development parcels are selected - 'random' or 'weighted'
  policy_params$offset_parcel_for_parcel = c(FALSE) # TRUE - one-to-one selection of offset parcels for one development, FALSE = many-to-one selection of offset parcels for one development
  policy_params$offset_time_horizon = c(15)
  policy_params$offset_calc_type = c('net_gains') #'net_gains', 'restoration_gains', 'avoided_degs' 
  policy_params$dev_calc_type = c('future_condition')    #'future_condition', 'current_condition' 
  
  policy_params$include_potential_developments_in_offset_calc = c(FALSE)
  policy_params$include_potential_offsets_in_offset_calc = c(FALSE)
  policy_params$include_illegal_clearing_in_offset_calc = c(FALSE)
  
  policy_params$dev_counterfactual_type = 'offset_counterfactual'
  policy_params$illegal_clearing_prob = 1e-3
  
  policy_params$offset_multiplier = 1
  policy_params$offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
  policy_params$offset_action_type = c('restore')
  policy_params$use_parcel_set_dev_credit = TRUE
  policy_params$dev_start = 1
  policy_params$dev_end = 50
  policy_params$total_dev_num = 100
  return(policy_params)
}
