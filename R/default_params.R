initialise_default_global_params <- function(){
  default_global_params = list()
  
  # Set the random number seed
  default_global_params$set_seed = FALSE
  
  # When specifying multiple scenarios using initialise_default_simulation_params,
  # the param can be used to force a single scenario to be run. Set to 'all' to 
  # run all scenarios.
  default_global_params$scenario_subset = 'all'
  
  # If set to 'defaul', the values in set in
  # initialise_default_simulated_ecology_params() are used. Otherwise this can
  # be set to point to a file, that contains the initialise_default_simulated_ecology_params() 
  # function.
  # TODO(Isaac): check whether all params are needed or whether only the ones
  # that are overwritten need to be specified
  default_global_params$user_simulated_ecology_params_file = 'default'  
  
  # TODO(Isaac): This may now be obsolete, Isaac to check and then remove 
  default_global_params$overwrite_default_params = TRUE
  
  # Where simulation outputs will be written
  default_global_params$simulation_folder = 'default'
 
  # Whether the package is to be used to generate simulated data. If this is
  # FALSE then the it will look in the
  # default_global_params$unique_simulation_folder to get the required
  # infromation
  default_global_params$use_simulated_data = TRUE
  
  # Fix the output directory (will overwrite existing files) instead of creating unique 
  default_global_params$unique_simulation_folder = TRUE

  # The number of realizations to run
  default_global_params$realisation_num = 1

  # Specify how many cores to run on. Default setting here it to use all available
  default_global_params$number_of_cores = 'all'

  # Whether all of the outputs of the model are kept after a scenario is
  # finished. If false only data required to generate the plots is kept.
  # Setting to FALSE saves a lot of disk space
  default_global_params$save_simulation_outputs = FALSE

  # When TRUE, input data will be regenerated when a new run is done. If
  # FALSE, it will use the same input data as the previous run.
  # TODO(Isaac): chanfge this name to make clear it only applies to simulated data
  default_global_params$overwrite_existing_landscape_data = TRUE

  # Use inputs previously saved in landscape inputs folder to run current simulation
  default_global_params$run_from_saved = FALSE

  # Saves all the initialization data use by simulation into the run specific simulation params folder
  default_global_params$backup_simulation_inputs = TRUE

  # Create an animation of the outputs
  default_global_params$write_movie = FALSE

  # Makes a single pdf at the end of the simulation showing the locatons of all offsets and developments
  default_global_params$write_offset_layer = FALSE
  
  return(default_global_params)
}



initialise_default_simulation_params <- function(){ 

  # Parameters controlling offset policy settings. Note that any of these
  # parameters can take an arbitrary number of values, and the code will then
  # every combination of parameter values over all these variables. To do this the user
  # must pass the variable combinations in as a list structure 
  # example  x=list(a,b); y=list(c,d) will then run the model with x=a y=c, x=a y=d,
  # x=b y=c, x=b y=d. Each of these combinations will be designated a
  # 'scenario'. There caution should be used when specifying multiple values
  # as it's easy to create a large number scenarios.

    default_simulation_params = list()

    # how long to run the simulaton in years
    default_simulation_params$time_steps = 50
    
    # The time step at which development starts
    default_simulation_params$dev_start = 1
    
    # The time at which development ends
    default_simulation_params$dev_end = 50
    
    # The total number of parcel that will be developed. The number of
    # developments per time step is determined as follows: First the mean number
    # number per time step is determined, then sampling is done around this
    # mean number using a normal distribution such that the total number of
    # developments will always equal the total number (Note sd for this
    # distribution is set in the code the currently isn't user settable)
    default_simulation_params$total_dev_num = 5
    
    # What subset of features to use in the simulation (specified by the index
    # of the feature e.g. c(1,4,13)
    default_simulation_params$features_to_use_in_simulation = 1 
    
    # The total number of layers to use in the offset calcuation (iterating from the start)
    default_simulation_params$features_to_use_in_offset_calc = 1
    
    # What features are affected by the offset intervention
    default_simulation_params$features_to_use_in_offset_intervention = default_simulation_params$features_to_use_in_offset_calc

    # The maxoimum number of parcels can be selected to offset a single development
    default_simulation_params$max_offset_parcel_num = 10
    
    # Sample the restoration rates from a normal distribution to they vary per parcel and per feature
    default_simulation_params$sample_restoration_rate = FALSE
    
    # Sample the decline rates from a normal distribution to they vary per parcel and per feature
    default_simulation_params$sample_decline_rate = FALSE
    
    # Stops the offset from delivering any further gains once it has acheived the gains required
    default_simulation_params$limit_offset_restoration = TRUE
    
    # The probability per parcel of it being stochasticly cleared, every parcel gets set to this number - set to zero to turn off
    default_simulation_params$stochastic_loss_prob = 1e-3
    
    # Lowest value that the logistic decline curve can reach. It will asypotote to this value
    default_simulation_params$min_eco_val = 0  
    
    # Max value that the logistic decline curve can reach. It will asypotote to this value
    default_simulation_params$max_eco_val = 100 
    
    #ignore offset sites with zero value
    default_simulation_params$screen_offset_zeros = TRUE
    
    # ignore development sites with zero value
    default_simulation_params$screen_dev_zeros = TRUE
    
    # ignore parcels with size below this number of elements 
    default_simulation_params$site_screen_size = 0 
    
    default_simulation_params$match_threshold_ratio = 0.01 
    
    default_simulation_params$match_threshold_noise = 1e-10
    # NOT CURRENTLY USED (Limit the amount of restoration to this percentage of the total available)
    default_simulation_params$max_restoration_eco_val = 70
    
    # The mean and the standard deviation of a normal distribution fro which to sample the restoration parameters from
    default_simulation_params$restoration_rate = 0.02
    default_simulation_params$restoration_rate_std = 0.005
    
  # The Options are 'restoration_gains' - the gains are calculated relative to
  # the site value at the time of the intervention above  - forces offsets to restore sites
  # 'avoided_condition_decline - the gains are calculated relative to the biodiversity
  # 'condition without the offset in place (the do nothing counterfactual) - forces offsets to maintain present value of sites
  # 'net_gains' - is the sum of the previous 2- forces offsets to restore sites
  # 'current_condition_maintain' is the present condition of the site assuming the site is maintained
  # 'current_condition_protect' is the present condition of the site assuming the site is protected
  # 'protected_condition' is the projected protected value of the site when protected i.e. the counterfactual.
  
  # parameters to control the offset calculation and how the intervention is implemented
  # later internally processed into two additional parameters as (offset_calc_type, offset_action_type)
    
  
  default_simulation_params$offset_action_params = c('net_gains', 'restore')
  
  # This is the equivalent of offset_calc_type for the dev parcel. Options
  # are: 'current_condition' - losses are calcuated relative to the value of
  # the site at the time of the intervention 
  # 'future_condition' - is the do nothing trjectory of the development site.
  default_simulation_params$dev_calc_type = 'future_condition'    #'future_condition', 'current_condition' 

  # Track accumulated credit from previous exchanges (eithger in current or
  # previous time step) and use them to allow developments to proceed if the
  # credit is large enough. FALSE means ignore any exces credit from offset exchanges
  default_simulation_params$allow_developments_from_credit = TRUE
  
  # How the development parcels are selected options are 'random' or
  # 'weighted'. Note tha weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # initialise_routines.R  (or put the files in simulation_inputs)
  
  default_simulation_params$development_selection_type = 'random'  

  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  default_simulation_params$use_offset_bank = FALSE

  # The time at which the offset in the bank offsets are first are implemented and start acurring grains, 
  default_simulation_params$offset_bank_start = 1 

  # The time at which no more offsets are added to the bank. The number of
  # offsets per time step is determined as follows: First the mean number
  # number per time step is determined, then sampling is done around this
  # mean number using a normal distribution such that the total number of
  # developments will always equal the total number (Note sd for this
  # distribution is set in the code the currently isn't user settable)
  default_simulation_params$offset_bank_end = 1 

  # THe number parcels to include in banking scheme. These are randomly selected.
  default_simulation_params$offset_bank_num = 200 

  # Options are 'credit' or 'parcel_set'. 'credit' means there is accumulated
  # gain that is subtracted as parcels are developed. 'parcel_set' one or more
  # parcels in the bank are traded for one development site. If there is left
  # over credit (and allow_developments_from_credit is set to TRUE) then this excess credit is used on subsequent developments
  default_simulation_params$offset_bank_type = 'credit'     
  
  # TRUE - one-to-one selection of offset parcels for one development, FALSE =
  # many-to-one selection of offset parcels for one development
  default_simulation_params$site_for_site = FALSE

  # The time horizon in which the offset gains need to equal the devlopment impact
  default_simulation_params$offset_time_horizon = 15

  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  default_simulation_params$include_potential_developments_in_offset_calc = FALSE

  # Include future stochastic developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  default_simulation_params$include_stochastic_loss_in_offset_calc = FALSE
  
  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the offset. The decreases the impact of the offset (due to
  # future gains that are avoided) - UNDER DEVELOPMENT - LEAVE SET TO FALSE
  default_simulation_params$include_potential_offsets_in_offset_calc = FALSE

  # include ability to set the counterfactual adjustment (include/exclude stochastic clearing, 
  # potential developments, and potential offsets) to be the same as the offset calculations or independent
  # settings are 'as_offset' or 'independent_to_offset'
  
  default_simulation_params$dev_counterfactual_adjustment = 'as_offset' 
  
  # Include future developments in calculating contribution of avoided losses
  # to the impact of the development. This reduces the development impact because
  # projected future value of the site is lower if there is some probability
  # the site may be developed in the future
  
  # default_simulation_params$include_potential_developments_in_dev_calc = FALSE

  # Include stochastic clearing in the calculating the contribution of avoided
  # losses to the impact of the development. This reduces the development
  # impact because projected future value of the site is lower if there is
  # some probability the site may be stochasticly developed in the future
  
  # default_simulation_params$include_stochastic_loss_in_dev_calc = default_simulation_params$include_stochastic_loss_in_offset_calc

  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the development. This increases the impact of the development as
  # future gains are avoided
  
  # default_simulation_params$include_potential_offsets_in_dev_calc = FALSE
  
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  default_simulation_params$offset_multiplier = 1

  return(default_simulation_params)
}

initialise_default_simulated_ecology_params <- function(){
  
  # Construct the static initial landscape 
  
  default_simulated_ecology_params = list()
  
  #how many feature layers to generate
  default_simulated_ecology_params$feature_num = 1
  
  # logistic decline rate means across simulation features. Sample form a normal distribution with this mean and add noise using  default_simulation_params$decline_rate_std
  default_simulated_ecology_params$mean_decline_rates = list(rep(list(-1e-2), length(default_simulated_ecology_params$features_to_use_in_simulation)) )
  
  #set this parameter to zero to yield no noise
  default_simulated_ecology_params$decline_rate_std = list(rep(list(1e-3), length(default_simulated_ecology_params$features_to_use_in_simulation)))
  
  # Number of pixels in (y, x) for the feature layes 
  default_simulated_ecology_params$ecology_size = c(300, 300)
  
  # Numnber of parcels in x (but total size varies)
  default_simulated_ecology_params$parcel_num_x = 30 
  
  # Numnber of parcels in y (but total size varies)
  default_simulated_ecology_params$parcel_num_y = 30 
  
  #how much the site dimensions should vary
  
  default_simulated_ecology_params$site_width_variation_param = 1
  # Minimum allowable initial ecological value of smallest ecological element
  # (pixel) ie min value to sample from
  default_simulated_ecology_params$min_initial_eco_val = 20
  
  # Max allowable initial ecological value of largest element (pixel) ie max
  # value to sample from
  default_simulated_ecology_params$max_initial_eco_val = 90
  
  # list of length equal to feature number defining proportion of parcels occupied by the feature(s) 
  default_simulated_ecology_params$occupation_ratio = list(1)
  
  # Mow much initial variation in pixels per land parcel (this is the width of
  # uniform dist) used to add noise to each pixel. Eg if the pixel has a vlaue
  # of 35, a new value will be sampled from between 35-45
  default_simulated_ecology_params$initial_eco_noise = 10
  
  # Defining multiple regions eg different states where different polcies can apply 
  default_simulated_ecology_params$region_num_x = 1
  
  # Defining multiple regions eg different states where different rules can apply 
  default_simulated_ecology_params$region_num_y = 1
  
  return(default_simulated_ecology_params)
}


initialise_default_plot_params <- function(base_folder){
  default_plot_params = list()
  default_plot_params$output_plot_folder = vector()
  default_plot_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  default_plot_params$output_type = 'scenarios' # set to plot through 'features', 'scenarios' or 'site_sets'
  default_plot_params$realisation_num = 'all' # 'all' or number to plot
  default_plot_params$write_pdf = FALSE
  default_plot_params$sets_to_plot = 1 # example site to plot
  default_plot_params$scenario_vec = 'all' #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  default_plot_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  default_plot_params$program_col_vec = c('darkgreen', 'red', 'black') 
  default_plot_params$cfac_col = 'blue' 
  default_plot_params$landscape_col = 'black'
  default_plot_params$lwd_vec = c(3, 0.5)
  default_plot_params$plot_subset_type = 'all'
  default_plot_params$plot_subset_param = 'all'
  
  default_plot_params$plot_site_offset = TRUE 
  default_plot_params$plot_site_dev = TRUE
  default_plot_params$plot_site_net = TRUE
  default_plot_params$plot_site = TRUE
  default_plot_params$plot_program = TRUE
  default_plot_params$plot_landscape = TRUE
  default_plot_params$features_to_plot = 'all'
  default_plot_params$site_impact_lwd = 0.5
  default_plot_params$site_outcome_lwd_vec = c(0.5)
  default_plot_params$program_lwd_vec = c(3, 0.5)
  default_plot_params$program_outcome_lwd_vec = c(3, 0.5)
  default_plot_params$landscape_lwd_vec  = c(3)
  default_plot_params$landscape_outcome_lwd_vec = c(3)
  
  default_plot_params$string_width = 3 # how many digits are used to store scenario index and realisation index
  default_plot_params$nx = 3 
  default_plot_params$ny = 4
  
  default_plot_params$site_outcome_plot_lims_set = rep(list(c(0, 3e4)), length(default_plot_params$scenario_vec))
  default_plot_params$program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), length(default_plot_params$scenario_vec))
  default_plot_params$landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), length(default_plot_params$scenario_vec))
  
  default_plot_params$site_impact_plot_lims_set = rep(list(c(-1e3, 1e3)), length(default_plot_params$scenario_vec))
  default_plot_params$program_impact_plot_lims_set = rep(list(c(-1e6, 1e6)), length(default_plot_params$scenario_vec)) 
  default_plot_params$landscape_impact_plot_lims_set = rep(list(c(-1e6, 0)), length(default_plot_params$scenario_vec))
  
  return(default_plot_params)
}

