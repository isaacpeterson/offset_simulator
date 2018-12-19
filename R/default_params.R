initialise_default_global_params <- function(){
  default_global_params = list()

  
  # Set the random number seed
  default_global_params$set_seed = FALSE

  default_global_params$feature_raster_files = 'default'
  default_global_params$condition_class_raster_files = 'default'
  default_global_params$numeric_placeholder_width = 3 # how many digits are used to store scenario index and realisation index
  # When specifying multiple scenarios using initialise_default_simulation_params,
  # the param can be used to force a single scenario to be run. Set to 'all' to 
  # run all scenarios.
  
  default_global_params$planning_units_raster = 'default'
  default_global_params$scenario_subset = 'all'
  
  default_global_params$raster_file_type = '.tif'
  default_global_params$output_raster_tiff = TRUE
  # If set to 'defaul', the values in set in
  # initialise_default_feature_params() are used. Otherwise this can
  # be set to point to a file, that contains the initialise_default_feature_params() 
  # function.
  # TODO(Isaac): check whether all params are needed or whether only the ones
  # that are overwritten need to be specified
  default_global_params$user_feature_params_file = 'default'  
  
  # Where simulation outputs will be written
  default_global_params$simulation_folder = 'default'
 
  # Whether the package is to be used to generate simulated data. If this is
  # FALSE then the it will look in the
  # default_global_params$unique_simulation_folder to get the required
  # infromation
  
  default_global_params$run_from_simulated_data = TRUE
  
  # build simulated data on the fly (or not)
  default_global_params$build_simulated_data = TRUE
  
  # Fix the output directory (will overwrite existing files) instead of creating unique 
  default_global_params$unique_simulation_folder = TRUE

  # The number of realizations to run
  default_global_params$realisation_num = 1

  # Specify how many cores to run on. Default setting here it to use single core
  default_global_params$number_of_cores = 1
  
  default_global_params$store_zeros_as_sparse = FALSE
  
  # saves all raw data. Whether all of the outputs of the model are kept after a scenario is
  # finished. If false only data required to generate the plots is kept.
  # Setting to FALSE saves a lot of disk space
  default_global_params$save_simulation_outputs = TRUE

  # Saves all the initialization data use by simulation into the run specific simulation params folder
  default_global_params$backup_simulation_inputs = TRUE
  
  default_global_params$overwrite_site_characteristics = FALSE
  
  default_global_params$overwrite_site_features = FALSE
  default_global_params$overwrite_condition_class_layers = FALSE
  
  default_global_params$save_feature_dynamics = FALSE
  default_global_params$save_management_dynamics = FALSE
  default_global_params$overwrite_dev_probability_list = FALSE
  default_global_params$overwrite_offset_probability_list = FALSE
  default_global_params$overwrite_unregulated_probability_list = FALSE
  default_global_params$overwrite_management_dynamics = FALSE
  default_global_params$overwrite_feature_dynamics = FALSE
  default_global_params$overwrite_condition_classes = FALSE
  
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
    default_simulation_params$transform_params = vector()
    # how long to run the simulaton in years
    default_simulation_params$time_steps = 50
    default_simulation_params$intervention_num = 50
    
    # when the interventions are set to take place, in this case force to occur once per year
    intervention_locs = seq(1, default_simulation_params$time_steps, 1)
    default_simulation_params$development_control = array(0, default_simulation_params$time_steps)
    default_simulation_params$development_control[intervention_locs] = 1
    
    default_simulation_params$features_to_use_in_simulation = 1
    
    # The total number of layers to use in the offset calcuation (iterating from the start)
    default_simulation_params$features_to_use_in_offset_calc = 1
    
    # What features are affected by the offset intervention
    default_simulation_params$features_to_use_in_offset_intervention = default_simulation_params$features_to_use_in_offset_calc
    
    # The maxoimum number of parcels can be selected to offset a single development
    default_simulation_params$max_offset_parcel_num = 10
    
    # Stops the offset from delivering any further gains once it has acheived the gains required
    default_simulation_params$limit_offset_restoration = TRUE
    
    # The probability per parcel of it being stochasticly cleared, every parcel gets set to this number - set to zero to turn off
    default_simulation_params$unregulated_loss_prob = 0
    
    # Lowest value that the logistic decline curve can reach. It will asypotote to this value
    default_simulation_params$min_eco_val = 0  
    
    # Max value that the logistic decline curve can reach. It will asypotote to this value
    default_simulation_params$max_eco_val = 100 
    
    #ignore offset sites with zero value
    default_simulation_params$screen_offset_zeros = TRUE
    
    # ignore development sites with zero value
    default_simulation_params$screen_dev_zeros = TRUE
    
    # ignore parcels with size below this number of elements 
    default_simulation_params$min_site_screen_size = 0 
    
    # ignore parcels with size below this number of elements 
    default_simulation_params$max_site_screen_size_quantile = 1
    
    default_simulation_params$match_threshold_ratio = 0.01 
    
    default_simulation_params$match_threshold_noise = 1e-10

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
  
  #use a specified offset metric in the site match calculation
  default_simulation_params$use_offset_metric = FALSE
  
  # How the development/offset parcels are selected options are 'stochastic',
  # 'weighted', or 'pre_determined'. Note that weighted requires an additonal weighting layer. If
  # you are running on your own data you need to specify the weights file in
  # initialise_routines.R  (or put the files in simulation_inputs)
  
  default_simulation_params$development_selection_type = 'stochastic'  

  default_simulation_params$offset_selection_type = 'greedy'  
  
  # Whether to use banking. FALSE - means perform offsets simultaneously with development, TRUE -
  # means perform offset banking prior to development according to offset bank
  # parameters
  default_simulation_params$use_offset_bank = FALSE

  # Options are 'credit' or 'parcel_set'. 'credit' means there is accumulated
  # gain that is subtracted as parcels are developed. 'parcel_set' one or more
  # parcels in the bank are traded for one development site. If there is left
  # over credit (and allow_developments_from_credit is set to TRUE) then this excess credit is used on subsequent developments
  default_simulation_params$offset_bank_type = 'credit'     

  default_simulation_params$banked_offset_control = list()
  
  # The time horizon in which the offset gains need to equal the devlopment impact
  default_simulation_params$offset_time_horizon = 15

  # Include future legal developments in calculating contribution of avoided
  # losses to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  default_simulation_params$include_potential_developments_in_offset_calc = FALSE

  # Include future stochastic developments in calculating contribution of avoided losses
  # to the impact of the offset. This increases the impact of the
  # offset (due to future losses that are avoided)
  default_simulation_params$include_unregulated_loss_in_offset_calc = FALSE
  
  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the offset. The decreases the impact of the offset (due to
  # future gains that are avoided) - NOW WORKING
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
  
  default_simulation_params$include_unregulated_loss_in_dev_calc = default_simulation_params$include_unregulated_loss_in_offset_calc

  # Include future offsets in calculating contribution of avoided gains to the
  # impact of the development. This increases the impact of the development as
  # future gains are avoided
  
  # default_simulation_params$include_potential_offsets_in_dev_calc = FALSE
  
  # The development impacts is multiplied by this factor (irrespective of how
  # they were caluclated) and the offset impact then needs to match this
  # multiplied development impact
  
  default_simulation_params$offset_multiplier = 1
  default_simulation_params$project_by_group = FALSE
  


  # set to 'default' to use same probability of loss for each site and each time step
  # set to unregulated_stochastic_development' in conjunction with defining simulation_params$unregulated_intervention_vec to mirror development 
  # set to 'unregulated_directed_development' to perform directed development where specific vector of sites to be developed is specifed prior to simulation
  default_simulation_params$unregulated_loss_type = 'default'
  default_simulation_params$unregulated_intervention_vec = vector()
  default_simulation_params$directed_developments = vector()
  # set to false to stop initial credit being transformed - this has a big impact when using the BAM metric which 
  # transforms large values to ceiling defined by 100.68
  
  # allow pass of credit to simulation - can be used to run developments without offsets
  default_simulation_params$initial_credit = 0
  default_simulation_params$transform_initial_credit = TRUE
  return(default_simulation_params)
}

initialise_default_feature_params <- function(){
  
  # Construct the static initial landscape 
  
  default_feature_params = list()
  
  #setting this parameter to TRUE effectively evenly weights all features, setting to FALSE
  #gives an implicit weighting depending on the values of the features layers
  default_feature_params$scale_features = TRUE
  
  #how many feature layers to generate
  default_feature_params$simulated_feature_num = 1
  
  # logistic decline rate means across simulation features. Sample form a normal distribution with this mean and add noise using  default_simulation_params$decline_rate_std
  default_feature_params$mean_decline_rates = rep(list(-1e-2), default_feature_params$simulated_feature_num)
  
  #set this parameter to zero to yield no noise
  default_feature_params$decline_rate_std = rep(list(1e-3), default_feature_params$simulated_feature_num)
  
  default_feature_params$simulated_time_vec = 1:100
  
  # Number of pixels in (y, x) for the feature layes 
  default_feature_params$feature_layer_size = c(300, 300)
  
  # Numnber of parcels in x (but total size varies)
  default_feature_params$site_num_characteristics = c(30 , 30, 5)

  default_feature_params$feature_num_characteristics = c(20 , 10, 5)
  # Minimum allowable initial ecological value of smallest ecological element
  # (pixel) ie min value to sample from
  default_feature_params$min_initial_eco_val = 20
  
  # Max allowable initial ecological value of largest element (pixel) ie max
  # value to sample from
  default_feature_params$max_initial_eco_val = 80
  
  # list of length equal to feature number defining proportion of parcels occupied by the feature(s) 
  #TODO add error flag  when the length of this does not match feature_num
  default_feature_params$occupation_ratio = list(1)
  
  # Mow much initial variation in pixels per land parcel (this is the width of
  # uniform dist) used to add noise to each pixel. Eg if the pixel has a vlaue
  # of 35, a new value will be sampled from between 35-45
  default_feature_params$feature_value_distribution_width = 10
  
  # Defining multiple regions eg different states where different polcies can apply 
  default_feature_params$region_num_x = 1
  
  # Defining multiple regions eg different states where different rules can apply 
  default_feature_params$region_num_y = 1
  
  default_feature_params$simulated_time_vec = vector()
  default_feature_params$background_dynamics_bounds = list()
  default_feature_params$management_dynamics_bounds = list()
  default_feature_params$background_mode_num = vector()
  default_feature_params$management_mode_num = vector()
  default_feature_params$initial_condition_class_bounds = vector()
  default_feature_params$management_condition_class_bounds = vector()
  default_feature_params$sample_management_dynamics = TRUE 
  default_feature_params$sample_background_dynamics = TRUE
  default_feature_params$dynamics_sample_type = vector()
  default_feature_params$management_dynamics_type = vector()
  default_feature_params$background_dynamics_type = vector()
  default_feature_params$condition_class_bounds = list(list(c(0, 1)))
  default_feature_params$perform_management_dynamics_time_shift = vector()
  default_feature_params$management_dynamics_sample_type = vector()
  default_feature_params$perform_background_dynamics_time_shift = vector()
  default_feature_params$management_update_dynamics_by_differential = TRUE
  default_feature_params$background_update_dynamics_by_differential = TRUE
  default_feature_params$site_sample_type = 'uniform'
  default_feature_params$initial_site_sd = 1
  default_feature_params$project_by_mean = FALSE
  default_feature_params$initial_site_mean_sd = 1
  default_feature_params$management_condition_class = 'background'
  return(default_feature_params)
}


initialise_default_output_params <- function(base_folder){
  default_output_params = list()
  default_output_params$output_type = 'plot' # choose from 'raster', 'png', 'plot', 'csv'
  default_output_params$output_folder = vector()
  default_output_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  default_output_params$realisation_num = 'all' # 'all' or number to plot
  default_output_params$write_pdf = TRUE

  
  default_output_params$sets_to_plot = 1 # example site to plot
  default_output_params$scenario_vec = 'all' #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  default_output_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  default_output_params$program_col_vec = c('darkgreen', 'red', 'black') 
  default_output_params$cfac_col = 'blue' 
  default_output_params$landscape_col = 'black'
  default_output_params$lwd_vec = c(3, 0.5)
  default_output_params$plot_subset_type = 'all'
  default_output_params$plot_subset_param = 'all'
  default_output_params$plot_offset_metric = FALSE
  default_output_params$plot_site_offset = TRUE 
  default_output_params$plot_site_dev = TRUE
  default_output_params$plot_site_net = TRUE
  default_output_params$plot_site = TRUE
  default_output_params$plot_program = TRUE
  default_output_params$plot_landscape = TRUE
  default_output_params$features_to_output = 'all'
  default_output_params$site_impact_lwd = 0.5
  default_output_params$site_outcome_lwd_vec = c(0.5)
  default_output_params$program_lwd_vec = c(3, 0.5)
  default_output_params$program_outcome_lwd_vec = c(3, 0.5)
  default_output_params$landscape_lwd_vec  = c(3)
  default_output_params$landscape_outcome_lwd_vec = c(3)
  default_output_params$print_dev_offset_sites = TRUE
  default_output_params$example_realisation_to_output = 1
  
  #ouput offset sites as block colors rather than site_vals
  default_output_params$output_block_offsets = FALSE
  default_output_params$nx = 3 
  default_output_params$ny = 4
  
  default_output_params$site_outcome_plot_lims_set = rep(list(c(0, 3e4)), length(default_output_params$scenario_vec))
  default_output_params$program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), length(default_output_params$scenario_vec))
  default_output_params$landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), length(default_output_params$scenario_vec))
  
  default_output_params$site_impact_plot_lims_set = rep(list(c(-1e4, 1e4)), length(default_output_params$scenario_vec))
  default_output_params$program_impact_plot_lims_set = rep(list(c(-1e6, 1e6)), length(default_output_params$scenario_vec)) 
  default_output_params$landscape_impact_plot_lims_set = rep(list(c(-1e6, 0)), length(default_output_params$scenario_vec))
  
  # set 
  black_green.palette <- colorRampPalette(c("black", "green"), space = "rgb") #landscape palette
  black_blue.palette <- colorRampPalette(c("black", "blue"), space = "rgb") #offset palette
  default_output_params$col_vec = c(black_green.palette(128), black_blue.palette(128), 'red', 'orange') #c(landscape_palette, offset_palette, dev_colour, unregulated_loss_colour)
  
  # use this parameter to map feature value outputs of simulation to colour map specified below. Setting to false outputs raw condition values
  default_output_params$map_vals = TRUE
  
  # standard feature representation: 0-127 :black-green - 
  # offset representation: 128-255 :black-blue - 
  # development: 256 : red  
  # unregulated_loss: 257 :orange
  
  default_output_params$col_map_vector = c(128, 128, 256, 256, 257) #c(offset_col, offset_bank_col, dev_col, dev_credit_col, unregulated_loss_col)
  
 
  
  return(default_output_params)
}

