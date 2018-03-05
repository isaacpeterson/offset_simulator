#' Runs the Offset Simulator
#' @param user_global_params user configured parameters to use
#' @param user_simulation_params user configured parameters to use
#' @param user_simulated_ecology_params user configured parameters to use
#' @param loglevel logging level to use, for instance futile.logger::WARN
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @import futile.logger
#' @import raster
#' @export
#' 
osim.run <- function(user_global_params = NULL, user_simulation_params = NULL, user_simulated_ecology_params = NULL, loglevel = WARN){
  
  flog.threshold(loglevel)
  flog.info('starting offsetsim')
  
  # Undertake all the run intialization proceedures, including generating
  # simulated data if required or reading in previously generated input data
  params_object <- run_initialise_routines(user_global_params, user_simulation_params, user_simulated_ecology_params)
  

  # Read in list object containing feature values by feature layer for all
  # sites.  This is 3-level nested list, the top level is parcel index, for each
  # parcel there is sublist containing the value of each feature for each cell
  # in that parcel. So if there were 3 parcels and 2 features, it would be a 3
  # element list, where each element was subsequent list of length 2
  # containing the values for the 2 features in that parcel. The values of the
  # feature for the parcel are a vector of length given by the number of
  # pixels in the parcel.

  initial_features <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'parcel_ecology.rds'))
  

  # This is a list of vectors of length number of sites. First level is the parcel index; the second
  # level is the a vector containing the index values for the pixels in each
  # parcel
  parcels <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'parcels.rds'))
  
  # This is a list of single values of length number of sites where the values
  # representing the probabilities of sites being developed. Default is that
  # all sites have equal probability simulated data, with user data this must
  # be specified.
  # TODO(Isaac): if this isn't supplied by user, assign all values to be equal (and give warning)
  dev_weights <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'dev_weights.rds'))
  
  # This is a list of single values of length number of sites, where the values
  # represent the probabilities of sites being offset. Default is that all
  # sites have equal probability simulated data, with user data this must be
  # specified.
  # TODO(Isaac): if this isn't supplied by user, assign all values to be equal (and give warning)
  offset_weights <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'offset_weights.rds'))
  
  # This is a nested list. Top level is of length number of sites, and each
  # element is another list containing a single number for each feature.  This
  # number represent the rate that each fature changes over time. For now this
  # assumes that there is an analytic expression for the how each feature
  # changes over time.

  # TODO(Isaac): for Sydney model, will need to be able to use arbitrary
  # curves given by experts. In that case this number will change to represent
  # something like  the identity fo the lookup table that describes chondition
  # change over time, which can be based on a funciton or can be from expert
  # elicitation
  decline_rates_initial <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'decline_rates_initial.rds'))
  
  # Write initial logging info
  flog.info('Running %s scenarios with %s realisations on %s cores', 
            length(params_object$simulation_params_group),  
            params_object$global_params$realisation_num,
            params_object$global_params$number_of_cores ) 


  # Loop over all defined scenarios if only a subset of the scenarios is to be
  # reun (as defined by params_object$global_params$scenario_subset) then only
  # run these. By default params_object$global_params$scenario_subset

  # TODO(Issac) change scenario_run_vec to be scenario_subset

  for (scenario_ind in params_object$global_params$scenario_subset){
    
    # Store the start time
    loop_strt <- Sys.time()
    

    # Extract out the parameters for the current scenario to be run
    current_simulation_params <- params_object$simulation_params_group[[scenario_ind]]

    flog.info('running scenario %s of %s in %s mode with %s offsets',  
              scenario_ind, 
              length(params_object$simulation_params_group), 
              current_simulation_params$offset_calc_type, 
              current_simulation_params$offset_action_type)
    

    # select subset of feature layers to use in current simulation 
    # (e.g. if there 100 layers just run with 10 of them)
    initial_features_to_use <- select_feature_subset(initial_features, current_simulation_params$features_to_use_in_simulation)
    
    decline_rates_initial_to_use <- select_feature_subset(decline_rates_initial, current_simulation_params$features_to_use_in_simulation)
    
    # Set up the object used to store all simulation inputs and pass them to the simulation function 
    simulation_inputs = initialise_input_object(parcels, 
                                                 initial_features_to_use, 
                                                 current_simulation_params, 
                                                 decline_rates_initial_to_use, 
                                                 dev_weights, 
                                                 offset_weights)
    
    flog.info('developing %s of %s available sites with %s available offset_sites in a landscape with %s sites and %s x %s elements', 
              current_simulation_params$total_dev_num, 
              length(unlist(simulation_inputs$index_object$indexes_to_use$devs)),  # total number of sites available to develop
              length(unlist(simulation_inputs$index_object$indexes_to_use$offsets)), # total number sites avaulable to offset
              length(parcels$land_parcels), # total number of parcles
              parcels$landscape_dims[1], 
              parcels$landscape_dims[2])
    
    # Work out if more than one core is specified, and if so, run the
    # simulation in parallel using the doParallel, foreach and doRNG packages

    # TODO(Isaac) test whether can set check and set seed if necessary much
    # earlier in the code before run_initialise_routines() is called
    if (params_object$global_params$number_of_cores > 1 && params_object$global_params$set_seed == TRUE){
      # case when running DETERMINISTIC realisations in parallel
      # doRNG needed to get deterministic foreach loops, dsingh 24/nov/17
      flog.info('will use doRNG with seed %s to get determinisitc parallel runs', 123)
      registerDoRNG(123) 

      # foreach runs realizations in parallel
      foreach(realisation_ind = seq_len(params_object$global_params$realisation_num)) %dorng%{
        
        run_offset_simulation_routines(simulation_inputs, 
                                       current_simulation_params,
                                       params_object$global_params,
                                       parcels,
                                       initial_features_to_use,
                                       decline_rates_initial_to_use,
                                       dev_weights,
                                       offset_weights,
                                       scenario_ind,
                                       realisation_ind)
      }
    } else if ((params_object$global_params$number_of_cores > 1) && (params_object$global_params$realisation_num > 1)){
      
      # case when running NON-DETERMINISTIC realisations in parallel
      foreach(realisation_ind = seq_len(params_object$global_params$realisation_num)) %dopar%{
        
        run_offset_simulation_routines(simulation_inputs, 
                                       current_simulation_params,
                                       params_object$global_params,
                                       parcels,
                                       initial_features_to_use,
                                       decline_rates_initial_to_use,
                                       dev_weights,
                                       offset_weights,
                                       scenario_ind,
                                       realisation_ind)
      }
    } else {
      # Case when running single realisation
      # TODO(Isaac): need to add case for running a single realization either with or without having the seed set.
      run_offset_simulation_routines(simulation_inputs, 
                                     current_simulation_params,
                                     params_object$global_params,
                                     parcels,
                                     initial_features_to_use,
                                     decline_rates_initial_to_use,
                                     dev_weights,
                                     offset_weights,
                                     scenario_ind,
                                     realisation_ind = 1)
    }
    
    flog.info('scenario %s done in %s %s', 
              scenario_ind,
              round(difftime(Sys.time(), loop_strt), 1), 
              units(difftime(Sys.time(), loop_strt)))
    
  }
  
  if (params_object$global_params$save_simulation_outputs == FALSE){
    # This deletes all folders and subfolders that were created in the run
    # process that are no longer needed
    unlink(params_object$global_params$output_folder, recursive = TRUE)
  }
  
  flog.info('all scenarios done in %s %s', 
            round(difftime(Sys.time(), params_object$global_params$strt), 1), 
            units(difftime(Sys.time(), params_object$global_params$strt)))
  
  flog.info('all outputs written into %s', params_object$global_params$run_folder)

  # Clean up the parallel processes if there was more than one
  parallel::stopCluster(params_object$global_params$clstr)
}


