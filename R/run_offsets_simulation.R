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
#' @import truncnorm
#' @import Matrix
#' @export 
#' 
osim.run <- function(user_global_params = NULL, user_simulation_params = NULL, user_feature_params = NULL, loglevel = WARN){
  
  flog.threshold(loglevel)
  flog.info('starting offsetsim')
  
  params_object <- build_params(user_global_params, user_feature_params, user_simulation_params)
    
  # Undertake all the run intialization proceedures, including generating
  # simulated data if required or reading in previously generated input data
  
  # Write initial logging info
  flog.info('Running %s scenarios with %s realisations on %s cores', 
            length(params_object$simulation_params_group),  
            params_object$global_params$realisation_num,
            params_object$global_params$number_of_cores ) 
  
  # Loop over all defined scenarios if only a subset of the scenarios is to be
  # reun (as defined by global_params$scenario_subset) then only
  # run these. By default global_params$scenario_subset
  
  for (scenario_ind in params_object$global_params$scenario_subset){
    
    # Store the start time
    loop_strt <- Sys.time()
    
    #load objects from files etc
    simulation_data_object <- build_input_data(params_object, scenario_ind)

    simulation_data_object$output_data = build_output_data(simulation_data_object)
    flog.info('running scenario %s of %s, offsetting using %s impact calculation with %s management regime and %s year time horizon for gain/loss calculations',  
              scenario_ind, 
              length(params_object$simulation_params_group),
              simulation_data_object$simulation_params$offset_calc_type, 
              simulation_data_object$simulation_params$offset_action_type, 
              simulation_data_object$simulation_params$offset_time_horizon)
    
    flog.info('system composed of %s x %s elements and %s sites',
              simulation_data_object$site_characteristics$landscape_dims[1], 
              simulation_data_object$site_characteristics$landscape_dims[2],
              length(simulation_data_object$site_characteristics$land_parcels))
    flog.info('%s potential development sites, %s potential offset sites',
              length(simulation_data_object$output_data$index_object$available_indexes$devs), 
              length(simulation_data_object$output_data$index_object$available_indexes$offsets)) 
    
    flog.info('developing (and offsetting) %s sites', sum(simulation_data_object$simulation_params$intervention_vec)) 
    
    flog.info('estimated %s sites lost to unregulated clearing', 
              round(length(simulation_data_object$site_characteristics$land_parcels) -
                length(simulation_data_object$site_characteristics$land_parcels)*(1 - simulation_data_object$simulation_params$unregulated_loss_prob)^simulation_data_object$simulation_params$time_steps))
    
    # Work out if more than one core is specified, and if so, run the
    # simulation in parallel using the doParallel, foreach and doRNG packages

    # TODO(Isaac) test whether can set check and set seed if necessary much
    # earlier in the code before run_initialise_routines() is called
    if (simulation_data_object$global_params$number_of_cores > 1 && simulation_data_object$global_params$set_seed == TRUE){
      # case when running DETERMINISTIC realisations in parallel
      # doRNG needed to get deterministic foreach loops, dsingh 24/nov/17
      flog.info('will use doRNG with seed %s to get determinisitc parallel runs', 123)
      registerDoRNG(123) 
      
      # foreach runs realizations in parallel
      foreach(realisation_ind = seq_len(simulation_data_object$global_params$realisation_num)) %dorng%{
        
        run_offset_simulation_routines(simulation_data_object, 
                                       scenario_ind,
                                       realisation_ind)
      }
    } else if ((simulation_data_object$global_params$number_of_cores > 1) && (simulation_data_object$global_params$realisation_num > 1)){
      # case when running NON-DETERMINISTIC realisations in parallel
      foreach(realisation_ind = seq_len(simulation_data_object$global_params$realisation_num)) %dopar%{
        
        run_offset_simulation_routines(simulation_data_object, 
                                       scenario_ind,
                                       realisation_ind)
      }
    } else {
      # Case when running single realisation
      # TODO(Isaac): need to add case for running a single realization either with or without having the seed set.
      for (realisation_ind in 1:simulation_data_object$global_params$realisation_num){
        
        run_offset_simulation_routines(simulation_data_object,
                                       scenario_ind,
                                       realisation_ind)
      }
    }
    
    flog.info('scenario %s done in %s %s', 
              scenario_ind,
              round(difftime(Sys.time(), loop_strt), 1), 
              units(difftime(Sys.time(), loop_strt)))
  }
  
  if (simulation_data_object$global_params$save_simulation_outputs == FALSE){
    # This deletes all folders and subfolders that were created in the run
    # process that are no longer needed
    unlink(simulation_data_object$global_params$output_folder, recursive = TRUE)
  }
  
  flog.info('all scenarios done in %s %s', 
            round(difftime(Sys.time(), simulation_data_object$global_params$strt), 1), 
            units(difftime(Sys.time(), simulation_data_object$global_params$strt)))
  
  flog.info('all outputs written into %s', simulation_data_object$global_params$run_folder)
  
  # Clean up the parallel processes if there was more than one
  parallel::stopCluster(simulation_data_object$global_params$clstr)
}


