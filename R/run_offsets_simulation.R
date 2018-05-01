#' Runs the Offset Simulator
#' @param user_global_params user configured parameters to use
#' @param user_simulation_params user configured parameters to use
#' @param user_simulated_ecology_params user configured parameters to use
#' @param loglevel logging level to use, for instance futile.logger::WARN
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @import futile.logger
#' @export
#' 
osim.run <- function(user_global_params = NULL, user_simulation_params = NULL, user_simulated_ecology_params = NULL, user_simulation_dynamics = NULL, loglevel = WARN){
  
  flog.threshold(loglevel)
  flog.info('starting offsetsim')
  
  params_object <- build_simulation_params(user_global_params, user_simulation_params, user_simulated_ecology_params)
  
  # Undertake all the run intialization proceedures, including generating
  # simulated data if required or reading in previously generated input data
  
  global_input_object <- generate_global_inputs(params_object)
  
  # Write initial logging info
  flog.info('Running %s scenarios with %s realisations on %s cores', 
            length(params_object$simulation_params_group),  
            params_object$global_params$realisation_num,
            params_object$global_params$number_of_cores ) 
  
  # Loop over all defined scenarios if only a subset of the scenarios is to be
  # reun (as defined by params_object$global_params$scenario_subset) then only
  # run these. By default params_object$global_params$scenario_subset
  

  for (scenario_ind in params_object$global_params$scenario_subset){
    
    # Store the start time
    loop_strt <- Sys.time()
    
    # Extract out the parameters for the current scenario to be run
    current_simulation_params <- params_object$simulation_params_group[[scenario_ind]]
    
    index_object <- initialise_index_object(global_input_object$parcels, 
                                            global_input_object$current_feature_layers, 
                                            current_simulation_params, 
                                            offset_indexes_to_exclude = which(unlist(global_input_object$offset_probability_list) == 0), 
                                            dev_indexes_to_exclude = which(unlist(global_input_object$dev_probability_list) == 0))

    flog.info('running scenario %s of %s in %s mode with %s offsets',  
              scenario_ind, 
              length(params_object$simulation_params_group), 
              current_simulation_params$offset_calc_type, 
              current_simulation_params$offset_action_type)
    
    flog.info('developing %s of %s available sites with %s available offset_sites in a landscape with %s sites and %s x %s elements', 
              sum(current_simulation_params$intervention_vec), 
              length(unlist(index_object$indexes_to_use$devs)),  # total number of sites available to develop
              length(unlist(index_object$indexes_to_use$offsets)), # total number sites avaulable to offset
              length(global_input_object$parcels$land_parcels), # total number of parcles
              global_input_object$parcels$landscape_dims[1], 
              global_input_object$parcels$landscape_dims[2])
    
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
        
        run_offset_simulation_routines(global_input_object, 
                                       current_simulation_params,
                                       index_object,
                                       scenario_ind,
                                       realisation_ind)
      }
    } else if ((params_object$global_params$number_of_cores > 1) && (params_object$global_params$realisation_num > 1)){
      
      # case when running NON-DETERMINISTIC realisations in parallel
      foreach(realisation_ind = seq_len(params_object$global_params$realisation_num)) %dopar%{
        
        run_offset_simulation_routines(global_input_object, 
                                       current_simulation_params,
                                       index_object,
                                       scenario_ind,
                                       realisation_ind)
      }
    } else {
      # Case when running single realisation
      # TODO(Isaac): need to add case for running a single realization either with or without having the seed set.
      for (realisation_ind in 1:params_object$global_params$realisation_num){
        run_offset_simulation_routines(global_input_object,
                                       current_simulation_params,
                                       index_object,
                                       scenario_ind,
                                       realisation_ind)
      }
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


