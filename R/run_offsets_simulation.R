#' Runs the Offset Simulator
#' @param user_global_params user configured parameters to use
#' @param user_combination_params user configured parameters to use
#' @param loglevel logging level to use, for instance futile.logger::WARN
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @import futile.logger
#' @export
#' 
osim.run <- function(user_global_params = NULL, user_combination_params = NULL, loglevel = WARN){

flog.threshold(loglevel)
flog.info('starting offsetsim')

params_object <- run_initialise_routines(user_global_params, user_combination_params)

# nested list object containing ecological values by feature layer for all sites
initial_ecology <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'parcel_ecology.rds'))

# list containing information on site geography
parcels <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'parcels.rds'))

# list containing probabilities of sites being developed
dev_weights <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'dev_weights.rds'))

# list containing probabilities of sites being developed
offset_weights <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'offset_weights.rds'))

# list used to govern ecology rate changes
decline_rates_initial <- simulate_decline_rates(parcel_num = length(parcels$land_parcels), 
                                                sample_decline_rate = TRUE, 
                                                mean_decline_rates = params_object$global_params$mean_decline_rates, 
                                                decline_rate_std = params_object$global_params$decline_rate_std, 
                                                feature_num = params_object$global_params$feature_num)       # set up array of decline rates that are eassociated with each cell

# select subset of ecology to use in current simulation 
# (e.g. if initial ecology is 100 layers deep just run with 10 of them)
initial_ecology <- select_feature_subset(initial_ecology, params_object$global_params$features_to_use_in_simulation)

for (scenario_ind in seq_along(params_object$combination_params_group)){

  loop_strt <- Sys.time()
  flog.info('running scenario %s of %s with %s realisations on %s cores with %s features on a landscape with size %s x %s', 
            scenario_ind, 
            length(params_object$combination_params_group),  
            params_object$global_params$realisation_num,
            params_object$global_params$number_of_cores, 
            length(params_object$global_params$features_to_use_in_simulation), 
            parcels$landscape_dims[1], 
            parcels$landscape_dims[2])

  if (params_object$global_params$number_of_cores > 1 && params_object$global_params$set_seed == TRUE){
    # case when running DETERMINISTIC realisations in parallel
    # doRNG needed to get deterministic foreach loops, dsingh 24/nov/17
    flog.info('will use doRNG with seed %s to get determinisitc parallel runs', 123)
    registerDoRNG(123) 
    foreach(realisation_ind = seq_len(params_object$global_params$realisation_num)) %dorng%{
              
              simulation_outputs <- run_offset_simulation_routines(combination_params = params_object$combination_params_group[[scenario_ind]],
                                                                   params_object$global_params,
                                                                   parcels,
                                                                   initial_ecology,
                                                                   dev_weights,
                                                                   offset_weights,
                                                                   scenario_ind,
                                                                   realisation_ind)
            }
  } else if (params_object$global_params$number_of_cores > 1){

    # case when running NON-DETERMINISTIC realisations in parallel
    foreach(realisation_ind = seq_len(params_object$global_params$realisation_num)) %dopar%{

      simulation_outputs <- run_offset_simulation_routines(combination_params = params_object$combination_params_group[[scenario_ind]],
                                                           params_object$global_params,
                                                           parcels,
                                                           initial_ecology,
                                                           dev_weights,
                                                           offset_weights,
                                                           scenario_ind,
                                                           realisation_ind)
    }
  } else {
    # case when running single realisation
    # bypasses foreach, but could be merged into earlier case of non-determinisitc realisations in parallel, dsingh 24/nov/17
    simulation_outputs <- run_offset_simulation_routines(combination_params = params_object$combination_params_group[[scenario_ind]],
                                                         params_object$global_params,
                                                         parcels,
                                                         initial_ecology,
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
  #remove all temporary files
  unlink(params_object$global_params$output_folder, recursive = TRUE)
}

flog.info('all scenarios done in %s %s', 
          round(difftime(Sys.time(), params_object$global_params$strt), 1), 
          units(difftime(Sys.time(), params_object$global_params$strt)))

flog.info('all outputs written into %s', params_object$global_params$run_folder)
parallel::stopCluster(params_object$global_params$clstr)
}


