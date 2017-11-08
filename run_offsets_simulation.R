rm(list = ls())
source('initialise_routines.R')                              # functions to collate simulation outputs

user_params_file = 'user_params/initialise_params_hunter.R'

run_params <- run_initialise_routines(user_params_file)
initial_ecology <- readRDS(paste0(run_params$simulation_inputs_folder, 'parcel_ecology.rds'))
parcels <- readRDS(paste0(run_params$simulation_inputs_folder, 'parcels.rds'))
dev_weights <- readRDS(paste0(run_params$simulation_inputs_folder, 'dev_weights.rds'))

decline_rates_initial <- simulate_decline_rates(parcel_num = length(parcels$land_parcels), 
                                                sample_decline_rate = TRUE, 
                                                mean_decline_rates = run_params$mean_decline_rates, 
                                                decline_rate_std = run_params$decline_rate_std, 
                                                feature_num = run_params$feature_num)       # set up array of decline rates that are eassociated with each cell

initial_ecology <- select_feature_subset(initial_ecology, run_params$features_to_use_in_simulation)

cl<-makeCluster(run_params$crs)  # allow parallel processing on n = 4 processors
registerDoParallel(cl)

for (scenario_ind in seq_along(run_params$policy_params_group)){
  
  loop_strt <- Sys.time()
  cat('\nrunning scenario', scenario_ind, 'of', length(run_params$policy_params_group), 'with', run_params$realisation_num, 
               'realisations on', run_params$crs, ' cores \n')

  if (run_params$realisation_num > 1){
    foreach(realisation_ind = seq_len(run_params$realisation_num)) %dopar%{
      
      simulation_outputs <- run_offset_simulation_routines(policy_params = run_params$policy_params_group[[scenario_ind]], 
                                                           run_params,
                                                           parcels, 
                                                           initial_ecology, 
                                                           decline_rates_initial,
                                                           dev_weights,
                                                           scenario_ind, 
                                                           realisation_ind)
    }
  } else {
    simulation_outputs <- run_offset_simulation_routines(policy_params = run_params$policy_params_group[[scenario_ind]], 
                                                         run_params,
                                                         parcels, 
                                                         initial_ecology, 
                                                         decline_rates_initial,
                                                         dev_weights,
                                                         scenario_ind, 
                                                         realisation_ind = 1)
  }
  
  cat('\n   scenario', scenario_ind, 'done in', 
              round(difftime(Sys.time(), loop_strt), 1), units(difftime(Sys.time(), loop_strt)))
  
}

if (run_params$save_simulation_outputs == FALSE){
  unlink(run_params$output_folder, recursive = TRUE)
}

cat('\nall scenarios done in', round(difftime(Sys.time(), run_params$strt), 1), units(difftime(Sys.time(), run_params$strt)))
stopCluster(cl)
