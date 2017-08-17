rm(list = ls())

strt = Sys.time()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source('initialise_params_hunter.R')
source('initialise_routines.R')                              # functions to collate simulation outputs
source('simulation_routines.R')                # functions to run simulation
source('collate_routines.R')                                # functions to collate simulation outputs
source('plot_routines.R')                                   # functions to plot collated outputs

run_params <- initialise_run_params()
policy_params_group = generate_policy_params_group(run_params)
run_params <- run_initialise_routines(run_params, policy_params_group)

initial_ecology <- readRDS(paste0(run_params$landscape_data_folder, 'parcel_ecology.rds'))
decline_rates_initial <- readRDS(paste0(run_params$landscape_data_folder, 'decline_rates_initial.rds'))
parcels <- readRDS(paste0(run_params$landscape_data_folder, 'parcels.rds'))
dev_weights <- readRDS(paste0(run_params$landscape_data_folder, 'dev_weights.rds'))

initial_ecology <- select_feature_subset(initial_ecology, run_params$features_to_use_in_simulation)
decline_rates_initial <- select_feature_subset(decline_rates_initial, run_params$features_to_use_in_simulation)

cl<-makeCluster(run_params$crs)  # allow parallel processing on n = 4 processors
registerDoParallel(cl)

for (scenario_ind in seq_along(policy_params_group)){
  loop_strt <- Sys.time()
  print(paste0('running ', scenario_ind, ' of ', length(policy_params_group), ' scenarios with ', run_params$realisation_num, 
        ' realisations on ', run_params$crs, ' cores'))
  
  #foreach(realisation_ind = seq_len(run_params$realisation_num)) %dopar%{
    
    global_object <- perform_offsets_simulation(policy_params = policy_params_group[[scenario_ind]], 
                                                run_params,
                                                parcels, 
                                                initial_ecology, 
                                                decline_rates_initial,
                                                dev_weights,
                                                scenario_ind, 
                                                realisation_ind = 1)
  #}

  print(paste('scenario ', scenario_ind, ' done in', round(Sys.time() - loop_strt), ' mins'))
  
}

if (run_params$save_simulation_outputs == FALSE){
  unlink(run_params$output_folder, recursive = TRUE)
}
print(paste('all scenarios done in', round(Sys.time() - strt), ' mins'))
stopCluster(cl)
