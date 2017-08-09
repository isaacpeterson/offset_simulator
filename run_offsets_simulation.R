rm(list = ls())
strt<-Sys.time()
#test update

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source('initialise_params.R')
source('initialise_routines.R')                              # functions to collate simulation outputs
source('simulation_routines.R')                # functions to run simulation
source('collate_routines.R')                                # functions to collate simulation outputs
source('plot_routines.R')                                   # functions to plot collated outputs

run_params <- initialise_run_params()
policy_params_group = generate_policy_params_group(run_params)
run_params <- run_initialise_routines(run_params, policy_params_group)

initial_ecology <- readRDS(paste0(run_params$simulation_inputs_folder, 'parcel_ecology.rds'))
decline_rates_initial <- readRDS(paste0(run_params$simulation_inputs_folder, 'decline_rates_initial.rds'))
parcels <- readRDS(paste0(run_params$simulation_inputs_folder, 'parcels.rds'))
dev_weights <- readRDS(paste0(run_params$simulation_inputs_folder, 'dev_weights.rds'))

cl<-makeCluster(run_params$crs)        #allow parallel processing on n = 4 processors
registerDoParallel(cl)

print(paste('testing ', length(policy_params_group), ' combinations on ', run_params$crs, ' cores'))

for (scenario_ind in seq_along(policy_params_group)){
  
  print(paste('running ', run_params$realisation_num, ' realisations on scenario ', scenario_ind))
  foreach(realisation_ind = seq_len(run_params$realisation_num)) %dopar%{
  
  global_object <- perform_offsets_simulation(policy_params = policy_params_group[[scenario_ind]], 
                                                run_params,
                                                parcels, 
                                                initial_ecology, 
                                                decline_rates_initial,
                                                dev_weights,
                                                scenario_ind, 
                                                realisation_ind)
  } 
  
  fin <- Sys.time()
  print(paste('scenario ', scenario_ind, ' done at', round(fin - strt), ' mins')
 
}

stopCluster(cl)
