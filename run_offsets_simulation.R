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
  
  if (run_params$plot_outputs == TRUE){
    collated_realisations = bind_collated_realisations(scenario_ind, 
                                                       file_path = run_params$collated_folder, 
                                                       eco_ind = 1)
    sim_characteristics = get_current_sim_characteristics(policy_params_group[[scenario_ind]], run_params$realisation_num)
    setup_sub_plots(nx = 3, ny = 2, x_space = 5, y_space = 5)
    
    sets_to_plot = 10
    policy_params = policy_params_group[[scenario_ind]]
    
    if (policy_params$use_offset_bank == TRUE){
      site_plot_lims = c(0, 1e7)
      site_impact_plot_lims = c(-1e6, 1e6)
    } else {
      site_plot_lims = c(0, 1e5)
      site_impact_plot_lims = c(-1e4, 1e4)
    }
    
    program_plot_lims = c(0e6, 5e6) 
    landscape_plot_lims = c(0e6, 1e7)
    
    program_impact_plot_lims = c(-6e5, 6e5) 
    landscape_impact_plot_lims = c(-6e5, 6e5)

    plot_impact_set(collated_realisations, 
                    policy_params, 
                    site_impact_plot_lims,
                    program_impact_plot_lims, 
                    landscape_impact_plot_lims, 
                    sets_to_plot,
                    lwd_vec = c(3, 0.5), 
                    time_steps = run_params$time_steps, 
                    parcels$land_parcel_num,
                    realisation_num = run_params$realisation_num) 
    
    plot_outcome_set(collated_realisations,
                     policy_params,
                     site_plot_lims,
                     program_plot_lims, 
                     landscape_plot_lims,
                     sets_to_plot,
                     lwd_vec = c(3, 0.5), 
                     time_steps = run_params$time_steps,
                     realisation_num = run_params$realisation_num) 
  }
  
  fin <- Sys.time()
  print(fin - strt)
  
}

stopCluster(cl)
