rm(list = ls())
WD = getwd()

strt<-Sys.time()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = paste0(WD, '/')
output_folder = paste0(path.expand('~'), '/offset_data/')

source(paste0(source_folder, 'initialise_params.R'))
source(paste0(source_folder, 'initialise_routines.R'))                              # functions to collate simulation outputs
source(paste0(source_folder,'simulation_routines.R'))                # functions to run simulation
source(paste0(source_folder,'collate_routines.R'))                                # functions to collate simulation outputs
source(paste0(source_folder,'plot_routines.R'))                                   # functions to plot collated outputs


run_params <- initialise_run_params()
ecology_params <- initialise_ecology_params()
policy_params_group = generate_policy_params_group(run_params)

if (run_params$save_realisations == TRUE){
  run_params <- write_output_folders(run_params, output_folder)
}

data_folder = paste0(path.expand('~'), '/offset_data/', run_params$data_type, '/data/')



simulation_data <- initialise_simulation_data(run_params, ecology_params)

cl<-makeCluster(run_params$crs)        #allow parallel processing on n = 4 processors
registerDoParallel(cl)

print(paste('testing ', length(policy_params_group), ' combinations on ', run_params$crs, ' cores'))

for (policy_ind in seq_along(policy_params_group)){
  current_policy_params = select_current_policy(policy_params_group, policy_ind, run_params$realisation_num)
  
  realisations <- foreach(run_ind = seq_len(run_params$realisation_num)) %dopar%{
   perform_offsets_simulation(simulation_data, current_policy_params, ecology_params, run_params)
  }
  
  if (run_params$save_realisations == TRUE){
    sim_group_to_save = list(simulation_data, current_policy_params, run_params, ecology_params)
    names(sim_group_to_save) = c('simulation_data', 'current_policy_params', 'run_params', 'ecology_params')
    saveRDS(sim_group_to_save, paste0(run_params$sim_group_folder, current_policy_params$sim_characteristics, '.rds'))
    saveRDS(realisations, paste0(run_params$realisations_folder, current_policy_params$sim_characteristics, '.rds'))
  }
  
  if (run_params$collate_realisations == TRUE){
    collated_realisations <- run_collate_routines(realisations,
                                                  run_params,
                                                  ecology_params, 
                                                  current_policy_params, 
                                                  use_cfac_type_in_sim = TRUE, 
                                                  simulation_data$decline_rates_initial, 
                                                  simulation_data$parcels, 
                                                  simulation_data$initial_ecology) #take simulation ouputs and calculate gains and losses
    
    
    if (run_params$save_collated_realisations == TRUE){
      saveRDS(collated_realisations, paste0(run_params$collated_folder, current_policy_params$sim_characteristics, '.rds')) 
    }
    if (run_params$plot_impacts == TRUE){
      setup_sub_plots(nx = 3, ny = 2, x_space = 5, y_space = 5)
      sets_to_plot = 10
      plot_impact_set(collated_realisations, 
                      current_policy_params, 
                      site_plot_lims = c(-5e3, 5e3),
                      program_plot_lims = c(-6e5, 6e5), 
                      landscape_plot_lims = c(-6e5, 6e5), 
                      sets_to_plot,
                      eco_ind = 1, 
                      lwd_vec = c(3, 0.5), 
                      edge_title = current_policy_params$sim_characteristics, 
                      time_steps = 50, 
                      offset_bank = current_policy_params$use_offset_bank,
                      simulation_data$parcels$land_parcel_num)
      
      plot_outcome_set(collated_realisations,
                       current_policy_params,
                       offset_bank = FALSE,
                       site_plot_lims = c(0, 5e4),
                       program_plot_lims = c(0, 6e6), 
                       landscape_plot_lims = c(0, 1e7),
                       sets_to_plot,
                       eco_ind = 1, 
                       lwd_vec = c(3, 0.5), 
                       edge_title = '', 
                       time_steps = 50) 
    }
  }
  
  fin <- Sys.time()
  print(fin - strt)
  
}

stopCluster(cl)
