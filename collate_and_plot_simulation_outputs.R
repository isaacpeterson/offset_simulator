rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = paste0(WD, '/')
source(paste0(source_folder,'simulation_routines.R'))                 # functions to run simulation
source(paste0(source_folder,'collate_routines.R'))                                # functions to collate simulation outputs
source(paste0(source_folder,'plot_routines.R'))                                   # functions to plot collated outputs

collated_folder = '~/offset_data/simulated/2017-177-163657/collated_realisations/'
plot_type = 'impacts' #'outcomes' or 'impacts'
write_pdf = FALSE
species_ind = 1
read_collated_object_from_file = FALSE

if (write_pdf == TRUE){
  filename = paste0(collated_folder, '/outputs/', plot_type, '.pdf')
  pdf(filename, width = 8.3, height = 11.7) 
} 
  
program_plot_lims = c(0e6, 5e6) 
landscape_plot_lims = c(2.5e6, 5e6)
program_impact_plot_lims = c(-6e5, 6e5) 
landscape_impact_plot_lims = c(-6e5, 6e5)
lwd_vec = c(3, 0.5)
sets_to_plot = 5
setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)

current_simulation_filenames <- list.files(path = collated_folder, pattern = 'simulation_params', all.files = FALSE, 
                                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                          include.dirs = FALSE, no.. = FALSE)

scenario_num = length(current_simulation_filenames)
print(scenario_num)

for (scenario_ind in seq_len(scenario_num)){
  simulation_params = readRDS(paste0(collated_folder, current_simulation_filenames[scenario_ind]))
  
  if (read_collated_object_from_file){
    collated_realisations = readRDS(paste0(simulation_params$run_params$collated_folder, 
                                          'bound_collated_realisations_scenario_', scenario_ind, 
                                          '_species_', species_ind, '.rds'))
  } else {
    collated_realisations = bind_collated_realisations(scenario_ind, 
                                                       file_path = simulation_params$run_params$collated_folder, 
                                                       species_ind)
    saveRDS(collated_realisations, paste0(simulation_params$run_params$collated_folder, 
                                          'bound_collated_realisations_scenario_', scenario_ind, 
                                          '_species_', species_ind, '.rds'))
  }
  if (simulation_params$policy_params$use_offset_bank == TRUE){
    site_plot_lims = c(0, 2e6)
    site_impact_plot_lims = c(-1e6, 1e6)
  } else {
    site_plot_lims = c(-2e4, 2e4)
    site_impact_plot_lims = c(-1e4, 1e4)
  }
  
  if (plot_type == 'impacts'){

    plot_impact_set(collated_realisations, 
                    simulation_params$policy_params, 
                    site_plot_lims,
                    program_impact_plot_lims, 
                    landscape_impact_plot_lims, 
                    sets_to_plot,
                    lwd_vec, 
                    edge_title = simulation_params$sim_characteristics, 
                    time_steps = simulation_params$run_params$time_steps, 
                    offset_bank = simulation_params$policy_params$use_offset_bank,
                    1600,
                    realisation_num = simulation_params$run_params$realisation_num) 
  } else {
    
    plot_outcome_set(collated_realisations,
                     simulation_params$policy_params,
                     offset_bank = simulation_params$policy_params$use_offset_bank,
                     site_plot_lims,
                     program_plot_lims, 
                     landscape_plot_lims,
                     sets_to_plot,
                     lwd_vec, 
                     edge_title = '', 
                     time_steps = simulation_params$run_params$time_steps,
                     realisation_num = simulation_params$run_params$realisation_num) 
  }

}


if (write_pdf == TRUE){
  dev.off()
}
