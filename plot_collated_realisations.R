rm(list = ls())
library(foreach)
library(doParallel)
library(abind)
library(pixmap)

action_type = 'plot_outcomes' #'collate_realisations', 'plot_impacts', or 'plot_outcomes'.
offset_bank = FALSE
write_pdf = TRUE
data_type = 'simulated'
run_number = 40
runstring = formatC(run_number, width = 5, format = "d", flag = "0")

collated_folder = paste0('~/offset_data/collated_realisations/')  #LOCATION OF COLLATED FILES
output_plot_folder = collated_folder

source('plot_routines.R')                                   # functions to plot collated outputs
source('collate_routines.R')

run_params = readRDS(paste0(collated_folder, 'run_params.rds'))
current_filenames <- list.files(path = collated_folder, pattern = '_collated_realisation', all.files = FALSE, 
                                full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                include.dirs = FALSE, no.. = FALSE)

if (!file.exists(output_plot_folder)){
  dir.create(output_plot_folder)
}

if (write_pdf == TRUE){
  if (action_type == 'plot_impacts'){
    filename = paste0(output_plot_folder, runstring, '_impacts.pdf')
  } else if (action_type == 'plot_outcomes'){
    filename = paste0(output_plot_folder, runstring, '_outcomes.pdf')
  }
  pdf(filename, width = 8.3, height = 11.7) 
} 

if (offset_bank == TRUE){
  site_plot_lims = c(0, 2e6)
  site_impact_plot_lims = c(-1e6, 1e6)
} else {
  site_plot_lims = c(0, 1e4)
  site_impact_plot_lims = c(-1e4, 1e4)
}

program_plot_lims = c(0e6, 5e6) 
landscape_plot_lims = c(0e6, 1e7)

program_impact_plot_lims = c(-6e5, 6e5) 
landscape_impact_plot_lims = c(-6e5, 6e5)
sets_to_plot = 5
setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)

policy_num = length(current_filenames)

if (length(current_filenames) == 0){
  print('no files found')
}

scenario_filenames <- list.files(path = collated_folder, pattern = '_policy_params', all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)
for (scenario_ind in seq_along(scenario_filenames)){
  current_policy_params = readRDS(paste0(collated_folder, scenario_filenames[scenario_ind]))
  collated_realisations = bind_collated_realisations(scenario_ind, 
                                                     file_path = collated_folder, 
                                                     eco_ind = 1)
  if (action_type == 'plot_impacts'){
    plot_impact_set(collated_realisations, 
                    current_policy_params, 
                    site_impact_plot_lims,
                    program_impact_plot_lims, 
                    landscape_impact_plot_lims, 
                    sets_to_plot,
                    lwd_vec = c(3, 0.5), 
                    time_steps = run_params$time_steps, 
                    parcel_num = 1600,
                    realisation_num = run_params$realisation_num) 
  } else {
    plot_outcome_set(collated_realisations,
                     current_policy_params,
                     site_plot_lims,
                     program_plot_lims, 
                     landscape_plot_lims,
                     sets_to_plot,
                     lwd_vec = c(3, 0.5), 
                     time_steps = run_params$time_steps,
                     realisation_num = run_params$realisation_num) 
  }
  print(paste0('policy ', scenario_ind, ' done'))
}


if (write_pdf == TRUE){
  dev.off()
}
