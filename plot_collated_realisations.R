rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = paste0(path.expand("~"), '/Documents/R_Codes/Offsets_Working_Feb_3/')

action_type = 'plot_impacts' #'collate_realisations', 'plot_impacts', or 'plot_outcomes'.
policy_type = "restoration_gains"  #'restoration_gains', 'avoided_degs', 'net_gains'
offset_bank = FALSE
write_pdf = FALSE
data_type = 'simulated'
time_created = '2017-159-103225'

if (offset_bank == FALSE){
  policy_type = paste0(policy_type, '_offset_bank_FALSE')
} else {
  policy_type = 'offset_bank_TRUE'
}

data_folder = paste0(path.expand("~"), '/offset_data/', data_type, '/')


collated_folder = paste0(data_folder, 'collated_realisations/', time_created, '/')
realisations_folder = paste0(data_folder, 'realisations/', time_created, '/')
sim_group_folder = paste0(data_folder, 'sim_group/', time_created, '/')

source(paste0(source_folder,'simulation_routines.R'))                 # functions to run simulation
source(paste0(source_folder,'collate_routines.R'))                                # functions to collate simulation outputs
source(paste0(source_folder,'plot_routines.R'))                                   # functions to plot collated outputs

if (action_type == 'collate_realisations'){
  current_filenames <- list.files(path = realisations_folder, pattern = policy_type, all.files = FALSE, 
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  if (!file.exists(collated_folder)){
    dir.create(collated_folder)
  }
} else if ((action_type == 'plot_impacts') | (action_type == 'plot_outcomes')){
  current_filenames <- list.files(path = collated_folder, pattern = policy_type, all.files = FALSE, 
                                  full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                  include.dirs = FALSE, no.. = FALSE)
  output_plot_folder = paste0(data_folder, 'policy_comparison_pdfs/')
  
  if (!file.exists(output_plot_folder)){
    dir.create(output_plot_folder)
  }
  
  if (write_pdf == TRUE){
    if (action_type == 'plot_impacts'){
      filename = paste0(output_plot_folder, policy_type, '_outcomes.pdf')
    } else if (action_type == 'plot_outcomes'){
      filename = paste0(output_plot_folder, policy_type, '_impacts.pdf')
    }
    pdf(filename, width = 8.3, height = 11.7) 
  } 
  
  if (offset_bank == TRUE){
    site_plot_lims = c(0, 6e5)
    site_impact_plot_lims = c(-1e6, 1e6)
  } else {
    site_plot_lims = c(0, 1e4)
    site_impact_plot_lims = c(-1e4, 1e4)
  }
  
  program_plot_lims = c(0e6, 5e6) 
  landscape_plot_lims = c(2.5e6, 5e6)
  
  program_impact_plot_lims = c(-6e5, 6e5) 
  landscape_impact_plot_lims = c(-6e5, 6e5)
  sets_to_plot = 5
  setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)
}

current_sim_group_filenames <- list.files(path = sim_group_folder, pattern = policy_type, all.files = FALSE, 
                                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                          include.dirs = FALSE, no.. = FALSE)

policy_num = length(current_filenames)
print(policy_num)


for (policy_ind in seq_len(policy_num)){
  sim_group = readRDS(paste0(sim_group_folder, current_sim_group_filenames[policy_ind]))
  
  if (action_type == 'collate_realisations'){
    realisations = readRDS(paste0(realisations_folder, current_filenames[policy_ind]))
    collated_realisations <- run_collate_routines(realisations,
                                                  sim_group$run_params,
                                                  sim_group$ecology_params, 
                                                  sim_group$current_policy_params, 
                                                  use_cfac_type_in_sim = TRUE, 
                                                  sim_group$simulation_data$decline_rates_initial, 
                                                  sim_group$simulation_data$parcels, 
                                                  sim_group$simulation_data$initial_ecology) #take simulation ouputs and calculate gains and losses

    saveRDS(collated_realisations, paste0(collated_folder, sim_group$current_policy_params$sim_characteristics, '.rds')) 
  } else if (action_type == 'plot_outcomes'){
    current_collated_realisation = readRDS(paste0(collated_folder, sim_group$current_policy_params$sim_characteristics, '.rds')) 
    
    plot_outcome_set(current_collated_realisation,
                     sim_group$current_policy_params,
                     offset_bank,
                     site_plot_lims,
                     program_plot_lims, 
                     landscape_plot_lims,
                     sets_to_plot,
                     eco_ind = 1, 
                     lwd_vec = c(3, 0.5), 
                     edge_title = policy_type, 
                     time_steps = sim_group$run_params$time_steps) 

  } else if (action_type == 'plot_impacts'){
    current_collated_realisation = readRDS(paste0(collated_folder, sim_group$current_policy_params$sim_characteristics, '.rds')) 
    plot_impact_set(current_collated_realisation, 
                    sim_group$current_policy_params, 
                    site_impact_plot_lims,
                    program_impact_plot_lims, 
                    landscape_impact_plot_lims,
                    sets_to_plot,
                    eco_ind = 1, 
                    lwd_vec = c(3, 0.5), 
                    edge_title = policy_type, 
                    time_steps = sim_group$run_params$time_steps, 
                    offset_bank,
                    sim_group$parcels$land_parcel_num)
  }
  print(paste0('policy ', policy_ind, ' done'))
}


if (write_pdf == TRUE){
  dev.off()
}
