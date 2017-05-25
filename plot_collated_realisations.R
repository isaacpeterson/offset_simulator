rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = paste(path.expand("~"), '/Documents/R_Codes/Offsets_Working_Feb_3/', sep = '', collapse = '')

policy_type = "net_gains_offset_bank_FALSE"
offset_bank = FALSE
load_grassland_data = FALSE
load_collated_realisations = FALSE
write_pdf = FALSE

data_folder = paste(path.expand("~"), '/Documents/offset_data/', sep = '', collapse = '')

if (load_grassland_data == TRUE){
  data_folder = paste(data_folder, 'grassland/', sep = '', collapse = '')
  parcel_num = 1238
  
} else{
  data_folder = paste(data_folder, 'simulated/', sep = '', collapse = '')
  parcel_num = 1600
}

collated_folder = paste(data_folder, 'collated_realisations/', sep = '', collapse = '')
realisations_folder = paste(data_folder, 'realisations/', sep = '', collapse = '')
sim_group_folder = paste(data_folder, 'sim_group/', sep = '', collapse = '')

output_plot_folder = paste(data_folder, 'policy_comparison_pdfs/', sep = '', collapse = '')

if (!file.exists(output_plot_folder)){
  dir.create(output_plot_folder)
}

source(paste(source_folder, 'initialise_params.R', sep = '', collapse = ''))                              # functions to collate simulation outputs
source(paste(source_folder,'run_system_routines_modularised.R', sep = '', collapse = ''))                 # functions to run simulation
source(paste(source_folder,'collate_routines.R', sep = '', collapse = ''))                                # functions to collate simulation outputs
source(paste(source_folder,'plot_routines.R', sep = '', collapse = ''))                                   # functions to plot collated outputs

current_collated_filenames <- list.files(path = collated_folder, pattern = policy_type, all.files = FALSE, 
                                         full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                         include.dirs = FALSE, no.. = FALSE)
current_realisations_filenames <- list.files(path = realisations_folder, pattern = policy_type, all.files = FALSE, 
                                             full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                             include.dirs = FALSE, no.. = FALSE)
current_sim_group_filenames <- list.files(path = sim_group_folder, pattern = policy_type, all.files = FALSE, 
                                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                          include.dirs = FALSE, no.. = FALSE)

policy_num = length(current_realisations_filenames)
print(policy_num)
collated_realisation_set = vector('list', policy_num)
program_params_set = vector('list', policy_num)

setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)

for (policy_ind in 1){
  sim_group = readRDS(paste(sim_group_folder, current_sim_group_filenames[policy_ind], sep = '', collapse = ''))
  
  if (load_collated_realisations == TRUE){
    current_collated_realisation = readRDS(paste(collated_folder, current_collated_filenames[policy_ind], sep = '', collapse = ''))
    
  } else {
    
    realisations = readRDS(paste(realisations_folder, current_realisations_filenames[policy_ind], sep = '', collapse = ''))
    current_collated_realisation <- collate_realisations(realisations, 
                                                         sim_group$global_params, 
                                                         sim_group$program_params_to_use, 
                                                         use_cfac_type_in_sim = FALSE, 
                                                         sim_group$decline_rates_initial, 
                                                         sim_group$parcels, 
                                                         sim_group$initial_ecology) #take simulation ouputs and calculate gains and losses
  }
  
  plot_outcome_set(current_collated_realisation,
                   current_program_params = sim_group$program_params_to_use,
                   offset_bank,
                   site_plot_lims = c(0, 1e4),
                   program_plot_lims = c(0.5e6, 3.5e6), 
                   landscape_plot_lims = c(2.5e6, 5e6),
                   sets_to_plot = 50,
                   eco_ind = 1, 
                   lwd_vec = c(3, 0.5), 
                   edge_title = policy_type, 
                   time_steps = 50) 
  
  plot_impact_set(current_collated_realisation, 
                  current_program_params = sim_group$program_params_to_use, 
                  site_plot_lims = c(-5e3, 8e3),
                  program_plot_lims = c(-5e5, 6e5), 
                  landscape_plot_lims = c(-3e5, 3e5),
                  sets_to_plot = 50,
                  eco_ind = 1, 
                  lwd_vec = c(3, 0.5), 
                  edge_title = policy_type, 
                  time_steps = 50, 
                  offset_bank,
                  parcel_num)
  
}


# if (write_pdf == TRUE){
#   filename = paste(output_plot_folder, policy_type, '_outcomes.pdf', sep = '', collapse = '')
#   pdf(filename, width = 8.3, height = 11.7) 
# }
# 
# 
# if (write_pdf == TRUE){
#   dev.off()
# }
# 
# if (write_pdf == TRUE){
#   filename = paste(output_plot_folder, policy_type, '_impacts.pdf', sep = '', collapse = '')
#   pdf(filename, width = 8.3, height = 11.7) 
# }
# 
# 
# if (write_pdf == TRUE){
#   dev.off()
# }