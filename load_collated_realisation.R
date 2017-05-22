rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = '~/Documents/R_Codes/Offsets_Working_Feb_3/'

policy_type = "net_gains_offset_bank_FALSE"
offset_bank = FALSE
load_grassland_data = FALSE
load_collated_realisations = FALSE
write_pdf = FALSE

if (load_grassland_data == TRUE){
  parcel_num = 1238
  collated_folder = '~/Documents/offset_plots_new/collated_realisations/grasslands/'
  realisations_folder = '~/Documents/offset_plots_new/realisations/'
  sim_group_folder = '~/Documents/offset_plots_new/sim_group/'
} else{
  parcel_num = 1600
  collated_folder = '~/Documents/offset_plots_new/reals/'
  realisations_folder = '/Volumes/Seagate\ Backup\ Plus\ Drive/offset_sims/200_devs/'
  sim_group_folder = '/Volumes/Seagate\ Backup\ Plus\ Drive/offset_sims/sim_group/'
}

output_folder = paste('~/Documents/offset_plots_new/policy_comparison_pdfs/', sep = '', collapse = '')

if (!file.exists(output_folder)){
  dir.create(output_folder)
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

for (policy_ind in seq_len(policy_num)){
  
  if (load_collated_realisations == TRUE){
    collated_realisations = readRDS(paste(collated_folder, current_collated_filenames[policy_ind], sep = '', collapse = ''))

  } else {
   
    realisations = readRDS(paste(realisations_folder, current_realisations_filenames[policy_ind], sep = '', collapse = ''))
    sim_group = readRDS(paste(sim_group_folder, current_sim_group_filenames[policy_ind], sep = '', collapse = ''))
    collated_realisations <- collate_realisations(realisations, 
                                                  sim_group$global_params, 
                                                  sim_group$program_params_to_use, 
                                                  use_cfac_type_in_sim = TRUE, 
                                                  sim_group$decline_rates_initial, 
                                                  sim_group$parcels, 
                                                  sim_group$initial_ecology) #take simulation ouputs and calculate gains and losses
  }
  
  collated_realisation_set[[policy_ind]] = collated_realisations
  program_params_set[[policy_ind]] = sim_group$program_params_to_use
  
}

if (write_pdf == TRUE){
  filename = paste(output_folder, policy_type, '_outcomes.pdf', sep = '', collapse = '')
  pdf(filename, width = 8.3, height = 11.7) 
}

# if (offset_bank == TRUE){
#   site_plot_lims = c(0, 6e6)
# } else{
#   site_plot_lims = c(0, 1e4)
# }
  
plot_policy_outcome_comparisons(collated_realisation_set,
                                program_params_set,
                                offset_bank,
                                site_plot_lims = c(0, 1e4),
                                program_plot_lims = c(0.5e6, 3.5e6), 
                                landscape_plot_lims = c(2.5e6, 5e6),
                                sets_to_plot = 50,
                                eco_ind = 1, 
                                lwd_vec = c(3, 0.5), 
                                edge_title = policy_type, 
                                time_steps = 50) 

if (write_pdf == TRUE){
  dev.off()
}

if (write_pdf == TRUE){
  filename = paste(output_folder, policy_type, '_impacts.pdf', sep = '', collapse = '')
  pdf(filename, width = 8.3, height = 11.7) 
}

# if (offset_bank == TRUE){
#   site_impact_plot_lims = c(-1.5e6, 1.5e6)
#   program_impact_plot_lims = site_impact_plot_lims 
#   landscape_impact_plot_lims = site_impact_plot_lims
# } else{ 
#   site_impact_plot_lims = c(-1e4, 1e4)
#   program_impact_plot_lims = c(-1.5e6, 1.5e6)
#   landscape_impact_plot_lims = c(-0.5e6, 0.5e6)
# }

plot_policy_impact_comparisons(collated_realisation_set,
                               program_params_set,
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

if (write_pdf == TRUE){
  dev.off()
}




