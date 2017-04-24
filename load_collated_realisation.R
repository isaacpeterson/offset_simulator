rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = '~/Documents/R_Codes/Offsets_Working_Feb_3/'

policy_type = "restoration_gains_offset_bank_FALSE"
load_from_data = FALSE
load_coll_real_from_data = TRUE
write_pdf = FALSE

if (load_from_data == TRUE){
  parcel_num = 1238
  collated_folder = '~/Documents/offset_plots_new_2/collated_realisations/grasslands/'
} else{
  parcel_num = 1600
  collated_folder = '~/Documents/offset_plots_new_2/collated_realisations/'
}

output_folder = paste(collated_folder, 'pdf_comparisons/', sep = '', collapse = '')
if (!file.exists(output_folder)){
  dir.create(output_folder)
}


source(paste(source_folder, 'initialise_params.R', sep = '', collapse = ''))                              # functions to collate simulation outputs
source(paste(source_folder,'run_system_routines_modularised.R', sep = '', collapse = ''))                # functions to run simulation
source(paste(source_folder,'collate_routines.R', sep = '', collapse = ''))                                # functions to collate simulation outputs
source(paste(source_folder,'plot_routines.R', sep = '', collapse = ''))                                   # functions to plot collated outputs

current_policy_set <- list.files(path = collated_folder, pattern = policy_type, all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)

policy_num = length(current_policy_set)
print(policy_num)
collated_realisation_set = vector('list', policy_num)

for (policy_ind in seq(policy_num)){
  if (load_coll_real_from_data == TRUE){
    collated_realisations = readRDS(paste(collated_folder, current_policy_set[policy_ind], sep = '', collapse = ''))
  } else {
    collated_realisations <- collate_realisations(realisations, global_params, use_cfac_type_in_sim = TRUE, decline_rates_initial, land_parcels = parcels$land_parcels, initial_ecology) #take simulation ouputs and calculate gains and losses
  }
  collated_realisation_set[[policy_ind]] = collated_realisations
}

if (write_pdf == TRUE){
  filename = paste(output_folder, policy_type, '_outcomes.pdf', sep = '', collapse = '')
  pdf(filename, width = 8.3, height = 11.7) 
}
plot_policy_outcome_comparisons(collated_realisation_set,
                                 plot_lims = c(-6e5, 6e5), 
                                 eco_ind = 1, 
                                 lwd_vec = c(3, 0.5), 
                                 edge_title = '', 
                                 time_steps = 50) 
     
if (write_pdf == TRUE){
  dev.off()
}

if (write_pdf == TRUE){
  filename = paste(output_folder, policy_type, '_impacts.pdf', sep = '', collapse = '')
  pdf(filename, width = 8.3, height = 11.7) 
}

plot_policy_impact_comparisons(collated_realisation_set,
                               plot_lims = c(-6e5, 6e5), 
                               eco_ind = 1, 
                               lwd_vec = c(3, 0.5), 
                               edge_title = '', 
                               time_steps = 50, 
                               policy_type, 
                               parcel_num)

if (write_pdf == TRUE){
  dev.off()
}

