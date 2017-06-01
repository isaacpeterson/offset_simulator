rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = paste(path.expand("~"), '/Documents/R_Codes/Offsets_Working_Feb_3/', sep = '', collapse = '')

action_type = 'plot_outcomes' #'collate_realisations', 'plot_impacts', or 'plot_outcomes'.
policy_type = "restoration_gains"  #'restoration_gains', 'avoided_degs', 'net_gains'
offset_bank = FALSE
write_pdf = TRUE

if (offset_bank == FALSE){
  policy_type = paste(policy_type, '_offset_bank_FALSE', sep = '', collapse = '')
} else {
  policy_type = 'offset_bank_TRUE'
}


data_folder = paste(path.expand("~"), '/offset_data/simulated/', sep = '', collapse = '')

collated_folder = paste(data_folder, 'collated_realisations/', sep = '', collapse = '')
realisations_folder = paste(data_folder, 'realisations/', sep = '', collapse = '')
sim_group_folder = paste(data_folder, 'sim_group/', sep = '', collapse = '')

source(paste(source_folder, 'initialise_params.R', sep = '', collapse = ''))                              # functions to collate simulation outputs
source(paste(source_folder,'run_system_routines_modularised.R', sep = '', collapse = ''))                 # functions to run simulation
source(paste(source_folder,'collate_routines.R', sep = '', collapse = ''))                                # functions to collate simulation outputs
source(paste(source_folder,'plot_routines.R', sep = '', collapse = ''))                                   # functions to plot collated outputs

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
  output_plot_folder = paste(data_folder, 'policy_comparison_pdfs/', sep = '', collapse = '')
  
  if (!file.exists(output_plot_folder)){
    dir.create(output_plot_folder)
  }
  
  if (write_pdf == TRUE){
    if (action_type == 'plot_impacts'){
      filename = paste(output_plot_folder, policy_type, '_outcomes.pdf', sep = '', collapse = '')
    } else if (action_type == 'plot_outcomes'){
      filename = paste(output_plot_folder, policy_type, '_impacts.pdf', sep = '', collapse = '')
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
  
  setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)
}

current_sim_group_filenames <- list.files(path = sim_group_folder, pattern = policy_type, all.files = FALSE, 
                                          full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                          include.dirs = FALSE, no.. = FALSE)

policy_num = length(current_filenames)
print(policy_num)


for (policy_ind in seq_len(policy_num)){
  sim_group = readRDS(paste(sim_group_folder, current_sim_group_filenames[policy_ind], sep = '', collapse = ''))
  sim_characteristics <- get_sim_characteristics(sim_group$program_params_to_use, sim_group$global_params$realisation_num)
  
  if (action_type == 'collate_realisations'){
    realisations = readRDS(paste(realisations_folder, current_filenames[policy_ind], sep = '', collapse = ''))
    collated_realisations <- run_collate_routines(realisations, 
                                                         sim_group$global_params, 
                                                         sim_group$program_params_to_use, 
                                                         use_cfac_type_in_sim = FALSE, 
                                                         sim_group$decline_rates_initial, 
                                                         sim_group$parcels, 
                                                         sim_group$initial_ecology) #take simulation ouputs and calculate gains and losses
    
    saveRDS(collated_realisations, paste(collated_folder, sim_characteristics, '.rds', sep = '', collapse = '')) 
  } else if (action_type == 'plot_outcomes'){
    current_collated_realisation = readRDS(paste(collated_folder, sim_characteristics, '.rds', sep = '', collapse = '')) 
    plot_outcome_set(current_collated_realisation,
                     current_program_params = sim_group$program_params_to_use,
                     offset_bank,
                     site_plot_lims, 
                     program_plot_lims, 
                     landscape_plot_lims,
                     sets_to_plot = 50,
                     eco_ind = 1, 
                     lwd_vec = c(3, 0.5), 
                     edge_title = policy_type, 
                     time_steps = 50) 
  } else if (action_type == 'plot_impacts'){
    current_collated_realisation = readRDS(paste(collated_folder, sim_characteristics, '.rds', sep = '', collapse = '')) 
    plot_impact_set(current_collated_realisation, 
                    current_program_params = sim_group$program_params_to_use, 
                    site_impact_plot_lims,
                    program_impact_plot_lims, 
                    landscape_impact_plot_lims,
                    sets_to_plot = 50,
                    eco_ind = 1, 
                    lwd_vec = c(3, 0.5), 
                    edge_title = policy_type, 
                    time_steps = 50, 
                    offset_bank,
                    sim_group$parcels$land_parcel_num)
  }
 print(paste('policy ', policy_ind, ' done'))
}


if (write_pdf == TRUE){
  dev.off()
}
