source('plot_routines.R')                                   # functions to plot collated outputs
source('collate_routines.R')

#---------------------
# User parameters
#---------------------

plot_type = 'outcomes' # can be 'outcomes'  or 'impacts',
output_type = 'scenarios' # set to 'features' for multiple feature layers or 'scenarios' for multiple scenarios
realisation_num = 'all' # 'all' or number to plot
offset_bank = FALSE
write_pdf = FALSE
run_number = 31 # for output plot name
sets_to_plot = 10 #example offset/development set to plot
plot_vec = 1:12
string_width = 3 #how many digits are used to store scenario index and realisation index

# base_folder = paste0('~/offset_data/simulated/simulation_runs/', 
#                         formatC(run_number, width = 5, format = "d", flag = "0"), '/')
base_folder = '~/Downloads/00002/'

collated_folder = paste0(base_folder, '/collated_outputs/')  # LOCATION OF COLLATED FILES

simulation_params_folder = paste0(base_folder, '/simulation_params/')
#simulation_params_folder = collated_folder

#collated_folder = '/Users/ascelin/analysis/src/offset_simulator/data3/collated_realisations/'
output_plot_folder = collated_folder
site_outcome_plot_lims_set = rep(list(c(0, 1e4)), length(plot_vec))
site_impact_plot_lims_set = rep(list(c(-1e4, 1e4)), length(plot_vec))

program_outcome_plot_lims_set = rep(list(c(0e6, 10e6)), length(plot_vec))
landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), length(plot_vec))

program_impact_plot_lims_set = rep(list(c(-1e5, 1e5)), length(plot_vec)) 
landscape_impact_plot_lims_set = rep(list(c(-6e5, 0)), length(plot_vec))


if (output_type == 'scenarios'){
  feature_ind = 1
} else if (output_type == 'features'){
  scenario_ind = 1
}




if (plot_type == 'impacts'){
  filename = paste0(output_plot_folder, '/impacts.pdf')
} else if (plot_type == 'outcomes'){
  filename = paste0(output_plot_folder, '/outcomes.pdf')
}

check_plot_options()

run_params_filename <- list.files(path = simulation_params_folder, pattern = 'run_params.rds', all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)

if (length(run_params_filename) > 0){
  run_params = readRDS(paste0(simulation_params_folder, '/run_params.rds'))
} else {
  stop (paste('no simulation run parameter file in pwd'))
}

if (!file.exists(output_plot_folder)){
  dir.create(output_plot_folder)
}

# Set the output filename, and open the pdf file for reading
if (write_pdf == TRUE){pdf(filename, width = 8.3, height = 11.7)} 

# write plots to nx * ny subplots
setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)

scenario_filenames <- list.files(path = simulation_params_folder, pattern = '_policy_params', all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)

if (length(plot_vec) == 0){
  plot_vec = seq_along(scenario_filenames)
}

if (output_type == 'scenarios'){
  if (length(scenario_filenames) == 0){
    stop( paste('\nERROR: No files that match _policy_params found in', simulation_params_folder) )
  } else if (length(scenario_filenames) < max(plot_vec)){
    stop ( paste('\nERROR: only ', length(scenario_filenames), ' scenario params files found, plot_vec parameter does not match'))
  }
}
for (plot_ind in plot_vec){
  if (output_type == 'features'){
    feature_ind = plot_ind
  } else {
    scenario_ind = plot_ind
  }
  current_policy_params = readRDS(paste0(simulation_params_folder, '/', scenario_filenames[scenario_ind]))
  collated_filenames = find_collated_files(file_path = collated_folder, 
                                           scenario_string = formatC(scenario_ind, width = string_width, format = "d", flag = "0"), 
                                           feature_string = formatC(feature_ind, width = string_width, format = "d", flag = "0"), 
                                           realisation_num)
  
  collated_realisations = bind_collated_realisations(collated_filenames)
  
  if (plot_type == 'impacts'){
    plot_impact_set(collated_realisations, 
                    output_type,
                    current_policy_params, 
                    site_impact_plot_lims_set[[plot_ind]],
                    program_impact_plot_lims_set[[plot_ind]], 
                    landscape_impact_plot_lims_set[[plot_ind]], 
                    sets_to_plot,
                    lwd_vec = c(3, 0.5), 
                    time_steps = run_params$time_steps, 
                    parcel_num = vector(),
                    realisation_num = collated_realisations$realisation_num,
                    feature_ind) 
  } else {
    plot_outcome_set(collated_realisations,
                     output_type,
                     current_policy_params,
                     site_outcome_plot_lims_set[[plot_ind]],
                     program_outcome_plot_lims_set[[plot_ind]], 
                     landscape_outcome_plot_lims_set[[plot_ind]],
                     sets_to_plot,
                     lwd_vec = c(3, 0.5), 
                     time_steps = run_params$time_steps,
                     realisation_num = collated_realisations$realisation_num, 
                     feature_ind) 
  }
  
  print(paste0('policy ', scenario_ind, ' done'))
  
}


# Close the pdf file for reading
if (write_pdf == TRUE) dev.off()

