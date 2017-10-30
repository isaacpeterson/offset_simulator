source('plot_routines.R')                                   # functions to plot collated outputs
source('collate_routines.R')

#---------------------
# User parameters
#---------------------

plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
output_type = 'features' # set to 'features' for multiple feature layers or 'scenarios' for multiple scenarios
realisation_num = 'all' # 'all' or number to plot
offset_bank = FALSE
write_pdf = FALSE

plot_site_offset_impact = TRUE 
plot_site_dev_impact = TRUE
plot_site_net_impact = TRUE


plot_site_offset_outcome = TRUE
plot_site_dev_outcome = TRUE

run_number = 7 # for output plot name
example_set_to_plot = 3 # example site to plot
plot_vec = 1:2 #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
string_width = 3 # how many digits are used to store scenario index and realisation index

# write plots to nx * ny subplots
setup_sub_plots(nx = 3, ny = 2, x_space = 5, y_space = 5)
                
base_folder = paste0('~/offset_data/uruguay/simulation_runs/', 
                        formatC(run_number, width = 5, format = "d", flag = "0"), '/')


collated_folder = paste0(base_folder, '/collated_outputs/')  # LOCATION OF COLLATED FILES

simulation_params_folder = paste0(base_folder, '/simulation_params/')
#simulation_params_folder = collated_folder


#collated_folder = '/Users/ascelin/analysis/src/offset_simulator/data3/collated_realisations/'
output_plot_folder = collated_folder

site_outcome_plot_lims_set = rep(list(c(0, 3e2)), length(plot_vec))
program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), length(plot_vec))
landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), length(plot_vec))

site_impact_plot_lims_set = rep(list(c(-2e3, 2e3)), length(plot_vec))
program_impact_plot_lims_set = rep(list(c(-1e4, 1e4)), length(plot_vec)) 
landscape_impact_plot_lims_set = rep(list(c(-6e4, 0)), length(plot_vec))



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

if (run_params$total_dev_num < example_set_to_plot){
  stop (paste('chosen example set to plot needs to be less than ', run_params$total_dev_num))
}


if (!file.exists(output_plot_folder)){
  dir.create(output_plot_folder)
}

# Set the output filename, and open the pdf file for reading
if (write_pdf == TRUE){pdf(filename, width = 8.3, height = 11.7)} 

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
} else {
  if (run_params$feature_num < max(plot_vec)){
      stop ( paste('\nERROR: plot_vec exceeds number of features (', run_params$feature_num, ')'))
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
                                           feature_string = formatC(run_params$features_to_use_in_simulation[feature_ind], 
                                                                    width = string_width, format = "d", flag = "0"), 
                                           realisation_num)
  
  collated_realisations = bind_collated_realisations(collated_filenames)
  
  if (plot_type == 'impacts'){
    plot_impact_set(collated_realisations, 
                    plot_site_offset_impact, 
                    plot_site_dev_impact, 
                    plot_site_net_impact, 
                    output_type,
                    current_policy_params, 
                    site_impact_plot_lims_set[[plot_ind]],
                    program_impact_plot_lims_set[[plot_ind]], 
                    landscape_impact_plot_lims_set[[plot_ind]], 
                    example_set_to_plot,
                    lwd_vec = c(3, 0.5), 
                    time_steps = run_params$time_steps, 
                    parcel_num = vector(),
                    realisation_num = collated_realisations$realisation_num,
                    feature_ind = run_params$features_to_use_in_simulation[feature_ind]) 
  } else {
    plot_outcome_set(collated_realisations,
                     plot_site_offset_outcome, 
                     plot_site_dev_outcome, 
                     output_type,
                     current_policy_params,
                     site_outcome_plot_lims_set[[plot_ind]],
                     program_outcome_plot_lims_set[[plot_ind]], 
                     landscape_outcome_plot_lims_set[[plot_ind]],
                     example_set_to_plot,
                     lwd_vec = c(3, 0.5), 
                     time_steps = run_params$time_steps,
                     realisation_num = collated_realisations$realisation_num, 
                     feature_ind) 
  }
  
  print(paste0('policy ', scenario_ind, ' done'))
  
}


# Close the pdf file for reading
if (write_pdf == TRUE) dev.off()

