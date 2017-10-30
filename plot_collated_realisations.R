rm(list = ls())
source('plot_routines.R')                                   # functions to plot collated outputs
source('collate_routines.R')

#---------------------
# User parameters
#---------------------
plot_params_file = 'user_params/plot_params.R'
source(plot_params_file)
plot_params <- initialise_plot_params()

# Set the output filename, and open the pdf file for reading
if (plot_params$write_pdf == TRUE){pdf(plot_params$filename, width = 8.3, height = 11.7)} 

# write plots to nx * ny subplots
setup_sub_plots(plot_params$nx, plot_params$ny, x_space = 5, y_space = 5)

if (plot_params$output_type == 'scenarios'){
  feature_ind = 1
} else if (plot_params$output_type == 'features'){
  scenario_ind = 1
}

if (file.exists(plot_params$run_params_filename) == FALSE){
  stop (paste('no simulation run parameter file in pwd'))
}
run_params = readRDS(plot_params$run_params_filename)
scenario_filenames <- list.files(path = plot_params$simulation_params_folder, pattern = '_policy_params', all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)

check_plot_options(plot_params, run_params, scenario_filenames)


if (!file.exists(plot_params$output_plot_folder)){
  dir.create(plot_params$output_plot_folder)
}


for (plot_ind in plot_params$plot_vec){
  if (plot_params$output_type == 'features'){
    feature_ind = plot_ind
  } else {
    scenario_ind = plot_ind
  }
  current_policy_params = readRDS(paste0(plot_params$simulation_params_folder, '/', scenario_filenames[scenario_ind]))
  collated_filenames = find_collated_files(file_path = plot_params$collated_folder, 
                                           scenario_string = formatC(scenario_ind, width = plot_params$string_width, format = "d", flag = "0"), 
                                           feature_string = formatC(run_params$features_to_use_in_simulation[feature_ind], 
                                                                    width = plot_params$string_width, format = "d", flag = "0"), 
                                           plot_params$realisation_num)
  
  collated_realisations = bind_collated_realisations(collated_filenames)
  
  if (plot_params$plot_type == 'impacts'){
    plot_impact_set(collated_realisations, 
                    plot_params$plot_site_offset_impact, 
                    plot_params$plot_site_dev_impact, 
                    plot_params$plot_site_net_impact, 
                    plot_params$output_type,
                    current_policy_params, 
                    plot_params$site_impact_plot_lims_set[[plot_ind]],
                    plot_params$program_impact_plot_lims_set[[plot_ind]], 
                    plot_params$landscape_impact_plot_lims_set[[plot_ind]], 
                    plot_params$example_set_to_plot,
                    plot_params$lwd_vec, 
                    time_steps = run_params$time_steps, 
                    parcel_num = vector(),
                    realisation_num = collated_realisations$realisation_num,
                    feature_ind = run_params$features_to_use_in_simulation[feature_ind]) 
  } else {
    plot_outcome_set(collated_realisations,
                     plot_params$plot_site_offset_outcome, 
                     plot_params$plot_site_dev_outcome, 
                     plot_params$output_type,
                     current_policy_params,
                     plot_params$site_outcome_plot_lims_set[[plot_ind]],
                     plot_params$program_outcome_plot_lims_set[[plot_ind]], 
                     plot_params$landscape_outcome_plot_lims_set[[plot_ind]],
                     plot_params$example_set_to_plot,
                     plot_params$lwd_vec, 
                     time_steps = run_params$time_steps,
                     realisation_num = collated_realisations$realisation_num, 
                     feature_ind) 
  }
  
  print(paste0('policy ', scenario_ind, ' done'))
  
}


# Close the pdf file for reading
if (plot_params$write_pdf == TRUE) dev.off()

