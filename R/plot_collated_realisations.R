#' Plots the results of the Offset Simulator run
#' @param plot_params user configured plotting parameters to use
#' @param loglevel logging level to use, for instance futile.logger::INFO
#' @import futile.logger
#' @export
osim.plot <- function(plot_params_file, loglevel = INFO){

  if (is.null(plot_params_file)) {
    stop('please provide offsetsim plotting configuration')
  } else if (!plot_is_config_valid(plot_params_file)) {
    stop('please provide a valid offsetsim plotting configuration')
  }

source(plot_params_file)
plot_params <- initialise_plot_params()

  flog.threshold(loglevel)


# Set the output filename, and open the pdf file for reading
if (plot_params$write_pdf == TRUE){
  flog.info('will start writing to PDF %s', plot_params$filename)
  pdf(plot_params$filename, width = 8.3, height = 11.7)
}

# write plots to nx * ny subplots
setup_sub_plots(plot_params$nx, plot_params$ny, x_space = 5, y_space = 5)

if (plot_params$output_type == 'scenarios'){
  feature_ind = 1
  set_to_plot = plot_params$sets_to_plot
} else if (plot_params$output_type == 'features'){
  scenario_ind = 1
  set_to_plot = plot_params$sets_to_plot
} else if (plot_params$output_type == 'site_sets'){
  scenario_ind = 1
  feature_ind = 1
  plot_params$plot_program = FALSE
  plot_params$plot_landscape = FALSE
}

if (file.exists(plot_params$run_params_filename) == FALSE){
  stop (paste('offsetsim run parameter file not found in ', plot_params$run_params_filename))
}


flog.info('reading %s', plot_params$run_params_filename)

run_params = readRDS(plot_params$run_params_filename)
scenario_filenames <- list.files(path = plot_params$simulation_params_folder, pattern = '_policy_params', all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                 include.dirs = FALSE, no.. = FALSE)

check_plot_options(plot_params, run_params, scenario_filenames)

if (!file.exists(plot_params$output_plot_folder)){
  flog.info('creating output plot folder %s', plot_params$output_plot_folder)
  dir.create(plot_params$output_plot_folder)
}

for (plot_ind in seq_along(plot_params$plot_vec)){
  flog.info('creating plot %d', plot_ind)
  if (plot_params$output_type == 'features'){
    feature_ind = plot_params$plot_vec[plot_ind]
  } else if (plot_params$output_type == 'scenarios'){
    scenario_ind = plot_params$plot_vec[plot_ind]
  } else if (plot_params$output_type == 'site_sets'){
    set_to_plot = plot_params$plot_vec[plot_ind]
  }
  toRead = paste0(plot_params$simulation_params_folder, '/', scenario_filenames[scenario_ind])
  flog.info(' reading %s', toRead)
  current_policy_params = readRDS(toRead)

  collated_filenames = find_collated_files(file_path = plot_params$collated_folder,
                                           scenario_string = formatC(scenario_ind, width = plot_params$string_width, format = "d", flag = "0"),
                                           feature_string = formatC(run_params$features_to_use_in_simulation[feature_ind],
                                                                    width = plot_params$string_width, format = "d", flag = "0"),
                                           plot_params$realisation_num)

  collated_realisations = bind_collated_realisations(collated_filenames)

  param_inds_to_subset = match(plot_params$plot_subset_type, names(current_policy_params))
  flog.info(' writing plot %d of type %s', plot_ind, plot_params$plot_type)
  if (all(current_policy_params[param_inds_to_subset] == plot_params$plot_subset_param)){
    if (plot_params$plot_type == 'impacts'){
      plot_impact_set(collated_realisations,
                      current_policy_params,
                      plot_params,
                      run_params,
                      realisation_num = collated_realisations$realisation_num,
                      plot_ind,
                      feature_ind,
                      set_to_plot)
    } else if (plot_params$plot_type == 'outcomes'){
      plot_outcome_set(collated_realisations,
                       current_policy_params,
                       plot_params,
                       run_params,
                       realisation_num = collated_realisations$realisation_num,
                       plot_ind,
                       feature_ind,
                       set_to_plot)

    }
  }

  flog.info(' finished writing plot %d', plot_ind)

}


# Close the pdf file for reading
if (plot_params$write_pdf == TRUE) {
  graphics.off()
  flog.info('closing PDF %s', plot_params$filename)
}
flog.info('all done')
}
