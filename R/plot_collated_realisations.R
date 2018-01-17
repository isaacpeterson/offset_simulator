#' Plots the results of the Offset Simulator run
#' @param user_plot_params user configured plotting parameters to use
#' @param simulation_folder user configured simulation folder to use
#' @param run_number
#' @param loglevel logging level to use, for instance futile.logger::INFO
#' @import futile.logger
#' @export
#' 
#' 
osim.plot <- function(user_plot_params = NULL, simulation_folder = NULL, run_number = NULL, loglevel = INFO){
  
  if (is.null(user_plot_params)) {
    flog.error('provide plot params file')
    stop()
  } 
    
  plot_params = overwrite_current_params(user_plot_params, 
                                         default_params = initialise_default_plot_params())
    
  if (!is.null(simulation_folder)){
    base_folder = paste0(simulation_folder, '/simulation_runs/')
  } else {
    base_folder = paste0(getwd(), '/simulated_data/simulation_runs')
  }
  
  if (!is.null(run_number)){
    current_run = run_number
  } else {
    filenames = list.files(path = base_folder, all.files = FALSE,
                         full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                         include.dirs = FALSE, no.. = FALSE, pattern='^[0-9]{1,45}$')
  
    current_run = as.numeric(filenames[length(filenames)])
  }
  
 
  base_folder = paste0(base_folder, formatC(current_run, width = 5, format = "d", flag = "0"), '/')
  if (!dir.exists(base_folder)){
    flog.error('simulation folder does not exist')
    stop()
  } 
  collated_folder = paste0(base_folder, '/collated_outputs/')  # LOCATION OF COLLATED FILES
  simulation_params_folder = paste0(base_folder, '/simulation_params/')
  
  if (length(plot_params$output_plot_folder) == 0){
    output_plot_folder = collated_folder
  } else {
    output_plot_folder = plot_params$output_plot_folder
  }
  
  if (plot_params$plot_type == 'impacts'){
    output_pdf_filename = paste0(output_plot_folder, '/impacts.pdf')
  } else if (plot_params$plot_type == 'outcomes'){
    output_pdf_filename = paste0(output_plot_folder, '/outcomes.pdf')
  }
  
  flog.threshold(loglevel)
  
  # Set the output filename, and open the pdf file for reading
  if (plot_params$write_pdf == TRUE){
    flog.info('will start writing to PDF %s', output_pdf_filename)
    pdf(output_pdf_filename, width = 8.3, height = 11.7)
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
  
  
  global_params_filename <- paste0(simulation_params_folder, '/global_params.rds')
  if (!file.exists(global_params_filename)){
    flog.error(paste('offsetsim run parameter file not found in ', global_params_filename))
    stop()
  } else {
    flog.info('reading %s', global_params_filename)
    
    # get the parameters values for all scenarios in the run
    global_params = readRDS(global_params_filename)
  }
  
  

  
  # get the names of all the files containing the results
  scenario_filenames <- list.files(path = simulation_params_folder, pattern = '_combination_params', all.files = FALSE,
                                   full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                   include.dirs = FALSE, no.. = FALSE)
  check_plot_options(plot_params, global_params, scenario_filenames)
  
  if (!file.exists(output_plot_folder)){
    flog.info('creating output plot folder %s', output_plot_folder)
    dir.create(output_plot_folder)
  }
  
  for (plot_ind in seq_along(plot_params$plot_vec)){
    flog.info('_________________________________')
    if (plot_params$output_type == 'features'){
      feature_ind = plot_params$plot_vec[plot_ind]
    } else if (plot_params$output_type == 'scenarios'){
      scenario_ind = plot_params$plot_vec[plot_ind]
    } else if (plot_params$output_type == 'site_sets'){
      set_to_plot = plot_params$plot_vec[plot_ind]
    }
    toRead = paste0(simulation_params_folder, '/', scenario_filenames[scenario_ind])
    flog.trace('reading %s', toRead)
    current_combination_params = readRDS(toRead)
    
    if (!is.na(match('all', plot_params$plot_subset_type))){
      plot_flag = TRUE
    } else {
      param_inds_to_subset = match(plot_params$plot_subset_type, names(current_combination_params))
    
      if (any(!is.na(param_inds_to_subset)) & all(current_combination_params[param_inds_to_subset] == plot_params$plot_subset_param)) {
        plot_flag = TRUE 
      } else {
        plot_flag = FALSE
      }
      
    }
    if (plot_flag == TRUE){
      flog.info(' generating plot %d of type: %s', plot_ind, plot_params$plot_type)  
      
      collated_filenames = find_collated_files(file_path = collated_folder,
                                               scenario_string = formatC(scenario_ind, width = plot_params$string_width, format = "d", flag = "0"),
                                               feature_string = formatC(global_params$features_to_use_in_simulation[feature_ind],
                                                                        width = plot_params$string_width, format = "d", flag = "0"),
                                               plot_params$realisation_num)
      
      collated_realisations = bind_collated_realisations(collated_filenames)
      
      
      
      if (plot_params$plot_type == 'impacts'){
        plot_impact_set(collated_realisations,
                        current_combination_params,
                        plot_params,
                        global_params,
                        realisation_num = collated_realisations$realisation_num,
                        plot_ind,
                        feature_ind,
                        set_to_plot)
      } else if (plot_params$plot_type == 'outcomes'){
        plot_outcome_set(collated_realisations,
                         current_combination_params,
                         plot_params,
                         global_params,
                         realisation_num = collated_realisations$realisation_num,
                         plot_ind,
                         feature_ind,
                         set_to_plot)
        
      }
      
      #flog.info(' finished writing plot %d', plot_ind)
    } else {
      flog.info(' skipping plot %d', plot_ind )
    }
    
    
    
  }
  
  
  # Close the pdf file for reading
  if (plot_params$write_pdf == TRUE) {
    graphics.off()
    flog.info('closing PDF %s', output_pdf_filename)
  }
  flog.info('all done')
}
