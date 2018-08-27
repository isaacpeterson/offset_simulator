#' Plots the results of the Offset Simulator run
#' @param user_output_params user configured plotting parameters to use
#' @param simulation_folder user configured simulation folder to use
#' @param run_number user defined folder
#' @param loglevel logging level to use, for instance futile.logger::INFO
#' @import futile.logger
#' @export

osim.output <- function(user_output_params = NULL, simulation_folder = NULL, loglevel = INFO){
  flog.threshold(loglevel)
  if (is.null(user_output_params)) {
    flog.error('provide plot params file')
    stop()
  } 
  
  output_params = overwrite_current_params(user_output_params, default_params = initialise_default_output_params())

  collated_folder = paste0(simulation_folder, '/collated_outputs/')  # LOCATION OF COLLATED FILES
  simulation_params_folder = paste0(simulation_folder, '/simulation_params/')
  simulation_output_folder = paste0(simulation_folder, '/simulation_outputs/')
  
  global_params = readRDS(paste0(simulation_params_folder, 'global_params.rds'))
  feature_params = readRDS(paste0(simulation_params_folder, 'feature_params.rds'))
  site_characteristics = readRDS(paste0(global_params$simulation_folder, 'simulation_inputs/', 'site_characteristics.rds'))
  site_element_indexes_grouped_by_condition_classes = readRDS(paste0(global_params$simulation_folder, 'simulation_inputs/', 'site_element_indexes_grouped_by_condition_classes.rds'))
  
  # read in file with stored param settings to identify plots easier
  param_variants_filename = paste0(simulation_params_folder, 'param_variants.rds')
  if (file.exists(param_variants_filename)){
    param_variants = readRDS(paste0(simulation_params_folder, 'param_variants.rds'))
  }
  
  # get the names of all parameter files, separated into run scenarios
  scenario_filenames <- list.files(path = simulation_params_folder, pattern = '_simulation_params.rds', all.files = FALSE,
                                   full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                   include.dirs = FALSE, no.. = FALSE)
  
  if ( (class(output_params$scenario_vec) == 'character')){
    if (output_params$scenario_vec == 'all'){
      scenario_vec = 1:length(scenario_filenames)
    }
  } else {
    scenario_vec = output_params$scenario_vec
  }
  
  if (length(output_params$output_plot_folder) == 0){
    output_plot_folder = collated_folder
  } else {
    output_plot_folder = output_params$output_plot_folder
  }
  
  if (!file.exists(output_plot_folder)){
    flog.info('creating output plot folder %s', output_plot_folder)
    dir.create(output_plot_folder)
  }
  
  
  raster_output_folder = paste0(collated_folder, '/output_rasters/')
  if (!dir.exists(raster_output_folder)){
    dir.create(raster_output_folder)
  }
  
  
  if (output_params$output_plot == TRUE){
    # Set the output filename, and open the pdf file for reading
    if (output_params$write_pdf == TRUE){
      if (output_params$plot_type == 'impacts'){
        output_pdf_filename = paste0(output_plot_folder, '/impacts.pdf')
      } else if (output_params$plot_type == 'outcomes'){
        output_pdf_filename = paste0(output_plot_folder, '/outcomes.pdf')
      }
      flog.info('writing to PDF %s', output_pdf_filename)
      pdf(output_pdf_filename, width = 8.3, height = 11.7)
      
    }
    setup_sub_plots(output_params$nx, output_params$ny, x_space = 5, y_space = 5)
    plot_ctr <- 1
    
    for (scenario_ind in scenario_vec){
      output_scenario(output_type = 'plot', 
                      simulation_params_folder, 
                      simulation_output_folder, 
                      raster_output_folder,
                      scenario_filenames, 
                      scenario_ind, 
                      output_params, 
                      param_variants_filename,
                      global_params, 
                      collated_folder, 
                      plot_ctr, 
                      site_element_indexes_grouped_by_condition_classes, 
                      site_characteristics, 
                      feature_params)
      plot_ctr <- plot_ctr + 1
    }
    if (output_params$write_pdf == TRUE) {
      graphics.off()
      flog.info('closing PDF %s', output_pdf_filename)
    }
  }
  
  if (output_params$output_movie == TRUE){
    
    for (scenario_ind in scenario_vec){
      output_scenario(output_type = 'feature_layer', 
                      simulation_params_folder, 
                      simulation_output_folder, 
                      raster_output_folder,
                      scenario_filenames, 
                      scenario_ind, 
                      output_params, 
                      param_variants_filename,
                      global_params, 
                      collated_folder, 
                      plot_ctr = vector(), 
                      site_element_indexes_grouped_by_condition_classes, 
                      site_characteristics, 
                      feature_params)
      
    }
    
  } 
  
  flog.info('all done')
}



# find the current simulation folder - the function looks in the base_folder directory if supplied 
# and assumes the current working directory contains the simulation folder otherwise. If the user supplies the
# current run number the function looks for thaht specified folder and looks for the latest run otherwise.

#' @export
find_current_run_folder <- function(base_folder = NULL, run_number = NULL){
  
  #if (!is.null(base_folder) & (length(base_folder) > 0) & (base_folder != 'default')){
  if (!is.null(base_folder)){
    if (base_folder == 'default'){
      simulation_folder = 'simulation_runs/'
    } else {
      simulation_folder = paste0(base_folder, '/simulation_runs/')
    }
  } else { 
    simulation_folder = 'simulation_runs/'
  }
  
  if (!is.null(run_number)){
    current_run = run_number
  } else {
    filenames = list.files(path = simulation_folder, all.files = FALSE,
                           full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                           include.dirs = FALSE, no.. = FALSE, pattern='^[0-9]{1,45}$')
    current_run = as.numeric(filenames[length(filenames)])
  }
  
  simulation_folder = paste0(simulation_folder, formatC(current_run, width = 5, format = "d", flag = "0"), '/')
  if (!dir.exists(simulation_folder)){
    flog.error('simulation_folder %s does not exist.', simulation_folder)
    stop()
  } else{
    return(simulation_folder)
  }
}




output_scenario <- function(output_type, simulation_params_folder, simulation_output_folder, raster_output_folder, 
                            scenario_filenames, scenario_ind, output_params, param_variants_filename,
                            global_params, collated_folder, plot_ctr, site_element_indexes_grouped_by_condition_classes, site_characteristics, feature_params){
  
  flog.info('_________________________________')
  
  file_to_Read = paste0(simulation_params_folder, '/', scenario_filenames[scenario_ind])
  flog.trace('reading %s', file_to_Read)
  current_simulation_params = readRDS(file_to_Read)
  current_data_dir = paste0(simulation_output_folder, '/scenario_', 
                            formatC(scenario_ind, width = global_params$string_width, format = "d", flag = "0"),
                            '/realisation_', formatC(output_params$example_realisation_to_output, width = global_params$string_width, format = "d", flag = "0"), '/') 
  
  example_simulation_outputs = readRDS(paste0(current_data_dir,'realisation_', 
                                              formatC(output_params$example_realisation_to_output, width = global_params$string_width, format = "d", flag = "0"), 
                                              '_outputs.rds'))
  
  if (class(output_params$features_to_output) == 'character'){
    if (output_params$features_to_output == 'all'){
      features_to_output = current_simulation_params$features_to_use_in_simulation
    } else {
      flog.error("features to plot parameter is poorly defined assign vector of integers or 'all'")
    }
  }  else {
    features_to_output = output_params$features_to_output
  }
  
  
  if (output_params$print_dev_offset_sites == TRUE){
    sites_used = collated_realisations$sites_used
    stats_to_use = which(unlist(lapply(seq_along(sites_used), function(i) length(unlist(sites_used[[i]]))>0)))
    mean_sites_used = lapply(stats_to_use, function (i) round(mean(unlist( sites_used[[i]] ))))
    flog.info(rbind(names(sites_used[stats_to_use]), mean_sites_used))
  }
  
  
  if (output_type == 'plot'){
    
    param_inds_to_subset = match(output_params$plot_subset_type, names(current_simulation_params))
    
    if (any(!is.na(param_inds_to_subset)) & all(current_simulation_params[param_inds_to_subset] == output_params$plot_subset_param)) {
      plot_flag = TRUE 
    } else {
      if (length(output_params$plot_subset_type) > 1){
        plot_flag = FALSE
      } else {
        if (output_params$plot_subset_type == 'all'){
          plot_flag = TRUE
        } else {
          plot_flag = FALSE
        }
      }
    } 
    
    if (plot_flag == FALSE){
      flog.trace(' skipping scenario %d', scenario_ind )
    } else {
      
      flog.info(' generating plot %d (scen %d of type: %s)', plot_ctr, scenario_ind, output_params$plot_type)  
      if (file.exists(param_variants_filename)){
        flog.info(rbind(names(param_variants[[scenario_ind]]), as.vector(param_variants[[scenario_ind]]))) 
      }
      
      if (output_params$plot_type == 'impacts'){
        
        site_plot_lims = output_params$site_impact_plot_lims_set[[1]]
        program_plot_lims = output_params$program_impact_plot_lims_set[[1]]
        landscape_plot_lims = output_params$landscape_impact_plot_lims_set[[1]]
      } else {
        site_plot_lims = output_params$site_outcome_plot_lims_set[[1]]
        program_plot_lims = output_params$program_outcome_plot_lims_set[[1]]
        landscape_plot_lims = output_params$landscape_outcome_plot_lims_set[[1]]
      }
      
      
      output_collated_features(features_to_use = features_to_output, 
                               plot_offset_metric = FALSE, 
                               scenario_ind, 
                               output_params,
                               global_params, 
                               feature_params,
                               collated_realisations, 
                               current_simulation_params, 
                               site_element_indexes_grouped_by_condition_classes,
                               example_simulation_outputs,
                               site_characteristics,
                               current_data_dir, 
                               collated_folder, 
                               raster_output_folder)
      
      if (output_params$plot_offset_metric == TRUE){
        output_collated_features(features_to_use = 1, 
                                 plot_offset_metric = TRUE, 
                                 scenario_ind, 
                                 output_params, 
                                 global_params, 
                                 feature_params,
                                 collated_realisations, 
                                 current_simulation_params, 
                                 site_element_indexes_grouped_by_condition_classes,
                                 example_simulation_outputs,
                                 site_characteristics,
                                 current_data_dir, 
                                 collated_folder,
                                 raster_output_folder)
      }
      
    } 
  } 
  
} 


output_collated_features <- function(features_to_use, plot_offset_metric, scenario_ind, output_params, global_params, feature_params, collated_realisations, current_simulation_params, 
                                     site_element_indexes_grouped_by_condition_classes, example_simulation_outputs, site_characteristics, current_data_dir, collated_folder, raster_output_folder){
  
  for (feature_ind in features_to_use){
    
    if (plot_offset_metric == TRUE){
      collated_filenames = paste0(collated_folder, list.files(path = collated_folder, all.files = FALSE,
                                      full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                      include.dirs = FALSE, no.. = FALSE, pattern = '_metric'))
      
    }  else {
      collated_filenames = find_collated_files(file_path = collated_folder,
                                               scenario_string = formatC(scenario_ind, width = global_params$string_width, format = "d", flag = "0"),
                                               feature_string = formatC(feature_ind, width = global_params$string_width, format = "d", flag = "0"),
                                               output_params$realisation_num)
      
      
    }
    
    collated_realisations = bind_collated_realisations(collated_filenames)
    
    if (output_params$output_plot == TRUE){
      flog.info('writing plot outputs')
      
      
      plot_outputs(output_params, feature_ind, collated_realisations, current_simulation_params)
    }
    
    if ((output_params$output_mov == TRUE) | (output_params$save_output_raster == TRUE)){
      flog.info('writing feature_layer outputs')
      if (plot_offset_metric == FALSE){
      current_element_indexes_grouped_by_feature_condition_class = lapply(seq_along(site_element_indexes_grouped_by_condition_classes), 
                                                                          function(i) site_element_indexes_grouped_by_condition_classes[[i]][[feature_ind]])
      
      output_feature_layers(feature_ind, 
                            current_data_dir, 
                            example_simulation_outputs,
                            raster_output_folder, 
                            plot_offset_metric, 
                            save_output_raster = output_params$save_output_raster, 
                            output_params$output_mov,
                            output_params$mov_file_type, 
                            current_element_indexes_grouped_by_feature_condition_class, 
                            current_simulation_params$time_steps, 
                            site_characteristics, 
                            scale_factor = max(unlist(feature_params$condition_class_bounds[[feature_ind]])), 
                            mov_file_prefix = paste0(raster_output_folder, 'feature_', formatC(feature_ind, width = global_params$string_width, format = "d", flag = "0"), '_'))
      
      } else {
        # get the largest value for each condition class
        vals_to_transform = lapply(seq_along(feature_params$condition_class_bounds), function(i) max(unlist(feature_params$condition_class_bounds[[i]])))
        #transform this set to user metric and use this to scale the colors
        scale_factor = user_transform_function(vals_to_transform, transform_params = current_simulation_params$transform_params)

        output_feature_layers(feature_ind, 
                              current_data_dir, 
                              example_simulation_outputs,
                              raster_output_folder, 
                              use_offset_metric = TRUE, 
                              save_output_raster = output_params$save_output_raster, 
                              output_params$output_mov,
                              output_params$mov_file_type, 
                              current_element_indexes_grouped_by_feature_condition_class = vector(), 
                              current_simulation_params$time_steps, 
                              site_characteristics, 
                              scale_factor, 
                              mov_file_prefix = paste0(raster_output_folder, 'metric_'))
      } 
    } 
    
    if (output_params$output_csv == TRUE){
      
      flog.info('writing csv outputs')
      write.table( data.frame(collated_realisations$program_outcomes$net_outcome), col.names = F, row.names = F, 
                   paste0(collated_folder, 'program_outcomes.csv'), sep=',' )
      write.table( data.frame(collated_realisations$program_scale_impacts$program_total), col.names = F, row.names = F, 
                   paste0(collated_folder, 'program_impacts.csv'), sep=',' )
      write.table( data.frame(collated_realisations$landscape$net_landscape), col.names = F, row.names = F, 
                   paste0(collated_folder, 'landscape_outcomes.csv'), sep=',' )
      write.table( data.frame(collated_realisations$landscape$landscape_impact), col.names = F, row.names = F, 
                   paste0(collated_folder, 'landscape_impacts.csv'), sep=',' )
      
    }
    
  } 
}

plot_outputs <- function(output_params, feature_ind, collated_realisations, current_simulation_params){
  
  if (output_params$plot_type == 'impacts'){
    
    plot_impact_set(collated_realisations,
                    current_simulation_params,
                    output_params,
                    realisation_num = collated_realisations$realisation_num,
                    site_plot_lims = output_params$site_impact_plot_lims_set[[feature_ind]],
                    program_plot_lims = output_params$program_impact_plot_lims_set[[feature_ind]],
                    landscape_plot_lims = output_params$landscape_impact_plot_lims_set[[feature_ind]],
                    feature_ind,
                    output_params$sets_to_plot)
    
  } else if (output_params$plot_type == 'outcomes'){
    
    plot_outcome_set(collated_realisations,
                     current_simulation_params,
                     output_params,
                     realisation_num = collated_realisations$realisation_num,
                     site_plot_lims = output_params$site_outcome_plot_lims_set[[feature_ind]],
                     program_plot_lims = output_params$program_outcome_plot_lims_set[[feature_ind]],
                     landscape_plot_lims = output_params$landscape_outcome_plot_lims_set[[feature_ind]],
                     current_feature,
                     output_params$sets_to_plot)
  }
  
}

output_feature_layers <- function(feature_ind, current_data_dir, example_simulation_outputs, output_folder, use_offset_metric, save_output_raster, output_mov, mov_file_type, 
                                  current_element_indexes_grouped_by_feature_condition_class, time_steps, site_characteristics, scale_factor, mov_file_prefix){
  
  
  intervention_pool = lapply(seq_along(example_simulation_outputs$interventions), function(i) example_simulation_outputs$interventions[[i]]$site_indexes)
  
  if (output_mov == TRUE){
    graphics.off()
    # standard feature representation: 0-127 :black-green - 
    # offset representation: 128-255 :black-blue - 
    # development: 256 : red  
    # unregulated_loss: 257 :orange 
    black_green.palette <- colorRampPalette(c("black", "green"), space = "rgb")  
    black_blue.palette <- colorRampPalette(c("black", "blue"), space = "rgb")
    col_vec = c(black_green.palette(128), black_blue.palette(128), 'red', 'orange')
    mov_filename = paste0(mov_file_prefix, "%03d.", mov_file_type, sep = '')
    
    col_map_vector = c(128, 128, 256, 256, 257) #c(offset_col, offset_bank_col, dev_col, dev_credit_col, unregulated_loss_col)
    
    if (mov_file_type == 'png'){
      png(mov_filename, height = site_characteristics$landscape_dims[1], width = site_characteristics$landscape_dims[2])
    } else if (mov_file_type == 'jpg'){
      jpeg(mov_filename, height = site_characteristics$landscape_dims[1], width = site_characteristics$landscape_dims[2])
    }
  }

  for (yr in 0:time_steps){
    
    current_feature_layer = matrix(0, nrow = site_characteristics$landscape_dims[1], ncol = site_characteristics$landscape_dims[2])

    if (use_offset_metric == FALSE){
      
      feature_layer_to_use = readRDS(paste0(current_data_dir, 'feature_', formatC(feature_ind, width = 3, format = "d", flag = "0"), 
                                            '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
      feature_layer_to_use = lapply(seq_along(feature_layer_to_use), 
                                    function(i) lapply(seq_along(feature_layer_to_use[[i]]), function(j) as.matrix(feature_layer_to_use[[i]][[j]])))
      
      current_feature_layer[unlist(current_element_indexes_grouped_by_feature_condition_class)] = unlist(feature_layer_to_use)
      
    } else {

      feature_layer_to_use = readRDS(paste0(current_data_dir, 'metric_layer_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
      feature_layer_to_use = lapply(seq_along(feature_layer_to_use), function(i) as.matrix(feature_layer_to_use[[i]]))
      current_feature_layer[unlist(site_characteristics$land_parcels)] = unlist(feature_layer_to_use)
      
    }
    
    if (save_output_raster == TRUE){
      
      if (use_offset_metric == FALSE){
        raster_filename = paste0(output_folder, 'feature_', formatC(feature_ind, width = 3, format = "d", flag = "0"), 
                                 '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.tif')
      } else{
        raster_filename = paste0(output_folder, 'metric_layer_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.tif')
      }
      
      current_feature_raster = raster(current_feature_layer)

      writeRaster(current_feature_raster, raster_filename, overwrite = TRUE)
      
    }
    
    if (output_mov == TRUE){

      current_feature_layer = current_feature_layer * 127/scale_factor #map to color vector 0:127
      
      sets_to_use = lapply(seq_along(example_simulation_outputs$interventions), function(i) which(unlist(example_simulation_outputs$interventions[[i]]$intervention_yrs) <= yr))
      interventions_to_use = which(unlist(lapply(seq_along(sets_to_use), function(i) length(sets_to_use[[i]]) > 0)))
      
      if (length(interventions_to_use) > 0){
        
        sites_to_use = lapply(interventions_to_use, function(i) unlist(example_simulation_outputs$interventions[[i]]$site_indexes[sets_to_use[[i]]]))
        
        if (use_offset_metric == FALSE){
          inds_to_update = lapply(seq_along(interventions_to_use), function(i) unlist(current_element_indexes_grouped_by_feature_condition_class[sites_to_use[[i]]]))
        } else {
          inds_to_update = lapply(seq_along(interventions_to_use), function(i) unlist(site_characteristics$land_parcels[sites_to_use[[i]]]))
        }

        new_vals = lapply(seq_along(interventions_to_use), function(i) current_feature_layer[inds_to_update[[i]]] + col_map_vector[interventions_to_use[i]])

        current_feature_layer[unlist(inds_to_update)] = unlist(new_vals)
      }
      
      # rotate image with t(...) to align with tiff output

      image(t(apply(current_feature_layer, 2, rev)), zlim = c(0, 257), col = col_vec)
    }
    
  }
  if (output_mov == TRUE){
    dev.off()
  }
  
}



