#' Plots the results of the Offset Simulator run
#' @param user_output_params user configured plotting parameters to use
#' @param simulation_folder user configured simulation folder to use
#' @param run_number user defined folder
#' @param loglevel logging level to use, for instance futile.logger::INFO
#' @import futile.logger
#' @export


osim.output <- function(user_output_params = NULL, simulation_folder = NULL, output_type = NULL, loglevel = INFO){
  
  flog.threshold(loglevel)
  if (is.null(user_output_params)) {
    flog.error('provide plot params file')
    stop()
  } 
  
  object_to_output = list()
  object_to_output$output_params = overwrite_current_params(user_output_params, default_params = initialise_default_output_params())
  
  object_to_output$collated_folder = paste0(simulation_folder, '/collated_outputs/')  # LOCATION OF COLLATED FILES
  object_to_output$simulation_params_folder = paste0(simulation_folder, '/simulation_params/')
  object_to_output$simulation_output_folder = paste0(simulation_folder, '/simulation_outputs/')
  
  object_to_output$global_params = readRDS(paste0(object_to_output$simulation_params_folder, 'global_params.rds'))
  object_to_output$feature_params = readRDS(paste0(object_to_output$simulation_params_folder, 'feature_params.rds'))
  
  
  # read in file with stored param settings to identify plots easier
  param_variants_filename = paste0(object_to_output$simulation_params_folder, 'param_variants.rds')
  if (file.exists(param_variants_filename)){
    object_to_output$param_variants = readRDS(paste0(object_to_output$simulation_params_folder, 'param_variants.rds'))
  }
  
  # get the names of all parameter files, separated into run scenarios
  object_to_output$scenario_filenames <- list.files(path = object_to_output$simulation_params_folder, pattern = '_simulation_params.rds', all.files = FALSE,
                                                    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                                    include.dirs = FALSE, no.. = FALSE)
  
  if ( (class(object_to_output$output_params$scenario_vec) == 'character')){
    if (object_to_output$output_params$scenario_vec == 'all'){
      scenario_vec = 1:length(object_to_output$scenario_filenames)
    }
  } else {
    scenario_vec = object_to_output$output_params$scenario_vec
  }
  
  if (length(object_to_output$output_params$output_folder) == 0){
    object_to_output$output_folder = object_to_output$collated_folder
  } else {
    object_to_output$output_folder = object_to_output$output_params$output_folder
  }
  
  if (!file.exists(object_to_output$output_folder)){
    flog.info('creating output plot folder %s', object_to_output$output_folder)
    dir.create(object_to_output$output_folder)
  }
  
  if (object_to_output$output_params$output_type == 'plot'){
    # Set the output filename, and open the pdf file for reading
    
    if (object_to_output$output_params$plot_type == 'impacts'){
      pdf_to_output = paste0(object_to_output$output_folder, '/impacts.pdf')
    } else if (object_to_output$output_params$plot_type == 'outcomes'){
      pdf_to_output = paste0(object_to_output$output_folder, '/outcomes.pdf')
    }
    flog.info('writing PDF to %s', pdf_to_output)
    pdf(pdf_to_output, width = 8.3, height = 11.7)
    
    setup_sub_plots(object_to_output$output_params$nx, object_to_output$output_params$ny, x_space = 5, y_space = 5)
    
  } else if ((object_to_output$output_params$output_type == 'raster') || (object_to_output$output_params$output_type == 'png')){
    object_to_output$site_characteristics = readRDS(paste0(object_to_output$global_params$simulation_inputs_folder, 'site_characteristics.rds'))
    object_to_output$site_scale_condition_class_key = readRDS(paste0(object_to_output$global_params$simulation_inputs_folder, 'site_scale_condition_class_key.rds'))
    
    if (object_to_output$output_params$output_type == 'raster'){
      object_to_output$output_raster_folder = paste0(object_to_output$collated_folder, '/output_rasters/')
      if (!dir.exists(object_to_output$output_raster_folder)){
        dir.create(object_to_output$output_raster_folder)
      }
    } else if (object_to_output$output_params$output_type == 'png'){
      
      object_to_output$output_image_folder = paste0(object_to_output$collated_folder, '/output_image_layers/')
      if (!dir.exists(object_to_output$output_image_folder)){
        dir.create(object_to_output$output_image_folder)
      }
    } 
  }
  
  for (scenario_ind in scenario_vec){
    
    output_flag = check_output_flag(object_to_output$output_params, object_to_output$current_simulation_params)
    
    if (output_flag == FALSE){
      flog.trace(' skipping scenario %d', scenario_ind )
    } else {
      flog.info(rbind(names(object_to_output$param_variants[[scenario_ind]]), as.vector(object_to_output$param_variants[[scenario_ind]]))) 
      output_scenario(object_to_output,  scenario_ind)
    }
    
  }
  
  if (object_to_output$output_params$output_type == 'plot'){
    graphics.off()
    flog.info('closing PDF %s', pdf_to_output)
  }
  
  flog.info('all done')
}


check_output_flag <- function(output_params, current_simulation_params){
  param_inds_to_subset = match(output_params$plot_subset_type, names(current_simulation_params))
  
  if (any(!is.na(param_inds_to_subset)) & all(current_simulation_params[param_inds_to_subset] == output_params$plot_subset_param)) {
    output_flag = TRUE 
  } else {
    if (length(output_params$plot_subset_type) > 1){
      output_flag = FALSE
    } else {
      if (output_params$plot_subset_type == 'all'){
        output_flag = TRUE
      } else {
        output_flag = FALSE
      }
    }
  } 
  return(output_flag)
}


# find the current simulation folder - the function looks in the base_folder directory if supplied 
# and assumes the current working directory contains the simulation folder otherwise. If the user supplies the
# current run number the function looks for thaht specified folder and looks for the latest run otherwise.

#' @export
find_current_run_folder <- function(base_folder = NULL, run_number = NULL, numeric_placeholder_width = NULL){
  
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
  
  if (!is.null(numeric_placeholder_width)){
    simulation_folder = paste0(simulation_folder, formatC(current_run, width = numeric_placeholder_width, format = "d", flag = "0"), '/')
  } else {
    simulation_folder = paste0(simulation_folder, formatC(current_run, width = 3, format = "d", flag = "0"), '/')
  }
  
  if (!dir.exists(simulation_folder)){
    flog.error('simulation_folder %s does not exist.', simulation_folder)
    stop()
  } else{
    return(simulation_folder)
  }
}


output_scenario <- function(object_to_output, scenario_ind){
  
  flog.info('_________________________________')
  
  file_to_Read = paste0(object_to_output$simulation_params_folder, '/', object_to_output$scenario_filenames[scenario_ind])
  flog.trace('reading %s', file_to_Read)
  object_to_output$current_simulation_params = readRDS(file_to_Read)
  
  current_data_dir = paste0(object_to_output$simulation_output_folder, '/scenario_', 
                            formatC(scenario_ind, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                            '/realisation_', formatC(object_to_output$output_params$example_realisation_to_output, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"), '/') 
  
  if ((object_to_output$output_params$output_type == 'raster') || (object_to_output$output_params$output_type == 'png')){
    browser()
    object_to_output$example_simulation_outputs = readRDS(paste0(current_data_dir,'realisation_', 
                                                                 formatC(object_to_output$output_params$example_realisation_to_output, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
                                                                 '_outputs.rds'))
  } 
  
  if (class(object_to_output$output_params$features_to_output) == 'character'){
    if (object_to_output$output_params$features_to_output == 'all'){
      features_to_output = object_to_output$global_params$features_to_use_in_simulation
    } else {
      flog.error("features to plot parameter is poorly defined, assign vector of integers or 'all'")
    }
  }  else {
    features_to_output = object_to_output$output_params$features_to_output
  }
  
  if (sum(features_to_output) > 0){
    output_collated_features(object_to_output,
                             features_to_output, 
                             use_offset_metric = FALSE, 
                             scenario_ind, 
                             current_data_dir)
  }
  
  if (object_to_output$output_params$plot_offset_metric == TRUE){
    output_collated_features(object_to_output,
                             features_to_output = 1, 
                             use_offset_metric = TRUE, 
                             scenario_ind, 
                             current_data_dir)
  }
  
} 


output_collated_features <- function(object_to_output, features_to_output, use_offset_metric, scenario_ind, current_data_dir){
  
  for (feature_ind in features_to_output){
    
    if (use_offset_metric == TRUE){
      collated_filenames = paste0(object_to_output$collated_folder, list.files(path = object_to_output$collated_folder, all.files = FALSE,
                                                                               full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                                                               include.dirs = FALSE, no.. = FALSE, pattern = '_metric'))
      
    }  else {
      collated_filenames = find_collated_files(file_path = object_to_output$collated_folder,
                                               scenario_string = formatC(scenario_ind, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                                               feature_string = formatC(feature_ind, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                                               object_to_output$output_params$realisation_num)
    }
    
    collated_realisations = run_bind_collated_realisations_routines(collated_filenames)

    if (object_to_output$output_params$output_type == 'plot'){
      
      if (object_to_output$output_params$print_dev_offset_sites == TRUE){
        sites_used = collated_realisations$sites_used
        stats_to_use = which(unlist(lapply(seq_along(sites_used), function(i) length(unlist(sites_used[[i]]))>0)))
        mean_sites_used = lapply(stats_to_use, function (i) round(mean(unlist( sites_used[[i]] ))))
        flog.info(rbind(names(sites_used[stats_to_use]), mean_sites_used))
      }
      
      user_output_params <- initialise_user_output_params()
      plot_outputs(object_to_output$output_params, feature_ind, scenario_ind, collated_realisations, object_to_output$current_simulation_params, object_to_output$global_params)
      
    } else if (object_to_output$output_params$output_type == 'csv'){
      flog.info('writing csv outputs')
      
      write_output_block(unlist(collated_realisations$program_scale$outcomes, recursive = FALSE),
                         paste0(object_to_output$collated_folder, 'program_scale_outcomes.csv'))
      
      block_to_use = which(names(collated_realisations$program_scale$impacts) %in% c("net_offset_gains", "net_dev_losses", "net_impacts"))
      write_output_block(unlist(collated_realisations$program_scale$impacts[block_to_use], recursive = FALSE),
                         paste0(object_to_output$collated_folder, 'program_scale_impacts.csv'))
      write_output_block(unlist(collated_realisations$landscape_scale$outcomes, recursive = FALSE),
                         paste0(object_to_output$collated_folder, 'landscape_scale_outcomes.csv'))
      write_output_block(unlist(collated_realisations$landscape_scale$impacts, recursive = FALSE),
                         paste0(object_to_output$collated_folder, 'landscape_scale_impacts.csv'))
      
    } else {
      
      if (use_offset_metric == FALSE){
        
        flog.info(paste0('writing ', object_to_output$output_params$output_type, ' layer outputs for feature %s'), feature_ind)
        object_to_output$current_site_scale_condition_class_key = lapply(seq_along(object_to_output$site_scale_condition_class_key), 
                                                                                             function(i) object_to_output$site_scale_condition_class_key[[i]][[feature_ind]])
        
        if (object_to_output$output_params$output_type == 'raster'){
          file_prefix = paste0(object_to_output$output_raster_folder, 'feature_', formatC(feature_ind, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"), '_yr_') 
        } else if (object_to_output$output_params$output_type == 'png'){
          file_prefix = paste0(object_to_output$output_image_folder, 'feature_', formatC(feature_ind, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"), '_yr_')
        } 
        
        output_feature_layers(object_to_output, 
                              feature_ind, 
                              current_data_dir, 
                              file_prefix,
                              use_offset_metric = FALSE, 
                              scale_factor = max(unlist(object_to_output$feature_params$condition_class_bounds[[feature_ind]])))
        
      } else {
        
        flog.info('writing metric layer outputs')
        
        # get the largest value for each condition class
        vals_to_transform = lapply(seq_along(object_to_output$feature_params$condition_class_bounds), function(i) max(unlist(object_to_output$feature_params$condition_class_bounds[[i]])))
        #transform this set to user metric and use this to scale the colors
        scale_factor = user_transform_function(vals_to_transform, transform_params = object_to_output$current_simulation_params$transform_params)
        
        if (object_to_output$output_params$output_type == 'raster'){
          file_prefix = paste0(object_to_output$output_raster_folder, 'metric_yr_') 
        } else{
          file_prefix = paste0(object_to_output$output_image_folder, 'metric_yr_')
        }
        
        output_feature_layers(object_to_output, 
                              feature_ind, 
                              current_data_dir, 
                              file_prefix,
                              use_offset_metric = TRUE, 
                              scale_factor)
      } 
    } 
    
  } 
}

plot_outputs <- function(output_params, feature_ind, scenario_ind, collated_realisations, current_simulation_params, global_params){
  
  if (output_params$plot_type == 'impacts'){
    
    plot_impact_set(collated_realisations,
                    current_simulation_params,
                    global_params,
                    output_params,
                    realisation_num = collated_realisations$realisation_num,
                    site_plot_lims = output_params$site_impact_plot_lims_set[[scenario_ind]][[feature_ind]],
                    program_plot_lims = output_params$program_impact_plot_lims_set[[scenario_ind]][[feature_ind]],
                    landscape_plot_lims = output_params$landscape_impact_plot_lims_set[[scenario_ind]][[feature_ind]],
                    feature_ind,
                    output_params$sets_to_plot)
    
  } else if (output_params$plot_type == 'outcomes'){
    
    plot_outcome_set(collated_realisations,
                     current_simulation_params,
                     global_params,
                     output_params,
                     realisation_num = collated_realisations$realisation_num,
                     site_plot_lims = output_params$site_outcome_plot_lims_set[[scenario_ind]][[feature_ind]],
                     program_plot_lims = output_params$program_outcome_plot_lims_set[[scenario_ind]][[feature_ind]],
                     landscape_plot_lims = output_params$landscape_outcome_plot_lims_set[[scenario_ind]][[feature_ind]],
                     feature_ind,
                     output_params$sets_to_plot)
  }
  
}

write_output_block <- function(block_to_output, filename){
  
  #   block_to_output = setNames(lapply(seq_along(block_to_output), 
  #                            function(i) lapply(seq_along(block_to_output[[i]]), 
  #                                               function(j) append(block_to_output[[i]][[j]], block_to_output[[i]][[j]]))), 
  #                            names(block_to_output))
  
  block_lengths = lapply(seq_along(block_to_output), function(i) length(unlist(block_to_output[[i]])))
  sets_to_use = which(unlist(block_lengths) > 0)
  block_to_use = block_to_output[sets_to_use]
#  data_block = lapply(seq_along(block_to_use), 
#                      function(i) lapply(seq_along(block_to_use[[i]]), 
#                                         function(j) setNames(as.data.frame(block_to_use[[i]]), 
#                                                              paste0(names(block_to_use)[i], '_realisation_', seq(length(block_to_use[[i]][[j]]))))
#                                        )
#                      )
  data_block = lapply(seq_along(block_to_use), 
                      function(i) lapply(seq_along(block_to_use[[i]]), 
                                         function(j) setNames(as.data.frame(block_to_use[[i]]), 
                                                              names(block_to_use)[i])
                                        )
                      )  
  
  data_block = as.data.frame(data_block)
  write.table( data_block, col.names = T, row.names = F, filename, sep=',' )
}

output_feature_layers <- function(object_to_output, feature_ind, current_data_dir, file_prefix, use_offset_metric, scale_factor){
  
  intervention_pool = lapply(seq_along(object_to_output$example_simulation_outputs$interventions), function(i) object_to_output$example_simulation_outputs$interventions[[i]]$site_indexes)
  
  if (object_to_output$output_params$output_type == 'png'){
    
    graphics.off()
    
    image_filename = paste0(file_prefix, "%03d.", 'png', sep = '')
    png(image_filename, height = object_to_output$site_characteristics$landscape_dims[1], width = object_to_output$site_characteristics$landscape_dims[2])
  } else if (object_to_output$output_params$output_type == 'jpg'){
    jpeg(image_filename, height = object_to_output$site_characteristics$landscape_dims[1], width = object_to_output$site_characteristics$landscape_dims[2])
  }
  
  for (yr in 0:object_to_output$global_params$time_steps){
    
    flog.info(paste0('writing ', object_to_output$output_params$output_type, ' layer outputs for year %s'), yr)
    feature_layer_to_output = matrix(0, nrow = object_to_output$site_characteristics$landscape_dims[1], ncol = object_to_output$site_characteristics$landscape_dims[2])
    
    if (use_offset_metric == FALSE){
      
      feature_layer_to_use = readRDS(paste0(current_data_dir, 'feature_', formatC(feature_ind, width = 3, format = "d", flag = "0"), 
                                            '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
      
      feature_layer_to_use = lapply(seq_along(feature_layer_to_use), 
                                    function(i) lapply(seq_along(feature_layer_to_use[[i]]), function(j) as.matrix(feature_layer_to_use[[i]][[j]])))
      
      feature_layer_to_output[unlist(object_to_output$current_site_scale_condition_class_key)] = unlist(feature_layer_to_use)
      
    } else {
      
      browser()
      feature_layer_to_use = readRDS(paste0(current_data_dir, 'metric_layer_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
      feature_layer_to_use = lapply(seq_along(feature_layer_to_use), function(i) as.matrix(feature_layer_to_use[[i]]))
      feature_layer_to_output[unlist(object_to_output$site_characteristics$land_parcels)] = unlist(feature_layer_to_use)
      
    }
    
    if (object_to_output$output_params$map_vals == TRUE){
      
      feature_layer_to_output = feature_layer_to_output * (object_to_output$output_params$col_map_vector[1] - 1)/scale_factor #map to color vector 0:127
      
      sets_to_use = lapply(seq_along(object_to_output$example_simulation_outputs$interventions), 
                           function(i) which(unlist(object_to_output$example_simulation_outputs$interventions[[i]]$intervention_yrs) <= yr))
      
      # ignore empty intervention sets
      interventions_to_use = which(unlist(lapply(seq_along(sets_to_use), function(i) length(sets_to_use[[i]]) > 0)))
      
      if (length(interventions_to_use) > 0){
        
        sites_to_use = lapply(interventions_to_use, function(i) unlist(object_to_output$example_simulation_outputs$interventions[[i]]$site_indexes[sets_to_use[[i]]]))
        
        if (use_offset_metric == FALSE){
          #sort to original raster pixel indices
          inds_to_update = lapply(seq_along(interventions_to_use), function(i) unlist(object_to_output$current_site_scale_condition_class_key[sites_to_use[[i]]]))
        } else {
          #sorting not necessary as order is preserved for metric calculations
          inds_to_update = lapply(seq_along(interventions_to_use), function(i) unlist(object_to_output$site_characteristics$land_parcels[sites_to_use[[i]]]))
        }
        
        if (object_to_output$output_params$output_block_offsets == TRUE){

          # if setting offsets to block of color 
          # 1) identify offset sites through example_simulation_outputs$interventions - named as dev_object, offset_object, unregulted_loss_object etc.
          # i.e. use example_simulation_outputs$interventions$offset_object$site_indexes to identify offset sites
          
          sites_to_use = setNames(lapply(interventions_to_use, 
                                         function(i) unlist(object_to_output$example_simulation_outputs$interventions[[i]]$site_indexes[sets_to_use[[i]]])), 
                                  names(object_to_output$example_simulation_outputs$interventions[interventions_to_use])) 
          
          # get site ids for offsets via something like sites_to_use$offset_object
          offsets_to_map = which(names(sites_to_use) %in% c("offset_object", "uncoupled_offset_object"))
          if (length(offsets_to_map) > 0){
            feature_layer_to_output[unlist(object_to_output$site_characteristics$land_parcels[unlist(sites_to_use[offsets_to_map])])] = 127
          }
          
        }
        
        #mapping step to determine new mapped values 
        
        new_vals = lapply(seq_along(interventions_to_use), function(i) feature_layer_to_output[inds_to_update[[i]]] + object_to_output$output_params$col_map_vector[interventions_to_use[i]])
        feature_layer_to_output[unlist(inds_to_update)] = unlist(new_vals)
        
      }
    }
    
    if (object_to_output$output_params$output_type == 'raster'){
      raster_filename = paste0(file_prefix, formatC(yr, width = object_to_output$global_params$numeric_placeholder_width, format = "d", flag = "0"), '.tif')
      current_feature_raster = raster(feature_layer_to_output)
      writeRaster(current_feature_raster, raster_filename, overwrite = TRUE)
      
    } else if (object_to_output$output_params$output_type == 'png'){
      
      # rotate image with t(...) to align with tiff output
      feature_layer_to_output = t(apply(feature_layer_to_output, 2, rev))
      if (object_to_output$output_params$map_vals == TRUE){
        image(feature_layer_to_output, zlim = c(0, max(object_to_output$output_params$col_map_vector)), col = object_to_output$output_params$col_vec)
      } else {
        image(feature_layer_to_output)
      }
    }
    
  }
  
  if (object_to_output$output_params$output_type == 'png'){
    dev.off()
  }
  
}



plot_outcome_set <- function(collated_realisations, current_simulation_params, global_params, output_params, 
                             realisation_num, site_plot_lims, program_plot_lims, landscape_plot_lims, feature_ind,  set_to_plot){
  
  if (output_params$plot_site == TRUE){
    plot_site_outcomes(collated_realisations, 
                       output_params$plot_site_offset, 
                       output_params$plot_site_dev, 
                       output_params$output_type, 
                       current_simulation_params,
                       set_to_plot, 
                       site_plot_lims, 
                       feature_ind,  
                       realisation_ind = output_params$example_realisation_to_output,
                       output_params$site_outcome_lwd_vec,
                       global_params$time_steps)
    
  }
  
  
  if (output_params$plot_program == TRUE){
    
    overlay_realisations(plot_list = list(unlist(collated_realisations$program_scale$outcomes$net_outcome, recursive = FALSE), 
                                          unlist(collated_realisations$program_scale$outcomes$net_offsets, recursive = FALSE),
                                          unlist(collated_realisations$program_scale$outcomes$net_devs, recursive = FALSE)),
                         plot_title = 'Program Outcomes', 
                         x_lab = '',
                         collated_realisations$realisation_num,
                         output_params$program_outcome_lwd_vec, 
                         col_vec = output_params$program_col_vec, 
                         legend_loc = 'topleft',
                         legend_vec = c('net', 'offsets', 'developments'), 
                         plot_lims = program_plot_lims, 
                         global_params$time_steps)
    
    #     plot_outcomes(plot_list = list(unlist(collated_realisations$program_scale$outcomes$net_outcome, recursive = FALSE), 
    #                                    unlist(collated_realisations$program_scale$outcomes$net_offsets, recursive = FALSE),
    #                                    unlist(collated_realisations$program_scale$outcomes$net_devs, recursive = FALSE)),
    #                   plot_type = 'program', 
    #                   include_legend = FALSE, 
    #                   plot_lims = program_plot_lims,
    #                   plot_title = 'Program Outcome', 
    #                   loss_stats = collated_realisations$net_program_loss, 
    #                   collated_realisations$realisation_num, 
    #                   collated_realisations$program_scale_cfacs$program_cfac_sum,
    #                   output_params$program_outcome_lwd_vec, 
    #                   outcome_col = output_params$landscape_col, 
    #                   cfac_col = output_params$cfac_col,
    #                   legend_vec = c('Outcome', 'Counterfactual'), 
    #                   global_params$time_steps)
  }
  
  if (output_params$plot_landscape == TRUE){ 
    
    plot_outcomes(unlist(collated_realisations$landscape_scale$outcomes$net_outcome, recursive = FALSE), 
                  plot_type = 'landscape', 
                  include_legend = FALSE, 
                  plot_lims =landscape_plot_lims,
                  plot_title = 'Landscape Outcome', 
                  loss_stats = collated_realisations$landscape_loss, 
                  collated_realisations$realisation_num, 
                  collated_realisations$landscape_scale$net_landscape_cfac[[1]], 
                  output_params$landscape_outcome_lwd_vec, 
                  outcome_col = output_params$landscape_col, 
                  cfac_col = output_params$cfac_col,
                  legend_vec = c('Outcome', 'Counterfactual'), 
                  time_steps = global_params$time_steps)
    
  }
}


plot_site_outcomes <- function(collated_realisations, plot_site_offset_outcome, plot_site_dev_outcome, 
                               output_type, current_simulation_params, set_to_plot, plot_lims, feature_ind,realisation_ind, site_lwd, time_steps){
  
  if (current_simulation_params$use_uncoupled_offsets == TRUE){
    null_plot()
    return()
  }
  if ( set_to_plot > length(collated_realisations$intervention_pool$dev_object[[realisation_ind]])){
    stop ( paste('\nERROR: output_params$set_to_plot exceeds number of devs/offsets'))
  }
  
  y_lab = get_y_lab(output_type, current_simulation_params, feature_ind)
  
  if (length(plot_lims) == 0){
    site_group = unlist(c(unlist(collated_realisations$intervention_pool$dev_object[[realisation_ind]][set_to_plot]), 
                          collated_realisations$intervention_pool$offset_object[[realisation_ind]][set_to_plot]))
    plot_lims = find_plot_lims(plot_list = collated_realisations$site_scale$outcomes[[realisation_ind]][site_group])
  } 
  graphics::plot(NULL, type = 'l', ylab = y_lab, main = 'Site Outcomes', xlab = '',  ylim = plot_lims, xlim = c(0, time_steps))
  abline(h = 0, lty = 2)
  
  if (plot_site_dev_outcome == TRUE){
    site_indexes_to_use = collated_realisations$intervention_pool$dev_object[[realisation_ind]][[set_to_plot]]
    plot_list = collated_realisations$site_scale$outcomes[[realisation_ind]][site_indexes_to_use]
    overlay_plot_list(plot_list, col_vec = rep('red', length(plot_list)), lty_vec = rep(1, length(plot_list)), lwd_vec = rep(site_lwd, length(plot_list)))
  }
  
  if (plot_site_offset_outcome == TRUE){
    
    if (current_simulation_params$use_uncoupled_offsets == FALSE){
      site_indexes_to_use = collated_realisations$intervention_pool$offset_object[[realisation_ind]][[set_to_plot]]
      plot_list = collated_realisations$site_scale$outcomes[[realisation_ind]][site_indexes_to_use]
    } else {
      site_indexes_to_use = collated_realisations$intervention_pool$offset_object[[realisation_ind]]
      plot_list = list(Reduce('+', collated_realisations$site_scale$outcomes[[realisation_ind]][site_indexes_to_use]))
    }
    
    overlay_plot_list(plot_list, 
                      col_vec = rep('darkgreen', length(plot_list)), 
                      lty_vec = rep(1, length(plot_list)), 
                      lwd_vec = rep(site_lwd, length(plot_list)))
  }
  
}


plot_impact_set <- function(collated_realisations, current_simulation_params, global_params, output_params, realisation_num, 
                            site_plot_lims, program_plot_lims, landscape_plot_lims, current_feature, sets_to_plot){
  
  # Plot the site scale impacts
  if (output_params$plot_site == TRUE){
    
    overlay_site_impacts(collated_realisations,
                         output_params$plot_site_offset, 
                         output_params$plot_site_dev, 
                         output_params$plot_site_net, 
                         output_params$output_type,
                         current_simulation_params,
                         realisation_ind = output_params$example_realisation_to_output, 
                         current_feature, 
                         plot_from_impact_yr = FALSE, 
                         sets_to_plot,
                         plot_lims = site_plot_lims,
                         global_params$time_steps, 
                         output_params$site_impact_col_vec, 
                         output_params$site_impact_lwd)
  }
  
  # Plot the program scale impacts
  if (output_params$plot_program == TRUE){
    
    NNL_object <- find_NNL_characteristics(unlist(collated_realisations$program_scale$loss_characteristics$NNL, recursive = FALSE),
                                           unlist(collated_realisations$program_scale$impacts$net_impacts, recursive = FALSE))
    
    overlay_realisations(plot_list = list(unlist(collated_realisations$program_scale$impacts$net_offset_gains, recursive = FALSE), 
                                          unlist(collated_realisations$program_scale$impacts$net_dev_losses,recursive = FALSE), 
                                          unlist(collated_realisations$program_scale$impacts$net_impacts,recursive = FALSE)),
                         plot_title = 'Program Impact', 
                         x_lab = NNL_object$NNL_label,
                         collated_realisations$realisation_num,
                         output_params$program_lwd_vec, 
                         col_vec = output_params$program_col_vec, 
                         legend_loc = 'topleft',
                         legend_vec = c('Net Offset Impact', 'Net Development Impact', 'Net Impact'), 
                         plot_lims = program_plot_lims, 
                         global_params$time_steps)
    
    if (length(NNL_object$mean_NNL) >0){
      abline(v = NNL_object$mean_NNL, lty = 2)
    }
    
    ###### TODO intervention control has moved to index object - fix this #######
    if (length(unlist(collated_realisations$site_scale$impacts$dev_object)) > 0){
      flog.error('intervention control has moved to index object - fix this')
      last_dev_yr = mean(unlist(lapply(seq_along(collated_realisations$site_scale$impacts$dev_object), 
                                       function(i) tail(unlist(collated_realisations$site_scale$impacts$dev_object[[i]]$intervention_yrs), 1))))
      dev_end = tail(which(current_simulation_params$intervention_control > 0), 1)
      
      if (last_dev_yr < dev_end){
        line_to_use = last_dev_yr
        plot_col = 'red'
      } else {
        line_to_use = dev_end
        plot_col = 'black'
      }
      abline(v = line_to_use, lty = 3, col = plot_col)
    }
    
  }
  
  # Plot the landscape scale impacts
  if (output_params$plot_landscape == TRUE){
    
    NNL_object <- find_NNL_characteristics(unlist(collated_realisations$landscape_scale$loss_characteristics$NNL, recursive = FALSE),
                                           unlist(collated_realisations$landscape_scale$impacts$net_impact, recursive = FALSE))
    
    overlay_realisations(plot_list = list(unlist(collated_realisations$landscape_scale$impacts$net_impact, recursive = FALSE)),
                         plot_title = 'Landscape Impact', 
                         x_lab = NNL_object$NNL_label,
                         collated_realisations$realisation_num,
                         output_params$landscape_lwd_vec, 
                         output_params$landscape_col,
                         legend_loc = 'topright',
                         legend_vec = 'NA', 
                         landscape_plot_lims, 
                         global_params$time_steps) 
  }
}






NNL_test <- function(NNL_set, collated_impacts){
  
  if (length(NNL_set) == 0){
    return(NULL)
  } else {
    inds_to_use = which(unlist(lapply(seq_along(NNL_set), function(i) length(NNL_set[[i]]) > 0)))
    
    NNL_to_use = NNL_set[inds_to_use]
    last_vals = lapply(inds_to_use, function(i) collated_impacts[[i]][ length(collated_impacts[[i]]) ])
    inds_to_reject = which(unlist(last_vals) < 0)
    if (length(inds_to_reject) > 0){
      NNL_to_use = NNL_to_use[-inds_to_reject]
    }
  }
  return(NNL_to_use)
}



find_NNL_characteristics <- function(NNL_set, collated_impacts){
  
  ######### test routines for when we have an increase rather than decrease 
  ######### also when impact is exactly zero at initialisation and positive thereafter
  NNL_object = list()
  
  if (length(unlist(NNL_set)) == 0){
    NNL_object$NNL_label = 'All realisations failed NNL'
    return(NNL_object)
  } else {
    NNL_object$mean_NNL = round(find_list_mean(NNL_set))
    mean_collated_impact <- find_list_mean(collated_impacts)
    final_collated_impact <- tail(mean_collated_impact, n=1)
    
    NNL_object$NNL_label <- cbind(paste0(length(unlist(NNL_set)), ' realisations achieved NNL at ', NNL_object$mean_NNL, ' years'), 
                                  paste0('mean final impact = ', format(final_collated_impact, scientific=TRUE, digits=3)))
    
  } 
  
  return(NNL_object)
}



get_y_lab <- function(output_type, current_simulation_params, feature_ind){
  y_lab = paste('Feature', feature_ind)
  if (feature_ind %in% current_simulation_params$features_to_use_in_offset_calc){
    ylab = paste0(y_lab, 'T') 
  } 
  
  if (feature_ind %in% current_simulation_params$features_to_use_in_offset_intervention){
    ylab = paste0(y_lab, 'O') 
  } 
  
  ylab = paste0(y_lab, '\n', current_simulation_params$offset_calc_type, '/', current_simulation_params$dev_calc_type )
  
  #   if (current_simulation_params$use_uncoupled_offsets == FALSE){
  #     y_lab = cbind(y_lab, paste0('T.H.', current_simulation_params$offset_time_horizon, ', ill_clear ', current_simulation_params$include_unregulated_loss_in_offset_calc))
  #   } else{
  #     y_lab = cbind(y_lab, paste0(' uncoupled_offset T, Clearing ', current_simulation_params$include_unregulated_loss_in_offset_calc))
  #   }
  #  y_lab = t(y_lab)
  return(y_lab)
}


overlay_site_impacts <- function(collated_realisations, plot_site_offset_impact, plot_site_dev_impact, plot_site_net_impact, output_type, current_simulation_params, realisation_ind, 
                                 feature_ind, plot_from_impact_yr, sets_to_plot, plot_lims, time_steps, col_vec, plot_lwd){
  
  y_lab = get_y_lab(output_type, current_simulation_params, feature_ind)
  plot_lwd = 1
  
  stats_to_use = unlist(collated_realisations$sites_used)
  x_lab = ''
  if (current_simulation_params$use_uncoupled_offsets == FALSE){
    offset_set = collated_realisations$site_scale$impacts$offset_object
    dev_set = collated_realisations$site_scale$impacts$dev_object
    net_plot_list = collated_realisations$site_scale$net_impacts$net_impacts[[realisation_ind]][sets_to_plot]
    
  } else {
    offset_set = unlist(collated_realisations$program_scale$impacts$net_offset_gains, recursive = FALSE)
    dev_set = unlist(collated_realisations$program_scale$impacts$net_dev_losses, recursive = FALSE)
    net_plot_list = unlist(collated_realisations$program_scale$impacts$net_impacts, recursive = FALSE)
  }
  
  if (length(plot_lims) == 0){
    plot_lims = find_plot_lims(plot_list = list(offset_set, dev_set))
  } 
  
  graphics::plot(NULL, type = 'l', ylab = '', main = 'Site Impact', xlab = '',  ylim = plot_lims, xlim = c(0, time_steps))
  abline(h = 0, lty = 2)
  
  plot_type = 'non-overlay'
  
  for (plot_ind in seq_along(sets_to_plot)){
    current_set_to_plot = sets_to_plot[plot_ind]
    # Plot the impact of the offset site(s) 
    if (plot_site_offset_impact == TRUE){
      
      overlay_impact(collated_object = offset_set,
                     current_simulation_params$use_uncoupled_offsets,
                     visualisation_type = 'stacked', 
                     realisation_ind, 
                     plot_col = col_vec[1], 
                     plot_lwd,
                     plot_type,
                     y_lab,
                     x_lab,
                     plot_from_impact_yr,
                     current_set_to_plot, 
                     site_plot_lims, 
                     time_steps)
      
      plot_type = 'overlay'
    }
    
    
    # Overlay the impact of the development site 
    
    if (plot_site_dev_impact == TRUE){
      
      overlay_impact(dev_set,
                     current_simulation_params$use_uncoupled_offsets,
                     visualisation_type = 'non-stacked', 
                     realisation_ind, 
                     plot_col = col_vec[2],
                     plot_lwd,
                     plot_type,
                     y_lab = '',
                     x_lab,
                     plot_from_impact_yr,
                     current_set_to_plot, 
                     site_plot_lims, 
                     time_steps)
    }
    
    # Overlay the net impact of the offset and development impact 
    
    if (plot_site_net_impact == TRUE){
      overlay_plot_list(net_plot_list[plot_ind], 
                        col_vec = rep(col_vec[3], length(net_plot_list)), 
                        lty_vec = rep(1, length(net_plot_list)), 
                        lwd_vec = rep(plot_lwd, length(net_plot_list)))
      
      #       overlay_plot_list(plot_type, net_plot_list[plot_ind], yticks = 'y', ylims = site_plot_lims, heading = 'Site Outcomes', ylab = '', x_lab = '', 
      #                         col_vec = rep(col_vec[3], length(net_plot_list)), lty_vec = rep(1, length(net_plot_list)), lwd_vec = rep(plot_lwd, length(net_plot_list)), 
      #                         legend_vec = 'NA', legend_loc = FALSE)
    }
    
    plot_type = 'non-overlay'
  }
}


overlay_impact <- function(collated_object, use_uncoupled_offsets, visualisation_type, realisation_ind, 
                           plot_col, plot_lwd, plot_type, y_lab, x_lab, plot_from_impact_yr, 
                           set_to_plot, plot_lims, time_steps){
  
  if (use_uncoupled_offsets == FALSE){
    collated_traj_set = collated_object[[realisation_ind]]$nets
    site_indexes = unlist(collated_object[[realisation_ind]]$site_indexes[set_to_plot])
    inds_to_plot = which(unlist(collated_object[[realisation_ind]]$site_indexes) %in% site_indexes)
    if (plot_from_impact_yr){
      intervention_yrs = collated_object[[realisation_ind]]$intervention_yrs[inds_to_plot]
    } else {
      intervention_yrs = rep(list(1), length(inds_to_plot))
    }
    plot_list = lapply(seq_along(inds_to_plot), function(i) collated_traj_set[[inds_to_plot[i]]][intervention_yrs[[i]]:time_steps])
    if (visualisation_type == 'stacked'){
      plot_list = lapply(seq_along(plot_list), function(i) Reduce('+', plot_list[1:i]))
    }
  } else {
    plot_list = list(collated_object[[realisation_ind]])
  }
  
  overlay_plot_list(plot_list, 
                    col_vec = rep(plot_col, length(plot_list)), 
                    lty_vec = rep(1, length(plot_list)), 
                    lwd_vec = rep(plot_lwd, length(plot_list)))
  
  #   overlay_plot_list(plot_type, plot_list, yticks = 'y', ylims = plot_lims, heading = 'Site Impact', y_lab, x_lab, 
  #                     col_vec = rep(plot_col, length(plot_list)), lty_vec = rep(1, length(plot_list)), lwd_vec = rep(plot_lwd, length(plot_list)), 
  #                     legend_vec = 'NA', legend_loc = FALSE)
}



find_list_mean <- function(list_to_average){
  
  
  if (is.null(list_to_average)){
    return(NULL)
  } else {
    
    set_lengths = lapply(list_to_average, 'length')
    set_to_use = which(unlist(set_lengths) > 0) 
    if (length(set_to_use) > 0){
      list_to_average = list_to_average[set_to_use]
      list_mean = Reduce('+', list_to_average)/length(list_to_average)
      return(list_mean)
    } else {
      return(vector('list', 1))
    }
  }
}

plot_collated_realisation_set <- function(plot_list, plot_col, realisation_num, lwd_vec, time_steps){
  
  if (plot_col == 'blue'){
    back_plot_col = 'skyblue'
  } else if (plot_col == 'black'){
    back_plot_col = 'gray40'
  } else if (plot_col == 'red'){
    back_plot_col = 'darkorange'
  } else if (plot_col == 'mediumorchid4'){
    back_plot_col = 'mediumorchid1'
  } else if (plot_col == 'darkgreen'){
    back_plot_col = 'green'
  }
  
  for (realisation_ind in seq_len(realisation_num)){
    if (length(plot_list[[realisation_ind]]) > 0){
      lines(plot_list[[realisation_ind]], lwd = lwd_vec[2], col = back_plot_col)
    }
  }
  
  current_plot_mean = find_list_mean(plot_list)
  
  if (length(current_plot_mean) > 0){
    lines(current_plot_mean, col = plot_col, lwd = lwd_vec[1], lty = 2)
  } 
  
  
}


plot_NNL_hist <- function(NNL_plot_object, plot_tit, x_lim, feature_ind){
  
  if (NNL_plot_object$NNL_success > 0){
    x_lab = t(cbind( paste0('Mean NNL at  ', round(NNL_plot_object$NNL_mean), 'years'), paste0('NNL success = ', round(NNL_plot_object$NNL_success*100), '%' )))
    NNL_yrs = unlist(NNL_plot_object$NNL_yrs)
    NNL_yrs <- NNL_yrs[which(NNL_yrs > 0)]
    hist(NNL_yrs, main = plot_tit, xlab = x_lab, xlim = x_lim, breaks=seq(min(NNL_yrs),max(NNL_yrs),by=1))
    
  } else {
    null_plot()
  }
  
}



null_plot <- function(){
  graphics::plot(NULL, type= 'n', xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
}

plot_parcel_sums_hist <- function(parcel_sums_at_offset, feature_ind, parcel_sum_lims){
  current_parcel_sums_at_offset = unlist(parcel_sums_at_offset, recursive = FALSE)
  parcel_sums_at_offset_array = unlist(lapply(seq_along(current_parcel_sums_at_offset), function(i) current_parcel_sums_at_offset[[i]]))
  hist(parcel_sums_at_offset_array, main = 'offset parcel values', xlab = 'selected offset parcel values', xlim = parcel_sum_lims)
}

plot_NNL_hists <- function(parcel_set_NNL, program_scale_NNL, system_NNL, use_parcel_sets, feature_ind){
  
  if (use_parcel_sets == TRUE){
    plot_NNL_hist(parcel_set_NNL, plot_tit = 'Site scale NNL Assessment', x_lim = c(0, 100), feature_ind) 
  } else {
    null_plot()
  }
  
  plot_NNL_hist(program_scale_NNL, plot_tit = 'Program scale NNL Assessment', x_lim = c(0, 100), feature_ind)
  plot_NNL_hist(system_NNL, plot_tit = 'Landscape scale NNL Assessment', x_lim = c(0, 100), feature_ind)
  
}


overlay_realisations <- function(plot_list, plot_title, x_lab, realisation_num, lwd_vec, 
                                 col_vec, legend_vec, legend_loc, plot_lims, time_steps){
  
  inds_to_use = lapply(seq_along(plot_list), function(i) !is.null(unlist(plot_list[[i]])))
  plot_list <- plot_list[unlist(inds_to_use)]
  col_vec <- col_vec[unlist(inds_to_use)]
  legend_vec <- legend_vec[unlist(inds_to_use)]
  
  if (length(plot_list) == 0){
    return()
  }
  
  if (length(plot_lims) == 0){
    plot_lims = find_plot_lims(plot_list)
  } 
  
  graphics::plot(NULL, type = 'l', ylab = '', main = plot_title, xlab = x_lab,  ylim = plot_lims, xlim = c(0, time_steps))
  
  abline(h = 0, lty = 2)
  
  for (plot_ind in seq_along(plot_list)){
    plot_collated_realisation_set(plot_list[[plot_ind]], plot_col = col_vec[plot_ind], 
                                  realisation_num, lwd_vec,  time_steps)
  }
  
  if (legend_vec[1] != 'NA'){
    legend(legend_loc, legend_vec, bty="n", lty = c(2, 2, 2, 2), lwd = array(lwd_vec[1], 4), col = col_vec)
  }
  
}



plot_outcomes <- function(current_outcome_set, plot_type, include_legend, plot_lims, plot_title, 
                          loss_stats, realisation_num,  cfacs, lwd_vec, outcome_col, cfac_col, legend_vec, time_steps){
  
  if (length(unlist(current_outcome_set)) == 0){
    return()
  }
  
  current_total_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_stats$total_loss[[i]]))
  if (length(current_total_loss) > 0){
    loss_tit = paste0('Net Loss at ', time_steps, 'yrs = ', round(mean(unlist(current_total_loss))*100), '%')
    current_NNL_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_stats$NNL_loss[[i]]))
    if (length(current_NNL_loss) > 0){
      NNL_tit = paste0('Mean NNL at  ', round(mean(current_NNL_loss*100)), '% landscape loss')
    } else {
      NNL_tit = 'All realisations failed NNL'
    }
  } else {
    loss_tit = ''
    NNL_tit = ''
  }
  
  sub_tit = cbind(NNL_tit, loss_tit)
  
  if (length(plot_lims) == 0){
    plot_lims = find_plot_lims(current_outcome_set)
  } 
  
  graphics::plot(NULL, type = 'l', ylab = '', main = plot_title, xlab = sub_tit,  ylim = plot_lims, xlim = c(0, time_steps))
  abline(h = 0, lty = 2)
  
  plot_collated_realisation_set(current_outcome_set, plot_col = outcome_col, realisation_num, lwd_vec, time_steps)
  
  if (plot_type == 'program'){
    plot_collated_realisation_set(cfacs, plot_col = cfac_col, realisation_num, lwd_vec,  time_steps)
  } else {
    lines(cfacs, col = cfac_col, lty = 2, lwd = 2)
  }
  
  if (include_legend == TRUE){
    legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  }
  
}


setup_sub_plots <- function(nx, ny, x_space, y_space){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(x_space, y_space, 1, 0), oma = c(2, 4, 2.5, 0.5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.3, 0))
  
}




find_plot_lims <- function(plot_list){
  if (length(unlist(plot_list)) > 0){
    mn = min(unlist(plot_list))
    mx = max(unlist(plot_list))
  } else {
    mn = 0
    mx = 1
  }
  plot_lims = c(mn, mx)
  return(plot_lims)
}



overlay_plot_list <- function(plot_list, col_vec, lty_vec, lwd_vec){
  
  for (plot_ind in seq_along(plot_list)){
    if (length(plot_list[[plot_ind]]) > 0){
      lines(plot_list[[plot_ind]],  col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
    }
  }
  
  #   if (length(plot_list) == 0){
  #     null_plot()
  #   } else {
  #     if (plot_type == 'non-overlay'){
  #       graphics::plot(plot_list[[1]], type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  #     } else {
  #       lines(plot_list[[1]], type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  #     }
  #     
  #     if (length(plot_list) > 1){
  #       for (plot_ind in 2:length(plot_list)){
  #         lines(plot_list[[plot_ind]],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
  #       }
  #     }
  #     abline(h = 0, lty = 2)
  #     if (legend_vec[1] != 'NA'){
  #       legend(legend_loc, legend_vec, bty="n", lty = lty_vec, cex = 1,  pt.cex = 1, lwd = lwd_vec, col = col_vec)
  #     }
  #   }
}

# write_site_mask <- function(output_filename, landscape_dims, land_parcels, current_site_indexes){ 
#   
#   site_mask = array(0, landscape_dims)
#   site_mask[ unlist(land_parcels[unlist(current_site_indexes)])] = 1
#   # rgb.palette <- colorRampPalette(color_vec, space = "rgb")
#   png(filename = output_filename, height = dim(site_mask)[1], width = dim(site_mask)[2])
#   image(site_mask, zlim = c(0,1), col = rgb.palette(512))
#   dev.off()
#   return(site_mask)
# }

# check_plot_options <- function(output_params, current_simulation_params, scenario_filenames) {
#   
#   if(output_params$plot_type != 'impacts' & output_params$plot_type != 'outcomes'){
#     stop( paste0('\nERROR: Illegal plot option specified. Variable plot_type is set to ', output_params$plot_type) )
#   }
#   
#   if (sum(current_simulation_params$intervention_control) < max(output_params$sets_to_plot)){
#     stop (paste('chosen example set to plot needs to be less than ', sum(current_simulation_params$intervention_control)))
#   }
#   
#   if (output_params$output_type == 'scenarios'){
#     if (length(scenario_filenames) == 0){
#       stop( paste('\nERROR: No files that match _simulation_params found in', output_params$simulation_params_folder) )
#     } else if (length(scenario_filenames) < max(output_params$plot_vec)){
#       stop ( paste('\nERROR: only ', length(scenario_filenames), ' scenario params files found, output_params$plot_vec parameter does not match'))
#     }
#   } else if (output_params$output_type == 'features'){
#     if (global_params$feature_num < max(output_params$plot_vec)){
#       stop ( paste('\nERROR: output_params$plot_vec exceeds number of features (', global_params$feature_num, ')'))
#     }
#     
#   } else if (output_params$output_type == 'multiple_sets'){
#     if ( max(output_params$sets_to_plot) > sum(current_simulation_params$intervention_control)){
#       stop ( paste('\nERROR: output_params$sets_to_plot exceeds number of developments (', sum(current_simulation_params$intervention_control), ')'))
#     }
#   }
#   
# }
# 
# 
# 
