#' Runs the Offset Simulator
#' @param user_global_params user configured parameters to use
#' @param user_simulation_params user configured parameters to use
#' @param user_simulated_ecology_params user configured parameters to use
#' @param loglevel logging level to use, for instance futile.logger::WARN
#' @import doParallel
#' @import doRNG
#' @import foreach
#' @import futile.logger
#' @import raster
#' @import truncnorm
#' @import Matrix
#' @export 
#' 
osim.run <- function(user_global_params = NULL, user_simulation_params = NULL, user_feature_params = NULL, loglevel = WARN){
  
  flog.threshold(loglevel)
  flog.info('starting offsetsim')
  
  params_object <- build_params(user_global_params, user_feature_params, user_simulation_params)
  
  # Undertake all the run intialization proceedures, including generating
  # simulated data if required or reading in previously generated input data
  
  # Write initial logging info
  flog.info('Running %s scenarios with %s realisations on %s cores', 
            length(params_object$simulation_params_group),  
            params_object$global_params$realisation_num,
            params_object$global_params$number_of_cores ) 
  
  # Loop over all defined scenarios if only a subset of the scenarios is to be
  # reun (as defined by global_params$scenario_subset) then only
  # run these. By default global_params$scenario_subset
  
  for (scenario_ind in params_object$global_params$scenario_subset){
    
    # Store the start time
    loop_strt <- Sys.time()
    
    #load objects from files etc
    simulation_data_object <- build_input_data(params_object$global_params, params_object$feature_params, params_object$simulation_params_group[[scenario_ind]])
    
    simulation_data_object$output_data = build_output_data(simulation_data_object)
    flog.info('running scenario %s of %s, offsetting using %s impact calculation with %s management regime and %s year time horizon for gain/loss calculations',  
              scenario_ind, 
              length(params_object$simulation_params_group),
              simulation_data_object$simulation_params$offset_calc_type, 
              simulation_data_object$simulation_params$offset_action_type, 
              simulation_data_object$simulation_params$offset_time_horizon)
    
    flog.info('system composed of %s x %s elements and %s sites',
              simulation_data_object$site_characteristics$landscape_dims[1], 
              simulation_data_object$site_characteristics$landscape_dims[2],
              length(simulation_data_object$site_characteristics$land_parcels))
    flog.info('%s potential development sites, %s potential offset sites',
              length(simulation_data_object$output_data$index_object$available_indexes$devs), 
              length(simulation_data_object$output_data$index_object$available_indexes$offsets)) 
    
    flog.info('developing (and offsetting) %s sites', sum(simulation_data_object$simulation_params$intervention_vec)) 
    
    flog.info('estimated %s sites lost to unregulated clearing', 
              round(length(simulation_data_object$site_characteristics$land_parcels) -
                      length(simulation_data_object$site_characteristics$land_parcels)*(1 - simulation_data_object$simulation_params$unregulated_loss_prob)^simulation_data_object$simulation_params$time_steps))
    
    # Work out if more than one core is specified, and if so, run the
    # simulation in parallel using the doParallel, foreach and doRNG packages
    
    
    
    # TODO(Isaac) test whether can set check and set seed if necessary much
    # earlier in the code before run_initialise_routines() is called
    if (simulation_data_object$global_params$number_of_cores > 1 && simulation_data_object$global_params$set_seed == TRUE){
      # case when running DETERMINISTIC realisations in parallel
      # doRNG needed to get deterministic foreach loops, dsingh 24/nov/17
      flog.info('will use doRNG with seed %s to get determinisitc parallel runs', 123)
      registerDoRNG(123) 
      
      # foreach runs realizations in parallel
      foreach(realisation_ind = seq_len(simulation_data_object$global_params$realisation_num)) %dorng%{
        
        run_offset_simulation_routines(simulation_data_object, 
                                       scenario_ind,
                                       realisation_ind)
        
      }
    } else if ((simulation_data_object$global_params$number_of_cores > 1) && (simulation_data_object$global_params$realisation_num > 1)){
      # case when running NON-DETERMINISTIC realisations in parallel
      foreach(realisation_ind = seq_len(simulation_data_object$global_params$realisation_num)) %dopar%{
        
        run_offset_simulation_routines(simulation_data_object, 
                                       scenario_ind,
                                       realisation_ind)
        
      }
    } else {
      # Case when running single realisation
      # TODO(Isaac): need to add case for running a single realization either with or without having the seed set.
      for (realisation_ind in 1:simulation_data_object$global_params$realisation_num){
        
        run_offset_simulation_routines(simulation_data_object,
                                       scenario_ind,
                                       realisation_ind)
      }
      
    }
    
    flog.info('scenario %s done in %s %s', 
              scenario_ind,
              round(difftime(Sys.time(), loop_strt), 1), 
              units(difftime(Sys.time(), loop_strt)))
  }
  
  flog.info('running collate routines')
  
  if ((simulation_data_object$global_params$overwrite_feature_dynamics == TRUE) |
      !file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_cfacs.rds'))){
      flog.info('building background counterfactuals - this may take a while')
      background_cfacs_object = build_background_cfacs(simulation_data_object)
      flog.info('saving background counterfactuals')
      saveRDS(background_cfacs_object, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_cfacs.rds'))
  } else {
    flog.info('loading background counterfactuals from file')
    background_cfacs_object = readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_cfacs.rds'))
  }
    
  if ((simulation_data_object$global_params$number_of_cores > 1) && (simulation_data_object$global_params$realisation_num > 1)){
    # case when running NON-DETERMINISTIC realisations in parallel
    foreach(realisation_ind = seq_len(simulation_data_object$global_params$realisation_num)) %dopar%{
      collate_simulation_outputs(simulation_data_object, background_cfacs_object, scenario_ind, realisation_ind)
    }
  } else {
    # Case when running single realisation
    # TODO(Isaac): need to add case for running a single realization either with or without having the seed set.
    for (realisation_ind in 1:simulation_data_object$global_params$realisation_num){
      collate_simulation_outputs(simulation_data_object, background_cfacs_object, scenario_ind, realisation_ind)
    }
    
  }
  
  flog.info('scenario %s done in %s %s', 
            scenario_ind,
            round(difftime(Sys.time(), loop_strt), 1), 
            units(difftime(Sys.time(), loop_strt)))
  
  if (simulation_data_object$global_params$save_simulation_outputs == FALSE){
    # This deletes all folders and subfolders that were created in the run
    # process that are no longer needed
    unlink(simulation_data_object$global_params$output_folder, recursive = TRUE)
  }
  
  flog.info('all scenarios done in %s %s', 
            round(difftime(Sys.time(), simulation_data_object$global_params$strt), 1), 
            units(difftime(Sys.time(), simulation_data_object$global_params$strt)))
  
  flog.info('all outputs written into %s', simulation_data_object$global_params$run_folder)
  
  # Clean up the parallel processes if there was more than one
  parallel::stopCluster(simulation_data_object$global_params$clstr)
}


run_offset_simulation_routines <- function(simulation_data_object, scenario_ind, realisation_ind){  
  
  # run simulation with identical realisation instantiation
  
  current_data_dir = write_folder(paste0(simulation_data_object$global_params$output_folder, 
                                         'scenario_', formatC(scenario_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
                                         '/realisation_', formatC(realisation_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), '/'))
  
  save_landscape_routine(simulation_data_object, current_data_dir, yr = 0)
  
  flog.info('current data dir is %s', current_data_dir)
  
  simulation_outputs <- run_simulation(simulation_data_object, current_data_dir)
  
  # save raw simulation data

    saveRDS(simulation_outputs, paste0(current_data_dir, 'realisation_',
                                       formatC(realisation_ind, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                                       '_outputs.rds'))

  
  # delete current temporary files and folder
  if (simulation_data_object$global_params$save_simulation_outputs == FALSE){
    unlink(current_data_dir, recursive = TRUE)
  }
  
}

build_background_cfacs <- function(simulation_data_object){
  
  background_cfacs_object = list()
  background_projection_yrs_pool = lapply(seq_along(simulation_data_object$site_features), 
                                          function(i) lapply(seq(simulation_data_object$simulation_params$feature_num), 
                                                             function(j) rep(list(1), length(simulation_data_object$feature_dynamics_modes[[i]][[j]])) ))  
  
  background_cfacs_object$background_cfacs = collate_cfacs(simulation_data_object$site_features,
                                                           simulation_data_object$simulation_params, 
                                                           simulation_data_object$feature_params,
                                                           simulation_data_object$eature_dynamics,
                                                           simulation_data_object$feature_dynamics_modes,
                                                           simulation_data_object$site_element_index_key,
                                                           background_projection_yrs_pool,
                                                           intervention_yrs = rep(1, length(initial_feature_layer)),
                                                           vector(),
                                                           cfac_type = 'background',
                                                           object_type = vector(), 
                                                           use_cfac_type_in_sim = FALSE, 
                                                           condition_class_bounds = simulation_data_object$feature_params$condition_class_bounds, 
                                                           use_offset_metric = FALSE)
  
  saveRDS(background_cfacs_object$background_cfacs, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_cfacs.rds'))
  
  if (simulation_data_object$simulation_params$use_offset_metric == TRUE){
    
    background_cfacs_object$user_metric_background_cfacs = collate_cfacs(simulation_data_object$site_features,
                                                                         simulation_data_object$simulation_params, 
                                                                         simulation_data_object$feature_params,
                                                                         simulation_data_object$feature_dynamics,
                                                                         simulation_data_object$feature_dynamics_modes,
                                                                         simulation_data_object$site_element_index_key,
                                                                         background_projection_yrs_pool,
                                                                         intervention_yrs = rep(1, length(initial_feature_layer)),
                                                                         vector(),
                                                                         cfac_type = 'background',
                                                                         object_type = vector(), 
                                                                         use_cfac_type_in_sim = FALSE, 
                                                                         condition_class_bounds = simulation_data_object$feature_params$condition_class_bounds, 
                                                                         use_offset_metric = TRUE)
    saveRDS(background_cfacs_object$background_cfacs, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'user_metric_background_cfacs.rds'))
    
  }
  
  return(background_cfacs_object)
}


# main engine for code - returns all simulation outputs including developments, offsets etc.
run_simulation <- function(simulation_data_object, current_data_dir){
  
  #run through main time loop
  for (yr in seq_len(simulation_data_object$simulation_params$time_steps)){
    
    flog.info('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    flog.info('t = %s', yr) 
    flog.info('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    
    #when running in offset banking select out current set of sites to add to bank
    
    if (simulation_data_object$simulation_params$use_offset_bank == TRUE){
      simulation_data_object <- banking_routine(simulation_data_object, yr)
    }
    
    
    if (simulation_data_object$simulation_params$intervention_vec[yr] > 0){
      
      flog.info('Projecting sites')
      
      # determine current set of available offset sites and calculate gains structure as detailed in current policy params
      simulation_data_object$offset_pool_object <- build_intervention_pool(simulation_data_object, 
                                                                           pool_type = 'offsets',
                                                                           current_pool = simulation_data_object$output_data$index_object$available_indexes$offsets,
                                                                           yr)
      
      # determine current set of available development sites and calculate gains structure as detailed in current policy params

      if (simulation_data_object$simulation_params$development_selection_type == 'directed') {
        simulation_data_object$dev_pool_object <- build_intervention_pool(simulation_data_object, 
                                                                          pool_type = 'developments',
                                                                          current_pool = setdiff(simulation_data_object$simulation_params$directed_developments[[yr]], 
                                                                                                 simulation_data_object$output_data$index_object$site_indexes_used),
                                                                          yr)
      } else {
        simulation_data_object$dev_pool_object <- build_intervention_pool(simulation_data_object, 
                                                                        pool_type = 'developments',
                                                                        current_pool = simulation_data_object$output_data$index_object$available_indexes$devs,
                                                                        yr)
      }
    }
    
    
    for (current_dev_index in seq_len(simulation_data_object$simulation_params$intervention_vec[yr])){
      
      ###### TODO REINSTATE OFFSETBANK ROUTINES ####
      #           if (simulation_data_object$simulation_params$use_offset_bank == TRUE){
      #             simulation_data_object$output_data$current_credit = assess_banking_credit(simulation_data_object$output_data, simulation_data_object$simulation_params)
      #           }
      
      # attempt to develop from current available credit
      
      if (simulation_data_object$simulation_params$allow_developments_from_credit == TRUE){
        simulation_data_object <- credit_match_routine(simulation_data_object, yr)
      } 
      
      #if insufficient credits accumulated to allow development attempt development with offset match.
      if ( simulation_data_object$output_data$credit_match_flag == FALSE && simulation_data_object$simulation_params$use_parcel_sets == TRUE){
        simulation_data_object <- match_sites_routine(simulation_data_object, yr)
      }
      
    }
    

    if (!((simulation_data_object$simulation_params$unregulated_loss_type == 'default') & (simulation_data_object$simulation_params$unregulated_loss_prob == 0))){
      simulation_data_object <- run_unregulated_loss_routine(simulation_data_object, yr)
    }
    projection_yrs = lapply(seq_along(simulation_data_object$site_features), 
                            function(i) find_projection_yrs(perform_dynamics_time_shift = simulation_data_object$feature_params$perform_background_dynamics_time_shift, 
                                                            simulation_data_object$site_features[[i]], 
                                                            yr, 
                                                            feature_dynamics_to_use = simulation_data_object$feature_dynamics[[i]], 
                                                            simulation_data_object$feature_dynamics_modes[[i]],
                                                            dynamics_type = simulation_data_object$feature_params$background_dynamics_type))
    
    # update sites in landscape (both inside and outside development/offset program)
    flog.info('updating sites...')
    simulation_data_object$site_features = project_features(simulation_data_object$site_features,
                                                            simulation_data_object$feature_params$background_dynamics_type,
                                                            simulation_data_object$feature_params$background_update_dynamics_by_differential, 
                                                            simulation_data_object$feature_dynamics,
                                                            simulation_data_object$feature_dynamics_modes,
                                                            current_time_horizons = rep(list(1), length(simulation_data_object$site_features)),
                                                            perform_dynamics_time_shift = simulation_data_object$feature_params$perform_background_dynamics_time_shift,
                                                            time_fill = FALSE,
                                                            projection_yrs, 
                                                            condition_class_bounds = simulation_data_object$feature_params$condition_class_bounds)
    
    save_landscape_routine(simulation_data_object, current_data_dir, yr)
    
  }
  
  return(simulation_data_object$output_data)
}


unwrap_condition_classes <- function(current_site_feature, current_site_element_index_key){
  current_site_feature <- do.call(cbind, current_site_feature)
  current_site_feature <- current_site_feature[current_site_element_index_key]
  return(current_site_feature)
}

save_landscape_routine <- function(simulation_data_object, current_data_dir, yr){
  
  for (feature_ind in seq_along(simulation_data_object$simulation_params$features_to_use_in_simulation)){
    feature_layer_to_save = lapply(seq_along(simulation_data_object$site_features), 
                                   function(i) simulation_data_object$site_features[[i]][[feature_ind]])
    
    file_prefix = paste0('feature_', formatC(simulation_data_object$simulation_params$features_to_use_in_simulation[feature_ind], width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
                         '_yr_', formatC(yr, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"))
    
    saveRDS(feature_layer_to_save, paste0(current_data_dir, file_prefix, '.rds'))
    
    if (simulation_data_object$global_params$output_raster_tiff == TRUE){
      
      if (!file.exists(paste0(current_data_dir, 'raster_folder/'))){
        dir.create(paste0(current_data_dir, 'raster_folder/'))
      }
      feature_layer_to_save = lapply(seq_along(feature_layer_to_save), 
                                     function(i) as.matrix(unwrap_condition_classes(feature_layer_to_save[[i]], 
                                                                                    simulation_data_object$site_element_index_key[[i]][[feature_ind]])))
      feature_layer_to_output = matrix(0, nrow = simulation_data_object$site_characteristics$landscape_dims[1], ncol = simulation_data_object$site_characteristics$landscape_dims[2])
      feature_layer_to_output[unlist(simulation_data_object$site_characteristics$land_parcels)] = unlist(feature_layer_to_save)
      feature_layer_to_output = raster(feature_layer_to_output)
      writeRaster(feature_layer_to_output, paste0(current_data_dir, 'raster_folder/', file_prefix, simulation_data_object$global_params$raster_file_type))
    }
  }
  
  if (simulation_data_object$simulation_params$use_offset_metric == TRUE){
    
    #     current_metric_layers = lapply(seq_along(simulation_data_object$site_features), 
    #                                    function(i) lapply(seq_along(simulation_data_object$site_features[[i]]),
    #                                                       function(j) do.call(cbind, simulation_data_object$site_features[[i]][[j]])))
    #     
    #     current_metric_layers = lapply(seq_along(simulation_data_object$site_features), 
    #                                    function(i) lapply(seq_along(simulation_data_object$site_features[[i]]),
    #                                                       function(j) current_metric_layers[[i]][[j]][simulation_data_object$site_element_index_key[[i]][[j]]]))
    
    current_metric_layers = lapply(seq_along(simulation_data_object$site_features), 
                                   function(i) lapply(seq_along(simulation_data_object$site_features[[i]]),
                                                      function(j) unwrap_condition_classes(simulation_data_object$site_features[[i]][[j]], 
                                                                                           simulation_data_object$site_element_index_key[[i]][[j]])))
    
    current_metric_layers = lapply(seq_along(current_metric_layers), 
                                   function(i) user_transform_function(current_metric_layers[[i]], simulation_data_object$simulation_params$transform_params))
    
    saveRDS(current_metric_layers, paste0(current_data_dir, 'metric_layer', '_yr_', formatC(yr, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), '.rds'))
    
    #     if(simulation_data_object$global_params$save_output_raster == TRUE){
    #       current_feature_layer = matrix(0, nrow = simulation_data_object$site_characteristics$landscape_dims[1], ncol = simulation_data_object$site_characteristics$landscape_dims[2])
    #       current_feature_layer[unlist(simulation_data_object$site_characteristics$land_parcels)] = unlist(current_metric_layers)
    #       writeRaster(current_feature_raster, paste0(current_raster_folder, file_prefix, '.tif'), overwrite = TRUE)
    #       
    #     }
    
  }
  
}

match_sites_routine <- function(simulation_data_object, yr){
  #perform the matching routine - i.e. find a matching development/offset set 
  
  match_object <- match_sites(simulation_data_object, match_type = 'develop_using_offset_pool', yr) 
  
  # if a match was found record current development and associated offsets and update site parameters.
  
  if (match_object$match_flag == TRUE){
    
    flog.info(cat('developed site', paste(match_object$development_object$site_indexes),
                  'with value', paste( round(Reduce('+', match_object$development_object$parcel_vals_used), 1)), '\n',
                  'offset with sites', paste(match_object$offset_object$site_indexes), 
                  'with net value', paste(round(Reduce('+', match_object$offset_object$parcel_vals_used), 1)), '\n'))
    
    #update available credit
    simulation_data_object$output_data$current_credit = match_object$current_credit
    
    # remove selected offset sites from available pool, save offset site characteristics, update decline rates to implement offset.
    simulation_data_object <- run_offset_routines(simulation_data_object,
                                                  current_offset_object = match_object$offset_object,
                                                  yr)
    
    # remove selected development sites from available pool, save development site characteristics,
    # update decline rates to implement development loss.
    simulation_data_object <- run_clearing_routines(simulation_data_object,
                                                    current_dev_object = match_object$development_object,
                                                    clearing_type = 'development',
                                                    yr)
  }
  return(simulation_data_object)
}





credit_match_routine <- function(simulation_data_object, yr){
  
  if (all(simulation_data_object$output_data$current_credit <= 0) | (length(simulation_data_object$dev_pool_object$site_indexes) == 0)){
    match_object = setNames(list(FALSE), 'match_flag')
    
  } else {
    pool_vals_to_use = simulation_data_object$dev_pool_object$parcel_vals_used
    match_object <- match_from_pool(match_type = 'development', 
                                    current_pool = simulation_data_object$dev_pool_object$site_indexes, 
                                    pool_vals_to_use, 
                                    simulation_data_object$output_data$current_credit,
                                    simulation_data_object$dev_probability_list,
                                    vals_to_match_initial = simulation_data_object$output_data$current_credit,
                                    simulation_data_object$simulation_params,
                                    yr)
    if (match_object$match_flag == TRUE){
      
      simulation_data_object$output_data$current_credit = match_object$current_credit
      
      subset_pool = list_intersect(simulation_data_object$dev_pool_object$site_indexes, match_object$match_indexes)
      development_object = subset_current_pool(simulation_data_object$dev_pool_object, subset_pool = subset_pool$match_ind)
      simulation_data_object <- run_clearing_routines(simulation_data_object,
                                                      development_object,
                                                      clearing_type = 'develop_from_credit',
                                                      yr)
      
      flog.info(cat('developed site with value', paste(lapply(development_object$parcel_vals_used, round, 2)), 
                    'from credit, remaining = ', paste(lapply(match_object$current_credit, round, 2)), '\n'))
      
    }
  } 
  
  
  
  simulation_data_object$output_data$credit_match_flag = match_object$match_flag
  
  return(simulation_data_object)
}



#write all offset parcels to single layer to output as image

write_site_mask <- function(output_filename, landscape_dims, land_parcels, current_site_indexes){ 
  
  site_mask = array(0, landscape_dims)
  site_mask[ unlist(land_parcels[unlist(current_site_indexes)])] = 1
  # rgb.palette <- colorRampPalette(color_vec, space = "rgb")
  png(filename = output_filename, height = dim(site_mask)[1], width = dim(site_mask)[2])
  image(site_mask, zlim = c(0,1), col = rgb.palette(512))
  dev.off()
  return(site_mask)
}




#sample over uniform random vector, indicies less than the threshold level are selected for clearing
select_sites_to_clear <- function(available_site_indexes, simulation_params, yr){
  
  if (simulation_params$unregulated_loss_type == 'default'){
    clearing_thresh <- rep(simulation_params$unregulated_loss_prob, length(available_site_indexes))
    discrim <- runif(length(clearing_thresh)) < clearing_thresh
    inds_to_clear <- available_site_indexes[discrim]
  } else if (simulation_params$unregulated_loss_type == 'unregulated_stochastic_development'){
      inds_to_clear = sample(available_site_indexes, simulation_params$unregulated_intervention_vec[yr])
  } else if (simulation_params$unregulated_loss_type == 'unregulated_directed_development'){
      inds_to_clear = simulation_params$unregulated_intervention_vec[[yr]]
  }
  
  return(inds_to_clear)
}



run_unregulated_loss_routine <- function(simulation_data_object, yr){
  
  available_site_indexes = setdiff(unlist(simulation_data_object$output_data$index_object$available_indexes$unregulated_loss), 
                                   unlist(simulation_data_object$output_data$index_object$site_indexes_used))
  
  inds_to_clear <- select_sites_to_clear(available_site_indexes, simulation_data_object$simulation_params, yr)
  
  if (length(inds_to_clear) == 0){ #return null for no sites selected for clearing
    return(simulation_data_object)
  } else {
    flog.info(cat('unregulated loss of sites ' , paste(inds_to_clear), '\n'))
  }
  
  # store group of site characteristics in site characteristics object
  unregulated_loss_object <- record_site_characteristics(simulation_data_object$site_features[inds_to_clear],
                                                         inds_to_clear,
                                                         parcel_num_remaining = length(available_site_indexes),
                                                         yr)
  
  current_pool = unlist(unregulated_loss_object$site_indexes)
  # record characteristics of cleared site
  unregulated_loss_object <- assess_current_pool(pool_object = unregulated_loss_object,
                                                 pool_type = 'developments',
                                                 features_to_use = seq_along(simulation_data_object$simulation_params$features_to_use_in_simulation),
                                                 site_features = simulation_data_object$site_features[current_pool],
                                                 feature_dynamics = simulation_data_object$feature_dynamics[current_pool],
                                                 management_dynamics = simulation_data_object$management_dynamics[current_pool],
                                                 feature_dynamics_mode = simulation_data_object$feature_dynamics_modes[current_pool],
                                                 site_element_index_key = simulation_data_object$site_element_index_key[current_pool],
                                                 calc_type = simulation_data_object$simulation_params$dev_calc_type,
                                                 cfacs_flag = simulation_data_object$simulation_params$dev_cfacs_flag,
                                                 adjust_cfacs_flag = simulation_data_object$simulation_params$adjust_dev_cfacs_flag,
                                                 action_type = simulation_data_object$simulation_params$offset_action_type,
                                                 include_potential_developments = simulation_data_object$simulation_params$include_potential_developments_in_dev_calc,
                                                 include_potential_offsets = simulation_data_object$simulation_params$include_potential_offsets_in_dev_calc,
                                                 include_unregulated_loss = simulation_data_object$simulation_params$include_unregulated_loss_in_dev_calc,
                                                 recalculate_probabilities(simulation_data_object$dev_probability_list[current_pool]), 
                                                 recalculate_probabilities(simulation_data_object$offset_probability_list[current_pool]), 
                                                 time_horizon_type = 'future',
                                                 simulation_data_object$simulation_params,
                                                 simulation_data_object$feature_params,
                                                 simulation_data_object$simulation_params$offset_time_horizon,
                                                 yr)  
  
  # remove selected sites from available pool, save site characteristics, update decline rates to implement loss.
  if (!is.null(unregulated_loss_object)){
    simulation_data_object <- run_clearing_routines(simulation_data_object,
                                                    current_dev_object = unregulated_loss_object,
                                                    clearing_type = 'unregulated',
                                                    yr)
  }
  
  return(simulation_data_object)
}


update_feature_dynamics_modes <- function(feature_dynamics_modes, feature_num, features_to_update, action_type, current_pool){
  
  for (current_site_index in current_pool){
    
    if (action_type == 'development'){
      feature_dynamics_modes[[current_site_index]] <- rep(list(0), feature_num)
    } else if (action_type == 'maintain'){
      feature_dynamics_modes[[current_site_index]][features_to_update] = lapply(seq_along(features_to_update), 
                                                                                      function(i) rep(0, length(feature_dynamics_modes[[current_site_index]])))
    } 
  }
  
  return(feature_dynamics_modes)
  
}


update_feature_dynamics <- function(site_group_to_use, feature_dynamics_to_update, management_dynamics_to_use, 
                                    feature_dynamics_modes_to_use, feature_params, features_to_update, action_type, yr){
  
  if (action_type == 'develop'){
    current_feature_dynamics_set = lapply(seq_along(site_group_to_use), function(i) rep(list(array(0, length(feature_params$simulated_time_vec))), length(features_to_update)))
  } else if (action_type == 'offset'){
    
    time_shifts = lapply(seq_along(site_group_to_use), 
                         function(i) find_time_shifts(site_group_to_use[[i]], 
                                                      management_dynamics_to_use[[i]], 
                                                      feature_dynamics_modes_to_use[[i]], 
                                                      dynamics_type = feature_params$management_dynamics_type))
    
    current_feature_dynamics_set = lapply(seq_along(site_group_to_use), function(i) shift_dynamics_set(site_group_to_use[[i]], 
                                                                                                          management_dynamics_to_use[[i]],
                                                                                                          feature_dynamics_modes_to_use[[i]],
                                                                                                          dynamics_type = feature_params$management_dynamics_type, 
                                                                                                          project_by_mean = feature_params$project_by_mean,
                                                                                                          update_dynamics_by_differential = feature_params$management_update_dynamics_by_differential,
                                                                                                          time_shifts[[i]], 
                                                                                                          time_fill = TRUE))
  } 
  
  updated_feature_dynamics <- merge_dynamics(feature_dynamics_to_update, features_to_update, current_feature_dynamics_set, site_group_to_use, 
                                             feature_params$management_dynamics_type, yr, feature_params$project_by_mean)
  
  return(updated_feature_dynamics)
  
}


merge_dynamics <- function(feature_dynamics_to_update, features_to_update, current_feature_dynamics, site_group_to_use, management_dynamics_type, yr, project_by_mean){
  
  updated_feature_dynamics = feature_dynamics_to_update
  
  for (site_ind in seq_along(site_group_to_use)){
    if (management_dynamics_type == 'element_scale'){
      updated_feature_dynamics[[site_ind]][features_to_update] = lapply(features_to_update, 
                                                                        function(i) lapply(seq_along(feature_dynamics_to_update[[site_ind]][[i]]), 
                                                                                           function(j) append(feature_dynamics_to_update[[site_ind]][[i]][[j]][seq_len(yr - 1)], 
                                                                                                              current_feature_dynamics[[site_ind]][[i]][[j]])))
    }  else if (management_dynamics_type == 'site_scale'){
        if (project_by_mean == TRUE){
            updated_feature_dynamics[[site_ind]][features_to_update] = lapply(features_to_update, 
                                                                              function(i) lapply(seq_along(feature_dynamics_to_update[[site_ind]][[i]]), 
                                                                                                 function(j) append(feature_dynamics_to_update[[site_ind]][[i]][[j]][seq_len(yr - 1)], 
                                                                                                                    current_feature_dynamics[[site_ind]][[i]][[j]])))
        } else {
          updated_feature_dynamics[[site_ind]][features_to_update] = lapply(features_to_update, 
                                                                            function(i) lapply(seq_along(feature_dynamics_to_update[[site_ind]][[i]]), 
                                                                                               function(j) append(feature_dynamics_to_update[[site_ind]][[i]][seq_len(yr - 1)],
                                                                                                                  current_feature_dynamics[[site_ind]][[i]][[j]])))
        }
    }
  }
  return(updated_feature_dynamics)
}


find_time_shifts <- function(site_group_to_use, current_feature_dynamics_set, feature_dynamics_modes_to_use, dynamics_type){
  
  if (dynamics_type == 'element_scale'){
    lapply(seq_along(site_group_to_use), 
           function(i) lapply(seq_along(site_group_to_use[[i]]),
                              function(j) find_time_shift(site_group_to_use[[i]][j], 
                                                          current_feature_dynamics_set[[i]][[j]], feature_dynamics_modes_to_use[[i]][[j]])))
  } else if (dynamics_type == 'site_scale'){
    
    time_shifts = lapply(seq_along(site_group_to_use), 
                         function(i) lapply(seq_along(feature_dynamics_modes_to_use[[i]]),
                                            function(j) find_time_shift(mean(site_group_to_use[[i]][[j]]),
                                                                        current_feature_dynamics_set[[i]][[j]], feature_dynamics_modes_to_use[[i]][[j]])))
  }
  
  return(time_shifts)
  
}






shift_dynamics_set <- function(site_group_to_use, current_feature_dynamics_set, feature_dynamics_modes_to_use, dynamics_type, project_by_mean, update_dynamics_by_differential,
                               time_shifts, time_fill){
  
  if (dynamics_type == 'element_scale'){
    
    shifted_dynamics_set = lapply(seq_along(site_group_to_use), 
                                  function(i) lapply(seq_along(site_group_to_use[[i]]),
                                                     function(j) shift_dynamics(site_group_to_use[[i]][[j]],
                                                                                current_feature_dynamics_to_use = current_feature_dynamics_set[[i]][[j]],
                                                                                feature_dynamics_modes_to_use[[i]][[j]],
                                                                                current_time_shift = time_shifts[[i]][[j]], 
                                                                                time_fill, 
                                                                                update_dynamics_by_differential) ))
    
  } else if (dynamics_type == 'site_scale'){
    
    shifted_dynamics_set = lapply(seq_along(site_group_to_use), 
                                  function(i) lapply(seq_along(site_group_to_use[[i]]),
                                                     function(j) shift_dynamics(mean(site_group_to_use[[i]][[j]]),
                                                                                current_feature_dynamics_to_use = current_feature_dynamics_set[[i]][[j]],
                                                                                feature_dynamics_modes_to_use[[i]][[j]],
                                                                                current_time_shift = time_shifts[[i]][[j]], 
                                                                                time_fill, 
                                                                                update_dynamics_by_differential) ))
  }
  return(shifted_dynamics_set)
}



shift_dynamics <- function(current_feature_val, current_feature_dynamics_to_use, current_feature_dynamics_mode, current_time_shift, time_fill, update_dynamics_by_differential){
  
  if (current_feature_dynamics_mode == 0){
    return(current_feature_dynamics_to_use)
  }
  if (length(current_time_shift) > 0){
    if (time_fill == TRUE){
      
      time_vec = current_time_shift + 0:(length(current_feature_dynamics_to_use) - ceiling(current_time_shift))
    } else {
      time_vec = current_time_shift 
    }
    current_shifted_dynamics = approx(1:length(current_feature_dynamics_to_use), current_feature_dynamics_to_use, time_vec)$y
    
    
    if (update_dynamics_by_differential == TRUE){
      current_shifted_dynamics = diff(current_shifted_dynamics)
      #current_shifted_dynamics = cumsum(diff(current_shifted_dynamics))
    }
  } else {
    
    if (update_dynamics_by_differential == TRUE){
      current_shifted_dynamics = diff(current_feature_dynamics_to_use)
      #current_shifted_dynamics = cumsum(diff(current_feature_dynamics_to_use))
    } else {
      current_shifted_dynamics = current_feature_val + diff(current_feature_dynamics_to_use)
      #current_shifted_dynamics = current_feature_val + cumsum(diff(current_feature_dynamics_to_use))
    }
    
  }
  
  return(current_shifted_dynamics)
}



find_time_shift <- function(current_feature_val, current_feature_dynamics_to_use, current_feature_dynamics_mode){
  
  if (current_feature_dynamics_mode == 0){
    time_shift = 0 
  } else {
    min_loc = which(current_feature_dynamics_to_use == min(current_feature_dynamics_to_use))[1]
    
    if (current_feature_val == current_feature_dynamics_to_use[1]){
      time_shift = 1
    } else {
      intervals = findInterval(current_feature_dynamics_to_use, current_feature_val)
      peak_loc = which( diff(diff(current_feature_dynamics_to_use) >= 0) < 0) + 1
      
      if (length(peak_loc) > 0){
        peak_loc = which(current_feature_dynamics_to_use == max(current_feature_dynamics_to_use[peak_loc]))[1]
      } else {
        peak_loc = which(current_feature_dynamics_to_use == max(current_feature_dynamics_to_use))[1]
      } 
      
      if (all(intervals == 1)){
        #condition for which current_feature_val is less than current_feature_dynamics curve
        
        if (min_loc > peak_loc){
          time_shift = 1
        } else {
          time_shift = min_loc
        } 
        
      } else if (all(intervals == 0)){
        #condition for which current_feature_val is greater than current_feature_dynamics curve
        time_shift = peak_loc
        
      } else {
        #condition for which all current feature val is on current_feature_dynamics curve
        
        lower_bound = min(which(abs(diff(intervals)) == 1))
        
        if ((peak_loc > 1 ) & (lower_bound > peak_loc)){
          time_shift = 1
        } else{
          time_shift = approx(current_feature_dynamics_to_use[lower_bound:(lower_bound + 1)], lower_bound:(lower_bound + 1), current_feature_val)$y
        }
        
      }
      
    }
  }
  return(time_shift)
}


# series of routines to implement offset
run_offset_routines <- function(simulation_data_object, current_offset_object, yr){
  
  # if running in banking mode remove offset site from available bank
  if (simulation_data_object$simulation_params$use_offset_bank == TRUE){
    banked_offset_pool = simulation_data_object$output_data$interventions$offset_bank_object$site_indexes
    banked_offset_inds_used = list_intersect(banked_offset_pool, current_offset_object$site_indexes)
    
    # determine parcels used in matching routine and remove from available pool
    simulation_data_object$output_data$interventions$offset_bank_object$site_indexes = remove_index(banked_offset_pool, banked_offset_inds_used$match_ind)
    
  } else {
    # determine parcels used in matching routine and remove from available pool
    
    simulation_data_object$output_data$index_object <- update_index_object(simulation_data_object$output_data$index_object, update_type = 'offset', current_offset_object$site_indexes)
    
  }
  
  #record current offset site characteristics
  simulation_data_object$output_data$interventions$offsets_object <- append_current_group(simulation_data_object$output_data$interventions$offsets_object, current_offset_object, append_routine = 'standard')
  
  #remove offset sites from available pool
  if (length(current_offset_object$site_indexes) > 0){
    simulation_data_object$offset_pool_object <- remove_site_from_current_pool(simulation_data_object$offset_pool_object, current_site_indexes = current_offset_object$site_indexes)
    simulation_data_object$dev_pool_object <- remove_site_from_current_pool(simulation_data_object$dev_pool_object, current_site_indexes = current_offset_object$site_indexes)
  }
  current_pool = unlist(current_offset_object$site_indexes)
  
  simulation_data_object$feature_dynamics[current_pool] = update_feature_dynamics(simulation_data_object$site_features[current_pool], 
                                                                                  simulation_data_object$feature_dynamics[current_pool], 
                                                                                  simulation_data_object$management_dynamics[current_pool],
                                                                                  simulation_data_object$feature_dynamics_modes[current_pool],
                                                                                  simulation_data_object$feature_params, 
                                                                                  features_to_update = simulation_data_object$simulation_params$features_to_use_in_offset_intervention, 
                                                                                  action_type = 'offset', 
                                                                                  yr)
  #TODO: note do same as with current pool
  simulation_data_object$feature_dynamics_modes = update_feature_dynamics_modes(simulation_data_object$feature_dynamics_modes, 
                                                                                simulation_data_object$simulation_params$feature_num,
                                                                                simulation_data_object$simulation_params$features_to_use_in_offset_intervention, 
                                                                                simulation_data_object$simulation_params$offset_action_type, 
                                                                                current_pool)
  
  return(simulation_data_object)
}

# remove site characteristics from current pool of available sites
remove_site_from_current_pool <- function(pool_object, current_site_indexes){
  # work out indexes of current sites in pool
  sites_to_remove <- list_intersect(pool_object$site_indexes, current_site_indexes)
  if (length(sites_to_remove$match_ind) > 0){
    # remove site index from available pool
    subset_pool_to_use <- seq_along(pool_object$site_indexes)[-sites_to_remove$match_ind]
    # redefine pool with current used sites removed
    pool_object <- subset_current_pool(pool_object, subset_pool_to_use)
  }
  return(pool_object)
}


# routines to mark and destroy feature_layers in cleared sites e.g. Development or unregulated

run_clearing_routines <- function(simulation_data_object, current_dev_object, clearing_type, yr){
  
  if (length(current_dev_object$site_indexes) == 0){
    return(simulation_data_object)
  }
  #remove development parcels from available pool
  
  simulation_data_object$output_data$index_object <- update_index_object(simulation_data_object$output_data$index_object, update_type = clearing_type, current_dev_object$site_indexes)
  if (clearing_type == 'development'){
    #record current development site characteristics
    
    simulation_data_object$output_data$interventions$dev_object <- append_current_group(simulation_data_object$output_data$interventions$dev_object, current_dev_object, append_routine = 'standard')
    
  } else if (clearing_type == 'develop_from_credit'){
    #record current development site characteristics
    simulation_data_object$output_data$interventions$credit_object <- append_current_group(simulation_data_object$output_data$interventions$credit_object, current_dev_object, append_routine = 'standard')
  } else if (clearing_type == 'unregulated'){
    #record current cleared site characteristics
    simulation_data_object$output_data$interventions$unregulated_loss_object <- append_current_group(simulation_data_object$output_data$interventions$unregulated_loss_object, current_dev_object, append_routine = 'standard')
  }
  
  if (length(current_dev_object$site_indexes) > 0){
    simulation_data_object$offset_pool_object <- remove_site_from_current_pool(simulation_data_object$offset_pool_object, current_site_indexes = current_dev_object$site_indexes)
    simulation_data_object$dev_pool_object <- remove_site_from_current_pool(simulation_data_object$dev_pool_object, current_site_indexes = current_dev_object$site_indexes)
  }
  
  current_pool = unlist(current_dev_object$site_indexes)
  
  simulation_data_object$feature_dynamics[current_pool] = update_feature_dynamics(simulation_data_object$site_features[current_pool],
                                                                                  simulation_data_object$feature_dynamics[current_pool], 
                                                                                  simulation_data_object$management_dynamics[current_pool],
                                                                                  simulation_data_object$feature_dynamics_modes[current_pool],
                                                                                  simulation_data_object$feature_params, 
                                                                                  features_to_update = seq(simulation_data_object$simulation_params$feature_num), 
                                                                                  action_type = 'develop', 
                                                                                  yr)
  
  simulation_data_object$feature_dynamics_modes = update_feature_dynamics_modes(simulation_data_object$feature_dynamics_modes, 
                                                                                simulation_data_object$simulation_params$feature_num,
                                                                                seq(simulation_data_object$simulation_params$feature_num), 
                                                                                action_type = 'development', 
                                                                                current_pool)
  
  
  simulation_data_object$site_features[current_pool] = kill_site_features(simulation_data_object$site_features[current_pool], simulation_data_object$global_params$store_zeros_as_sparse)
  
  
  return(simulation_data_object)
  
}


# determine current available credit accumulated through offsets for development
assess_banking_credit <- function(output_data, simulation_params){
  
  features_to_use_in_offset_calc = simulation_params$features_to_use_in_offset_calc
  # determine total offset gains
  
  offset_credit = nested_list_sum(offset_pool_object$parcel_vals_used)
  
  dev_list = append(output_data$interventions$credit_object$parcel_vals_used, output_data$interventions$dev_object$parcel_vals_used)
  
  if (length(dev_list) > 0){
    # determine total development losses
    dev_sum = nested_list_sum(dev_list)
    current_credit = subtract_nested_lists(offset_credit, dev_sum)
    
  } else{
    current_credit = offset_credit
  }
  
  return(current_credit)
}

# determine characteristics of potential offset sites
build_intervention_pool <- function(simulation_data_object, pool_type, current_pool, yr){
  
  # if no developments or banked offsets for the current year return null object
  if (simulation_data_object$simulation_params$intervention_vec[yr] ==  0){
    pool_object = list()
    return(pool_object)
  }
  
  # if pool is empty return null object and print error
  
  if (length(current_pool) == 0){
    flog.error(paste0('empty ', pool_type, ' pool flag'))
    pool_object = list()
    return(pool_object)
  }
  
  if (pool_type == 'offset_bank'){
    
    flog.error('offset bank in development')
    stop()
    #     subset_pool = simulation_data_object$output_data$interventions$offset_bank_object$site_indexes
    #     
    #     pool_object <- subset_current_pool(simulation_data_object$output_data$interventions$offset_bank_object, subset_pool)
    # 
    #     current_pool = unlist(simulation_data_object$output_data$interventions$offset_bank_object$site_indexes[subset_pool])
    #     pool_object$projected_vals <- find_current_parcel_sums(simulation_data_object$site_features[current_pool])
    
  } else {
    
    pool_object <- record_site_characteristics(simulation_data_object$site_features[current_pool],
                                               current_pool,
                                               parcel_num_remaining = length(current_pool),
                                               yr)   #arrange available parcel pool into form to use in parcel set determination
  }
  
  pool_object <- assess_current_pool(pool_object = pool_object,
                                     pool_type,
                                     features_to_use = simulation_data_object$simulation_params$features_to_use_in_offset_calc,
                                     site_features = simulation_data_object$site_features[current_pool],
                                     feature_dynamics = simulation_data_object$feature_dynamics[current_pool],
                                     management_dynamics = simulation_data_object$management_dynamics[current_pool],
                                     feature_dynamics_modes = simulation_data_object$feature_dynamics_modes[current_pool],
                                     site_element_index_key = simulation_data_object$site_element_index_key[current_pool],
                                     calc_type = simulation_data_object$simulation_params$offset_calc_type,
                                     cfacs_flag = simulation_data_object$simulation_params$offset_cfacs_flag,
                                     adjust_cfacs_flag = simulation_data_object$simulation_params$adjust_offset_cfacs_flag,
                                     action_type = simulation_data_object$simulation_params$offset_action_type,
                                     include_potential_developments = simulation_data_object$simulation_params$include_potential_developments_in_offset_calc,
                                     include_potential_offsets = simulation_data_object$simulation_params$include_potential_offsets_in_offset_calc,
                                     include_unregulated_loss = simulation_data_object$simulation_params$include_unregulated_loss_in_offset_calc,
                                     recalculate_probabilities(simulation_data_object$dev_probability_list[current_pool]), 
                                     recalculate_probabilities(simulation_data_object$offset_probability_list[current_pool]), 
                                     time_horizon_type = simulation_data_object$simulation_params$offset_time_horizon_type,
                                     simulation_data_object$simulation_params,
                                     simulation_data_object$feature_params,
                                     time_horizon = simulation_data_object$simulation_params$offset_time_horizon,
                                     yr)      
  
  if(any(is.na(unlist(pool_object$parcel_vals_used)))){
    browser()
  }
  return(pool_object)
}


# routines to perform offset banking
banking_routine <- function(simulation_data_object, yr){
  
  # how many offsets to be added in current year
  offset_bank_num = unlist(simulation_data_object$simulation_params$banked_offset_vec[yr])
  if (offset_bank_num == 0){
    return(simulation_data_object$output_data)
  }
  
  # select current number of offset sites from current available pool to add to banked offset pool
  current_banked_offset_pool <- sample(simulation_data_object$output_data$index_object$available_indexes$offsets, offset_bank_num)
  
  # number of sites to potentially offset
  parcel_num_remaining = length(c(unlist(simulation_data_object$output_data$index_object$available_indexes$offsets),
                                  unlist(simulation_data_object$output_data$index_object$available_indexes$devs)))
  
  # record current offset pool characteristics
  current_banked_object <- record_site_characteristics(simulation_data_object$site_features[current_banked_offset_pool],
                                                       current_pool = current_banked_offset_pool,
                                                       parcel_num_remaining,
                                                       yr)   # arrange current parcel data
  
  simulation_data_object$output_data$interventions$offset_bank_object = append_current_group(object_to_append = simulation_data_object$output_data$interventions$offset_bank_object,
                                                                                             current_object = current_banked_object,
                                                                                             append_routine = 'banked_offset')
  
  # remove current group of sites from available pool
  simulation_data_object$output_data$index_object <- update_index_object(simulation_data_object$output_data$index_object,
                                                                         update_type = 'banking',
                                                                         current_banked_offset_pool)
  
  current_pool = unlist(current_banked_object$site_indexes)
  
  simulation_data_object$feature_dynamics_modes = update_feature_dynamics_modes(simulation_data_object$feature_dynamics_modes, 
                                                                                simulation_data_object$simulation_params$feature_num,
                                                                                simulation_data_object$simulation_params$features_to_use_in_offset_intervention, 
                                                                                action_type = simulation_data_object$simulation_params$offset_action_type, 
                                                                                current_pool)
  
  return(simulation_data_object)
}



assess_parcel_sets <- function(simulation_data_object, offsets_object, offset_parcel_sets, time_horizon, yr){
  
  assessed_parcel_sets_object <- list()
  
  parcel_set_count = length((offset_parcel_sets))
  discriminator_metric <- vector('list', parcel_set_count)
  
  for (parcel_set_ind in seq_len(parcel_set_count)){
    
    # select current set of sites from offsets object
    current_parcel_set <- which(unlist(offsets_object$site_indexes) %in% unlist(offset_parcel_sets[[parcel_set_ind]]))
    
    # select characteristics of current set of sites
    current_offset_object <- lapply(seq_along(offsets_object), function(i) offsets_object[[i]][current_parcel_set])
    names(current_offset_object) <- names(offsets_object)
    
    parcel_vals_achieved <- assess_current_gain_pool(site_features,
                                                     pool_object = current_offset_object,
                                                     calc_type = simulation_data_object$simulation_params$offset_calc_type,
                                                     include_potential_developments = simulation_data_object$simulation_params$include_potential_developments_in_offset_calc,
                                                     include_potential_offsets = simulation_data_object$simulation_params$include_potential_offsets_in_offset_calc,
                                                     include_unregulated_loss = simulation_data_object$simulation_params$include_unregulated_loss_in_offset_calc,
                                                     simulation_data_object$simulation_params,
                                                     simulation_data_object$feature_params, 
                                                     feature_dynamics,
                                                     time_horizon,
                                                     yr)
    
    for (feature_ind in seq_len(simulation_data_object$simulation_params$feature_num)){
      discriminator_metric[[parcel_set_ind]] = nested_list_sum(subtract_nested_lists(parcel_vals_achieved, current_offset_object$parcel_vals_used))
    }
  }
  
  # determine whether site has reached gains for subset of features used in offset calculations
  parcel_set_success_inds <- unlist(lapply(seq_along(discriminator_metric),
                                           function(i) (all(unlist(discriminator_metric[[i]][simulation_data_object$simulation_params$features_to_use_in_offset_calc]) > 0))))
  
  site_success_inds = which(unlist(offsets_object$site_indexes) %in% unlist(offset_parcel_sets[parcel_set_success_inds]))
  assessed_parcel_sets_object$site_success_inds <- offsets_object$site_indexes[site_success_inds]
  assessed_parcel_sets_object$discriminator_metric <- discriminator_metric
  
  return(assessed_parcel_sets_object)
}



# assess_current_gain_pool <- function(site_features, pool_object, calc_type, time_fill,
#                                      include_potential_developments, include_potential_offsets, include_unregulated_loss,
#                                      simulation_params, feature_params, feature_dynamics, time_horizon, yr){
#   
#   stop()
#   print('reinstate routine')
#   current_pool = unlist(pool_object$site_indexes)
#   parcel_count = length(current_pool)
#   intervention_yrs = unlist(pool_object$intervention_yrs)
#   
#   time_horizons <- generate_time_horizons(project_type = 'current', yr, intervention_yrs, time_horizon, parcel_count)
#   
#   if (cfacs_flag == TRUE){
#     
#     if (adjust_cfacs_flag == FALSE){
#       time_fill = FALSE
#     } else {
#       time_fill = TRUE
#     }
#     
#     if (simulation_data_object$simulation_params$use_offset_metric == FALSE){
#       cfacs = unlist(lapply(seq_along(site_group_to_use), 
#                             function(i) sum_site_features(calc_site_cfacs(site_group_to_use[i],
#                                                              parcel_num_remaining = pool_object$parcel_num_remaining[i],
#                                                              simulation_data_object$simulation_params,
#                                                              feature_params,
#                                                              feature_dynamics_to_use[i],
#                                                              feature_dynamics_modes_to_use[i],
#                                                              time_horizons[i],
#                                                              unlist(pool_object$intervention_yrs)[i],
#                                                              include_potential_developments,
#                                                              include_potential_offsets,
#                                                              include_unregulated_loss,
#                                                              adjust_cfacs_flag = simulation_data_object$simulation_params$adjust_offset_cfacs_flag,
#                                                              time_fill,
#                                                              yr), simulation_data_object$simulation_params$feature_num)), recursive = FALSE)
#       
#     } else {
#       
#       cfacs = lapply(seq_along(site_group_to_use), 
#                      function(i) sum_cols(user_transform_function(unlist(calc_site_cfacs(site_group_to_use[i],
#                                                                                     parcel_num_remaining = pool_object$parcel_num_remaining[i],
#                                                                                     simulation_data_object$simulation_params,
#                                                                                     feature_params,
#                                                                                     feature_dynamics_to_use[i],
#                                                                                     feature_dynamics_modes_to_use[i],
#                                                                                     time_horizons[i],
#                                                                                     unlist(pool_object$intervention_yrs)[i],
#                                                                                     include_potential_developments,
#                                                                                     include_potential_offsets,
#                                                                                     include_unregulated_loss,
#                                                                                     adjust_cfacs_flag = simulation_data_object$simulation_params$adjust_offset_cfacs_flag,
#                                                                                     time_fill,
#                                                                                     yr), recursive = FALSE), simulation_data_object$simulation_params$transform_params)))
#     }
#     
#     cfac_vals = nested_list_tail(cfacs)
#     
#   }
#   projected_vals = site_features[current_pool]
#   
#   projected_vals = lapply(seq_along(projected_vals), function(i) lapply(seq_along(simulation_data_object$simulation_params$features_to_use_in_offset_calc), function(j) sum(projected_vals[[i]][[j]] )))
#   
#   parcel_vals_achieved = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, projected_vals, cfac_vals)
#   
#   return(parcel_vals_achieved)
# }


#construct a list of zero arrays with identical dimensions defined by array_dims
list_of_zeros <- function(list_dims, array_dims){
  list_object = vector('list', list_dims)
  for (list_ind in seq_len(list_dims)){
    list_object[[list_ind]] = array(0, array_dims)
  }
  return(list_object)
}




#used to break up array into smaller set of sub arrays defined by vx and vy that fit together to give input array
mcell <- function(x, vx, vy){
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  # make an array composed of lists with dimenisons that define the land parcels/regions.
  # The list format allows arrays of different sizes to be stored
  B = vector('list', rows*cols)
  colStart = 0
  # run down through the columns of input array
  for (i in seq_len(cols)){
    rowStart = 0
    for (j in seq_len(rows)){
      #group elements of input array into sub arrays and assign to B
      B[[a]] = x[rowStart+(1:rowsizes[j]), colStart+(1:colsizes[i])]
      rowStart = rowStart + rowsizes[j]
      a = a + 1
    }
    colStart = colStart + colsizes[i]
  }
  
  parcel = list()
  parcel$dims = c(length(vy), length(vx))
  parcel$elements = B
  return(parcel)
  
}


project_feature_layer <- function(dynamics_type, update_dynamics_by_differential, current_site_feature_layer, feature_dynamics_to_use, 
                                  feature_dynamics_modes_to_use, current_condition_class_bounds, time_horizon, perform_dynamics_time_shift, time_fill, projection_yrs){
  
  if (time_horizon == 0){
    return(current_site_feature_layer)
  }
  
  if (time_fill == TRUE){
    time_vec = seq_len(time_horizon)
    
  } else {
    time_vec = time_horizon
  }
  
  if (dynamics_type == 'site_scale'){
    projected_feature = lapply(seq_along(feature_dynamics_modes_to_use), function(i) project_elements(current_mode = feature_dynamics_modes_to_use[i], 
                                                                                                      current_vals_set = current_site_feature_layer[[i]], 
                                                                                                      time_horizon, 
                                                                                                      time_vec,
                                                                                                      projection_yr = projection_yrs[[i]], 
                                                                                                      feature_dynamics_to_use[[i]], 
                                                                                                      current_condition_class_bounds))
  }
  
  return(projected_feature)
}


project_elements <- function(current_mode, current_vals_set, time_horizon, time_vec, projection_yr, feature_dynamics_to_use, current_condition_class_bounds){
  
  
  if (current_mode == 0){
    projected_elements = current_vals_set
  } else {
    
    if ( (projection_yr + time_horizon) <= length(feature_dynamics_to_use) ){
      current_feature_dynamics = feature_dynamics_to_use[projection_yr:(projection_yr + (time_horizon - 1))]
      
    } else {
      
      current_feature_dynamics = array(0, time_horizon)
      
      if (projection_yr <= length(feature_dynamics_to_use)){
        feature_dynamics_to_merge = feature_dynamics_to_use[projection_yr:length(feature_dynamics_to_use)]
        current_feature_dynamics[1:length(feature_dynamics_to_merge)] = feature_dynamics_to_merge
      } 
      
    }
    
    current_feature_dynamics = cumsum(current_feature_dynamics)[time_vec]
    
    #current_feature_dynamics = current_feature_dynamics[time_vec]
    
    current_feature_dynamics = matrix( rep(current_feature_dynamics, length(current_vals_set)), byrow = FALSE, nrow = length(time_vec))
    
    projected_elements = matrix(rep(current_vals_set, length(time_vec)), byrow = TRUE, nrow = length(time_vec)) + current_feature_dynamics
    
    # enforce limits on projection
    overbounds = projected_elements > max(current_condition_class_bounds[[current_mode]])
    
    if (length(overbounds) > 0){
      projected_elements[overbounds] = max(current_condition_class_bounds[[current_mode]])
    }
    
    underbounds = projected_elements < min(current_condition_class_bounds[[current_mode]])
    
    if (length(underbounds) > 0){
      projected_elements[underbounds] = min(current_condition_class_bounds[[current_mode]])
    }
    
  }
  
  return(projected_elements)
}




# project sites through all features - returns nested list of arrays where each nested array has length
# defined by current_time_horizons and depth defined by feature number for all sites

project_features <- function(current_site_features, dynamics_type, update_dynamics_by_differential, feature_dynamics_to_use, feature_dynamics_modes_to_use, 
                             current_time_horizons, perform_dynamics_time_shift, time_fill, projection_yrs, condition_class_bounds){
  
  projected_features = lapply(seq_along(current_site_features),
                              function(i) lapply(seq_along(current_site_features[[i]]),
                                                 function(j) project_feature_layer( dynamics_type,
                                                                                    update_dynamics_by_differential, 
                                                                                    current_site_features[[i]][[ j ]],
                                                                                    feature_dynamics_to_use[[i]][[ j ]],
                                                                                    feature_dynamics_modes_to_use[[i]][[ j ]],
                                                                                    condition_class_bounds[[j]],
                                                                                    time_horizon = unlist(current_time_horizons[i]),
                                                                                    perform_dynamics_time_shift,
                                                                                    time_fill, 
                                                                                    projection_yrs[[i]][[j]])))
  
  
  return(projected_features)
  
}



# find sum nested list of depth j with identical structure
nested_list_sum <- function(nested_list){
  
  if (length(nested_list) > 0){
    summed_list <- lapply(seq_along(nested_list[[1]]),
                          function(j) Reduce('+', lapply(seq_along(nested_list), function(i) nested_list[[i]][[j]])))
    return(summed_list)
  } else {
    return(NULL)
  }
  
}


#build nested list with length outer_dim and depth defined by inner_dim
generate_nested_list <- function(outer_dim, inner_dim){
  if (outer_dim > 0){
    nested_list <- vector('list', outer_dim)
  } else {
    nested_list = list()
  }
  for (outer_ind in seq_len(outer_dim)){
    nested_list[[outer_ind]] <- vector('list', inner_dim)
  }
  return(nested_list)
}


remove_index <- function(object_list, ind_to_remove){
  if (length(ind_to_remove) > 0){
    object_list <- object_list[-ind_to_remove]
  }
  return(object_list)
}



recalculate_probabilities <- function(current_probability_list){
  current_probability_list = lapply(seq_along(current_probability_list), function(i) current_probability_list[[i]]/sum(unlist(current_probability_list)))
  return(current_probability_list)
}


match_sites <- function(simulation_data_object, match_type, yr){
  ###### TODO check zero development routines the code below may not be the most efficient method for this ###
  
  match_object = setNames(list(FALSE), 'match_flag')
  
  if (match_type == 'develop_using_offset_pool'){
    current_match_vals_pool = simulation_data_object$dev_pool_object$parcel_vals_used
    
    if (sum(unlist(current_match_vals_pool))== 0){
      
      flog.info('all projected developments are zero - blocking all developments')
      
      match_object$offset_object = list()
      match_object$current_credit = simulation_data_object$output_data$current_credit
      
      return(match_object)
      
    } else {
      
      current_match_pool = unlist(simulation_data_object$dev_pool_object$site_indexes)
      
      group_pool_vals = simulation_data_object$offset_pool_object$parcel_vals_used
      group_pool_indexes = simulation_data_object$offset_pool_object$site_indexes
      parcel_num_remaining = length(current_match_pool)
    }
    
  }
  
  if ((length(group_pool_indexes) == 0) | (parcel_num_remaining == 0)){
    return(match_object)
  } 
  
  zero_inds <- which(unlist(lapply(seq_along(group_pool_vals),  function(i) all(group_pool_vals[[i]] == 0))))
  
  group_pool_vals = remove_index(group_pool_vals, zero_inds)
  group_pool_indexes = remove_index(group_pool_indexes, zero_inds)
  
  while( (match_object$match_flag == FALSE)){   
    
    if ((simulation_data_object$simulation_params$development_selection_type == 'sampled') 
        | (simulation_data_object$simulation_params$development_selection_type == 'directed')){
      current_dev_probability_list = rep(1/length(current_match_pool))
    } else if (simulation_data_object$simulation_params$development_selection_type == 'weighted'){
      current_dev_probability_list = recalculate_probabilities(simulation_data_object$dev_probability_list[unlist(current_match_pool)])
    } 
    
    sample_ind = sample(seq_along(current_match_pool), size = 1, prob = current_dev_probability_list)
    current_test_index = current_match_pool[sample_ind]
    vals_to_match = current_match_vals_pool[[sample_ind]]
    
    if (simulation_data_object$simulation_params$use_offset_bank == FALSE){
      dev_ind = list_intersect(group_pool_indexes, current_test_index) #find and remove index that corresponds to potiential development index
      match_pool_to_use = remove_index(group_pool_indexes, dev_ind$match_ind)
      pool_vals_to_use = remove_index(group_pool_vals, dev_ind$match_ind)
      if (length(match_pool_to_use) == 0){
        break()
      } 
    } else {
      match_pool_to_use = group_pool_indexes  #if performing offset banking use any of the available banked offset pool
      pool_vals_to_use = group_pool_vals
    }
    
    match_object <- match_from_pool(match_type = 'offset',
                                    match_pool_to_use,
                                    pool_vals_to_use,
                                    unlist(simulation_data_object$output_data$current_credit),
                                    simulation_data_object$offset_probability_list,
                                    vals_to_match_initial = vals_to_match,
                                    simulation_data_object$simulation_params,
                                    yr) #perform matching routine
    
    if (match_object$match_flag == FALSE){
      
      # only try to match sites with smaller ecological value than current site
      inds_to_keep = which(lapply(seq_along(simulation_data_object$dev_pool_object$parcel_vals_used),
                                  function(i) all(unlist(subtract_nested_lists(simulation_data_object$dev_pool_object$parcel_vals_used[[i]], vals_to_match)) < 0) ) == TRUE)
      
      current_match_pool = simulation_data_object$dev_pool_object$site_indexes[inds_to_keep]     
      current_match_vals_pool = simulation_data_object$dev_pool_object$parcel_vals_used[inds_to_keep]
    }
  }
  
  if (match_object$match_flag == TRUE){
    dev_match_index = which(unlist(simulation_data_object$dev_pool_object$site_indexes) == current_test_index)
    match_object$development_object = subset_current_pool(simulation_data_object$dev_pool_object, unlist(dev_match_index))
    subset_pool =  list_intersect(simulation_data_object$offset_pool_object$site_indexes, match_object$match_indexes)
    offset_object <- subset_current_pool(pool_object = simulation_data_object$offset_pool_object, subset_pool = subset_pool$match_ind)
    match_object$offset_object = offset_object
  } else if (match_object$match_flag == FALSE){
    match_object$offset_object = list()
    match_object$current_credit = simulation_data_object$output_data$current_credit
  }
  
  return(match_object)
  
}


develop_from_credit <- function(simulation_data_object, intervention_vec, dev_indexes_to_use, yr, time_horizon){
  
  parcel_num_remaining = length(dev_indexes_to_use)
  
  # store group of site characteristics in site characteristics object
  dev_pool_object <- record_site_characteristics(simulation_data_object$site_features[dev_indexes_to_use],  
                                                 dev_indexes_to_use,  
                                                 parcel_num_remaining,  
                                                 yr)
  current_pool = unlist(dev_pool_object$site_indexes)
  
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object,
                                         pool_type = 'developments',
                                         features_to_use = simulation_data_object$simulation_params$features_to_use_in_offset_calc,
                                         site_features = simulation_data_object$site_features[current_pool],
                                         feature_dynamics = simulation_data_object$feature_dynamics[current_pool],  
                                         management_dynamics = simulation_data_object$management_dynamics[current_pool],
                                         feature_dynamics_modes = simulation_data_object$feature_dynamics_modes[current_pool],
                                         site_element_index_key = simulation_data_object$site_element_index_key[current_pool],
                                         calc_type = simulation_data_object$simulation_params$dev_calc_type,
                                         cfacs_flag = simulation_data_object$simulation_params$dev_cfacs_flag,
                                         adjust_cfacs_flag = simulation_data_object$simulation_params$adjust_dev_cfacs_flag,
                                         action_type = simulation_data_object$simulation_params$offset_action_type,
                                         include_potential_developments = simulation_data_object$simulation_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = simulation_data_object$simulation_params$include_potential_offsets_in_dev_calc,
                                         include_unregulated_loss = simulation_data_object$simulation_params$include_unregulated_loss_in_dev_calc,
                                         recalculate_probabilities(simulation_data_object$dev_probability_list[current_pool]), 
                                         recalculate_probabilities(simulation_data_object$offset_probability_list[current_pool]), 
                                         time_horizon_type = 'future',
                                         simulation_data_object$simulation_params,
                                         simulation_data_object$feature_params,
                                         time_horizon,
                                         yr)
  
  #   if (any(unlist(dev_pool_object$parcel_vals_used) < 0)){
  #     browser()
  #   }
  #   subset_pool =  list_intersect(dev_pool_object$site_indexes, match_object$match_indexes)
  #   match_object$development_object = subset_current_pool(dev_pool_object, subset_pool = subset_pool$match_ind)
  
  if (length(unlist(dev_pool_object$site_indexes)) > 0){
    
    pool_vals_to_use = dev_pool_object$parcel_vals_used
    match_object <- match_from_pool(match_type = 'development', 
                                    current_pool = dev_pool_object$site_indexes, 
                                    pool_vals_to_use, 
                                    simulation_data_object$output_data$current_credit,
                                    simulation_data_object$dev_probability_list,
                                    vals_to_match_initial = simulation_data_object$output_data$current_credit,
                                    simulation_data_object$simulation_params,
                                    yr)
    
  } else{
    match_object = setNames(list(FALSE), 'match_flag')
  }
  
  if (match_object$match_flag == TRUE){
    subset_pool =  list_intersect(dev_pool_object$site_indexes, match_object$match_indexes)
    match_object$development_object = subset_current_pool(dev_pool_object, subset_pool = subset_pool$match_ind)
  } else{
    #match_object$development_object = list()
    #match_object$current_credit = simulation_data_object$output_data$current_credit
  }
  
  return(match_object)
  
}


evaluate_parcel_vals <- function(calc_type, current_condition_vals, projected_vals, cfac_vals){
  
  if (calc_type == 'current_condition'){
    projected_vals = current_condition_vals
    cfac_vals = list_of_zeros(length(current_condition_vals), 1)
  } else if (calc_type == 'restoration_gains'){
    cfac_vals = current_condition_vals
  } else if (calc_type == 'restoration_condition_value'){
    cfac_vals = list_of_zeros(length(projected_vals), 1)
  } else if (calc_type == 'avoided_condition_decline'){
    projected_vals = current_condition_vals
  } else if ((calc_type == 'future_condition')){
    projected_vals = cfac_vals
    cfac_vals = list_of_zeros(length(current_condition_vals), 1)
  }
  parcel_vals_pool = subtract_nested_lists(projected_vals, cfac_vals)
  return(parcel_vals_pool)
}



subtract_nested_lists <- function(list_a, list_b){
  length_a = unlist(lapply(seq_along(list_a), function(i) length(list_a[[i]])))
  length_b = unlist(lapply(seq_along(list_b), function(i) length(list_b[[i]])))
  stopifnot(all(length_a == length_b))
  list_c = lapply( seq_along(list_a), function(i)  mapply('-', list_a[[i]], list_b[[i]], SIMPLIFY = FALSE))
  return(list_c)
}


sum_nested_lists <- function(list_to_sum){
  
  summed_list = list_to_sum[[1]]
  if (length(list_to_sum) > 1){
    for (list_ind in 2:length(list_to_sum)){
      current_list = list_to_sum[[list_ind]]
      summed_list = lapply(seq_along(summed_list), function(i) mapply('+', summed_list[[i]], current_list[[i]], SIMPLIFY = FALSE))
    }
  }
  
  return(summed_list)
}


subtract_lists <- function(list_a, list_b){
  list_c = mapply('-', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}

sum_lists <- function(list_a, list_b){
  if ( (length(list_a) == length(list_b)) & (length(list_a) >0)){
    list_c = mapply('+', list_a, list_b, SIMPLIFY = FALSE)
  } else if (length(list_a) == 0){
    if (length(list_b) == 0){
      list_c = list()
    } else{
      list_c = list_b
    }
  } else if (length(list_b) == 0){
    if (length(list_a) == 0){
      list_c = list()
    } else{
      list_c = list_a
    }
  }
  return(list_c)
}


# store group of site characteristics in parcel_set_object
record_site_characteristics <- function(site_features, current_pool, parcel_num_remaining, yr){
  
  parcel_set_object = list()
  parcel_set_object$intervention_yrs = rep(list(yr), length(current_pool))
  parcel_set_object$parcel_sums_at_offset = lapply(seq_along(site_features), function(i) do.call(cbind, sum_site_features(site_features[[i]])))
  parcel_set_object$site_indexes = as.list(current_pool)
  parcel_set_object$parcel_num_remaining = rep(list(parcel_num_remaining), length(current_pool))
  
  return(parcel_set_object)
  
}

# determine last element of all nested list vectors
nested_list_tail <- function(list_a){
  last_elements <- lapply(seq_along(list_a),
                          function(i) unlist(lapply(seq_along(list_a[[i]]),
                                                    function(j) list_a[[i]][[j]][ length(list_a[[i]][[j]]) ] )))
  return(last_elements)
}



##### TODO probability list should be passed to this - otherwise underlying assumption is equal weights for all sites which is wrong #####
###### NOTE THAT PROBABILITY CALC IS WRONG 
find_intervention_probability <- function(intervention_vec, site_intervention_probability, intervention_yrs, calc_type, offset_intervention_scale, 
                                          time_horizons, parcel_num, parcel_num_remaining, time_steps){
  
  intervention_probs = vector('list', parcel_num)
  parcel_num_remaining = unlist(parcel_num_remaining)
  intervention_yrs = unlist(intervention_yrs)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_ind]
    
    current_prob = array(0, (time_horizon))
    
    current_intervention_vec = intervention_vec[intervention_yrs[parcel_ind]:time_steps]
    
    if (length(current_intervention_vec) < (time_horizon)){
      current_intervention_vec = c(current_intervention_vec, array(0, ((time_horizon) - length(current_intervention_vec))))
    }
    
    current_intervention_vec = current_intervention_vec[1:(time_horizon)]
    
    current_parcel_num_remaining = parcel_num_remaining[parcel_ind]
    
    if (current_parcel_num_remaining > 0) {
      current_prob = current_prob + site_intervention_probability[[parcel_ind]]*(current_intervention_vec)
    }
    
    if (calc_type == 'offset'){
      current_prob = offset_intervention_scale*current_prob
    }
    
    intervention_probs[[parcel_ind]] = current_prob
    
  }
  
  return(intervention_probs)
}



# test to determine nearest neighbour by Euclidean norm given a pool of potential candidates (parcel_vals_pool)
# and the criterion to match to (vals_to_match)

euclidean_norm_match <- function(parcel_vals_pool, vals_to_match){
  
  euclidean_norm = lapply(seq_along(parcel_vals_pool), function(i) parcel_vals_pool[[i]] - vals_to_match)
  
  #build euclidean metric to perform euclidean match
  euclidean_norm = unlist(lapply(seq_along(euclidean_norm), function(i) sum(euclidean_norm[[i]]^2)))
  
  match_ind = which(euclidean_norm == min(euclidean_norm))
  
  if (length(match_ind) > 1){
    
    #list where the duplicate occurred
    flog.error(cat('duplicate site value match on ', length(match_ind), 'sites \n'))
    # sample site from multiple match pool
    match_ind = sample(match_ind, 1)
  }
  
  match_vals = parcel_vals_pool[match_ind]
  
  match_object = list()
  match_object$match_vals = match_vals
  match_object$match_ind = match_ind
  return(match_object)
}


select_cols <- function(arr_to_use, col_inds){
  arr_to_use <- arr_to_use[, col_inds]
  arr_to_use <- t(t(arr_to_use))
  return(arr_to_use)
}


select_pool_to_match <- function(vals_to_match, use_offset_metric, thresh, pool_vals_to_use, 
                                 max_parcel_num, current_pool, match_type, screen_site_zeros){
  
  pool_object = list()
  
  if (length(unlist(pool_vals_to_use)) == 0){
    pool_object$break_flag = TRUE
    return(pool_object)
  } 
  
  if (max_parcel_num > 1){
    pool_vals_to_test = unlist(lapply(seq_along(vals_to_match), 
                                      function(i) vals_to_match[i] - sum(tail(sort(unlist(lapply(seq_along(pool_vals_to_use),  
                                                                                                 function(j) pool_vals_to_use[[j]][[i]]))), max_parcel_num)) < thresh[i]))
    if (any(pool_vals_to_test == FALSE)){
      pool_object$break_flag = TRUE
      return(pool_object)
    }
    
  } else {
    
    if (screen_site_zeros == TRUE){ 
      zero_inds <- which(unlist(lapply(seq_along(pool_vals_to_use), function(i) sum(pool_vals_to_use[[i]]) == 0)))
      
      if (length(zero_inds) > 0){
        current_pool <- remove_index(current_pool, zero_inds)
        pool_vals_to_use <- remove_index(pool_vals_to_use, zero_inds)
      }
      
      if (length(current_pool) == 0){
        cat('\nall sites yield zero assessment')
        pool_object$break_flag = TRUE
        return(pool_object)
      } 
      
    } 
    
    if (match_type == 'offset'){
      pool_condition = unlist(lapply(seq_along(pool_vals_to_use), function(i) all( (vals_to_match - pool_vals_to_use[[i]]) <= thresh)))
    } else if (match_type == 'development'){
      pool_condition = unlist(lapply(seq_along(pool_vals_to_use), function(i) all( (vals_to_match - pool_vals_to_use[[i]]) >= -thresh)))
    }
    
    pool_vals_to_use <- pool_vals_to_use[pool_condition]
    current_pool <- current_pool[pool_condition]
    
    if (all(pool_condition == FALSE)){
      pool_object$break_flag = TRUE
      return(pool_object)
    } 
  }
  
  pool_object$break_flag = FALSE
  pool_object$pool_vals_to_use = pool_vals_to_use
  pool_object$current_pool = current_pool
  
  return(pool_object)
}







match_from_pool <- function(match_type, current_pool, pool_vals_to_use, current_credit, current_probability_list, 
                            vals_to_match_initial, simulation_params, yr){
  
  if (length(unlist(current_pool)) == 0){
    match_object = setNames(list(FALSE), 'match_flag')
    return(match_object)
  } 
  
  if (match_type == 'offset'){
    max_parcel_num = simulation_params$max_offset_parcel_num
    match_procedure = simulation_params$offset_selection_type
    screen_site_zeros = simulation_params$screen_offset_zeros
    
    vals_to_match = simulation_params$offset_multiplier * vals_to_match_initial
    
    if ( simulation_params$allow_developments_from_credit == TRUE){
      vals_to_match = vals_to_match - current_credit
    }
    
  } else if (match_type == 'development'){
    
    # force only single development for development routine
    max_parcel_num = 1
    
    if (simulation_params$screen_dev_zeros == FALSE){
      # if zeros are allowed use random selection 
      match_procedure = 'sampled'
    } else {
      match_procedure = simulation_params$development_selection_type
    }
    # when developing from credit, use inverse offset multiplier
    vals_to_match = 1/simulation_params$offset_multiplier * vals_to_match_initial
  }
  
  #create an array of threshold values defined by user based proportion 
  thresh = simulation_params$match_threshold_ratio * vals_to_match         
  
  pool_object <- select_pool_to_match(vals_to_match, simulation_params$use_offset_metric, thresh, 
                                      pool_vals_to_use, max_parcel_num, current_pool, match_type, simulation_params$screen_dev_zeros)
  
  if (pool_object$break_flag == TRUE){
    match_object = setNames(list(FALSE), 'match_flag')
    return(match_object)
  }
  
  # initialise parcel_vals for matching procedure
  parcel_vals_pool = pool_object$pool_vals_to_use
  current_pool = pool_object$current_pool
  match_flag = FALSE
  match_vals = list()
  match_indexes = list()
  
  while(match_flag == FALSE){
    
    if (length(current_pool) == 0){
      break
    }
    
    if (match_procedure == 'greedy'){
      match_params = euclidean_norm_match(parcel_vals_pool, vals_to_match)
    } else {
      
      if (match_procedure == 'weighted'){
        probability_list_to_use = recalculate_probabilities(current_probability_list[unlist(current_pool)])
        
      } else {
        probability_list_to_use = rep(1/length(current_pool), length(current_pool))
      }
      match_params = list()
      match_params$match_ind = sample(length(current_pool), 1, probability_list_to_use)
      match_params$match_vals = parcel_vals_pool[match_params$match_ind]
    }
    
    current_match_val = unlist(match_params$match_vals)
    current_match_index = current_pool[match_params$match_ind]
    vals_to_match = vals_to_match - current_match_val
    
    if (max_parcel_num > 1){
      ind_to_remove = list_intersect(current_pool, current_match_index)
      current_pool = remove_index(current_pool, ind_to_remove$match_ind)
      parcel_vals_pool = remove_index(parcel_vals_pool, ind_to_remove$match_ind)
      match_vals = append(match_vals, current_match_val)
      match_indexes = append(match_indexes, current_match_index)
      
      if (length(unlist(match_indexes)) > max_parcel_num){
        match_flag = FALSE
        break
      }
      
    } else {
      match_indexes = list(current_match_index)
      match_vals = list(current_match_val)
    }
    
    if (match_type == 'offset'){
      
      match_flag = all(vals_to_match <= thresh)
    } else if (match_type == 'development'){
      match_flag = all(vals_to_match >= -thresh)
    } 
    
  }
  
  #### TODO NOTE if running with offset_metric specified current credit is wrong as it copies all entries to the same value
  
  if (match_type == 'offset'){
    # switch sign for any additional credit from offset
    current_credit_to_update = -vals_to_match 
  } else if (match_type == 'development'){
    current_credit_to_update = vals_to_match
  }
  
  match_object = list()
  match_object$match_indexes = match_indexes
  match_object$match_vals = match_vals
  match_object$match_flag = match_flag
  match_object$current_credit = current_credit_to_update
  
  return(match_object)
  
}


sum_site_features <- function(site_features){
  parcel_sums = lapply(seq_along(site_features), function(i) do.call(sum, lapply(site_features[[i]], sum)))
  return(parcel_sums)
}


#remove site from available pool for offsets and developments. This is a two stage process to cover when offsets and developments may not overlap
update_index_object <- function(index_object, update_type, site_indexes){
  #remove site from available list
  index_object$available_indexes$offsets = setdiff(index_object$available_indexes$offsets, site_indexes)
  index_object$available_indexes$devs = setdiff(index_object$available_indexes$devs, site_indexes)
  
  
  
  if (update_type == 'offset'){
    index_object$site_indexes_used$offsets_object = append(index_object$site_indexes_used$offsets, list(site_indexes))
  } else if (update_type == 'development'){
    index_object$site_indexes_used$dev_object = append(index_object$site_indexes_used$dev_object, list(site_indexes))
  } else if (update_type == 'unregulated'){
    index_object$site_indexes_used$unregulated_loss_object = append(index_object$site_indexes_used$unregulated_loss_object, list(site_indexes))
  } else if (update_type == 'develop_from_credit'){
    index_object$site_indexes_used$credit_object = append(index_object$site_indexes_used$credit_object, list(site_indexes))
  } else if (update_type == 'banking'){
    index_object$site_indexes_used$offset_bank_object = append(index_object$site_indexes_used$offset_bank_object, list(site_indexes))
  }
  
  return(index_object)
}

#intersection routine for lists with catches on null lists
list_intersect <- function(list_a, list_b){
  list_match = list()
  vec_a <- unlist(list_a)
  vec_b <- unlist(list_b)
  
  if ( (length(vec_a) == 0) || (length(vec_b) == 0)){
    return(list_match)
  }
  
  match_ind <- which(vec_a %in% vec_b)
  match_val <- vec_a[match_ind]
  
  
  list_match$match_ind = match_ind
  list_match$match_val = match_val
  return(list_match)
}


# Determine subset of object (eg development or offset group) containing a nested set of components with the same structure
subset_current_pool <- function(pool_object, subset_pool){
  object_subset = lapply(seq_along(pool_object), function(i) pool_object[[i]][subset_pool])
  names(object_subset) <- names(pool_object)
  return(object_subset)
}


#function to work out vector of time intervals used in gains calculations
generate_time_horizons <- function(project_type, yr, intervention_yrs, time_horizon, parcel_count){
  
  if (project_type == 'current'){
    # work out time intervals from time of offset to current year
    time_horizons = rep(yr, parcel_count) - intervention_yrs
  } else if (project_type == 'future'){
    # work out time intervals from current year to projected year defined by time_horizon
    time_horizons = rep(time_horizon, parcel_count)
  } else if (project_type == 'zeros'){
    #set up dummy times
    time_horizons = rep(0, parcel_count)
  }
  return(time_horizons)
}


assess_current_pool <- function(pool_object, pool_type, features_to_use, site_features_group, feature_dynamics, management_dynamics, 
                                feature_dynamics_modes, site_element_index_key, calc_type, cfacs_flag, adjust_cfacs_flag, action_type, 
                                include_potential_developments, include_potential_offsets, include_unregulated_loss,
                                dev_probability_list, offset_probability_list, time_horizon_type, simulation_params, feature_params, time_horizon, yr){
  
  if (length(site_features_group) == 0){
    return(list())
  }
  
  feature_num = length(features_to_use)
  
  if (feature_num < length(site_features_group[[1]])){
    site_features_group = lapply(seq_along(site_features_group), function(i) site_features_group[[i]][features_to_use])
    feature_dynamics = lapply(seq_along(feature_dynamics), function(i) feature_dynamics[[i]][features_to_use])
    feature_dynamics = lapply(seq_along(feature_dynamics_modes), function(i) feature_dynamics_modes[[i]][features_to_use])
    management_dynamics = lapply(seq_along(management_dynamics), function(i) management_dynamics[[i]][features_to_use])
    site_element_index_key = lapply(seq_along(site_element_index_key), function(i) site_element_index_key[[i]][features_to_use])
  }
  
  current_condition_vals = lapply(seq_along(pool_object$parcel_sums_at_offset), function(i) pool_object$parcel_sums_at_offset[[i]][features_to_use])
  
  if (calc_type == 'current_condition') {
    projected_vals = current_condition_vals
    cfac_vals = lapply(seq_along(pool_object$parcel_sums_at_offset), function(i) matrix(0, ncol = length(features_to_use), nrow = 1))
  } else {
    time_horizons <- generate_time_horizons(project_type = 'future', yr, unlist(pool_object$intervention_yrs), time_horizon, length(site_features_group))
    
    if (cfacs_flag == TRUE){
      
      if (adjust_cfacs_flag == FALSE){
        cfac_weights = lapply(seq_along(site_features_group), function(i) lapply(seq_along(site_features_group[[i]]), function(j) array(1, time_horizons[[i]])))
      } else {
        cfac_weights = calc_cfac_weights(site_num = length(site_features_group), include_potential_developments, include_potential_offsets, include_unregulated_loss,
                                         dev_probability_list, offset_probability_list, simulation_params, feature_params, pool_object$parcel_num_remaining, time_horizons, unlist(pool_object$intervention_yrs))
      }
      
      projection_yrs = lapply(seq_along(site_features_group), 
                              function(i) find_projection_yrs(perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift, 
                                                              site_features_group[[i]],  yr, feature_dynamics[[i]],  feature_dynamics_modes[[i]], dynamics_type))
      
      if (simulation_params$use_offset_metric == FALSE){
        
        cfac_vals = lapply(seq_along(site_features_group), 
                           function(i) matrix(do.call(cbind, sum_site_features(calc_site_cfacs(site_features_group[[i]],
                                                                                               projection_yrs[[i]],
                                                                                               cfac_weights[[i]],
                                                                                               simulation_params,
                                                                                               feature_params,
                                                                                               feature_dynamics[[i]],
                                                                                               feature_dynamics_modes[[i]],
                                                                                               time_horizons[[i]],
                                                                                               include_potential_developments,
                                                                                               include_potential_offsets,
                                                                                               include_unregulated_loss,
                                                                                               adjust_cfacs_flag = simulation_params$adjust_offset_cfacs_flag,
                                                                                               time_fill = FALSE,
                                                                                               unlist_condition_classes = FALSE, 
                                                                                               site_element_index_key = vector()))), nrow = 1))
        
      } else {
        
        cfac_vals = lapply(seq_along(site_features_group), 
                           function(i) matrix(sum(user_transform_function(calc_site_cfacs(site_features_group[[i]],
                                                                                          projection_yrs[[i]],
                                                                                          cfac_weights[[i]],
                                                                                          simulation_params,
                                                                                          feature_params,
                                                                                          feature_dynamics[[i]],
                                                                                          feature_dynamics_modes[[i]],
                                                                                          time_horizons[[i]],
                                                                                          include_potential_developments,
                                                                                          include_potential_offsets,
                                                                                          include_unregulated_loss,
                                                                                          adjust_cfacs_flag = simulation_params$adjust_offset_cfacs_flag,
                                                                                          time_fill = FALSE,
                                                                                          unlist_condition_classes = TRUE, 
                                                                                          site_element_index_key[[i]]), simulation_params$transform_params)), ncol = 1))
        
      }
      
    } else if (calc_type == 'restoration_gains'){
      
      cfac_vals = current_condition_vals
    } 
    
    if (pool_type == 'offsets') {
      
      if (action_type == 'restore'){

        time_shifts = lapply(seq_along(site_features_group), 
                             function(i) find_projection_yrs(perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift, 
                                                             site_features_group[[i]], yr, management_dynamics[[i]], feature_dynamics_modes[[i]], dynamics_type))
        
        feature_dynamics = lapply(seq_along(site_features_group), function(i) shift_dynamics_set(site_features_group[[i]], 
                                                                                                 management_dynamics[[i]], 
                                                                                                 feature_dynamics_modes[[i]],
                                                                                                 dynamics_type = feature_params$management_dynamics_type, 
                                                                                                 project_by_mean = feature_params$project_by_mean,
                                                                                                 update_dynamics_by_differential = feature_params$management_update_dynamics_by_differential,
                                                                                                 time_shifts[[i]],
                                                                                                 time_fill = TRUE))
        
        projection_yrs = lapply(seq_along(site_features_group), 
                                function(i) find_projection_yrs(perform_dynamics_time_shift = FALSE, 
                                                                site_features_group[[i]],  yr = 1,  management_dynamics[[i]],  feature_dynamics_modes[[i]], dynamics_type))
        
      } 
      
      projected_feature_layers = project_features(site_features_group,
                                                  dynamics_type = feature_params$management_dynamics_type,
                                                  feature_params$management_update_dynamics_by_differential, 
                                                  feature_dynamics,
                                                  feature_dynamics_modes,
                                                  time_horizons,
                                                  perform_dynamics_time_shift = feature_params$perform_management_dynamics_time_shift,
                                                  time_fill = FALSE,
                                                  projection_yrs, 
                                                  condition_class_bounds = feature_params$condition_class_bounds)
      
      if (simulation_params$use_offset_metric == TRUE){
        
        #         projected_feature_layers <- lapply(seq_along(projected_feature_layers), 
        #                                            function(i) lapply(seq_along(projected_feature_layers[[i]]), function(j) do.call(cbind, projected_feature_layers[[i]][[j]])))
        #         
        #         projected_feature_layers <- lapply(seq_along(projected_feature_layers), 
        #                                            function(i) lapply(seq_along(projected_feature_layers[[i]]), 
        #                                                               function(j) projected_feature_layers[[i]][[j]][ site_element_index_key [[i]][[j]]]))
        projected_feature_layers = lapply(seq_along(projected_feature_layers), 
                                          function(i) lapply(seq_along(projected_feature_layers[[i]]),
                                                             function(j) unwrap_condition_classes(projected_feature_layers[[i]][[j]], 
                                                                                                  site_element_index_key[[i]][[j]])))
        
        projected_vals = lapply(seq_along(projected_feature_layers), function(i) sum(user_transform_function(projected_feature_layers[[i]], simulation_params$transform_params)))
        
      } else {
        projected_vals = lapply(seq_along(projected_feature_layers), function(i) matrix(do.call(cbind, sum_site_features(projected_feature_layers[[i]])), nrow = 1))
      }
      
    } else if (pool_type == 'developments') {
      projected_vals = lapply(seq_along(cfac_vals), function(i) matrix(0, ncol = ncol(cfac_vals[[i]]), nrow = nrow(cfac_vals[[i]])))
    }
  }
  
  if (pool_type == 'developments') {
    pool_object$parcel_vals_used = mapply('-', cfac_vals, projected_vals, SIMPLIFY = FALSE)
  } else {
    
    pool_object$parcel_vals_used = mapply('-', projected_vals, cfac_vals, SIMPLIFY = FALSE)
  }
  if (any(is.na(pool_object$parcel_vals_used))){
    browser()
    for (f_ind in seq_along(pool_object$parcel_vals_used[[1]])){
      bad_inds = which(unlist(lapply(seq_along(pool_object$parcel_vals_used), function(i) any(unlist(pool_object$parcel_vals_used[[i]][[f_ind]]) < 0))))
      
      print(unlist(lapply(bad_inds, function(i) feature_dynamics_modes[[i]][f_ind])))
    }
  }
  
  return(pool_object)
  
}




update_modes <- function(dynamics_type, current_modes, feature_layers_to_use, condition_class_bounds){
  print('mode update not working ')
  stop()
  
  if (dynamics_type == 'site_scale' ){
      feature_dynamics_modes = lapply(seq_along(feature_layers_to_use), 
                                      function(i) lapply(seq_along(feature_layers_to_use[[i]]), 
                                                         function(j) switch_current_mode(mean(as.vector(feature_layers_to_use[[i]][[j]])), 
                                                                                         condition_class_bounds[[j]], 
                                                                                         current_modes[[i]][[j]]) ))
  } else if (dynamics_type == 'element_scale'){
    feature_dynamics_modes = lapply(seq_along(feature_layers_to_use), 
                                    function(i) lapply(seq_along(feature_layers_to_use[[i]]), 
                                                       function(j) sapply(feature_layers_to_use[[i]][[j]], 
                                                                          switch_current_mode, 
                                                                          condition_class_bounds[[j]], 
                                                                          current_modes[[i]][[j]])))
  }
  
  return(feature_dynamics_modes)
}





# function to append current object characteristics to group
append_current_group <- function(object_to_append, current_object, append_routine){
  
  if (length(object_to_append) == 0){
    #append null object with same fields to maintain order
    object_to_append = vector('list', length(current_object))
    names(object_to_append) = names(current_object)
  }
  
  appended_object <- append_current_object(object_to_append,
                                           current_object,
                                           append_type = 'as_list',
                                           inds_to_append = seq_along(object_to_append))
  return(appended_object)
  
}




append_current_object <- function(parcel_set_object, current_parcel_set_object, append_type, inds_to_append){
  if (append_type == 'as_group'){
    #routine to append by characteristic for nested object
    
    parcel_set_object[inds_to_append] <- lapply(inds_to_append, function(i) append(parcel_set_object[[i]], list(current_parcel_set_object[[i]])))
  } else if (append_type == 'as_list'){
    #routine to append by characteristic for non nested object
    parcel_set_object[inds_to_append] <- lapply(inds_to_append, function(i) append(parcel_set_object[[i]], current_parcel_set_object[[i]]))
  }
  #set names of group to current object name
  names(parcel_set_object) = names(current_parcel_set_object)
  return(parcel_set_object)
}


kill_site_features <- function(site_features_to_develop, store_zeros_as_sparse){
  
  if (store_zeros_as_sparse == TRUE){
    developed_feature_layers = lapply(seq_along(site_features_to_develop), 
                                      function(i) lapply(seq_along(site_features_to_develop[[i]]),  
                                                         function(j) list(Matrix(0, do.call(sum, lapply(site_features_to_develop[[i]][[j]], length)), nrow = 1, sparse = TRUE))))
  } else {
    developed_feature_layers = lapply(seq_along(site_features_to_develop), 
                                      function(i) lapply(seq_along(site_features_to_develop[[i]]),  
                                                         function(j) list(matrix(0, length(unlist(site_features_to_develop[[i]][[j]])), nrow = 1))))
  }
  
  return(developed_feature_layers)
}


find_projection_yrs <- function(perform_dynamics_time_shift, current_site_features, yr, 
                                feature_dynamics_to_use, feature_dynamics_modes_to_use, dynamics_type){
  
  if (perform_dynamics_time_shift == FALSE){
    
    projection_yrs = lapply(seq_along(current_site_features), function(i) rep(list(yr), length(feature_dynamics_modes_to_use[[i]])) )
  } else {
    projection_yrs = find_time_shifts(current_site_features,  feature_dynamics_to_use, feature_dynamics_modes_to_use, dynamics_type)
  }
  
}



calc_site_cfacs <- function(site_features, projection_yrs, cfac_weights, simulation_params, feature_params, feature_dynamics_to_use, feature_dynamics_modes_to_use,
                            time_horizons, include_potential_developments, include_potential_offsets, include_unregulated_loss,
                            adjust_cfacs_flag, time_fill, unlist_condition_classes, site_element_index_key){
  
  if (include_potential_offsets == TRUE){
    internal_time_fill = TRUE 
  } else {
    internal_time_fill = time_fill
  }
  
  cfacs = lapply(seq_along(site_features), function(i) project_feature_layer( dynamics_type = feature_params$background_dynamics_type,
                                                                              update_dynamics_by_differential = feature_params$background_update_dynamics_by_differential, 
                                                                              site_features[[ i ]],
                                                                              feature_dynamics_to_use[[ i ]],
                                                                              feature_dynamics_modes_to_use[[ i ]],
                                                                              feature_params$condition_class_bounds[[i]],
                                                                              time_horizon = time_horizons,
                                                                              perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift,
                                                                              time_fill = internal_time_fill, 
                                                                              projection_yrs[[i]]))
  
  
  if (adjust_cfacs_flag == TRUE){
    cfacs = lapply(seq_along(cfacs), function(i) lapply(seq_along(cfacs[[i]]), 
                                                        function(j) adjust_cfacs(cfacs[[i]][[j]], cfac_weights[[i]], feature_dynamics_modes_to_use[[ i ]][[j]], time_fill)))
  } 
  
  if (unlist_condition_classes == TRUE){
    cfacs = lapply(seq_along(cfacs), function(i) unwrap_condition_classes(cfacs[[i]], site_element_index_key[[i]]))
  }
  
  return(cfacs)
  
}


adjust_cfacs <- function(current_cfac, current_cfac_weights, current_mode, time_fill){
  
  
  if (current_mode == 0){
    
    return(current_cfac)
    
  } else {
    
    if (length(current_cfac_weights) > 0){
      
      if (time_fill == TRUE){
        current_cfac = current_cfac * matrix(rep(current_cfac_weights, dim(current_cfac)[2]), nrow = dim(current_cfac)[1], byrow = FALSE)
      } else {
        current_cfac = current_cfac * tail(current_cfac_weights, 1)
      }
      
      return(current_cfac)
      
    } else {
      return(current_cfac)
    }
    
  }
  
}



calc_cfac_weights <- function(site_num, include_potential_developments, include_potential_offsets, include_unregulated_loss,
                              dev_probability_list, offset_probability_list, simulation_params, feature_params, parcel_num_remaining, time_horizons, intervention_yrs){
  
  cfac_weights = lapply(seq(site_num), function(i) lapply(seq(simulation_params$feature_num), function(j) rep(1, time_horizons[[i]])))
  
  if (include_unregulated_loss == TRUE){
    cfac_weights <- lapply(seq_along(cfac_weights), 
                           function(i) lapply(seq_along(cfac_weights[[i]]), 
                                              function(j) cfac_weights[[i]][[j]]*(1 - simulation_params$unregulated_loss_prob)^(seq(time_horizons[[i]]))))
  }
  
  if (include_potential_developments == TRUE){
    dev_probability_weights <- generate_weights(calc_type = 'development',
                                                site_intervention_probability = dev_probability_list,
                                                simulation_params$max_offset_parcel_num,
                                                simulation_params$intervention_vec,
                                                intervention_yrs,
                                                time_horizons,
                                                site_num,
                                                parcel_num_remaining,
                                                simulation_params$time_steps)
    
    cfac_weights <- lapply(seq_along(cfac_weights), 
                           function(i) lapply(seq_along(cfac_weights[[i]]), 
                                              function(j) cfac_weights[[i]][[j]] - cumsum(dev_probability_weights[[i]])))
  }
  
  if (include_potential_offsets == TRUE){
    flog.error('offset projections still in development')
    stop()
    offset_probability_weights <- generate_weights(calc_type = 'offset',
                                                   site_intervention_probability = offset_probability_list,
                                                   simulation_params$max_offset_parcel_num,
                                                   simulation_params$intervention_vec,
                                                   intervention_yrs,
                                                   time_horizons,
                                                   site_num,
                                                   parcel_num_remaining,
                                                   simulation_params$time_steps)
    
    cfac_weights <- lapply(seq_along(cfac_weights), 
                           function(i) lapply(seq_along(cfac_weights[[i]]), 
                                              function(j) cfac_weights[[i]][[j]] - cumsum(unregulated_loss_weights[[i]])))
  }
  
  
  cfac_weights <- lapply(seq_along(cfac_weights), 
                         function(i) lapply(seq_along(cfac_weights[[i]]), 
                                            function(j) cfac_weights[[i]][[j]]*(cfac_weights[[i]][[j]] >= 0)))
  
  if (include_potential_offsets == TRUE){
    flog.error('offset projections still in development')
    stop()
  }
  #     offset_intervention_probs <- remove_neg_probs(current_offset_probability_list$weighted_probs, inds_to_accept)
  #     offset_projections <- calc_offset_projections(feature_params$background_dynamics_type,
  #                                                   current_cfacs, 
  #                                                   offset_intervention_probs, 
  #                                                   simulation_params$restoration_rate, 
  #                                                   time_horizons, 
  #                                                   simulation_params$feature_num, 
  #                                                   simulation_params$min_eco_val, 
  #                                                   simulation_params$max_eco_val)
  #     
  #     summed_offset_projections <- sum_offset_projs(offset_projections,
  #                                                   offset_intervention_probs, 
  #                                                   simulation_params$feature_num, 
  #                                                   time_horizons)
  #     
  #     adjusted_cfacs = sum_clearing_offsets(adjusted_cfacs, summed_offset_projections, simulation_params$feature_num)
  
  
  inds = lapply(seq_along(cfac_weights), 
                function(i) lapply(seq_along(cfac_weights[[i]]), function(j) length(cfac_weights[[i]][[j]]) == 0))
  
  return(cfac_weights)
}

remove_neg_probs <- function(weight_list, inds_to_accept){
  weight_list <- lapply(seq_along(weight_list), function(i) weight_list[[i]]*inds_to_accept[[i]])
  return(weight_list)
}


generate_weights <- function(calc_type, site_intervention_probability, offset_intervention_scale, intervention_vec, intervention_yrs, time_horizons,
                             site_num, parcel_num_remaining, time_steps){
  
  if (calc_type == 'unregulated_loss'){
    weighted_probs <- lapply(seq_len(site_num), function(i) rep(site_intervention_probability, time_horizons[i]))    #runif(n = (time_horizon), min = 0, max = simulation_params$unregulated_loss_prob)
  } else {
    weighted_probs <- find_intervention_probability(intervention_vec,
                                                    site_intervention_probability,
                                                    intervention_yrs,
                                                    calc_type,
                                                    offset_intervention_scale,
                                                    time_horizons,
                                                    site_num,
                                                    parcel_num_remaining,
                                                    time_steps)
  }
  
  
  return(weighted_probs)
}




calc_offset_projections <- function(dynamics_type, current_cfacs, offset_probs, restoration_rate, time_horizons, feature_num, min_eco_val, max_eco_val){
  
  parcel_num = length(current_cfacs)
  offset_projections = vector('list', parcel_num)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_ind] + 1
    current_offset_probs = offset_probs[[parcel_ind]]
    current_offset_projections = generate_nested_list(outer_dim = feature_num, inner_dim = time_horizon)
    
    for (feature_ind in seq_len(feature_num)){
      
      current_cfac = current_cfacs[[parcel_ind]][[feature_ind]]
      
      for (proj_yr in seq_len(time_horizon)){
        current_offset_projections[[feature_ind]][[proj_yr]] = array(0, dim(current_cfac))
        
        if (current_offset_probs[proj_yr] > 0){
          print('offset projections not working')
          stop()
          current_offset_proj = project_feature_layer(dynamics_type, 
                                                      update_dynamics_by_differential, 
                                                      current_site_feature_layer, 
                                                      feature_dynamics_to_use, 
                                                      feature_dynamics_mode_to_use, 
                                                      current_condition_class_bounds, 
                                                      time_horizon, 
                                                      perform_dynamics_time_shift, 
                                                      time_fill, 
                                                      projection_yrs)
          
          current_offset_projections[[feature_ind]][[proj_yr]][proj_yr:time_horizon, ] = current_offset_proj 
        }
      }
    }
    offset_projections[[parcel_ind]] = current_offset_projections
  }
  
  return(offset_projections)
  
}


sum_offset_projs <- function(offset_projections, offset_probs, feature_num, time_horizons){
  parcel_num = length(offset_projections)
  summed_offset_projections = vector('list', parcel_num)
  for (parcel_ind in seq_len(parcel_num)){
    
    summed_offset_projections[[parcel_ind]] = vector('list', feature_num)
    current_offset_prob = offset_probs[[parcel_ind]]
    current_offset_prob <- current_offset_prob*(current_offset_prob > 0)
    
    current_offset_proj = offset_projections[[parcel_ind]]
    
    for (feature_ind in seq_len(feature_num)){
      current_offset_projections <- current_offset_proj[[feature_ind]]
      current_offset_projections <- lapply(seq_along(current_offset_projections), function(i) current_offset_projections[[i]]*current_offset_prob[i])
      summed_offset_projections[[parcel_ind]][[feature_ind]] = Reduce('+', current_offset_projections)
    }
  }
  
  return(summed_offset_projections)
}



sum_clearing_offsets <- function(cfacs_include_clearing, summed_offset_projections, feature_num){
  parcel_num = length(cfacs_include_clearing)
  cfacs_include_clearing_offsets = vector('list', parcel_num)
  
  for (parcel_ind in 1:parcel_num){
    cfacs_include_clearing_offsets[[parcel_ind]] = vector('list', feature_num)
  }
  
  for (parcel_ind in seq_len(parcel_num)){
    if (length(summed_offset_projections[[parcel_ind]]) > 0 ){
      for (feature_ind in seq_len(feature_num)){
        cfacs_include_clearing_offsets[[parcel_ind]][[feature_ind]] = summed_offset_projections[[parcel_ind]][[feature_ind]] + cfacs_include_clearing[[parcel_ind]][[feature_ind]]
      }
    }
  }
  return(cfacs_include_clearing_offsets)
}



sum_cols <- function(array_to_sum){
  
  if (length(dim(array_to_sum)) <= 1){
    summed_array = matrix(sum(array_to_sum), ncol = 1)
  } else if (length(dim(array_to_sum)) == 2){
    summed_array = matrix(apply(array_to_sum, MARGIN = 1, sum), ncol = 1)
  } else if (length(dim(array_to_sum)) == 3){
    summed_array = apply(array_to_sum, MARGIN = c(1, 3), sum)
    dim(summed_array) = c(dim(summed_array), 1)
    summed_array = aperm(summed_array, c(1, 3, 2))
  }
  return(summed_array)
}


