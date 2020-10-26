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
osim.run <- function(user_global_params = NULL, user_simulation_params = NULL, user_feature_params = NULL, user_transform_function = NULL, loglevel = WARN){
  
  flog.threshold(loglevel)
  flog.info('starting offsetsim')
  
  # Loop over all defined scenarios if only a subset of the scenarios is to be
  # reun (as defined by global_params$scenario_subset) then only
  # run these. By default global_params$scenario_subset
  
  #load all necessary GIS layers, objects from files etc
  simulation_params_group <- build_simulation_params_group(user_simulation_params, user_global_params$features_to_use_in_simulation)
  global_input_data <- build_input_data(user_global_params, user_feature_params, user_transform_function, simulation_params_group)
  
  # Write initial logging info
  flog.info('Running %s scenarios with %s realisations on %s cores', 
            length(simulation_params_group),  
            global_input_data$global_params$realisation_num,
            global_input_data$global_params$number_of_cores ) 
  
  for (scenario_ind in global_input_data$global_params$scenario_subset){
    
    # Store the start time
    sim_time <- Sys.time()
    
    output_data_template = build_output_data(global_input_data, simulation_params_group[[scenario_ind]])
    
    current_background_cfacs_object = build_cfacs_routines(global_input_data, simulation_params_group[[scenario_ind]])
    
    flog.info('running scenario %s of %s, with %s iterations',  
              scenario_ind, 
              length(simulation_params_group),
              global_input_data$global_params$time_steps)
    
    flog.info('offsetting using %s impact calculation and %s year time horizon for gain/loss calculations',  
              simulation_params_group[[scenario_ind]]$offset_calc_type, 
              simulation_params_group[[scenario_ind]]$offset_action_type, 
              simulation_params_group[[scenario_ind]]$offset_time_horizon)
    
    flog.info('system comprised of %s x %s elements and %s sites',
              global_input_data$site_characteristics$landscape_dims[1], 
              global_input_data$site_characteristics$landscape_dims[2],
              length(global_input_data$site_characteristics$cell_id_groups))
    
    flog.info('%s potential development sites, %s potential offset sites',
              length(output_data_template$index_object$available_indexes$developments), 
              length(output_data_template$index_object$available_indexes$offsets)) 
    
    flog.info('developing (and offsetting) %s sites', sum(output_data_template$index_object$intervention_control)) 
    
    flog.info('estimated %s sites lost to unregulated clearing', 
              round(estimate_illegal_sites(simulation_params_group[[scenario_ind]]$unregulated_loss_prob, 
                                           global_input_data$global_params$time_steps, 
                                           length(global_input_data$site_characteristics$cell_id_groups)))) 
    
    ##### TODO(Isaac) test whether can set check and set seed if necessary much earlier in the code before run_initialise_routines() is called
    if (global_input_data$global_params$number_of_cores > 1 && global_input_data$global_params$set_seed == TRUE){
      # case when running DETERMINISTIC realisations in parallel
      # doRNG needed to get deterministic foreach loops, dsingh 24/nov/17
      flog.info('will use doRNG with seed %s to get determinisitc parallel runs', 123)
      registerDoRNG(123) 
      
      # foreach runs realizations in parallel
      foreach(realisation_ind = seq_len(global_input_data$global_params$realisation_num)) %dorng%{
        run_offset_simulation_routines(global_input_data, 
                                       simulation_params_group[[scenario_ind]],  
                                       output_data_template, 
                                       current_background_cfacs_object, 
                                       scenario_ind, 
                                       realisation_ind)
      }
    } else if ((global_input_data$global_params$number_of_cores > 1) && (global_input_data$global_params$realisation_num > 1)){
      # case when running NON-DETERMINISTIC realisations in parallel
      foreach(realisation_ind = seq_len(global_input_data$global_params$realisation_num)) %dopar%{
        run_offset_simulation_routines(global_input_data, 
                                       simulation_params_group[[scenario_ind]],  
                                       output_data_template, 
                                       current_background_cfacs_object, 
                                       scenario_ind, 
                                       realisation_ind)
      }
    } else {
      # Case when running single realisation
      ##### TODO(Isaac): need to add case for running a single realization either with or without having the seed set.
      for (realisation_ind in 1:global_input_data$global_params$realisation_num){
        run_offset_simulation_routines(global_input_data, 
                                       simulation_params_group[[scenario_ind]],  
                                       output_data_template, 
                                       current_background_cfacs_object, 
                                       scenario_ind, 
                                       realisation_ind)
      }
      
    }
    
    flog.info('scenario %s simulation outputs generated for %s realisations in %s %s', 
              scenario_ind,
              global_input_data$global_params$realisation_num,
              round(difftime(Sys.time(), sim_time), 1), 
              units(difftime(Sys.time(), sim_time)))
  }
  
  
  
  #   for (scenario_ind in global_input_data$global_params$scenario_subset){
  #     
  # 
  #     if ((global_input_data$global_params$number_of_cores > 1) && (global_input_data$global_params$realisation_num > 1) &&
  #         (global_input_data$global_params$collate_with_parallel_cores == TRUE)){
  # 
  #       foreach(realisation_ind = seq_len(global_input_data$global_params$realisation_num)) %dopar%{
  #         collate_simulation_outputs(global_input_data, simulation_params_group[[scenario_ind]],  background_cfacs_object, scenario_ind, realisation_ind)
  #       }
  #     } else {
  #       # Case when running single realisation
  # 
  #       for (realisation_ind in 1:global_input_data$global_params$realisation_num){
  #         collate_simulation_outputs(global_input_data, simulation_params_group[[scenario_ind]],  background_cfacs_object, scenario_ind, realisation_ind)
  #       }
  #       
  #     }
  #     
  #     flog.info('scenario %s collated and completed in %s %s', 
  #               scenario_ind,
  #               round(difftime(Sys.time(), sim_time), 1), 
  #               units(difftime(Sys.time(), sim_time)))
  #   }
  #   
  
  
  if (global_input_data$global_params$save_simulation_outputs == FALSE){
    # This deletes all folders and subfolders that were created in the run
    # process that are no longer needed
    unlink(global_input_data$global_params$output_folder, recursive = TRUE)
  }
  
  flog.info('all scenarios done in %s %s', 
            round(difftime(Sys.time(), global_input_data$global_params$strt), 1), 
            units(difftime(Sys.time(), global_input_data$global_params$strt)))
  
  flog.info('all outputs written into %s', global_input_data$global_params$run_folder)
  
  # Clean up the parallel processes if there was more than one
  parallel::stopCluster(global_input_data$global_params$clstr)
}


run_offset_simulation_routines <- function(global_input_data, simulation_params, output_data, current_background_cfacs_object, scenario_ind, realisation_ind){  
  
  current_data_dir = write_folder(paste0(global_input_data$global_params$output_folder, 
                                         'scenario_', formatC(scenario_ind, width = global_input_data$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
                                         '/realisation_', formatC(realisation_ind, width = global_input_data$global_params$numeric_placeholder_width, format = "d", flag = "0"), '/'))
  
  save_landscape_routine(global_input_data, simulation_params, current_data_dir, yr = 0)
  
  flog.info('current data dir is %s', current_data_dir)
  
  global_input_data$credit_object <- build_initial_credit(simulation_params, global_input_data)
  global_input_data$output_data <- output_data
  simulation_outputs <- run_simulation(global_input_data, simulation_params, current_data_dir)
  
  # save raw simulation data
  
  saveRDS(simulation_outputs, paste0(current_data_dir, 'realisation_',
                                     formatC(realisation_ind, width = global_input_data$global_params$numeric_placeholder_width, format = "d", flag = "0"),
                                     '_outputs.rds'))
  
  flog.info('running collate routines')
  collate_simulation_outputs(global_input_data, simulation_params, current_background_cfacs_object, scenario_ind, realisation_ind)
  
  # delete current temporary files and folder
  if (global_input_data$global_params$save_simulation_outputs == FALSE){
    unlink(current_data_dir, recursive = TRUE)
  }
  
}




# main engine for code - returns all simulation outputs including developments, offsets etc.
run_simulation <- function(simulation_data_object, simulation_params, current_data_dir){
  
  #run through main time loop
  for (yr in seq_len(simulation_data_object$global_params$time_steps)){
    
    t1 = Sys.time()
    
    flog.info('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    flog.info('t = %s', yr) 
    flog.info('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    
    flog.info('program composed of %s development and %s offset sites',
              sum(length(unlist(simulation_data_object$output_data$index_object$site_indexes_used$development_object)), 
                  length(unlist(simulation_data_object$output_data$index_object$site_indexes_used$uncoupled_development_object))),
              sum(length(unlist(simulation_data_object$output_data$index_object$site_indexes_used$offset_object)), 
                  length(unlist(simulation_data_object$output_data$index_object$site_indexes_used$uncoupled_offset_object))))
    
    #when running in uncoupled offset mode select out current set of sites to add
    
    if (simulation_params$use_uncoupled_offsets == TRUE){
      simulation_data_object <- run_uncoupled_offset_routine(simulation_data_object, simulation_params, yr)
    }
    
    if (simulation_data_object$output_data$index_object$intervention_control[yr] > 0){
      
      # determine current set of available offset sites and calculate gains structure as detailed in current policy params
      simulation_data_object$offset_pool_object <- build_intervention_pool(simulation_data_object, 
                                                                           simulation_params, 
                                                                           pool_type = 'offsets',
                                                                           current_pool = simulation_data_object$output_data$index_object$available_indexes$offsets,
                                                                           yr)
      
      # determine current set of available development sites and calculate gains structure as detailed in current policy params
      
      if (simulation_params$development_selection_type == 'pre_determined') {
        simulation_data_object$dev_pool_object <- build_intervention_pool(simulation_data_object, 
                                                                          simulation_params,
                                                                          pool_type = 'developments',
                                                                          current_pool = setdiff(simulation_data_object$output_data$index_object$development_control[[yr]], 
                                                                                                 simulation_data_object$output_data$index_object$site_indexes_used),
                                                                          yr)
      } else {
        simulation_data_object$dev_pool_object <- build_intervention_pool(simulation_data_object, 
                                                                          simulation_params, 
                                                                          pool_type = 'developments',
                                                                          current_pool = simulation_data_object$output_data$index_object$available_indexes$developments,
                                                                          yr)
      }
      
      t2 = Sys.time()
      print(paste('development pool in', t2 - t1))
      t1 = t2
    }
    
    for (development_counter in seq_len(simulation_data_object$output_data$index_object$intervention_control[yr])){
      
      ###### TODO REINSTATE OFFSETBANK ROUTINES ####
      #           if (simulation_params$use_uncoupled_offsets == TRUE){
      #             simulation_data_object$credit_object$current_credit = assess_banking_credit(simulation_data_object$output_data, simulation_params)
      #           }
      
      # attempt to develop from current available credit
      
      if (simulation_params$develop_from_credit == TRUE){
        simulation_data_object <- develop_from_credit_routine(simulation_data_object, simulation_params, yr)
      } 
      
      #if insufficient credits accumulated to allow development attempt development with offset match.
      if (simulation_data_object$credit_object$match_flag == FALSE && simulation_params$use_site_sets == TRUE){
        simulation_data_object <- run_development_offset_routines(simulation_data_object, simulation_params, yr)
      }
    }
    
    t2 = Sys.time()
    print(paste('development routine in', t2 - t1))
    t2 = t1
    
    
    if (!( (simulation_params$unregulated_loss_type == 'default') & (simulation_params$unregulated_loss_prob == 0) ) ){
      simulation_data_object <- run_unregulated_loss_routine(simulation_data_object, simulation_params, yr)
    }
    
    flog.info('%s available development sites, %s available offset sites',
              sum(length(unlist(simulation_data_object$output_data$index_object$available_indexes$developments))),
              sum(length(unlist(simulation_data_object$output_data$index_object$available_indexes$offsets))))
    
    output_current_intervention_block_stats(simulation_data_object$output_data$interventions$uncoupled_development_object, 
                                            'credit',
                                            simulation_data_object$site_characteristics$site_IDs, 
                                            yr)
    output_current_intervention_block_stats(simulation_data_object$output_data$interventions$development_object, 
                                            'development',
                                            simulation_data_object$site_characteristics$site_IDs, 
                                            yr)
    output_current_intervention_block_stats(simulation_data_object$output_data$interventions$offset_object, 
                                            'offset',
                                            simulation_data_object$site_characteristics$site_IDs, 
                                            yr)
    
    # update sites in landscape (both inside and outside development/offset program)
    flog.info('updating sites...')
    
    t0 = Sys.time()

    simulation_data_object$site_scale_features = run_projection_routines(simulation_data_object$site_scale_features,
                                                                         simulation_data_object$feature_dynamics,
                                                                         simulation_data_object$feature_dynamics_modes,
                                                                         simulation_data_object$mode_characteristics,
                                                                         condition_class_characteristics = simulation_data_object$feature_params$condition_class_characteristics,
                                                                         dynamics_update_type = 'dynamic',
                                                                         projection_yr = yr,
                                                                         time_horizon = 1,
                                                                         collapse_features = FALSE,
                                                                         adjust_cfacs_flag = FALSE,
                                                                         cfac_weights = FALSE,
                                                                         simulation_data_object$site_characteristics$cell_num,
                                                                         break_flag = FALSE)
    
    flog.info(paste('landscape projection in', Sys.time() - t0))
    
    save_landscape_routine(simulation_data_object, simulation_params, current_data_dir, yr)
    
  }
  
  return(simulation_data_object$output_data)
  
}

output_current_intervention_block_stats <- function(current_intervention_object, intervention_type, site_IDs, yr){
  
  if (length(current_intervention_object$intervention_yrs) == 0){
    return(NULL)
  }
  current_block = which(unlist(lapply(seq_along(current_intervention_object$intervention_yrs), 
                                      function(i) all(current_intervention_object$intervention_yrs[[i]] == yr))))
  
  if (length(current_block) > 0){
    
    current_group = unlist(current_intervention_object$site_indexes[current_block])
    
    if (intervention_type == 'offset'){
      intervention_sum = lapply(current_block, function(i) apply(current_intervention_object$site_score[[i]], MARGIN = 2, 'sum'))
      intervention_sum = Reduce('+', intervention_sum)
    } else {
      intervention_sum = Reduce('+', current_intervention_object$site_score[current_block])
    }
    
    if (intervention_type == 'offset'){
      intervention_string = 'offset'
    } else if (intervention_type == 'development'){
      intervention_string = 'developed'
    } else if (intervention_type == 'credit'){
      intervention_string = 'credit used to develop'
    }
    
    if (length(current_block) >= 5 ){
      flog.info(cat(intervention_string, length(current_group), 
                    'sites, net value =', intervention_sum, '\n'))
    } else {
      
      flog.info(cat(intervention_string, length(current_group), 
                    'sites, with IDs', paste(site_IDs[current_group]), 
                    ', net value =', intervention_sum, '\n'))
    }
  }
  
}

save_landscape_routine <- function(simulation_data_object, simulation_params, current_data_dir, yr){

  yr_string = paste0('yr_', formatC(yr, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"))
  
  saveRDS(simulation_data_object$site_scale_features, paste0(current_data_dir, 'feature_outputs_', yr_string, '.rds'))
  
  #  saveRDS(simulation_data_object$feature_dynamics_modes, paste0(current_data_dir, 'feature_dynamics_modes_', yr_string, '.rds'))
  
  #   if (simulation_params$use_offset_metric == TRUE){
  #     
  #     
  #     feature_set_to_save = lapply(seq_along(simulation_data_object$site_scale_features), 
  #                                  function(i) simulation_data_object$global_params$user_transform_function(lapply(seq_along(simulation_data_object$site_scale_features[[i]]),
  #                                                                                                                  function(j) unwrap_feature_dynamics_modes(array(0, simulation_data_object$site_characteristics$cell_num[[i]]), 
  #                                                                                                                                                       simulation_data_object$site_scale_features[[i]][[j]], 
  #                                                                                                                                                       simulation_data_object$site_scale_condition_class_key[[i]][[j]])), 
  #                                                                                                           simulation_params$transform_params))
  # 
  #     saveRDS(feature_set_to_save,  paste0(current_data_dir, 'feature_outputs_user_metric_', yr_string, '.rds'))
  #     
  #   }
  
  #   for (feature_ind in seq_along(simulation_data_object$global_params$features_to_use_in_simulation)){
  # 
  #     file_prefix = paste0('feature_', formatC(simulation_data_object$global_params$features_to_use_in_simulation[feature_ind], width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), 
  #                          '_yr_', formatC(yr, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"))
  #     
  #     feature_set_to_save = lapply(seq_along(simulation_data_object$site_scale_features), 
  #                                  function(i) simulation_data_object$site_scale_features[[i]][[feature_ind]])
  #     
  #     saveRDS(feature_set_to_save, paste0(current_data_dir, file_prefix, '.rds'))
  #     
  #     if (simulation_data_object$global_params$output_raster_tiff == TRUE){
  #       
  #       if (!file.exists(paste0(current_data_dir, 'raster_folder/'))){
  #         dir.create(paste0(current_data_dir, 'raster_folder/'))
  #       }
  #       
  #       feature_set_to_output = lapply(seq_along(simulation_data_object$site_scale_features), 
  #                                    function(i) unwrap_feature_dynamics_modes(array(0, simulation_data_object$site_characteristics$cell_num[[i]]), 
  #                                                                         feature_set_to_save[[i]], 
  #                                                                         simulation_data_object$site_scale_condition_class_key[[i]][[feature_ind]]))
  #       
  #       feature_layer_to_output = matrix(0, nrow = simulation_data_object$site_characteristics$landscape_dims[1], ncol = simulation_data_object$site_characteristics$landscape_dims[2])
  #       feature_layer_to_output[unlist(simulation_data_object$site_characteristics$cell_id_groups)] = unlist(feature_set_to_save)
  #       feature_layer_to_output = raster(feature_layer_to_output)
  #       writeRaster(feature_layer_to_output, paste0(current_data_dir, 'raster_folder/', file_prefix, simulation_data_object$global_params$raster_file_type))
  #       
  #     }
  #     
  #   }
  #   
  #   if (simulation_params$use_offset_metric == TRUE){
  # 
  #     feature_set_to_save = lapply(seq_along(simulation_data_object$site_scale_features), 
  #                                    function(i) simulation_data_object$global_params$user_transform_function(lapply(seq_along(simulation_data_object$site_scale_features[[i]]),
  #                                                                                                                    function(j) unwrap_feature_dynamics_modes(array(0, simulation_data_object$site_characteristics$cell_num[[i]]), 
  #                                                                                                                                                         simulation_data_object$site_scale_features[[i]][[j]], 
  #                                                                                                                                                         simulation_data_object$site_scale_condition_class_key[[i]][[j]])), 
  #                                                                                                             simulation_params$transform_params))
  #     saveRDS(feature_set_to_save, paste0(current_data_dir, 'metric_layer', '_yr_', formatC(yr, width = simulation_data_object$global_params$numeric_placeholder_width, format = "d", flag = "0"), '.rds'))
  #     
  #   }
  
}

run_development_offset_routines <- function(simulation_data_object, simulation_params, yr){
  #perform the matching routine - i.e. find a matching development/offset set 
  
  output_match_object <- match_development_to_offset(simulation_data_object, simulation_params, match_type = 'develop_with_offset_pool', yr) 
  
  # if a match was found record current development and associated offsets and update site parameters.
  
  if (output_match_object$match_flag == TRUE){
    
    simulation_data_object$credit_object$current_credit = output_match_object$current_credit
    
    # remove selected offset sites from available pool, save offset site characteristics, update decline rates to implement offset.
    simulation_data_object <- run_offset_routines(simulation_data_object,
                                                  simulation_params, 
                                                  current_offset_object = output_match_object$offset_object,
                                                  yr)
    
    # remove selected development sites from available pool, save development site characteristics,
    # update decline rates to implement development loss.
    
    simulation_data_object <- run_clearing_routines(simulation_data_object,
                                                    simulation_params,
                                                    current_development_object = output_match_object$development_object,
                                                    clearing_type = 'development',
                                                    yr)
    
    flog.info(cat('developed site', paste(simulation_data_object$site_characteristics$site_IDs[output_match_object$development_object$site_indexes]),
                  'with value', paste( round(colSums(output_match_object$development_object$site_score), 1)),
                  ', offset with sites', paste(simulation_data_object$site_characteristics$site_IDs[output_match_object$offset_object$site_indexes]), 
                  'with net value', paste(round(colSums(output_match_object$offset_object$site_score), 1)), '\n'))
    
  }
  
  return(simulation_data_object)
}





develop_from_credit_routine <- function(simulation_data_object, simulation_params, yr){
  
  flog.info(cat('remaining credit = ', as.vector(simulation_data_object$credit_object$current_credit), '\n'))
  
  if (all(simulation_data_object$credit_object$current_credit <= 0) | (length(simulation_data_object$dev_pool_object$site_indexes) == 0)){
    output_match_object = setNames(list(FALSE), 'match_flag')
    
  } else {
    output_match_object <- run_match_routine(match_type = 'development', 
                                             simulation_data_object$dev_pool_object$site_indexes, 
                                             simulation_data_object$dev_pool_object$site_score, 
                                             simulation_data_object$credit_object$current_credit,
                                             simulation_data_object$dev_probability_list,
                                             simulation_data_object$credit_object$current_credit,
                                             simulation_params,
                                             yr)
    
    if (output_match_object$match_flag == TRUE){
      
      simulation_data_object$credit_object$current_credit = output_match_object$current_credit
      
      subset_pool = list_intersect(simulation_data_object$dev_pool_object$site_indexes, output_match_object$match_indexes)
      development_object = subset_current_pool(simulation_data_object$dev_pool_object, subset_pool = subset_pool$match_ind)
      simulation_data_object <- run_clearing_routines(simulation_data_object,
                                                      simulation_params,
                                                      development_object,
                                                      clearing_type = 'develop_from_credit',
                                                      yr)
      
      flog.info(cat('developed site', paste(simulation_data_object$site_characteristics$site_IDs[unlist(development_object$site_indexes)]),
                    'with value', paste(lapply(development_object$site_score, round, 2)), 'from credit,',
                    'remaining =', paste(lapply(output_match_object$current_credit, round, 2)), '\n'))
      
    }
  } 
  
  simulation_data_object$credit_object$match_flag = output_match_object$match_flag
  
  return(simulation_data_object)
}



#sample over uniform random vector, indicies less than the threshold level are selected for clearing
select_sites_to_clear <- function(available_site_indexes, simulation_params, yr){
  
  if (simulation_params$unregulated_loss_type == 'default'){
    clearing_thresh <- rep(simulation_params$unregulated_loss_prob, length(available_site_indexes))
    discrim <- runif(length(clearing_thresh)) < clearing_thresh
    inds_to_clear <- available_site_indexes[discrim]
    
    # use this to set the unregulated losses as development without offsets
  } 
  
  return(inds_to_clear)
}



run_unregulated_loss_routine <- function(simulation_data_object, simulation_params, yr){
  
  available_site_indexes = setdiff(unlist(simulation_data_object$output_data$index_object$available_indexes$unregulated_loss), 
                                   unlist(simulation_data_object$output_data$index_object$site_indexes_used))
  
  inds_to_clear <- select_sites_to_clear(available_site_indexes, simulation_params, yr)
  
  if (length(inds_to_clear) == 0){ #return null for no sites selected for clearing
    return(simulation_data_object)
  } 
  # store group of site characteristics in site characteristics object
  unregulated_loss_object <- record_site_characteristics(simulation_data_object$site_scale_features[inds_to_clear],
                                                         inds_to_clear,
                                                         site_num_remaining = length(available_site_indexes),
                                                         yr)
  
  if (length(inds_to_clear) <= 5){
    flog.info(cat('unregulated loss of ', length(inds_to_clear), ' sites ' ,
                  ' with site IDs ', paste(simulation_data_object$site_characteristics$site_IDs[inds_to_clear]), 
                  ', net value ', apply(unregulated_loss_object$site_sums_at_offset, 2, 'sum'), '\n'))
    
  } else {
    flog.info(cat('unregulated loss of ', length(inds_to_clear), ' sites ' ,
                  ' with net value ', apply(unregulated_loss_object$site_sums_at_offset, 2, 'sum'), '\n'))
  }
  
  current_pool = unlist(unregulated_loss_object$site_indexes)
  # record characteristics of cleared site
  unregulated_loss_object <- assess_current_pool(pool_object = unregulated_loss_object,
                                                 pool_type = 'developments',
                                                 features_to_use = seq_along(simulation_data_object$global_params$features_to_use_in_simulation),
                                                 site_scale_features = simulation_data_object$site_scale_features[current_pool],
                                                 feature_dynamics = simulation_data_object$feature_dynamics,
                                                 management_dynamics = simulation_data_object$management_dynamics,
                                                 mode_characteristics = simulation_data_object$mode_charateristics, 
                                                 feature_dynamics_modes = simulation_data_object$feature_dynamics_modes[current_pool],
                                                 cell_num = simulation_data_object$site_characteristics$cell_num[current_pool],
                                                 calc_type = simulation_params$dev_calc_type,
                                                 cfacs_flag = simulation_params$dev_cfacs_flag,
                                                 adjust_cfacs_flag = simulation_params$adjust_dev_cfacs_flag,
                                                 action_type = simulation_params$offset_action_type,
                                                 include_potential_developments = simulation_params$include_potential_developments_in_dev_calc,
                                                 include_potential_offsets = simulation_params$include_potential_offsets_in_dev_calc,
                                                 include_unregulated_loss = simulation_params$include_unregulated_loss_in_dev_calc,
                                                 recalculate_probabilities(simulation_data_object$dev_probability_list[current_pool]), 
                                                 recalculate_probabilities(simulation_data_object$offset_probability_list[current_pool]), 
                                                 time_horizon_type = 'future',
                                                 simulation_params,
                                                 simulation_data_object$feature_params,
                                                 simulation_params$offset_time_horizon,
                                                 yr, 
                                                 simulation_data_object$global_params$user_transform_function)  
  
  # remove selected sites from available pool, save site characteristics, update decline rates to implement loss.
  if (!is.null(unregulated_loss_object)){
    simulation_data_object <- run_clearing_routines(simulation_data_object,
                                                    simulation_params, 
                                                    current_development_object = unregulated_loss_object,
                                                    clearing_type = 'unregulated_loss',
                                                    yr)
  }
  
  return(simulation_data_object)
}


# update_feature_dynamics_modes <- function(feature_dynamics_modes, feature_num, features_to_update, action_type, current_pool){
#   
#   for (current_site_index in current_pool){
#     
#     if (action_type == 'development'){
#       feature_dynamics_modes[[current_site_index]] <- rep(list(0), feature_num)
#     } else if (action_type == 'maintain'){
#       browser()
#       feature_dynamics_modes[[current_site_index]][features_to_update] = lapply(seq_along(features_to_update), 
#                                                                                       function(i) rep(0, length(feature_dynamics_modes[[current_site_index]])))
#     } 
#   }
#   
#   return(feature_dynamics_modes)
#   
# }


shift_feature_dynamics <- function(current_feature_val, dynamics_to_update, feature_params, features_to_update, yr){
  
  shifted_dynamics <- shift_dynamics(current_feature_val, dynamics_to_update)
  
  updated_feature_dynamics = Matrix(0, nrow = 1, ncol = length(dynamics_to_update), sparse = TRUE)

  if (length(shifted_dynamics) > 0){
    
    cutoff = min(length(shifted_dynamics), (length(dynamics_to_update) - yr))
    updated_feature_dynamics[, yr:(yr + cutoff - 1)] <- shifted_dynamics[1:cutoff]
    
  }
  
  return(updated_feature_dynamics)
  
}


shift_dynamics <- function(current_feature_val, dynamics_to_update){
  
  current_time_shift <- find_time_shift(current_feature_val, dynamics_to_update)
  
  if (current_time_shift > 0){
    
    time_vec = current_time_shift + 0:(length(dynamics_to_update) - ceiling(current_time_shift))
    updated_dynamics = approx(1:length(dynamics_to_update), dynamics_to_update, time_vec)$y
    
  } 
  
  updated_dynamics = diff(updated_dynamics)
  
  return(updated_dynamics)
}


find_time_shift <- function(current_feature_val, current_feature_dynamics){  
  
  min_loc = which(current_feature_dynamics == min(current_feature_dynamics))[1]
  
  if (current_feature_val == current_feature_dynamics[1]){
    time_shift = 1
  } else {
    intervals = findInterval(current_feature_dynamics, current_feature_val)
    peak_loc = which( diff(diff(current_feature_dynamics) >= 0) < 0) + 1
    
    if (length(peak_loc) > 0){
      peak_loc = which(current_feature_dynamics == max(current_feature_dynamics[peak_loc]))[1]
    } else {
      peak_loc = which(current_feature_dynamics == max(current_feature_dynamics))[1]
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
        time_shift = approx(current_feature_dynamics[lower_bound:(lower_bound + 1)], lower_bound:(lower_bound + 1), current_feature_val)$y
      }
      
    }
    
  }
  
  return(time_shift)
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

# series of routines to implement offset
run_offset_routines <- function(simulation_data_object, simulation_params, current_offset_object, yr){
  
  # if running in uncoupled mode remove offset site from available set
  if (simulation_params$use_uncoupled_offsets == TRUE){
    
    uncoupled_offset_pool = simulation_data_object$output_data$interventions$uncoupled_offset_object$site_indexes
    
    # determine parcels used in matching routine and remove from available pool
    simulation_data_object$output_data$interventions$uncoupled_offset_object$site_indexes = remove_sites_from_pool(uncoupled_offset_pool, current_offset_object$site_indexes)
    
  } else {
    # determine parcels used in matching routine and remove from available pool
    
    simulation_data_object$output_data$index_object <- update_index_object(simulation_data_object$output_data$index_object, 
                                                                           update_type = 'offset', 
                                                                           current_offset_object$site_indexes)
  }
  
  #record current offset site characteristics
  
  simulation_data_object$output_data$interventions$offset_object <- append_current_group(simulation_data_object$output_data$interventions$offset_object, 
                                                                                         current_offset_object, 
                                                                                         append_routine = 'matrix')
  
  current_pool <- unlist(current_offset_object$site_indexes)
  
  updated_dynamics_object <- update_feature_dynamics(simulation_data_object$feature_dynamics,
                                                     simulation_data_object$management_dynamics,
                                                     simulation_data_object$feature_dynamics_modes[current_pool], 
                                                     simulation_data_object$site_scale_features[current_pool],
                                                     simulation_params$features_to_offset,
                                                     action_type = 'offset', 
                                                     simulation_data_object$background_mode_num_total,
                                                     simulation_data_object$feature_params,
                                                     yr)

  simulation_data_object$feature_dynamics[updated_dynamics_object$modes_to_update + simulation_data_object$background_mode_num_total, ] = updated_dynamics_object$feature_dynamics
  simulation_data_object$feature_dynamics_modes[current_pool] = updated_dynamics_object$feature_dynamics_modes
  
  #remove offset sites from available pool
  if (length(current_offset_object$site_indexes) > 0){
    simulation_data_object$offset_pool_object <- remove_sites_from_pool(simulation_data_object$offset_pool_object, current_pool)
    simulation_data_object$dev_pool_object <- remove_sites_from_pool(simulation_data_object$dev_pool_object, current_pool)
  }
  
  return(simulation_data_object)
}

#simulation_data_object$feature_dynamics[modes_to_update[dynamic_modes] + simulation_data_object$background_mode_num_total, ]
#simulation_data_object$background_mode_num_total

update_feature_dynamics <- function(feature_dynamics, management_dynamics, feature_dynamics_modes, site_scale_features, features_to_update, 
                                      action_type, background_mode_num, feature_params, yr){
    
  output_dynamics_object = list()
  output_dynamics_object$feature_dynamics = vector('list', length(site_scale_features))
  output_dynamics_object$modes_to_update = vector('list', length(site_scale_features))
  output_dynamics_object$feature_dynamics_modes <- feature_dynamics_modes
  
  for (site_ind in seq_along(site_scale_features)){
    
    if (action_type == 'development'){
      
      updated_mode_block = Matrix(0, nrow = nrow(feature_dynamics_modes[[ site_ind ]]), ncol = length(features_to_update))
      
    } else if (action_type == 'offset'){

      updated_mode_block = as.matrix(feature_dynamics_modes[[ site_ind ]][, features_to_update, drop = FALSE])
      
      mode_IDs_to_update = updated_mode_block > 0
      modes_to_update = unique(updated_mode_block[mode_IDs_to_update])
      
      dynamics_to_update <- management_dynamics[modes_to_update, , drop = FALSE]
      
      dynamics_set_to_update <- rowSums(dynamics_to_update) > 0
      
      dynamic_modes <- which(dynamics_set_to_update)
      
      if (length(dynamic_modes) > 0){
        
        output_dynamics_object$modes_to_update[[site_ind]] = modes_to_update[dynamic_modes]
        
        feature_block = as.vector(site_scale_features[[ site_ind ]][, features_to_update])
        
        feature_means = lapply(dynamic_modes, 
                               function(i) mean(feature_block[which(updated_mode_block == modes_to_update[i])]))
        
        updated_dynamics <- lapply(seq_along(feature_means), 
                                   function(i) shift_feature_dynamics(feature_means[[i]], 
                                                                      dynamics_to_update[dynamic_modes[i], , drop = FALSE],
                                                                      feature_params, 
                                                                      features_to_update, 
                                                                      yr))
        
        output_dynamics_object$feature_dynamics[[site_ind]] = do.call(rbind, updated_dynamics)
        
        dynamic_mode_block <- updated_mode_block %in% modes_to_update[dynamic_modes]
        
        updated_mode_block[dynamic_mode_block] = updated_mode_block[dynamic_mode_block] + rep(background_mode_num, length(which(dynamic_mode_block))) 
        
      } 
      
      static_modes <- which(!dynamics_set_to_update)
      
      if (length(static_modes) > 0){
        browser()
        updated_mode_block[updated_mode_block %in% modes_to_update[static_modes]] <- 0
      }

    } 
    
    output_dynamics_object$feature_dynamics_modes[[site_ind]][, features_to_update] = updated_mode_block
  }
  
  output_dynamics_object$modes_to_update <- do.call(c, output_dynamics_object$modes_to_update)
  output_dynamics_object$feature_dynamics <- do.call(rbind, output_dynamics_object$feature_dynamics)
  
  return(output_dynamics_object)
}





#########  TODO  COMBINE WITH ABOVE OFFSET ROUTINE

run_uncoupled_offset_routine <- function(simulation_data_object, simulation_params, yr){
  
  # how many offsets to be added in current year
  
  if (simulation_params$uncoupled_offset_selection_type == 'stochastic'){
    uncoupled_num = min(simulation_data_object$output_data$index_object$uncoupled_offset_control[[yr]], 
                        length(simulation_data_object$output_data$index_object$available_indexes$offsets))
    
  } else {
    uncoupled_num = length(which(simulation_data_object$output_data$index_object$uncoupled_offset_control[[yr]] > 0))
  }
  
  if (uncoupled_num == 0){
    return(simulation_data_object)
  }
  
  # select current number of offset sites from current available pool to add to uncoupled offset pool
  
  if (simulation_params$uncoupled_offset_selection_type == 'stochastic'){
    current_pool <- sample(simulation_data_object$output_data$index_object$available_indexes$offsets, uncoupled_num)
  } else {
    current_pool = setdiff(simulation_data_object$output_data$index_object$uncoupled_offset_control[[yr]], 
                           unlist(simulation_data_object$output_data$index_object$site_indexes_used))
  }
  
  # number of sites to potentially offset
  site_num_remaining = length(simulation_data_object$output_data$index_object$available_indexes$offsets)
  
  # record current offset pool characteristics
  current_uncoupled_object <- record_site_characteristics(simulation_data_object$site_scale_features[current_pool],
                                                          current_pool,
                                                          site_num_remaining,
                                                          yr)   # arrange current parcel data
  
  simulation_data_object$output_data$interventions$uncoupled_offset_object = append_current_group(object_to_append = simulation_data_object$output_data$interventions$uncoupled_offset_object,
                                                                                                  current_object = current_uncoupled_object,
                                                                                                  append_routine = 'matrix')
  
  # remove current group of sites from available pool
  simulation_data_object$output_data$index_object <- update_index_object(simulation_data_object$output_data$index_object,
                                                                         update_type = 'uncoupled',
                                                                         current_pool)
  
  current_pool = unlist(current_pool)
  
  updated_dynamics_object <- update_feature_dynamics(simulation_data_object$feature_dynamics,
                                                    simulation_data_object$management_dynamics,
                                                    simulation_data_object$feature_dynamics_modes[current_pool], 
                                                    simulation_data_object$site_scale_features[current_pool],
                                                    simulation_params$features_to_offset,
                                                    action_type = 'offset', 
                                                    simulation_data_object$background_mode_num_total,
                                                    simulation_data_object$feature_params,
                                                    yr)
  
  simulation_data_object$feature_dynamics[updated_dynamics_object$modes_to_update + simulation_data_object$background_mode_num_total, ] = updated_dynamics_object$feature_dynamics
  simulation_data_object$feature_dynamics_modes[current_pool] = updated_dynamics_object$feature_dynamics_modes
  
  #simulation_data_object$background_mode_num_total
    
  # simulation_data_object$feature_dynamics_modes = update_feature_dynamics_modes(simulation_data_object$feature_dynamics_modes,
  #                                                                               simulation_data_object$global_params$feature_num,
  #                                                                               simulation_params$features_to_offset,
  #                                                                               action_type = simulation_params$offset_action_type,
  #                                                                               current_pool)
  # 
  # simulation_data_object$feature_dynamics[current_pool] = update_feature_dynamics(simulation_data_object$site_scale_features[current_pool], 
  #                                                                                 simulation_data_object$feature_dynamics[current_pool, , drop = FALSE], 
  #                                                                                 simulation_data_object$management_dynamics[current_pool, , drop = FALSE],
  #                                                                                 simulation_data_object$feature_dynamics_modes[current_pool],
  #                                                                                 simulation_data_object$feature_params, 
  #                                                                                 features_to_update = simulation_params$features_to_offset, 
  #                                                                                 action_type = 'offset', 
  #                                                                                 yr)
  
  
  
  flog.info('added %s uncoupled offset sites to program with site IDs', length(current_pool))
  flog.info(cat(paste(simulation_data_object$site_characteristics$site_IDs[current_pool]), '\n'))
  
  return(simulation_data_object)
}





# remove site characteristics from current pool of available sites
remove_sites_from_pool <- function(pool_object, current_site_indexes){
  
  pool_indexes_to_remove = which(pool_object$site_indexes %in% current_site_indexes)
  
  if (length(pool_indexes_to_remove) > 0){
    pool_object <- setNames(lapply(seq_along(pool_object), function(i) pool_object[[i]][-pool_indexes_to_remove, , drop = FALSE]), 
                            names(pool_object))
  } 
  
  return(pool_object)
}


# routines to mark and destroy feature_layers in cleared sites e.g. Development or unregulated

run_clearing_routines <- function(simulation_data_object, simulation_params, current_development_object, clearing_type, yr){
  
  if (length(current_development_object$site_indexes) == 0){
    return(simulation_data_object)
  }
  #remove development parcels from available pool
  
  simulation_data_object$output_data$index_object <- update_index_object(simulation_data_object$output_data$index_object, update_type = clearing_type, current_development_object$site_indexes)
  if (clearing_type == 'development'){
    #record current development site characteristics
    
    simulation_data_object$output_data$interventions$development_object <- append_current_group(simulation_data_object$output_data$interventions$development_object, current_development_object, append_routine = 'matrix')
    
  } else if (clearing_type == 'develop_from_credit'){
    #record current development site characteristics
    
    simulation_data_object$output_data$interventions$uncoupled_development_object <- append_current_group(simulation_data_object$output_data$interventions$uncoupled_development_object, current_development_object, append_routine = 'matrix')
  } else if (clearing_type == 'unregulated_loss'){
    #record current cleared site characteristics
    
    simulation_data_object$output_data$interventions$unregulated_loss_object <- append_current_group(simulation_data_object$output_data$interventions$unregulated_loss_object, current_development_object, append_routine = 'matrix')
  }
  
  if (length(current_development_object$site_indexes) > 0){
    
    simulation_data_object$offset_pool_object <- remove_sites_from_pool(simulation_data_object$offset_pool_object, 
                                                                        current_development_object$site_indexes)
    simulation_data_object$dev_pool_object <- remove_sites_from_pool(simulation_data_object$dev_pool_object, 
                                                                     current_development_object$site_indexes)
    
  }
  
  current_pool <- unlist(current_development_object$site_indexes)

  updated_dynamics_object <- update_feature_dynamics(simulation_data_object$feature_dynamics,
                                                     simulation_data_object$management_dynamics,
                                                     simulation_data_object$feature_dynamics_modes[current_pool], 
                                                     simulation_data_object$site_scale_features[current_pool],
                                                     simulation_params$features_to_clear,
                                                     action_type = 'development', 
                                                     simulation_data_object$background_mode_num_total,
                                                     simulation_data_object$feature_params,
                                                     yr)
  
  simulation_data_object$feature_dynamics_modes[current_pool] = updated_dynamics_object$feature_dynamics_modes
  
  # simulation_data_object$feature_dynamics_modes = update_feature_dynamics_modes(simulation_data_object$feature_dynamics_modes,
  #                                                                               simulation_data_object$global_params$feature_num,
  #                                                                               seq(simulation_data_object$global_params$feature_num),
  #                                                                               action_type = 'development',
  #                                                                               current_pool)
  # 
  simulation_data_object$site_scale_features[current_pool] = lapply(seq_along(current_pool), 
                                                                    function(i) Matrix(0, 
                                                                                       nrow = nrow(simulation_data_object$site_scale_features[[ current_pool[i] ]]), 
                                                                                       ncol = ncol(simulation_data_object$site_scale_features[[ current_pool[i] ]]), 
                                                                                       sparse = TRUE))
  # simulation_data_object$feature_dynamics[current_pool] = update_feature_dynamics(simulation_data_object$site_scale_features[current_pool],
  #                                                                                 simulation_data_object$feature_dynamics[current_pool, , drop = FALSE], 
  #                                                                                 simulation_data_object$management_dynamics[current_pool, , drop = FALSE],
  #                                                                                 simulation_data_object$feature_dynamics_modes[current_pool],
  #                                                                                 simulation_data_object$feature_params, 
  #                                                                                 features_to_update = seq(simulation_data_object$global_params$feature_num), 
  #                                                                                 action_type = 'development', 
  #                                                                                 yr)
  # 
  
  return(simulation_data_object)
  
}


# determine current available credit accumulated through offsets for development
assess_banking_credit <- function(output_data, simulation_params){
  
  features_to_use_in_offset_calc = simulation_params$features_to_use_in_offset_calc
  # determine total offset gains
  
  offset_credit = nested_list_sum(offset_pool_object$site_score)
  
  dev_list = append(output_data$interventions$uncoupled_development_object$site_score, output_data$interventions$development_object$site_score)
  
  if (length(dev_list) > 0){
    browser()
    # determine total development losses
    dev_sum = nested_list_sum(dev_list)
    current_credit = subtract_nested_lists(offset_credit, dev_sum)
    
  } else{
    current_credit = offset_credit
  }
  
  return(current_credit)
}






# determine characteristics of potential offset sites
build_intervention_pool <- function(simulation_data_object, simulation_params, pool_type, current_pool, yr){
  
  # if no developments or uncoupled offsets for the current year return null object
  if (simulation_data_object$output_data$index_object$intervention_control[yr] ==  0){
    pool_object = list()
    return(pool_object)
  }
  
  # if pool is empty return null object and print error
  
  if (length(current_pool) == 0){
    flog.info(paste0('empty ', pool_type, ' pool'))
    pool_object = list()
    return(pool_object)
  }
  
  t0 = Sys.time()
  
  if (pool_type == 'uncoupled'){
    
    subset_pool = simulation_data_object$output_data$interventions$uncoupled_offset_object$site_indexes
    
    pool_object <- subset_current_pool(simulation_data_object$output_data$interventions$uncoupled_offset_object, subset_pool)
    
    current_pool = unlist(simulation_data_object$output_data$interventions$uncoupled_offset_object$site_indexes[subset_pool])
    pool_object$projected_vals <- find_current_site_sums(simulation_data_object$site_scale_features[current_pool])
    
  } else {
    
    pool_object <- record_site_characteristics(simulation_data_object$site_scale_features[current_pool],
                                               current_pool,
                                               site_num_remaining = length(current_pool),
                                               yr)   #arrange available parcel pool into form to use in parcel set determination
  }
  
  
  pool_object <- assess_current_pool(pool_object = pool_object,
                                     pool_type,
                                     features_to_use = simulation_params$features_to_use_in_offset_calc,
                                     site_scale_features = simulation_data_object$site_scale_features[current_pool],
                                     feature_dynamics = simulation_data_object$feature_dynamics,
                                     management_dynamics = simulation_data_object$management_dynamics,
                                     mode_characteristics = simulation_data_object$mode_charateristics, 
                                     feature_dynamics_modes = simulation_data_object$feature_dynamics_modes[current_pool],
                                     cell_num = simulation_data_object$site_characteristics$cell_num[current_pool],
                                     calc_type = simulation_params$offset_calc_type,
                                     cfacs_flag = simulation_params$offset_cfacs_flag,
                                     adjust_cfacs_flag = simulation_params$adjust_offset_cfacs_flag,
                                     action_type = simulation_params$offset_action_type,
                                     include_potential_developments = simulation_params$include_potential_developments_in_offset_calc,
                                     include_potential_offsets = simulation_params$include_potential_offsets_in_offset_calc,
                                     include_unregulated_loss = simulation_params$include_unregulated_loss_in_offset_calc,
                                     recalculate_probabilities(simulation_data_object$dev_probability_list[current_pool]), 
                                     recalculate_probabilities(simulation_data_object$offset_probability_list[current_pool]), 
                                     time_horizon_type = simulation_params$offset_time_horizon_type,
                                     simulation_params,
                                     simulation_data_object$feature_params,
                                     time_horizon = simulation_params$offset_time_horizon,
                                     yr, 
                                     simulation_data_object$global_params$user_transform_function)      
  
  flog.info(paste(pool_type, ' pool in', Sys.time() - t0))
  
  return(pool_object)
}









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

project_elements <- function(elements_to_project, current_feature_dynamics, current_condition_class_characteristics, 
                             time_horizon, t0_flag, break_flag){
  
  if (break_flag == TRUE){
    
  }
  
  if (length(time_horizon) > 1){
    projected_elements = Matrix(rep(elements_to_project, length(time_horizon)), byrow = FALSE, ncol = length(time_horizon))
    over_discriminator = Matrix(rep(current_condition_class_characteristics$upper_bound, length(time_horizon)), ncol = length(time_horizon))
    under_discriminator = Matrix(rep(current_condition_class_characteristics$lower_bound, length(time_horizon)), ncol = length(time_horizon))
  } else {
    projected_elements = elements_to_project
    over_discriminator = current_condition_class_characteristics$upper_bound
    under_discriminator = current_condition_class_characteristics$lower_bound
  }
  
  if (t0_flag == FALSE){
    projected_elements = projected_elements + current_feature_dynamics
  } else {
    projected_elements[, 2:length(time_horizon)] = projected_elements[, 2:length(time_horizon)] + current_feature_dynamics
  }
  
  # enforce limits on projection
  over = which(projected_elements > over_discriminator)
  if (length(over) > 0){
    projected_elements[over] = over_discriminator[over]
  }
  
  under = which(projected_elements < under_discriminator)
  if (length(under) > 0){
    projected_elements[under] = under_discriminator[under]
  }
  
  return(projected_elements)
}




# project sites through all features - returns nested list of arrays where each nested array has length
# defined by current_time_horizons and depth defined by feature number for all sites

project_features <- function(site_scale_features, current_feature_dynamics, feature_dynamics_modes, mode_characteristics, condition_class_characteristics, 
                             time_horizon, t0_flag, adjust_cfacs_flag, flatten_features, collapse_features, cell_num, break_flag){
  
  feature_dim = ncol(site_scale_features)
  
  if (break_flag == TRUE){
    browser()
  }
  
  project_discriminator = as.vector(feature_dynamics_modes > 0)
  set_to_project = which(project_discriminator)
  
  if (flatten_features == TRUE){
    dim(site_scale_features) = c(length(site_scale_features), 1)
  }
  
  if (length(time_horizon) > 1){
    rep_discriminator = which( (!(site_scale_features == 0) ) & !(project_discriminator) )
    features_to_export = Matrix(0, nrow = length(site_scale_features), ncol = length(time_horizon), sparse = TRUE)
    
    if (length(rep_discriminator) > 0){
      features_to_export[rep_discriminator, ] = rep(site_scale_features[rep_discriminator], length(time_horizon))
    }
    
  } else{
    features_to_export = site_scale_features
  }
  
  if (length(set_to_project) > 0){
    
    current_mode_set = match(feature_dynamics_modes[set_to_project], mode_characteristics$ID)
    
    current_condition_class_set = mode_characteristics$condition_class[current_mode_set]
    
    projected_elements = project_elements(site_scale_features[set_to_project], 
                                          current_feature_dynamics[feature_dynamics_modes[set_to_project], , drop = FALSE], 
                                          condition_class_characteristics[current_condition_class_set, ], 
                                          time_horizon,
                                          t0_flag,
                                          break_flag)
    
    
    if (length(time_horizon) == 1){
      
      features_to_export[set_to_project] = projected_elements
      
    } else {
      features_to_export[set_to_project, ] = projected_elements
    }
    
  }
  
  if (adjust_cfacs_flag == TRUE){
    features_to_export = adjust_cfacs(features_to_export,
                                      cfac_weights, 
                                      feature_dynamics_modes_to_use, 
                                      time_fill)
  } 
  
  if (collapse_features == TRUE){
    
    if (length(time_horizon) == 1){
      
      features_to_export <- matrix(colSums(features_to_export), nrow = length(time_horizon), ncol = feature_dim)
      
    } else{
      
      collapsed_features = matrix(0, nrow = length(time_horizon), ncol = feature_dim)
      
      for (feature_ind in seq(feature_dim)){
        collapsed_features[, feature_ind] <- colSums(features_to_export[(1:cell_num) + cell_num * (feature_ind - 1), , drop = FALSE])
      }
      
      features_to_export = collapsed_features
      
    }
    
  }
  
  return(features_to_export)
  
}



# find sum nested list of depth j with identical structure
nested_list_sum <- function(nested_list){
  
  if (length(nested_list) > 0){
    summed_list <- lapply(seq_along(nested_list[[1]]),
                          function(j) Reduce('+', lapply(seq_along(nested_list), function(i) nested_list[[i]][[j]])))
    return(summed_list)
  } else {
    return(vector())
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


recalculate_probabilities <- function(current_probability_list){
  current_probability_list = lapply(seq_along(current_probability_list), function(i) current_probability_list[[i]]/sum(unlist(current_probability_list)))
  return(current_probability_list)
}


match_development_to_offset <- function(simulation_data_object, simulation_params, match_type, yr){
  ###### TODO check zero development routines the code below may not be the most efficient method for this ###
  
  output_match_object = setNames(list(FALSE), 'match_flag')
  
  if (match_type == 'develop_with_offset_pool'){
    test_pool = list()
    test_pool$vals = simulation_data_object$dev_pool_object$site_score
    test_pool$site_indexes = simulation_data_object$dev_pool_object$site_indexes
    
    match_pool = list()
    match_pool$vals = simulation_data_object$offset_pool_object$site_score
    match_pool$site_indexes = simulation_data_object$offset_pool_object$site_indexes
    
  }
  
  if ( (length(match_pool$site_indexes) == 0) | (length(test_pool$site_indexes) == 0) ){
    
    return(output_match_object)
    
  } else {
    
    non_zero_discriminator = rowSums(test_pool$vals) > 0
    
    if ( length(which(non_zero_discriminator)) == 0){
      
      return(output_match_object)
      
    } else {
      
      if (length(which(!non_zero_discriminator)) > 0){
        match_pool <- remove_sites_from_pool(match_pool, test_pool$site_indexes[which(!non_zero_discriminator)])
      }
      
    }
    
  }
  
  while( (output_match_object$match_flag == FALSE)){   
    
    if (length(test_pool$site_indexes) == 0){
      return(output_match_object)
    }
    
    if ((simulation_params$development_selection_type == 'stochastic') | (simulation_params$development_selection_type == 'pre_determined')){
      current_dev_probability_list = rep(1/nrow(test_pool$site_indexes), nrow(test_pool$site_indexes))
    } else if (simulation_params$development_selection_type == 'weighted'){
      current_dev_probability_list = recalculate_probabilities(simulation_data_object$dev_probability_list[test_pool$site_indexes])
    } 
    
    current_site = sample(x = seq_along(test_pool$site_indexes), 
                          size = 1, 
                          prob = current_dev_probability_list, 
                          replace = TRUE)
    
    current_test_index = test_pool$site_indexes[current_site]
    current_sample_vals = test_pool$vals[current_site, , drop = FALSE]
    
    if (simulation_params$use_uncoupled_offsets == FALSE){
      current_match_pool <- remove_sites_from_pool(match_pool, test_pool$site_indexes[current_site])
    } else {
      current_match_pool = match_pool
    }
    
    if (length(current_match_pool$site_indexes) == 0){
      return(output_match_object)
    }
    
    output_match_object <- run_match_routine(match_type = 'offset',
                                             current_match_pool$site_indexes,
                                             current_match_pool$vals,
                                             simulation_data_object$credit_object$current_credit,
                                             simulation_data_object$offset_probability_list,
                                             current_sample_vals,
                                             simulation_params,
                                             yr) #perform matching routine
    
    if (output_match_object$zero_flag == TRUE){
      return(output_match_object)
    } else if (output_match_object$match_flag == FALSE){
      
      # only try to match sites with smaller ecological value than current site
      match_block = matrix(rep(current_sample_vals, nrow(simulation_data_object$dev_pool_object$site_score)), 
                           nrow = nrow(simulation_data_object$dev_pool_object$site_score), byrow = TRUE)
      
      match_discriminator = rowSums(simulation_data_object$dev_pool_object$site_score - match_block < 0) == ncol(simulation_data_object$dev_pool_object$site_score)
      
      test_pool$site_indexes = simulation_data_object$dev_pool_object$site_indexes[match_discriminator, , drop = FALSE]     
      test_pool$vals = simulation_data_object$dev_pool_object$site_score[match_discriminator, , drop = FALSE]
      
    }
    
  }
  
  if (output_match_object$match_flag == TRUE){
    
    dev_match_index = which(unlist(simulation_data_object$dev_pool_object$site_indexes) == current_test_index)
    output_match_object$development_object = subset_current_pool(simulation_data_object$dev_pool_object, unlist(dev_match_index))
    subset_pool = list_intersect(simulation_data_object$offset_pool_object$site_indexes, output_match_object$match_indexes)
    offset_object <- subset_current_pool(pool_object = simulation_data_object$offset_pool_object, subset_pool = subset_pool$match_ind)
    output_match_object$offset_object = offset_object
    
  } 
  
  return(output_match_object)
  
}




subtract_nested_lists <- function(list_a, list_b){
  length_a = unlist(lapply(seq_along(list_a), function(i) length(list_a[[i]])))
  length_b = unlist(lapply(seq_along(list_b), function(i) length(list_b[[i]])))
  stopifnot(all(length_a == length_b))
  list_c = lapply( seq_along(list_a), function(i)  mapply('-', list_a[[i]], list_b[[i]], SIMPLIFY = FALSE))
  return(list_c)
}


subtract_lists <- function(list_a, list_b){
  list_c = mapply('-', list_a, list_b, SIMPLIFY = FALSE)
  return(list_c)
}

# sum_lists <- function(list_a, list_b){
#   if ( (length(list_a) == length(list_b)) & (length(list_a) >0)){
#     list_c = mapply('+', list_a, list_b, SIMPLIFY = FALSE)
#   } else if (length(list_a) == 0){
#     if (length(list_b) == 0){
#       list_c = list()
#     } else{
#       list_c = list_b
#     }
#   } else if (length(list_b) == 0){
#     if (length(list_a) == 0){
#       list_c = list()
#     } else{
#       list_c = list_a
#     }
#   }
#   return(list_c)
# }
# 

# store group of site characteristics in site_set_object
record_site_characteristics <- function(site_scale_features, current_pool, site_num_remaining, yr){
  
  site_set_object = list()
  site_set_object$intervention_yrs = matrix(rep(yr, length(current_pool)), nrow = length(current_pool))
  site_set_object$site_sums_at_offset = lapply(seq_along(site_scale_features), function(i) matrix(colSums(site_scale_features[[i]]), nrow = 1))
  site_set_object$site_sums_at_offset = do.call(rbind, site_set_object$site_sums_at_offset)
  site_set_object$site_indexes = matrix(current_pool, length(current_pool))
  site_set_object$site_num_remaining = matrix(rep(site_num_remaining, length(current_pool)), length(current_pool))
  
  return(site_set_object)
  
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
find_intervention_probability <- function(intervention_control, site_intervention_probability, intervention_yrs, calc_type, offset_intervention_scale, 
                                          time_horizons, site_num, site_num_remaining, time_steps){
  
  intervention_probs = vector('list', site_num)
  site_num_remaining = unlist(site_num_remaining)
  intervention_yrs = unlist(intervention_yrs)
  
  for (site_ind in seq_len(site_num)){
    
    time_horizon = time_horizons[site_ind]
    
    current_prob = array(0, (time_horizon))
    
    current_intervention_control = intervention_control[intervention_yrs[site_ind]:time_steps]
    
    if (length(current_intervention_control) < (time_horizon)){
      current_intervention_control = c(current_intervention_control, array(0, ((time_horizon) - length(current_intervention_control))))
    }
    
    current_intervention_control = current_intervention_control[1:(time_horizon)]
    
    current_site_num_remaining = site_num_remaining[site_ind]
    
    if (current_site_num_remaining > 0) {
      current_prob = current_prob + site_intervention_probability[[site_ind]]*(current_intervention_control)
    }
    
    if (calc_type == 'offset'){
      current_prob = offset_intervention_scale*current_prob
    }
    
    intervention_probs[[site_ind]] = current_prob
    
  }
  
  return(intervention_probs)
}



# test to determine nearest neighbour by Euclidean norm given a pool of potential candidates (site_vals_pool)
# and the criterion to match to (current_sample_vals)

euclidean_norm_match <- function(site_vals_pool, current_sample_vals){
  
  match_block = lapply(seq(nrow(site_vals_pool)), function(i) current_sample_vals)
  match_block = do.call(rbind, match_block)
  
  euclidean_norm = rowSums(site_vals_pool - match_block)^2
  
  match_ind = which(euclidean_norm == min(euclidean_norm))
  
  if (length(match_ind) > 1){
    
    #list where the duplicate occurred
    flog.warn(cat(paste('duplicate site value match on ', length(match_ind), 'sites \n')))
    # sample site from multiple match pool
    match_ind = sample(match_ind, 1)
  }
  
  return(match_ind)
}


select_pool_to_match <- function(current_sample_vals, use_offset_metric, thresh, current_match_pool_vals, 
                                 current_match_pool, max_site_num, match_type, screen_site_zeros, non_zero_discriminator){
  
  pool_object = setNames(list(FALSE, FALSE), c('match_flag', 'zero_flag'))
  
  if (length(current_match_pool_vals) == 0){
    return(pool_object)
  } 
  
  if (screen_site_zeros == TRUE){ 
    
    zero_inds <- which(rowSums(current_match_pool_vals[, non_zero_discriminator, drop = FALSE]) == 0)
    
    if (length(zero_inds) > 0){

      flog.info(cat('matching on feature indicies ', paste(non_zero_discriminator), 
                    ' removing ', length(zero_inds), 'sites with zero score for these features', '\n'))
      
      current_match_pool <- current_match_pool[-zero_inds, , drop = FALSE]
      current_match_pool_vals <- current_match_pool_vals[-zero_inds, , drop = FALSE]
    }
    
    if (length(current_match_pool) == 0){
      cat('all sites in', match_type, 'pool have zero value \n')
      pool_object$zero_flag = TRUE
      return(pool_object)
    } 
    
  } 
  
  if (max_site_num > 1){
    
    max_discriminator = unlist(lapply(non_zero_discriminator, function(i) sum(tail(sort(current_match_pool_vals[, i, drop = FALSE]), max_site_num))))
    
    if ( any( (max_discriminator - current_sample_vals[, non_zero_discriminator] > thresh[non_zero_discriminator]) == FALSE)){
      return(pool_object)
    } 
    
  } else {
    
    if (match_type == 'offset'){
      browser()
      match_discriminator = unlist(lapply(seq(nrow(current_match_pool_vals)), 
                                          function(i) all( (current_sample_vals[, non_zero_discriminator, drop = FALSE] - current_match_pool_vals[i, non_zero_discriminator, drop = FALSE]) <= thresh[non_zero_discriminator])))
    } else if (match_type == 'development'){
      match_discriminator = unlist(lapply(seq(nrow(current_match_pool_vals)), 
                                          function(i) all( (current_sample_vals[, non_zero_discriminator, drop = FALSE] - current_match_pool_vals[i, non_zero_discriminator, drop = FALSE]) >= -thresh[non_zero_discriminator])))
    }
    
    if (all(match_discriminator == FALSE)){
      return(pool_object)
    }
    current_match_pool_vals <- current_match_pool_vals[match_discriminator, , drop = FALSE]
    current_match_pool <- current_match_pool[match_discriminator, drop = FALSE]
    
  }
  
  pool_object$match_flag = TRUE
  pool_object$current_match_pool$vals = current_match_pool_vals
  pool_object$current_match_pool$site_indexes = current_match_pool
  
  if (length(pool_object$match_flag) == 0){
    browser()
  }
  
  return(pool_object)
}


run_match_routine <- function(match_type, current_match_pool, current_match_pool_vals, current_credit, current_probability_list, 
                              current_sample_vals, simulation_params, yr){
  
  if (match_type == 'offset'){
    non_zero_discriminator = which(current_sample_vals > 0)
    max_site_num = simulation_params$max_offset_site_num
    match_procedure = simulation_params$offset_selection_type
    screen_site_zeros = simulation_params$screen_offset_zeros
    
    current_sample_vals = simulation_params$offset_multiplier * current_sample_vals
    
    if ( simulation_params$use_credit_in_offset == TRUE){
      current_sample_vals = current_sample_vals - current_credit
    }
    
  } else if (match_type == 'development'){
    
    # force offset of all features
    non_zero_discriminator = seq(ncol(current_sample_vals))
    # force only single development for development routine
    max_site_num = 1
    match_procedure = simulation_params$development_selection_type
    # when developing from credit, use inverse offset multiplier
    current_sample_vals = 1 / simulation_params$offset_multiplier * current_sample_vals
    screen_site_zeros = simulation_params$screen_dev_zeros
  }
  
  #create an array of threshold values defined by user based proportion 
  thresh = simulation_params$match_threshold_ratio*current_sample_vals
  
  pool_object <- select_pool_to_match(current_sample_vals, 
                                      simulation_params$use_offset_metric, 
                                      thresh,
                                      current_match_pool_vals, 
                                      current_match_pool, 
                                      max_site_num, 
                                      match_type, 
                                      screen_site_zeros, 
                                      non_zero_discriminator)
  
  if (pool_object$match_flag == FALSE){
    return(pool_object)
  }
  
  output_match_object = setNames(list(FALSE, FALSE), c('match_flag', 'zero_flag'))
  output_match_object$match_vals = list()
  output_match_object$match_indexes = list()
  
  while(output_match_object$match_flag == FALSE){
    
    if (length(pool_object$current_match_pool$site_indexes) == 0){
      break
    }
    
    current_output_match_object = list()
    
    
    if (match_procedure == 'greedy'){
      
      if (any(apply(pool_object$current_match_pool$vals[ , non_zero_discriminator, drop = FALSE] , 1, 'sum') == 0)){
        browser()
      }
      
      current_output_match_object$match_ind = euclidean_norm_match(pool_object$current_match_pool$vals[ , non_zero_discriminator, drop = FALSE], 
                                                                   current_sample_vals[ , non_zero_discriminator, drop = FALSE])
      
      current_output_match_object$match_vals = pool_object$current_match_pool$vals[current_output_match_object$match_ind, , drop = FALSE]
      
    } else {
      
      if (match_procedure == 'weighted'){
        probability_list_to_use = recalculate_probabilities(current_probability_list[pool_object$current_match_pool$site_indexes])
      } else if ( (match_procedure == 'stochastic')) {
        probability_list_to_use = rep(1/length(pool_object$current_match_pool$site_indexes), length(pool_object$current_match_pool$site_indexes))
      }
      
      current_output_match_object$match_ind = sample(x = seq_along(pool_object$current_match_pool$site_indexes), 
                                                     size = 1, 
                                                     prob = probability_list_to_use, 
                                                     replace = TRUE)
      
      current_output_match_object$match_vals = pool_object$current_match_pool$vals[current_output_match_object$match_ind, , drop = FALSE]
    }
    
    
    current_match_index = pool_object$current_match_pool$site_indexes[current_output_match_object$match_ind]
    
    if (max_site_num > 1){
      
      output_match_object$match_vals = append(output_match_object$match_vals, current_output_match_object$match_vals)
      output_match_object$match_indexes = append(output_match_object$match_indexes, current_match_index)
      
      #site_to_remove = list_intersect(pool_object$current_match_pool, current_match_index)
      
      #if (length(site_to_remove$match_ind) > 0){
      pool_object$current_match_pool = remove_sites_from_pool(pool_object$current_match_pool, current_match_index)
      #pool_object$current_match_pool_vals = pool_object$current_match_pool_vals[-site_to_remove$match_ind, , drop = FALSE]
      #}
      
      if (length(output_match_object$match_indexes) > max_site_num){
        output_match_object$match_flag = FALSE
        break
      }
      
      current_sample_vals = current_sample_vals - current_output_match_object$match_vals
      if (match_type == 'offset'){
        global_match_discriminator = current_sample_vals[, non_zero_discriminator, drop = FALSE] <= thresh[non_zero_discriminator]
      } else if (match_type == 'development'){
        global_match_discriminator = current_sample_vals[, non_zero_discriminator, drop = FALSE] >= -thresh[non_zero_discriminator]
      } 
      
      output_match_object$match_flag = all(global_match_discriminator)
      
      if ( (output_match_object$match_flag == FALSE) && any(global_match_discriminator)){
        
        non_zero_discriminator = non_zero_discriminator[!global_match_discriminator]
        pool_object <- select_pool_to_match(current_sample_vals, 
                                            simulation_params$use_offset_metric, 
                                            thresh,
                                            pool_object$current_match_pool$vals, 
                                            pool_object$current_match_pool$site_indexes, 
                                            max_site_num, 
                                            match_type, 
                                            screen_site_zeros, 
                                            non_zero_discriminator)
      }
      
    } else {
      output_match_object$match_flag = TRUE
      output_match_object$match_indexes = list(current_match_index)
      output_match_object$match_vals = list(current_output_match_object$match_vals)
      current_sample_vals = current_sample_vals - current_output_match_object$match_vals
    }
    
  }
  
  #### TODO if running with offset_metric specified current credit is wrong as it copies all entries to the same value
  
  if (match_type == 'offset'){
    # switch sign for any additional credit from offset
    output_match_object$current_credit = -current_sample_vals 
  } else if (match_type == 'development'){
    output_match_object$current_credit = current_sample_vals
  }
  
  return(output_match_object)
  
}


#remove site from available pool for offsets and developments. This is a two stage process to cover when offsets and developments may not overlap
update_index_object <- function(index_object, update_type, site_indexes){
  #remove site from available list
  index_object$available_indexes$offsets = setdiff(index_object$available_indexes$offsets, site_indexes)
  index_object$available_indexes$developments = setdiff(index_object$available_indexes$developments, site_indexes)
  
  if (update_type == 'offset'){
    index_object$site_indexes_used$offset_object = append(index_object$site_indexes_used$offset_object, list(site_indexes))
  } else if (update_type == 'development'){
    index_object$site_indexes_used$development_object = append(index_object$site_indexes_used$development_object, list(site_indexes))
  } else if (update_type == 'unregulated_loss'){
    index_object$site_indexes_used$unregulated_loss_object = append(index_object$site_indexes_used$unregulated_loss_object, list(site_indexes))
  } else if (update_type == 'develop_from_credit'){
    index_object$site_indexes_used$uncoupled_development_object = append(index_object$site_indexes_used$uncoupled_development_object, list(site_indexes))
  } else if (update_type == 'uncoupled'){
    index_object$site_indexes_used$uncoupled_offset_object = append(index_object$site_indexes_used$uncoupled_offset_object, list(site_indexes))
  }
  
  return(index_object)
}

#intersection routine for lists with catches on null lists
list_intersect <- function(list_a, list_b){
  
  list_match = list()
  
  if ( (length(as.vector(list_a)) == 0) || (length(as.vector(list_b)) == 0)){
    return(list_match)
  }
  
  list_match$match_ind = which(list_a %in% list_b)
  list_match$match_val = list_a[list_match$match_ind]
  return(list_match)
}


# Determine subset of object (eg development or offset group) containing a nested set of components with the same structure
subset_current_pool <- function(pool_object, subset_pool){
  object_subset = setNames(lapply(seq_along(pool_object), function(i) pool_object[[i]][subset_pool, , drop = FALSE]), names(pool_object))
  return(object_subset)
}


#function to work out vector of time intervals used in gains calculations
generate_time_horizons <- function(project_type, yr, intervention_yrs, time_horizon, site_count){
  
  if (project_type == 'current'){
    # work out time intervals from time of offset to current year
    time_horizons = rep(yr, site_count) - intervention_yrs
  } else if (project_type == 'future'){
    # work out time intervals from current year to projected year defined by time_horizon
    time_horizons = rep(time_horizon, site_count)
  } else if (project_type == 'zeros'){
    #set up dummy times
    time_horizons = rep(0, site_count)
  }
  return(time_horizons)
}


assess_current_pool <- function(pool_object, pool_type, features_to_use, site_scale_features, feature_dynamics, management_dynamics, mode_characteristics, 
                                feature_dynamics_modes, cell_num, calc_type, cfacs_flag, adjust_cfacs_flag, action_type, 
                                include_potential_developments, include_potential_offsets, include_unregulated_loss,
                                dev_probability_list, offset_probability_list, time_horizon_type, simulation_params, feature_params, time_horizon, yr, user_transform_function){
  
  if (length(site_scale_features) == 0){
    return(list())
  }
  
  if (length(features_to_use) < ncol(site_scale_features[[1]])){
    site_scale_features = lapply(seq_along(site_scale_features), function(i) site_scale_features[[i]][, features_to_use, drop = FALSE])
    feature_dynamics_modes = lapply(seq_along(feature_dynamics_modes), function(i) feature_dynamics_modes[[i]][, features_to_use, drop = FALSE])
  }
  
  if (calc_type == 'current_condition') {
    current_condition_vals = pool_object$site_sums_at_offset[, features_to_use, drop = FALSE]
    
    projected_vals = current_condition_vals
    cfac_vals = rep(list(0), length(site_scale_features))
    
  } else {
    time_horizons <- generate_time_horizons(project_type = 'future', yr, unlist(pool_object$intervention_yrs), time_horizon, length(site_scale_features))
    
    if (cfacs_flag == TRUE){
      
      if (adjust_cfacs_flag == FALSE){
        cfac_weights = FALSE
      } else {
        cfac_weights = calc_cfac_weights(site_num = length(site_scale_features), include_potential_developments, include_potential_offsets, include_unregulated_loss,
                                         dev_probability_list, offset_probability_list, simulation_params, global_params, feature_params, pool_object$site_num_remaining, time_horizons, unlist(pool_object$intervention_yrs))
      }
      
      if (simulation_params$use_offset_metric == FALSE){
        
        cfac_vals = run_projection_routines(site_scale_features,
                                            feature_dynamics,
                                            feature_dynamics_modes,
                                            mode_characteristics,
                                            condition_class_characteristics = feature_params$condition_class_characteristics,
                                            dynamics_update_type = 'static',
                                            projection_yr = yr,
                                            time_horizon,
                                            collapse_features = TRUE,
                                            adjust_cfacs_flag = FALSE,
                                            cfac_weights,
                                            cell_num,
                                            break_flag = FALSE)
        
      } else {
        
        # cfac_vals = lapply(seq_along(site_scale_features), 
        #                    function(i) sum(user_transform_function(run_projection_routines(site_scale_features[[i]],
        #                                                                            projection_yrs[[i]],
        #                                                                            cfac_weights,
        #                                                                            cell_num[[i]],
        #                                                                            feature_dynamics,
        #                                                                            feature_dynamics_modes[[i]],
        #                                                                            time_horizons[[i]],
        #                                                                            adjust_cfacs_flag,
        #                                                                            time_fill = FALSE,
        #                                                                            flatten_features = FALSE), 
        #                                                                   simulation_params$transform_params)))
        
        stop()  
        
        cfac_vals = run_projection_routines(site_scale_features,
                                            feature_dynamics,
                                            feature_dynamics_modes,
                                            mode_characteristics,
                                            condition_class_characteristics = feature_params$condition_class_characteristics,
                                            dynamics_update_type = 'static',
                                            projection_yr = yr,
                                            time_horizon,
                                            collapse_features = TRUE,
                                            adjust_cfacs_flag = FALSE,
                                            cfac_weights,
                                            cell_num,
                                            break_flag = FALSE)
        
      }
      
    } else if (calc_type == 'restoration_gains'){
      
      cfac_vals = pool_object$site_sums_at_offset[, features_to_use, drop = FALSE]
      
    } 
    
    cfac_vals <- do.call(rbind, cfac_vals)
    
    if (pool_type == 'developments') {
      
      projected_site_scores = matrix(0, nrow = nrow(cfac_vals), ncol = ncol(cfac_vals))
      
    } else if (pool_type == 'offsets') {
      
      # management_dynamics <- update_feature_dynamics(feature_dynamics, 
      #                                                management_dynamics, 
      #                                                feature_dynamics_modes, 
      #                                                site_scale_features, 
      #                                                features_to_update, 
      #                                                action_type, 
      #                                                background_mode_num, 
      #                                                feature_params, 
      #                                                yr){
      
      if (simulation_params$use_offset_metric == FALSE){
        
        updated_dynamics_object <- update_feature_dynamics(feature_dynamics,
                                                                      management_dynamics,
                                                                      feature_dynamics_modes, 
                                                                      site_scale_features,
                                                                      simulation_params$features_to_offset,
                                                                      action_type = 'offset', 
                                                                      0,
                                                                      feature_params,
                                                                      yr)
        
        feature_dynamics[updated_dynamics_object$modes_to_update, ] = updated_dynamics_object$feature_dynamics
        
        projected_site_scores = run_projection_routines(site_scale_features,
                                                        feature_dynamics,
                                                        updated_dynamics_object$feature_dynamics_modes,
                                                        mode_characteristics,
                                                        condition_class_characteristics = feature_params$management_condition_class_characteristics,
                                                        dynamics_update_type = 'static',
                                                        projection_yr = yr,
                                                        time_horizon,
                                                        collapse_features = TRUE,
                                                        adjust_cfacs_flag = FALSE,
                                                        cfac_weights,
                                                        cell_num,
                                                        break_flag = FALSE)
        
      } else {
        stop()
        # projected_site_scores = lapply(seq_along(site_scale_features),
        #                             function(i) apply(user_transform_function(run_projection_routines(site_scale_features[[i]],
        #                                                                                       projection_yrs[[i]],
        #                                                                                       cfac_weights,
        #                                                                                       cell_num[[i]],
        #                                                                                       management_dynamics,
        #                                                                                       feature_dynamics_modes[[i]],
        #                                                                                       time_horizons[[i]],
        #                                                                                       adjust_cfacs_flag,
        #                                                                                       time_fill = FALSE,
        #                                                                                       flatten_features = FALSE), 
        #                                                                       simulation_params$transform_params), 2, 'sum')
        #                             )
        
        # projected_vals = lapply(seq_along(projected_site_scores), function(i) sum(user_transform_function(projected_site_scores[[i]], simulation_params$transform_params)))
        # 
      }
      
      projected_site_scores = do.call(rbind, projected_site_scores)
      
    }
    
  }
  
  if (pool_type == 'developments') {
    
    pool_object$site_score = cfac_vals - projected_site_scores
  } else {
    pool_object$site_score = projected_site_scores - cfac_vals
  }
  
  # match_discriminator = table(pool_object$site_score)
  # 
  # if (any(as.numeric(match_discriminator) > 0)){
  #   browser()
  # }
  # if (any(is.na(pool_object$site_score))){
  # 
  #   for (f_ind in seq_along(pool_object$site_score[[1]])){
  #     bad_inds = which(unlist(lapply(seq_along(pool_object$site_score), function(i) any(unlist(pool_object$site_score[[i]][[f_ind]]) < 0))))
  #     
  #     print(unlist(lapply(bad_inds, function(i) feature_dynamics_modes[[i]][f_ind])))
  #   }
  # }
  
  return(pool_object)
  
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
                                           append_routine,
                                           inds_to_append = seq_along(object_to_append))
  return(appended_object)
  
}




append_current_object <- function(site_set_object, current_site_set_object, append_type, inds_to_append){
  if (append_type == 'matrix'){
    #routine to append by characteristic for non nested object
    site_set_object[inds_to_append] <- lapply(inds_to_append, function(i) append(site_set_object[[i]], list(current_site_set_object[[i]])))
  } else if (append_type == 'list'){
    #routine to append by characteristic for nested object
    site_set_object[inds_to_append] <- lapply(inds_to_append, function(i) append(site_set_object[[i]], current_site_set_object[[i]]))
  }
  #set names of group to current object name
  names(site_set_object) = names(current_site_set_object)
  return(site_set_object)
}


kill_site_scale_features <- function(site_scale_features, sparse_representation){
  
  
  if (sparse_representation == TRUE){
    developed_feature_layers = lapply(seq_along(site_scale_features), 
                                      function(i) lapply(seq_along(site_scale_features[[i]]),  
                                                         function(j) list(vector())))
  } else {
    developed_feature_layers = lapply(seq_along(site_scale_features), 
                                      function(i) lapply(seq_along(site_scale_features[[i]]),  
                                                         function(j) list(array(0, length(unlist(site_scale_features[[i]][[j]]))))))
  }
  
  return(developed_feature_layers)
  
}


build_projection_shifts <- function(perform_dynamics_time_shift, current_site_scale_features, yr, 
                                    feature_dynamics_to_use, feature_dynamics_modes_to_use, dynamics_type){
  
  if ((perform_dynamics_time_shift == FALSE)){
    
    projection_yrs = yr
    
  } else {

    feature_block = as.vector(current_site_scale_features)
    mode_IDs_to_update = feature_dynamics_modes_to_use > 0
    
    modes_to_update = unique(feature_dynamics_modes_to_use[mode_IDs_to_update])
    
    feature_means = lapply(dynamic_modes, 
                           function(i) mean(feature_block[which(feature_dynamics_modes_to_use == modes_to_update[i])]))
    

    projection_yrs <- lapply(seq_along(feature_means), 
                               function(i) calc_curve_loc(feature_means[[i]], 
                                                          current_dynamics[dynamic_modes[i], , drop = FALSE],
                                                          feature_dynamics_modes_to_use, 
                                                          dynamics_type, 
                                                          perform_dynamics_time_shift))
    
    projection_yrs = calc_curve_loc(current_site_scale_features,  
                                    feature_dynamics_to_use, 
                                    feature_dynamics_modes_to_use, 
                                    dynamics_type, 
                                    perform_dynamics_time_shift)
  }
  
}


run_projection_routines <- function(site_scale_features, feature_dynamics, feature_dynamics_modes, mode_characteristics, condition_class_characteristics, dynamics_update_type, projection_yr, time_horizon, 
                                    collapse_features, adjust_cfacs_flag, cfac_weights, cell_num, break_flag){
  
  if (time_horizon[1] == 0){
    
    if (length(time_horizon) == 1){
      
      if (collapse_features == TRUE){
        unprojected_features <- lapply(seq_along(site_scale_features), function(i) matrix(apply(site_scale_features[[i]], 2, 'sum'), nrow = 1))  
      } else {
        unprojected_features <- site_scale_features
      }
      
      return(unprojected_features)
      
    } else {
      t0_flag = TRUE
    }
    
  } else {
    t0_flag = FALSE
  }
  
  if ( (projection_yr + max(time_horizon) - 1) <= ncol(feature_dynamics) ){
    
    current_time_horizon = projection_yr:(projection_yr + (max(time_horizon) - 1))
    
    current_feature_dynamics = feature_dynamics[, current_time_horizon, drop = FALSE]
    
  } else {
    
    current_feature_dynamics = Matrix(0, nrow = length(elements_to_project), ncol = length(time_horizon), sparse = TRUE)
    
    if (projection_yr <= length(feature_dynamics)){
      current_feature_dynamics[, 1:(ncol(feature_dynamics) - projection_yr + 1)] = feature_dynamics[, projection_yr:ncol(feature_dynamics), drop = FALSE]
    } 
    
  }
  
  if (ncol(current_feature_dynamics) > 1){
    
    dynamics_to_update <- which(!(rowSums(current_feature_dynamics, na.rm = FALSE) == 0))
    
    if (dynamics_update_type == 'static'){
      current_feature_dynamics[dynamics_to_update, ] = t(apply(current_feature_dynamics[dynamics_to_update, , drop = FALSE], 1, cumsum))
    } 
    
  }
  
  if (t0_flag == TRUE){
    current_feature_dynamics = current_feature_dynamics[, time_horizon[time_horizon > 0], drop = FALSE] 
  } else {
    current_feature_dynamics = current_feature_dynamics[, time_horizon, drop = FALSE]
  }
  
  flatten_features = (length(time_horizon) > 1)  & (ncol(site_scale_features[[1]]) > 1)
  
  if (break_flag == TRUE){
    browser()
  }
  
  site_scale_features <- lapply(seq_along(site_scale_features),
                                function(i) project_features(site_scale_features[[i]],
                                                             current_feature_dynamics,
                                                             feature_dynamics_modes[[i]],
                                                             mode_characteristics,
                                                             condition_class_characteristics, 
                                                             time_horizon,
                                                             t0_flag,
                                                             adjust_cfacs_flag = FALSE,
                                                             flatten_features, 
                                                             collapse_features, 
                                                             cell_num[[i]],
                                                             break_flag))
  
  return(site_scale_features)
  
}


adjust_cfacs <- function(current_cfac, current_cfac_weights, current_mode, time_fill){
  
  if (current_mode == 0){
    
    return(current_cfac)
    
  } else {
    
    if (length(current_cfac_weights) > 0){
      
      if (time_fill == TRUE){
        browser()
        current_cfac = current_cfac * Matrix(rep(current_cfac_weights, dim(current_cfac)[2]), nrow = dim(current_cfac)[1], byrow = FALSE)
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
                              dev_probability_list, offset_probability_list, simulation_params, global_params, feature_params, 
                              site_num_remaining, time_horizons, intervention_yrs){
  
  cfac_weights = lapply(seq(site_num), function(i) lapply(seq(global_params$feature_num), function(j) rep(1, time_horizons[[i]])))
  
  if (include_unregulated_loss == TRUE){
    cfac_weights <- lapply(seq_along(cfac_weights), 
                           function(i) lapply(seq_along(cfac_weights[[i]]), 
                                              function(j) cfac_weights[[i]][[j]]*(1 - simulation_params$unregulated_loss_prob)^(seq(time_horizons[[i]]))))
  }
  
  if (include_potential_developments == TRUE){
    dev_probability_weights <- generate_weights(calc_type = 'development',
                                                site_intervention_probability = dev_probability_list,
                                                simulation_params$max_offset_site_num,
                                                index_object$intervention_control,
                                                intervention_yrs,
                                                time_horizons,
                                                site_num,
                                                site_num_remaining,
                                                simulation_data_object$global_params$time_steps)
    
    cfac_weights <- lapply(seq_along(cfac_weights), 
                           function(i) lapply(seq_along(cfac_weights[[i]]), 
                                              function(j) cfac_weights[[i]][[j]] - cumsum(dev_probability_weights[[i]])))
  }
  
  if (include_potential_offsets == TRUE){
    flog.error('offset projections still in development')
    stop()
    offset_probability_weights <- generate_weights(calc_type = 'offset',
                                                   site_intervention_probability = offset_probability_list,
                                                   simulation_params$max_offset_site_num,
                                                   index_object$intervention_control,
                                                   intervention_yrs,
                                                   time_horizons,
                                                   site_num,
                                                   site_num_remaining,
                                                   simulation_data_object$global_params$time_steps)
    
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
  #                                                   global_params$feature_num, 
  #                                                   simulation_params$min_eco_val, 
  #                                                   simulation_params$max_eco_val)
  #     
  #     summed_offset_projections <- sum_offset_projs(offset_projections,
  #                                                   offset_intervention_probs, 
  #                                                   global_params$feature_num, 
  #                                                   time_horizons)
  #     
  #     adjusted_cfacs = sum_clearing_offsets(adjusted_cfacs, summed_offset_projections, global_params$feature_num)
  
  
  inds = lapply(seq_along(cfac_weights), 
                function(i) lapply(seq_along(cfac_weights[[i]]), function(j) length(cfac_weights[[i]][[j]]) == 0))
  
  return(cfac_weights)
}

remove_neg_probs <- function(weight_list, inds_to_accept){
  weight_list <- lapply(seq_along(weight_list), function(i) weight_list[[i]]*inds_to_accept[[i]])
  return(weight_list)
}


generate_weights <- function(calc_type, site_intervention_probability, offset_intervention_scale, intervention_control, intervention_yrs, time_horizons,
                             site_num, site_num_remaining, time_steps){
  
  if (calc_type == 'unregulated_loss'){
    weighted_probs <- lapply(seq_len(site_num), function(i) rep(site_intervention_probability, time_horizons[i]))    #runif(n = (time_horizon), min = 0, max = simulation_params$unregulated_loss_prob)
  } else {
    weighted_probs <- find_intervention_probability(intervention_control,
                                                    site_intervention_probability,
                                                    intervention_yrs,
                                                    calc_type,
                                                    offset_intervention_scale,
                                                    time_horizons,
                                                    site_num,
                                                    site_num_remaining,
                                                    time_steps)
  }
  
  
  return(weighted_probs)
}




calc_offset_projections <- function(dynamics_type, current_cfacs, offset_probs, restoration_rate, time_horizons, feature_num, min_eco_val, max_eco_val){
  
  site_num = length(current_cfacs)
  offset_projections = vector('list', site_num)
  
  for (site_ind in seq_len(site_num)){
    
    time_horizon = time_horizons[site_ind] + 1
    current_offset_probs = offset_probs[[site_ind]]
    current_offset_projections = generate_nested_list(outer_dim = feature_num, inner_dim = time_horizon)
    
    for (feature_ind in seq_len(feature_num)){
      
      current_cfac = current_cfacs[[site_ind]][[feature_ind]]
      
      for (proj_yr in seq_len(time_horizon)){
        current_offset_projections[[feature_ind]][[proj_yr]] = array(0, dim(current_cfac))
        
        if (current_offset_probs[proj_yr] > 0){
          print('offset projections not working')
          stop()
          current_offset_proj = project_feature_layer(dynamics_type, 
                                                      update_dynamics_by_differential, 
                                                      current_site_scale_feature_layer, 
                                                      feature_dynamics_to_use, 
                                                      condition_class_mode_to_use, 
                                                      current_condition_class_characteristics, 
                                                      time_horizon, 
                                                      perform_dynamics_time_shift, 
                                                      time_fill, 
                                                      projection_yrs)
          
          current_offset_projections[[feature_ind]][[proj_yr]][proj_yr:time_horizon, ] = current_offset_proj 
        }
      }
    }
    offset_projections[[site_ind]] = current_offset_projections
  }
  
  return(offset_projections)
  
}

# 
# sum_offset_projs <- function(offset_projections, offset_probs, feature_num, time_horizons){
#   site_num = length(offset_projections)
#   summed_offset_projections = vector('list', site_num)
#   for (site_ind in seq_len(site_num)){
#     
#     summed_offset_projections[[site_ind]] = vector('list', feature_num)
#     current_offset_prob = offset_probs[[site_ind]]
#     current_offset_prob <- current_offset_prob*(current_offset_prob > 0)
#     
#     current_offset_proj = offset_projections[[site_ind]]
#     
#     for (feature_ind in seq_len(feature_num)){
#       current_offset_projections <- current_offset_proj[[feature_ind]]
#       current_offset_projections <- lapply(seq_along(current_offset_projections), function(i) current_offset_projections[[i]]*current_offset_prob[i])
#       summed_offset_projections[[site_ind]][[feature_ind]] = Reduce('+', current_offset_projections)
#     }
#   }
#   
#   return(summed_offset_projections)
# }

# 
# 
# sum_clearing_offsets <- function(cfacs_include_clearing, summed_offset_projections, feature_num){
#   site_num = length(cfacs_include_clearing)
#   cfacs_include_clearing_offsets = vector('list', site_num)
#   
#   for (site_ind in 1:site_num){
#     cfacs_include_clearing_offsets[[site_ind]] = vector('list', feature_num)
#   }
#   
#   for (site_ind in seq_len(site_num)){
#     if (length(summed_offset_projections[[site_ind]]) > 0 ){
#       for (feature_ind in seq_len(feature_num)){
#         cfacs_include_clearing_offsets[[site_ind]][[feature_ind]] = summed_offset_projections[[site_ind]][[feature_ind]] + cfacs_include_clearing[[site_ind]][[feature_ind]]
#       }
#     }
#   }
#   return(cfacs_include_clearing_offsets)
# }



