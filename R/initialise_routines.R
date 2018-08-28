# set of initialisation routines that are called for every simulation
build_params <- function(user_global_params = NULL, user_feature_params = NULL, user_simulation_params = NULL){
  
  params_object = list()
  global_params <- build_global_params(user_global_params)
  params_object$simulation_params_group = build_simulation_params_group(user_simulation_params)
  params_object$feature_params <- build_feature_params(user_feature_params)
  params_object$global_params <- save_params(global_params, params_object$simulation_params_group, params_object$feature_params)
  return(params_object)
}



build_global_params <- function(user_global_params){
  #' @import foreach
  #' @import doParallel
  #' @import abind
  #' @import pixmap
  
  default_global_params = initialise_default_global_params()
  
  if (!is.null(user_global_params) == TRUE){
    global_params <- overwrite_current_params(user_params = user_global_params, default_params = default_global_params)
    #check_global_params(global_params)
  } else {
    global_params = default_global_params
  }
  
  # run simulation with identical realisation instantiation
  if (global_params$set_seed == TRUE){
    seed=123
    flog.info('fixing random number seed to %d', 123)
    set.seed(seed)
  }
  
  
  
  global_params <- initialise_cores(global_params)
  global_params$strt = Sys.time()
  
  return(global_params)
}


build_simulation_params_group <- function(user_simulation_params){
  
  default_simulation_params = initialise_default_simulation_params()
  if (!is.null(user_simulation_params) == TRUE){  
    simulation_params <- overwrite_current_params(user_params = user_simulation_params, default_params = default_simulation_params)
    check_simulation_params(simulation_params)
  } else{
    simulation_params = default_simulation_params
  }
  
  simulation_params_object = build_simulation_variants(simulation_params)
  simulation_params_group <- lapply(seq_along(simulation_params_object$param_variants), 
                                    function(i) process_current_simulation_params(simulation_params_object$param_variants[[i]], simulation_params_object$common_params))
  
  return(simulation_params_group)
}


build_feature_params <- function(user_feature_params){
  default_feature_params = initialise_default_feature_params()
  
  if (!is.null(user_feature_params) == TRUE){  
    feature_params <- overwrite_current_params(user_params = user_feature_params, default_params = default_feature_params)
  } else {
    feature_params = default_feature_params
  }
}

build_input_data <- function(params_object, scenario_ind){
  
  simulation_data_object = list()
  simulation_data_object$simulation_params = params_object$simulation_params_group[[scenario_ind]]
  simulation_data_object$global_params = params_object$global_params 
  simulation_data_object$feature_params = select_feature_condition_class_bounds(params_object$feature_params, simulation_data_object$simulation_params) 
  
  
  # generate simulated feature
  if (simulation_data_object$global_params$run_from_simulated_data == TRUE) {
    current_filenames <- list.files(path = simulation_data_object$global_params$simulation_inputs_folder, all.files = FALSE,
                                    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                    include.dirs = FALSE, no.. = FALSE)
    if ((simulation_data_object$global_params$build_simulated_data == TRUE) | (length(current_filenames) == 0)){
      construct_simulated_data(simulation_data_object$feature_params, 
                               simulation_data_object$global_params$simulation_inputs_folder, 
                               simulation_data_object$global_params$simulation_params_folder, 
                               simulation_data_object$global_params$backup_simulation_inputs)
    }
  }
  
  if (!all(file.exists(simulation_data_object$global_params$feature_raster_files))){
    flog.error(paste('one or more feature raster files missing'))
  }
  
  feature_raster_layers = load_rasters(simulation_data_object$global_params$feature_raster_files, 
                                       features_to_use = simulation_data_object$simulation_params$features_to_use_in_simulation)

  initial_features = lapply(seq(dim(feature_raster_layers)[3]), function(i) raster_to_array(subset(feature_raster_layers, i)))
  
  if (simulation_data_object$feature_params$scale_features == TRUE){
    initial_features = scale_features(initial_features)
  }
  
  # list object containing feature values by feature layer for all
  # sites.  This is 3-level nested list, the top level is parcel index, for each
  # parcel there is sublist containing the value of each feature for each cell
  # in that parcel. So if there were 3 parcels and 2 features, it would be a 3
  # element list, where each element was subsequent list of length 2
  # containing the values for the 2 features in that parcel. The values of the
  # feature for the parcel are a vector of length given by the number of
  # pixels in the parcel.
  
  if (!file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_characteristics.rds')) || 
      (simulation_data_object$global_params$overwrite_site_characteristics == TRUE)){
    planning_units <- load_rasters(simulation_data_object$global_params$planning_units_raster, 'all')
    planning_units_array <- raster_to_array(planning_units)
    flog.info('building site characteristics object (this may take a while with large arrays) ..')
    simulation_data_object$site_characteristics <- build_site_characteristics(planning_units_array)
    
    flog.info('saving site characteristics object')
    saveRDS(object = simulation_data_object$site_characteristics, file = paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_characteristics.rds'))
    
  } else {
    simulation_data_object$site_characteristics = readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_characteristics.rds'))
  }
  
  if (!file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'feature_dynamics_modes.rds'))
      | (simulation_data_object$global_params$overwrite_condition_classes == TRUE)){
    background_condition_class_layers = build_condition_class_layers(initial_features, 
                                                                     simulation_data_object$global_params, 
                                                                     simulation_data_object$feature_params, 
                                                                     simulation_data_object$simulation_params)
    
    simulation_data_object$feature_dynamics_modes = split_modes(simulation_data_object$site_characteristics$land_parcels, background_condition_class_layers)
    saveRDS(object = simulation_data_object$feature_dynamics_modes, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'feature_dynamics_modes.rds'))
    
  } else {
    simulation_data_object$feature_dynamics_modes = readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'feature_dynamics_modes.rds'))
  }
  
  
  if ((!file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_features.rds')))
      | (simulation_data_object$global_params$overwrite_site_features == TRUE)){
    
    if (!exists('background_condition_class_layers')){
      background_condition_class_layers = build_condition_class_layers(initial_features, 
                                                                       simulation_data_object$global_params, 
                                                                       simulation_data_object$feature_params, 
                                                                       simulation_data_object$simulation_params)
    } 
    

    simulation_data_object$site_features <- separate_features_by_condition_class(initial_features, 
                                                           split_type = 'feature_vals',
                                                           store_zeros_as_sparse = simulation_data_object$global_params$store_zeros_as_sparse,
                                                           simulation_data_object$site_characteristics$land_parcels, 
                                                           background_condition_class_layers, 
                                                           simulation_data_object$feature_dynamics_modes)
    
    site_element_indexes_grouped_by_condition_classes <- separate_features_by_condition_class(initial_features, 
                                                         split_type = 'element_index',
                                                         store_zeros_as_sparse = simulation_data_object$global_params$store_zeros_as_sparse,
                                                         simulation_data_object$site_characteristics$land_parcels, 
                                                         background_condition_class_layers, 
                                                         simulation_data_object$feature_dynamics_modes)

    simulation_data_object$site_element_index_key = lapply(seq_along(simulation_data_object$site_features), 
                                                           function(i) lapply(seq_along(simulation_data_object$site_features[[i]]),
                                                                              function(j) match(simulation_data_object$site_characteristics$land_parcels[[i]], 
                                                                                                do.call(cbind, site_element_indexes_grouped_by_condition_classes[[i]][[j]]))))
    
    saveRDS(object = simulation_data_object$site_features, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_features.rds'))
    saveRDS(object = site_element_indexes_grouped_by_condition_classes, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_element_indexes_grouped_by_condition_classes.rds'))
    saveRDS(object = simulation_data_object$site_element_index_key, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_element_index_key.rds'))
    
  } else {
    simulation_data_object$site_features = readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'site_features.rds'))
  }
  
  if (simulation_data_object$feature_params$management_condition_class == 'background'){
    simulation_data_object$management_dynamics_modes = simulation_data_object$feature_dynamics_modes 
  } else {
    if (file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'management_condition_class_layers.rds'))){
      management_condition_class_layers <- readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'management_condition_class_layers.rds'))
    } else {
      flog.info('building management condition class layers, this may take a while..')
      management_condition_class_layers = build_modes(features_to_use = initial_features, 
                                                      condition_class_bounds = simulation_data_object$feature_params$management_condition_class_bounds, 
                                                      unique_site_vals = simulation_data_object$feature_params$unique_site_vals)
      
      #saveRDS(management_condition_class_modes_layers, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'management_condition_class_layers.rds'))
    }
    
    simulation_data_object$management_dynamics_modes = split_modes(simulation_data_object$site_characteristics$land_parcels, management_condition_class_layers)
  } 
  
  
  
  if ((!file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_dynamics.rds'))) |
      (simulation_data_object$global_params$overwrite_feature_dynamics == TRUE)){
    simulation_data_object$feature_dynamics <- build_dynamics(simulation_data_object$site_features,
                                                              features_to_use = seq_along(simulation_data_object$simulation_params$features_to_use_in_simulation),
                                                              simulation_data_object$feature_params$sample_background_dynamics,
                                                              simulation_data_object$feature_params$background_dynamics_type,
                                                              store_dynamics_as_differential = simulation_data_object$feature_params$background_update_dynamics_by_differential,
                                                              simulation_data_object$feature_params$unique_site_vals,
                                                              simulation_data_object$feature_params$dynamics_sample_type,
                                                              simulation_data_object$feature_params$background_dynamics_bounds, 
                                                              simulation_data_object$feature_dynamics_modes)

    if (any(is.na(unlist(simulation_data_object$feature_dynamics)))){
      flog.error('poorly defined feature_dynamics')
      stop()
    }
    saveRDS(simulation_data_object$feature_dynamics, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'feature_dynamics.rds'))
  } else {
    simulation_data_object$feature_dynamics <- readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'background_dynamics.rds'))
  }
  
  if (!(file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'management_dynamics.rds')))|
      (simulation_data_object$global_params$overwrite_management_dynamics == TRUE)){
    simulation_data_object$management_dynamics <- build_dynamics(simulation_data_object$site_features,
                                                                 features_to_use = simulation_data_object$simulation_params$features_to_use_in_offset_intervention,
                                                                 simulation_data_object$feature_params$sample_management_dynamics,
                                                                 simulation_data_object$feature_params$management_dynamics_type,
                                                                 store_dynamics_as_differential = FALSE,
                                                                 simulation_data_object$feature_params$unique_site_vals,
                                                                 simulation_data_object$feature_params$management_dynamics_sample_type,
                                                                 simulation_data_object$feature_params$management_dynamics_bounds, 
                                                                 simulation_data_object$management_dynamics_modes)
    
    if (any(is.na(unlist(simulation_data_object$management_dynamics)))){
      flog.error('poorly defined management_dynamics')
      stop()
    }
    saveRDS(simulation_data_object$management_dynamics, paste0(simulation_data_object$global_params$simulation_inputs_folder, 'management_dynamics.rds'))
    
  } else {
    simulation_data_object$management_dynamics <- readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'management_dynamics.rds'))
  }
  
  if (!(file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'dev_probability_list.rds'))) |
      (simulation_data_object$global_params$overwrite_dev_probability_list == TRUE)){
    flog.info('assigning equal development probability to all sites')
    simulation_data_object$dev_probability_list <- rep(list(1/length(simulation_data_object$site_characteristics$land_parcels)), length(simulation_data_object$site_characteristics$land_parcels))
    
  } else {
    simulation_data_object$dev_probability_list <- readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'dev_probability_list.rds'))
  }
  
  if (!(file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'offset_probability_list.rds'))) |
      (simulation_data_object$global_params$overwrite_offset_probability_list == TRUE)){
    flog.info('assigning equal offset probability to all sites')
    simulation_data_object$offset_probability_list <- rep(list(1/length(simulation_data_object$site_characteristics$land_parcels)), length(simulation_data_object$site_characteristics$land_parcels))
    
  } else {
    simulation_data_object$offset_probability_list <- readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'offset_probability_list.rds'))
    
  }
  if (!(file.exists(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'unregulated_probability_list.rds'))) |
      (simulation_data_object$global_params$overwrite_unregulated_probability_list == TRUE)){
    flog.info('assigning equal unregulated loss probability to all sites')
    simulation_data_object$unregulated_probability_list <- rep(list(1/length(simulation_data_object$site_characteristics$land_parcels)), length(simulation_data_object$site_characteristics$land_parcels))
    
  } else {
    simulation_data_object$unregulated_probability_list <- readRDS(paste0(simulation_data_object$global_params$simulation_inputs_folder, 'unregulated_probability_list.rds'))
    
  }
  return(simulation_data_object)
  
}




build_condition_class_layers <- function(initial_features, global_params, feature_params, simulation_params){
  
  load_condition_classes = FALSE
  if (length(global_params$condition_class_raster_files) == simulation_params$feature_num){
    if (all(file.exists(global_params$condition_class_raster_files))){
      load_condition_classes = TRUE
    }
  }
  
  if (load_condition_classes == TRUE){
    
    background_condition_class_layers <- load_rasters(global_params$condition_class_raster_files, 
                                                      features_to_use = simulation_params$features_to_use_in_simulation)
    background_condition_class_layers = lapply(seq(dim(background_condition_class_layers)[3]), function(i) raster_to_array(subset(background_condition_class_layers, i)))
    
  } else {
    flog.info('building condition classes ..')
    background_condition_class_layers = build_modes(features_to_use = initial_features, 
                                                    condition_class_bounds = feature_params$initial_condition_class_bounds, 
                                                    unique_site_vals = feature_params$unique_site_vals)
    
    flog.info('writing condition classes ..')
    
    background_condition_class_rasters = lapply(seq_along(background_condition_class_layers), function(i) raster(background_condition_class_layers[[i]]))
    
    lapply(seq_along(background_condition_class_layers), function(i) writeRaster(background_condition_class_rasters[[i]], 
                                                                                 paste0(global_params$simulation_inputs_folder, 'condition_class_raster_', 
                                                                                        formatC(simulation_params$features_to_use_in_simulation[i], width = global_params$integer_placeholder_width, format = "d", flag = "0"), '.tif'),
                                                                                 overwrite = TRUE))
    
  }
  
  return(background_condition_class_layers)
}

build_output_data <- function(simulation_data_object){
  output_data = list()
  output_data$index_object = initialise_index_object(simulation_data_object$site_characteristics, 
                                                     simulation_data_object$site_features, 
                                                     simulation_data_object$simulation_params,
                                                     offset_indexes_to_exclude = which(unlist(simulation_data_object$offset_probability_list) == 0), 
                                                     dev_indexes_to_exclude = which(unlist(simulation_data_object$dev_probability_list) == 0), 
                                                     unregulated_indexes_to_exclude = which(unlist(simulation_data_object$unregulated_probability_list) == 0))
  interventions = vector('list', 5)
  names(interventions) = names(output_data$index_object$site_indexes_used)
  output_data$interventions = interventions
  output_data$offset_pool_object <- list()
  current_credit = matrix(rep(0, length(simulation_data_object$simulation_params$features_to_use_in_offset_calc)), ncol = length(simulation_data_object$simulation_params$features_to_use_in_offset_calc))
  if (simulation_data_object$simulation_params$use_offset_metric == TRUE){
    current_credit = user_transform_function(current_credit, simulation_data_object$simulation_params$transform_params)
  }
  output_data$current_credit = current_credit
  output_data$credit_match_flag = FALSE
  return(output_data)
} 

select_feature_condition_class_bounds <- function(feature_params, current_simulation_params){
  feature_params_to_use = feature_params
  feature_params_to_use$management_condition_class_bounds = feature_params$management_condition_class_bounds[current_simulation_params$features_to_use_in_simulation]
  feature_params_to_use$condition_class_bounds = feature_params$condition_class_bounds[current_simulation_params$features_to_use_in_simulation]
  feature_params_to_use$initial_condition_class_bounds = feature_params$initial_condition_class_bounds[current_simulation_params$features_to_use_in_simulation]
  feature_params_to_use$background_dynamics_bounds = feature_params$background_dynamics_bounds[current_simulation_params$features_to_use_in_simulation]
  feature_params_to_use$management_dynamics_bounds = feature_params$management_dynamics_bounds[current_simulation_params$features_to_use_in_simulation]
  return(feature_params_to_use)
}

sample_current_dynamics <- function(current_feature_dynamics_group, current_mode, current_feature_val, store_dynamics_as_differential, sample_dynamics, dynamics_sample_type){
  
  if (current_mode == 0){
    current_feature_dynamics = array(0, length(current_feature_dynamics_group[[1]]$best_estimate))
    return(current_feature_dynamics)
  } else {
    current_feature_dynamics_set = current_feature_dynamics_group[[current_mode]]
    
    if (sample_dynamics == FALSE){
      current_feature_dynamics = current_feature_dynamics_set$best_estimate
      return(current_feature_dynamics)
    } 
  }
  
  if (dynamics_sample_type == 'by_initial_value'){
    current_discriminator = current_feature_val - current_feature_dynamics_set$best_estimate[1]
  } else if (dynamics_sample_type == 'by_distribution'){
    current_discriminator = runif(n = 1, min = -1, max = 1)
  }
  
  if (current_discriminator >= 0){
    factor_to_use = current_feature_dynamics_set$upper_bound - current_feature_dynamics_set$best_estimate
  } else {
    factor_to_use = current_feature_dynamics_set$best_estimate - current_feature_dynamics_set$lower_bound
  }
  
  if (dynamics_sample_type == 'by_initial_value'){
    if (factor_to_use[1] == 0){
      flog.error('dynamics bounds are poorly defined - redefine or set dynamics_sample_type to "by_distribution"')
    }
    current_discriminator = current_discriminator/(factor_to_use[1])
  }
  
  current_feature_dynamics = current_feature_dynamics_set$best_estimate + current_discriminator*factor_to_use  
  
  if (store_dynamics_as_differential == TRUE){
    current_feature_dynamics = diff(current_feature_dynamics)
    #current_feature_dynamics = cumsum(diff(current_feature_dynamics))
  } 
  
  return(current_feature_dynamics)
}


build_dynamics <- function(site_features_to_use, features_to_use, sample_dynamics, dynamics_type, store_dynamics_as_differential, 
                           unique_site_vals, dynamics_sample_type, feature_dynamics_bounds, feature_dynamics_modes){
  
  if (dynamics_type == 'element_scale'){
    
    dynamics_set = lapply(seq_along(site_features_to_use), 
                          function(i) lapply(features_to_use,
                                             function(j) lapply(seq_along(site_features_to_use[[i]][[j]]), 
                                                                function(k) sample_current_dynamics(feature_dynamics_bounds[[j]],
                                                                                                    feature_dynamics_modes[[i]][[j]][k],
                                                                                                    site_features_to_use[[i]][[j]][k],
                                                                                                    store_dynamics_as_differential, 
                                                                                                    sample_dynamics,
                                                                                                    dynamics_sample_type)  )))
  } else if (dynamics_type == 'site_scale'){
    
    if (unique_site_vals == TRUE){
      
      dynamics_set = lapply(seq_along(feature_dynamics_modes), 
                            function(i) lapply(features_to_use,
                                               function(j) lapply(seq_along(feature_dynamics_modes[[i]][[j]]), 
                                                                  function(k) sample_current_dynamics(feature_dynamics_bounds[[j]],
                                                                                                      feature_dynamics_modes[[i]][[j]][k],
                                                                                                      mean(site_features_to_use[[i]][[j]][[k]]),
                                                                                                      store_dynamics_as_differential, 
                                                                                                      sample_dynamics,
                                                                                                      dynamics_sample_type) )))
      
      #       dynamics_set = lapply(seq_along(site_features_to_use), 
      #                             function(i) lapply(seq_along(site_features_to_use[[i]]),
      #                                                function(j) lapply(seq_along(unique(feature_dynamics_modes[[i]][[j]])), 
      #                                                                   function(k) sample_current_dynamics(feature_dynamics_bounds[[j]],
      #                                                                                                       unique(feature_dynamics_modes[[i]][[j]])[k],
      #                                                                                                       mean(site_features_to_use[[i]][[j]][which(feature_dynamics_modes[[i]][[j]] 
      #                                                                                                                                                       == unique(feature_dynamics_modes[[i]][[j]])[k])]),
      #                                                                                                       store_dynamics_as_differential, 
      #                                                                                                       sample_dynamics,
      #                                                                                                       dynamics_sample_type) )))
      
    } else {
      dynamics_set = lapply(seq_along(feature_dynamics_modes), 
                            function(i) lapply(features_to_use,
                                               function(j) sample_current_dynamics(feature_dynamics_bounds[[j]],
                                                                                   feature_dynamics_modes[[i]][[j]],
                                                                                   unique(site_features_to_use[[i]][[j]]),
                                                                                   store_dynamics_as_differential, 
                                                                                   sample_dynamics,
                                                                                   dynamics_sample_type)))
    }
  }
  return(dynamics_set)
}


find_current_mode <- function(current_feature_val, current_condition_class_bounds){
  
  if (length(current_feature_val) > 0){
    
    mode_discriminator = current_feature_val
    if (mode_discriminator == 0){
      current_mode = 0
    } else{
      
      current_modes = lapply(seq_along(current_condition_class_bounds), 
                             function(i) ( (mode_discriminator >= min(current_condition_class_bounds[[i]]) && mode_discriminator <= max(current_condition_class_bounds[[i]])) ))
      
      mode_ind_to_use = which(unlist(current_modes) > 0)
      
      if (length(mode_ind_to_use) == 0){
        flog.error('condition class bounds do not match site values')
        stop()
      } else if (length(mode_ind_to_use) == 1){
        current_mode = mode_ind_to_use
      } else {
        current_mode = sample(mode_ind_to_use, 1)
      }
    } 
    
  } else {
    current_mode = 0
  }
  return(current_mode)
}



build_modes <- function(features_to_use, condition_class_bounds, unique_site_vals){
  
  feature_dynamics_modes = lapply(seq_along(features_to_use),  
                                  function(i) matrix(sapply(features_to_use[[i]], find_current_mode, condition_class_bounds[[i]]), dim(features_to_use[[i]])))
  
  return(feature_dynamics_modes)
}




build_splines <- function(feature_dynamics){
  spline_set = lapply(seq_along(feature_dynamics), 
                      function(i) lapply(seq_along(feature_dynamics[[i]]),
                                         function(j) smooth.spline(1:length(feature_dynamics[[i]][[j]]), feature_dynamics[[i]][[j]])))
  
  #   spline_object$inv_spline_set = lapply(seq_along(spline_set), 
  #                       function(i) lapply(seq_along(spline_set[[i]]),
  #                                          function(j) splinefun(spline_set[[i]][[j]]$y, spline_set[[i]][[j]]$x)))
  
  #   spline_object = lapply(seq_along(feature_dynamics), 
  #                          function(i) lapply(seq_along(feature_dynamics[[i]]),
  #                                          function(j) splinefun(spline_set[[i]][[j]]$x, spline_set[[i]][[j]]$y)))
  #                                          
  #inv_spline = splinefun(current_spline_fit$y, current_spline_fit$x)
  return(spline_set)
}


initialise_cores <- function(global_params){
  max_crs = parallel::detectCores(all.tests = FALSE, logical = TRUE)
  
  if (is.character(global_params$number_of_cores)){
    if (global_params$number_of_cores == 'all'){
      current_crs = max_crs
    }
    else {
      flog.error('specified number of cores must be set to "all" or positive integer, currently set to %s', global_params$number_of_cores)
      stop()
    }
    
  } else {
    if ( (global_params$number_of_cores %% 1 != 0) ||(global_params$number_of_cores < 1) ){
      flog.error('specified number of cores must be set to "all" or positive integer currently set to %s', global_params$number_of_cores)
      stop()
    } else { 
      if (global_params$number_of_cores > max_crs){
        flog.warn('specified %s of cores is greater than %s available cores', global_params$number_of_cores, max_crs)
        current_crs = max_crs
      } else if ((global_params$number_of_cores >= 1) & (global_params$number_of_cores <= max_crs)){
        current_crs = global_params$number_of_cores
      } 
    }
  }
  
  global_params$number_of_cores = current_crs
  
  clstr<-parallel::makeCluster(current_crs, output = "")  # allow parallel workers on n = global_params$number_of_cores processors
  registerDoParallel(clstr)
  global_params$clstr = clstr
  return(global_params)
}


split_vector <- function(N, M, sd, min_width) {               # make a vector of length N where the elements sum to M and with values normally distributed about M/N with std dev "sd"
  
  vec <- rnorm(N, M/N, sd)                                    # select vector from normal distribution
  vec <- round(vec / sum(vec) * M)                             
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  while (any(vec <= min_width)) {
    negs <- vec <= min_width
    pos  <- vec > min_width
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}


check_simulation_params <- function(simulation_params){
  
  offset_action_set = simulation_params$offset_action_params
  valid_offset_calc_type = c('net_gains', 'restoration_gains', 'avoided_condition_decline', 'avoided_loss',
                             'protected_condition', 'current_condition', 'restored_condition')
  
  
  for (offset_action_ind in seq_along(offset_action_set)){
    current_offset_calc_type = offset_action_set[[offset_action_ind]][1]
    check_current_param(current_offset_calc_type, valid_offset_calc_type)
    current_offset_action = offset_action_set[[offset_action_ind]][2]
    if (current_offset_calc_type == 'avoided_condition_decline'){
      valid_offset_action_type = 'maintain'
    } else if (current_offset_calc_type %in% c('net_gains', 'restoration_gains', 'restored_condition')){
      valid_offset_action_type = 'restore'
    } else if (current_offset_calc_type %in% c('current_condition')){
      valid_offset_action_type = c('protect', 'maintain')
    } else if (current_offset_calc_type %in% c('avoided_loss')){
      valid_offset_action_type = c('protect', 'maintain')
    }
    check_current_param(current_offset_action, valid_offset_action_type)
  }
  valid_dev_calc_type = c('future_condition', 'current_condition')
  check_current_param(simulation_params$dev_calc_type, valid_dev_calc_type)
  
}



check_current_param <- function(current_calc_type, valid_offset_calc_type){
  if(length(current_calc_type) == 0){
    flog.error(cat('\n parameter ', valid_offset_calc_type, 'not set'))
    stop()
  }
  discriminant = setdiff(current_calc_type, valid_offset_calc_type)
  if ((length(discriminant) > 0)){
    flog.error(cat('\n invalid parameter ', discriminant))
    stop()
  }
}


overwrite_current_params <- function(user_params, default_params){
  
  param_matches = match(names(user_params), names(default_params))
  obsolete_param_inds = which(is.na(param_matches))
  
  if (length(obsolete_param_inds) > 0){
    flog.error(cat('remove or rename in user params file: \n', names(user_params[obsolete_param_inds]), '\n'))
    stop()
  }
  updated_params = default_params
  param_inds_to_use = seq_along(updated_params)
  updated_params[param_matches] = user_params
  
  return(updated_params)
}


select_feature_subset <- function(input_object, features_to_use){
  if (length(input_object[[1]]) < features_to_use[length(features_to_use)]){
    flog.error( cat('\nERROR: features in simulation_params$features_to_use do not match initial_features_dimensions'))
    stop()
  }
  input_object <- lapply(seq_along(input_object),
                         function(i) (input_object[[i]][features_to_use]))
  return(input_object)
}


write_folder <- function(current_folder){
  if (!file.exists(current_folder)){
    dir.create(current_folder, recursive = TRUE)
  }
  return(current_folder)
}


find_current_run <- function(base_run_folder){
  filenames = list.files(path = base_run_folder, all.files = FALSE,
                         full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                         include.dirs = FALSE, no.. = FALSE, pattern='^[0-9]{1,45}$')
  current_run = as.numeric(filenames[length(filenames)])
  return(current_run)
}

save_params <- function(global_params, simulation_params_group, feature_params){
  
  if (global_params$simulation_folder != 'default'){
    simulation_folder = write_folder(global_params$simulation_folder)
    simulation_inputs_folder = write_folder(paste0(simulation_folder, '/simulation_inputs/'))
    base_run_folder = paste0(simulation_folder, '/simulation_runs/')
  } else {
    simulation_inputs_folder = ('simulation_inputs/')
    base_run_folder = ('simulation_runs/')
  }
  
  current_run = find_current_run(base_run_folder)
  
  if ((global_params$unique_simulation_folder) & (length(current_run) > 0)){
    current_run = current_run + 1
  } else {
    current_run = 1
  } 
  
  global_params$run_folder = write_folder(paste0(base_run_folder, formatC(current_run, width = global_params$integer_placeholder_width, format = "d", flag = "0"), '/'))
  global_params$output_folder = write_folder(paste0(global_params$run_folder, '/simulation_outputs/'))
  flog.info('writing simulation outputs into %s', global_params$run_folder)
  
  if (class(global_params$scenario_subset) == 'character'){
    if (global_params$scenario_subset == 'all'){
      global_params$scenario_subset = seq_along(simulation_params_group)
    } else {
      flog.error('incorrect setting for scenario_subset - must be vector of natural numbers or "all"')
    }
  }
  
  for (scenario_ind in global_params$scenario_subset){
    write_folder(paste0(global_params$output_folder, '/scenario_', formatC(scenario_ind, width = global_params$integer_placeholder_width, format = "d", flag = "0"), '/'))
  }
  
  global_params$simulation_params_folder = write_folder(paste0(global_params$run_folder, '/simulation_params/'))
  global_params_file = paste0(global_params$simulation_params_folder,  'global_params')
  browser()
  saveRDS(global_params, paste0(global_params$simulation_params_folder,  'global_params.rds'))
  
  dump('global_params', paste0(global_params$simulation_params_folder,  'global_params.R'), control = NULL)
  
  saveRDS(feature_params, paste0(global_params$simulation_params_folder,  'feature_params.rds'))
  dump('feature_params', paste0(global_params$simulation_params_folder,  'feature_params.R'), control = NULL)

  for (scenario_ind in global_params$scenario_subset){
    current_simulation_params = simulation_params_group[[scenario_ind]]
    simulation_params_file = paste0(global_params$simulation_params_folder,  
                                    'scenario_', formatC(scenario_ind, width = global_params$integer_placeholder_width, format = "d", flag = "0"), '_simulation_params')
    
    saveRDS(current_simulation_params, paste0(simulation_params_file, '.rds'))
    dump('current_simulation_params', paste0(simulation_params_file, '.R'), control = NULL)
  }
  
  global_params$simulation_inputs_folder = simulation_inputs_folder
  global_params$collated_folder = write_folder(paste0(global_params$run_folder, 'collated_outputs/'))
  
  return(global_params)
}



generate_simulation_combs <- function(simulation_params_group){
  
  param_inds <- lapply(seq_along(simulation_params_group), function(i) 1:length(simulation_params_group[[i]]))
  param_combs <- expand.grid(param_inds)
  
  return(param_combs)
  
}

# build_simulation_params <- function(common_params, simulation_params_group, current_simulation_param_inds){
#   
#   current_simulation_params_variant <- lapply(seq_along(simulation_params_group), function(i) simulation_params_group[[i]][[current_simulation_param_inds[i] ]])
#   current_simulation_params <- append(common_params, current_simulation_params_variant)
#   names(current_simulation_params) <- append(names(common_params), names(simulation_params_group))
#   
#   return(current_simulation_params)
#   
# }


process_current_simulation_params <- function(current_simulation_params, common_params){
  
  current_simulation_params = append(current_simulation_params, common_params)
  
  if (class(current_simulation_params$features_to_use_in_simulation) == "character"){
    browser()
  } 
  
  current_simulation_params$feature_num = length(current_simulation_params$features_to_use_in_simulation)
  
  if (current_simulation_params$use_offset_bank == TRUE){
    current_simulation_params$offset_time_horizon_type = 'current'  # 'current' - used for banking only - determine accrued offset gains till current year.
  } else {
    current_simulation_params$offset_time_horizon_type = 'future'  #'future' - project from time of development to offset time horizon
  }
  
  current_simulation_params$features_to_use_in_offset_calc = match(current_simulation_params$features_to_use_in_offset_calc, current_simulation_params$features_to_use_in_simulation)
  current_simulation_params$features_to_use_in_offset_intervention = match(current_simulation_params$features_to_use_in_offset_intervention, current_simulation_params$features_to_use_in_simulation)
  current_simulation_params$offset_calc_type = current_simulation_params$offset_action_params[1]
  current_simulation_params$offset_action_type = current_simulation_params$offset_action_params[2]
  
  current_simulation_params$use_parcel_sets = (current_simulation_params$offset_bank_type == 'parcel_set') || (current_simulation_params$use_offset_bank == FALSE)
  if ((current_simulation_params$offset_bank_type == 'parcel_set') || (current_simulation_params$use_offset_bank == FALSE)){
    current_simulation_params$match_type = 'parcel_set'
  }
  if ((current_simulation_params$offset_calc_type == 'current_condition') && (current_simulation_params$dev_calc_type == 'current_condition')){
    current_simulation_params$use_offset_time_horizon = FALSE
  } else {current_simulation_params$use_offset_time_horizon = TRUE}
  
  if( (current_simulation_params$offset_calc_type == 'protected_condition') || (current_simulation_params$offset_calc_type == 'current_condition') || (current_simulation_params$offset_calc_type == 'restored_condition') ){
    current_simulation_params$offset_cfacs_flag = FALSE
  } else{
    current_simulation_params$offset_cfacs_flag = TRUE
  }
  
  if ( (current_simulation_params$offset_calc_type == 'restoration_gains') || (current_simulation_params$offset_calc_type == 'net_gains')
       || (current_simulation_params$offset_calc_type == 'restored_condition')){
    current_simulation_params$offset_restoration_flag = TRUE
  } else {
    current_simulation_params$offset_restoration_flag = FALSE
  }
  
  if( (current_simulation_params$dev_calc_type == 'future_condition')){
    current_simulation_params$dev_cfacs_flag = TRUE
  } else{
    current_simulation_params$dev_cfacs_flag = FALSE
  }
  
  if (current_simulation_params$use_offset_bank == TRUE){
    current_simulation_params$banked_offset_vec = build_stochastic_intervention(time_steps = simulation_params$time_steps,
                                                                                current_simulation_params$offset_bank_start,
                                                                                current_simulation_params$offset_bank_end,
                                                                                current_simulation_params$offset_bank_num,
                                                                                sd = 1)
  } else {
    current_simulation_params$banked_offset_vec = list()
  }
  
  if (current_simulation_params$dev_counterfactual_adjustment == 'as_offset'){
    current_simulation_params$include_potential_developments_in_dev_calc = current_simulation_params$include_potential_developments_in_offset_calc
    current_simulation_params$include_potential_offsets_in_dev_calc = current_simulation_params$include_potential_offsets_in_offset_calc
    current_simulation_params$include_unregulated_loss_in_dev_calc = current_simulation_params$include_unregulated_loss_in_offset_calc
  } else {
    flog.info('using independent adjustment of cfacs in development impact calculation')
  }
  current_simulation_params$adjust_offset_cfacs_flag = any(c(current_simulation_params$include_potential_developments_in_offset_calc,
                                                             current_simulation_params$include_potential_offsets_in_offset_calc,
                                                             current_simulation_params$include_unregulated_loss_in_offset_calc) == TRUE)
  current_simulation_params$adjust_dev_cfacs_flag = any(c(current_simulation_params$include_potential_developments_in_dev_calc,
                                                          current_simulation_params$include_potential_offsets_in_dev_calc,
                                                          current_simulation_params$include_unregulated_loss_in_dev_calc) == TRUE)
  
  
  # select subset of feature layers to use in current simulation 
  # (e.g. if there 100 layers just run with 10 of them)
  return(current_simulation_params)
  
}


build_simulation_variants <- function(simulation_params){
  
  variant_cond = unlist(lapply(seq_along(simulation_params), function(i) (is.list(simulation_params[[i]]))))
  
  indexes_to_vary = which(variant_cond)
  variants = simulation_params[indexes_to_vary]
  common_params = simulation_params[which(!variant_cond)]
  
  if (length(indexes_to_vary) > 0){
    simulation_combs <- generate_simulation_combs(variants)  #generate all combinations of offset programs
    simulation_num = dim(simulation_combs)[1] #how many combinations there are in total
  } else {
    simulation_num = 1
  }
  simulation_params_group = vector('list', simulation_num)
  
  param_variants = lapply(seq(simulation_num), function(i)  
    build_current_variant(current_variant_indexes = unlist(simulation_combs[i, ]), variants))
  simulation_params_object = list()
  simulation_params_object$param_variants = param_variants
  simulation_params_object$common_params = common_params
  
  return(simulation_params_object)
  
}

build_current_variant <- function(current_variant_indexes, variants){
  current_simulation_params_variant <- lapply(seq_along(variants), function(i) variants[[i]][[current_variant_indexes[i] ]])
  names(current_simulation_params_variant) = names(variants)
  return(current_simulation_params_variant)
}



parcel_set_list_names <- function(){
  list_names = c("site_indexes", "parcel_num_remaining", "offset_yrs", "parcel_ecologies", "parcel_sums_at_offset", "cfac_trajs", "parcel_vals_used",
                 "restoration_vals", "cfac_vals")
  return(list_names)
}

#' @export
build_stochastic_intervention <- function(time_steps, intervention_start, intervention_end, intervention_num, sd){
  intervention_vec = array(0, time_steps)
  intervention_vec[intervention_start:intervention_end] = split_vector((intervention_end - intervention_start + 1), intervention_num, sd, min_width = -1)
  return(intervention_vec)
}

split_vector <- function(N, M, sd, min_width) {               # make a vector of length N where the elements sum to M and with values normally distributed about M/N with std dev "sd"
  
  vec <- rnorm(N, M/N, sd)                                    # select vector from normal distribution
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  while (any(vec <= min_width)) {
    negs <- vec <= min_width
    pos  <- vec > min_width
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}


initialise_shape_parcels <- function(feature_params){
  parcels = list()
  parcels$landscape_dims = c(feature_params$feature_size, feature_params$feature_size)
  parcel_num_x = feature_params$parcel_num_x   #length in parcels of array in x
  parcel_num_y = feature_params$parcel_num_y #length in parcels of array in y
  parcel_vx = split_vector(parcel_num_x, feature_params$feature_size, sd = 5, min_width = 3) # make normally distributed vector that sums to feature size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, feature_params$feature_size, sd = 5, min_width = 3) # as above for y
  
  pixel_indexes = 1:(feature_params$feature_size*feature_params$feature_size)     #index all elements of feature array
  dim(pixel_indexes) = c(feature_params$feature_size, feature_params$feature_size)  # arrange feature array index vector into array of landscape dimensions
  land_parcels = mcell(pixel_indexes, parcel_vx, parcel_vy) #split the feature array into a series of subarrays with dimensions sz_x by sz_y
  land_parcel_num = length(land_parcels$elements) #total number of parcels
  site_indexes = 1:land_parcel_num #index all parcels
  dim(site_indexes) = c(parcel_num_y, parcel_num_x) #arrange indicies into array with dimensions of land parcels
  parcels$site_indexes = site_indexes
  parcels$land_parcel_num = land_parcel_num
  parcels$land_parcels = land_parcels$elements
  parcels$land_parcel_dims = land_parcels$dims
  parcels$parcel_vx = parcel_vx
  parcels$parcel_vy = parcel_vy
  
  return(parcels)
}



mcell <- function(x, vx, vy){       #used to break up array into samller set of sub arrays defined by vx and vy that fit together to give input array
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  B = vector('list', rows*cols)   # make an array composed of lists with dimenisons that define the land parcels/regions. The list format allows arrays of different sizes to be stored
  colStart = 0
  for (i in seq_len(cols)){       # run down through the columns of input array
    rowStart = 0
    for (j in seq_len(rows)){ #group elements of input array into sub arrays and assign to B
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



initialise_index_object <- function(site_characteristics, site_features_initial, simulation_params, offset_indexes_to_exclude, dev_indexes_to_exclude, unregulated_indexes_to_exclude){
  
  index_object = list()
  index_object$banked_offset_pool = list()
  index_object$site_indexes_used = vector('list', 5)
  names(index_object$site_indexes_used) = c('offsets_object', 'offset_bank_object', 'dev_object', 'credit_object', 'unregulated_loss_object')
  
  index_object$available_indexes = list()
  
  index_object$available_indexes$offsets = set_available_indexes(global_indexes = site_characteristics$site_indexes, 
                                                                 offset_indexes_to_exclude, 
                                                                 site_characteristics$land_parcels, 
                                                                 site_features_initial, 
                                                                 screen_site_zeros = simulation_params$screen_offset_zeros,
                                                                 min_site_screen_size = simulation_params$min_site_screen_size,
                                                                 max_site_screen_size_quantile = simulation_params$max_site_screen_size_quantile,
                                                                 simulation_params$features_to_use_in_offset_calc)
  
  index_object$available_indexes$devs = set_available_indexes(global_indexes = site_characteristics$site_indexes, 
                                                              dev_indexes_to_exclude,
                                                              site_characteristics$land_parcels, 
                                                              site_features_initial, 
                                                              screen_site_zeros = simulation_params$screen_dev_zeros,
                                                              min_site_screen_size = simulation_params$min_site_screen_size,
                                                              max_site_screen_size_quantile = simulation_params$max_site_screen_size_quantile,
                                                              simulation_params$features_to_use_in_offset_calc)
  
  index_object$available_indexes$unregulated_loss = set_available_indexes(global_indexes = site_characteristics$site_indexes, 
                                                              indexes_to_exclude = unregulated_indexes_to_exclude,
                                                              site_characteristics$land_parcels, 
                                                              site_features_initial, 
                                                              screen_site_zeros = TRUE,
                                                              min_site_screen_size = simulation_params$min_site_screen_size,
                                                              max_site_screen_size_quantile = simulation_params$max_site_screen_size_quantile,
                                                              simulation_params$features_to_use_in_offset_calc)
  return(index_object)
}


set_available_indexes <- function(global_indexes, indexes_to_exclude, land_parcels, initial_features, screen_site_zeros, min_site_screen_size, max_site_screen_size_quantile, 
                                  features_to_use_in_offset_calc){
  
  if (screen_site_zeros == TRUE){

    initial_parcel_sums = lapply(seq_along(initial_features), 
                                 function(i) lapply(seq_along(initial_features[[i]]), 
                                                    function(j) do.call(sum, lapply(initial_features[[i]][[j]], sum) ) ))
    
    zeros_to_exclude = which(unlist(lapply(seq_along(initial_parcel_sums), 
                                           function(i) do.call(sum, initial_parcel_sums[[i]][features_to_use_in_offset_calc]) == 0)))
    indexes_to_exclude = unique(c(indexes_to_exclude, zeros_to_exclude))
  }
  
  site_element_num <- unlist(lapply(seq_along(land_parcels), function(i) length(land_parcels[[i]])))
  
  if (min_site_screen_size > 0){
    smalls_to_exclude = which(site_element_num < min_site_screen_size)
    indexes_to_exclude = unique(c(indexes_to_exclude, smalls_to_exclude))
  } 
  
  if (max_site_screen_size_quantile < 1){
    bigs_to_exclude = which(site_element_num > quantile(site_element_num, probs = max_site_screen_size_quantile))
    indexes_to_exclude = unique(c(indexes_to_exclude, bigs_to_exclude))
  }
  
  inds_to_remove = which(global_indexes %in% indexes_to_exclude)
  available_indexes = global_indexes
  
  if (length(inds_to_remove) > 0){
    available_indexes = available_indexes[-inds_to_remove]
  }
  
  return(available_indexes)
}

#' @export
scale_features <- function(features){
  flog.info('scaling features')
  scaled_features <- list_of_zeros(length(features), dim(features[[1]])) 
  
  for (feature_ind in seq_along(features)){
    if (max(features[[feature_ind]]) > 0){
      scaled_features[[feature_ind]] = features[[feature_ind]]/max(features[[feature_ind]])
    }
  }
  
  return(scaled_features)
}

split_modes <- function(land_parcels, feature_modes_layer){
  feature_condition_class_modes = lapply(seq_along(land_parcels), 
                                         function(i) lapply(seq_along(feature_modes_layer), 
                                                            function(j) unique(feature_modes_layer[[j]][ land_parcels[[i]]])))
  return(feature_condition_class_modes)
}

separate_features_by_condition_class <- function(features, split_type, store_zeros_as_sparse, land_parcels, feature_modes_layer, condition_class_modes){
  
  if (split_type == 'feature_vals'){
    group_to_split = lapply(seq_along(land_parcels), 
                               function(i) lapply(seq_along(features), 
                                                  function(j) features[[j]][ land_parcels[[i]] ] ))
  
    split_feature_group = lapply(seq_along(land_parcels), 
                                  function(i) lapply(seq_along(condition_class_modes[[i]]), 
                                                     function(j) lapply(condition_class_modes[[i]][[j]], 
                                                                        function(k) split_site_feature(group_to_split[[i]][[j]], 
                                                                                                       site_feature_modes_layer = feature_modes_layer[[j]][ land_parcels[[i]] ], 
                                                                                                       k, 
                                                                                                       store_zeros_as_sparse))))
  } else {
    
    split_feature_group = lapply(seq_along(land_parcels), 
                               function(i) lapply(seq_along(condition_class_modes[[i]]), 
                                                  function(j) lapply(condition_class_modes[[i]][[j]], 
                                                                     function(k) as.matrix(split_site_feature(land_parcels[[i]], 
                                                                                                    site_feature_modes_layer = feature_modes_layer[[j]][ land_parcels[[i]] ], 
                                                                                                    k, 
                                                                                                    store_zeros_as_sparse)))))
  }
  
  return(split_feature_group)
}

split_site_feature <- function(current_site_feature, site_feature_modes_layer, current_condition_class_mode, store_zeros_as_sparse){

  if (current_condition_class_mode == 0){

    if (store_zeros_as_sparse == TRUE){
      current_feature_condition_class = Matrix(current_site_feature[which(site_feature_modes_layer == current_condition_class_mode)], nrow = 1, sparse = TRUE)
    } else {
      current_feature_condition_class = matrix(current_site_feature[which(site_feature_modes_layer == current_condition_class_mode)], nrow = 1)
    }
  } else{
    current_feature_condition_class = matrix(current_site_feature[which(site_feature_modes_layer == current_condition_class_mode)], nrow = 1)
  }
  return(current_feature_condition_class)
}

#' @export
build_site_characteristics <- function(planning_units_array){
  
  site_ID_vals = unique(as.vector(planning_units_array))
  land_parcels <- lapply(seq_along(site_ID_vals), function(i) which(planning_units_array == site_ID_vals[i]))
  
  site_characteristics = list()
  site_characteristics$landscape_dims = dim(planning_units_array)
  site_characteristics$site_indexes = seq_along(land_parcels)
  site_characteristics$land_parcel_num = length(land_parcels)
  site_characteristics$land_parcels = land_parcels
  site_characteristics$parcel_array = planning_units_array
  site_characteristics$site_ID_vals = site_ID_vals
  return(site_characteristics)
}


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


#' @export 
write_folder <- function(current_folder){
  if (!file.exists(current_folder)){
    dir.create(current_folder)
  }
  return(current_folder)
}



#' @export
shp_to_raster <- function(shp, raster_dims){
  r <- raster(ncol=raster_dims[2], nrow=raster_dims[1])
  extent(r) <- extent(shp)
  raster_object <- rasterize(shp, r)
  return(raster_object)
}

#' @export
load_rasters <- function(current_filenames, features_to_use){
  if (class(features_to_use) == "character"){
    if (features_to_use == 'all'){
      features_to_use = seq_along(current_filenames)
    } else {
      flog.error('raster features poorly defined in params')
    }
    
  }
  for (feature_ind in seq_along(features_to_use)){
    
    current_raster = raster(current_filenames[features_to_use[feature_ind]])
    if (feature_ind == 1){
      raster_stack = current_raster
    } else{
      raster_stack = stack(raster_stack, current_raster)
    }
  }
  return(raster_stack) 
  
}

#' @export
raster_to_array <- function(raster_object){
  raster_array = raster::as.matrix(raster_object, ncol = ncol(raster_object))
  raster_array[is.na(raster_array)] = 0
  return(raster_array)
}

#' @export
read_pnm_layer <- function(filename){
  img = read.pnm(file = filename, cellres = 1)
  array_to_use = img@grey
  return(array_to_use)
}

#' @export
save_simulation_inputs <- function(objects_to_save, simulation_inputs_folder){
  write_folder(simulation_inputs_folder)
  filenames_to_save = names(objects_to_save)
  for (file_ind in seq_along(objects_to_save)){
    saveRDS(objects_to_save[[file_ind]], paste0(simulation_inputs_folder, filenames_to_save[file_ind], '.rds')) 
  }
}

