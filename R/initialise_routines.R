#generic set of initialisation routines that are called for every simulation
build_simulation_params <- function(user_global_params = NULL, user_simulation_params = NULL, 
                                    user_feature_params = NULL){
  #' @import foreach
  #' @import doParallel
  #' @import abind
  #' @import pixmap
  
  default_global_params = initialise_default_global_params()
  default_simulation_params = initialise_default_simulation_params()
  default_feature_params = initialise_default_feature_params()
  
  if (!is.null(user_global_params) == TRUE){
    global_params <- overwrite_current_params(user_params = user_global_params, default_params = default_global_params)
    #check_global_params(global_params)
  } else {
    global_params = default_global_params
  }
  
  if (!is.null(user_simulation_params) == TRUE){  
    simulation_params <- overwrite_current_params(user_params = user_simulation_params, default_params = default_simulation_params)
    #check_simulation_params(simulation_params)
  } else{
    simulation_params = default_simulation_params
  }
  
  if (!is.null(user_feature_params) == TRUE){  
    feature_params <- overwrite_current_params(user_params = user_feature_params, default_params = default_feature_params)
  } else {
    feature_params = default_feature_params
  }
  
  # run simulation with identical realisation instantiation
  if (global_params$set_seed == TRUE){
    seed=123
    flog.info('fixing random number seed to %d', 123)
    set.seed(seed)
  }
  
  #params_object <- check_param_conflicts(global_params, simulation_params, feature_params)
  
  simulation_params_object = build_simulation_variants(simulation_params)
  global_params <- write_simulation_folders(global_params, length(simulation_params_object$param_variants))
  
  simulation_params_group <- lapply(seq_along(simulation_params_object$param_variants), 
                                    function(i) collate_current_policy(simulation_params_object$param_variants[[i]], simulation_params_object$common_params, global_params))
  
  dump('global_params', paste0(global_params$simulation_params_folder, 'global_params.R'))
  simulation_params_file = paste0(global_params$simulation_params_folder, 'simulation_params.R')
  
  for (scenario_ind in seq_along(simulation_params_group)){
    current_simulation_params = simulation_params_group[[scenario_ind]]
    file_prefix = paste0(global_params$simulation_params_folder, 'scenario_',
                         formatC( scenario_ind, width = 3, format = "d", flag = "0"),
                         '_simulation_params')
    saveRDS(current_simulation_params, paste0(file_prefix, '.rds'))
    dump('current_simulation_params', simulation_params_file, append = TRUE)
  }
  
  if (class(global_params$scenario_subset) == 'character'){
    if (global_params$scenario_subset == 'all'){
      global_params$scenario_subset = seq_along(simulation_params_group)
    } else {
      flog.error('incorrect setting for scenario_subset - must be vector of natural numbers or "all"')
    }
  }
  
  global_params <- initialise_cores(global_params)
  global_params$strt = Sys.time()
  
  saveRDS(global_params, paste0(global_params$simulation_params_folder, 'global_params.rds'))
  saveRDS(simulation_params_object$param_variants, paste0(global_params$simulation_params_folder, 'param_variants.rds'))
  
  params_object = list()
  params_object$global_params = global_params
  params_object$simulation_params_group = simulation_params_group
  params_object$feature_params = feature_params
  return(params_object)
  
}


generate_global_inputs <- function(params_object){
  
  # generate simulated ecology
  if (params_object$global_params$use_simulated_data == TRUE) {
    current_filenames <- list.files(path = params_object$global_params$simulation_inputs_folder, all.files = FALSE,
                                    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                    include.dirs = FALSE, no.. = FALSE)
    if ( (length(current_filenames) == 0) | (params_object$global_params$run_from_saved_simulated_data == FALSE)){
      construct_simulated_data(params_object$feature_params, 
                               params_object$global_params$simulation_inputs_folder, 
                               params_object$global_params$simulation_params_folder, 
                               params_object$global_params$backup_simulation_inputs)
    }
  }
  
  raster_filenames <- list.files(path = params_object$global_params$simulation_inputs_folder, pattern = params_object$global_params$raster_file_type, all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)
  if (length(raster_filenames) == 0){
    flog.error(paste('cannot find any raster files with extension', params_object$global_params$raster_file_type))
  }
  
  feature_raster_layers = load_rasters(params_object$global_params$simulation_inputs_folder, raster_filenames, features_to_use = params_object$global_params$features_to_use)
  
  feature_layers = lapply(seq(dim(feature_raster_layers)[3]), function(i) raster_to_array(subset(feature_raster_layers, i)))
  
  parcel_characteristics <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'parcel_characteristics.rds'))
  
  #feature_layers = scale_ecology(feature_layers, dim(feature_layers[[1]]))
  
  # build list object containing feature values by feature layer for all
  # sites.  This is 3-level nested list, the top level is parcel index, for each
  # parcel there is sublist containing the value of each feature for each cell
  # in that parcel. So if there were 3 parcels and 2 features, it would be a 3
  # element list, where each element was subsequent list of length 2
  # containing the values for the 2 features in that parcel. The values of the
  # feature for the parcel are a vector of length given by the number of
  # pixels in the parcel.
  
  site_feature_layers_initial <- split_ecology(feature_layers, parcel_characteristics$land_parcels)
  
  # This is a list of single values of length number of sites where the values
  # representing the probabilities of sites being developed. Default is that
  # all sites have equal probability simulated data, with user data this must
  # be specified.
  
  if (file.exists(paste0(params_object$global_params$simulation_inputs_folder, 'dev_probability_list.rds'))){
    dev_probability_list <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'dev_probability_list.rds'))
  } else {
    dev_probability_list <- rep(list(1/length(parcel_characteristics$land_parcels)), length(parcel_characteristics$land_parcels))
  }
  
  # This is a list of single values of length number of sites, where the values
  # represent the probabilities of sites being offset. Default is that all
  # sites have equal probability simulated data, with user data this must be
  # specified.
  
  if (file.exists(paste0(params_object$global_params$simulation_inputs_folder, 'offset_probability_list.rds'))){
    offset_probability_list <- readRDS(paste0(params_object$global_params$simulation_inputs_folder, 'offset_probability_list.rds'))
  } else {
    offset_probability_list <- rep(list(1/length(parcel_characteristics$land_parcels)), length(parcel_characteristics$land_parcels))
  }
  
  if (class(params_object$global_params$features_to_use_in_simulation) == "character"){
    if (params_object$global_params$features_to_use_in_simulation == 'all'){
      features_to_use_in_simulation = seq_along(site_feature_layers_initial[[1]])
    } 
  } else {
    features_to_use_in_simulation = params_object$global_params$features_to_use_in_simulation
  }
  
  # select subset of feature layers to use in current simulation 
  # (e.g. if there 100 layers just run with 10 of them)
  
  input_data_object = list()
  input_data_object$site_feature_layers_initial = select_feature_subset(site_feature_layers_initial, features_to_use_in_simulation)
  input_data_object$parcel_characteristics = parcel_characteristics
  input_data_object$dev_probability_list = dev_probability_list
  input_data_object$offset_probability_list = offset_probability_list
  input_data_object$global_params = params_object$global_params
  input_data_object$feature_params = params_object$feature_params
  return(input_data_object)
  
}



sample_current_dynamics <- function(current_feature_dynamics_set, current_feature_val, dynamics_update_type, dynamics_sample_type){

  # TODO MUST INCLUDE ABILITY TO CHECK IF OUT OF BOUNDS AND SWITCH MODES
  current_feature_dynamics = current_feature_dynamics_set$best_estimate
  
  if (dynamics_sample_type == 'by_initial_value'){
    current_discriminator = current_feature_val - current_feature_dynamics_set$best_estimate[1]
  } else if (dynamics_sample_type == 'by_distribution'){
    current_discriminator = runif(n = 1, min = -1, max = 1)
  }
  
  if (current_discriminator >= 0){
    bound_to_use = current_feature_dynamics_set$upper_bound
  } else {
    bound_to_use = current_feature_dynamics_set$lower_bound
  }
  
  if (dynamics_sample_type == 'by_initial_value'){
    current_discriminator = current_discriminator/(bound_to_use[1] - current_feature_dynamics_set$best_estimate[1])
  }
  
  current_feature_dynamics = current_feature_dynamics_set$best_estimate + current_discriminator*(bound_to_use - current_feature_dynamics_set$best_estimate)   
  
  if (dynamics_update_type == 'by_differential'){
    current_feature_dynamics = diff(current_feature_dynamics)
    if (any((cumsum(current_feature_dynamics) + current_feature_val) < 0)){
      browser()
    }
  }
  

  return(current_feature_dynamics)
  
}


#     if (current_discriminator >= 0){
#       current_discriminator = current_discriminator/(current_feature_dynamics_set$upper_bound[1] - current_feature_dynamics_set$best_estimate[1])
#       if (current_discriminator > 1){
#         browser()
#         current_discriminator = 1
#       }
# 
#       current_feature_dynamics = current_feature_dynamics_set$best_estimate + current_discriminator*(current_feature_dynamics_set$upper_bound - current_feature_dynamics_set$best_estimate)
#     } else{
#       current_discriminator = current_discriminator/(current_feature_dynamics_set$best_estimate[1] - current_feature_dynamics_set$lower_bound[1])
#       
#       if (current_discriminator < -1){
#         browser()
#         current_discriminator = -1
#       }
#       current_feature_dynamics = current_feature_dynamics_set$best_estimate - current_discriminator*(current_feature_dynamics_set$best_estimate - current_feature_dynamics_set$lower_bound)
#     }
#   }
#   


build_dynamics <- function(site_feature_layers_to_use, features_to_use, sample_dynamics, projection_type, dynamics_update_type, dynamics_sample_type, feature_dynamics_bounds, feature_dynamics_modes){

  if (sample_dynamics == TRUE){
    if (projection_type == 'by_element'){
      dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                            function(i) lapply(features_to_use,
                                               function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]), 
                                                                                 function(k) sample_current_dynamics(feature_dynamics_bounds[[j]][[feature_dynamics_modes[[i]][[j]] [k] ]],
                                                                                                                     site_feature_layers_to_use[[i]][[j]][[k]],
                                                                                                                     dynamics_update_type, 
                                                                                                                     dynamics_sample_type)  )))
    } else if (projection_type == 'by_site'){
      dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                            function(i) lapply(seq_along(site_feature_layers_to_use[[i]]),
                                               function(j) sample_current_dynamics(feature_dynamics_bounds[[j]][[feature_dynamics_modes[[i]][[j]]]],
                                                                                   mean(site_feature_layers_to_use[[i]][[j]]),
                                                                                   dynamics_update_type, 
                                                                                   dynamics_sample_type)  ))
    }
  } else {
    if (projection_type == 'by_element'){
      dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                            function(i) lapply(features_to_use,
                                               function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]), 
                                                                  function(k) feature_dynamics_bounds[[j]][[feature_dynamics_modes[[i]][[j]] [k] ]]$best_estimate)))
    } else if (projection_type == 'by_site'){                                                                             
      dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                            function(i) lapply(seq_along(site_feature_layers_to_use[[i]]),
                                               function(j) feature_dynamics_bounds[[j]][[feature_dynamics_modes[[i]][[j]]]]$best_estimate))
    }
  }
  
  
  return(dynamics_set)
}


find_current_mode <- function(current_feature_val, current_condition_class_bounds){
  
  if (length(current_feature_val) > 0){
    
    mode_discriminator = current_feature_val
    current_modes = lapply(seq_along(current_condition_class_bounds), 
                           function(i) (mode_discriminator <= max(current_condition_class_bounds[[i]])) && (mode_discriminator > min(current_condition_class_bounds[[i]]) ))
    mode_ind_to_use = which(unlist(current_modes) > 0)
    if (length(mode_ind_to_use) != 1){
      flog.error('poorly defined condition class bounds')
      stop()
    } else {
      current_mode = mode_ind_to_use
    }
  } else {
    current_mode = vector()
  }
  return(current_mode)
}



  
find_modes <- function(projection_type, feature_layers_to_use, condition_class_bounds){
  
  if (projection_type == 'by_site' ){
    feature_dynamics_modes = lapply(seq_along(feature_layers_to_use), 
                                    function(i) lapply(seq_along(feature_layers_to_use[[i]]), 
                                                       function(j) find_current_mode(mean(feature_layers_to_use[[i]][[j]]), 
                                                                                     condition_class_bounds[[j]])))
  } else if (projection_type == 'by_element'){
    feature_dynamics_modes = lapply(seq_along(feature_layers_to_use), 
                                    function(i) lapply(seq_along(feature_layers_to_use[[i]]), 
                                                       function(j) sapply(feature_layers_to_use[[i]][[j]], 
                                                                          find_current_mode, 
                                                                          condition_class_bounds[[j]])))
  }
  
  return(feature_dynamics_modes)
}

initialise_output_object <- function(current_simulation_params, index_object, global_params, site_feature_layers_initial, parcel_characteristics, 
                                     offset_probability_list, dev_probability_list, feature_params){
  output_object = list()
  output_object$offsets_object <- list()
  output_object$dev_object <- list()
  output_object$unregulated_loss_object <- list()
  output_object$credit_object <- list()
  output_object$offset_bank_object <- list()
  output_object$offset_pool_object <- list()
  output_object$site_feature_layers <- site_feature_layers_initial
  current_credit = array(0, length(current_simulation_params$features_to_use_in_offset_calc))
  
  if (current_simulation_params$use_specified_offset_metric == TRUE){
    current_credit = transform_features_to_offset_metric(current_credit, metric_type = simulation_params$current_offset_metric_type)
  }
  
  output_object$current_credit = current_credit
  output_object$credit_match_flag = FALSE
  output_object$index_object = index_object
  
  if (file.exists(paste0(global_params$simulation_inputs_folder, 'management_mode.rds'))){
    feature_dynamics_modes <- readRDS(paste0(global_params$simulation_inputs_folder, 'management_mode.rds'))
  } else {

    feature_dynamics_modes = find_modes(projection_type = feature_params$background_projection_type, 
                                        feature_layers_to_use = site_feature_layers_initial, 
                                        condition_class_bounds = feature_params$condition_class_bounds)
    
  }
  
  if (file.exists(paste0(global_params$simulation_inputs_folder, 'background_dynamics.rds'))){
    feature_dynamics <- readRDS(paste0(global_params$simulation_inputs_folder, 'background_dynamics.rds'))
  } else {
    
    feature_dynamics <- build_dynamics(site_feature_layers_initial,
                                       features_to_use = seq(feature_params$feature_num),
                                       feature_params$sample_background_dynamics,
                                       feature_params$background_projection_type,
                                       feature_params$dynamics_update_type,
                                       feature_params$dynamics_sample_type,
                                       feature_params$background_dynamics_bounds, 
                                       feature_dynamics_modes)
    
#     management_dynamics <- build_dynamics(site_feature_layers_initial,
    # features_to_use = feature_params$features_to_use_in_offset_interventionn,
#    feature_params$sample_background_dynamics,
#                                        feature_params$background_projection_type,
#                                        feature_params$dynamics_update_type,
#                                        feature_params$dynamics_sample_type,
#                                        feature_params$management_dynamics_bounds, 

#                                        feature_dynamics_modes)
  }
  
  output_object$feature_dynamics = feature_dynamics
  #output_object$feature_value_conflicts = check_feature_value_conflicts(site_feature_layers_initial, feature_dynamics)
  output_object$feature_dynamics_modes = feature_dynamics_modes

  return(output_object)
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

check_feature_value_conflicts <- function(site_feature_layers_initial, feature_dynamics){
  feature_value_conflicts = lapply(seq_along(site_feature_layers_initial), 
                                   function(i) lapply(seq_along(site_feature_layers_initial[[i]]), 
                                                      function(j) any(site_feature_layers_initial[[i]][[j]] > max(feature_dynamics[[i]][[j]]) 
                                                                      || site_feature_layers_initial[[i]][[j]] < min(feature_dynamics[[i]][[j]])))) 
  return(feature_value_conflicts)
}


check_param_conflicts <- function(simulation_params, feature_params, global_params){
  
  feature_test = match(global_params$features_to_use_in_simulation, seq(feature_params$feature_num))
  if (any(is.na(feature_test))){
    flog.error(paste('\n ERROR: global_params$features_to_use_in_simulation does not match simulated ecology feature parameters'))
    stop()
  } 
  
  offset_calc_test = 
    if (any(is.na(offset_calc_test))){
      flog.error(paste('\n ERROR: simulation_params$features_to_use_in_offset_calc does not match global_params$features_to_use_in_simulation'))
      stop()
    } 
  params_object = list()
  params_object$global_params = global_params
  params_object$simulation_params = simulation_params
  params_object$feature_params = feature_params
  return(params_object)
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
  
  #   if (mean(unlist(decline_rates_initial)) > simulation_params$restoration_rate){
  #     flog.error('restoration parameter set below decline rate')
  #     stop()
  #   }
  
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
  
  #   if ( (length(simulation_params$mean_decline_rates) != length(global_params$features_to_use_in_simulation)) ){
  #     flog.error(cat('\n decline rates mean parameter does not match feature number'))
  #     stop()
  #   }
  #   
  #   if ( (length(simulation_params$decline_rate_std) != length(global_params$features_to_use_in_simulation)) ){
  #     flog.error(cat('\n decline rates std parameter dpes not match feature number'))
  #     stop()
  #   }
  
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
    flog.error( cat('\nERROR: features in simulation_params$features_to_use do not match site_feature_layers_initial_dimensions'))
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

write_simulation_folders <- function(global_params, scenario_num){
  
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
  
  global_params$run_folder = write_folder(paste0(base_run_folder, formatC(current_run, width = 5, format = "d", flag = "0"), '/'))
  global_params$output_folder = write_folder(paste0(global_params$run_folder, '/simulation_outputs/'))
  flog.info('writing simulation outputs into %s', global_params$run_folder)
  
  for (scenario_ind in (seq(scenario_num))){
    write_folder(paste0(global_params$output_folder, '/scenario_', formatC(scenario_ind, width = 3, format = "d", flag = "0"), '/'))
  }
  
  global_params$simulation_params_folder = write_folder(paste0(global_params$run_folder, '/simulation_params/'))
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


collate_current_policy <- function(current_simulation_params, common_params, global_params){
  
  current_simulation_params = append(current_simulation_params, common_params)
  
  if (current_simulation_params$use_offset_bank == TRUE){
    current_simulation_params$offset_time_horizon_type = 'current'  # 'current' - used for banking only - determine accrued offset gains till current year.
  } else {
    current_simulation_params$offset_time_horizon_type = 'future'  #'future' - project from time of development to offset time horizon
  }
  
  
  current_simulation_params$features_to_use_in_offset_calc = match(current_simulation_params$features_to_use_in_offset_calc, global_params$features_to_use_in_simulation)
  current_simulation_params$features_to_use_in_offset_intervention = match(current_simulation_params$features_to_use_in_offset_intervention, global_params$features_to_use_in_simulation)
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
    current_simulation_params$banked_offset_vec = generate_stochastic_intervention_vec(time_steps = simulation_params$time_steps,
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
  params_object = list()
  params_object$param_variants = param_variants
  params_object$common_params = common_params
  
  return(params_object)
  
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
generate_stochastic_intervention_vec <- function(time_steps, intervention_start, intervention_end, intervention_num, sd){
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


initialise_shape_parcels <- function(ecology_params){
  parcels = list()
  parcels$landscape_dims = c(ecology_params$ecology_size, ecology_params$ecology_size)
  parcel_num_x = ecology_params$parcel_num_x   #length in parcels of array in x
  parcel_num_y = ecology_params$parcel_num_y #length in parcels of array in y
  parcel_vx = split_vector(parcel_num_x, ecology_params$ecology_size, sd = 5, min_width = 3) # make normally distributed vector that sums to ecology size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, ecology_params$ecology_size, sd = 5, min_width = 3) # as above for y
  
  pixel_indexes = 1:(ecology_params$ecology_size*ecology_params$ecology_size)     #index all elements of ecology array
  dim(pixel_indexes) = c(ecology_params$ecology_size, ecology_params$ecology_size)  # arrange ecology array index vector into array of landscape dimensions
  land_parcels = mcell(pixel_indexes, parcel_vx, parcel_vy) #split the ecology array into a series of subarrays with dimensions sz_x by sz_y
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



initialise_index_object <- function(parcel_characteristics, site_feature_layers_initial, simulation_params, offset_indexes_to_exclude, dev_indexes_to_exclude){
  
  index_object = list()
  index_object$banked_offset_pool = vector()
  index_object$site_indexes_used = vector('list', 5)
  names(index_object$site_indexes_used) = c('offsets', 'devs', 'illegals', 'dev_credits', 'banking')
  
  index_object$available_indexes = list()
  
  index_object$available_indexes$offsets = set_available_indexes(global_indexes = parcel_characteristics$site_indexes, 
                                                                 offset_indexes_to_exclude, 
                                                                 parcel_characteristics$land_parcels, 
                                                                 site_feature_layers_initial, 
                                                                 screen_site_zeros = simulation_params$screen_offset_zeros,
                                                                 site_screen_size = simulation_params$site_screen_size,
                                                                 simulation_params$features_to_use_in_offset_calc)
  
  index_object$available_indexes$devs = set_available_indexes(global_indexes = parcel_characteristics$site_indexes, 
                                                              dev_indexes_to_exclude,
                                                              parcel_characteristics$land_parcels, 
                                                              site_feature_layers_initial, 
                                                              screen_site_zeros = simulation_params$screen_dev_zeros,
                                                              site_screen_size = simulation_params$site_screen_size,
                                                              simulation_params$features_to_use_in_offset_calc)
  return(index_object)
}


set_available_indexes <- function(global_indexes, indexes_to_exclude, land_parcels, site_feature_layers_initial, screen_site_zeros, site_screen_size, features_to_use_in_offset_calc){
  
  if (screen_site_zeros == TRUE){
    
    initial_parcel_sums = lapply(seq_along(site_feature_layers_initial), 
                                 function(i) lapply(seq_along(site_feature_layers_initial[[i]]), 
                                                    function(j) sum(site_feature_layers_initial[[i]][[j]]) ) )
    
    zeros_to_exclude = which(unlist(lapply(seq_along(initial_parcel_sums), 
                                           function(i) all(unlist(initial_parcel_sums[[i]][features_to_use_in_offset_calc]) == 0))))
    indexes_to_exclude = unique(c(indexes_to_exclude, zeros_to_exclude))
  }
  
  if (site_screen_size > 0){
    parcel_lengths <- unlist(lapply(seq_along(land_parcels), function(i) length(land_parcels[[i]])))
    smalls_to_exclude = which(parcel_lengths < site_screen_size)
    indexes_to_exclude = unique(c(indexes_to_exclude, smalls_to_exclude))
  } 
  
  inds_to_remove = which(global_indexes %in% indexes_to_exclude)
  available_indexes = global_indexes
  if (length(inds_to_remove) > 0){
    available_indexes = available_indexes[-inds_to_remove]
  }
  
  return(available_indexes)
}


# initialise_decline_rates <- function(parcel_characteristics, sample_decline_rate, mean_decline_rates, decline_rate_std, feature_num){
#   
#   land_parcels = parcel_characteristics$land_parcels
#   parcel_num = length(land_parcels)
#   decline_rates = vector('list', parcel_num)
#   for (parcel_ind in seq(parcel_num)){
#     decline_rates[[parcel_ind]] = lapply(seq(feature_num), function(i) rnorm(1, mean_decline_rates[i], decline_rate_std[i]))
#   }
#   
#   return(decline_rates)
#   
# }

split_ecology_to_land_parcels <- function(landscape_ecology, land_parcels, feature_num){
  parcel_num = length(land_parcels)
  current_feature_layers = generate_nested_list(outer_dim = parcel_num, inner_dim = feature_num)
  
  for (parcel_ind in seq_len(parcel_num)){
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (feature_ind in seq_len(feature_num)){
      current_feature_layers[[parcel_ind]][[feature_ind]] = landscape_ecology[[feature_ind]][current_parcel]
    }
  }
  return(current_feature_layers)
  
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
