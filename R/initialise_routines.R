# set of initialisation routines that are called for every simulation

build_input_data <- function(user_global_params, user_feature_params, user_transform_function, simulation_params_group){
  
  # Undertake all the run intialization proceedures, including generating
  # simulated data if required or reading in previously generated input data

  input_data = list()

  input_data$global_params <- build_global_params(user_global_params, user_transform_function, simulation_params_group)
  input_data$feature_params <- build_feature_params(user_feature_params, input_data$global_params)
  
  save_params(input_data$global_params, input_data$feature_params, simulation_params_group)

  # generate simulated feature
  if (input_data$global_params$build_simulated_feature_layers == TRUE) {
    
    construct_simulated_data(input_data$feature_params, 
                             input_data$global_params$simulation_inputs_folder, 
                             input_data$global_params$simulation_params_folder, 
                             input_data$global_params$backup_simulation_inputs)
    
    input_data$global_params$overwrite_site_characteristics = TRUE
    input_data$global_params$overwrite_condition_classes = TRUE
    input_data$global_params$overwrite_feature_dynamics = TRUE
    input_data$global_params$overwrite_management_dynamics = TRUE
    
  } 
  
  if (!file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'site_characteristics.rds')) || 
      (input_data$global_params$overwrite_site_characteristics == TRUE)){
    
    if (input_data$global_params$planning_units_raster == 'default'){
      planning_units_filename <- paste0(input_data$global_params$simulation_inputs_folder, 'planning_units.tif')
    } else {
      planning_units_filename = input_data$global_params$planning_units_raster
    }

    planning_units <- load_rasters(planning_units_filename)
    
    flog.info('building site characteristics object (this may take a while) ..')

    input_data$site_characteristics <- build_ID_characteristics(planning_units)
    
    flog.info('saving site characteristics object')
    saveRDS(object = input_data$site_characteristics, file = paste0(input_data$global_params$simulation_inputs_folder, 'site_characteristics.rds'))
    
  } else {
    input_data$site_characteristics = readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'site_characteristics.rds'))
  }
  
  
  if (all(input_data$global_params$feature_raster_files == 'default')){
    feature_raster_files = list.files(path = input_data$global_params$simulation_inputs_folder, all.files = FALSE,
                                      full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                      include.dirs = FALSE, no.. = FALSE, pattern = 'feature_vals_')
    feature_raster_files = paste0(input_data$global_params$simulation_inputs_folder, 
                                  feature_raster_files[input_data$global_params$features_to_use_in_simulation])
  } else if (!all(file.exists(input_data$global_params$feature_raster_files))){
    flog.error(paste('one or more feature raster files missing - if running from simulated data set global_params$build_simulated_feature_layers = TRUE'))
  } else{
    feature_raster_files = input_data$global_params$feature_raster_files
  }
  
  feature_ID_files = list.files(path = input_data$global_params$simulation_inputs_folder, all.files = FALSE,
                                    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                    include.dirs = FALSE, no.. = FALSE, pattern = 'feature_IDs_')
  
  feature_ID_files = paste0(input_data$global_params$simulation_inputs_folder, 
                            feature_ID_files[input_data$global_params$features_to_use_in_simulation])
  
  input_data$feature_ID = build_sparse_features(feature_ID_files, 
                                                input_data$site_characteristics$max_cell_ID, 
                                                TRUE)
  
  initial_features = build_sparse_features(feature_raster_files[input_data$global_params$features_to_use_in_simulation],
                                                      input_data$site_characteristics$max_cell_ID, 
                                                      FALSE)
  
  if (!file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'condition_classes.rds')) | 
      (input_data$global_params$overwrite_condition_classes == TRUE)){

    input_data$condition_classes = build_condition_classes(initial_features, 
                                                           input_data$site_characteristics$max_cell_ID, 
                                                           input_data$global_params, 
                                                           input_data$feature_params$condition_class_characteristics, 
                                                           input_data$feature_params$background_condition_class_num)
    
    flog.info('saving condition classes ..')

    saveRDS(input_data$condition_classes, paste0(input_data$global_params$simulation_inputs_folder, 'condition_classes.rds'))
    
  } else {
    input_data$condition_classes = readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'condition_classes.rds'))
  }
  
  if (input_data$feature_params$management_condition_class == 'background'){
    input_data$management_condition_classes = input_data$condition_classes 
  } else {
    
    if (file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'management_condition_class_layers.rds'))){
      management_condition_class_layers <- readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'management_condition_class_layers.rds'))
    } else {
      
      flog.info('building management condition class layers...')

      input_data$management_condition_classes = build_condition_classes(initial_features, 
                                                                        input_data$site_characteristics$max_cell_ID, 
                                                                        input_data$global_params, 
                                                                        input_data$feature_params$management_condition_class_characteristics, 
                                                                        input_data$feature_params$management_condition_class_mode_num)
      
    }
    
  #   input_data$management_dynamics_modes = split_modes(input_data$site_characteristics$cell_id_groups, management_condition_class_layers)
  } 

  dim(initial_features) = c(input_data$site_characteristics$max_cell_ID, length(input_data$global_params$features_to_use_in_simulation))
  
  input_data$site_scale_features = lapply(seq_along(input_data$site_characteristics$cell_id_groups), 
                                          function(i) initial_features[input_data$site_characteristics$cell_id_groups[[i]], , drop= FALSE])
  
  build_background_dynamics_flag = !file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'feature_dynamics.rds')) | (input_data$global_params$overwrite_feature_dynamics == TRUE)
    
  build_management_dynamics_flag = !file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'management_dynamics.rds')) | (input_data$global_params$overwrite_management_dynamics == TRUE)
  
  input_data$background_mode_num = input_data$site_characteristics$max_cell_ID*sum(input_data$feature_params$background_condition_class_num)

  unique_feature_IDs = setdiff(unique(as.vector(input_data$feature_ID)), 0)
  input_data$feature_blocks = lapply(seq_along(unique_feature_IDs), function(i) which(input_data$feature_ID == unique_feature_IDs[i]))
  
  # block_to_use = input_data$feature_blocks
  # condition_class_block <- lapply(seq_along(block_to_use), function(i) unique(input_data$condition_classes[ block_to_use[[i]] ]))
  # condition_class_discriminator <- unlist(lapply(condition_class_block, 'length')) > 1
  # 
  # input_data$mode_ID <- build_mode_mapping(input_data$condition_classes,
  #                                          input_data$feature_blocks, 
  #                                          input_data$site_characteristics$max_cell_ID,
  #                                          features_to_use = seq_along(input_data$global_params$features_to_use_in_simulation),
  #                                          input_data$feature_params$background_dynamics_bounds)

  # condition_class_groups = lapply(seq_along(input_data$feature_blocks), 
  #                                        function(i) input_data$condition_classes[input_data$feature_blocks[[i]], ])
  # 
  # condition_class_discriminator = lapply(seq_along(condition_class_groups), 
  #                                        function(i) length(unique(condition_class_groups[[i]])))
  # 

  if ((build_background_dynamics_flag == TRUE) | (build_management_dynamics_flag == TRUE)){

    mode_ID = unlist(lapply(seq_along(input_data$feature_blocks), function(i) input_data$feature_blocks[[i]][1]))
    input_data$mode_characteristics = data.frame(ID = mode_ID,
                                                 feature = ceiling(mode_ID/input_data$site_characteristics$max_cell_ID),
                                                 condition_class = input_data$condition_classes[mode_ID],
                                                 management_condition_class = input_data$management_condition_classes[mode_ID])
    
    initial_vals = unlist(lapply(seq_along(input_data$feature_blocks), function(i) mean(initial_features[input_data$feature_blocks[[i]] ])))
  }
  
  if (build_background_dynamics_flag == FALSE){
    input_data$feature_dynamics = readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'feature_dynamics.rds'))
  } else {
    flog.info('building background dynamics')
    
    input_data$feature_dynamics = build_dynamics(dynamics_type = 'background',
                                                 input_data$mode_characteristics,
                                                 initial_vals, 
                                                 input_data$feature_params$condition_class_characteristics,
                                                 input_data$feature_params$sample_background_dynamics,
                                                 store_dynamics_as_differential = input_data$feature_params$update_background_dynamics_by_differential,
                                                 input_data$feature_params$dynamics_sample_type,
                                                 input_data$feature_params$background_dynamics_bounds, 
                                                 input_data$site_characteristics$max_cell_ID*length(input_data$global_params$features_to_use_in_simulation),
                                                 length(input_data$feature_params$dynamics_time))
    # if (input_data$feature_params$dynamics_update_type == 'static'){
    #   input_data$feature_dynamics[input_data$mode_characteristics$ID, ] = t(apply(input_data$feature_dynamics[input_data$mode_characteristics$ID, , drop = FALSE], 1, cumsum))
    # }
    
    if (any(is.na(unlist(input_data$feature_dynamics)))){
      flog.error('poorly defined dynamics')
      stop()
    } else {
      saveRDS(input_data$feature_dynamics, paste0(input_data$global_params$simulation_inputs_folder, 'feature_dynamics.rds'))
    }
  }
  
  input_data$mode_blocks = lapply(seq_along(input_data$feature_blocks), 
                                  function(i) rep(input_data$feature_blocks[[i]][1], length(input_data$feature_blocks[[i]])))
  
  input_data$mode_ID = Matrix(0, nrow = input_data$site_characteristics$max_cell_ID*length(input_data$global_params$features_to_use_in_simulation), 1)
  input_data$mode_ID[unlist(input_data$feature_blocks)] = unlist(input_data$mode_blocks)
  
  mode_ID = input_data$mode_ID
  dim(mode_ID) = c(input_data$site_characteristics$max_cell_ID, length(input_data$global_params$features_to_use_in_simulation))

  # input_data$feature_dynamics_modes = lapply(seq_along(input_data$site_characteristics$cell_id_groups),
  #                                           function(i) Matrix(as.vector(mode_ID[input_data$site_characteristics$cell_id_groups[[i]], ]), ncol = 1, sparse = TRUE))

  input_data$feature_dynamics_modes = lapply(seq_along(input_data$site_characteristics$cell_id_groups),
                                             function(i) mode_ID[input_data$site_characteristics$cell_id_groups[[i]], , drop = FALSE])
  
  if (build_management_dynamics_flag == FALSE){
    input_data$management_dynamics = readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'management_dynamics.rds'))
  } else {
    flog.info('building management dynamics')
    input_data$management_dynamics = build_dynamics(dynamics_type = 'management', 
                                                    input_data$mode_characteristics,
                                                    initial_vals, 
                                                    input_data$feature_params$management_condition_class_characteristics,
                                                     input_data$feature_params$sample_management_dynamics,
                                                     store_dynamics_as_differential = FALSE,
                                                     input_data$feature_params$management_dynamics_sample_type,
                                                     input_data$feature_params$management_dynamics_bounds, 
                                                     input_data$site_characteristics$max_cell_ID*length(input_data$global_params$features_to_use_in_simulation),
                                                     length(input_data$feature_params$dynamics_time))
    
    saveRDS(input_data$management_dynamics, paste0(input_data$global_params$simulation_inputs_folder, 'management_dynamics.rds'))
  } 
  
  if (!(file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'dev_probability_list.rds'))) |
      (input_data$global_params$overwrite_dev_probability_list == TRUE)){
    flog.info('assigning equal development probability to all sites')
    input_data$dev_probability_list <- rep(list(1/length(input_data$site_characteristics$cell_id_groups)), 
                                                       length(input_data$site_characteristics$cell_id_groups))
  } else {
    input_data$dev_probability_list <- readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'dev_probability_list.rds'))
  }
  
  if (!(file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'offset_probability_list.rds'))) |
      (input_data$global_params$overwrite_offset_probability_list == TRUE)){
    flog.info('assigning equal offset probability to all sites')
    input_data$offset_probability_list <- rep(list(1/length(input_data$site_characteristics$cell_id_groups)), 
                                                          length(input_data$site_characteristics$cell_id_groups))
  } else {
    input_data$offset_probability_list <- readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'offset_probability_list.rds'))
  }
  
  if (!(file.exists(paste0(input_data$global_params$simulation_inputs_folder, 'unregulated_probability_list.rds'))) |
      (input_data$global_params$overwrite_unregulated_probability_list == TRUE)){
    flog.info('assigning equal unregulated loss probability to all sites')

    input_data$unregulated_probability_list <- rep(list(1/length(input_data$site_characteristics$cell_id_groups)), length(input_data$site_characteristics$cell_id_groups))
    
  } else {
    input_data$unregulated_probability_list <- readRDS(paste0(input_data$global_params$simulation_inputs_folder, 'unregulated_probability_list.rds'))
    
  }
  
  return(input_data)
  
}


build_cfacs <- function(site_scale_features, feature_dynamics, condition_classes, feature_dynamics_modes, mode_characteristics, 
                          cell_num, projection_yrs, intervention_yrs, site_num_remaining_pool, cfac_type, 
                          object_type, use_cfac_type_in_sim, condition_class_characteristics, use_offset_metric, user_transform_function, 
                          simulation_params, feature_params, global_params){
  
  if ((use_cfac_type_in_sim == FALSE) || (cfac_type == 'background')){
    cfac_params = lapply(seq_along(site_scale_features), function(i) setNames(rep(list(FALSE), 4), 
                                                                              c('include_potential_developments', 
                                                                                'include_potential_offsets', 
                                                                                'include_unregulated_loss', 
                                                                                'adjust_cfacs_flag')))
  } else {
    
    cfac_params = lapply(seq_along(site_scale_features), function(i) select_cfac_params(object_type[[i]], simulation_params))
  }
  
  if (cfac_type == 'background'){ 
    adjust_cfacs_flag = FALSE
    time_horizon <- 0:global_params$time_steps
    projection_yr <- 1
    
    if (use_offset_metric == FALSE){
      flog.info('building background counterfactuals - this may take a while ...')
    } else {
      flog.info('building user metric background counterfactuals ...')
    }
    
  } else if (cfac_type == 'site_scale'){
    
    
    
    adjust_cfacs_flag = FALSE
    if (use_offset_metric == FALSE){
      flog.info('building site scale counterfactuals ...')
    } else {
      flog.info('building user metric site scale counterfactuals...')
    }

    time_horizons = generate_time_horizons(project_type = 'current', 
                                           yr = global_params$time_steps, 
                                           unlist(intervention_yrs),
                                           time_horizon = global_params$time_steps, 
                                           length(site_scale_features))
    
    time_horizons = lapply(seq_along(time_horizons), function(i) 0:time_horizons[i])
    
    #   cfac_weights_group = lapply(seq_along(cfac_params), function(i) unlist(calc_cfac_weights(site_num = 1, 
    #                                                                                            cfac_params[[i]]$include_potential_developments,
    #                                                                                            cfac_params[[i]]$include_potential_offsets,
    #                                                                                            cfac_params[[i]]$include_unregulated_loss,
    #                                                                                            dev_probability_list = vector(), 
    #                                                                                            offset_probability_list = vector(), 
    #                                                                                            simulation_params, 
    #                                                                                            feature_params, 
    #                                                                                            site_num_remaining_pool[[i]], 
    #                                                                                            time_horizons[i], 
    #                                                                                            unlist(intervention_yrs)[i]), recursive = FALSE))
    
  }
  
  if (use_offset_metric == FALSE){
    
    flog.info('building site scale counterfactuals ...')

    if (cfac_type == 'site_scale'){
      
      cfacs = lapply(seq_along(time_horizons), 
                     function(i) run_projection_routines(site_scale_features[i],
                                                         feature_dynamics,
                                                         feature_dynamics_modes[i],
                                                         mode_characteristics,
                                                         condition_class_characteristics,
                                                         dynamics_update_type = 'static',
                                                         projection_yrs[i],
                                                         time_horizons[[i]],
                                                         collapse_features = TRUE,
                                                         adjust_cfacs_flag,
                                                         cfac_weights = FALSE,
                                                         cell_num[i],
                                                         break_flag = FALSE))
      
      cfacs = unlist(cfacs, recursive = FALSE)
      
    } else {

      cfacs = run_projection_routines(site_scale_features,
                                      feature_dynamics,
                                      feature_dynamics_modes,
                                      mode_characteristics,
                                      condition_class_characteristics,
                                      dynamics_update_type = 'static',
                                      projection_yr,
                                      time_horizon,
                                      collapse_features = TRUE,
                                      adjust_cfacs_flag,
                                      cfac_weights = FALSE,
                                      cell_num,
                                      break_flag = FALSE)
    }
    
  } else {
    flog.info('building user metric background counterfactuals ...')
    browser()
    cfacs = lapply(seq_along(site_scale_features),
                   function(i) do.call(rbind, lapply(seq_along(time_horizons[[i]]), 
                                                     function(j) sum(user_transform_function(calc_site_cfacs(site_scale_features[[i]],
                                                                                                             projection_yrs[[i]],
                                                                                                             cfac_weights = vector(),
                                                                                                             cell_num[[i]],
                                                                                                             simulation_params,
                                                                                                             feature_params,
                                                                                                             feature_dynamics[[i]],
                                                                                                             condition_classes[[i]],
                                                                                                             time_horizons[[i]][j],
                                                                                                             cfac_params[[i]]$include_potential_developments,
                                                                                                             cfac_params[[i]]$include_potential_offsets,
                                                                                                             cfac_params[[i]]$include_unregulated_loss,
                                                                                                             cfac_params[[i]]$adjust_cfacs_flag,
                                                                                                             time_fill = FALSE,
                                                                                                             unlist_condition_classes = TRUE, 
                                                                                                             site_scale_condition_class_key[[i]], 
                                                                                                             flatten_features = FALSE), 
                                                                                             simulation_params$transform_params )))))
    
  }
  
  return(cfacs)
  
}


build_sparse_features <- function(feature_raster_files, max_cell_id, translate_factor){

  feature_num = length(feature_raster_files)
  sparse_features = Matrix(0, max_cell_id*feature_num, 1, sparse = TRUE)

  for (feature_ind in seq(feature_num)){

    current_feature_file = feature_raster_files[feature_ind]
    flog.info(paste('loading ', current_feature_file, 'as feature %s of %s'), feature_ind, feature_num)
    
    current_raster = raster(current_feature_file)
    ID_translate = max_cell_id*(feature_ind - 1)
    
    current_vals = getValues(current_raster)
    
    if (translate_factor > 0){
      current_vals[current_vals > 0] = current_vals[current_vals > 0] + ID_translate*translate_factor
    }
      
    current_block = (1:max_cell_id) + ID_translate
    
    sparse_features[current_block] = current_vals

  }

  sparse_features[which(is.na(sparse_features))] = 0
  
  return(sparse_features)
}

build_cfacs_routines <- function(global_input_data, simulation_params){

  if (global_input_data$global_params$background_cfacs_file == 'default'){
    background_cfacs_file = paste0(global_input_data$global_params$simulation_inputs_folder, 'background_cfacs.rds')
  } else {
    background_cfacs_file = global_input_data$global_params$background_cfacs_file
  }
  
  build_cfacs_flag = (global_input_data$global_params$overwrite_feature_dynamics == TRUE) | !file.exists(background_cfacs_file)
                        
  if (!build_cfacs_flag){
    
    flog.info('loading background counterfactuals from file')
    cfacs_object = readRDS(background_cfacs_file)
    
    time_vec_to_use = 0:global_input_data$global_params$time_steps
    
    if (length(time_vec_to_use) <= ncol(cfacs_object$background_cfacs[[1]])){
      build_current_background_cfacs_flag = FALSE
      flog.info('processing background counterfactuals')

      if (length(time_vec_to_use) < dim(cfacs_object$background_cfacs[[1]])[1]){
        browser()
        cfacs_object <- setNames(lapply(seq_along(cfacs_object), 
                                                   function(i) lapply(seq_along(cfacs_object[[i]]), 
                                                                      function(j) cfacs_object[[i]][[j]][seq_along(time_vec_to_use), , drop = FALSE]
                                                   )), 
                                            names(cfacs_object))
      }

    } else {

      flog.info('specified simulation time is not a subset of saved background counterfactuals')
      build_current_background_cfacs_flag = TRUE
    }
    
  } else {
    build_current_background_cfacs_flag = TRUE
  }
  
  if (build_current_background_cfacs_flag == TRUE){

    cfacs_object = list()

    cfacs_object$background_cfacs = build_cfacs(global_input_data$site_scale_features,
                                                global_input_data$feature_dynamics,
                                                global_input_data$condition_classes, 
                                                global_input_data$feature_dynamics_modes,
                                                global_input_data$mode_characteristics, 
                                                global_input_data$site_characteristics$cell_num,
                                                rep(1, length(global_input_data$site_scale_features)),
                                                rep(1, length(global_input_data$site_scale_features)),
                                                site_num_remaining_pool = FALSE,
                                                cfac_type = 'background',
                                                object_type = FALSE, 
                                                use_cfac_type_in_sim = FALSE, 
                                                global_input_data$feature_params$condition_class_characteristics, 
                                                use_offset_metric = FALSE, 
                                                global_input_data$global_params$user_transform_function, 
                                                simulation_params, 
                                                global_input_data$feature_params,
                                                global_input_data$global_params)
    
    if (simulation_params$use_offset_metric == TRUE){
      browser()
      cfacs_object$user_metric_background_cfacs = build_cfacs(global_input_data$site_scale_features,
                                                                simulation_params, 
                                                                global_input_data$feature_params,
                                                                global_input_data$global_params,
                                                                global_input_data$feature_dynamics,
                                                                global_input_data$condition_classes,
                                                                global_input_data$site_scale_condition_class_key,
                                                                global_input_data$site_characteristics$cell_num,
                                                                rep(1, length(global_input_data$site_scale_features)),
                                                                rep(1, length(global_input_data$site_characteristics$cell_id_groups)),
                                                                vector(),
                                                                cfac_type = 'background',
                                                                object_type = FALSE, 
                                                                use_cfac_type_in_sim = FALSE, 
                                                                global_input_data$feature_params$condition_class_characteristics, 
                                                                use_offset_metric = TRUE, 
                                                                global_input_data$global_params$user_transform_function)
    }
    
    flog.info('saving background counterfactuals object')
    saveRDS(cfacs_object, background_cfacs_file)
    
  }
  
  return(cfacs_object)
}

estimate_illegal_sites <- function(loss_prob, time_steps, site_num){
  estimated_sites_cleared = (site_num - site_num*(1 - loss_prob)^time_steps)
  return(estimated_sites_cleared)
}


build_global_params <- function(user_global_params, user_transform_function, simulation_params_group){
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
  
  if (global_params$simulation_folder != 'default'){
    simulation_folder = write_folder(global_params$simulation_folder)
    base_run_folder = paste0(simulation_folder, '/simulation_runs/')
  } else {
    simulation_folder = vector()
    base_run_folder = ('simulation_runs/')
  }
  
  if (global_params$simulation_inputs_folder != 'default'){
    global_params$simulation_inputs_folder = write_folder(global_params$simulation_inputs_folder)
  } else {
    if (global_params$simulation_folder != 'default'){
      global_params$simulation_inputs_folder =  write_folder(paste0(simulation_folder, '/simulation_inputs/'))
    } else {
      global_params$simulation_inputs_folder = write_folder('simulation_inputs/')
    }
  }
  current_run = find_current_run(base_run_folder)
  
  if ((global_params$unique_simulation_folder) & (length(current_run) > 0)){
    current_run = current_run + 1
  } else {
    current_run = 1
  } 
  
  if (class(global_params$scenario_subset) == 'character'){
    if (global_params$scenario_subset == 'all'){
      global_params$scenario_subset = seq_along(simulation_params_group)
    } else {
      flog.error('incorrect setting for scenario_subset - must be vector of natural numbers or "all"')
    }
  }
  
  global_params$run_folder = write_folder(paste0(base_run_folder, formatC(current_run, width = global_params$numeric_placeholder_width, format = "d", flag = "0"), '/'))
  global_params$output_folder = write_folder(paste0(global_params$run_folder, '/simulation_outputs/'))
  global_params$collated_folder = write_folder(paste0(global_params$run_folder, 'collated_outputs/'))
  global_params$simulation_params_folder = write_folder(paste0(global_params$run_folder, '/simulation_params/'))
  
  flog.info('writing simulation outputs into %s', global_params$run_folder)
  
  global_params <- initialise_cores(global_params)
  global_params$strt = Sys.time()
  global_params$feature_num = length(global_params$features_to_use_in_simulation)
  global_params$user_transform_function = user_transform_function
  
  return(global_params)
}


build_simulation_params_group <- function(user_simulation_params, features_to_use_in_simulation){
  
  default_simulation_params = initialise_default_simulation_params()
  if (!is.null(user_simulation_params) == TRUE){  
    simulation_params <- overwrite_current_params(user_params = user_simulation_params, default_params = default_simulation_params)
    #check_simulation_params(simulation_params)
  } else{
    simulation_params = default_simulation_params
  }
  
  simulation_params_object = build_simulation_variants(simulation_params)
  simulation_params_group <- lapply(seq_along(simulation_params_object$param_variants), 
                                    function(i) process_current_simulation_params(simulation_params_object$param_variants[[i]], 
                                                                                  simulation_params_object$common_params, features_to_use_in_simulation))
  
  return(simulation_params_group)
}


build_feature_params <- function(user_feature_params, global_params){
  default_feature_params = initialise_default_feature_params()
  
  if (!is.null(user_feature_params) == TRUE){  
    feature_params <- overwrite_current_params(user_params = user_feature_params, default_params = default_feature_params)
  } else {
    feature_params = default_feature_params
  }
  
  feature_params$management_condition_class_bounds = feature_params$management_condition_class_bounds[global_params$features_to_use_in_simulation]
  feature_params$condition_class_bounds = feature_params$condition_class_bounds[global_params$features_to_use_in_simulation]
  feature_params$background_dynamics_bounds = feature_params$background_dynamics_bounds[global_params$features_to_use_in_simulation]
  feature_params$management_dynamics_bounds = feature_params$management_dynamics_bounds[global_params$features_to_use_in_simulation]
  
  feature_params$background_condition_class_num = unlist(lapply(seq_along(feature_params$condition_class_bounds), 
                                                                function(i) nrow(feature_params$condition_class_bounds[[i]])))

  feature_params$condition_class_characteristics = do.call(rbind, feature_params$condition_class_bounds)
  feature_params$management_condition_class_characteristics = do.call(rbind, feature_params$management_condition_class_bounds)

  return(feature_params)
}


build_condition_classes <- function(feature_vals, max_cell_ID, global_params, condition_class_characteristics, condition_class_mode_num){

  if (length(global_params$condition_class_raster_files) == global_params$feature_num){
    if (all(file.exists(global_params$condition_class_raster_files))){
      load_condition_classes = TRUE
    } else {
      flog.info('could not locate condition class raster files ..')
      load_condition_classes = FALSE
    }
  } else {
    load_condition_classes = FALSE
  }

  if (load_condition_classes == TRUE){
    flog.info('loading condition class raster files ..')
    flog.info('instate shifts of condition classes')
    stop()
    background_condition_classes <- build_sparse_features(global_params$condition_class_raster_files[global_params$features_to_use_in_simulation],
                                                          max_cell_ID, 
                                                          FALSE)
    
  } else {
    
    flog.info('building condition classes ..')

    background_condition_classes = construct_condition_classes(feature_vals, 
                                                               max_cell_ID,
                                                               condition_class_characteristics, 
                                                               condition_class_mode_num)

  }
  
  return(background_condition_classes)
}


build_output_data <- function(input_data, simulation_params){
  output_data = list()
  output_data$index_object = build_index_object(input_data, simulation_params)
  interventions = vector('list', 5)
  names(interventions) = names(output_data$index_object$site_indexes_used)
  output_data$interventions = interventions
  output_data$offset_pool_object <- list()

  return(output_data)
} 


build_initial_credit <- function(simulation_params, input_data){
  if (simulation_params$transform_initial_credit == TRUE){
    if (length(input_data$global_params$user_transform_function) > 0){
      current_credit = input_data$global_params$user_transform_function(simulation_params$initial_credit, simulation_params$transform_params)
    } else{
      flog.error('user_transform_function not set')
      stop()
    }
  } else {
    if ((simulation_params$use_offset_metric == FALSE) &
        ( length(simulation_params$initial_credit) != length(simulation_params$features_to_use_in_offset_calc))){
      flog.error('setting length of credit vector to match simulation_params$features_to_use_in_offset_calc')
      current_credit = matrix(rep(simulation_params$initial_credit, 
                                  length(simulation_params$features_to_use_in_offset_calc)), 
                              ncol = length(simulation_params$features_to_use_in_offset_calc))
    } else{
      current_credit = simulation_params$initial_credit
    }
  }
  credit_object = list()
  credit_object$current_credit = current_credit
  credit_object$match_flag = FALSE
  return(credit_object)
}

sample_current_dynamics <- function(current_mode_characteristics, condition_class_characteristics, initial_val, feature_dynamics_set, store_dynamics_as_differential, sample_dynamics, dynamics_sample_type){
  
  current_condition_class_mode = current_mode_characteristics$condition_class
  current_feature = condition_class_characteristics$feature[current_condition_class_mode] 
  current_condition_class = condition_class_characteristics$condition_class[current_condition_class_mode]
  
  current_feature_dynamics_set = feature_dynamics_set[[current_feature]][[current_condition_class]]
  
  if (sample_dynamics == FALSE){
    current_feature_dynamics = current_feature_dynamics_set$best_estimate
    return(current_feature_dynamics)
  } 
  
  if (dynamics_sample_type == 'by_initial_value'){
    current_discriminator = initial_val - current_feature_dynamics_set$best_estimate[1]
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
      browser()
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


build_mode_mapping <- function(condition_classes, feature_groups, max_cell_ID, features_to_use, feature_dynamics_bounds){

  mode_num = unlist(lapply(feature_dynamics_bounds, 'length'))
  global_mode_translate = (1:(max_cell_ID*length(features_to_use)) - 1)*sum(mode_num)

  mode_translate = lapply(seq_along(feature_groups), function(i) rep(global_mode_translate[i], length(feature_groups[[i]])))
  
  for (feature_ind in features_to_use){
    mode_ID_block_template = Matrix(0, max_cell_ID, 1)
  }
  
  mode_ID_block[unlist(feature_groups)] = unlist(mode_translate)
  mode_ID_block = matrix(rep(mode_ID_block, length(features_to_use)), ncol = length(features_to_use))

  
  dim(condition_classes) = c(max_cell_ID, length(features_to_use))
  feature_mode_translate = Matrix(rep(cumsum(mode_num) - 1, nrow(condition_classes)), ncol = length(mode_num), byrow = TRUE)
  mode_ID_block = mode_ID_block + feature_mode_translate
  mode_ID_block = mode_ID_block * (condition_classes > 0)
  mode_ID_block = mode_ID_block + condition_classes

  return(mode_ID_block)

}


# build_mode_mapping <- function(condition_classes, site_characteristics, features_to_use, feature_dynamics_bounds){
#   
#   mode_num = unlist(lapply(feature_dynamics_bounds, 'length'))
#   global_mode_translate = (seq_len(site_characteristics$site_num) - 1)*sum(mode_num)
#   
#   site_mode_translate = lapply(seq_len(site_characteristics$site_num), function(i) rep(global_mode_translate[i], site_characteristics$cell_num[[i]]))
#   mode_ID_block = Matrix(0, max(site_characteristics$cell_ids), 1)
#   
#   mode_ID_block[unlist(site_characteristics$cell_id_groups)] = unlist(site_mode_translate)
#   mode_ID_block = matrix(rep(mode_ID_block, length(features_to_use)), ncol = length(features_to_use))
#   
#   browser()
#   feature_mode_translate = Matrix(rep(cumsum(mode_num) - 1, nrow(condition_classes)), ncol = length(mode_num), byrow = TRUE)
#   mode_ID_block = mode_ID_block + feature_mode_translate
#   mode_ID_block = mode_ID_block * (condition_classes > 0)
#   mode_ID_block = mode_ID_block + condition_classes
#   
#   return(mode_ID_block)
#   
# }

build_dynamics <- function(dynamics_type, mode_characteristics, initial_vals, condition_class_characteristics, sample_dynamics, store_dynamics_as_differential,
                           dynamics_sample_type, feature_dynamics_bounds, mode_num, dynamics_length){
  
  feature_dynamics = matrix(0, nrow(mode_characteristics), dynamics_length)
  
  if (store_dynamics_as_differential == TRUE){
    dynamics_length = dynamics_length - 1
  }
  
  if (dynamics_type == 'management'){
    mode_characteristics$condition_class = mode_characteristics$management_condition_class
  }
  
  for (mode_ind in seq_len(nrow(mode_characteristics))){
    
    feature_dynamics[mode_ind, 1:dynamics_length] = sample_current_dynamics(mode_characteristics[mode_ind, ],
                                                                                                     condition_class_characteristics,
                                                                                                     initial_vals[mode_ind], 
                                                                                                     feature_dynamics_bounds, 
                                                                                                     store_dynamics_as_differential,
                                                                                                     sample_dynamics,
                                                                                                     dynamics_sample_type)
  }
  
  return(feature_dynamics)
  
}



# build_dynamics <- function(site_scale_features_to_use, site_characteristics, features_to_use, sample_dynamics, dynamics_type, store_dynamics_as_differential, 
#                            dynamics_sample_type, feature_dynamics_bounds, condition_classes){
# 
#   if (dynamics_type == 'element_scale'){
#     
#     dynamics_set = lapply(seq_along(site_scale_features_to_use), 
#                           function(i) lapply(features_to_use,
#                                              function(j) lapply(seq_along(site_scale_features_to_use[[i]][[j]]), 
#                                                                 function(k) sample_current_dynamics(feature_dynamics_bounds[[j]],
#                                                                                                     condition_classes[[i]][[j]][k],
#                                                                                                     site_scale_features_to_use[[i]][[j]][k],
#                                                                                                     store_dynamics_as_differential, 
#                                                                                                     sample_dynamics,
#                                                                                                     dynamics_sample_type)  )))
#   } else if (dynamics_type == 'site_scale'){
# 
#       # dynamics_set = lapply(seq_along(condition_classes), 
#       #                       function(i) lapply(features_to_use,
#       #                                          function(j) lapply(seq_along(condition_classes[[i]][[j]]), 
#       #                                                             function(k) sample_current_dynamics(feature_dynamics_bounds[[j]],
#       #                                                                                                 condition_classes[[i]][[j]][k],
#       #                                                                                                 mean(site_scale_features_to_use[[i]][[j]][[k]]),
#       #                                                                                                 store_dynamics_as_differential, 
#       #                                                                                                 sample_dynamics,
#       #                                                                                                 dynamics_sample_type) )))
#       # 
#       
#       browser()
#       
#       mode_num = unlist(lapply(feature_dynamics_bounds, 'length'))
#       global_mode_translate = (seq_len(site_characteristics$site_num) - 1)*sum(mode_num)
#       
#       site_mode_translate = lapply(seq_len(site_characteristics$site_num), function(i) rep(global_mode_translate[i], site_characteristics$cell_num[[i]]))
#       element_mode_translate = Matrix(0, max(site_characteristics$cell_ids), 1)
#       
#       element_mode_translate[unlist(site_characteristics$cell_id_groups)] = unlist(site_mode_translate)
#       element_mode_translate = matrix(rep(element_mode_translate, length(features_to_use)), ncol = length(features_to_use))
#       
#       feature_mode_translate = Matrix(rep(cumsum(mode_num) - 1, nrow(condition_classes)), ncol = length(mode_num), byrow = TRUE)
#       element_mode_translate = element_mode_translate + feature_mode_translate
#       element_mode_translate = element_mode_translate * (condition_classes > 0)
#       mode_ID = element_mode_translate + condition_classes
#       unique_mode_IDs = unique(as.vector(mode_ID))
#       
#       dynamics_template = Matrix(0, site_characteristics$site_num*sum(mode_num), nrow(feature_dynamics_bounds[[1]][[1]]))
#       
#       ccm = Matrix(0, nrow = nrow(condition_classes), ncol = ncol(condition_classes))
#       partitioned_sums = lapply(seq_along(site_scale_features_to_use), 
#                                 function(i) colSums(site_scale_features_to_use[[i]]))
#       
#       ccms = lapply(seq_along(site_scale_features_to_use), 
#                     function(i) condition_classes[site_characteristics$cell_id_groups[[i]], ])
#       
#       i = 10
#       lapply(features_to_use, function(i) unique(ccms[[10]][, i]))
#       # dynamics_set = lapply(seq_along(condition_classes), 
#       #                       function(i) lapply(features_to_use,
#       #                                          function(j) lapply(seq_along(condition_classes[[i]][[j]]), 
#       #                                                             function(k) sample_current_dynamics(feature_dynamics_bounds[[j]],
#       #                                                                                                 condition_classes[[i]][[j]][k],
#       #                                                                                                 mean(site_scale_features_to_use[[i]][[j]][[k]]),
#       #                                                                                                 store_dynamics_as_differential, 
#       #                                                                                                 sample_dynamics,
#       #                                                                                                 dynamics_sample_type) )))
#       # 
#       
#   }
#   return(dynamics_set)
# }
# 

find_current_mode <- function(current_feature_val, current_condition_class_characteristics){
  
  if (length(current_feature_val) == 0){
    current_mode = 0
  } else {
    if (length(current_feature_val) == 1){
      mode_discriminator = current_feature_val
    } else if (length(current_feature_val) > 1){
      mode_discriminator = mean(current_feature_val)
    } 
    
    if (mode_discriminator == 0){
      current_mode = 0
    } else {
      current_modes = lapply(seq(nrow(current_condition_class_characteristics)), 
                             function(i) ( (mode_discriminator >= min(current_condition_class_characteristics[i, ]) && mode_discriminator <= max(current_condition_class_characteristics[[i]])) ))
      
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
    
  }
  
  return(current_mode)
  
}



construct_condition_classes <- function(features_to_use, max_cell_id, condition_class_characteristics, condition_class_mode_num){

  condition_classes = Matrix(0, dim(features_to_use)[1], dim(features_to_use)[2], sparse = TRUE)

  for (feature_ind in unique(condition_class_characteristics$feature)){

    flog.info('building condition classes for feature %s', feature_ind)
    current_block = (1:max_cell_id) + max_cell_id*(feature_ind - 1)
    current_condition_class_set = which(!(is.na(match(condition_class_characteristics$feature, feature_ind))))
    current_condition_class_characteristics = condition_class_characteristics[current_condition_class_set, ]
    current_condition_classes = sapply(features_to_use[current_block, ], find_current_mode, current_condition_class_characteristics)
    if (feature_ind > 1){
      current_condition_classes[current_condition_classes > 0] = current_condition_classes[current_condition_classes > 0] + sum(condition_class_mode_num[1:(feature_ind - 1)])
    }
    condition_classes[current_block, ] = current_condition_classes

  }

  return(condition_classes)
  
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

save_params <- function(global_params, feature_params, simulation_params_group){
  
  for (scenario_ind in global_params$scenario_subset){
    write_folder(paste0(global_params$output_folder, '/scenario_', formatC(scenario_ind, width = global_params$numeric_placeholder_width, format = "d", flag = "0"), '/'))
  }
  
  global_params_file = paste0(global_params$simulation_params_folder,  'global_params')
  saveRDS(global_params, paste0(global_params$simulation_params_folder,  'global_params.rds'))
  dump('global_params', paste0(global_params$simulation_params_folder,  'global_params.R'), control = NULL)
  
  saveRDS(feature_params, paste0(global_params$simulation_params_folder,  'feature_params.rds'))
  dump('feature_params', paste0(global_params$simulation_params_folder,  'feature_params.R'), control = NULL)
  
  for (scenario_ind in global_params$scenario_subset){
    current_simulation_params = simulation_params_group[[scenario_ind]]
    simulation_params_file = paste0(global_params$simulation_params_folder,  
                                    'scenario_', formatC(scenario_ind, width = global_params$numeric_placeholder_width, format = "d", flag = "0"), '_simulation_params')
    
    saveRDS(current_simulation_params, paste0(simulation_params_file, '.rds'))
    dump('current_simulation_params', paste0(simulation_params_file, '.R'), control = NULL)
  }
  
}



generate_simulation_combs <- function(simulation_params_group){
  
  param_inds <- lapply(seq_along(simulation_params_group), function(i) 1:length(simulation_params_group[[i]]))
  param_combs <- expand.grid(param_inds)
  
  return(param_combs)
  
}


process_current_simulation_params <- function(current_simulation_params, common_params, features_to_use_in_simulation){
  
  current_simulation_params = append(current_simulation_params, common_params)
  
  if (current_simulation_params$use_uncoupled_offsets == TRUE){
    current_simulation_params$offset_time_horizon_type = 'current'  # 'current' - used for banking only - determine accrued offset gains till current year.
  } else {
    current_simulation_params$offset_time_horizon_type = 'future'  #'future' - project from time of development to offset time horizon
  }
  
  current_simulation_params$features_to_use_in_offset_calc = match(current_simulation_params$features_to_use_in_offset_calc, features_to_use_in_simulation)
  current_simulation_params$features_to_offset = match(current_simulation_params$features_to_offset, features_to_use_in_simulation)
  # current_simulation_params$offset_calc_type = current_simulation_params$offset_action_params[1]
  # current_simulation_params$offset_action_type = current_simulation_params$offset_action_params[2]
  
  current_simulation_params$use_site_sets = (current_simulation_params$uncoupled_offset_type == 'site_set') || (current_simulation_params$use_uncoupled_offsets == FALSE)
  if ((current_simulation_params$uncoupled_offset_type == 'site_set') || (current_simulation_params$use_uncoupled_offsets == FALSE)){
    current_simulation_params$match_type = 'site_set'
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
  
  indexes_to_vary = which(as.vector(lapply(simulation_params, 'length')) > 1)
  variants = simulation_params[indexes_to_vary]
  common_params = simulation_params[setdiff(seq_along(simulation_params), indexes_to_vary)]
  
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
  simulation_params_object$common_params = setNames(lapply(seq_along(common_params), function(i) common_params[[i]][[1]]), 
                                                    names(common_params));
  
  return(simulation_params_object)
  
}

build_current_variant <- function(current_variant_indexes, variants){
  current_simulation_params_variant <- lapply(seq_along(variants), function(i) variants[[i]][[current_variant_indexes[i] ]])
  names(current_simulation_params_variant) = names(variants)
  return(current_simulation_params_variant)
}



site_set_list_names <- function(){
  list_names = c("site_indexes", "site_num_remaining", "offset_yrs", "site_ecologies", "site_sums_at_offset", "cfac_trajs", "site_vals_used",
                 "restoration_vals", "cfac_vals")
  return(list_names)
}

#' @export
build_stochastic_intervention <- function(time_steps, intervention_start, intervention_end, intervention_num, sd){

  intervention_control = lapply(seq(time_steps), function(i) array(0, 1))
  intervention_control[intervention_start:intervention_end] = split_vector((intervention_end - intervention_start + 1), intervention_num, sd, min_width = -1)
  return(intervention_control)
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



build_index_object <- function(input_data, simulation_params){

  index_object = list()
  
  if (simulation_params$development_selection_type == 'pre_determined'){
    index_object$development_control <- transform_control(simulation_params$development_control, 
                                                          input_data$site_characteristics$site_IDs, 
                                                          input_data$site_characteristics$site_IDs[which(unlist(input_data$dev_probability_list) > 0)])
  }
  
  if (simulation_params$use_uncoupled_offsets == TRUE){
    if (simulation_params$uncoupled_offset_selection_type == 'pre_determined'){
      index_object$uncoupled_offset_control <- transform_control(simulation_params$uncoupled_offset_control, 
                                                              input_data$site_characteristics$site_IDs, 
                                                              input_data$site_characteristics$site_IDs[which(unlist(input_data$offset_probability_list) > 0)])
      
    } else if (simulation_params$uncoupled_offset_selection_type == 'stochastic'){
      index_object$uncoupled_offset_control = simulation_params$uncoupled_offset_control
    }
  }
  
  if ((simulation_params$development_selection_type == 'pre_determined') & 
    (simulation_params$uncoupled_offset_selection_type == 'pre_determined')){
    internal_dev_indexes = unlist(index_object$development_control)[which(unlist(index_object$development_control)>0)]
    internal_offset_indexes = unlist(index_object$uncoupled_offset_control[which(unlist(index_object$uncoupled_offset_control)>0)])
    overlap = which(internal_dev_indexes %in% internal_offset_indexes)
    if (any(overlap)){
      flog.error(paste('pre-defined development and offset regions overlap for sites ', 
                       paste(list(input_data$site_characteristics$site_IDs[internal_dev_indexes[overlap] ])),
                       '- redefine development and offset regions'))
      stop()
    }
  }
  
  if (simulation_params$development_selection_type == 'pre_determined'){
    index_object$intervention_control = unlist(lapply(seq_along(index_object$development_control), 
                                                      function(i) length(which(index_object$development_control[[i]] > 0))))
  } else if (simulation_params$development_selection_type == 'stochastic'){
    index_object$intervention_control = unlist(simulation_params$development_control)
  }

  offset_indexes_to_exclude = which(unlist(input_data$offset_probability_list) == 0)
  unregulated_indexes_to_exclude = which(unlist(input_data$unregulated_probability_list) == 0)
  dev_indexes_to_exclude = which(unlist(input_data$dev_probability_list) == 0)
  
  if (simulation_params$development_selection_type == 'pre_determined'){
    offset_indexes_to_exclude = unique(c(which(unlist(input_data$offset_probability_list) == 0), 
                                         unlist(simulation_params$development_control)))
    unregulated_indexes_to_exclude = unique(c(which(unlist(input_data$unregulated_probability_list) == 0), 
                                              unlist(simulation_params$development_control)))
  }
  
  if (simulation_params$uncoupled_offset_selection_type == 'pre_determined'){
    dev_indexes_to_exclude = unique(c(which(unlist(input_data$dev_probability_list) == 0), 
                                      unlist(simulation_params$uncoupled_offset_control)))
    unregulated_indexes_to_exclude = unique(c(unregulated_indexes_to_exclude, 
                                              unlist(simulation_params$uncoupled_offset_control)))
  }
  

  index_object$site_indexes = seq(length(input_data$site_characteristics$cell_id_groups))
  index_object$uncoupled_offset_pool = list()
  index_object$site_indexes_used = vector('list', 5)
  names(index_object$site_indexes_used) = c('offset_object', 'uncoupled_offset_object', 'development_object', 'uncoupled_development_object', 'unregulated_loss_object')
  
  index_object$available_indexes = list()

  
  index_object$available_indexes$offsets = set_available_indexes(index_object$site_indexes, 
                                                                 offset_indexes_to_exclude, 
                                                                 input_data$site_characteristics$cell_id_groups, 
                                                                 input_data$site_scale_features, 
                                                                 screen_site_zeros = simulation_params$screen_offset_zeros,
                                                                 min_site_screen_size = simulation_params$min_site_screen_size,
                                                                 max_site_screen_size_quantile = simulation_params$max_site_screen_size_quantile,
                                                                 simulation_params$features_to_use_in_offset_calc)
  
  index_object$available_indexes$developments = set_available_indexes(index_object$site_indexes, 
                                                              dev_indexes_to_exclude,
                                                              input_data$site_characteristics$cell_id_groups, 
                                                              input_data$site_scale_features, 
                                                              screen_site_zeros = simulation_params$screen_dev_zeros,
                                                              min_site_screen_size = simulation_params$min_site_screen_size,
                                                              max_site_screen_size_quantile = simulation_params$max_site_screen_size_quantile,
                                                              simulation_params$features_to_use_in_offset_calc)
  
  index_object$available_indexes$unregulated_loss = set_available_indexes(index_object$site_indexes, 
                                                                          indexes_to_exclude = unregulated_indexes_to_exclude,
                                                                          input_data$site_characteristics$cell_id_groups, 
                                                                          input_data$site_scale_features, 
                                                                          screen_site_zeros = TRUE,
                                                                          min_site_screen_size = simulation_params$min_site_screen_size,
                                                                          max_site_screen_size_quantile = simulation_params$max_site_screen_size_quantile,
                                                                          simulation_params$features_to_use_in_offset_calc)

  return(index_object)
}


transform_control <- function(control_object, site_IDs, available_indexes){

  # determine matching site_IDs
  control_object <- lapply(seq_along(control_object), function(yr) site_IDs[ which(site_IDs  %in% control_object[[yr]])])

  # determine which site_IDs match intervention zone
  control_object <- lapply(seq_along(control_object), function(yr) available_indexes[which(available_indexes %in% control_object[[yr]])])
  
  # determine which site_IDs match intervention zone
  control_object <- lapply(seq_along(control_object), function(yr) which(site_IDs %in% control_object[[yr]]))
  
  empties <- which(unlist(lapply(seq_along(control_object), function(yr) length(control_object[[yr]]) == 0)))
  
  if (length(empties) > 0){
    control_object[empties] = 0
  }
  return(control_object)
}

set_available_indexes <- function(global_indexes, indexes_to_exclude, cell_id_groups, initial_features, screen_site_zeros, 
                                  min_site_screen_size, max_site_screen_size_quantile, features_to_use_in_offset_calc){

  if (screen_site_zeros == TRUE){
    
    initial_site_sums = lapply(seq_along(initial_features), 
                                 function(i) sum(initial_features[[i]][, features_to_use_in_offset_calc]))
    
    zeros_to_exclude = which(unlist(initial_site_sums) == 0)

    indexes_to_exclude = unique(c(indexes_to_exclude, zeros_to_exclude))
  }
  
  cell_num <- unlist(lapply(seq_along(cell_id_groups), function(i) length(cell_id_groups[[i]])))
  
  if (min_site_screen_size > 0){
    sites_to_exclude = which(cell_num < min_site_screen_size)
    indexes_to_exclude = unique(c(indexes_to_exclude, sites_to_exclude))
  } 
  
  if (max_site_screen_size_quantile < 1){
    sites_to_exclude = which(cell_num > quantile(cell_num, probs = max_site_screen_size_quantile))
    indexes_to_exclude = unique(c(indexes_to_exclude, sites_to_exclude))
  }
  
  indexes_to_remove = which(global_indexes %in% indexes_to_exclude)
  available_indexes = global_indexes
  
  if (length(indexes_to_remove) > 0){
    available_indexes = available_indexes[-indexes_to_remove]
  }
  
  return(available_indexes)
}

#' @export
scale_features <- function(features){
  flog.info('scaling features')

  for (feature_ind in seq(dim(features)[2])){
    sc = max(features[, feature_ind])
    if (sc > 0){
      block_to_scale = features[, feature_ind] > 0
      features[block_to_scale, feature_ind] = features[block_to_scale, feature_ind]/sc
    }
  }
  
  return(features)
}

# split_modes <- function(cell_id_groups, condition_class_layers){
#   browser()
#   condition_classes = lapply(seq_along(cell_id_groups), 
#                                          function(i) lapply(seq_along(condition_class_layers), 
#                                                             function(j) sort(unique(condition_class_layers[[j]][ cell_id_groups[[i]]]), decreasing = TRUE)))
#   
#   # zeros_to_remove <- lapply(seq_along(condition_classes), 
#   #                           function(i) lapply(seq_along(condition_classes[[i]]), 
#   #                                              function(j) which(condition_classes[[i]][[j]] == 0)))
#   
#   
#   return(condition_classes)
# }


separate_features_by_condition_class <- function(input_data, initial_features, condition_class_layer){
    
    flog.info('building condition class keys...')
    
    site_scale_condition_class_group = lapply(seq_along(input_data$site_characteristics$cell_id_groups), 
                                              function(i) lapply(seq_along(input_data$condition_classes[[i]]), 
                                                                 function(j) condition_class_layer[[j]][ input_data$site_characteristics$cell_id_groups[[i]] ]))
    
    input_data$site_scale_condition_class_key = lapply(seq_along(site_scale_condition_class_group), 
                                                           function(i) lapply(seq_along(site_scale_condition_class_group[[i]]), 
                                                                              function(j) lapply(input_data$condition_classes[[i]][[j]], 
                                                                                                 function(current_mode) build_site_condition_class(site_scale_condition_class_group[[i]][[j]], 
                                                                                                                                                   current_mode, 
                                                                                                                                                   input_data$global_params$sparse_representation)
                                                                              )))
    
    landscape_scale_condition_class_key = lapply(seq_along(input_data$site_scale_condition_class_key), 
                                                 function(i) lapply(seq_along(input_data$site_scale_condition_class_key[[i]]),
                                                                    function(j) lapply(seq_along(input_data$site_scale_condition_class_key[[i]][[j]]), 
                                                                                       function(k) select_site_scale_condition_class(input_data$site_characteristics$cell_id_groups[[i]], 
                                                                                                                                     input_data$site_scale_condition_class_key[[i]][[j]][[k]], 
                                                                                                                                     input_data$condition_classes[[i]][[j]][[k]])
                                                                    )))
    
    flog.info('saving site scale condition class key')
    
    saveRDS(object = input_data$site_scale_condition_class_key, paste0(input_data$global_params$simulation_inputs_folder, 'site_scale_condition_class_key.rds'))
    
    flog.info('saving landscape scale condition class key')
    
    saveRDS(object = landscape_scale_condition_class_key, paste0(input_data$global_params$simulation_inputs_folder, 'landscape_scale_condition_class_key.rds'))

    site_scale_feature_group = lapply(seq_along(input_data$site_characteristics$cell_id_groups), 
                                      function(i) lapply(seq_along(initial_features), 
                                                         function(j) initial_features[[j]][ input_data$site_characteristics$cell_id_groups[[i]] ] ))

    flog.info('building site scale features...')
    input_data$site_scale_features = lapply(seq_along(input_data$site_scale_condition_class_key), 
                                                   function(i) lapply(seq_along(input_data$site_scale_condition_class_key[[i]]),
                                                                      function(j) lapply(seq_along(input_data$site_scale_condition_class_key[[i]][[j]]), 
                                                                                         function(k) select_site_scale_condition_class(site_scale_feature_group[[i]][[j]], 
                                                                                                                                       input_data$site_scale_condition_class_key[[i]][[j]][[k]], 
                                                                                                                                       input_data$condition_classes[[i]][[j]][[k]])
                                                                      )))
    
  return(input_data)
  
}

select_site_scale_condition_class <- function(current_site_scale_group, current_condition_class_group, current_condition_class_mode){
 
  if (current_condition_class_mode > 0){
    current_site_scale_group <- current_site_scale_group[current_condition_class_group]
  } else {
    current_site_scale_group = vector()
  }
  
  return(current_site_scale_group)
}

build_site_condition_class <- function(site_condition_class_layer, current_condition_class_mode, sparse_representation){
  
  if (current_condition_class_mode > 0){
    site_condition_class_key = which(site_condition_class_layer == current_condition_class_mode)
  } else {
    site_condition_class_key = vector()
  }
  
#   else {
#     if (sparse_representation == TRUE){
#       site_condition_class_key = Matrix(0, sum(site_condition_class_layer == 0), nrow = 1, sparse = TRUE)
#     }  else {
#       current_condition_class_feature = matrix(0, sum(site_condition_class_layer == 0), nrow = 1)
#     }
#   }
  
  return(site_condition_class_key)
  
}




#transforms planning units array into series of arrays describing allocation of elements to sites
#' @export
build_ID_characteristics <- function(ID_raster){
  
  raster_vals = getValues(ID_raster)
  
  ID_characteristics = list()
  
  #create site ID's as consecutive integers
  ID_characteristics$site_IDs = unique(raster_vals)
  # scan through planning units ID array and associate all elements with the same ID val to a particular site
  # results in nested list where raster array indicies corresponding to a particular planning unit ID are assigned to the same nested list block
  ID_characteristics$cell_id_groups <- lapply(seq_along(ID_characteristics$site_IDs), function(i) which(raster_vals == ID_characteristics$site_IDs[i]))
  ID_characteristics$site_num = length(ID_characteristics$cell_id_groups)
  ID_characteristics$cell_num <- lapply(ID_characteristics$cell_id_groups, length)
  ID_characteristics$landscape_dims = dim(ID_raster)
  ID_characteristics$cell_ids = 1:(ID_characteristics$landscape_dims[1] * ID_characteristics$landscape_dims[2])
  ID_characteristics$max_cell_ID = ID_characteristics$landscape_dims[1] * ID_characteristics$landscape_dims[2]
  # ID_characteristics$col_inds = ceiling(ID_characteristics$cell_ids/ID_characteristics$landscape_dims[2])
  # ID_characteristics$row_inds = rep(1:ID_characteristics$landscape_dims[1], ID_characteristics$landscape_dims[2])

  return(ID_characteristics)
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
shp_to_raster <- function(shp, raster_dims){
  r <- raster(ncol=raster_dims[2], nrow=raster_dims[1])
  extent(r) <- extent(shp)
  raster_object <- rasterize(shp, r)
  return(raster_object)
}

#' @export
load_rasters <- function(current_filenames){

  for (feature_ind in seq_along(current_filenames)){

    current_raster = raster(current_filenames[feature_ind])
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

