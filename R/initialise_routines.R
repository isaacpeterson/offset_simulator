#generic set of initialisation routines that are called for every simulation

run_initialise_routines <- function(user_global_params = NULL, user_simulation_params = NULL, user_simulated_ecology_params = NULL){
  #' @import foreach
  #' @import doParallel
  #' @import abind
  #' @import pixmap

  default_global_params = initialise_default_global_params()
  default_simulation_params = initialise_default_simulation_params()
  default_simulated_ecology_params = initialise_default_simulated_ecology_params()
  
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
  
  if (!is.null(user_simulated_ecology_params) == TRUE){  
    simulated_ecology_params <- overwrite_current_params(user_params = user_simulated_ecology_params, default_params = default_simulated_ecology_params)
  } else {
    simulated_ecology_params = default_simulated_ecology_params
  }
  
  # run simulation with identical realisation instantiation
  if (global_params$set_seed == TRUE){
    seed=123
    flog.info('fixing random number seed to %d', 123)
    set.seed(seed)
  }
  
  #params_object <- check_param_conflicts(global_params, simulation_params, simulated_ecology_params)
  
  simulation_params_group = generate_simulation_params_group(simulation_params)
  
  global_params <- write_simulation_folders(global_params, length(simulation_params_group))
  
  # generate simulated ecology
  if (global_params$use_simulated_data == TRUE) {
    current_filenames <- list.files(path = global_params$simulation_inputs_folder, all.files = FALSE,
                                    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                                    include.dirs = FALSE, no.. = FALSE)
    if ( (length(current_filenames) == 0) | (global_params$run_from_saved == FALSE)){
      construct_simulated_data(simulated_ecology_params, 
                               global_params$simulation_inputs_folder, 
                               global_params$simulation_params_folder, 
                               global_params$backup_simulation_inputs)
    }
  }
  
  saveRDS(global_params, paste0(global_params$simulation_params_folder, 'global_params.rds'))
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
  
  global_params <- initialise_cores(global_params)
  global_params$strt = Sys.time()

  params_object = list()
  params_object$global_params = global_params
  params_object$simulation_params_group = simulation_params_group
  return(params_object)
  
}


check_param_conflicts <- function(simulation_params, simulated_ecology_params){
  
  feature_test = match(simulation_params$features_to_use_in_simulation, seq(simulated_ecology_params$feature_num))
  if (any(is.na(feature_test))){
    flog.error(paste('\n ERROR: simulation_params$features_to_use_in_simulation does not match simulated ecology feature parameters'))
    stop()
  } 
  
  offset_calc_test = 
  if (any(is.na(offset_calc_test))){
    flog.error(paste('\n ERROR: simulation_params$features_to_use_in_offset_calc does not match simulation_params$features_to_use_in_simulation'))
    stop()
  } 
  params_object = list()
  params_object$global_params = global_params
  params_object$simulation_params = simulation_params
  params_object$simulated_ecology_params = simulated_ecology_params
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


check_simulation_params <- function(simulation_params, decline_rates_initial){
  
  offset_action_set = simulation_params$offset_action_params
  valid_offset_calc_type = c('net_gains', 'restoration_gains', 'avoided_condition_decline', 'avoided_loss',
                             'protected_condition', 'current_condition', 'restored_condition')
  
  if (mean(unlist(decline_rates_initial)) > simulation_params$restoration_rate){
    flog.error('restoration parameter set below decline rate')
    stop()
  }
  
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
  
#   if ( (length(simulation_params$mean_decline_rates) != length(simulation_params$features_to_use_in_simulation)) ){
#     flog.error(cat('\n decline rates mean parameter does not match feature number'))
#     stop()
#   }
#   
#   if ( (length(simulation_params$decline_rate_std) != length(simulation_params$features_to_use_in_simulation)) ){
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
    flog.error( cat('\nERROR: features in simulation_params$features_to_use do not match initial_ecology_dimensions'))
    stop()
  }
  output_object <- lapply(seq_along(input_object),
                          function(i) (input_object[[i]][features_to_use]))
  return(output_object)
}


write_nested_folder <- function(current_folder){
  while (!(file.exists(current_folder))){
    dir_to_use = current_folder
    while (!(file.exists(dir_to_use))){
      dir_to_use_tmp = dir_to_use
      dir_to_use = dirname(dir_to_use)
    }
    dir.create(dir_to_use_tmp)
  }
  return(current_folder)
}

write_folder <- function(current_folder){
  if (!file.exists(current_folder)){
    dir.create(current_folder)
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
    simulation_folder = write_nested_folder(global_params$simulation_folder)
    simulation_inputs_folder = write_folder(paste0(simulation_folder, '/simulation_inputs/'))
    base_run_folder = paste0(simulation_folder, '/simulation_runs/')
  } else {
    simulation_inputs_folder = 'simulation_inputs/'
    base_run_folder = 'simulation_runs/'
  }
  
  current_run = find_current_run(base_run_folder)
  
  if ((global_params$unique_simulation_folder) & (length(current_run) > 0)){
    current_run = current_run + 1
  } else {
    current_run = 1
  } 

  global_params$run_folder = write_nested_folder(paste0(base_run_folder, formatC(current_run, width = 5, format = "d", flag = "0"), '/'))
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

build_simulation_params <- function(common_params, simulation_params_group, current_simulation_param_inds){

  current_simulation_params_variant <- lapply(seq_along(simulation_params_group), function(i) simulation_params_group[[i]][[current_simulation_param_inds[i] ]])
  current_simulation_params <- append(common_params, current_simulation_params_variant)
  names(current_simulation_params) <- append(names(common_params), names(simulation_params_group))

  return(current_simulation_params)
}


collate_current_policy <- function(current_simulation_params){
  
  if (current_simulation_params$use_offset_bank == TRUE){
    current_simulation_params$offset_time_horizon_type = 'current'  # 'current' - used for banking only - determine accrued offset gains till current year.
  } else {
    current_simulation_params$offset_time_horizon_type = 'future'  #'future' - project from time of development to offset time horizon
  }
  
  current_simulation_params$feature_num = length(current_simulation_params$features_to_use_in_simulation)   # The total number of features in the simulation
  current_simulation_params$features_to_use_in_offset_calc = match(current_simulation_params$features_to_use_in_offset_calc, current_simulation_params$features_to_use_in_simulation)
  
  current_simulation_params$offset_calc_type = current_simulation_params$offset_action_params[1]
  current_simulation_params$offset_action_type = current_simulation_params$offset_action_params[2]
  
  current_simulation_params$allow_developments_from_credit = (current_simulation_params$allow_developments_from_credit) || (current_simulation_params$offset_bank_type == 'credit')
  
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
    current_simulation_params$banked_offset_vec = generate_intervention_vec(time_steps = simulation_params$time_steps,
                                                                          prog_start = current_simulation_params$offset_bank_start,
                                                                          prog_end = current_simulation_params$offset_bank_end,
                                                                          total_simulation_num = current_simulation_params$offset_bank_num,
                                                                          sd = 1)
  } else {
    current_simulation_params$banked_offset_vec = list()
  }
  
  if (current_simulation_params$dev_counterfactual_adjustment == 'as_offset'){
    current_simulation_params$include_potential_developments_in_dev_calc = current_simulation_params$include_potential_developments_in_offset_calc
    current_simulation_params$include_potential_offsets_in_dev_calc = current_simulation_params$include_potential_offsets_in_offset_calc
    current_simulation_params$include_illegal_clearing_in_dev_calc = current_simulation_params$include_illegal_clearing_in_offset_calc
  } else {
    flog.info('using independent adjustment of cfacs in development impact calculation')
  }
  current_simulation_params$adjust_offset_cfacs_flag = any(current_simulation_params$include_potential_developments_in_offset_calc,
                                                         current_simulation_params$include_potential_offsets_in_offset_calc,
                                                         current_simulation_params$include_illegal_clearing_in_offset_calc) == TRUE
  current_simulation_params$adjust_dev_cfacs_flag = any(current_simulation_params$include_potential_developments_in_dev_calc,
                                                      current_simulation_params$include_potential_offsets_in_dev_calc,
                                                      current_simulation_params$include_illegal_clearing_in_dev_calc) == TRUE
  
  return(current_simulation_params)
  
}


generate_simulation_params_group <- function(simulation_params){
  
  list_cond = unlist(lapply(seq_along(simulation_params), function(i) is.list(simulation_params[[i]])))
  list_inds = which(list_cond)
  common_params = simulation_params[which(!list_cond)]
  
  if (length(list_inds) > 0){
    simulation_combs <- generate_simulation_combs(simulation_params[list_inds])  #generate all combinations of offset programs
    simulation_num = dim(simulation_combs)[1] #how many combinations there are in total
  } else {
    simulation_num = 1
  }
  simulation_params_group = vector('list', simulation_num)
  
  # when the interventions are set to take place
  intervention_vec = generate_intervention_vec(time_steps = simulation_params$time_steps,
                                                              prog_start = simulation_params$dev_start,
                                                              prog_end = simulation_params$dev_end,
                                                              simulation_params$total_dev_num,
                                                              sd = 1)
  
  for (simulation_ind in seq(simulation_num)){
    
    current_simulation_param_inds = unlist(simulation_combs[simulation_ind, ])
    current_simulation_params <- build_simulation_params(common_params, simulation_params[list_inds], current_simulation_param_inds) #write current policy as a list
    current_simulation_params$intervention_vec = intervention_vec
    
    simulation_params_group[[simulation_ind]] <- collate_current_policy(current_simulation_params)  #setup flags for cfacs, cfac adjustment etc.
    
  }
  
  return(simulation_params_group)
  
}



initialise_output_object <- function(parcels, initial_ecology, simulation_params, decline_rates_initial, dev_weights, offset_weights){
  output_object = list()
  output_object$offsets_object <- list()
  output_object$dev_object <- list()
  output_object$illegal_clearing_object <- list()
  output_object$credit_object <- list()
  output_object$offset_bank_object <- list()
  output_object$current_ecology = initial_ecology
  output_object$index_object <- initialise_index_object(parcels, 
                                                        initial_ecology, 
                                                        simulation_params, 
                                                        offset_indexes_to_exclude = which(unlist(offset_weights) == 0), 
                                                        dev_indexes_to_exclude = which(unlist(dev_weights) == 0))
  
  output_object$current_credit = array(0, length(simulation_params$features_to_use_in_offset_calc))
  output_object$decline_rates = decline_rates_initial
  return(output_object)
}

initialise_trajectories <- function(feature_num, land_parcels, time_steps){
  parcel_num =  length(land_parcels)
  trajectories = generate_nested_list(outer_dim = parcel_num, inner_dim = feature_num)
  for (parcel_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(feature_num)){
      parcel_dims = length(land_parcels[[parcel_ind]])
      trajectories[[parcel_ind]][[eco_ind]] = array(0, c(time_steps, parcel_dims))
    }
  }
  return(trajectories)
}


parcel_set_list_names <- function(){
  list_names = c("site_indexes", "parcel_num_remaining", "offset_yrs", "parcel_ecologies", "parcel_sums_at_offset", "cfac_trajs", "parcel_vals_used",
                 "restoration_vals", "cfac_vals", "region_ind")
  return(list_names)
}

generate_intervention_vec <- function(time_steps, prog_start, prog_end, total_simulation_num, sd){
  intervention_vec = array(0, time_steps)
  intervention_vec[prog_start:prog_end] = split_vector((prog_end - prog_start + 1), total_simulation_num, sd, min_width = -1)
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

#
#
# initialise_parcels_from_data <- function(data_folder, data_type){
#
#   if (data_type == 'grassland'){
#     filename = paste0(data_folder, 'planning.units.uid_20ha.pgm')
#     img = read.pnm(file = filename, cellres = 1)
#     parcel_array = img@grey
#   } else if (data_type == 'hunter'){
#     parcels_raster <- readRDS(paste0(data_folder, 'parcels_raster.rds'))
#     LGA_raster <- readRDS(paste0(data_folder, 'LGA_raster.rds'))
#     parcels_mask_raster <- readRDS(paste0(data_folder, 'parcels_mask_raster.rds'))
#     parcel_array = as.matrix(parcels_raster*parcels_mask_raster)
#     parcel_array[is.na(parcel_array)] = 0
#   }
#
#   land_index_vals = unique(as.vector(parcel_array))
#   landscape_dims = dim(parcel_array)
#   land_parcels <- lapply(seq_along(land_index_vals), function(i) which(parcel_array == land_index_vals[i]))
#   regions = list()
#   regions[[1]] = seq_len(length(land_parcels))
#   region_num = length(regions)
#   parcels = list()
#   parcels$landscape_dims = landscape_dims
#   parcels$site_indexes = seq_along(land_parcels)
#   parcels$land_parcel_num = length(land_parcels)
#   parcels$land_parcels = land_parcels
#   parcels$regions = regions
#   parcels$region_num = region_num
#   parcels$parcel_array = parcel_array
#
#   return(parcels)
# }





raster_to_array <- function(raster_object){
  raster_array = as.matrix(raster_object)
  raster_array[is.na(raster_array)] = 0
  return(raster_array)
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
  region_vx = split_vector(ecology_params$region_num_x, parcel_num_x, 1, min_width = 3) # perform similar operation used to split array into smallest elements, but this time for land parcels, arranging into regions
  region_vy = split_vector(ecology_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  regions = mcell(site_indexes, region_vx, region_vy)   # split land parcel indexes into regions
  
  region_num = length(regions$elements)
  
  parcels$site_indexes = site_indexes
  parcels$land_parcel_num = land_parcel_num
  parcels$land_parcels = land_parcels$elements
  parcels$land_parcel_dims = land_parcels$dims
  parcels$regions = regions$elements
  parcels$region_dims = regions$dims
  parcels$region_num = region_num
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



initialise_index_object <- function(parcels, initial_ecology, simulation_params, offset_indexes_to_exclude, dev_indexes_to_exclude){
  
  index_object = list()
  index_object$banked_offset_pool = vector('list', parcels$region_num)
  index_object$site_indexes = vector('list', 5)
  names(index_object$site_indexes) = c('offsets', 'devs', 'illegals', 'dev_credits', 'banking')
  
  index_object$indexes_to_use = list()
  
  index_object$indexes_to_use$offsets = set_available_indexes(global_indexes = parcels$regions, 
                                                              offset_indexes_to_exclude, 
                                                              parcels, 
                                                              initial_ecology, 
                                                              simulation_params, 
                                                              screen_site_zeros = simulation_params$screen_offset_zeros)
  
  index_object$indexes_to_use$devs = set_available_indexes(global_indexes = parcels$regions, 
                                                           dev_indexes_to_exclude,
                                                           parcels, 
                                                           initial_ecology, 
                                                           simulation_params, 
                                                           screen_site_zeros = simulation_params$screen_dev_zeros)
  return(index_object)
  
}



set_available_indexes <- function(global_indexes, indexes_to_exclude, parcels, initial_ecology, simulation_params, screen_site_zeros){
  
  if (screen_site_zeros == TRUE){
    
    initial_parcel_sums = lapply(seq_along(initial_ecology), 
                                 function(i) lapply(seq_along(initial_ecology[[i]]), 
                                                    function(j) sum(initial_ecology[[i]][[j]]) ) )
    
    zeros_to_exclude = which(unlist(lapply(seq_along(initial_parcel_sums), 
                                           function(i) all(unlist(initial_parcel_sums[[i]][simulation_params$features_to_use_in_offset_calc]) == 0))))
    indexes_to_exclude = unique(c(indexes_to_exclude, zeros_to_exclude))
  }
  
  if (simulation_params$site_screen_size > 0){
    parcel_lengths <- unlist(lapply(seq_along(parcels$land_parcels), function(i) length(parcels$land_parcels[[i]])))
    smalls_to_exclude = which(parcel_lengths < simulation_params$site_screen_size)
    indexes_to_exclude = unique(c(indexes_to_exclude, smalls_to_exclude))
  } 
  
  indexes_to_use = screen_available_sites(global_indexes, indexes_to_exclude, parcels$region_num)
  
  return(indexes_to_use)
}

screen_available_sites <- function(indexes_to_use, indexes_to_exclude, region_num){
  
  for (region_ind in seq_len(region_num)){
    current_region = indexes_to_use[[region_ind]]
    inds_to_remove = which(current_region %in% indexes_to_exclude)
    if (length(inds_to_remove) > 0){
      indexes_to_use[[region_ind]] = indexes_to_use[[region_ind]][-inds_to_remove]
    }
  }
  
  return(indexes_to_use)
}


initialise_decline_rates <- function(parcels, sample_decline_rate, mean_decline_rates, decline_rate_std, feature_num){
  
  land_parcels = parcels$land_parcels
  parcel_num = length(land_parcels)
  decline_rates = vector('list', parcel_num)
  for (parcel_ind in seq(parcel_num)){
    decline_rates[[parcel_ind]] = lapply(seq(feature_num), function(i) rnorm(1, mean_decline_rates[i], decline_rate_std[i]))
  }
  
  return(decline_rates)
  
}

split_ecology_to_land_parcels <- function(landscape_ecology, land_parcels, feature_num){
  parcel_num = length(land_parcels)
  current_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = feature_num)
  
  for (parcel_ind in seq_len(parcel_num)){
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (eco_ind in seq_len(feature_num)){
      current_ecology[[parcel_ind]][[eco_ind]] = landscape_ecology[[eco_ind]][current_parcel]
    }
  }
  return(current_ecology)
  
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
