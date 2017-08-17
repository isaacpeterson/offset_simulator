run_initialise_routines <- function(run_params, policy_params_group){
  
  run_params <- write_simulation_folders(run_params, length(policy_params_group))
  
  if (run_params$run_from_saved == FALSE){
    if (run_params$simulate_data == TRUE){
        overwrite <- readline("overwrite existing landscape data? (Y/N) ")
        if (overwrite == 'Y'){
          source('simulate_ecology_routines.R')
          prepare_simulated_data(run_params$landscape_data_folder)
        }
    }
  }

  saveRDS(run_params, paste0(run_params$simulation_inputs_folder, 'run_params.rds'))
  
  for (scenario_ind in seq_along(policy_params_group)){
    saveRDS(policy_params_group[[scenario_ind]], 
            paste0(run_params$simulation_inputs_folder, 'scenario_', 
                   formatC( scenario_ind, width = 3, format = "d", flag = "0"), 
                   '_policy_params.rds'))
  }
  
  if (run_params$backup_landscape_data == TRUE){
    save_simulation_inputs(run_params$simulation_inputs_folder, LGA_array, decline_rates_initial, parcels, landscape_ecology,
                           parcel_ecology, dev_weights, ecology_params)
  }
  
  run_params$features_to_use_in_offset_calc = match(run_params$features_to_use_in_offset_calc, run_params$features_to_use_in_simulation)
  return(run_params)
  
}


select_feature_subset <- function(input_object, features_to_use){
  output_object <- lapply(seq_along(input_object), 
                              function(i) (input_object[[i]][features_to_use]))
  return(output_object)
}




bind_params <- function(policy_params, run_params, ecology_params){
  simulation_params = list(policy_params, run_params, ecology_params)
  names(simulation_params) = c('policy_params', 'run_params', 'ecology_params')
  simulation_params$sim_characteristics <- get_current_sim_characteristics(simulation_params$policy_params, 
                                                                           simulation_params$run_params$realisation_num)
  return(simulation_params)
}

write_nested_folder <- function(current_folder){
  while (!(file.exists(current_folder))){
    dir_to_use = current_folder
    while (!(file.exists(dir_to_use))){
      dir_to_use_tmp = dir_to_use
      dir_to_use = dirname(dir_to_use_tmp)
    }
    dir.create(dir_to_use_tmp)
    return(current_folder)
  }
  return(current_folder)
}

write_folder <- function(current_folder){
  if (!file.exists(current_folder)){
    dir.create(current_folder)
  }
  return(current_folder)
}

write_simulation_folders <- function(run_params, scenario_num){
  simulation_folder = write_nested_folder(run_params$simulation_folder)
  landscape_data_folder = write_folder(paste0(run_params$simulation_folder, '/landscape_data/'))
  base_run_folder = paste0(run_params$simulation_folder, '/simulation_runs/')
  
  filenames = list.files(path = base_run_folder, all.files = FALSE, 
             full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
             include.dirs = FALSE, no.. = FALSE)
  
  if (length(filenames) > 0){
    current_run = as.numeric(filenames[length(filenames)]) + 1
  } else {
    current_run = 1
  }
  
  run_params$run_folder = write_nested_folder(paste0(base_run_folder, formatC(current_run, width = 5, format = "d", flag = "0"), '/'))
  run_params$output_folder = write_folder(paste0(run_params$run_folder, '/simulation_outputs/'))
  
  for (scenario_ind in (seq(scenario_num))){
    write_folder(paste0(run_params$output_folder, '/scenario_', formatC(scenario_ind, width = 3, format = "d", flag = "0"), '/'))
  }
  
  run_params$simulation_inputs_folder = write_folder(paste0(run_params$run_folder, '/simulation_inputs/'))
  run_params$landscape_data_folder = landscape_data_folder
  run_params$collated_folder = write_folder(paste0(run_params$run_folder, 'collated_outputs/'))

  return(run_params)
}



generate_policy_combs <- function(policy_params_group){
  
  param_inds <- lapply(seq_along(policy_params_group), function(i) 1:length(policy_params_group[[i]]))
  param_combs <- expand.grid(param_inds)
  
  return(param_combs)
  
}


generate_current_policy <- function(policy_params_group, current_policy_param_inds){
  current_policy <- lapply(seq_along(policy_params_group), function(i) policy_params_group[[i]][current_policy_param_inds[i]])
  names(current_policy) <- names(policy_params_group)
  return(current_policy)
}


collate_current_policy <- function(current_policy_params, run_params){
  
  if (current_policy_params$offset_calc_type == 'avoided_degs'){
    current_policy_params$offset_action_type = 'maintain'
  } else if (current_policy_params$offset_calc_type %in% c('net_gains', 'restoration_gains', 'restoration_condition_value')){
    current_policy_params$offset_action_type = 'restore'
  }
  
  
  
  current_policy_params$allow_developments_from_credit = (current_policy_params$allow_developments_from_credit) || (current_policy_params$offset_bank_type == 'credit')
  
  current_policy_params$use_parcel_sets = (current_policy_params$offset_bank_type == 'parcel_set') || (current_policy_params$use_offset_bank == FALSE)
  if ((current_policy_params$offset_bank_type == 'parcel_set') || (current_policy_params$use_offset_bank == FALSE)){
    current_policy_params$match_type = 'parcel_set'
  }
  if ((current_policy_params$offset_calc_type == 'current_condition') && (current_policy_params$dev_calc_type == 'current_condition')){
    current_policy_params$use_offset_time_horizon = FALSE
  } else {current_policy_params$use_offset_time_horizon = TRUE}
  
  if( (current_policy_params$offset_calc_type == 'avoided_degs') || (current_policy_params$offset_calc_type == 'net_gains') || (current_policy_params$offset_calc_type == 'protected_condition') ){
    current_policy_params$offset_cfacs_flag = TRUE
  } else{
    current_policy_params$offset_cfacs_flag = FALSE
  }
  
  if( (current_policy_params$offset_calc_type == 'restoration_gains') || (current_policy_params$offset_calc_type == 'net_gains') || (current_policy_params$offset_calc_type == 'restoration_condition_value') ){
    current_policy_params$offset_restoration_flag = TRUE
  } else {
    current_policy_params$offset_restoration_flag = FALSE
  }
  
  if( (current_policy_params$dev_calc_type == 'future_condition')){
    current_policy_params$dev_cfacs_flag = TRUE
  } else{
    current_policy_params$dev_cfacs_flag = FALSE
  }
  
  if (current_policy_params$use_offset_bank == TRUE){
    current_policy_params$banked_offset_vec = generate_intervention_vec(time_steps = run_params$time_steps, 
                                                                        prog_start = current_policy_params$offset_bank_start, 
                                                                        prog_end = current_policy_params$offset_bank_end, 
                                                                        total_policy_num = current_policy_params$offset_bank_num, 
                                                                        sd = 1)
  } else {
    current_policy_params$banked_offset_vec = list()
  }
  
  current_policy_params$intervention_vec = generate_intervention_vec(time_steps = run_params$time_steps, 
                                                                     prog_start = current_policy_params$dev_start,
                                                                     prog_end = current_policy_params$dev_end, 
                                                                     total_policy_num = current_policy_params$total_dev_num, 
                                                                     sd = 1)
  
  current_policy_params$adjust_offset_cfacs_flag = any(current_policy_params$include_potential_developments_in_offset_calc,
                                                       current_policy_params$include_potential_offsets_in_offset_calc,
                                                       current_policy_params$include_illegal_clearing_in_offset_calc) == TRUE
  current_policy_params$adjust_dev_cfacs_flag = any(current_policy_params$include_potential_developments_in_dev_calc,
                                                    current_policy_params$include_potential_offsets_in_dev_calc,
                                                    current_policy_params$include_illegal_clearing_in_dev_calc) == TRUE
  
  return(current_policy_params)
  
}


generate_policy_params_group <- function(run_params){
  
  policy_params <- initialise_policy_params() # list all program combinations to test
  policy_combs <- generate_policy_combs(policy_params)  #generate all combinations of offset programs
  policy_num = dim(policy_combs)[1] #how many combinations there are in total
  
  policy_params_group = vector('list', policy_num)
  
  for (policy_ind in seq(policy_num)){
    
    current_policy_param_inds = unlist(policy_combs[policy_ind, ])
    current_policy <- generate_current_policy(policy_params, current_policy_param_inds) #write current policy as a list
    policy_params_group[[policy_ind]] <- collate_current_policy(current_policy, run_params)  #setup flags for cfacs, cfac adjustment etc.
    
  }
  
  return(policy_params_group)
  
}



initialise_output_object <- function(parcels, initial_ecology, run_params, decline_rates_initial, save_realisations){
  output_object = list()
  output_object$offsets_object <- list()
  output_object$dev_object <- list()
  output_object$illegal_clearing_object <- list()
  output_object$credit_object <- list()
  output_object$offset_bank_object <- list()
  output_object$current_ecology = initial_ecology
  output_object$index_object <- initialise_index_object(parcels, initial_ecology, run_params)
  output_object$current_credit = list_of_zeros(run_params$feature_num, 1)
  output_object$decline_rates = decline_rates_initial
#   if (save_realisations == FALSE){
#     output_object$trajectories <- initialise_trajectories(run_params$feature_num, land_parcels = parcels$land_parcels, run_params$time_steps)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
#   }
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
  list_names = c("parcel_indexes", "parcel_num_remaining", "offset_yrs", "parcel_ecologies", "parcel_sums_at_offset", "cfac_trajs", "parcel_vals_used", 
                 "restoration_vals", "cfac_vals", "region_ind")
  return(list_names)
}

generate_intervention_vec <- function(time_steps, prog_start, prog_end, total_policy_num, sd){
  intervention_vec = array(0, time_steps)
  intervention_vec[prog_start:prog_end] = split_vector((prog_end - prog_start + 1), total_policy_num, sd, min_width = -1)
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
#   parcels$parcel_indexes = seq_along(land_parcels)
#   parcels$land_parcel_num = length(land_parcels)
#   parcels$land_parcels = land_parcels
#   parcels$regions = regions
#   parcels$region_num = region_num
#   parcels$parcel_array = parcel_array
#   
#   return(parcels)
# }

# initialise_ecology_from_grassland_data <- function(filename, land_parcels, feature_num){
#   img = read.pnm(file = filename, cellres = 1)
#   landscape_ecology = list()
#   img_to_use = img@grey
#   #   zero_inds = which(img_to_use == 0)
#   #   img_to_use[zero_inds] = runif(length(zero_inds), 0, 1e-10)
#   landscape_ecology[[1]] = 100*img_to_use
#   current_ecology <- split_ecology_to_land_parcels(landscape_ecology, land_parcels, feature_num)
#   return(current_ecology)
# }



raster_to_array <- function(raster_object){
  raster_array = as.matrix(raster_object)
  raster_array[is.na(raster_array)] = 0
  return(raster_array)
}

# 
# initialise_ecology_from_hunter_data <- function(data_folder, land_parcels, feature_num){
#   #veg_raster <- readRDS(paste0(data_folder, 'veg_raster.rds'))
#   animal_raster <- readRDS(paste0(data_folder, 'animal_raster.rds'))
#   #species_raster = stack(veg_raster, animal_raster)
#   landscape_ecology = vector('list', feature_num)
#   
#   for (eco_ind in seq(feature_num)){
#     current_species_layer = raster_to_array(subset(animal_raster, eco_ind))/992*100
#     current_species_layer[current_species_layer == 0] = 1e-10
#     current_species_layer[current_species_layer == 100] = 100 - 1e-10
#     landscape_ecology[[eco_ind]] = current_species_layer
#   }
#   
#   current_ecology <- split_ecology_to_land_parcels(landscape_ecology, land_parcels, feature_num)
#   return(current_ecology)
# }

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
  parcel_indexes = 1:land_parcel_num #index all parcels
  dim(parcel_indexes) = c(parcel_num_y, parcel_num_x) #arrange indicies into array with dimensions of land parcels
  region_vx = split_vector(ecology_params$region_num_x, parcel_num_x, 1, min_width = 3) # perform similar operation used to split array into smallest elements, but this time for land parcels, arranging into regions
  region_vy = split_vector(ecology_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  regions = mcell(parcel_indexes, region_vx, region_vy)   # split land parcel indexes into regions
  
  region_num = length(regions$elements)
  
  parcels$parcel_indexes = parcel_indexes
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


initialise_index_object <- function(parcels, current_ecology, run_params){
  index_object = list()
  index_object$indexes_to_use = vector('list', parcels$region_num)
  index_object$developments = list()
  index_object$offsets = list()
  index_object$illegal_clearing = list()
  index_object$dev_credit = list()
  index_object$banked_offset_pool = vector('list', parcels$region_num)
  index_object$parcel_num_remaining = vector()
  index_object$break_flag = FALSE
  
  indexes_to_use = parcels$regions
  
  parcel_lengths <- unlist(lapply(seq_along(parcels$land_parcels), function(i) length(parcels$land_parcels[[i]])))
  smalls_to_screen = which(parcel_lengths < run_params$parcel_screen_size)
  initial_parcel_sums = unlist(lapply(seq_along(current_ecology), function(i) sum(current_ecology[[i]][[1]])))
  zeros_to_screen = which(initial_parcel_sums < 1e-5)
  
  if (run_params$screen_parcels == TRUE){
    parcel_dist = quantile(initial_parcel_sums[-c(zeros_to_screen, smalls_to_screen)], probs = c(0.05, 0.95))
    parcels_to_screen = c(which(initial_parcel_sums < parcel_dist[1]), which(initial_parcel_sums > parcel_dist[2]))
  } else {
    parcels_to_screen = zeros_to_screen
  }
  
  parcels_to_screen = unique(c(parcels_to_screen, smalls_to_screen))
  
  for (region_ind in seq_len(parcels$region_num)){
    current_region = indexes_to_use[[region_ind]]
    inds_to_remove = which(current_region %in% parcels_to_screen)
    if (length(inds_to_remove) > 0){
      indexes_to_use[[region_ind]] = indexes_to_use[[region_ind]][-inds_to_remove]
    }
  }
  index_object$indexes_to_use = indexes_to_use
  
  return(index_object)
  
}

# 
# 
# 
# initialise_ecology <- function(ecology_params, run_params, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
#   
#   parcel_num = length(land_parcels)
#   feature_num = run_params$feature_num
#   current_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = run_params$feature_num)
#   
#   for (parcel_ind in seq_len(parcel_num)){  
#     current_parcel = land_parcels[[parcel_ind]]
#     parcel_dims = length(current_parcel)
#     
#     for (eco_ind in seq_len(feature_num)){
#       mean_eco_val = ecology_params$min_initial_eco_val + (ecology_params$max_initial_eco_val - ecology_params$min_initial_eco_val - ecology_params$initial_eco_noise)*runif(1)
#       current_ecology[[parcel_ind]][[eco_ind]] =  array(mean_eco_val, parcel_dims) + ecology_params$initial_eco_noise*array(runif(length(current_parcel)), c(parcel_dims))
#     }
#     
#   }
#   
#   return(current_ecology)
# }


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


split_ecology <- function(current_ecology, land_parcels, parcel_indexes, feature_num){
  parcel_indexes = unlist(parcel_indexes)
  parcel_ecologies = vector('list', length(parcel_indexes))
  
  for (parcel_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_ind] 
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    current_parcel_ecology = extract_parcel(current_parcel, current_ecology)
    parcel_ecologies[[parcel_ind]] = current_parcel_ecology
  }
  return(parcel_ecologies)
}
