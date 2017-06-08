initialise_simulation_data <- function(run_params, ecology_params){
  
  if (run_params$data_type == 'simulated'){
    parcels <- initialise_shape_parcels(ecology_params)
    initial_ecology <- initialise_ecology(ecology_params, land_parcels = parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
    dev_weights = list()  
  } else {
    parcels <- initialise_parcels_from_data(run_params$data_folder, run_params$data_type)
    if (run_params$data_type == 'grassland'){
      initial_ecology <- initialise_ecology_from_grassland_data(
        filename =  "~/Desktop/grassland_data/hab.map.master.zo1.pgm", 
        land_parcels = parcels$land_parcels, 
        eco_dims = ecology_params$eco_dims)
    } else if (run_params$data_type == 'hunter'){
      initial_ecology <- initialise_ecology_from_hunter_data(run_params$data_folder, 
                                                             land_parcels = parcels$land_parcels, 
                                                             eco_dims = ecology_params$eco_dims)
    }
  } 
  dev_weights <- initialise_dev_weights(run_params$data_type, run_params$data_folder, land_parcels)
  
  decline_rates_initial <- initialise_decline_rates(parcels, 
                                                    run_params$sample_decline_rate, 
                                                    ecology_params$mean_decline_rates, 
                                                    ecology_params$decline_rate_std, 
                                                    eco_dims = ecology_params$eco_dims)       # set up array of decline rates that are eassociated with each cell
  
  
  simulation_data = list(initial_ecology, decline_rates_initial, parcels, dev_weights)
  names(simulation_data) = c('initial_ecology', 'decline_rates_initial', 'parcels', 'dev_weights')
  
  if (run_params$save_initial_conditions == TRUE){
    saveRDS(paste0(run_params$sim_group_folder, 'simulation_data.rds')) 
  }
  return(simulation_data)
  
}


select_current_policy <- function(policy_params, policy_ind, realisation_num){
  
  current_policy_params = policy_params[[policy_ind]]
  current_policy_params$sim_characteristics <- get_current_sim_characteristics(policy_params[[policy_ind]], realisation_num)
  
  return(current_policy_params)
}

# generate_sim_characteristics <- function(policy_params_group, realisation_num){
#   policy_num = length(policy_params_group)
#   sim_characteristics <- vector('list')
#   for (policy_ind in seq(policy_num)){
#     sim_characteristics[[policy_ind]] <- get_current_sim_characteristics(policy_params_group[[policy_ind]], 
#                                                                        realisation_num)
#   }
#   return(sim_characteristics)
# }

get_current_sim_characteristics <- function(current_policy_params, realisation_num){
  
  sim_characteristics = vector()
  sim_characteristics = paste0(sim_characteristics, current_policy_params$offset_calc_type, '_')
  sim_characteristics = paste0(sim_characteristics, 'offset_bank_', current_policy_params$use_offset_bank, '_')
  if ((current_policy_params$use_offset_time_horizon == TRUE) & (current_policy_params$use_offset_bank == FALSE)){                                   
    sim_characteristics = paste0(sim_characteristics, 'time_horizon_', current_policy_params$offset_time_horizon)
  }
  sim_characteristics = paste0(sim_characteristics, '_include_illegal_clearing_', current_policy_params$include_illegal_clearing_in_offset_calc)
  
  sim_characteristics = paste0(sim_characteristics, '_reals_', realisation_num, '_')
  #   sim_characteristics = paste0(current_policy_params$offset_calc_type, '_', current_policy_params$dev_calc_type, '_', current_policy_params$cfac_type_in_offset_calc,  '_cfac_offset_bank_', 
  #                                current_policy_params$use_offset_bank, '_')
  #   
  #   if (current_policy_params$use_offset_bank == TRUE){                                   
  #     sim_characteristics = paste0(sim_characteristics, current_policy_params$offset_bank_start, '_', current_policy_params$offset_bank_end, '_', 
  #                                  current_policy_params$offset_bank_num, '_', current_policy_params$match_type)
  #   }
  #   
  #   sim_characteristics = paste0(sim_characteristics, '_', current_policy_params$offset_action_type, '_')
  #   if (current_policy_params$offset_action_type == 'restore'){
  #     sim_characteristics = paste0(sim_characteristics, current_policy_params$restoration_rate, '_')
  #   }
  #   
  #   if (current_policy_params$use_offset_time_horizon == TRUE){                                   
  #     sim_characteristics = paste0(sim_characteristics, '_time_horizon_', current_policy_params$offset_time_horizon)
  #   }
  
  
  #  sim_characteristics = paste0(sim_characteristics, '_offsets_potential_developments_', current_policy_params$include_potential_developments_in_offset_calc)
  
  #  sim_characteristics = paste0(sim_characteristics, '_offsets_potential_offsets_', current_policy_params$include_potential_offsets_in_offset_calc)
  
  #  sim_characteristics = paste0(sim_characteristics, '_devs_illegal_clearing_', current_policy_params$include_illegal_clearing_in_dev_calc)
  
  # sim_characteristics = paste0(sim_characteristics, '_devs_potential_developments_', current_policy_params$include_potential_developments_in_dev_calc)
  
  #  sim_characteristics = paste0(sim_characteristics, '_devs_potential_offsets_', current_policy_params$include_potential_offsets_in_dev_calc)
  
  
  return(sim_characteristics)
}


write_folder <- function(output_folder, folder_type){
  current_output_folder = paste0(output_folder, folder_type, '/')
  if (!file.exists(current_output_folder)){
    dir.create(current_output_folder)
  }
  current_output_folder = paste0(current_output_folder, format(Sys.time(), format = "%Y-%j-%H%M%S"), '/')
  if (!file.exists(current_output_folder)){
    dir.create(current_output_folder)
  }
  return(current_output_folder)
}

write_output_folders <- function(run_params, output_folder){
  
  if (!file.exists(output_folder)){
    dir.create(output_folder)
  }
  
  run_params$output_folder = paste0(output_folder, '/', run_params$data_type, '/')
  if (!file.exists(run_params$output_folder)){
    dir.create(run_params$output_folder)
  }
  
  run_params$realisations_folder = write_folder(run_params$output_folder, folder_type = 'realisations')
  run_params$sim_group_folder = write_folder(run_params$output_folder, folder_type = 'sim_group')
  run_params$collated_folder = write_folder(run_params$output_folder, 'collated_realisations')
  
  if (run_params$save_procedure == 'time_slice'){
    run_params$time_slice_folder = write_folder(run_params$realisations_folder, 'time_slices')
  }
  
  return(run_params)
}





initialise_dev_weights <- function(data_type, data_folder, land_parcels){
  if (data_type == 'hunter'){
    dev_weights <- readRDS(paste0(data_folder, 'mining_raster.rds'))
    dev_weights <- stackApply(dev_weights, indices = rep(1, dim(dev_weights)[3]), fun = sum)
    dev_weights_array <- raster_to_array(dev_weights)
    dev_weights = lapply(seq_along(land_parcels), function(i) sum(dev_weights_array[land_parcels[[i]] ])/sum(dev_weights_array))
  } else {
    dev_weights = list()
  }
  return(dev_weights)
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
  
  any(current_policy_params$include_potential_developments_in_offset_calc,
      current_policy_params$include_potential_offsets_in_offset_calc,
      current_policy_params$include_illegal_clearing_in_offset_calc) == TRUE
  
  current_policy_params$adjust_offset_cfacs_flag = any(current_policy_params$include_potential_developments_in_offset_calc,
                                                       current_policy_params$include_potential_offsets_in_offset_calc,
                                                       current_policy_params$include_illegal_clearing_in_offset_calc) == TRUE
  current_policy_params$adjust_dev_cfacs_flag = any(current_policy_params$include_potential_developments_in_dev_calc,
                                                    current_policy_params$include_potential_offsets_in_dev_calc,
                                                    current_policy_params$include_illegal_clearing_in_dev_calc) == TRUE
  
  current_policy_params$use_dev_credit = (current_policy_params$use_parcel_set_dev_credit) || (current_policy_params$offset_bank_type == 'credit')
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
  
  if (current_policy_params$dev_counterfactual_type == 'offset_counterfactual'){
    current_policy_params$include_potential_developments_in_dev_calc = current_policy_params$include_potential_developments_in_offset_calc
    current_policy_params$include_potential_offsets_in_dev_calc = current_policy_params$include_potential_offsets_in_offset_calc
    current_policy_params$include_illegal_clearing_in_dev_calc = current_policy_params$include_illegal_clearing_in_offset_calc
  }
  
  return(current_policy_params)
  
}


generate_policy_params_group <- function(run_params){
  policy_params_to_test <- initialise_policy_params() # list all program combinations to test
  policy_combs <- generate_policy_combs(policy_params_to_test)  #generate all combinations of offset programs
  policy_num = dim(policy_combs)[1] #how many combinations there are in total
  
  policy_params_group = vector('list', policy_num)
  
  for (policy_ind in seq(policy_num)){
    
    current_policy_param_inds = unlist(policy_combs[policy_ind, ])
    current_policy <- generate_current_policy(policy_params_to_test, current_policy_param_inds) #write current policy as a list
    policy_params_group[[policy_ind]] <- collate_current_policy(current_policy, run_params)  #setup flags for cfacs, cfac adjustment etc.
    
  }
  return(policy_params_group)
  
}




initialise_global_object <- function(initial_ecology, decline_rates_initial, parcels, ecology_params, run_params){
  global_object = list()
  global_object$dev_credit = array(0, ecology_params$eco_dims)
  global_object$recorded_dev_credit = vector()
  global_object$offsets_object <- initialise_parcel_set_object() 
  global_object$dev_object <- initialise_parcel_set_object() 
  global_object$illegal_clearing <- initialise_parcel_set_object()
  global_object$index_object <- initialise_index_object(parcels, initial_ecology, run_params)
  global_object$dev_credit_object <- initialise_parcel_set_object() 
  global_object$offset_bank <- initialise_parcel_set_object()
  global_object$decline_rates <- decline_rates_initial
  global_object$offset_pool_object <- list()
  global_object$trajectories <- initialise_trajectories(ecology_params$eco_dims, land_parcels = parcels$land_parcels, run_params$time_steps)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
  global_object$initial_ecology = initial_ecology
  global_object$current_ecology = initial_ecology
  global_object$credit_match_object$match_flag = FALSE
  global_object$match_object$match_flag = FALSE
  return(global_object)
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



initialise_parcels_from_data <- function(data_folder, data_type){
  
  if (data_type == 'grassland'){
    filename = paste0(data_folder, 'planning.units.uid_20ha.pgm')
    img = read.pnm(file = filename, cellres = 1)
    parcel_array = img@grey
  } else if (data_type == 'hunter'){
    parcels_raster <- readRDS(paste0(data_folder, 'parcels_raster.rds'))
    LGA_raster <- readRDS(paste0(data_folder, 'LGA_raster.rds'))
    parcels_mask_raster <- readRDS(paste0(data_folder, 'parcels_mask_raster.rds'))
    parcel_array = as.matrix(parcels_raster*parcels_mask_raster)
    parcel_array[is.na(parcel_array)] = 0
  }
  
  land_index_vals = unique(as.vector(parcel_array))
  landscape_dims = dim(parcel_array)
  land_parcels <- lapply(seq_along(land_index_vals), function(i) which(parcel_array == land_index_vals[i]))
  regions = list()
  regions[[1]] = seq_len(length(land_parcels))
  region_num = length(regions)
  parcels = list()
  parcels$landscape_dims = landscape_dims
  parcels$parcel_indexes = seq_along(land_parcels)
  parcels$land_parcel_num = length(land_parcels)
  parcels$land_parcels = land_parcels
  parcels$regions = regions
  parcels$region_num = region_num
  parcels$parcel_array = parcel_array
  
  return(parcels)
}

initialise_ecology_from_grassland_data <- function(filename, land_parcels, eco_dims){
  img = read.pnm(file = filename, cellres = 1)
  landscape_ecology = list()
  img_to_use = img@grey
  #   zero_inds = which(img_to_use == 0)
  #   img_to_use[zero_inds] = runif(length(zero_inds), 0, 1e-10)
  landscape_ecology[[1]] = 100*img_to_use
  initial_ecology <- split_ecology_to_land_parcels(landscape_ecology, land_parcels, eco_dims)
  return(initial_ecology)
}



raster_to_array <- function(raster_object){
  raster_array = as.matrix(raster_object)
  raster_array[is.na(raster_array)] = 0
  return(raster_array)
}


initialise_ecology_from_hunter_data <- function(data_folder, land_parcels, eco_dims){
  #veg_raster <- readRDS(paste0(data_folder, 'veg_raster.rds'))
  animal_raster <- readRDS(paste0(data_folder, 'animal_raster.rds'))
  #species_raster = stack(veg_raster, animal_raster)
  landscape_ecology = vector('list', eco_dims)
  
  for (eco_ind in seq(eco_dims)){
    current_species_layer = raster_to_array(subset(animal_raster, eco_ind))/992*100
    current_species_layer[current_species_layer == 0] = 1e-10
    current_species_layer[current_species_layer == 100] = 100 - 1e-10
    landscape_ecology[[eco_ind]] = current_species_layer
  }
  
  initial_ecology <- split_ecology_to_land_parcels(landscape_ecology, land_parcels, eco_dims)
  return(initial_ecology)
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


initialise_index_object <- function(parcels, initial_ecology, run_params){
  index_object = list()
  index_object$ind_available = vector('list', parcels$region_num)
  index_object$developments = list()
  index_object$offsets = list()
  index_object$illegal_clearing = list()
  index_object$dev_credit = list()
  index_object$banked_offset_pool = vector('list', parcels$region_num)
  index_object$parcel_num_remaining = vector()
  index_object$break_flag = FALSE
  
  ind_available = parcels$regions
  
  parcel_lengths <- unlist(lapply(seq_along(parcels$land_parcels), function(i) length(parcels$land_parcels[[i]])))
  smalls_to_screen = which(parcel_lengths < run_params$parcel_screen_size)
  initial_parcel_sums = unlist(lapply(seq_along(initial_ecology), function(i) sum(initial_ecology[[i]][[1]])))
  zeros_to_screen = which(initial_parcel_sums < 1e-5)
  
  if (run_params$screen_parcels == TRUE){
    parcel_dist = quantile(initial_parcel_sums[-c(zeros_to_screen, smalls_to_screen)], probs = c(0.05, 0.95))
    parcels_to_screen = c(which(initial_parcel_sums < parcel_dist[1]), which(initial_parcel_sums > parcel_dist[2]))
  } else {
    parcels_to_screen = zeros_to_screen
  }
  
  parcels_to_screen = unique(c(parcels_to_screen, smalls_to_screen))
  
  for (region_ind in seq_len(parcels$region_num)){
    current_region = ind_available[[region_ind]]
    inds_to_remove = which(current_region %in% parcels_to_screen)
    if (length(inds_to_remove) > 0){
      ind_available[[region_ind]] = ind_available[[region_ind]][-inds_to_remove]
    }
  }
  index_object$ind_available = ind_available
  index_object$landscape_inds = ind_available
  
  return(index_object)
  
}




initialise_ecology <- function(ecology_params, land_parcels){    #initialise ecolgy in a slice by slice fashion representing each ecological dimension
  
  parcel_num = length(land_parcels)
  eco_dims = ecology_params$eco_dims
  initial_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = ecology_params$eco_dims)
  
  for (parcel_ind in seq_len(parcel_num)){  
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (eco_ind in seq_len(eco_dims)){
      mean_eco_val = ecology_params$min_initial_eco_val + (ecology_params$max_initial_eco_val - ecology_params$min_initial_eco_val - ecology_params$initial_eco_noise)*runif(1)
      initial_ecology[[parcel_ind]][[eco_ind]] =  array(mean_eco_val, parcel_dims) + ecology_params$initial_eco_noise*array(runif(length(current_parcel)), c(parcel_dims))
    }
    
  }
  
  return(initial_ecology)
}


initialise_decline_rates <- function(parcels, sample_decline_rate, mean_decline_rates, decline_rate_std, eco_dims){
  
  land_parcels = parcels$land_parcels
  parcel_num = length(land_parcels)
  decline_rates = vector('list', parcel_num)
  for (parcel_ind in seq(parcel_num)){
    decline_rates[[parcel_ind]] = lapply(seq(eco_dims), function(i) rnorm(1, mean_decline_rates[i], decline_rate_std[i]))
  }
  
  return(decline_rates)
  
}

split_ecology_to_land_parcels <- function(landscape_ecology, land_parcels, eco_dims){
  parcel_num = length(land_parcels)
  current_ecology = generate_nested_list(outer_dim = parcel_num, inner_dim = eco_dims)
  
  for (parcel_ind in seq_len(parcel_num)){  
    current_parcel = land_parcels[[parcel_ind]]
    parcel_dims = length(current_parcel)
    
    for (eco_ind in seq_len(eco_dims)){
      current_ecology[[parcel_ind]][[eco_ind]] = landscape_ecology[[eco_ind]][current_parcel]
    }
  }
  return(current_ecology)
  
}


split_ecology <- function(current_ecology, land_parcels, parcel_indexes, eco_dims){
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
