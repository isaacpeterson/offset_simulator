run_offset_simulation_routines <- function(input_data_object, current_simulation_params, index_object, scenario_ind, realisation_ind){  
  
  # run simulation with identical realisation instantiation
  # list used to govern feature_layers rate changes
  
  current_data_dir = write_folder(paste0(input_data_object$global_params$output_folder, 
                                         'scenario_', formatC(scenario_ind, width = 3, format = "d", flag = "0"), 
                                         '/realisation_', formatC(realisation_ind, width = 3, format = "d", flag = "0"), '/'))
  
  flog.info('current data dir is %s', current_data_dir)
  
  # run the model and return outputs
  output_object = initialise_output_object(current_simulation_params, 
                                           index_object, 
                                           input_data_object$global_params,
                                           input_data_object$site_feature_layers_initial,
                                           input_data_object$parcel_characteristics, 
                                           input_data_object$offset_probability_list, 
                                           input_data_object$dev_probability_list, 
                                           input_data_object$feature_params)
  
  feature_dynamics_initial = output_object$feature_dynamics
  feature_dynamics_modes_initial = output_object$feature_dynamics_modes
  
  output_object <- run_simulation(input_data_object,
                                  output_object,
                                  input_data_object$global_params,
                                  current_simulation_params,
                                  current_data_dir)
  
  # save raw simulation data
  if (input_data_object$global_params$save_simulation_outputs == TRUE){
    saveRDS(output_object, paste0(current_data_dir, 'realisation_',
                                  formatC(realisation_ind, width = 3, format = "d", flag = "0"),
                                  '_outputs.rds'))
  }
  
  for (feature_ind in seq_along(input_data_object$global_params$features_to_use_in_simulation)){
    # select current layer index
    current_feature = input_data_object$global_params$features_to_use_in_simulation[feature_ind]
    # read current feature layer files over time series and arrange into data stack
    current_data_stack = form_data_stack(current_data_dir, 
                                         feature_string = formatC(current_feature, width = 3, format = "d", flag = "0"), 
                                         land_parcels = input_data_object$parcel_characteristics$land_parcels, 
                                         current_simulation_params$time_steps)
    
    # run series of routines used to calculate gains and losses at multiple scales over current feature layer
    
    current_feature_dynamics_initial = select_nested_subset(nested_object = feature_dynamics_initial, feature_ind, output_type = 'nested')
    
    current_initial_feature_layers = select_nested_subset(nested_object = input_data_object$site_feature_layers_initial, feature_ind, output_type = 'nested')
    
    current_feature_dynamics_modes_initial = select_nested_subset(nested_object = feature_dynamics_modes_initial, feature_ind, output_type = 'nested')
    
    current_collated_realisation = run_collate_routines(output_object, 
                                                        current_data_stack,
                                                        current_feature_dynamics_initial,
                                                        current_feature_dynamics_modes_initial,
                                                        current_initial_feature_layers,  
                                                        current_data_dir, 
                                                        current_simulation_params, 
                                                        input_data_object$feature_params,
                                                        realisation_ind,
                                                        feature_ind)
    
    file_prefix = paste0(input_data_object$global_params$collated_folder,
                         'collated_scenario_',  formatC(scenario_ind, width = 3, format = "d", flag = "0"),
                         '_realisation_', formatC(realisation_ind, width = 3, format = "d", flag = "0"))
    
    saveRDS(current_collated_realisation, paste0(file_prefix, '_feature_',
                                                 formatC(current_feature, width = 3, format = "d", flag = "0"), '.rds'))
    
    if ((realisation_ind == 1) && (feature_ind == 1)){
      mov_folder = paste0(input_data_object$global_params$collated_folder, '/mov_', 
                          formatC(scenario_ind, width = 3, format = "d", flag = "0"), '/')
      if( (input_data_object$global_params$write_movie == TRUE) || (input_data_object$global_params$write_offset_layer == TRUE) ){
        if(!(file.exists(mov_folder))){
          dir.create(mov_folder)
        }
      }
      if ( input_data_object$global_params$write_movie == TRUE){
        write_frames(current_data_stack, filetype = 'png', mov_folder, input_data_object$parcel_characteristics, input_data_object$global_params, current_simulation_params, 
                     offset_site_indexes = unlist(output_object$offsets$site_indexes), 
                     offset_yrs = unlist(output_object$offsets$offset_yrs))
      }
    }
  }
  
  # delete current temporary files and folder
  if (input_data_object$global_params$save_simulation_outputs == FALSE){
    unlink(current_data_dir, recursive = TRUE)
  }
  
}

# main engine for code - returns all simulation outputs including developments, offsets etc.
run_simulation <- function(input_data_object,
                           output_object,
                           global_params,
                           current_simulation_params,
                           current_data_dir){
  
  #run through main time loop
  for (yr in seq_len(current_simulation_params$time_steps)){
    flog.info('~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~')
    flog.info('t = %s', yr)
    
    # if running multiple regions with distinct policies cycle through each region
    
    #when running in offset banking select out current set of sites to add to bank
    
    if (current_simulation_params$use_offset_bank == TRUE){
      output_object <- banking_routine(output_object, current_simulation_params, yr)
    }
    
    # determine current set of available offset sites and calculate gains structure as detailed in current policy params
    output_object$offset_pool_object <- prepare_offset_pool(output_object, current_simulation_params, input_data_object, yr)
    
    # cycle through number of developments and associated offsets
    
    for (current_dev_index in seq_len(current_simulation_params$intervention_vec[yr])){
      
      ###### TODO REINSTATE OFFSETBANK ROUTINES ####
      #           if (current_simulation_params$use_offset_bank == TRUE){
      #             output_object$current_credit = assess_banking_credit(output_object, current_simulation_params)
      #           }
      
      # attempt to develop from current available credit
      if (current_simulation_params$allow_developments_from_credit == TRUE){
        output_object <- credit_match_routine(output_object, input_data_object, current_simulation_params, yr)
      } 
      
      #if insufficient credits accumulated to allow development attempt development with offset match.
      if ( output_object$credit_match_flag == FALSE && current_simulation_params$use_parcel_sets == TRUE){
        output_object <- match_sites_routine(output_object, input_data_object, current_simulation_params, yr)
        
      }
    }
    
    output_object <- unregulated_loss_routine(output_object, input_data_object, current_simulation_params, yr)
    
    #TODO reinstate assess_sites
    #output_object <- assess_sites_routine()
    
    save_landscape_routine(input_data_object, output_object, current_data_dir, current_simulation_params, yr)
    
    # update all features of all sites in landscape (both inside and outside development/offset program)
    
    projection_yrs = find_projection_yrs(perform_dynamics_time_shift = input_data_object$feature_params$perform_background_dynamics_time_shift, 
                                         output_object$site_feature_layers, 
                                         yr, 
                                         features_to_project = seq_along(input_data_object$global_params$features_to_use_in_simulation), 
                                         feature_dynamics_to_use = output_object$feature_dynamics, 
                                         projection_type = input_data_object$feature_params$background_projection_type, 
                                         unique_site_vals = input_data_object$feature_params$unique_site_vals)
    
    output_object$site_feature_layers = project_features(output_object$site_feature_layers,
                                                         input_data_object$feature_params$background_projection_type,
                                                         input_data_object$feature_params$background_update_dynamics_by_differential, 
                                                         output_object$feature_dynamics,
                                                         output_object$feature_dynamics_modes,
                                                         current_time_horizons = rep(list(1), length(output_object$site_feature_layers)),
                                                         current_simulation_params,
                                                         features_to_project = seq_along(input_data_object$global_params$features_to_use_in_simulation),
                                                         time_fill = FALSE,
                                                         perform_dynamics_time_shift = input_data_object$feature_params$perform_background_dynamics_time_shift,
                                                         unique_site_vals = input_data_object$feature_params$unique_site_vals,
                                                         projection_yrs)
    
  }
  
  return(output_object)
}

save_landscape_routine <- function(input_data_object, output_object, current_data_dir, current_simulation_params, yr){
  for (feature_ind in seq(input_data_object$global_params$features_to_use_in_simulation)){
    feature_layers_to_save = lapply(seq_along(output_object$site_feature_layers), function(i) output_object$site_feature_layers[[i]][[feature_ind]])
    saveRDS(feature_layers_to_save, paste0(current_data_dir,
                                           'feature_', formatC(input_data_object$global_params$features_to_use_in_simulation[feature_ind], width = 3, format = "d", flag = "0"),
                                           '_yr_', formatC(yr, width = 3, format = "d", flag = "0"), '.rds'))
  }
}

match_sites_routine <- function(output_object, input_data_object, current_simulation_params, yr){
  #perform the matching routine - i.e. find a matching development/offset set 
  match_object <- match_sites(input_data_object, 
                              output_object,
                              match_type = 'develop_using_offset_pool',
                              current_simulation_params,
                              intervention_vec = current_simulation_params$intervention_vec,
                              input_data_object$parcel_characteristics$land_parcels,
                              yr,
                              current_simulation_params$offset_time_horizon)  
  # if a match was found record current development and associated offsets and update site parameters.
  
  if (match_object$match_flag == TRUE){

    flog.info(cat('developed site', paste(match_object$development_object$site_indexes),
                  'with value', paste( lapply(match_object$development_object$parcel_vals_used, round, 1)), '\n',
                  'offset with sites', paste(match_object$offset_object$site_indexes), 
                  'wih value', paste( lapply(match_object$offset_object$parcel_vals_used, round, 1)), '\n'))
    
    #update available credit
    
    output_object$current_credit = match_object$current_credit
    
    # remove selected offset sites from available pool, save offset site characteristics, update decline rates to implement offset.
    output_object <- offset_routine(output_object, 
                                    feature_params = input_data_object$feature_params, 
                                    current_offset_object = match_object$offset_object,
                                    current_simulation_params, 
                                    yr)
    
    # remove selected development sites from available pool, save development site characteristics,
    # update decline rates to implement development loss.
    output_object <- clearing_routine(output_object,
                                      feature_params = input_data_object$feature_params, 
                                      current_dev_object = match_object$development_object,
                                      clearing_type = 'development',
                                      current_simulation_params, 
                                      yr)
    
  }
  return(output_object)
}
credit_match_routine <- function(output_object, input_data_object, current_simulation_params, yr){
  
  flog.info(cat('current credit ', paste(round(unlist(output_object$current_credit), 3)), '\n'))
  if (any(output_object$current_credit > 0)){
    credit_match_object = develop_from_credit(input_data_object, 
                                              output_object,
                                              current_simulation_params,
                                              intervention_vec = current_simulation_params$intervention_vec,
                                              dev_indexes_to_use = output_object$index_object$available_indexes$devs,
                                              input_data_object$parcel_characteristics$land_parcels,
                                              yr,
                                              current_simulation_params$offset_time_horizon)
  } else{
    output_object$credit_match_flag = FALSE
    return(output_object)
  }
  
  output_object$credit_match_flag = credit_match_object$match_flag
  # if development was permitted add current site to current group of developments, set all feature_layers of development site to zero
  if (credit_match_object$match_flag == TRUE){
    
    output_object$current_credit = credit_match_object$current_credit
    
    flog.info(cat('developed site with value', paste(lapply(credit_match_object$development_object$parcel_vals_used, round, 2)), 
                  'from credit, remaining =', paste(lapply(credit_match_object$current_credit, round, 2)), '\n'))
    
    output_object <- clearing_routine(output_object,
                                      feature_params = input_data_object$feature_params, 
                                      current_dev_object = credit_match_object$development_object,
                                      clearing_type = 'develop_from_credit',
                                      current_simulation_params)
    
  }
  return(output_object)
}


assess_sites_routine <- function(output_object){
  #     if ( (length(unlist(output_object$offsets_object$site_indexes)) > 0) & (current_simulation_params$offset_action_type == 'restore')){
  #       
  #       # assess whether offsets have achieved their gains as determined in initial offset gains calculation
  #       assessed_parcel_sets_object <- assess_parcel_sets(output_object$site_feature_layers,
  #                                                         output_object$offsets_object,
  #                                                         output_object$index_object$site_indexes_used$offsets,
  #                                                         current_simulation_params,
  #                                                         decline_rates_initial,
  #                                                         current_simulation_params$offset_time_horizon,
  #                                                         yr)
  #       
  #       # update feature_layers change parameters
  #       output_object$decline_rates <- update_decline_rates(output_object$decline_rates,
  #                                                           restoration_rate = current_simulation_params$restoration_rate,
  #                                                           restoration_rate_std = current_simulation_params$restoration_rate_std,
  #                                                           features_to_use_in_offset_intervention = current_simulation_params$features_to_use_in_offset_intervention,
  #                                                           feature_num = feature_params$feature_num,
  #                                                           decline_rate_type = 'offset',
  #                                                           action_type = 'maintain',
  #                                                           site_indexes = assessed_parcel_sets_object$site_success_inds)
  
  #     output_object$feature_dynamics = update_feature_dynamics(output_object$feature_dynamics, 
  #                                                              output_object$feature_dynamics_modes,
  #                                                              feature_params, 
  #                                                              current_simulation_params$features_to_use_in_offset_intervention, 
  #                                                              sample_dynamics = feature_params$sample_management_dynamics,
  #                                                              action_type = current_simulation_params$offset_action_type, 
  #                                                              current_pool = unlist(current_offset_object$site_indexes))
  #     
  #     output_object$feature_dynamics_modes = update_dynamics_modes(output_object$feature_dynamics_modes, 
  #                                                              feature_params,
  #                                                              current_simulation_params$features_to_use_in_offset_intervention, 
  #                                                              current_simulation_params$offset_action_type, 
  #                                                              current_pool = unlist(current_offset_object$site_indexes))
  
  #       
  #     }
}
# build nested list of feature_layers change parameters of used to determine how the feature_layers evolves in the absence of the
# development and offset intervention. List length is determined by how many sites and list depth is determined by
# how many features are currently in use

#' @export 
simulate_decline_rates <- function(parcel_num, sample_background_dynamics, mean_decline_rates, decline_rate_std){
  
  feature_num = length(mean_decline_rates)
  if (sample_background_dynamics == TRUE){
    # sample change rate from normal distribution
    decline_rates = lapply(seq(parcel_num), function(i) lapply(seq(feature_num),
                                                               function(j) rnorm(1, mean_decline_rates[[j]], decline_rate_std[[j]])))
  } else {
    # copy same rate to all sites
    decline_rates = lapply(seq(parcel_num), function(i) lapply(seq(feature_num),
                                                               function(j) mean_decline_rates[[j]]))
  }
  
  return(decline_rates)
  
}

# for multuiple regions with multiple policies select current policy
select_current_policy <- function(current_simulation_params_set, region_num){
  if (region_num > 1){
    current_simulation_params = current_simulation_params_set
  } else {
    current_simulation_params = current_simulation_params_set
  }
  return(current_simulation_params)
}



# used after simulation has completed. Takes time series layer files for each feature and stacks
form_data_stack <- function(current_data_dir, feature_string, land_parcels, time_steps){
  
  current_filenames <- list.files(path = current_data_dir,
                                  pattern = paste0('feature_', feature_string), all.files = FALSE,
                                  include.dirs = FALSE, no.. = FALSE)
  
  data_stack = lapply(seq_along(land_parcels), function(i) array(0, c(time_steps, length(land_parcels[[i]]))))
  
  #   yr = yr + 1
  #   site_feature_layers = readRDS(paste0(current_data_dir, current_filenames[yr]))
  #   data_stack = lapply(seq_along(data_stack), function(i) stack_current_yr(data_stack[[i]], site_feature_layers[[i]], yr))
  
  for (yr in seq(time_steps)){
    site_feature_layers = readRDS(paste0(current_data_dir, current_filenames[yr]))
    data_stack <- lapply(seq_along(data_stack), function(i) stack_current_yr(data_stack[[i]], site_feature_layers[[i]], yr))
  }
  
  return(data_stack)
}


stack_current_yr <- function(current_parcel, current_site_feature_layers, yr){
  current_parcel[yr, ] = current_site_feature_layers
  return(current_parcel)
}


write_frames <- function(data_stack, filetype, mov_folder, parcel_characteristics, global_params, current_simulation_params, offset_site_indexes, offset_yrs){
  # gray.colors(n = 1024, start = 0, end = 1, gamma = 2.2, alpha = NULL)
  graphics.off()
  rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
  
  if (!dir.exists(mov_folder)){
    dir.create(mov_folder)
  }
  filename = paste0(mov_folder, "tmp%03d.", filetype, sep = '')
  
  if (filetype == 'png'){
    png(filename, height = parcel_characteristics$landscape_dims[1], width = parcel_characteristics$landscape_dims[2])
  } else if (filetype == 'jpg'){
    jpeg(filename, height = parcel_characteristics$landscape_dims[1], width = parcel_characteristics$landscape_dims[2])
  }
  
  for (yr in seq(current_simulation_params$time_steps)){
    site_feature_layers = lapply(seq_along(data_stack), function(i) data_stack[[i]][yr, ])
    landscape = array(0, parcel_characteristics$landscape_dims)
    landscape[unlist(parcel_characteristics$land_parcels)] = unlist(site_feature_layers)
    if (global_params$write_offset_layer == TRUE){
      offset_sites_to_use = which(offset_yrs <= yr)
      landscape[unlist(parcel_characteristics$land_parcels[offset_site_indexes[offset_sites_to_use] ])] = current_simulation_params$max_eco_val
    } 
    image(landscape, zlim = c(0, current_simulation_params$max_eco_val), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
    
    cat('\n image ', yr, ' of ', current_simulation_params$time_steps, 'done\n')
  }
  dev.off()
  
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


# remove site from current offset pool
remove_site_from_pool <- function(offset_pool_object, current_site_indexes){
  subset_pool_to_remove <- list_intersect(offset_pool_object$site_indexes, current_site_indexes)
  if (length(subset_pool_to_remove$match_ind) > 0){
    subset_pool_to_use <- seq_along(offset_pool_object$site_indexes)[-subset_pool_to_remove$match_ind]
    offset_pool_object <- select_pool_subset(offset_pool_object, subset_pool_to_use)
  }
  return(offset_pool_object)
}


#sample over uniform random vector, indicies less than the threshold level are selected for clearing
select_sites_to_clear <- function(available_site_indexes, current_simulation_params){
  
  clearing_thresh <- rep(current_simulation_params$unregulated_loss_prob, length(available_site_indexes))
  discrim <- runif(length(clearing_thresh)) < clearing_thresh
  inds_to_clear <- available_site_indexes[discrim]
  
  return(inds_to_clear)
}



unregulated_loss_routine <- function(output_object, input_data_object, current_simulation_params, yr){
  
  if (current_simulation_params$unregulated_loss_prob == 0){
    # return null object when clearing is inactive
    return(output_object)
  }
  
  available_site_indexes = setdiff(input_data_object$parcel_characteristics$global_indexes, unique(unlist(output_object$index_object$site_indexes_used)))
  
  inds_to_clear <- select_sites_to_clear(available_site_indexes, current_simulation_params)
  
  if (length(inds_to_clear) == 0){ #return null for no sites selected for clearing
    return(output_object)
  } else {
    flog.info('unregulated losses %s' , as.vector(inds_to_clear))
  }
  
  # store group of site characteristics in site characteristics object
  unregulated_loss_object <- record_site_characteristics(output_object$site_feature_layers[inds_to_clear],
                                                         inds_to_clear,
                                                         parcel_num_remaining = length(available_site_indexes),
                                                         yr)
  
  current_pool = unlist(unregulated_loss_object$site_indexes)
  # record characteristics of cleared site
  unregulated_loss_object <- assess_current_pool(pool_object = unregulated_loss_object,
                                                 pool_type = 'developments',
                                                 site_feature_layers_to_use = output_object$site_feature_layers[current_pool],
                                                 feature_dynamics_to_use = output_object$feature_dynamics[current_pool],
                                                 feature_dynamics_modes_to_use = output_object$feature_dynamics_modes[current_pool],
                                                 calc_type = current_simulation_params$dev_calc_type,
                                                 cfacs_flag = current_simulation_params$dev_cfacs_flag,
                                                 adjust_cfacs_flag = current_simulation_params$adjust_dev_cfacs_flag,
                                                 action_type = current_simulation_params$offset_action_type,
                                                 include_potential_developments = current_simulation_params$include_potential_developments_in_dev_calc,
                                                 include_potential_offsets = current_simulation_params$include_potential_offsets_in_dev_calc,
                                                 include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_dev_calc,
                                                 time_horizon_type = 'future',
                                                 current_simulation_params,
                                                 feature_params,
                                                 current_simulation_params$offset_time_horizon,
                                                 yr)  #determine future development parcel attributes
  
  # remove selected sites from available pool, save site characteristics, update decline rates to implement loss.
  if (!is.null(unregulated_loss_object)){
    output_object <- clearing_routine(output_object,
                                      input_data_object$feature_params, 
                                      current_dev_object = unregulated_loss_object,
                                      clearing_type = 'unregulated',
                                      current_simulation_params)
  }
  
  return(output_object)
}


update_dynamics_modes <- function(feature_dynamics_modes, feature_params, features_to_use_in_offset_intervention, action_type, current_pool){
  
  for (current_parcel_ind in current_pool){
    
    if (action_type == 'development'){
      feature_dynamics_modes[[current_parcel_ind]] <- rep(list(0), feature_params$feature_num)
    } else if (action_type == 'maintain'){
      feature_dynamics_modes[[current_parcel_ind]][features_to_use_in_offset_intervention] = rep(list(-1), length(features_to_use_in_offset_intervention))
    } 
  }
  
  return(feature_dynamics_modes)
  
}


update_feature_dynamics <- function(site_feature_layers_to_use, feature_dynamics_to_use, feature_dynamics_modes_to_use, feature_params, features_to_use_in_offset_intervention, 
                                    sample_dynamics, action_type, yr){
  
  if (action_type == 'develop'){
    features_to_use = seq(feature_params$feature_num)
    updated_feature_dynamics = lapply(seq_along(site_feature_layers_to_use), function(i) rep(list(array(0, length(feature_params$simulated_time_vec))), length(features_to_use)))
  } else if (action_type == 'maintain'){
    features_to_use = features_to_use_in_offset_intervention
    updated_feature_dynamics = lapply(seq_along(site_feature_layers_to_use), function(i) rep(list(array(1, length(feature_params$simulated_time_vec))), length(features_to_use)))
  } else if (action_type == 'restore'){
    features_to_use = features_to_use_in_offset_intervention
    
    current_feature_dynamics_set = build_dynamics(site_feature_layers_to_use,
                                                  features_to_use,
                                                  feature_params$sample_management_dynamics,
                                                  feature_params$management_projection_type,
                                                  update_dynamics_by_differential = FALSE,
                                                  dynamics_sample_type = feature_params$management_dynamics_sample_type,
                                                  feature_params$management_dynamics_bounds, 
                                                  feature_dynamics_modes_to_use)
    
    time_shifts = find_time_shifts(site_feature_layers_to_use, 
                                   current_feature_dynamics_set, 
                                   features_to_use, 
                                   projection_type = feature_params$management_projection_type, 
                                   unique_site_vals = feature_params$unique_site_vals)
    
    current_feature_dynamics_set = shift_dynamics_set(site_feature_layers_to_use, 
                                                      current_feature_dynamics_set, 
                                                      features_to_use, 
                                                      projection_type = feature_params$management_projection_type, 
                                                      update_dynamics_by_differential = feature_params$management_update_dynamics_by_differential,
                                                      time_shifts, 
                                                      unique_site_vals = feature_params$unique_site_vals, 
                                                      time_fill = TRUE)
    
    
    updated_feature_dynamics <- merge_dynamics(feature_dynamics_to_use, features_to_use, current_feature_dynamics_set, site_feature_layers_to_use, feature_params$management_projection_type, feature_params$unique_site_vals, yr)
    
  }
  return(updated_feature_dynamics)
  
}


merge_dynamics <- function(feature_dynamics_to_use, features_to_use, current_feature_dynamics, site_feature_layers_to_use, management_projection_type, unique_site_vals, yr){
  updated_feature_dynamics = feature_dynamics_to_use
  
  for (site_ind in seq_along(site_feature_layers_to_use)){
    if (management_projection_type == 'by_element'){
      updated_feature_dynamics[[site_ind]][features_to_use] = lapply(seq_along(current_feature_dynamics[[site_ind]]), 
                                                                     function(i) lapply(seq_along(current_feature_dynamics[[site_ind]][[i]]), 
                                                                                        function(j) append(feature_dynamics_to_use[[site_ind]][[i]][[j]][seq_len(yr - 1)], current_feature_dynamics[[site_ind]][[i]][[j]])))
    }  else if (management_projection_type == 'by_site'){
      if (unique_site_vals == TRUE){
        updated_feature_dynamics[[site_ind]][features_to_use] = lapply(seq_along(current_feature_dynamics[[site_ind]]), 
                                                                       function(i) lapply(seq_along(current_feature_dynamics[[site_ind]][[i]]), 
                                                                                          function(j) append(feature_dynamics_to_use[[site_ind]][[i]][seq_len(yr - 1)], current_feature_dynamics[[site_ind]][[i]][[j]])))
      } else{
        
        updated_feature_dynamics[[site_ind]][features_to_use] = lapply(seq_along(current_feature_dynamics[[site_ind]]), 
                                                                       function(i)  append(feature_dynamics_to_use[[site_ind]][[i]][seq_len(yr - 1)], current_feature_dynamics[[site_ind]][[i]]))
      }
      
    }
    
  }
  return(updated_feature_dynamics)
}


find_time_shifts <- function(site_feature_layers_to_use, current_feature_dynamics_set, features_to_use, projection_type, unique_site_vals){
  
  if (projection_type == 'by_element'){
    time_shifts = lapply(seq_along(site_feature_layers_to_use), 
                         function(i) lapply(features_to_use, 
                                            function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]),
                                                               function(k) find_time_shift(current_feature_val = site_feature_layers_to_use[[i]][[j]][k], 
                                                                                           current_feature_dynamics_set[[i]][[j]][[k]]))))
  } else if (projection_type == 'by_site'){
    
    if (unique_site_vals == TRUE){
      time_shifts = lapply(seq_along(site_feature_layers_to_use), 
                           function(i) lapply(features_to_use, 
                                              function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]),
                                                                 function(k) find_time_shift(current_feature_val = site_feature_layers_to_use[[i]][[j]][k], 
                                                                                             current_feature_dynamics_set[[i]][[j]]))))
    } else{
      
      time_shifts = lapply(seq_along(site_feature_layers_to_use), 
                           function(i) lapply(features_to_use, 
                                              function(j) find_time_shift(current_feature_val = unique(as.vector(site_feature_layers_to_use[[i]][[j]])), 
                                                                          current_feature_dynamics_set[[i]][[j]])))
    }
  }
  
  return(time_shifts)
  
}




shift_dynamics_set <- function(site_feature_layers_to_use, current_feature_dynamics_set, features_to_use, projection_type, update_dynamics_by_differential,
                               time_shifts, unique_site_vals, time_fill){
  
  if (projection_type == 'by_element'){
    #   shifted_dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
    #                             function(i) lapply(features_to_use, 
    #                                                function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]),
    #                                                                   function(k) approx(1:length(current_feature_dynamics_set[[i]][[j]][[k]]), 
    #                                                                                      current_feature_dynamics_set[[i]][[j]][[k]], 
    #                                                                                   time_shifts[[i]][[j]][[k]] + 0:(length(current_feature_dynamics_set[[i]][[j]][[k]]) - ceiling(time_shifts[[i]][[j]][[k]]) ))$y )))
    
    shifted_dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                                  function(i) lapply(features_to_use, 
                                                     function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]),
                                                                        function(k) shift_dynamics(site_feature_layers_to_use[[i]][[j]][[k]],
                                                                                                   current_feature_dynamics_to_use = current_feature_dynamics_set[[i]][[j]][[k]],
                                                                                                   current_time_shift = time_shifts[[i]][[j]][[k]], 
                                                                                                   time_fill, 
                                                                                                   update_dynamics_by_differential) )))
    
  } else if (projection_type == 'by_site'){
    if (unique_site_vals == TRUE){
      #       shifted_dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
      #                                 function(i) lapply(features_to_use, 
      #                                                    function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]),
      #                                                                       function(k) approx(1:length(current_feature_dynamics_set[[i]][[j]]), 
      #                                                                                          current_feature_dynamics_set[[i]][[j]], 
      #                                                                                          time_shifts[[i]][[j]][[k]] + 0:(length(current_feature_dynamics_set[[i]][[j]]) - ceiling(time_shifts[[i]][[j]][[k]]) ) )$y) ))
      
      shifted_dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                                    function(i) lapply(features_to_use, 
                                                       function(j) lapply(seq_along(site_feature_layers_to_use[[i]][[j]]),
                                                                          function(k)   shift_dynamics(site_feature_layers_to_use[[i]][[j]][[k]],
                                                                                                       current_feature_dynamics_to_use = current_feature_dynamics_set[[i]][[j]],
                                                                                                       current_time_shift = time_shifts[[i]][[j]][[k]], 
                                                                                                       time_fill, 
                                                                                                       update_dynamics_by_differential)) ))
    } else {
      #       shifted_dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
      #                                 function(i) lapply(features_to_use, 
      #                                                    function(j) approx(1:length(current_feature_dynamics_set[[i]][[j]]), 
      #                                                                       current_feature_dynamics_set[[i]][[j]], 
      #                                                                       time_shifts[[i]][[j]] + 0:(length(current_feature_dynamics_set[[i]][[j]]) - ceiling(time_shifts[[i]][[j]]) ) )$y))
      
      shifted_dynamics_set = lapply(seq_along(site_feature_layers_to_use), 
                                    function(i) lapply(features_to_use, 
                                                       function(j) shift_dynamics(site_feature_layers_to_use[[i]][[j]][[k]],
                                                                                  current_feature_dynamics_to_use = current_feature_dynamics_set[[i]][[j]],
                                                                                  current_time_shift = time_shifts[[i]][[j]], 
                                                                                  time_fill, 
                                                                                  update_dynamics_by_differential)))
      
    }
  }
  return(shifted_dynamics_set)
}


# shift_dynamics(current_feature_dynamics_to_use = current_feature_dynamics_set[[i]][[j]][[k]],
#                current_time_shift = time_shifts[[i]][[j]][[k]])

shift_dynamics <- function(current_feature_val, current_feature_dynamics_to_use, current_time_shift, time_fill, update_dynamics_by_differential){
  
  if (length(current_time_shift) > 0){
    if (time_fill == TRUE){
      time_vec = current_time_shift + 0:(length(current_feature_dynamics_to_use) - ceiling(current_time_shift))
    } else {
      time_vec = current_time_shift 
    }
    current_shifted_dynamics = approx(1:length(current_feature_dynamics_to_use), 
                                      current_feature_dynamics_to_use, 
                                      time_vec)$y
    if (update_dynamics_by_differential == TRUE){
      current_shifted_dynamics = diff(current_shifted_dynamics)
    }
  } else {
    
    if (update_dynamics_by_differential == TRUE){
      current_shifted_dynamics = diff(current_feature_dynamics_to_use) 
    } else {
      current_shifted_dynamics = current_feature_val + diff(current_feature_dynamics_to_use)
    }
    
  }
  
  return(current_shifted_dynamics)
}



find_time_shift <- function(current_feature_val, current_feature_dynamics_to_use){
  
  min_loc = which(current_feature_dynamics_to_use == min(current_feature_dynamics_to_use))
  intervals = findInterval(current_feature_dynamics_to_use, current_feature_val)
  peak_loc = which( diff(diff(current_feature_dynamics_to_use) >= 0) < 0)
  
  if (length(peak_loc) > 0){
    peak_loc = which(current_feature_dynamics_to_use == max(current_feature_dynamics_to_use[peak_loc]))
  } else {
    peak_loc = max(current_feature_dynamics_to_use)
  } 
  
  if (all(intervals == 1)){
    #condition for which all current feature val is less than current_feature_dynamics curve
    
    if (min_loc > peak_loc){
      time_shift = 1
    } else {
      time_shift = min_loc
    } 
    
  } else if (all(intervals == 0)){
    #condition for which all current feature val is greater than current_feature_dynamics curve
    time_shift = peak_loc
    
  } else {
    #condition for which all current feature val is on current_feature_dynamics curve
    
    lower_bound = min(which(abs(diff(intervals)) == 1))
    if (length(lower_bound) < 1){
      browser()
    }
    
    if (lower_bound > peak_loc){
      time_shift = 1
    } else{
      time_shift = approx(current_feature_dynamics_to_use[lower_bound:(lower_bound + 1)], lower_bound:(lower_bound + 1), current_feature_val)$y
    }
    
  }
  
  
  return(time_shift)
}

# series of routines to implement offset
offset_routine <- function(output_object, feature_params, current_offset_object, current_simulation_params, yr){
  
  # if running in banking mode remove offset site from available bank
  if (current_simulation_params$use_offset_bank == TRUE){
    banked_offset_pool = output_object$offset_bank_object$site_indexes
    banked_offset_inds_used = list_intersect(banked_offset_pool, current_offset_object$site_indexes)
    
    # determine parcels used in matching routine and remove from available pool
    output_object$offset_bank_object$site_indexes = remove_index(banked_offset_pool, banked_offset_inds_used$match_ind)
    
  } else {
    # determine parcels used in matching routine and remove from available pool
    current_pool = unlist(current_offset_object$site_indexes)
    output_object$index_object <- update_index_object(output_object$index_object, update_type = 'offset', current_offset_object$site_indexes)
    
    output_object$feature_dynamics[current_pool] = update_feature_dynamics(output_object$site_feature_layers[current_pool], 
                                                                           output_object$feature_dynamics[current_pool], 
                                                                           output_object$feature_dynamics_modes[current_pool],
                                                                           feature_params, 
                                                                           current_simulation_params$features_to_use_in_offset_intervention, 
                                                                           sample_dynamics = feature_params$sample_management_dynamics,
                                                                           action_type = current_simulation_params$offset_action_type, 
                                                                           yr)
    #note do same with current pool
    output_object$feature_dynamics_modes = update_dynamics_modes(output_object$feature_dynamics_modes, 
                                                                 feature_params,
                                                                 current_simulation_params$features_to_use_in_offset_intervention, 
                                                                 current_simulation_params$offset_action_type, 
                                                                 current_pool = unlist(current_offset_object$site_indexes))
  }
  
  #record current offset site characteristics
  output_object$offsets_object <- append_current_group(output_object$offsets_object, current_offset_object, append_routine = 'standard')
  
  #remove offset sites from available pool
  output_object$offset_pool_object <- remove_parcel_from_current_pool(output_object$offset_pool_object,
                                                                      current_site_indexes = current_offset_object$site_indexes)
  return(output_object)
}

# remove site characteristics from current pool of available sites
remove_parcel_from_current_pool <- function(offset_pool_object, current_site_indexes){
  # work out indexes of current sites in pool
  sites_to_remove <- list_intersect(offset_pool_object$site_indexes, current_site_indexes)
  if (length(sites_to_remove$match_ind) > 0){
    # remove site index from available pool
    subset_pool_to_use <- seq_along(offset_pool_object$site_indexes)[-sites_to_remove$match_ind]
    # redefine pool with current used sites removed
    offset_pool_object <- select_pool_subset(offset_pool_object, subset_pool_to_use)
  }
  return(offset_pool_object)
}



# routines to mark and destroy feature_layers in cleared sites e.g. Development or unregulated

clearing_routine <- function(output_object, feature_params, current_dev_object, clearing_type, current_simulation_params, yr){
  
  if (length(current_dev_object$site_indexes) == 0){
    return(output_object)
  }
  #remove development parcels from available pool
  
  output_object$index_object <- update_index_object(output_object$index_object, update_type = clearing_type, current_dev_object$site_indexes)
  if (clearing_type == 'development'){
    #record current development site characteristics
    
    output_object$dev_object <- append_current_group(output_object$dev_object, current_dev_object, append_routine = 'standard')
    
  } else if (clearing_type == 'develop_from_credit'){
    #record current development site characteristics
    output_object$credit_object <- append_current_group(output_object$credit_object, current_dev_object, append_routine = 'standard')
  } else if (clearing_type == 'unregulated'){
    #record current cleared site characteristics
    output_object$unregulated_loss_object <- append_current_group(output_object$unregulated_loss_object, current_dev_object, append_routine = 'standard')
  }
  
  if (length(current_dev_object$site_indexes) > 0){
    # if any development was allowed remove developed site from available offset pool
    output_object$offset_pool_object <- remove_parcel_from_current_pool(output_object$offset_pool_object,
                                                                        current_site_indexes = current_dev_object$site_indexes)
  }
  
  current_pool = unlist(current_dev_object$site_indexes)
  
  output_object$feature_dynamics[current_pool] = update_feature_dynamics(output_object$site_feature_layers[current_pool],
                                                                         output_object$feature_dynamics[current_pool], 
                                                                         output_object$feature_dynamics_modes[current_pool],
                                                                         feature_params, 
                                                                         current_simulation_params$features_to_use_in_offset_intervention, 
                                                                         sample_dynamics = feature_params$sample_management_dynamics,
                                                                         action_type = 'develop', 
                                                                         yr)
  
  output_object$feature_dynamics_modes = update_dynamics_modes(output_object$feature_dynamics_modes, 
                                                               feature_params,
                                                               current_simulation_params$features_to_use_in_offset_intervention, 
                                                               action_type = 'development', 
                                                               current_pool = unlist(current_dev_object$site_indexes))
  
  return(output_object)
}


# determine current available credit accumulated through offsets for development
assess_banking_credit <- function(output_object, current_simulation_params){
  
  features_to_use_in_offset_calc = current_simulation_params$features_to_use_in_offset_calc
  # determine total offset gains
  
  offset_credit = nested_list_sum(output_object$offset_pool_object$parcel_vals_used)
  
  dev_list = append(output_object$credit_object$parcel_vals_used, output_object$dev_object$parcel_vals_used)
  
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
prepare_offset_pool <- function(output_object, current_simulation_params, input_data_object,  yr){
  
  # if no developments or banked offsets for the current year return null object
  if (current_simulation_params$intervention_vec[yr] ==  0){
    offset_pool_object = list()
    return(offset_pool_object)
  }
  
  # select current set of available offset sites
  current_pool <- output_object$index_object$available_indexes$offsets
  
  # if pool is empty return null object and print error
  if (length(current_pool) == 0){
    flog.error('empty offset pool flag')
    offset_pool_object = list()
    return(offset_pool_object)
  }
  
  if (current_simulation_params$use_offset_bank == TRUE){
    # if running in offset bank mode select sites from current region
    flog.error('offset bank in development')
    stop()
    subset_pool = output_object$offset_bank_object$site_indexes
    
    # find set of offset characteristics that apply to current set of available sites
    offset_pool_object <- select_pool_subset(output_object$offset_bank_object, subset_pool)
    
    # find set of current cumulative site vals, record as projected val as calculation is from time
    # of original offset to current time for banking
    offset_pool_object$projected_vals <- find_current_parcel_sums(output_object$site_feature_layers[unlist(output_object$offset_bank_object$site_indexes[subset_pool])])
    
    offset_pool_type = 'offset_bank'
  } else {
    # when running in standard mode record current offsite site characteristics
    offset_pool_object <- record_site_characteristics(output_object$site_feature_layers[current_pool],
                                                      current_pool,
                                                      parcel_num_remaining = length(current_pool),
                                                      yr)   #arrange available parcel pool into form to use in parcel set determination
    offset_pool_type = 'offsets'
  }
  
  # determine current gains characteristics
  current_pool = unlist(current_pool)
  offset_pool_object <- assess_current_pool(pool_object = offset_pool_object,
                                            pool_type = offset_pool_type,
                                            site_feature_layers_to_use = output_object$site_feature_layers[current_pool],
                                            feature_dynamics_to_use = output_object$feature_dynamics[current_pool],
                                            feature_dynamics_modes_to_use = output_object$feature_dynamics_modes[current_pool],
                                            calc_type = current_simulation_params$offset_calc_type,
                                            cfacs_flag = current_simulation_params$offset_cfacs_flag,
                                            adjust_cfacs_flag = current_simulation_params$adjust_offset_cfacs_flag,
                                            action_type = current_simulation_params$offset_action_type,
                                            include_potential_developments = current_simulation_params$include_potential_developments_in_offset_calc,
                                            include_potential_offsets = current_simulation_params$include_potential_offsets_in_offset_calc,
                                            include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_offset_calc,
                                            time_horizon_type = current_simulation_params$offset_time_horizon_type,
                                            current_simulation_params,
                                            input_data_object$feature_params,
                                            time_horizon = current_simulation_params$offset_time_horizon,
                                            yr)      #determine available parcel values, depending on what particular offset policy is in use using counterfactuals etc.
  return(offset_pool_object)
}


# routines to perform offset banking
banking_routine <- function(output_object, current_simulation_params, yr){
  
  # how many offsets to be added in current year
  offset_bank_num = unlist(current_simulation_params$banked_offset_vec[yr])
  if (offset_bank_num == 0){
    return(output_object)
  }
  
  # select current number of offset sites from current available pool to add to banked offset pool
  current_banked_offset_pool <- sample(output_object$index_object$available_indexes$offsets, offset_bank_num)
  
  # number of sites to potentially offset
  parcel_num_remaining = length(c(unlist(output_object$index_object$available_indexes$offsets),
                                  unlist(output_object$index_object$available_indexes$devs)))
  
  # record current offset pool characteristics
  current_banked_object <- record_site_characteristics(output_object$site_feature_layers[current_banked_offset_pool],
                                                       current_pool = current_banked_offset_pool,
                                                       parcel_num_remaining,
                                                       yr)   # arrange current parcel data
  
  output_object$offset_bank_object = append_current_group(object_to_append = output_object$offset_bank_object,
                                                          current_object = current_banked_object,
                                                          append_routine = 'banked_offset')
  
  # remove current group of sites from available pool
  output_object$index_object <- update_index_object(output_object$index_object,
                                                    update_type = 'banking',
                                                    current_banked_offset_pool)
  
  current_pool = unlist(current_banked_object$site_indexes)
  
  output_object$feature_dynamics[current_pool] = update_feature_dynamics(output_object$site_feature_layers[current_pool], 
                                                                         output_object$feature_dynamics[current_pool], 
                                                                         output_object$feature_dynamics_modes[current_pool],
                                                                         feature_params, 
                                                                         current_simulation_params$features_to_use_in_offset_intervention, 
                                                                         sample_dynamics = feature_params$sample_management_dynamics,
                                                                         action_type = 'banking',
                                                                         yr)
  
  output_object$feature_dynamics_modes = update_dynamics_modes(output_object$feature_dynamics_modes, 
                                                               feature_params,
                                                               current_simulation_params$features_to_use_in_offset_intervention, 
                                                               action_type = current_simulation_params$offset_action_type, 
                                                               current_pool)
  
  return(output_object)
}



assess_parcel_sets <- function(site_feature_layers, offsets_object, offset_parcel_sets, current_simulation_params, decline_rates_initial, time_horizon, yr){
  
  assessed_parcel_sets_object <- list()
  
  parcel_set_count = length((offset_parcel_sets))
  discriminator_metric <- vector('list', parcel_set_count)
  
  for (parcel_set_ind in seq_len(parcel_set_count)){
    
    # select current set of sites from offsets object
    current_parcel_set <- which(unlist(offsets_object$site_indexes) %in% unlist(offset_parcel_sets[[parcel_set_ind]]))
    
    # select characteristics of current set of sites
    current_offset_object <- lapply(seq_along(offsets_object), function(i) offsets_object[[i]][current_parcel_set])
    names(current_offset_object) <- names(offsets_object)
    
    parcel_vals_achieved <- assess_current_gain_pool(site_feature_layers,
                                                     pool_object = current_offset_object,
                                                     calc_type = current_simulation_params$offset_calc_type,
                                                     include_potential_developments = current_simulation_params$include_potential_developments_in_offset_calc,
                                                     include_potential_offsets = current_simulation_params$include_potential_offsets_in_offset_calc,
                                                     include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_offset_calc,
                                                     current_simulation_params,
                                                     feature_dynamics,
                                                     time_horizon,
                                                     yr)
    
    for (feature_ind in seq_len(feature_params$feature_num)){
      discriminator_metric[[parcel_set_ind]] = nested_list_sum(subtract_nested_lists(parcel_vals_achieved, current_offset_object$parcel_vals_used))
    }
  }
  
  # determine whether site has reached gains for subset of features used in offset calculations
  parcel_set_success_inds <- unlist(lapply(seq_along(discriminator_metric),
                                           function(i) (all(unlist(discriminator_metric[[i]][current_simulation_params$features_to_use_in_offset_calc]) > 0))))
  
  site_success_inds = which(unlist(offsets_object$site_indexes) %in% unlist(offset_parcel_sets[parcel_set_success_inds]))
  assessed_parcel_sets_object$site_success_inds <- offsets_object$site_indexes[site_success_inds]
  assessed_parcel_sets_object$discriminator_metric <- discriminator_metric
  
  return(assessed_parcel_sets_object)
}



assess_current_gain_pool <- function(site_feature_layers, pool_object, calc_type, time_fill,
                                     include_potential_developments, include_potential_offsets, include_unregulated_loss,
                                     current_simulation_params, feature_dynamics, time_horizon, yr){
  
  stop()
  print('reinstate routine')
  current_pool = unlist(pool_object$site_indexes)
  parcel_count = length(current_pool)
  offset_yrs = unlist(pool_object$offset_yrs)
  
  time_horizons <- generate_time_horizons(project_type = 'current', yr, offset_yrs, time_horizon, parcel_count)
  
  if (cfacs_flag == TRUE){
    if (adjust_cfacs_flag == FALSE){
      time_fill = FALSE
    } else {
      time_fill = TRUE
    }
    cfacs_object = calc_cfacs(site_feature_layers_to_use = output_object$site_feature_layers[current_pool],
                              parcel_num_remaining = pool_object$parcel_num_remaining,
                              current_simulation_params,
                              feature_params,
                              feature_dynamics_to_use = feature_dynamics[current_pool],
                              feature_dynamics_modes_to_use = feature_dynamics_modes[current_pool],
                              time_horizons,
                              offset_yrs,
                              include_potential_developments,
                              include_potential_offsets,
                              include_unregulated_loss,
                              adjust_cfacs_flag = current_simulation_params$adjust_offset_cfacs_flag,
                              features_to_project = current_simulation_params$features_to_use_in_offset_calc,
                              time_fill,
                              yr)
    
    cfac_vals = nested_list_tail(cfacs_object$cfacs_to_use)
    
  }
  projected_vals = site_feature_layers[current_pool]
  
  projected_vals = lapply(seq_along(projected_vals), function(i) lapply(seq_along(current_simulation_params$features_to_use_in_offset_calc), function(j) sum(projected_vals[[i]][[j]] )))
  
  parcel_vals_achieved = evaluate_parcel_vals(calc_type, pool_object$parcel_sums_at_offset, projected_vals, cfac_vals)
  
  return(parcel_vals_achieved)
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


# define feature_layers dynamics by logistic curve
#' @export 
logistic_projection <- function(parcel_vals, min_eco_val, max_eco_val, current_dec_rate, time_vec){
  
  t_sh = -1/current_dec_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
  
  # define logistic curve given logistic parameter set.
  eco_projected = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-current_dec_rate*(time_vec - t_sh)))
  
  return(eco_projected)
}



user_projection <- function(current_feature_val, current_feature_dynamics, update_dynamics_by_differential, 
                            perform_dynamics_time_shift, time_vec, projection_yr){
  ####### NOTE NEED TO CHECK WHETEHR RETURNS ARE CORRECT TO YR
  if (perform_dynamics_time_shift == TRUE){
    
    current_feature_dynamics <- shift_dynamics(current_feature_val, 
                                               current_feature_dynamics, 
                                               current_time_shift = projection_yr, 
                                               time_fill = TRUE, 
                                               update_dynamics_by_differential)
  } else {
    current_feature_dynamics = current_feature_dynamics[projection_yr:length(current_feature_dynamics)]
  }
  
  if (update_dynamics_by_differential == TRUE){
    projected_feature_vals = current_feature_val + cumsum(current_feature_dynamics)[time_vec]
    
    if (time_vec[1] == 0){
      projected_feature_vals = append(current_feature_val, projected_feature_vals)
    }
    
  } else {
    projected_feature_vals = current_feature_dynamics[time_vec + 1]
  }
  
  return(projected_feature_vals)
}


# calculate the expected feature_layers under maintenaince, restoration

project_feature_layers <- function(current_simulation_params, projection_type, update_dynamics_by_differential, current_site_feature_layer, feature_dynamics_to_use, 
                                   feature_dynamics_mode_to_use, time_horizon, perform_dynamics_time_shift, time_fill, projection_yrs){
  
  # for maintain feature_layers copy current feature_layers to matrix of depth (time_horizon + 1)
  
  if ( all(feature_dynamics_mode_to_use == -1) ){
    
    if (time_fill == TRUE){
      # return all feature_layers states over all time steps
      projected_feature_layers = matrix(rep(current_site_feature_layer, (time_horizon + 1)), ncol = length(current_site_feature_layer), byrow = TRUE)
    } else {
      # project for single time step
      return(current_site_feature_layer)
    }
    # for development feature_layers copy 0 to matrix of depth (time_horizon + 1)
  } else if (all(feature_dynamics_mode_to_use == 0)){
    
    if (time_fill == TRUE){
      # return all feature_layers states over all time steps
      projected_feature_layers = matrix(rep(array(0, length(current_site_feature_layer)), (time_horizon + 1)), ncol = length(current_site_feature_layer), byrow = TRUE)
    } else {
      # project for single time step
      projected_feature_layers = matrix(rep(0, length(current_site_feature_layer)), ncol = length(current_site_feature_layer))
      
    }
  } else {
    
    if (time_fill == TRUE){
      time_vec = 0:time_horizon
    } else {
      time_vec = time_horizon
    }
    
    # update feature_layers according to function defined in project_feature_layers function
    # function parameters are contained in decline_rates array
    
    if (projection_type == 'by_element'){
      if (perform_dynamics_time_shift == TRUE){
        projected_feature_layers = simplify2array(lapply(seq_along(current_site_feature_layer), 
                                                         function(i) user_projection(current_site_feature_layer[i], 
                                                                                     feature_dynamics_to_use[[i]], 
                                                                                     update_dynamics_by_differential, 
                                                                                     perform_dynamics_time_shift, 
                                                                                     time_vec, 
                                                                                     projection_yrs[[i]])))
      } else {
        projected_feature_layers = simplify2array(lapply(seq_along(current_site_feature_layer), 
                                                         function(i) user_projection(current_site_feature_layer[i], 
                                                                                     feature_dynamics_to_use[[i]], 
                                                                                     update_dynamics_by_differential, 
                                                                                     perform_dynamics_time_shift, 
                                                                                     time_vec, 
                                                                                     projection_yrs)))
      }
    } else if (projection_type == 'by_site'){
      #projected_feature_layers = sapply(current_site_feature_layer, user_projection, feature_dynamics_to_use, update_dynamics_by_differential, perform_dynamics_time_shift, time_vec, projection_yr)
      if (perform_dynamics_time_shift == TRUE){
        projected_feature_layers = simplify2array(lapply(seq_along(current_site_feature_layer), 
                                                         function(i) user_projection(current_site_feature_layer[i], 
                                                                                     feature_dynamics_to_use, 
                                                                                     update_dynamics_by_differential, 
                                                                                     perform_dynamics_time_shift, 
                                                                                     time_vec, 
                                                                                     projection_yrs[[i]])))
      } else {
        projected_feature_layers = sapply(current_site_feature_layer, 
                                          user_projection, 
                                          feature_dynamics_to_use, 
                                          update_dynamics_by_differential, 
                                          perform_dynamics_time_shift, 
                                          time_vec, 
                                          projection_yrs)
      }
    }
    
  }
  if (length(dim(projected_feature_layers)) == 0){
    dim(projected_feature_layers) = c(1, length(projected_feature_layers))
  }
  return(projected_feature_layers)
}


# project sites through all features - returns nested list of arrays where each nested array has length
# defined by current_time_horizons and depth defined by feature number for all sites

project_features <- function(current_site_feature_layers, projection_type, update_dynamics_by_differential, feature_dynamics_to_use, feature_dynamics_modes_to_use, 
                             current_time_horizons, current_simulation_params, features_to_project, perform_dynamics_time_shift, time_fill, unique_site_vals, projection_yrs){
  
  if (unique_site_vals == FALSE){
    site_feature_layers_to_use = lapply(seq_along(current_site_feature_layers), 
                                        function(i) lapply(seq_along(current_site_feature_layers[[i]]), 
                                                           function(j) current_site_feature_layers[[i]][[j]][1]))
  } else {
    site_feature_layers_to_use = current_site_feature_layers
  }
  
  
  projected_features = lapply(seq_along(current_site_feature_layers),
                              function(i) lapply(features_to_project,
                                                 function(j) project_feature_layers(current_simulation_params,
                                                                                    projection_type,
                                                                                    update_dynamics_by_differential, 
                                                                                    site_feature_layers_to_use[[i]][[j]],
                                                                                    feature_dynamics_to_use[[i]][[j]],
                                                                                    feature_dynamics_modes_to_use[[i]][[j]],
                                                                                    time_horizon = unlist(current_time_horizons[i]),
                                                                                    perform_dynamics_time_shift,
                                                                                    time_fill, 
                                                                                    projection_yrs[[i]][[j]])))
  if (unique_site_vals == FALSE){
    projected_features = lapply(seq_along(projected_features), 
                                function(i) lapply(seq_along(projected_features[[i]]), 
                                                   function(j) matrix(rep(projected_features[[i]][[j]], length(current_site_feature_layers[[i]][[j]])), ncol = length(current_site_feature_layers[[i]][[j]]))))
  }
  
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


match_sites <- function(input_data_object, output_object, match_type, current_simulation_params, intervention_vec, land_parcels, yr, time_horizon){
  ###### TODO check zero development routines the code below may not be the most efficient method for this ###
  
  match_object = false_match()
  
  if (match_type == 'develop_using_offset_pool'){
    current_match_pool = output_object$index_object$available_indexes$devs
    group_pool_vals = output_object$offset_pool_object$parcel_vals_used
    group_pool_indexes = output_object$offset_pool_object$site_indexes
    parcel_num_remaining = length(current_match_pool)
    
  }
  
  if ((length(group_pool_indexes) == 0) | (parcel_num_remaining == 0)){
    return(match_object)
  } 
  
  zero_inds <- which(unlist(lapply(seq_along(group_pool_vals),  function(i) all(group_pool_vals[[i]] == 0))))
  
  group_pool_vals = remove_index(group_pool_vals, zero_inds)
  group_pool_indexes = remove_index(group_pool_indexes, zero_inds)
  
  # store group of site characteristics in site characteristics object
  dev_pool_object <- record_site_characteristics(output_object$site_feature_layers[current_match_pool],
                                                 current_match_pool,
                                                 parcel_num_remaining,
                                                 yr) 
  current_pool = unlist(dev_pool_object$site_indexes)
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object,
                                         pool_type = 'developments',
                                         site_feature_layers_to_use = output_object$site_feature_layers[current_pool],
                                         feature_dynamics_to_use = output_object$feature_dynamics[current_pool], 
                                         feature_dynamics_modes_to_use = output_object$feature_dynamics_modes[current_pool],
                                         calc_type = current_simulation_params$dev_calc_type,
                                         cfacs_flag = current_simulation_params$dev_cfacs_flag,
                                         adjust_cfacs_flag = current_simulation_params$adjust_dev_cfacs_flag,
                                         action_type = current_simulation_params$offset_action_type,
                                         include_potential_developments = current_simulation_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = current_simulation_params$include_potential_offsets_in_dev_calc,
                                         include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_dev_calc,
                                         time_horizon_type = 'future',
                                         current_simulation_params,
                                         input_data_object$feature_params,
                                         time_horizon,
                                         yr)  #determine future development parcel attributes
  
  
  current_match_vals_pool = dev_pool_object$parcel_vals_used
  
  if (sum(unlist(current_match_vals_pool))== 0){
    flog.info('all projected developments are zero - blocking all developments')
    match_object = false_match()
    match_object$offset_object = list()
    match_object$current_credit = output_object$current_credit
    return(match_object)
  }
  
  #any potential parcel set match requires a minimum of two sites
  
  while( (match_object$match_flag == FALSE) & (length(current_match_pool) > 1) ){   
    
    if (current_simulation_params$development_selection_type == 'random'){
      sample_ind = sample(seq_along(current_match_pool), size = 1)
    } else if (current_simulation_params$development_selection_type == 'weighted'){
      current_dev_probability_list = recalculate_probabilities(input_data_object$dev_probability_list[unlist(current_match_pool)])
      sample_ind = sample(seq_along(current_match_pool), size = 1, prob = current_dev_probability_list)
    }
    
    current_test_index = current_match_pool[sample_ind]
    vals_to_match = current_match_vals_pool[[sample_ind]]
    
    if (current_simulation_params$use_offset_bank == FALSE){
      dev_ind = list_intersect(group_pool_indexes, current_test_index) #find and remove index that corresponds to potiential development index
      match_pool_to_use = remove_index(group_pool_indexes, dev_ind$match_ind)
      pool_vals_to_use = remove_index(group_pool_vals, dev_ind$match_ind)
    } else {
      match_pool_to_use = group_pool_indexes  #if performing offset banking use any of the available banked offset pool
      pool_vals_to_use = group_pool_vals
    }
    
    match_object <- match_from_pool(match_type = 'offset',
                                    current_pool = match_pool_to_use,
                                    pool_vals_to_use,
                                    output_object$current_credit,
                                    input_data_object$offset_probability_list,
                                    vals_to_match_initial = vals_to_match,
                                    current_simulation_params,
                                    yr) #perform matching routine
    
    if (match_object$match_flag == FALSE){
      
      # only try to match sites with smaller ecological value than current site
      inds_to_keep = which(lapply(seq_along(dev_pool_object$parcel_vals_used),
                                  function(i) all(unlist(subtract_nested_lists(dev_pool_object$parcel_vals_used[[i]], vals_to_match)) < 0) ) == TRUE)
      
      current_match_pool = dev_pool_object$site_indexes[inds_to_keep]     
      current_match_vals_pool = dev_pool_object$parcel_vals_used[inds_to_keep]
    }
  }
  
  if (match_object$match_flag == TRUE){
    dev_match_index = which(unlist(dev_pool_object$site_indexes) == current_test_index)
    match_object$development_object = select_pool_subset(dev_pool_object, unlist(dev_match_index))
    subset_pool =  list_intersect(output_object$offset_pool_object$site_indexes, match_object$match_indexes)
    offset_object <- select_pool_subset(pool_object = output_object$offset_pool_object, subset_pool = subset_pool$match_ind)
    match_object$offset_object = offset_object
  } else if (match_object$match_flag == FALSE){
    match_object$offset_object = list()
    match_object$current_credit = output_object$current_credit
  }
  
  return(match_object)
  
}


develop_from_credit <- function(input_data_object, output_object, current_simulation_params, 
                                intervention_vec, dev_indexes_to_use, land_parcels, yr, time_horizon){
  
  parcel_num_remaining = length(dev_indexes_to_use)
  
  # store group of site characteristics in site characteristics object
  dev_pool_object <- record_site_characteristics(output_object$site_feature_layers[dev_indexes_to_use],  
                                                 dev_indexes_to_use,  
                                                 parcel_num_remaining,  
                                                 yr)
  current_pool = unlist(dev_pool_object$site_indexes)
  
  dev_pool_object <- assess_current_pool(pool_object = dev_pool_object,
                                         pool_type = 'developments',
                                         site_feature_layers_to_use = output_object$site_feature_layers[current_pool],
                                         feature_dynamics_to_use = output_object$feature_dynamics[current_pool],  
                                         feature_dynamics_modes_to_use = output_object$feature_dynamics_modes[current_pool],
                                         calc_type = current_simulation_params$dev_calc_type,
                                         cfacs_flag = current_simulation_params$dev_cfacs_flag,
                                         adjust_cfacs_flag = current_simulation_params$adjust_dev_cfacs_flag,
                                         action_type = current_simulation_params$offset_action_type,
                                         include_potential_developments = current_simulation_params$include_potential_developments_in_dev_calc,
                                         include_potential_offsets = current_simulation_params$include_potential_offsets_in_dev_calc,
                                         include_unregulated_loss = current_simulation_params$include_unregulated_loss_in_dev_calc,
                                         time_horizon_type = 'future',
                                         current_simulation_params,
                                         input_data_object$feature_params,
                                         time_horizon,
                                         yr)
  
  if (any(unlist(dev_pool_object$parcel_vals_used) < 0)){
    
  }
  #   subset_pool =  list_intersect(dev_pool_object$site_indexes, match_object$match_indexes)
  #   match_object$development_object = select_pool_subset(dev_pool_object, subset_pool = subset_pool$match_ind)
  
  if (length(unlist(dev_pool_object$site_indexes)) > 0){
    
    pool_vals_to_use = dev_pool_object$parcel_vals_used
    
    match_object <- match_from_pool(match_type = 'development', 
                                    current_pool = dev_pool_object$site_indexes, 
                                    pool_vals_to_use, 
                                    output_object$current_credit,
                                    input_data_object$dev_probability_list,
                                    vals_to_match_initial = output_object$current_credit,
                                    current_simulation_params,
                                    yr)
    
  } else{
    match_object = false_match()
  }
  
  if (match_object$match_flag == TRUE){
    subset_pool =  list_intersect(dev_pool_object$site_indexes, match_object$match_indexes)
    match_object$development_object = select_pool_subset(dev_pool_object, subset_pool = subset_pool$match_ind)
  } else{
    #match_object$development_object = list()
    #match_object$current_credit = output_object$current_credit
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
record_site_characteristics <- function(site_feature_layers, current_pool, parcel_num_remaining, yr){
  
  parcel_set_object = list()
  parcel_set_object$offset_yrs = rep(list(yr), length(current_pool))
  parcel_set_object$parcel_sums_at_offset = sum_sites(site_feature_layers)
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

find_intervention_probability <- function(intervention_vec, offset_yrs, calc_type, offset_intervention_scale, 
                                          time_horizons, parcel_num, parcel_num_remaining, time_steps){
  
  intervention_probs = vector('list', parcel_num)
  parcel_num_remaining = unlist(parcel_num_remaining)
  offset_yrs = unlist(offset_yrs)
  
  for (parcel_ind in seq_len(parcel_num)){
    
    time_horizon = time_horizons[parcel_ind]
    offset_yr = offset_yrs[parcel_ind]
    current_prob = array(0, (time_horizon + 1))
    
    current_vec = intervention_vec[offset_yr:time_steps]
    
    if (length(current_vec) < (time_horizon + 1)){
      current_vec = c(current_vec, array(0, ((time_horizon + 1) - length(current_vec))))
    }
    
    current_vec = current_vec[1:(time_horizon + 1)]
    current_parcel_num_remaining = parcel_num_remaining[parcel_ind]
    if (current_parcel_num_remaining > 0) {
      current_prob = current_prob + current_vec/current_parcel_num_remaining
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
    flog.error(cat('duplicate site value match flag on ', length(match_ind), 'sites \n'))
    # sample site from multiple match pool
    match_ind = sample(match_ind, 1)
  }
  
  match_vals = parcel_vals_pool[match_ind]
  
  match_object = list()
  match_object$match_vals = match_vals
  match_object$match_ind = match_ind
  return(match_object)
}


#return false match object
false_match <- function(){
  match_object = list()
  match_object$match_flag = FALSE
  return(match_object)
}


select_cols <- function(arr_to_use, col_inds){
  arr_to_use <- arr_to_use[, col_inds]
  arr_to_use <- t(t(arr_to_use))
  return(arr_to_use)
}


select_pool_to_match <- function(vals_to_match, current_features_to_use_in_offset_calc, use_specified_offset_metric, metric_type, thresh, pool_vals_to_use, 
                                 max_parcel_num, current_pool, match_type, screen_site_zeros){
  
  pool_object = list()
  
  if (length(unlist(pool_vals_to_use)) == 0){
    pool_object$break_flag = TRUE
    return(pool_object)
  } 
  
  pool_vals_to_use <- lapply(seq_along(pool_vals_to_use), function(i) pool_vals_to_use[[i]][current_features_to_use_in_offset_calc])
  
  if (use_specified_offset_metric == TRUE){
    pool_vals_to_use = transform_features_to_offset_metric(pool_vals_to_use, metric_type)
  }
  
  zero_inds <- which(unlist(lapply(seq_along(pool_vals_to_use), function(i) sum(pool_vals_to_use[[i]]) == 0)))
  
  if (screen_site_zeros == TRUE){ 
    
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
  
  
  ##### TODO check robustness #######
  
  if (max_parcel_num > 1){
    pool_vals_to_test = Reduce('+', pool_vals_to_use)
  } else {
    pool_vals_to_test = pool_vals_to_use
  }
  
  if (match_type == 'offset'){
    pool_condition = unlist(lapply(seq_along(pool_vals_to_test), function(i) all( (vals_to_match - pool_vals_to_test[[i]]) <= thresh)))
  } else if (match_type == 'development'){
    pool_condition = unlist(lapply(seq_along(pool_vals_to_test), function(i) all( (vals_to_match - pool_vals_to_test[[i]]) >= -thresh)))
  }
  
  if (length(pool_condition) == 0){
    browser()
  }
  if (all(pool_condition == FALSE)){
    pool_object$break_flag = TRUE
    return(pool_object)
  } 
  
  if (max_parcel_num == 1){
    pool_vals_to_use <- pool_vals_to_use[pool_condition]
    current_pool <- current_pool[pool_condition]
  }
  
  pool_object$break_flag = FALSE
  pool_object$pool_vals_to_use = pool_vals_to_use
  pool_object$current_pool = current_pool
  pool_object$zero_inds = zero_inds
  
  return(pool_object)
}

transform_features_to_offset_metric <- function(pool_vals, metric_type){
  
  if (metric_type == 'euclidian_norm'){
    if (class(pool_vals) == 'list'){
      pool_vals = lapply(seq_along(pool_vals), function(i) sqrt(sum(pool_vals[[i]]^2)))
    }  else {
      pool_vals = sqrt(sum(pool_vals^2))
    }
  }
  
  return(pool_vals)
  
}


match_from_pool <- function(match_type, current_pool, pool_vals_to_use, current_credit, current_probability_list, 
                            vals_to_match_initial, current_simulation_params, yr){
  
  if (length(unlist(current_pool)) == 0){
    match_object = false_match()
    return(match_object)
  } 
  
  current_features_to_use_in_offset_calc = current_simulation_params$features_to_use_in_offset_calc
  
  if (match_type == 'offset'){
    max_parcel_num = current_simulation_params$max_offset_parcel_num
    match_procedure = current_simulation_params$offset_selection_type
    screen_site_zeros = current_simulation_params$screen_offset_zeros
    
    if (current_simulation_params$use_specified_offset_metric == FALSE){
      
      current_features_to_use_in_offset_calc = which(vals_to_match_initial > 0)
      if ( length(current_features_to_use_in_offset_calc) == 0 ) {
        match_object = false_match()
        return(match_object)
      }
    } else {
      current_features_to_use_in_offset_calc = current_simulation_params$features_to_use_in_offset_calc
    } 
    
    offset_multiplier = current_simulation_params$offset_multiplier
    
    vals_to_match = offset_multiplier * vals_to_match_initial[current_features_to_use_in_offset_calc]
    
    if (current_simulation_params$use_specified_offset_metric == TRUE){
      vals_to_match = transform_features_to_offset_metric(vals_to_match, metric_type = current_simulation_params$offset_metric_type)
    } 
    
    if ( (current_simulation_params$allow_developments_from_credit == TRUE)){
      
      if (current_simulation_params$use_specified_offset_metric == FALSE){
        vals_to_match = vals_to_match - current_credit[current_features_to_use_in_offset_calc]
      } else {
        vals_to_match = vals_to_match - current_credit
      }
      
    }
    
  } else if (match_type == 'development'){
    screen_site_zeros = current_simulation_params$screen_dev_zeros
    # force only single development for development routine
    max_parcel_num = 1
    if (screen_site_zeros == FALSE){
      # if zeros are allowed use random selection 
      match_procedure = 'random'
    } else {
      match_procedure = current_simulation_params$development_selection_type
    }
    
    # when developing from credit, inverse offset multiplier
    vals_to_match = 1/current_simulation_params$offset_multiplier*vals_to_match_initial
  }
  
  #create an array of threshold values defined by user based proportion 
  thresh = array(current_simulation_params$match_threshold_ratio * vals_to_match)         
  
  
  pool_object <- select_pool_to_match(vals_to_match, current_features_to_use_in_offset_calc, current_simulation_params$use_specified_offset_metric, 
                                      current_simulation_params$offset_metric_type, thresh, 
                                      pool_vals_to_use, max_parcel_num, current_pool, match_type, screen_site_zeros)
  
  if (pool_object$break_flag == TRUE){
    match_object = false_match()
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
    } else if (match_procedure == 'random'){
      match_params = list()
      match_params$match_ind = sample(length(current_pool), 1)
      match_params$match_vals = parcel_vals_pool[match_params$match_ind]
    } else if (match_procedure == 'weighted'){
      match_params = list()
      probability_list_to_use = recalculate_probabilities(current_probability_list[unlist(current_pool)])
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
  
  if (current_simulation_params$use_specified_offset_metric == FALSE){
    current_credit[current_features_to_use_in_offset_calc] = current_credit_to_update
  } else {
    current_credit = current_credit_to_update
  }
  
  match_object = list()
  match_object$match_indexes = match_indexes
  match_object$match_vals = match_vals
  match_object$match_flag = match_flag
  match_object$current_credit = current_credit
  
  return(match_object)
  
}



# determine cumulative value of all sites within parcel feature_layers for multiple features

sum_sites <- function(site_feature_layers){
  parcel_sums = lapply(seq_along(site_feature_layers), 
                       function(i) unlist(lapply(seq_along(site_feature_layers[[i]]),
                                                 function(j) sum(site_feature_layers[[i]][[j]]) )))
  return(parcel_sums)
}


# determine cumulative sum of all sites within parcel feature_layers for 1D features
sum_feature_layers <- function(site_feature_layers){
  parcel_sums <- lapply(seq_along(site_feature_layers), function(i) sum(site_feature_layers[[i]] ))
}


#remove site from available pool for offsets and developments. This is a two stage process to cover when offsets and developments may not overlap
update_index_object <- function(index_object, update_type, site_indexes){
  #remove parcel from available list
  index_object$available_indexes$offsets = setdiff(index_object$available_indexes$offsets, site_indexes)
  index_object$available_indexes$devs = setdiff(index_object$available_indexes$devs, site_indexes)
  
  if (update_type == 'offset'){
    index_object$site_indexes_used$offsets = append(index_object$site_indexes_used$offsets, list(site_indexes))
  } else if (update_type == 'development'){
    index_object$site_indexes_used$devs = append(index_object$site_indexes_used$devs, list(site_indexes))
  } else if (update_type == 'unregulated'){
    index_object$site_indexes_used$unregulated = append(index_object$site_indexes_used$unregulated, list(site_indexes))
  } else if (update_type == 'develop_from_credit'){
    index_object$site_indexes_used$dev_credit = append(index_object$site_indexes_used$dev_credits, list(site_indexes))
  } else if (update_type == 'banking'){
    index_object$site_indexes_used$banking = append(index_object$site_indexes_used$banking, list(site_indexes))
  }
  
  return(index_object)
}


# update feature_layers change parameters
# routine to label offset and development sites - updates decline_rates with 0 entry for developments,
# and 1/restoration_parameter for maintain/restoration offset. Protection offset leaves decline_rates unaffected

update_decline_rates <- function(decline_rates, restoration_rate, restoration_rate_std, sample_management_dynamics, 
                                 features_to_use_in_offset_intervention, feature_num, decline_rate_type, action_type, site_indexes){
  
  for (current_parcel_ind in unlist(site_indexes)){
    
    if (decline_rate_type == 'development'){
      decline_rates[[current_parcel_ind]] <- rep(list(0), feature_num)
    } else if (decline_rate_type == 'offset'){
      
      if (action_type == 'maintain'){
        decline_rates[[current_parcel_ind]][features_to_use_in_offset_intervention] = rep(list(1), length(features_to_use_in_offset_intervention))
      } else if (action_type == 'restore'){
        if (sample_management_dynamics == TRUE){
          # spread restoration rates about mean
          decline_rates[[current_parcel_ind]][features_to_use_in_offset_intervention] = as.list(rnorm(length(features_to_use_in_offset_intervention), mean = restoration_rate, sd = restoration_rate_std))
        } else{
          # use identical restoration rates for each site
          decline_rates[[current_parcel_ind]][features_to_use_in_offset_intervention] = rep(list(restoration_rate), length(features_to_use_in_offset_intervention))
        }
        
      }
    }
  }
  
  return(decline_rates)
  
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
select_pool_subset <- function(pool_object, subset_pool){
  object_subset = lapply(seq_along(pool_object), function(i) pool_object[[i]][subset_pool])
  names(object_subset) <- names(pool_object)
  return(object_subset)
}


#function to work out vector of time intervals used in gains calculations
generate_time_horizons <- function(project_type, yr, offset_yrs, time_horizon, parcel_count){
  
  if (project_type == 'current'){
    # work out time intervals from time of offset to current year
    time_horizons = rep(yr, parcel_count) - offset_yrs
  } else if (project_type == 'future'){
    # work out time intervals from current year to projected year defined by time_horizon
    time_horizons = rep(time_horizon, parcel_count)
  } else if (project_type == 'zeros'){
    #set up dummy times
    time_horizons = rep(0, parcel_count)
  }
  return(time_horizons)
}


assess_current_pool <- function(pool_object, pool_type, site_feature_layers_to_use, feature_dynamics_to_use, feature_dynamics_modes_to_use, calc_type, cfacs_flag, adjust_cfacs_flag, action_type, 
                                include_potential_developments, include_potential_offsets, include_unregulated_loss,
                                time_horizon_type, current_simulation_params, feature_params, time_horizon, yr){
  
  current_condition_vals = lapply(seq_along(pool_object$parcel_sums_at_offset),
                                  function(i) pool_object$parcel_sums_at_offset[[i]][current_simulation_params$features_to_use_in_offset_calc])
  
  if (calc_type == 'current_condition') {
    projected_vals = current_condition_vals
    
    cfac_vals = list_of_zeros(length(pool_object$parcel_sums_at_offset), length(current_simulation_params$features_to_use_in_offset_calc))
    
  } else {
    
    time_horizons <- generate_time_horizons(project_type = 'future', yr, unlist(pool_object$offset_yrs), time_horizon, length(site_feature_layers_to_use))
    
    if (cfacs_flag == TRUE){
      if (adjust_cfacs_flag == FALSE){
        time_fill = FALSE
      } else {
        time_fill = TRUE
      }
      cfacs_object = calc_cfacs(site_feature_layers_to_use,
                                parcel_num_remaining = pool_object$parcel_num_remaining,
                                current_simulation_params,
                                feature_params,
                                feature_dynamics_to_use,
                                feature_dynamics_modes_to_use,
                                time_horizons,
                                unlist(pool_object$offset_yrs),
                                include_potential_developments,
                                include_potential_offsets,
                                include_unregulated_loss,
                                adjust_cfacs_flag,
                                features_to_project = current_simulation_params$features_to_use_in_offset_calc, 
                                time_fill = TRUE,
                                yr)
      
      cfac_vals = nested_list_tail(cfacs_object$cfacs_to_use)
    } else if (calc_type == 'restoration_gains'){
      
      cfac_vals = current_condition_vals
    } else {
      cfac_vals = list_of_zeros(length(pool_object$parcel_sums_at_offset), length(current_simulation_params$features_to_use_in_offset_calc))
    }
    
    if (pool_type == 'offsets') {
      
      if (action_type == 'maintain'){
        feature_dynamics_modes_to_use = lapply(seq_along(site_feature_layers_to_use), function(i) rep(list(-1), feature_params$feature_num))
        
      } else if (action_type == 'restore'){
        #TODO add ability to assess current feature_dynamics_mode
        
        #         feature_dynamics_modes = update_modes(projection_type = feature_params$management_projection_type,
        #                                               current_modes = feature_dynamics_modes,
        #                                               feature_layers_to_use = site_feature_layers_to_use, 
        #                                               condition_class_bounds = feature_params$condition_class_bounds)
        
        
        feature_dynamics_to_use = build_dynamics(site_feature_layers = site_feature_layers_to_use,
                                                 features_to_use = current_simulation_params$features_to_use_in_offset_intervention,
                                                 sample_dynamics = FALSE, 
                                                 projection_type = feature_params$management_projection_type, 
                                                 update_dynamics_by_differential = FALSE,
                                                 dynamics_sample_type = feature_params$management_dynamics_sample_type,
                                                 feature_dynamics_bounds = feature_params$management_dynamics_bounds, 
                                                 feature_dynamics_modes_to_use)
        
        projection_yrs = find_projection_yrs(perform_dynamics_time_shift = feature_params$perform_management_dynamics_time_shift, 
                                             site_feature_layers_to_use, yr, 
                                             features_to_project = current_simulation_params$features_to_use_in_offset_calc, 
                                             feature_dynamics_to_use, 
                                             projection_type = feature_params$management_projection_type, 
                                             unique_site_vals = feature_params$unique_site_vals)
        
      } 
      
      projected_feature_layers = project_features(site_feature_layers_to_use,
                                                  projection_type = feature_params$management_projection_type,
                                                  feature_params$management_update_dynamics_by_differential, 
                                                  feature_dynamics_to_use,
                                                  feature_dynamics_modes_to_use,
                                                  time_horizons,
                                                  current_simulation_params,
                                                  features_to_project = current_simulation_params$features_to_use_in_offset_calc,
                                                  perform_dynamics_time_shift = feature_params$perform_management_dynamics_time_shift,
                                                  time_fill = FALSE,
                                                  unique_site_vals = feature_params$unique_site_vals,
                                                  projection_yrs)
      
      projected_vals = sum_sites(projected_feature_layers)
      
#       projected_offsets = project_features(site_feature_layers_to_use,
#                                            projection_type = feature_params$management_projection_type,
#                                            feature_params$management_update_dynamics_by_differential, 
#                                            feature_dynamics_to_use,
#                                            feature_dynamics_modes_to_use,
#                                            time_horizons,
#                                            current_simulation_params,
#                                            features_to_project = current_simulation_params$features_to_use_in_offset_calc,
#                                            perform_dynamics_time_shift = feature_params$perform_management_dynamics_time_shift,
#                                            time_fill = TRUE,
#                                            unique_site_vals = feature_params$unique_site_vals,
#                                            projection_yrs)
#       
    } else if (pool_type == 'developments') {
      projected_vals = cfac_vals
      cfac_vals = list_of_zeros(length(pool_object$parcel_sums_at_offset), length(current_simulation_params$features_to_use_in_offset_calc))
    }
  }
  
  
  pool_object$parcel_vals_used = mapply('-', projected_vals, cfac_vals, SIMPLIFY = FALSE)
  
  # NOTE TODO REMOVE THIS ROUTINE AND SWITCH ABOVE time_fill = TRUE in calc_cfacs to time_fill
  #pool_object$pool_cfacs = sum_trajectories(cfacs_object$cfacs)
  
#   if (any(unlist(pool_object$parcel_vals_used) < 0)){
#     browser()
#   }
  #   
  #   
  #   if (any(unlist(pool_object$parcel_vals_used) < 0)){
  #     #browser()
  #   }
  return(pool_object)
  
}




update_modes <- function(projection_type, current_modes, feature_layers_to_use, condition_class_bounds){
  
  if (projection_type == 'by_site' ){
    feature_dynamics_modes = lapply(seq_along(feature_layers_to_use), 
                                    function(i) lapply(seq_along(feature_layers_to_use[[i]]), 
                                                       function(j) switch_current_mode(unique(as.vector(feature_layers_to_use[[i]][[j]])), 
                                                                                       condition_class_bounds[[j]], 
                                                                                       current_modes[[i]][[j]]) ))
  } else if (projection_type == 'by_element'){
    feature_dynamics_modes = lapply(seq_along(feature_layers_to_use), 
                                    function(i) lapply(seq_along(feature_layers_to_use[[i]]), 
                                                       function(j) sapply(feature_layers_to_use[[i]][[j]], 
                                                                          switch_current_mode, 
                                                                          condition_class_bounds[[j]], 
                                                                          current_modes[[i]][[j]])))
  }
  
  return(feature_dynamics_modes)
}


# switch_current_mode <- function(current_feature_val, current_condition_class_bounds, current_mode){
#   
#   
#   switch_mode_flag = current_feature_val > max(current_condition_class_bounds[[current_mode]])
#   
#   if (current_mode == 1){
#     current_mode = sample(c(2, 3), 1)
#   } else if ((current_mode == 2) || (current_mode == 3)){
#     current_mode = 4
#   }
#   
#   return(current_mode)
#   
# }




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
  #   if (append_routine == 'banked_offset'){
  #     appended_object <- append_current_object(object_to_append,
  #                                              current_object,
  #                                              append_type = 'as_list',
  #                                              inds_to_append = seq_along(object_to_append))      #record current offset parcels in offsets object containing all offsets info
  #   } else {
  #record current offset parcels in offsets object containing all offset characteristics
  # needs a two step process as site_indexes are nested and the others are not
  
  #     appended_object <- append_current_object(object_to_append,
  #                                              current_object,
  #                                              append_type = 'as_list',
  #                                              inds_to_append = which(!(names(current_object) == 'site_indexes')))
  
  #     appended_object <- append_current_object(appended_object,
  #                                              current_object,
  #                                              append_type = 'as_group',
  #                                              inds_to_append = which(names(current_object) == 'site_indexes'))
  # }
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


kill_site_features <- function(site_feature_layers_to_develop){
  
  developed_feature_layers = lapply(seq_along(site_feature_layers_to_develop), 
                                    function(i) lapply(seq_along(site_feature_layers_to_develop[[i]]),  
                                                       function(j) array(0, length(site_feature_layers_to_develop[[i]][[j]]))))
  
  return(developed_feature_layers)
}


find_projection_yrs <- function(perform_dynamics_time_shift, current_site_feature_layers, yr, features_to_project, 
                                feature_dynamics_to_use, projection_type, unique_site_vals){
  
  if (perform_dynamics_time_shift == FALSE){
    projection_yrs = lapply(seq_along(current_site_feature_layers), function(i) rep(list(yr), length(features_to_project)))
  } else {
    projection_yrs = find_time_shifts(current_site_feature_layers, feature_dynamics_to_use, features_to_project, projection_type, unique_site_vals)
  }
  
}


calc_cfacs <- function(site_feature_layers_to_use, parcel_num_remaining, current_simulation_params, feature_params, feature_dynamics_to_use, feature_dynamics_modes_to_use,
                       time_horizons, offset_yrs, include_potential_developments, include_potential_offsets, include_unregulated_loss,
                       adjust_cfacs_flag, features_to_project, time_fill, yr){
  
  cfacs_object = list()
  
  projection_yrs = find_projection_yrs(perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift, 
                                       site_feature_layers_to_use, yr, features_to_project, feature_dynamics_to_use, projection_type, unique_site_vals)
  
  cfacs_object$cfacs = project_features(site_feature_layers_to_use,
                                        feature_params$background_projection_type,
                                        feature_params$background_update_dynamics_by_differential, 
                                        feature_dynamics_to_use,
                                        feature_dynamics_modes_to_use,
                                        time_horizons,
                                        current_simulation_params,
                                        features_to_project,
                                        time_fill, 
                                        perform_dynamics_time_shift = feature_params$perform_background_dynamics_time_shift,
                                        unique_site_vals = feature_params$unique_site_vals,
                                        projection_yrs)
  
  if (adjust_cfacs_flag == TRUE){
    cfacs_object$adjusted_cfacs = adjust_cfacs(cfacs_object$cfacs,
                                               include_potential_developments,
                                               include_potential_offsets,
                                               include_unregulated_loss,
                                               current_simulation_params,
                                               parcel_num_remaining,
                                               time_horizons,
                                               offset_yrs, 
                                               yr)
    
    cfacs_object$cfacs_to_use = sum_trajectories(cfacs_object$adjusted_cfacs)
  } else{
    cfacs_object$cfacs_to_use = sum_trajectories(cfacs_object$cfacs)
  }
  
  return(cfacs_object)
  
}


adjust_cfacs <- function(current_cfacs, include_potential_developments, include_potential_offsets, include_unregulated_loss,
                         current_simulation_params, parcel_num_remaining, time_horizons, offset_yrs, yr){
  
  time_horizons = unlist(time_horizons)
  
  parcel_num = length(current_cfacs)
  counter_weights = rep(list(1), parcel_num)
  if (include_unregulated_loss == TRUE){
    
    unregulated_loss_weights <- generate_weights(include_unregulated_loss,
                                                 calc_type = 'unregulated_loss',
                                                 current_simulation_params$max_offset_parcel_num,
                                                 current_simulation_params$intervention_vec,
                                                 offset_yrs,
                                                 time_horizons,
                                                 parcel_num,
                                                 parcel_num_remaining,
                                                 current_simulation_params$time_steps,
                                                 current_simulation_params$unregulated_loss_prob)
    counter_weights <- lapply(seq_len(parcel_num), function(i) counter_weights[[i]] - unregulated_loss_weights$weights[[i]])
  }
  
  if (include_potential_developments == TRUE){
    dev_probability_list <- generate_weights(include_potential_developments,
                                             calc_type = 'development',
                                             current_simulation_params$max_offset_parcel_num,
                                             current_simulation_params$intervention_vec,
                                             offset_yrs,
                                             time_horizons,
                                             parcel_num,
                                             parcel_num_remaining,
                                             current_simulation_params$time_steps,
                                             current_simulation_params$unregulated_loss_prob)
    counter_weights <- lapply(seq_len(parcel_num), function(i) counter_weights[[i]] - dev_probability_list$weights[[i]])
  }
  
  if (include_potential_offsets == TRUE){
    current_offset_probability_list <- generate_weights(include_potential_offsets,
                                                        calc_type = 'offset',
                                                        current_simulation_params$max_offset_parcel_num,
                                                        current_simulation_params$intervention_vec,
                                                        offset_yrs,
                                                        time_horizons,
                                                        parcel_num,
                                                        parcel_num_remaining,
                                                        current_simulation_params$time_steps,
                                                        current_simulation_params$unregulated_loss_prob)
    counter_weights <- lapply(seq_len(parcel_num), function(i) counter_weights[[i]] - current_offset_probability_list$weights[[i]])
  }
  
  inds_to_accept = lapply(seq_along(counter_weights), function(i) counter_weights[[i]] >= 0)
  counter_weights <- remove_neg_probs(counter_weights, inds_to_accept)
  
  adjusted_cfacs = lapply(seq_along(current_cfacs), function(i) lapply(seq_along(current_cfacs[[i]]),
                                                                       function(j) current_cfacs[[i]][[j]]*matrix(rep(counter_weights[[i]], dim(current_cfacs[[i]][[j]])[2]),
                                                                                                                  nrow = dim(current_cfacs[[i]][[j]])[1], byrow = FALSE)))
  if (include_potential_offsets == TRUE){
    offset_intervention_probs <- remove_neg_probs(current_offset_probability_list$weighted_probs, inds_to_accept)
    offset_projections <- calc_offset_projections(feature_params$background_projection_type,
                                                  current_cfacs, 
                                                  offset_intervention_probs, 
                                                  current_simulation_params$restoration_rate, 
                                                  time_horizons, 
                                                  feature_params$feature_num, 
                                                  current_simulation_params$min_eco_val, 
                                                  current_simulation_params$max_eco_val, 
                                                  yr)
    
    summed_offset_projections <- sum_offset_projs(offset_projections,
                                                  offset_intervention_probs, 
                                                  feature_params$feature_num, 
                                                  time_horizons)
    
    adjusted_cfacs = sum_clearing_offsets(adjusted_cfacs, summed_offset_projections, feature_params$feature_num)
    
  }
  
  return(adjusted_cfacs)
}





remove_neg_probs <- function(weight_list, inds_to_accept){
  weight_list <- lapply(seq_along(weight_list), function(i) weight_list[[i]]*inds_to_accept[[i]])
  return(weight_list)
}


generate_weights <- function(perform_weight, calc_type, offset_intervention_scale, intervention_vec, offset_yrs, time_horizons,
                             parcel_num, parcel_num_remaining, time_steps, unregulated_loss_prob){
  if (perform_weight == TRUE){
    if (calc_type == 'unregulated_loss'){
      weighted_probs <- lapply(seq_len(parcel_num), function(i) rep(unregulated_loss_prob, (time_horizons[i] + 1)))    #runif(n = (time_horizon + 1), min = 0, max = current_simulation_params$unregulated_loss_prob)
    } else {
      weighted_probs <- find_intervention_probability(intervention_vec,
                                                      offset_yrs,
                                                      calc_type,
                                                      offset_intervention_scale,
                                                      time_horizons,
                                                      parcel_num,
                                                      parcel_num_remaining,
                                                      time_steps)
    }
  } else {
    weighted_probs <- lapply(seq_len(parcel_num), function(i) rep(0, (time_horizons[i] + 1)))
  }
  
  weights <- lapply(weighted_probs, cumsum)
  weighted_object = list()
  weighted_object$weighted_probs = weighted_probs
  weighted_object$weights = weights
  return(weighted_object)
}




calc_offset_projections <- function(projection_type,current_cfacs, offset_probs, restoration_rate, time_horizons, feature_num, min_eco_val, max_eco_val, yr){
  
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
          current_offset_proj = project_feature_layers(current_simulation_params,
                                                       projection_type,
                                                       current_cfac[proj_yr, ],
                                                       restoration_rate,
                                                       (time_horizon - proj_yr),
                                                       time_fill = TRUE, 
                                                       yr)
          
          #           project_feature_layers(current_simulation_params,
          #                                  projection_type,
          #                                  update_dynamics_by_differential, 
          #                                  current_site_feature_layers[[i]][[j]],
          #                                  feature_dynamics_to_use[[i]][[j]],
          #                                  feature_dynamics_modes_to_use[[i]][[j]],
          #                                  time_horizon = unlist(current_time_horizons[i]),
          #                                  perform_dynamics_time_shift,
          #                                  time_fill, 
          #                                  yr)))
          
          
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


sum_cols_multi <- function(arr_to_use){
  if (length(dim(arr_to_use)) == 0){
    print('length error')
    stop()
  }else if (length(dim(arr_to_use)) == 1){
    arr_out = t(t(arr_to_use))
  } else if (length(dim(arr_to_use)) == 2){
    arr_out = apply(arr_to_use, MARGIN = 1, sum)
    arr_out = t(t(arr_out))
  } else if (length(dim(arr_to_use)) == 3){
    arr_out = apply(arr_to_use, MARGIN = c(1, 3), sum)
    dim(arr_out) = c(dim(arr_out), 1)
    arr_out = aperm(arr_out, c(1, 3, 2))
  }
  return(arr_out)
}


# sum through features for each site to yield single dimensional site state vector for each feature
sum_trajectories <- function(traj_list, features_to_use_in_offset_calc){
  
  parcel_traj_list = lapply(seq_along(traj_list), function(i) lapply(seq_along(traj_list[[i]]),
                                                                     function(j) sum_cols_multi(traj_list[[i]][[j]])))
  return(parcel_traj_list)
}
