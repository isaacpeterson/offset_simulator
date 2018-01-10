write_folder <- function(current_folder){
  if (!file.exists(current_folder)){
    dir.create(current_folder)
  }
  return(current_folder)
}


scale_ecology <- function(landscape_ecology, max_eco_val, landscape_dims){
  
  scaled_landscape_ecology <- list_of_zeros(length(landscape_ecology), landscape_dims) 
  
  for (feature_ind in seq_along(landscape_ecology)){
    if (max(landscape_ecology[[feature_ind]]) > 0){
      scaled_landscape_ecology[[feature_ind]] = landscape_ecology[[feature_ind]]/max(landscape_ecology[[feature_ind]]) * max_eco_val
    }
  }
  
  return(scaled_landscape_ecology)
}

shp_to_raster <- function(shp, raster_dims){
  r <- raster(ncol=raster_dims[2], nrow=raster_dims[1])
  extent(r) <- extent(shp)
  raster_object <- rasterize(shp, r)
  return(raster_object)
}


load_rasters <- function(current_data_path, current_filenames, layer_num){
  if (layer_num == 'all'){
    layer_num = length(current_filenames)
  }
  for (feature_ind in seq(layer_num)){
    current_species_filename = paste0(current_data_path, current_filenames[feature_ind])
    current_raster = raster(current_species_filename)
    if (feature_ind == 1){
      raster_stack = current_raster
    } else{
      raster_stack = stack(raster_stack, current_raster)
    }
  }
  return(raster_stack) 
  
}


raster_to_array <- function(raster_object){
  raster_array = as.matrix(raster_object, ncol = ncol(raster_object))
  raster_array[is.na(raster_array)] = 0
  return(raster_array)
}


read_pnm_layer <- function(filename){
  img = read.pnm(file = filename, cellres = 1)
  array_to_use = img@grey
  return(array_to_use)
}


save_simulation_inputs <- function(objects_to_save, simulation_inputs_folder){
  write_nested_folder(simulation_inputs_folder)
  filenames_to_save = names(objects_to_save)
  for (file_ind in seq_along(objects_to_save)){
    saveRDS(objects_to_save[[file_ind]], paste0(simulation_inputs_folder, filenames_to_save[file_ind], '.rds')) 
  }
}


split_ecology <- function(landscape_ecology, land_parcels){
  current_ecology = lapply(seq_along(land_parcels), 
                           function(i) lapply(seq_along(landscape_ecology), 
                                              function(j) landscape_ecology[[j]][land_parcels[[i]]]))
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


LGA_to_parcel_list <- function(LGA_array){

  site_group_vals = unique(as.vector(LGA_array))
  land_parcels <- lapply(seq_along(site_group_vals), function(i) which(LGA_array == site_group_vals[i]))
  
  parcels = list()
  parcels$landscape_dims = dim(LGA_array)
  parcels$site_indexes = seq_along(land_parcels)
  parcels$regions = list(parcels$site_indexes) #use only one region - if necessary this can be split up into multi-region
  parcels$region_num = length(parcels$regions)
  parcels$land_parcel_num = length(land_parcels)
  parcels$land_parcels = land_parcels
  parcels$parcel_array = LGA_array
  
  return(parcels)
}










