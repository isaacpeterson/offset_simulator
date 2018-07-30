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












