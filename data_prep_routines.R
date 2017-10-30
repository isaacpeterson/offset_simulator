write_folder <- function(current_folder){
  if (!file.exists(current_folder)){
    dir.create(current_folder)
  }
  return(current_folder)
}


scale_ecology <- function(landscape_ecology, max_eco_val){
  landscape_ecology <- lapply(seq_along(landscape_ecology), 
                              function (i) landscape_ecology[[i]]/max(landscape_ecology[[i]]) * max_eco_val)
  return(landscape_ecology)
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


raster_to_array <- function(raster_object){
  raster_array = as.matrix(raster_object)
  raster_array[is.na(raster_array)] = 0
  return(raster_array)
}



read_pnm_layer <- function(filename){
  img = read.pnm(file = filename, cellres = 1)
  array_to_use = img@grey
  return(array_to_use)
}


mcell <- function(Arr_in, vx, vy){       #used to break up array into samller set of sub arrays defined by vx and vy that fit together to give input array
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  output_list = vector('list', rows*cols)   # make an array composed of lists with dimenisons that define the land parcels/regions. The list format allows arrays of different sizes to be stored
  colStart = 0
  for (i in seq_len(cols)){       # run down through the columns of input array 
    rowStart = 0
    for (j in seq_len(rows)){ #group elements of input array into sub arrays and assign to output_list
      output_list[[a]] = Arr_in[rowStart+(1:rowsizes[j]), colStart+(1:colsizes[i])]
      rowStart = rowStart + rowsizes[j]
      a = a + 1
    }
    colStart = colStart + colsizes[i]
  }
  
  return(output_list)
  
}  



save_simulation_inputs <- function(simulation_input_folder, LGA_array, parcels, landscape_ecology,
                                   parcel_ecology, dev_weights){
  saveRDS(LGA_array, paste0(simulation_input_folder, 'LGA_array.rds')) 
  saveRDS(parcels, paste0(simulation_input_folder, 'parcels.rds')) 
  saveRDS(landscape_ecology, paste0(simulation_input_folder, 'landscape_ecology.rds')) 
  saveRDS(parcel_ecology, paste0(simulation_input_folder, 'parcel_ecology.rds')) 
  saveRDS(dev_weights, paste0(simulation_input_folder, 'dev_weights.rds')) 
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





simulate_LGA <- function(simulated_ecology_params){
  
  parcel_num_x = simulated_ecology_params$parcel_num_x   #length in parcels of array in x 
  parcel_num_y = simulated_ecology_params$parcel_num_y #length in parcels of array in y 
  parcel_vx = split_vector(parcel_num_x, simulated_ecology_params$ecology_size[2], sd = 5, min_width = 3) # make normally distributed vector that sums to ecology size, composed of n elements where n is the parcel dimension in x
  parcel_vy = split_vector(parcel_num_y, simulated_ecology_params$ecology_size[1], sd = 5, min_width = 3) # as above for y
  
  pixel_indexes = 1:(simulated_ecology_params$ecology_size[1]*simulated_ecology_params$ecology_size[2])     #index all elements of ecology array
  dim(pixel_indexes) = c(simulated_ecology_params$ecology_size[1], simulated_ecology_params$ecology_size[2])  # arrange ecology array index vector into array of landscape dimensions 
  parcels = mcell(pixel_indexes, parcel_vx, parcel_vy) #split the ecology array into a series of subarrays with dimensions sz_x by sz_y
  
  parcel_list = lapply(seq_along(parcels), function(i) array(i, dim(parcels[[i]])))
  parcel_array = array(0, dim(pixel_indexes))
  parcel_array[unlist(parcels)] = unlist(parcel_list)
  
  region_vx = split_vector(simulated_ecology_params$region_num_x, parcel_num_x, 1, min_width = 3) # perform similar operation used to split array into smallest elements, but this time for land parcels, arranging into regions
  region_vy = split_vector(simulated_ecology_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  parcel_indexes = seq(length(parcel_list))
  dim(parcel_indexes) = c(region_vy, region_vx)

  
  return(parcel_array)
}



LGA_to_parcel_list <- function(LGA_array){

  site_group_vals = unique(as.vector(LGA_array))
  land_parcels <- lapply(seq_along(site_group_vals), function(i) which(LGA_array == site_group_vals[i]))
  
  parcels = list()
  parcels$landscape_dims = dim(LGA_array)
  parcels$parcel_indexes = seq_along(land_parcels)
  parcels$regions = list(parcels$parcel_indexes) #use only one region - if necessary this can be split up into multi-region
  parcels$region_num = length(parcels$regions)
  parcels$land_parcel_num = length(land_parcels)
  parcels$land_parcels = land_parcels
  parcels$parcel_array = LGA_array
  
  return(parcels)
}










