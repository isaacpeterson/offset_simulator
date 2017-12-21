library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(abind)
library(pixmap)
library(offsetsim)

data_type = 'grassland'

max_eco_val = 100
data_folder = 'data/'

simulation_inputs_folder = write_nested_folder('simulation_inputs/')

objects_to_save = list()
objects_to_save$LGA_array <- read_pnm_layer(filename = paste0(data_folder, 'planning.units.uid_20ha.pgm'))
objects_to_save$parcels <- LGA_to_parcel_list(objects_to_save$LGA_array)
landscape_ecology <- list(read_pnm_layer(paste0(data_folder, 'hab.map.master.zo1.pgm')))

objects_to_save$landscape_ecology <- scale_ecology(landscape_ecology, dim(landscape_ecology[[1]]), max_eco_val)
objects_to_save$parcel_ecology <- split_ecology(objects_to_save$landscape_ecology, objects_to_save$parcels$land_parcels)
objects_to_save$dev_weights <- rep(list(1/length(objects_to_save$parcels$land_parcels)), length(objects_to_save$parcels$land_parcels))
objects_to_save$offset_weights <- objects_to_save$dev_weights

save_simulation_inputs(objects_to_save, simulation_inputs_folder)