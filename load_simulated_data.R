load_simulated_data <- function(){
  source('data_prep_routines.R')
  data_type = 'simulated'
  ecology_params <- initialise_ecology_params()
  
  simulated_ecology_params <- initialise_simulated_ecology_params()
  LGA_array <- simulate_LGA(simulated_ecology_params)
  parcels <- LGA_to_parcel_list(LGA_array)
  parcel_ecology <- simulate_ecology(simulated_ecology_params, eco_dims = ecology_params$eco_dims, land_parcels = parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
  landscape_ecology = list()
  dev_weights = list()  
  
  folder_to_use = write_folder(paste0(path.expand('~'), '/offset_data/'))
  folder_to_use = write_folder(paste0(folder_to_use, data_type, '/'))
  simulation_input_folder = write_folder(paste0(folder_to_use, 'simulation_inputs/'))
  
  save_simulation_inputs(simulation_input_folder, LGA_array, decline_rates_initial, parcels, landscape_ecology,
                         parcel_ecology, dev_weights, ecology_params)
}