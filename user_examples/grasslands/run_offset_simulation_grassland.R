rm(list = ls())
library(offsetsim)

source_offset_simulation <- function(simulation_user_params_file, user_plot_params_file){
  
  source(simulation_user_params_file)
  
  user_combination_params = initialise_user_combination_params()
  user_global_params = initialise_user_global_params()
  
  osim.run(user_global_params, user_combination_params, loglevel = 'TRACE')
  
  source(user_plot_params_file)
  plot_params <- initialise_plot_params()
  osim.plot(plot_params, simulation_folder = user_global_params$simulation_folder, loglevel = 'TRACE')
}

source('load_grassland_data.R')
source_offset_simulation(simulation_user_params_file = 'initialise_params_grassland.R', 
                      user_plot_params_file = 'plot_params_grasslands.R')