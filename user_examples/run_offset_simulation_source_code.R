rm(list = ls())
library(offsetsim)

source_offset_simulation <- function(simulation_user_params_file, plot_user_params_file){

  source(simulation_user_params_file)
  
  user_combination_params = initialise_default_combination_params()
  user_global_params = initialise_default_global_params()
  
  osim.run(user_global_params, user_combination_params, loglevel = 'TRACE')
  
  source(plot_user_params_file)
  user_plot_params <- initialise_default_plot_params()
  osim.plot(user_plot_params, simulation_folder = user_global_params$simulation_folder, loglevel = 'TRACE')
}

source_offset_simulation(simulation_user_params_file = 'default_initialise_params.R', 
                        plot_user_params_file = 'default_plot_params.R')