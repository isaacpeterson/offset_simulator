rm(list = ls())
library(offsetsim)

source_offset_simulation <- function(simulation_user_params_file, plot_user_params_file){

  source(simulation_user_params_file)
  
  user_variable_params = initialise_default_variable_params()
  user_params = initialise_default_run_params()
  
  osim.run(user_params, user_variable_params, loglevel = 'TRACE')
  
  source(plot_user_params_file)
  plot_params <- initialise_default_plot_params()
  osim.plot(plot_params, user_params, loglevel = 'TRACE')
}

source_offset_simulation(simulation_user_params_file = 'default_initialise_params.R', 
                        plot_user_params_file = 'default_plot_params.R')