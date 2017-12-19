run_offset_simulation <- function(simulation_user_params_file, plot_user_params_file){
  library(offsetsim)
  
  source(simulation_user_params_file)
  
  user_variable_params = initialise_variable_params()
  user_params = initialise_user_params()
  
  osim.run(user_params, user_variable_params, loglevel = 'TRACE')
  
  source(plot_user_params_file)
  plot_params <- initialise_plot_params()
  osim.plot(plot_params, user_params, loglevel = 'TRACE')
}

run_offset_simulation(simulation_user_params_file = 'user_params/initialise_params_testing_new.R', 
                      plot_user_params_file = 'user_params/plot_params.R')