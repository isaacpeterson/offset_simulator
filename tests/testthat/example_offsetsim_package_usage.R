library(offsetsim)

source('default_params.R')

user_simulation_params = initialise_default_simulation_params()
user_global_params = initialise_default_global_params()
user_simulated_ecology_params = initialise_default_simulated_ecology_params()
user_output_params <- initialise_default_output_params()

user_global_params$number_of_cores = 1; 
 user_output_params$output_csv_file = FALSE; 
 user_global_params$simulation_folder = '/Users/E24661/GitHub/offset_simulator/tests/output/test01out/'; 
 user_global_params$realisation_num = 1; 
 user_global_params$unique_simulation_folder = FALSE; 
 user_global_params$set_seed = TRUE; 
 osim.run(user_global_params, user_simulation_params, user_simulated_ecology_params, loglevel = 'TRACE')
current_simulation_folder = find_current_run_folder(user_global_params$simulation_folder)
osim.output(user_output_params, current_simulation_folder)
