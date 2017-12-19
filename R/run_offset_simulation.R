library(offsetsim)

source('user_params/initialise_params_testing_new.R')

user_variable_params = initialise_variable_params()
user_params = initialise_user_params()

osim.run(user_params, user_variable_params, loglevel = 'TRACE')

source('user_params/plot_params.R')
plot_params <- initialise_plot_params()
osim.plot(plot_params, user_params, loglevel = 'TRACE')
