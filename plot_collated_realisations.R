# Plot the collated simulaton results
# To run: source('plot_collated_realisations.R')

rm(list=ls(all=TRUE))

   #---------------------
    # User parameters
    #---------------------


plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
realisation_num = 'all' # 'all' or number to plot
offset_bank = FALSE
write_pdf = FALSE
run_number = 36 # for output plot name
feature_ind = 1
sets_to_plot = 1
plot_vector = 1
string_width = 3 #how many digits are used to store scenario index and realisation index

current_folder = paste0('~/offset_data/simulated/simulation_runs/', 
                        formatC(run_number, width = 5, format = "d", flag = "0"), '/')

#current_folder = paste0('~/offset_data/simulated/nectar_runs/data100reps/')
collated_folder = paste0(current_folder, '/collated_outputs/')  # LOCATION OF COLLATED FILES
simulation_inputs_folder = paste0(current_folder, '/simulation_inputs/')
#collated_folder = '/Users/ascelin/analysis/src/offset_simulator/data3/collated_realisations/'
output_plot_folder = collated_folder

    #---------------------
    # User parameters
    #---------------------

source('plot_routines.R')                                   # functions to plot collated outputs
source('collate_routines.R')

setup_sub_plots(nx = 3, ny = 4, x_space = 5, y_space = 5)

check_plot_options()

run_params = readRDS(paste0(simulation_inputs_folder, '/run_params.rds'))

if (!file.exists(output_plot_folder)){
  dir.create(output_plot_folder)
}

# Set the output filename, and open the pdf file for reading
if (write_pdf == TRUE){
  if (plot_type == 'impacts'){
    filename = paste0(output_plot_folder, '/impacts.pdf')
  } else if (plot_type == 'outcomes'){
    filename = paste0(output_plot_folder, '/outcomes.pdf')
  }
  pdf(filename, width = 8.3, height = 11.7) 
} 

if (offset_bank == TRUE){
  site_plot_lims = c(0, 2e6)
  site_impact_plot_lims = c(-1e6, 1e6)
} else {
  site_plot_lims = c(0, 1e4)
  site_impact_plot_lims = c(-1e4, 1e4)
}

program_plot_lims = c(0, 1e6) 
landscape_plot_lims = c(0, 3e6)

program_impact_plot_lims = c(-2e5, 2e5) 
landscape_impact_plot_lims = c(-6e5, 0)


scenario_filenames <- list.files(path = simulation_inputs_folder, pattern = '_policy_params', all.files = FALSE, 
                                 full.names = FALSE, recursive = FALSE, ignore.case = FALSE, 
                                 include.dirs = FALSE, no.. = FALSE)

if (length(plot_vector) == 0){
  plot_vector = seq_along(scenario_filenames)
}

if (length(scenario_filenames) == 0){
  stop( paste('\nERROR: No files that match _policy_params found in', simulation_inputs_folder) )
}

for (scenario_ind in plot_vector){
  current_policy_params = readRDS(paste0(simulation_inputs_folder, '/', scenario_filenames[scenario_ind]))
  collated_filenames = find_collated_files(file_path = collated_folder, 
                                            scenario_string = formatC(scenario_ind, width = string_width, format = "d", flag = "0"), 
                                            feature_string = formatC(feature_ind, width = string_width, format = "d", flag = "0"), 
                                           realisation_num)
  collated_realisations = bind_collated_realisations(collated_filenames)

  if (plot_type == 'impacts'){
    plot_impact_set(collated_realisations, 
                    current_policy_params, 
                    site_impact_plot_lims,
                    program_impact_plot_lims, 
                    landscape_impact_plot_lims, 
                    sets_to_plot,
                    lwd_vec = c(3, 0.5), 
                    time_steps = run_params$time_steps, 
                    parcel_num = vector(),
                    realisation_num = collated_realisations$realisation_num) 
  } else {
    plot_outcome_set(collated_realisations,
                     current_policy_params,
                     site_plot_lims,
                     program_plot_lims, 
                     landscape_plot_lims,
                     sets_to_plot,
                     lwd_vec = c(3, 0.5), 
                     time_steps = run_params$time_steps,
                     realisation_num = collated_realisations$realisation_num) 
  }
  
  print(paste0('policy ', scenario_ind, ' done'))
  
}


# Close the pdf file for reading
if (write_pdf == TRUE) dev.off()

