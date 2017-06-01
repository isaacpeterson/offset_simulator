rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = '~/Documents/R_Codes/Offsets_Working_Feb_3/'
output_folder = paste(path.expand('~'), '/offset_data/simulated/', sep = '', collapse = '')

if (!file.exists(output_folder)){
  dir.create(output_folder)
}

source(paste(source_folder, 'initialise_params.R', sep = '', collapse = ''))                              # functions to collate simulation outputs
source(paste(source_folder,'run_system_routines_modularised.R', sep = '', collapse = ''))                # functions to run simulation
source(paste(source_folder,'collate_routines.R', sep = '', collapse = ''))                                # functions to collate simulation outputs
source(paste(source_folder,'plot_routines.R', sep = '', collapse = ''))                                   # functions to plot collated outputs



save_realisations = TRUE 
collate_realisations = TRUE #TRUE, FALSE to perform collate routines as well as running the simulation or not
save_collated_realisations = TRUE
plot_impacts = TRUE #if collating realisations switch on to see impacts 



run_from_saved = FALSE                                   # run from previous data or run from newly generated ecology etc.
save_initial_conditions = FALSE                             # use this to run from simulated data (calculated at the initialisation of the code) or load data (eg from zonation etc)
data_type = 'simulated'                                  # use this to run from simulated data (FALSE) or load data (TRUE - eg data from zonation etc - this will need to be modified to fit the expected format)

write_movie = FALSE                                      # write evolving ecology to movie
show_movie = FALSE                                      # show output in movie form of evolving ecology
write_offset_layer = FALSE                                    # write layer containing all offset parcels to pdf

table_file = paste(output_folder, 'run_summary.csv', sep = '', collapse = '') 

if (run_from_saved == TRUE){
  parcels <- readRDS(paste(source_folder, 'parcels.rds', sep = '', collapse = '')) 
  initial_ecology <- readRDS(paste(source_folder, 'initial_ecology.rds', sep = '', collapse = '')) 
  decline_rates_initial <- readRDS(paste(source_folder, 'decline_rates_initial.rds', sep = '', collapse = ''))
  global_params <- readRDS(paste(source_folder, 'global_params.rds', sep = '', collapse = '')) 
} else {
  global_params <- initialise_global_params()
  if (data_type == 'grassland'){
    parcels <- initialise_parcels_from_data(data_folder = "~/offset_data/grassland_data/")
    initial_ecology <- initialise_ecology_from_grassland_data(filename =  "~/Desktop/grassland_data/hab.map.master.zo1.pgm", land_parcels = parcels$land_parcels, eco_dims = global_params$eco_dims)
    dev_weights = list()
  } else if (data_type == 'hunter'){
    data_folder = paste(path.expand('~'), '/offset_data/Hunter/data/', sep = '', collapse = '')
    
    parcels <- initialise_parcels_from_data(data_folder, data_type)
    
    initial_ecology <- initialise_ecology_from_hunter_data(data_folder, 
                                                           land_parcels = parcels$land_parcels, 
                                                           eco_dims = global_params$eco_dims)
    dev_weights <- readRDS(paste(data_folder, 'mining_raster.rds', sep = '', collapse = ''))
    dev_weights <- stackApply(dev_weights, indices = rep(1, dim(dev_weights)[3]), fun = sum)
    dev_weights_array <- raster_to_array(dev_weights)
    
    dev_weights = lapply(seq_along(land_parcels), function(i) sum(dev_weights_array[ land_parcels[[i]] ])/sum(dev_weights_array))
  } else if (data_type == 'simulated'){
    parcels <- initialise_shape_parcels(global_params)
    initial_ecology <- initialise_ecology(global_params, land_parcels = parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
    dev_weights = list()  
  }
  
  decline_rates_initial <- initialise_decline_rates(parcels, 
                                                    global_params$sample_decline_rate, 
                                                    global_params$mean_decline_rates, 
                                                    global_params$decline_rate_std, 
                                                    eco_dims = global_params$eco_dims)       # set up array of decline rates that are eassociated with each cell
  if (parcels$region_num > 1){
    regional_program_params_group = get_regional_params()
  } else {
    regional_program_params_group = list()
  }
  
  program_params_to_test <- initialise_program_params() # list all program combinations to test
  program_combs <- generate_program_combs(program_params_to_test)  #generate all combinations of offset programs
  policy_num = dim(program_combs)[1] #how many combinations there are in total
  program_params_group = generate_program_params_group(policy_num, program_combs, program_params_to_test)
  
}


if (save_initial_conditions == TRUE){
  saveRDS(parcels, paste('parcels.rds', sep = '', collapse = '')) 
  saveRDS(initial_ecology, paste('initial_ecology.rds', sep = '', collapse = '')) 
  saveRDS(decline_rates_initial, paste('decline_rates_initial.rds', sep = '', collapse = '')) 
  saveRDS(global_params, paste('global_params.rds', sep = '', collapse = '')) 
  saveRDS(program_params, paste('program_params_group.rds', sep = '', collapse = '')) 
}

crs = detectCores(all.tests = FALSE, logical = TRUE)

cl<-makeCluster(crs)        #allow parallel processing on n = 4 processors
registerDoParallel(cl)

print(paste('testing ', policy_num, ' combinations on ', crs, ' cores'))


for (policy_ind in seq(policy_num)){
  
  strt<-Sys.time()
  
  program_params_to_use = append(program_params_group[policy_ind], regional_program_params_group)
  sim_group = list(global_params, program_params_to_use, initial_ecology, decline_rates_initial, parcels, dev_weights)
  names(sim_group) = c('global_params', 'program_params_to_use', 'initial_ecology', 'decline_rates_initial', 'parcels', 'dev_weights')
  
  realisations <- foreach(run_ind = seq_len(global_params$realisation_num)) %dopar%{
    perform_offsets_simulation(sim_group)
  }
  
  sim_characteristics <- get_sim_characteristics(program_params_to_use, global_params$realisation_num)
  
  if (save_realisations == TRUE){
    
    current_output_folder = paste(output_folder, '/realisations/', sep = '', collapse = '')
    
    if (!file.exists(current_output_folder)){
      dir.create(current_output_folder)
    }
    
    realisations_filename = paste(current_output_folder, sim_characteristics, sep = '', collapse = '')
    
    current_output_folder = paste(output_folder, '/sim_group/', sep = '', collapse = '')
    if (!file.exists(current_output_folder)){
      dir.create(current_output_folder)
    }
    
    sim_group_filename = paste(current_output_folder, sim_characteristics, '_sim_group', sep = '', collapse = '')
    if (load_from_grassland_data == TRUE){
      realisations_filename = paste(realisations_filename, '_grass_land', sep = '', collapse = '')
      sim_group_filename = paste(sim_group_filename, '_grass_land', sep = '', collapse = '')
    }
    
    saveRDS(realisations, paste(realisations_filename, '.rds', sep = '', collapse = '')) 
    saveRDS(sim_group, paste(sim_group_filename, '.rds', sep = '', collapse = ''))  
    
  }
  
  if (collate_realisations == TRUE){
    collated_realisations <- run_collate_routines(realisations, 
                                                sim_group$global_params, 
                                                sim_group$program_params_to_use, 
                                                use_cfac_type_in_sim = FALSE, 
                                                sim_group$decline_rates_initial, 
                                                sim_group$parcels, 
                                                sim_group$initial_ecology) #take simulation ouputs and calculate gains and losses
  
    if (plot_impacts == TRUE){
      setup_sub_plots(nx = 3, ny = 1, x_space = 5, y_space = 5)
      plot_impact_set(collated_realisations, 
                      current_program_params = sim_group$program_params_to_use, 
                      site_plot_lims = c(-1e4, 1e4),
                      program_plot_lims = c(-6e5, 6e5), 
                      landscape_plot_lims = c(-6e5, 6e5), 
                      sets_to_plot = 50,
                      eco_ind = 1, 
                      lwd_vec = c(3, 0.5), 
                      edge_title = sim_characteristics, 
                      time_steps = 50, 
                      offset_bank = sim_group$program_params_to_use[[1]]$use_offset_bank,
                      sim_group$parcels$land_parcel_num)
    }
    if (save_collated_realisations == TRUE){
      current_output_folder = paste(output_folder, '/collated_realisations/', sep = '', collapse = '')
      
      if (!file.exists(current_output_folder)){
        dir.create(current_output_folder)
      }
      saveRDS(collated_realisations, paste(collated_folder, sim_characteristics, '.rds', sep = '', collapse = '')) 
    }
  }
  
  fin <- Sys.time()
  print(fin - strt)
  
}

stopCluster(cl)
