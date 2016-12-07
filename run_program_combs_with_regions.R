setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source('initialise_params.R')                               # functions to collate simulation outputs
source('run_system_routines_modularised.R')                 # functions to run simulation
source('collate_routines.R')                                # functions to collate simulation outputs
source('plot_routines.R')                                   # functions to plot collated outputs

run_from_saved = FALSE                                      # run from previous data or run from newly generated ecology etc.
save_initial_conditions = FALSE                             # use this to run from simulated data (calculated at the initialisation of the code) or load data (eg from zonation etc)
write_pdf = FALSE                                           # write graphical outputs to pdf (TRUE)
load_from_data = FALSE                                      # use this to run from simulated data (calculated at the initialisation of the code) or load data (eg from zonation etc)

write_params_to_table = FALSE
overwrite_table = FALSE
write_movie = FALSE                                      # write evolving ecology to movie
show_movie = FALSE                                      # show output in movie form of evolving ecology
write_offset_layer = TRUE                                      # write layer containing all offset parcels to pdf

table_file = '~/Documents/run_summary.csv'
print_folder = '~/Documents/offset_plots/'


if (run_from_saved == TRUE){
  parcels <- readRDS('parcels.rds')
  index_object <- readRDS('index_object.rds')
  initial_ecology <- readRDS('initial_ecology.rds')
  decline_rates_initial <- readRDS('decline_rates_initial.rds')
  global_params <- readRDS('global_params.rds')
} else {
  global_params <- initialise_global_params()
  if (load_from_data == TRUE){
    parcels <- initialise_parcels_from_data(filename = "~/Desktop/grassland_data/planning.units.uid_20ha.pgm")
    initial_ecology <- initialise_ecology_from_data(filename =  "~/Desktop/grassland_data/hab.map.master.zo1.pgm", land_parcels = parcels$land_parcels, eco_dims = global_params$eco_dims)
  } else {
    parcels <- initialise_shape_parcels(global_params)
    initial_ecology <- initialise_ecology(global_params, land_parcels = parcels$land_parcels)
  }
  decline_rates_initial <- initialise_decline_rates(parcels, global_params$mean_decline_rates, decline_rate_std = 1e-4, eco_dims = global_params$eco_dims)       # set up array of decline rates that are eassociated with each cell
}

if (save_initial_conditions == TRUE){
  saveRDS(parcels, 'parcels.rds')
  saveRDS(index_object, 'index_object.rds')
  saveRDS(initial_ecology, 'initial_ecology.rds')
  saveRDS(decline_rates_initial, 'decline_rates_initial.rds')
  saveRDS(global_params, 'global_params.rds')
  saveRDS(dev_vec, 'dev_vec.rds')
}

program_params = vector('list', 2)
program_params[[2]] <- initialise_program_params() # list all program combinations to test
program_combs <- generate_program_combs(program_params[[2]])  #generate all combinations of offset programs
prog_num = dim(program_combs)[1] #how many combinations there are in total
print(paste('combinations = ', prog_num))
cl<-makeCluster(4)        #allow parallel processing on n = 4 processors
registerDoParallel(cl)


for (comb_ind in seq(prog_num)){

  strt<-Sys.time()

  current_program_param_inds = unlist(program_combs[comb_ind, ])
  current_program <- generate_current_program(program_params[[2]], current_program_param_inds) #write current program as a list
  current_program_params <- collate_current_program(current_program, global_params)  #setup flags for cfacs, cfac adjustment etc.
  program_params[[1]] = current_program_params
  
  realisations <- foreach(run_ind = seq_len(global_params$realisation_num)) %dopar%{
    run_offsets_simulation(global_params, program_params, initial_ecology, decline_rates_initial, parcels, banked_offset_vec)
  }
  
  realisations <- prepare_realisations(realisations) #remove unsuccessful realisations for collate routine
  
  if (length(realisations) > 0){
    
    collated_realisations <- collate_realisations(realisations, global_params, use_cfac_type_in_sim = TRUE, decline_rates_initial, land_parcels = parcels$land_parcels, initial_ecology)
    plot_characteristics <- get_plot_characteristics(program_params, realisations)
    
    if (write_pdf == TRUE){
      filename = paste(print_folder, plot_characteristics, '.pdf', sep = '', collapse = '')
      pdf(filename, width = 8.3, height = 11.7)
      plot_collated_realisations(collated_realisations, 
                                 realisation_num = length(realisations), 
                                 global_params, 
                                 program_params, 
                                 parcel_sum_lims = c(0, 20000), 
                                 eco_ind = 1, 
                                 lwd_vec = c(3, 0.15), 
                                 edge_title = plot_characteristics)
      
      if (write_pdf == TRUE){
        dev.off()
      }
    } 
    
    plot_collated_realisations(collated_realisations, 
                               realisation_num = length(realisations), 
                               global_params, 
                               program_params, 
                               parcel_sum_lims = c(0, 20000), 
                               eco_ind = 1, 
                               lwd_vec = c(3, 0.5), 
                               edge_title = plot_characteristics)
  } else {
    print('no parcel sets found')
    collated_realisations = list()
  }

  fin <- Sys.time()
  print(fin - strt)
  
  if (write_params_to_table == TRUE){
    table_params <- current_program_params
    table_params$system_NNL_success <- collated_realisations$system_NNL_plot_object$NNL_success
    table_params$system_mean_NNL <- collated_realisations$system_NNL_plot_object$mean_NNL
    table_params$parcel_set_mean_NNL <- collated_realisations$parcel_set_NNL_plot_object$mean_NNL
    table_params$parcel_set_mean_NNL <- collated_realisations$parcel_set_NNL_plot_object$NNL_success
    table_params = as.data.frame(table_params)
    if ( (overwrite_table == TRUE) & (comb_ind == 1)){
      write.table(table_params, file = table_file, quote = FALSE, append = FALSE, sep = ",", eol = '\n', na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
    } else {
      write.table(table_params, file = table_file, quote = FALSE, append = TRUE, sep = ",", eol = '\n', na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
    }
  }
  
}

stopCluster(cl)

if (show_movie == TRUE){
  net_traj <- form_net_trajectory(trajectories_list = realisations[[1]]$trajectories, land_parcels= parcels$land_parcels, 
                                  time_steps = global_params$time_steps, landscape_dims = parcels$landscape_dims, eco_dims = global_params$eco_dims)
  graphics.off()
  for (yr in seq_len(global_params$time_steps)){
    image(net_traj[[1]][, , yr], zlim = c(global_params$min_eco_val, global_params$max_eco_val))
    Sys.sleep(0.1)
    print(paste('year = ', yr))
  }
}


if (write_movie == TRUE){
  net_traj <- form_net_trajectory(trajectories_list = realisations[[1]]$trajectories, land_parcels= parcels$land_parcels, 
                                  time_steps = global_params$time_steps, landscape_dims = parcels$landscape_dims, eco_dims = global_params$eco_dims)
  
  source('~/Documents/R_Codes/make_movie.R')
  make_mov(img_stack = net_traj[[1]], filetype = 'png', mov_name = 'long_offsets', mov_folder = '~/Documents/offsets_movs_sim/')
}

if (write_offset_layer == TRUE){
  rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
  offset_layer <- generate_offset_layer(trajectories = realisations[[1]]$trajectories, 
                                        layer_type = 'offset', 
                                        program_parcels = unlist(realisations[[1]]$index_object$offsets),
                                        land_parcels = parcels$land_parcels, 
                                        time_steps = global_params$time_steps, 
                                        landscape_dims = parcels$landscape_dims, 
                                        eco_dims = global_params$eco_dims)
  
  png(filename = '~/Documents/offset_plots/offset_layer.png', height = dim(offset_layer$layer)[1], width = dim(offset_layer$layer)[2])
  image(offset_layer$layer, zlim = c(0,1), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
  dev.off()
  
  rgb.palette <- colorRampPalette(c("black", "red"), space = "rgb")
  dev_layer <- generate_offset_layer(trajectories = realisations[[1]]$trajectories, 
                                        layer_type = 'development', 
                                        program_parcels = unlist(realisations[[1]]$index_object$developments),
                                        land_parcels = parcels$land_parcels, 
                                        time_steps = global_params$time_steps, 
                                        landscape_dims = parcels$landscape_dims, 
                                        eco_dims = global_params$eco_dims)
  
  png(filename = '~/Documents/offset_plots/dev_layer.png', height = dim(dev_layer$layer)[1], width = dim(dev_layer$layer)[2])
  image(dev_layer$layer, zlim = c(0,1), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
  dev.off()
}
