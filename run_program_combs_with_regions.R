
rm(list = ls())
WD = getwd()

library(foreach)
library(doParallel)
library(abind)
library(pixmap)

source_folder = '~/Documents/R_Codes/Offsets_Working_Feb_3/'
output_folder = '~/Documents/offset_plots_new/'

if (!file.exists(output_folder)){
  dir.create(output_folder)
}

source(paste(source_folder, 'initialise_params.R', sep = '', collapse = ''))                              # functions to collate simulation outputs
source(paste(source_folder,'run_system_routines_modularised.R', sep = '', collapse = ''))                # functions to run simulation
source(paste(source_folder,'collate_routines.R', sep = '', collapse = ''))                                # functions to collate simulation outputs
source(paste(source_folder,'plot_routines.R', sep = '', collapse = ''))                                   # functions to plot collated outputs

save_collated_realisations = TRUE
save_realisations = TRUE
run_from_saved = FALSE                                   # run from previous data or run from newly generated ecology etc.
save_initial_conditions = FALSE                             # use this to run from simulated data (calculated at the initialisation of the code) or load data (eg from zonation etc)
write_pdf = FALSE                                          # write graphical outputs to pdf (TRUE)
load_from_grassland_data = FALSE                                    # use this to run from simulated data (FALSE) or load data (TRUE - eg data from zonation etc - this will need to be modified to fit the expected format)

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
  if (load_from_grassland_data == TRUE){
    parcels <- initialise_parcels_from_data(filename = "~/Desktop/grassland_data/planning.units.uid_20ha.pgm")
    initial_ecology <- initialise_ecology_from_data(filename =  "~/Desktop/grassland_data/hab.map.master.zo1.pgm", land_parcels = parcels$land_parcels, eco_dims = global_params$eco_dims)
  } else {
    parcels <- initialise_shape_parcels(global_params)
    initial_ecology <- initialise_ecology(global_params, land_parcels = parcels$land_parcels) #generate initial ecology as randomised landscape divided into land parcels where each parcel is a cell composed of numerical elements
  }
  decline_rates_initial <- initialise_decline_rates(parcels, global_params$sample_decline_rate, global_params$mean_decline_rates, decline_rate_std = 1e-4, eco_dims = global_params$eco_dims)       # set up array of decline rates that are eassociated with each cell
  if (parcels$region_num > 1){
    regional_program_params_group = get_regional_params()
  } else {
    regional_program_params_group = list()
  }
  
  program_params_to_test <- initialise_program_params() # list all program combinations to test
  program_combs <- generate_program_combs(program_params_to_test)  #generate all combinations of offset programs
  prog_num = dim(program_combs)[1] #how many combinations there are in total
  program_params_group = generate_program_params_group(prog_num, program_combs, program_params_to_test)
  
  
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

print(paste('testing ', prog_num, ' combinations on ', crs, ' cores'))
  
# for (policy_ind in seq(prog_num)){
for (policy_ind in 3:prog_num){
  strt<-Sys.time()
  
  program_params_to_use = append(program_params_group[policy_ind], regional_program_params_group)
  sim_group = list(global_params, program_params_to_use, initial_ecology, decline_rates_initial, parcels)
  names(sim_group) = c('global_params', 'program_params_to_use', 'initial_ecology', 'decline_rates_initial', 'parcels')
  realisations <- foreach(run_ind = seq_len(global_params$realisation_num)) %dopar%{
    run_offsets_simulation(sim_group)
  }
  
#  realisations <- prepare_realisations(realisations) #remove unsuccessful realisations for collate routine
  
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
  
  if (perform_collate_realisations == TRUE){
    
    if (length(realisations) > 0){
      
      collated_realisations <- collate_realisations(realisations, global_params, program_params_to_use, use_cfac_type_in_sim = TRUE, decline_rates_initial, parcels, initial_ecology) #take simulation ouputs and calculate gains and losses
      
      if (save_collated_realisations == TRUE){
        current_output_folder = paste(output_folder, '/collated_realisations/', sep = '', collapse = '')
        if (!file.exists(current_output_folder)){
          dir.create(current_output_folder)
        }
        filename = paste(current_output_folder, sim_characteristics)
        if (load_from_grassland_data == TRUE){
          filename = paste(filename, '_grass_land', sep = '', collapse = '')
        }
        saveRDS(collated_realisations, paste(filename, '.rds', sep = '', collapse = '')) 
      }
    }
    
    if (write_pdf == TRUE){
      filename = paste(output_folder, sim_characteristics)
      if (load_from_grassland_data == TRUE){
        filename = paste(filename, '_grass_land', sep = '', collapse = '')
      }
      filename = paste(filename, '.pdf', sep = '', collapse = '')
      pdf(filename, width = 8.3, height = 11.7)
      plot_single_policy_collated_realisations(collated_realisations,                                
                                               realisation_num = length(realisations), 
                                               global_params, 
                                               global_params$program_params, 
                                               parcel_sum_lims = c(0, 20000), 
                                               eco_ind = 1, 
                                               lwd_vec = c(3, 0.15), 
                                               edge_title = sim_characteristics)              #write plot outputs from collated results to pdf
      
      if (write_pdf == TRUE){
        dev.off()
      }
      
    } 
    rm(collated_realisations)
  } 
  
  fin <- Sys.time()
  print(fin - strt)
  
}

stopCluster(cl)
# 
# if (show_movie == TRUE){ #combine outputs in list cell format to list of 3D arrays for each eco dimension "net_traj"
#   net_traj <- form_net_trajectory(trajectories_list = realisations[[1]]$trajectories, land_parcels= parcels$land_parcels, 
#                                   time_steps = global_params$time_steps, landscape_dims = parcels$landscape_dims, eco_dims = global_params$eco_dims)
#   graphics.off()
#   for (yr in seq_len(global_params$time_steps)){
#     image(net_traj[[1]][, , yr], zlim = c(global_params$min_eco_val, global_params$max_eco_val)) #output to series of image slices to build into movie using something like ImageJ
#     Sys.sleep(0.1)
#     print(paste('year = ', yr))
#   }
# }
# 
# 
# if (write_movie == TRUE){
#   net_traj <- form_net_trajectory(trajectories_list = realisations[[1]]$trajectories, land_parcels= parcels$land_parcels, 
#                                   time_steps = global_params$time_steps, landscape_dims = parcels$landscape_dims, eco_dims = global_params$eco_dims)
#   make_mov(img_stack = net_traj[[1]], filetype = 'png', mov_name = 'long_offsets', mov_folder = paste(output_folder, 'offset_time_slice/', sep = '', collapse = ''))
# }
# 
# if (write_offset_layer == TRUE){ #write all offset parcels to single layer to output as image
#   
#   rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
#   offset_layer <- generate_offset_layer(trajectories = realisations[[1]]$trajectories, 
#                                         layer_type = 'offset', 
#                                         program_parcels = unlist(realisations[[1]]$index_object$offsets),
#                                         land_parcels = parcels$land_parcels, 
#                                         time_steps = global_params$time_steps, 
#                                         landscape_dims = parcels$landscape_dims, 
#                                         eco_dims = global_params$eco_dims)
#   
#   png(filename = paste(output_folder, 'offset_layer.png', sep = '', collapse = ''), height = dim(offset_layer$layer)[1], width = dim(offset_layer$layer)[2])
#   image(offset_layer$layer, zlim = c(0,1), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
#   dev.off()
#   
#   rgb.palette <- colorRampPalette(c("black", "red"), space = "rgb")
#   dev_layer <- generate_offset_layer(trajectories = realisations[[1]]$trajectories, 
#                                         layer_type = 'development', 
#                                         program_parcels = unlist(realisations[[1]]$index_object$developments),
#                                         land_parcels = parcels$land_parcels, 
#                                         time_steps = global_params$time_steps, 
#                                         landscape_dims = parcels$landscape_dims, 
#                                         eco_dims = global_params$eco_dims) #write all developed parcels to single layer to output as image
#   
#   png(filename = paste(output_folder, 'dev_layer.png', sep = '', collapse = ''), height = dim(dev_layer$layer)[1], width = dim(dev_layer$layer)[2])
#   image(dev_layer$layer, zlim = c(0,1), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
#   dev.off()
#   
# }


