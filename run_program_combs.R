setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
rm(list = ls())
WD = getwd()


source('initialise_common_params.R')

source('run_system_routines.R')
source('collate_routines.R')
source('plot_routines.R')
source('initialise_program_params.R')
source('generate_program_params.R')
#.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

run_from_saved = FALSE
save_initial_conditions = FALSE
write_pdf = FALSE

write_params_to_table = FALSE
overwrite_table = FALSE

table_file = '~/Documents/run_summary.csv'
print_folder = '~/Documents/offset_plots'

realisation_num = 4

if (run_from_saved == TRUE){
  parcels <-readRDS('parcels.rds')
  index_object <- readRDS('index_object.rds')
  initial_ecology <- readRDS('initial_ecology.rds')
  decline_rates_initial <- readRDS('decline_rates_initial.rds')
  common_params <- readRDS('common_params.rds')
  dev_vec <- readRDS('dev_vec.rds')
  
} else {
  common_params <- initialise_common_params()
  parcels <- initialise_shape_parcels(common_params)
  index_object <- initialise_index_object(parcels, common_params)
  initial_ecology <- initialise_ecology(common_params, land_parcels = parcels$land_parcels)
  decline_rates_initial <- build_decline_rates_multi(parcels, condition_change = 'decline', mean_decline_rate = 0.02, decline_rate_std = 0.005, common_params$eco_dims)
  dev_vec = find_prog_vector(time_steps = common_params$time_steps, prog_start = common_params$dev_start, prog_end = common_params$dev_end, 
                             total_prog_num = common_params$total_dev_num, sd = 1)
  
}

if (save_initial_conditions == TRUE){
  saveRDS(parcels, 'parcels.rds')
  saveRDS(index_object, 'index_object.rds')
  saveRDS(initial_ecology, 'initial_ecology.rds')
  saveRDS(decline_rates_initial, 'decline_rates_initial.rds')
  saveRDS(common_params, 'common_params.rds')
  saveRDS(dev_vec, 'dev_vec.rds')
}

program_params <- initialise_program_param_combs() # list all program combinations to test
program_combs <- generate_program_combs(program_params)  #generate all combinations of offset programs
prog_num = dim(program_combs)[1] #how many combinations there are in total


cl<-makeCluster(4)        #allow parallel processing on n = 4 processors
registerDoParallel(cl)

#for (comb_ind in seq_len(prog_num)){
for (comb_ind in 1911:prog_num){
  strt<-Sys.time()
  current_program_param_inds = unlist(program_combs[comb_ind, ])
  current_program <- generate_current_program(program_params, current_program_param_inds) #write current program as a list
  current_program_params <- collate_current_program(current_program)  #setup flags for cfacs, cfac adjustment etc.
  global_params = append(common_params, current_program_params) # add program specific parameters to common parameters
  
  if (global_params$use_offset_bank == TRUE){
    banked_offset_vec = find_prog_vector(time_steps = global_params$time_steps, prog_start = global_params$offset_bank_start, 
                                         prog_end = global_params$offset_bank_end, total_prog_num = global_params$offset_bank_num, sd = 2)
  } else {
    banked_offset_vec = list()
  }

  realisations <- foreach(run_ind = 1:realisation_num) %dopar%{
    run_offsets_simulation(global_params, initial_ecology, decline_rates_initial, parcels, dev_vec, banked_offset_vec)
  }
  
  realisations <- prepare_realisations(realisations) #remove unsuccessful realisations for collate routine
  
  if (length(realisations) > 0){
    print(paste('offset scheme success on ', length(realisations)/realisation_num*100, '%'))
    collated_realisations <- collate_realisations(realisations, global_params, dev_vec, decline_rates_initial, land_parcels = parcels$land_parcels, initial_ecology)
    plot_characteristics <- get_plot_characteristics(global_params, realisations)
    
    if (write_pdf == TRUE){
      filename = paste(print_folder, plot_characteristics, '.pdf', sep = '', collapse = '')
      pdf(filename)
    }
    
    plot_collated_realisations(collated_realisations, realisation_num = length(realisations), global_params, parcel_sum_lims = c(0, 20000), eco_ind = 1, lwd_vec = c(3, 0.15), outer_title = plot_characteristics)
    
    if (write_pdf == TRUE){
      dev.off()
    }
  } else {
    print('offset scheme failed')
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

