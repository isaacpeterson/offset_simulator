setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
rm(list = ls())
WD = getwd()
strt<-Sys.time()

source('initialise_common_params.R')

source('run_system_routines.R')
source('collate_routines.R')
source('plot_routines.R')
source('initialise_program_params.R')

#.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

run_from_saved = TRUE
save_initial_conditions = FALSE
write_pdf = TRUE

write_params_to_table = TRUE
overwrite_table = FALSE

table_file = '~/Documents/run_summary.csv'
print_folder = '~/Documents/'

realisation_num = 20

offset_bank_type = 'parcel_set'       #'parcel_set' or 'credit'
use_offset_bank = TRUE

use_parcel_set_dev_credit = FALSE

offset_bank_params = c(1, 10, 50) # parameters for offset banking in format (start_time, end_time, number_of_banked_parcels)
offset_parcel_for_parcel = TRUE
restoration_rate = 0.03
offset_time_horizon = 10
offset_multiplier = 1
offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
offset_calc_type = 'restoration_from_cfac'   #'current_condition', 'avoided_degredation', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
dev_calc_type = 'future_condition'                     #'future_condition', 'current_condition' 
offset_action_type = 'restore'                         #'protect', 'maintain', 'restore'

if (offset_calc_type == 'avoided_degredation'){
  offset_action_type = 'maintain'
} else if (offset_calc_type %in% c('restoration_from_cfac', 'restoration_gains', 'restoration_condition_value')){
  offset_action_type = 'restore'
}



cfac_type_in_offset_calc = 'standard'        #'standard', 'include_clearing', 'include_clearing_offsets'

cfac_type_in_dev_calc = cfac_type_in_offset_calc        #'standard_cfac', 'include_clearing', 'include_clearing_offsets'



if (run_from_saved == TRUE){
  parcels <-readRDS('parcels.rds')
  index_object <- readRDS('index_object.rds')
  initial_ecology <- readRDS('initial_ecology.rds')
  decline_rates_initial <- readRDS('decline_rates_initial.rds')
  global_params <- readRDS('global_params.rds')
  dev_vec <- readRDS('dev_vec.rds')
  
} else {
  global_params <- initialise_common_params()
  parcels <- initialise_shape_parcels(global_params)
  index_object <- initialise_index_object(parcels, global_params)
  initial_ecology <- initialise_ecology(global_params, land_parcels = parcels$land_parcels)
  decline_rates_initial <- build_decline_rates_multi(parcels, condition_change = 'decline', mean_decline_rate = 0.02, decline_rate_std = 0.005, global_params$eco_dims)
  dev_vec = find_prog_vector(time_steps = global_params$time_steps, prog_start = global_params$dev_start, prog_end = global_params$dev_end, 
                                           total_prog_num = global_params$total_dev_num, sd = 1)
  
}

if (save_initial_conditions == TRUE){
  saveRDS(parcels, 'parcels.rds')
  saveRDS(index_object, 'index_object.rds')
  saveRDS(initial_ecology, 'initial_ecology.rds')
  saveRDS(decline_rates_initial, 'decline_rates_initial.rds')
  saveRDS(global_params, 'global_params.rds')
  saveRDS(dev_vec, 'dev_vec.rds')
}

land_parcels <- parcels$land_parcels

program_params <- initialise_program_params(offset_multiplier, restoration_rate, use_parcel_set_dev_credit, offset_parcel_for_parcel, offset_bank_type, use_offset_bank, offset_bank_params, offset_time_horizon_type, offset_calc_type, 
                                             dev_calc_type, offset_action_type, cfac_type_in_offset_calc, offset_time_horizon)

global_params = append(global_params, program_params)
if (global_params$use_offset_bank == TRUE){
  banked_offset_vec = find_prog_vector(time_steps = global_params$time_steps, prog_start = global_params$offset_bank_start, 
                                       prog_end = global_params$offset_bank_end, total_prog_num = global_params$offset_bank_num, sd = 2)
} else {
  banked_offset_vec = list()
}

cl<-makeCluster(4)        #allow parallel processing on n = 4 processors
registerDoParallel(cl)

realisations <- foreach(run_ind = 1:realisation_num) %dopar%{
  run_offsets_simulation(global_params, initial_ecology, decline_rates_initial, parcels, dev_vec, banked_offset_vec)
}

offset_success_flag = unlist(lapply(seq_along(realisations), function(i) realisations[[i]]$offset_success_flag))

if (all(offset_success_flag == TRUE)){
  collated_realisations <- collate_realisations(realisations, global_params, dev_vec, decline_rates_initial, land_parcels, initial_ecology)
  plot_characteristics <- get_plot_characteristics(global_params, realisations)

  if (write_pdf == TRUE){
    filename = paste(print_folder, plot_characteristics, '.pdf', sep = '', collapse = '')
    pdf(filename)
  }
  
  plot_collated_realisations(collated_realisations, realisation_num, global_params, parcel_sum_lims = c(0, 20000), eco_ind = 1, lwd_vec = c(3, 0.15), outer_title = plot_characteristics)
  
  if (write_pdf == TRUE){
    dev.off()
  }
} else {
  print(paste('offset scheme failed on ', length(which(offset_success_flag == FALSE))/length(offset_success_flag)*100, '%'))
}

fin <- Sys.time()
print(fin - strt)

if (write_params_to_table == TRUE){
  table_params <- program_params
  table_params$system_NNL_success <- collated_realisations$system_NNL_plot_object$NNL_success
  table_params$system_mean_NNL <- collated_realisations$system_NNL_plot_object$mean_NNL
    table_params$parcel_set_mean_NNL <- collated_realisations$parcel_set_NNL_plot_object$mean_NNL
    table_params$parcel_set_mean_NNL <- collated_realisations$parcel_set_NNL_plot_object$NNL_success
    
  table_params = as.data.frame(table_params)
  if (overwrite_table == TRUE){
    write.table(table_params, file = table_file, quote = FALSE, append = FALSE, sep = ",", eol = '\n', na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
  } else {
    write.table(table_params, file = table_file, quote = FALSE, append = TRUE, sep = ",", eol = '\n', na = "NA", dec = ".", row.names = FALSE, col.names = FALSE)
  }
}

mean(collated_realisations$system_NNLs$NNL_yrs)


# write.csv(...)
# write.csv2(...)
global_params.data_fr = as.data.frame(unlist(global_params))


stopCluster(cl)

