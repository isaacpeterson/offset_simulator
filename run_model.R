#setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
rm(list = ls())
WD = getwd()
strt<-Sys.time()

source('offsets_functions_current.R')
source('Global_Regional_Params.R')

.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

write_pdf = TRUE
print_folder = '~/Documents/'

realisation_num = 100

offset_calc_type = 'current_condition'   #'avoided degredation', 'restoration_from_cfac', 'parcel_condition_value', 'restoration_gains', 'restoration_condition_value'
dev_calc_type = 'current_condition'                     #'future_condition', 'current_condition' 
offset_action_type = 'restore'                         #'protect', 'maintain', 'restore'
cfac_type_in_offset_calc = 'include_clearing'               #'standard_cfac', 'include_clearing', 'include_clearing_offsets'
offset_time_horizon = 40

global_params <- initialise_global_params(offset_calc_type, dev_calc_type, offset_action_type, cfac_type_in_offset_calc , offset_time_horizon)
parcels <- initialise_shape_parcels(global_params)
land_parcels <- parcels$land_parcels
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, land_parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, condition_change = 'decline', mean_decline_rate = 0.02, decline_rate_std = 0.01, global_params$eco_dims)

region_params = list()
region_params[[1]] <- populate_region_list(offset_parcel_selection_type = 'regional',  offset_parcel_for_parcel = TRUE, offset_multiplier = 1)

cfacs <- build_cfacs_by_parcel_multi(global_params, decline_rates_initial, 1:(parcels$land_parcel_num), land_parcels, initial_ecology, (time_horizon = global_params$time_steps - 1))
cfac_parcel_trajs <- sum_parcel_trajectories(cfacs, global_params$eco_dims, parcel_indexes = 1:(parcels$land_parcel_num), time_horizon = global_params$time_steps)


cl<-makeCluster(4)
registerDoParallel(cl)

realisations <- foreach(run_ind = 1:realisation_num) %dopar%{
  run_offsets_model(global_params, region_params, initial_ecology, cfac_parcel_trajs, decline_rates_initial, parcels)
}

collated_realisations <- collate_realisations(realisations, (parcel_set_indexes = 1:(global_params$total_dev_num)), global_params$time_steps, global_params$eco_dims)

plot_characteristics = paste(offset_calc_type, '_', offset_action_type, '_', cfac_type_in_offset_calc,  sep = '', collapse = '')

if (global_params$use_offset_time_horizon == TRUE){                                   
  plot_characteristics = paste(plot_characteristics, '_time_horizon_', global_params$offset_time_horizon, sep = '', collapse = '')
}
 
if (write_pdf == TRUE){
  filename = paste(print_folder, plot_characteristics, '.pdf', sep = '', collapse = '')
  pdf(filename)
}
                 
plot_collated_realisations(collated_realisations, realisation_num = realisation_num, parcel_sum_lims = c(0, 20000), eco_ind = 1, lwd_vec = c(3, 0.15), outer_title = plot_characteristics)

if (write_pdf == TRUE){
  dev.off()
}

fin <- Sys.time()
print(fin - strt)

stopCluster(cl)


