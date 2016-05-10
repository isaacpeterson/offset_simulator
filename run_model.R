setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
rm(list = ls())
WD = getwd()
strt<-Sys.time()

source('run_system_routines.R')
source('collate_routines.R')
source('plot_routines.R')
source('Global_Regional_Params.R')

#.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

write_pdf = FALSE
print_folder = '~/Documents/'

realisation_num = 30

match_type = 'parcel_set'
use_offset_bank = FALSE
offset_time_horizon_type = 'current'                #'future', or 'current'
offset_calc_type = 'restoration_from_cfac'   #'current_condition', 'avoided_degredation', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
dev_calc_type = 'future_condition'                     #'future_condition', 'current_condition' 
offset_action_type = 'restore'                         #'protect', 'maintain', 'restore'
cfac_type_in_offset_calc = 'include_clearing'        #'standard', 'include_clearing', 'include_clearing_offsets'
cfac_type_in_dev_calc = cfac_type_in_offset_calc        #'standard_cfac', 'include_clearing', 'include_clearing_offsets'

offset_time_horizon = 20

global_params <- initialise_global_params(match_type, use_offset_bank, offset_time_horizon_type, offset_calc_type, dev_calc_type, offset_action_type, cfac_type_in_offset_calc, offset_time_horizon)
parcels <- initialise_shape_parcels(global_params)
land_parcels <- parcels$land_parcels
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, land_parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, condition_change = 'decline', mean_decline_rate = 0.02, decline_rate_std = 0.005, global_params$eco_dims)

region_params = list()
region_params[[1]] <- populate_region_list(offset_parcel_selection_type = 'regional',  offset_parcel_for_parcel = TRUE)


cl<-makeCluster(4)
registerDoParallel(cl)

realisations <- foreach(run_ind = 1:realisation_num) %dopar%{
  run_offsets_model(global_params, region_params, initial_ecology, decline_rates_initial, parcels)
}


offset_success_flag = unlist(lapply(seq_along(realisations), function(i) realisations[[i]]$offset_success_flag))

if (all(offset_success_flag == TRUE)){
  collated_realisations <- collate_realisations(realisations, global_params, decline_rates_initial, land_parcels, initial_ecology)
  
  plot_characteristics = paste('offset_bank_', use_offset_bank, '_', offset_calc_type, '_', offset_action_type, '_', cfac_type_in_offset_calc,  '_cfac', sep = '', collapse = '')
  
  if (global_params$use_offset_time_horizon == TRUE){                                   
    plot_characteristics = paste(plot_characteristics, '_time_horizon_', global_params$offset_time_horizon, sep = '', collapse = '')
  }
  
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


stopCluster(cl)


