rm(list = ls())
WD = getwd()
strt<-Sys.time()

source('~/Documents/R_Codes/Offsets_Working_FEB_3/offsets_functions_current.R')
source('~/Documents/R_Codes/Offsets_Working_FEB_3/Global_Regional_Params.R')

.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

write_pdf = FALSE
print_folder = '~/Documents/'

realisation_num = 100

offset_calc_type = 'restoration_from_counterfactual'   #'avoided degredation', 'restoration_from_counterfactual', 'parcel_condition_value', 'restoration_gains', 'restoration_condition_value'
dev_calc_type = 'future_condition'                     #'future_condition', 'current_condition' 
offset_action_type = 'restore'                         #'protect', 'maintain', 'restore'

global_params <- initialise_global_params()
parcels <- initialise_shape_parcels(global_params)
land_parcels <- parcels$land_parcels
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, land_parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, condition_change = 'decline', mean_decline_rate = 0.03, decline_rate_std = 0.01, global_params$eco_dims)
cfacs <- build_counterfactuals_by_parcel_multi(global_params, decline_rates_initial, 1:(parcels$land_parcel_num), land_parcels, initial_ecology, (time_horizon = global_params$time_steps - 1))
cfac_parcel_trajs <- find_parcel_traj_by_list(cfacs, global_params$time_steps)

region_params = list()
region_params[[1]] <- populate_region_list(offset_parcel_selection_type = 'regional', offset_calc_type, offset_action_type, 
                                 offset_parcel_for_parcel = TRUE, offset_multiplier = 1, dev_calc_type)

cl<-makeCluster(4)
registerDoParallel(cl)



record_parcel_sets = TRUE

realisations <- foreach(run_ind = 1:realisation_num) %dopar%{
  run_offsets_model(global_params, region_params, cfacs, cfac_parcel_trajs, offset_calc_type, current_ecology = initial_ecology, 
                    decline_rates = decline_rates_initial, parcels, perform_offsets = TRUE, set_seed = FALSE)
}

stopCluster(cl)

collated_realisations <- collate_realisations(realisations, (assessed_set_indexes = 1:(global_params$total_dev_num)), global_params$time_steps, global_params$eco_dims)
counterfactual_sum = apply(cfac_parcel_trajs, MARGIN = 1, sum)



if (write_pdf == TRUE){
  filename = paste(print_folder, offset_calc_type , '_time_horizon_', global_params$offset_time_horizon, '_', global_params$use_offset_time_horizon, 
                   '_include_avoided_clearing_', global_params$use_adjusted_counterfactual, '.pdf', sep = '', collapse = '')
  pdf(filename)
}

generate_single_realisation_plots(offset_calc_type, global_params, realisations, counterfactual_sum, eco_ind = 1)
plot_collated_realisations(collated_realisations, realisation_num = realisation_num, counterfactual_sum, eco_ind = 1, lwd_vec = c(3, 0.15))

if (write_pdf == TRUE){
  dev.off()
}

fin <- Sys.time()

print(fin - strt)