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

write_pdf = FALSE
print_folder = '~/Documents/'

realisation_num = 5

offset_calc_type = 'restoration_from_cfac'   #'avoided degredation', 'restoration_from_cfac', 'parcel_condition_value', 'restoration_gains', 'restoration_condition_value'
dev_calc_type = 'future_condition'                     #'future_condition', 'current_condition' 
offset_action_type = 'restore'                         #'protect', 'maintain', 'restore'
cfac_type = 'standard_cfac'               #'standard_cfac', 'include_clearing', 'include_clearing_offsets'
offset_time_horizon = 40

global_params <- initialise_global_params(offset_calc_type, dev_calc_type, offset_action_type, cfac_type, offset_time_horizon)
parcels <- initialise_shape_parcels(global_params)
land_parcels <- parcels$land_parcels
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, land_parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, condition_change = 'decline', mean_decline_rate = 0.02, decline_rate_std = 0.01, global_params$eco_dims)
cfacs <- build_cfacs_by_parcel_multi(global_params, decline_rates_initial, 1:(parcels$land_parcel_num), land_parcels, initial_ecology, (time_horizon = global_params$time_steps - 1))
cfac_parcel_trajs <- sum_parcel_trajectories(cfacs, eco_dims, parcel_indexes = 1:(parcels$land_parcel_num), time_horizon = global_params$time_steps)

region_params = list()
region_params[[1]] <- populate_region_list(offset_parcel_selection_type = 'regional',  offset_parcel_for_parcel = TRUE, offset_multiplier = 1)


outs = list()

outs$model_outputs <- run_system(global_params, region_params, current_ecology = initial_ecology, decline_rates = decline_rates_initial, parcels, index_object)

outs$parcel_sets_cfacs <- calc_parcel_set_cfacs(outs$model_outputs, global_params, decline_rates_initial)

outs$collated_parcel_sets_object <- collate_parcel_sets_object(outs$model_outputs, outs$parcel_sets_cfacs, land_parcels, global_params, decline_rates_initial)

outputs = outs$model_outputs
traj_list = build_traj_list(outputs$trajectories, land_parcels, parcel_indexes = 1:length(land_parcels), eco_dims)

# plot(outs$parcel_sets_object$offsets[[1]]$avoided_degs_include_clearing, col = 'red', type = 'l')
# lines(outs$parcel_sets_object$offsets[[1]]$avoided_degs_include_clearing_offsets, col = 'blue')
# lines(outs$parcel_sets_object$offsets[[1]]$avoided_degs)
# 
# plot(apply(outs$parcel_sets_cfacs$offsets$adjusted_cfacs[[1]]$include_clearing[[1]][[1]], MARGIN = 3, sum), type = 'l', col = 'blue')
# lines(apply(outs$parcel_sets_cfacs$offsets$adjusted_cfacs[[1]]$include_clearing_offsets[[1]][[1]], MARGIN = 3, sum), col = 'red')
# lines(apply(outs$parcel_sets_cfacs$offsets$cfacs[[1]][[1]][[1]], MARGIN = 3, sum))
# 


# 
# cl<-makeCluster(4)
# registerDoParallel(cl)
# 
# realisations <- foreach(run_ind = 1:realisation_num) %dopar%{
#   run_offsets_model(global_params, region_params, current_ecology = initial_ecology, decline_rates = decline_rates_initial, parcels)
# }
# 
# 
# collated_realisations <- collate_realisations(realisations, (assessed_set_indexes = 1:(global_params$total_dev_num)), global_params$time_steps, global_params$eco_dims)
# 
# # 
# # 
# 
# # if (write_pdf == TRUE){
# #   filename = paste(print_folder, offset_calc_type , '_time_horizon_', global_params$offset_time_horizon, '_', global_params$use_offset_time_horizon, 
# #                    '_include_clearing_', global_params$use_adjusted_cfac, '_include_offsets_', global_params$include_offsets_in_adjusted_cfac, '_offset_action_', offset_action_type, '.pdf', sep = '', collapse = '')
# #   pdf(filename)
# # }
# # 
# cfac_sum = apply(cfac_parcel_trajs, MARGIN = 1, sum)
# parcel_sums_object = find_current_parcel_sums(land_parcels, current_ecology = initial_ecology, parcel_indexes)
# parcel_sums_lims = c(0, max(parcel_sums_object$parcel_sums))
# 
# generate_single_realisation_plots(global_params, realisations, cfac_sum, eco_ind = 1)
# plot_collated_realisations(collated_realisations, realisation_num = realisation_num, cfac_sum, parcel_sums_lims, eco_ind = 1, lwd_vec = c(3, 0.15))
# 
# # 
# # if (write_pdf == TRUE){
# #   dev.off()
# # }
# # 
# fin <- Sys.time()
# print(fin - strt)
# 
# 
# 
# stopCluster(cl)


