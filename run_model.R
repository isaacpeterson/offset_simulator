rm(list = ls())
WD = getwd()

source('~/Documents/R_Codes/Offsets_Working_FEB_3/offsets_functions_current.R')
source('~/Documents/R_Codes/Offsets_Working_FEB_3/Global_Regional_Params.R')

.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

global_params <- initialise_global_params()
region_params <- initialise_region_params(global_params$region_num, mean_decline_rates = c(0.03, 0.01), offset_calc_type = 'current condition')
parcels <- initialise_shape_parcels(global_params)
land_parcels <- parcels$land_parcels
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, land_parcels)

decline_rates_initial <- build_decline_rates_multi(parcels, region_params, global_params$eco_dims)
cfacs <- build_counterfactuals_by_parcel_multi(global_params, decline_rates_initial, 1:(parcels$land_parcel_num), land_parcels, initial_ecology, (time_horizon = global_params$time_steps - 1))
cfac_parcel_trajs <- find_parcel_traj_by_list(cfacs[[1]])
#adjusted_cfac_trajs = adjust_counterfactual_post_facto(cfacs, global_params, decline_rates = decline_rates_initial, parcel_indexes = 1:(parcels$land_parcel_num), parcels, current_ecology = initial_ecology)

#outs <- run_offsets_model(global_params, region_params, parcels, index_object, initial_ecology, decline_rates_initial)
outs = list()
eco_ind = 1
outs$model_outputs <- calc_trajectories_multi(global_params, region_params, current_ecology = initial_ecology, decline_rates = decline_rates_initial, 
                                              parcels, index_object, perform_offsets = TRUE, record_parcel_sets = TRUE)

outs$parcel_sets_object <- group_parcel_sets(outs$model_outputs, land_parcels, global_params, decline_rates_initial)
outs$collated_parcel_sets_object <- collate_parcel_sets(outs$parcel_sets_object, global_params, global_params$eco_dims)
outs$parcel_trajs <- find_parcel_trajectories(land_parcels, 1:(parcels$land_parcel_num), outs$model_outputs$trajectories[[eco_ind]])

plot_single_net_regional(outs$collated_parcel_sets_object, outs$parcel_trajs, assessed_set_index = 5, global_params, cfac_parcel_trajs)

