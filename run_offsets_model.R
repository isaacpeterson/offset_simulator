rm(list = ls())
WD = getwd()

source('~/Documents/R_Codes/Offsets_Working_FEB_3/offsets_functions_current.R')
source('~/Documents/R_Codes/Offsets_Working_FEB_3/Global_Regional_Params.R')

.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

global_params <- initialise_global_params()
region_params <- initialise_region_params(global_params$region_num, restoration_rate = 0.03, mean_decline_rates = c(0.03, 0.01), 
                                          offset_calc_type = 'current condition')

parcels <- initialise_shape_parcels(global_params)
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, region_params, global_params$eco_dims)
cfacs <- build_counterfactuals_by_parcel_multi(global_params, decline_rates_initial, 1:(parcels$land_parcel_num), parcels$land_parcels, initial_ecology, time_steps = global_params$time_steps)
cfac_parcel_trajs <- find_parcel_traj_by_list(cfacs[[1]])


outs_current_condition <- run_offsets_model(global_params, region_params, parcels, index_object, initial_ecology, decline_rates_initial)

outs_rest_from_counter <- run_offsets_model(global_params, region_params, parcels, index_object, initial_ecology, decline_rates_initial)

region_params <- initialise_region_params(global_params$region_num, restoration_rate = 0.03, mean_decline_rates = c(0.03, 0.01), 
                                          offset_calc_type = 'avoided degredation')

outs_avoided_degs <- run_offsets_model(global_params, region_params, parcels, index_object, initial_ecology, decline_rates_initial)

region_params <- initialise_region_params(global_params$region_num, restoration_rate = 0.03, mean_decline_rates = c(0.03, 0.01), 
                                          offset_calc_type = 'restoration gains')

outs_rest_gains <- run_offsets_model(global_params, region_params, parcels, index_object, initial_ecology, decline_rates_initial)

plot_single_net_regional(outs_current_condition$collated_parcel_sets_object, outs_current_condition$parcel_trajs, assessed_set_index = 1, global_params, cfac_parcel_trajs)

plot_single_net_regional(outs_rest_from_counter$collated_parcel_sets_object, outs_rest_from_counter$parcel_trajs, assessed_set_index = 1, global_params, cfac_parcel_trajs)
plot_single_net_regional(outs_avoided_degs$collated_parcel_sets_object, outs_avoided_degs$parcel_trajs, assessed_set_index = 1, global_params, cfac_parcel_trajs)
plot_single_net_regional(outs_rest_gains$collated_parcel_sets_object, outs_rest_gains$parcel_trajs, assessed_set_index = 1, global_params, cfac_parcel_trajs)

