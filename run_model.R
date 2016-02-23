rm(list = ls())
WD = getwd()

source('~/Documents/R_Codes/Offsets_Working_FEB_3/offsets_functions_current.R')
source('~/Documents/R_Codes/Offsets_Working_FEB_3/Global_Regional_Params.R')

.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)


#offset_calc_type = 'avoided degredation', 'restoration_from_counterfactual', 'parcel_condition_value', 'restoration_gains', 'restoration_condition_value'
#dev_calc_type = 'future_condition', 'current_condition'

offset_calc_type = 'restoration_from_counterfactual'
dev_calc_type = 'future_condition'
offset_action_type = 'restore'

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

outs <- run_offsets_model(global_params, region_params, cfacs, cfac_parcel_trajs, offset_calc_type, current_ecology = initial_ecology, 
                          decline_rates = decline_rates_initial, parcels, perform_offsets = TRUE, set_seed = TRUE)


# outs <- run_offsets_model(global_params, region_params, cfacs, cfac_parcel_trajs, offset_calc_type, current_ecology = initial_ecology, 
#                           decline_rates = decline_rates_initial, parcels, perform_offsets = TRUE, set_seed = FALSE)

# offset_calc_type = 'restoration_from_counterfactual'
# region_params <- initialise_region_params(global_params$region_num, mean_decline_rates = c(0.03, 0.01), offset_calc_type, dev_calc_type)
# outs <- run_offsets_model(global_params, region_params, cfacs, cfac_parcel_trajs, offset_calc_type, current_ecology = initial_ecology, 
#                           decline_rates = decline_rates_initial, parcels, perform_offsets = TRUE, set_seed = TRUE)
# 
# 
#

# outs = list()
# eco_ind = 1
# outs$model_outputs <- calc_trajectories_multi(global_params, region_params, current_ecology = initial_ecology, decline_rates = decline_rates_initial, 
#                                               parcels, index_object, perform_offsets = TRUE, record_parcel_sets = TRUE)
# 
# parcel_sets_counterfactuals <- calc_parcel_set_counterfactuals(outs$model_outputs, global_params, decline_rates_initial)
#                                                               
# outs$parcel_sets_object <- group_parcel_sets(outs$model_outputs, parcel_sets_counterfactuals, counterfactual_type = 'counterfactual', land_parcels, global_params, decline_rates_initial)
# 
# outs$collated_parcel_sets_object <- collate_parcel_sets(outs$parcel_sets_object, global_params, global_params$eco_dims)
# 
# plot_all_parcel_sets(outs$collated_parcel_sets_object, assessed_set_indexes = (1:10), global_params)
# 
# 
# 
# outs$parcel_trajs <- find_parcel_trajectories(land_parcels, 1:(parcels$land_parcel_num), outs$model_outputs$trajectories[[eco_ind]])
# 
# par(mfrow = c(2, 1))
# par(mar = c(6, 4, 2, 0), oma = c(0, 0, 0, 0))
# NNL_object = outs$collated_parcel_sets_object$NNL_object
# xl = t(cbind(paste('mean = ', round(mean(NNL_object$NNL_dist))), paste('success = ', NNL_object$success )))
# hist(NNL_object$NNL_dist, main = '', xlab = xl)
# 
# hist(outs$collated_parcel_sets_object$offsets$initial_parcel_sums, main = '', xlab = 'selected parcel value')
# 





#plot_single_net_regional(outs$collated_parcel_sets_object, outs$parcel_trajs, assessed_set_index = 3, global_params, cfac_parcel_trajs)


# filename = paste('~/Documents/', offset_calc_type , '_time_horizon_', global_params$offset_time_horizon, '_', 
#                  global_params$use_offset_time_horizon, '_include_avoided_clearing_', global_params$use_adjusted_counterfactual,
#                  '.pdf', sep = '', collapse = '')
# pdf(filename)
# plot_all_parcel_sets(outs$collated_parcel_sets_object, outs$parcel_trajs, assessed_set_indexes = (1:10), global_params, cfac_parcel_trajs)
# plot_parcel_set_from_collated_object(outs$collated_parcel_sets_object, assessed_set_indexes = (1:global_params$total_dev_num), global_params, global_params$eco_dims)
# 
# setup_sub_plots(nx = 1, ny = 2)
# total_parcel_sums = apply(outs$parcel_trajs, MARGIN = 1, sum)
# total_counter_sums = apply(cfac_parcel_trajs, MARGIN = 1, sum)
# plot_array = cbind(t(t(total_parcel_sums)), t(t(total_counter_sums)))
# mx = max(plot_array)
# mn = min(plot_array)
# overlay_plots(plot_array, yticks = 'y', axis_lab = TRUE, ylims = c(mn, mx), (heading = "Net Ecological Value"), ylab = '', col_vec = c('black', 'black'), 
#               lty_vec = c(1, 2), lwd_vec = c(1, 1), legend_vec = c('Offset Policy Assessment', 'Landscape Decline'), legend_loc = 'topright')
# 
# plot((total_parcel_sums - total_counter_sums), type = 'l', main = "Net NNL Assessment", ylab = '')
# abline(h = 0, lty = 2)
# 
# dev.off()

# graphics.off()
# parcel_set_ind = 2
# mx = max(apply(parcel_sets_counterfactuals$offsets[[parcel_set_ind]]$counterfactuals[[1]][[1]], MARGIN = 3, sum))
# plot(apply(parcel_sets_counterfactuals$offsets[[parcel_set_ind]]$counterfactuals[[1]][[1]], MARGIN = 3, sum), type = 'l', ylim = c(0, mx))
# lines(apply(parcel_sets_counterfactuals$offsets[[parcel_set_ind]]$weighted_offset_projections[[1]][[1]], MARGIN = 3, sum), type = 'l')
# lines(apply(parcel_sets_counterfactuals$offsets[[parcel_set_ind]]$include_clearing[[1]][[1]], MARGIN = 3, sum), type = 'l')
# lines(apply(parcel_sets_counterfactuals$offsets[[parcel_set_ind]]$include_clearing_and_offsets[[1]][[1]], MARGIN = 3, sum), type = 'l', col = 'red')

