rm(list = ls())
source('~/Documents/R Codes/Offsets/offsets_functions_multi.R')
source('~/Documents/R Codes/Offsets/Params_multi.R')

graphics.off()
global_params <- initialise_global_params()
region_params <- initialise_region_params(global_params$region_num, global_params$eco_dims, restoration_rate = 0.03, mean_decline_rates = c(0.03, 0.01))
parcels <- initialise_shape_parcels(global_params)
index_object <- initialise_index_object(parcels, global_params)
initial_ecologies <- initialise_ecologies(global_params, parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, region_params, global_params$eco_dims)

trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps)

outputs <- calc_trajectories_multi(trajectories, global_params, region_params, current_ecologies = initial_ecologies, decline_rates = decline_rates_initial, 
                                   parcels, index_object, perform_offsets = TRUE, record_parcel_sets = TRUE)

assessed_offsets <- assess_parcel_sets_multi(outputs$offsets, parcels$land_parcels, outputs$trajectories, global_params$time_steps, decline_rates_initial)
assessed_developments <- assess_parcel_sets_multi(outputs$developments, parcels$land_parcels, outputs$trajectories, global_params$time_steps, decline_rates_initial)

plot_parcel_set_dimensional(assessed_offsets, assessed_developments, 5)

#plot_net_parcel_sets(assess_object)
#plot_parcel_sets(assess_object, assess_type = 'set', parcel_set_num = 10)

# print(Sys.time()-strt)

# offset_sums <- find_sums(outputs$offset_list, outputs$trajectories, parcels, global_params$time_steps)
# dev_sums <- find_sums(outputs$development_list, outputs$trajectories, parcels, global_params$time_steps)
# 
# counter_sums <- find_sums(1:length(parcels$land_parcels), counterfactuals_object$counterfactuals, parcels, global_params$time_steps)
# 
# 
# true_offset_sums <- true_sums(outputs$offset_list, offset_sums, counter_sums)
# true_dev_sums <- true_sums(outputs$development_list, dev_sums, counter_sums)








#plot_outs(traj_sums$parcel_sums[, 1:6], traj_sums$parcel_sums[, 1:6], c(2, 3))