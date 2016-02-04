rm(list = ls())
source('~/Documents/R_Codes/Offsets_Working_FEB_3/offsets_functions_current.R')
source('~/Documents/R_Codes/Offsets_Working_FEB_3/Global_Regional_Params.R')
.libPaths('~/Library/R/3.2/library')
library(foreach)
library(doParallel)
library(abind)

graphics.off()
global_params <- initialise_global_params()
region_params <- initialise_region_params(global_params$region_num, restoration_rate = 0.03, mean_decline_rates = c(0.03, 0.01), global_params$time_steps, global_params$total_dev_num)
parcels <- initialise_shape_parcels(global_params)
index_object <- initialise_index_object(parcels, global_params)
initial_ecology <- initialise_ecology(global_params, parcels)
decline_rates_initial <- build_decline_rates_multi(parcels, region_params, global_params$eco_dims)

cl<-makeCluster(4)
registerDoParallel(cl)

strt<-Sys.time()
run_num = 100
record_parcel_sets = TRUE

realisations <- foreach(run_ind = 1:run_num) %dopar%{
  outputs <- calc_trajectories_multi(global_params, region_params, current_ecology = initial_ecology, decline_rates = decline_rates_initial, 
                                     parcels, index_object, perform_offsets = TRUE, record_parcel_sets)
}

stopCluster(cl)
parcel_ind = 1
collated_parcel_realisations <- collate_parcel_realisations_multi(record_parcel_sets, realisations, parcel_ind, parcels$land_parcels, global_params$time_steps, global_params$eco_dims)

# mean_traj <- Reduce('+', traj_runs)
# mean_traj = mean_traj/run_num
# 
# graphics.off()
# 
# 
# parcel_survival_prob <- sum_development_vec(region_params[[1]]$dev_nums, parcel_num = length(parcels$parcel_indexes))
# 
# 
# counterfactuals <- initialise_counterfactuals_vectorised(global_params, region_params, parcels$land_parcels, initial_ecology, decline_rates)
# counter_sums <- find_sums(1:length(parcels$land_parcels), counterfactuals, parcels, global_params$time_steps)
# 
# parcel_num = length(parcels$regions[[1]])
# tst = array(0, c(100, parcel_num))
# 
# for (parcel_ind in 1:parcel_num){
#   
#   parcel_reals = parcel_realisations(traj_runs, parcel_ind, parcels$land_parcels, global_params$time_steps)
#   parcel_survival_prob <- sum_development_vec(region_params[[1]]$dev_nums, parcel_num = length(parcels$parcel_indexes))
#   tst[, parcel_ind] = rowSums(parcel_reals$sums/run_num)/counter_sums$parcel_sums[, parcel_ind]
#   #tst[, parcel_ind] = rowSums(parcel_reals$sums/run_num)
#   print(parcel_ind)
# }
# 
# 
# graphics.off()
# mx = max(tst)
# mn = min(tst)
# plot(tst[, 1], type = 'l', ylim = c(mn, mx))
# 
# for (parcel_ind in 2:parcel_num){
#   lines(tst[, parcel_ind], ylim = c(mn, mx))
# }
# 

# print(Sys.time()-strt)


# offset_sums <- find_sums(outputs$offset_list, outputs$trajectories, parcels, global_params$time_steps)
# dev_sums <- find_sums(outputs$development_list, outputs$trajectories, parcels, global_params$time_steps)
# 
# true_offset_sums <- true_sums(outputs$offset_list, offset_sums, counter_sums)
# true_dev_sums <- true_sums(outputs$development_list, dev_sums, counter_sums)
# a_object <- assess_parcel_sets(outputs$parcel_sets, parcels$land_parcels, outputs$trajectories, counterfactuals_object$counterfactuals, global_params$time_steps)
# 
#plot_outs(traj_sums$parcel_sums[, 1:6], traj_sums$parcel_sums[, 1:6], c(2, 3))
