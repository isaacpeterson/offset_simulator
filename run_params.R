params = list()
params$offset_bank_types = c('parcel_set', 'credit')       #'parcel_set' or 'credit'
params$use_offset_bank = c(TRUE, FALSE)
params$offset_parcel_for_parcel = c(TRUE, FALSE)
params$restoration_rate = c(0.01, 0.03)
params$offset_time_horizon = c(10, 20, 30)
params$offset_calc_type = c('current_condition', 'avoided_degredation', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value')   #'current_condition', 'avoided_degredation', 'restoration_from_cfac', 'future_condition', 'restoration_gains', 'restoration_condition_value'
params$dev_calc_type = c('current_condition', 'future_condition')                     #'future_condition', 'current_condition' 
#params$offset_action_type = c('protect', 'maintain', 'restore')                         #'protect', 'maintain', 'restore'
params$cfac_type_in_offset_calc = c('standard', 'include_clearing', 'include_clearing_offsets') #'standard', 'include_clearing', 'include_clearing_offsets'
params$cfac_type_in_dev_calc = cfac_type_in_offset_calc        #'standard_cfac', 'include_clearing', 'include_clearing_offsets'

param_inds <- lapply(seq_along(params), function(i) 1:length(params[[i]]))
param_combs <- expand.grid(param_inds)

for (comb_ind in seq_len(2)){
  current_inds = unlist(param_combs[comb_ind, ])
  current_comb = lapply(seq_along(params), function(i) params[[i]][current_inds[i]])
  print(current_comb)
}






offset_multiplier = 1
offset_time_horizon_type = 'current'                #'future' - project from time of development to offset time horizon, or 'current' - used for banking only - determine accrued offset gains till current year.
params$offset_bank_params = c(1, 10, 50) # parameters for offset banking in format (start_time, end_time, number_of_banked_parcels)

if (offset_calc_type == 'avoided_degredation'){
  offset_action_type = 'maintain'
} else if (offset_calc_type %in% c('restoration_from_cfac', 'restoration_gains', 'restoration_condition_value')){
  offset_action_type = 'restore'
}


