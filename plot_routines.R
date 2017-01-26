# parcel_sum_lims = c(0, 20000) 
# eco_ind = 1 
# lwd_vec = c(3, 0.15) 
# edge_title = plot_characteristics
# realisation_num = length(realisations)

plot_collated_realisations <- function(collated_realisations, realisation_num, global_params, program_params, 
                                       parcel_sum_lims, eco_ind, lwd_vec, edge_title){

  offset_col_vec = c('blue', 'red', 'mediumorchid4', 'darkgreen')
  dev_col_vec = c('blue', 'red')
  net_col_vec = c('darkgreen', 'red', 'mediumorchid4', 'black')
  
  time_horizon = global_params$time_steps
  eco_dims = global_params$eco_dims
  
  if (program_params[[1]]$use_parcel_sets == TRUE){
    setup_sub_plots(nx = 3, ny = 3, x_tit = TRUE)
    plot_parcel_sets(collated_realisations, 
                     realisation_ind = 1, 
                     eco_ind, 
                     col_list = list(offset_col_vec, dev_col_vec, net_col_vec), 
                     legend_loc = 'topleft')
  }
  
  setup_sub_plots(nx = 3, ny = 3, x_tit = TRUE)
  
  system_NNL = collated_realisations$system_NNL
  
  parcel_set_NNL = collated_realisations$parcel_set_NNL
  
  plot_lims <- find_plot_lims(plot_list = list(collated_realisations$net_offset_gains$net_outcome, 
                                               collated_realisations$net_dev_losses$net_outcome,
                                               collated_realisations$net_program_outcomes,
                                               collated_realisations$net_offset_gains$losses))
 
  overlay_realisations(plot_list = list(collated_realisations$net_offset_gains$rest_gains,
                                        collated_realisations$net_offset_gains$avoided_degs,
                                        collated_realisations$net_offset_gains$losses,
                                        collated_realisations$net_offset_gains$net_outcome),
                       plot_title = 'Offset Evaluation', 
                       x_lab = '',
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = offset_col_vec, 
                       legend_loc = 'topleft',
                       legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Illegal Clearing', 'Net Offset Gains'), 
                       edge_title, 
                       plot_lims = plot_lims) 
  
  overlay_realisations(plot_list = list(collated_realisations$net_dev_losses$rest_gains,
                                        collated_realisations$net_dev_losses$net_outcome),
                       plot_title = 'Development Evaluation', 
                       x_lab = '',
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = dev_col_vec, 
                       legend_loc = 'topleft',
                       legend_vec = c('Absolute Losses', 'Relative Losses'), 
                       edge_title, 
                       plot_lims = plot_lims) 

  overlay_realisations(plot_list = list(collated_realisations$net_offset_gains$net_outcome, 
                            collated_realisations$net_dev_losses$net_outcome,
                            collated_realisations$net_offset_gains$losses,
                            collated_realisations$net_program_outcomes),
                            plot_title = 'Net Program Evaluation', 
                            x_lab = system_NNL$x_lab,
                            realisation_num,
                            eco_ind, 
                            lwd_vec, 
                            col_vec = net_col_vec, 
                            legend_loc = 'topleft',
                            legend_vec = c('Offset Gains', 'Development Losses', 'Illegal Clearing', 'Program Evaluation'), 
                            edge_title, 
                            plot_lims = plot_lims)
  
  plot_realisation_hists(collated_realisations$system_NNL, collated_realisations$parcel_set_NNL, 
                         collated_realisations$offsets$parcel_sums_at_offset, parcel_sum_lims = c(0, 20000), 
                         use_parcel_sets = program_params[[1]]$use_parcel_sets, eco_ind, edge_title)
  
  plot_landscape_outcomes(collated_realisations$program_sums$outcome_rel_initial, 
                          plot_type = 'program', 
                          plot_title = 'Program Outcome', 
                          loss_object = collated_realisations$net_program_loss, 
                          realisation_num, 
                          collated_realisations$program_cfac_sum_rel_initial,
                          eco_ind, 
                          lwd_vec, 
                          col_vec = c('red', 'blue'), 
                          legend_vec = c('Program Outcome', 'Program Counterfactual'), 
                          edge_title, 
                          time_steps = global_params$time_steps)


#   plot_collated_realisation_set(collated_realisations$landscape_rel_to_counter, overlay_plots = FALSE, plot_col = 'black', realisation_num, eco_ind, lwd_vec, 
#                                 x_lab = '', plot_title = 'Program Outcome', plot_lims = vector())
# 
  plot_landscape_outcomes(collated_realisations$net_landscape, 
                          plot_type = 'landscape', 
                          plot_title = 'Landscape Outcome', 
                          loss_object = collated_realisations$landscape_loss, 
                          realisation_num, 
                          collated_realisations$net_cfac_sum, 
                          eco_ind, lwd_vec, 
                          col_vec = c('red', 'blue'), 
                          legend_vec = c('Landscape Outcome', 'Landscape Counterfactual'), 
                          edge_title, 
                          time_steps = global_params$time_steps)
  
  if (global_params$perform_illegal_clearing == TRUE){
    plot_list = list(collated_realisations$illegal_sum_rel_to_counter, collated_realisations$landscape_rel_to_counter)
    legend_vec = c('Illegal Ilearing Losses', 'Relative Landscape Losses')
  } else {
    plot_list = list(collated_realisations$landscape_rel_to_counter)
    legend_vec = c('Relative Landscape Losses')
  }

  overlay_realisations(plot_list,
                       plot_title = 'Landscape Evaluation', 
                       x_lab = '',
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = c('blue', 'red'),
                       legend_loc = 'topright',
                       legend_vec, 
                       edge_title, 
                       plot_lims = c(min(unlist(plot_list)), (max(unlist(plot_list))) + 0.25*max(abs(unlist(plot_list))))) 
  
}



get_plot_characteristics <- function(program_params, realisations){
#   plot_characteristics = paste(program_params[[1]]$offset_calc_type, '_', program_params[[1]]$dev_calc_type, '_', program_params[[1]]$cfac_type_in_offset_calc,  '_cfac_offset_bank_', 
#                                program_params[[1]]$use_offset_bank, '_', sep = '', collapse = '')
#   
#   if (program_params[[1]]$use_offset_bank == TRUE){                                   
#     plot_characteristics = paste(plot_characteristics, program_params[[1]]$offset_bank_start, '_', program_params[[1]]$offset_bank_end, '_', 
#                                  program_params[[1]]$offset_bank_num, '_', program_params[[1]]$match_type, sep = '', collapse = '')
#   }
#   
#   plot_characteristics = paste(plot_characteristics, '_', program_params[[1]]$offset_action_type, '_', sep = '', collapse = '')
#   if (program_params[[1]]$offset_action_type == 'restore'){
#     plot_characteristics = paste(plot_characteristics, program_params[[1]]$restoration_rate, '_', sep = '', collapse = '')
#   }
#   
#   if (program_params[[1]]$use_offset_time_horizon == TRUE){                                   
#     plot_characteristics = paste(plot_characteristics, '_time_horizon_', program_params[[1]]$offset_time_horizon, sep = '', collapse = '')
#   }
  
                
  plot_characteristics = vector()     
  if (program_params[[1]]$use_offset_time_horizon == TRUE){                                   
    plot_characteristics = paste(plot_characteristics, 'time_horizon_', program_params[[1]]$offset_time_horizon, sep = '', collapse = '')
  }
  plot_characteristics = paste(plot_characteristics, '_offsets_illegal_clearing_', program_params[[1]]$include_illegal_clearing_in_offset_calc, sep = '', collapse = '')
  
  plot_characteristics = paste(plot_characteristics, '_offsets_potential_developments_', program_params[[1]]$include_potential_developments_in_offset_calc, sep = '', collapse = '')
  
  plot_characteristics = paste(plot_characteristics, '_offsets_potential_offsets_', program_params[[1]]$include_potential_offsets_in_offset_calc, sep = '', collapse = '')
  
  plot_characteristics = paste(plot_characteristics, '_devs_illegal_clearing_', program_params[[1]]$include_illegal_clearing_in_dev_calc, sep = '', collapse = '')
  
  plot_characteristics = paste(plot_characteristics, '_devs_potential_developments_', program_params[[1]]$include_potential_developments_in_dev_calc, sep = '', collapse = '')
  
  plot_characteristics = paste(plot_characteristics, '_devs_potential_offsets_', program_params[[1]]$include_potential_offsets_in_dev_calc, sep = '', collapse = '')
  
  
  mean_dev_num = assess_allowed_devs(realisations)
  
  plot_characteristics = paste(plot_characteristics, '_mean_dev_num_', mean_dev_num, sep = '', collapse = '')
  
  return(plot_characteristics)
}




plot_split_realisations <- function(plot_type, rest_gains, avoided_degs, nets, plot_title, edge_title, eco_ind, lwd_vec, col_vec, legend_vec, legend_pos, realisation_num, ylim){

  plot_num = length(col_vec)
  plot_collated_realisation_set(rest_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)
  if (plot_type == 'offsets'){
    plot_collated_realisation_set(avoided_degs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)
  }
  plot_collated_realisation_set(nets, overlay_plots = TRUE, plot_col = col_vec[plot_num], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)  
  legend(legend_pos, legend_vec, bty="n", lty = rep(2, plot_num), lwd = array(lwd_vec[1], plot_num), col = col_vec)
  title(edge_title, outer = TRUE)
}



plot_collated_realisation_set <- function(current_collated_real, overlay_plots, plot_col, realisation_num, eco_ind, lwd_vec, x_lab, plot_title, plot_lims){
  if (plot_col == 'blue'){
    back_plot_col = 'skyblue'
  } else if (plot_col == 'black'){
    back_plot_col = 'gray40'
  } else if (plot_col == 'red'){
    back_plot_col = 'darkorange'
  } else if (plot_col == 'mediumorchid4'){
    back_plot_col = 'mediumorchid1'
  } else if (plot_col == 'darkgreen'){
    back_plot_col = 'green'
  }
  
  if (length(plot_lims) == 0){
    mn = min(unlist(current_collated_real))
    mx = max(unlist(current_collated_real))
  } else {
    mn = plot_lims[1]
    mx = plot_lims[2]
  }
  
  realisation_sum = nested_list_sum(current_collated_real)
  realisation_mean = realisation_sum[[eco_ind]]/realisation_num
  if (overlay_plots == FALSE){
    plot(current_collated_real[[1]][[eco_ind]], type = 'l', ylab = '', main = plot_title, xlab = x_lab, ylim = c(mn, mx), col = back_plot_col, lwd = lwd_vec[2])
  } else { lines(current_collated_real[[1]][[eco_ind]], lwd = lwd_vec[2], col = back_plot_col)
  }
  
  if (realisation_num > 1){
    for (realisation_ind in 2:realisation_num){
      lines(current_collated_real[[realisation_ind]][[eco_ind]], col = back_plot_col, lwd = lwd_vec[2])
    }
  }
  lines(realisation_mean, ylim = c(mn, mx), col = plot_col, lwd = lwd_vec[1], lty = 2)
  abline(h = 0, lty = 2)
  
}



generate_single_realisation_plots <- function(global_params, realisations, net_cfac_sum, eco_ind){
  time_horizon = global_params$time_steps
  eco_dims = global_params$eco_dims
  realisation_ind = sample(length(realisations), 1)
  collated_parcel_sets_object = realisations[[realisation_ind]]$collated_parcel_sets_object
  # plot_sample_parcel_sets(collated_parcel_sets_object, plot_num = 9, global_params)
  
  setup_sub_plots(nx = 1, ny = 3, x_tit = FALSE)
  
  plot_parcel_set_from_collated_object(collated_parcel_sets_object, parcel_set_indexes = (1:global_params$total_dev_num), time_horizon, global_params$eco_dims, 
                                       headings = c('Single Realisation Net Program Developments', 'Single Realisation Net Program Offsets', 'Single Realisation Net Program Outcomes'))
  
  parcel_trajs <- sum_parcel_trajectories(collated_parcel_sets_object$traj_list, eco_dims, parcel_indexes = 1:(parcels$land_parcel_num), time_horizon)
  
  
  par(mfrow = c(2, 1))
  par(mar = c(6, 4, 2, 0), oma = c(0, 0, 0, 0))
  NNL_object = collated_parcel_sets_object$NNL_object
  success_NNL = which(NNL_object$NNL_yrs > 0)
  
  if (length(success_NNL) > 0){
    NNL_success_yrs = NNL_object$NNL_yrs[success_NNL]
    xl = t(cbind(paste('parcel set mean years to NNL = ', round(mean(NNL_success_yrs))), paste('NNL success = ', round(NNL_object$success*100), '%' )))
    hist(NNL_success_yrs, main = 'Single Realisation NNL frequencies', xlab = xl)
  }
  
  hist(collated_parcel_sets_object$offsets$parcel_sums_at_offset, main = 'Single Realisation Selected Parcel Values', xlab = 'Selected offset parcel values')
  setup_sub_plots(nx = 3, ny = 1, x_tit = FALSE)
  total_parcel_sums = apply(parcel_trajs, MARGIN = 1, sum)
  total_counter_sums = apply(parcel_cfac_trajs, MARGIN = 1, sum)
  plot_array = cbind(t(t(total_parcel_sums)), t(t(total_counter_sums)))
  mx = max(plot_array)
  mn = min(plot_array)
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  overlay_plots_as_vector(plot_array, yticks = 'y', axis_lab = TRUE, x_lab = '', ylims = c(mn, mx), (heading = "Single Realisation Landscape Scale Outcome"), ylab = '', col_vec = c('red', 'blue'), 
                          lty_vec = c(1, 1), lwd_vec = c(3, 3), legend_vec = c('Offset Policy Assessment', 'Landscape Decline'), legend_loc = 'topright')
  
  net_cond = total_parcel_sums - total_counter_sums
  
  if (length(NNL_object$system_NNL) > 0 ){
    x_lab = paste('Mean System NNL = ', round(NNL_object$system_NNL), 'years')
  } else {x_lab = 'NNL fail'}
  
  plot((net_cond), type = 'l', main = 'Landscape Condition Relative to cfac', xlab = x_lab, ylab = '', col = 'red', lwd = 3)
  abline(h = 0, lty = 2)
  
}


# NNL_plot_object = parcel_set_NNL
# plot_tit = 'Parcel Set NNL Assessment'
# x_lim = c(0, 100)


plot_NNL_hist <- function(NNL_plot_object, plot_tit, x_lim, eco_ind){
  
  if (NNL_plot_object$NNL_success[[eco_ind]] > 0){
    x_lab = t(cbind( paste('Mean NNL at  ', round(NNL_plot_object$NNL_mean[[eco_ind]]), 'years'), paste('NNL success = ', round(NNL_plot_object$NNL_success[[eco_ind]]*100), '%' )))
    NNL_yrs = unlist(NNL_plot_object$NNL_yrs)
    NNL_yrs <- NNL_yrs[which(NNL_yrs > 0)]
    hist(NNL_yrs, main = plot_tit, xlab = x_lab, xlim = x_lim, breaks=seq(min(NNL_yrs),max(NNL_yrs),by=1))
    
  } else {
    null_plot()
  }
  
}




plot_mean_gains_degs <- function(summed_realisations, program_params, edge_title, realisation_num){
  
  rest_gains = sum_cols_multi(summed_realisations$rest_gains)/realisation_num
  net_degs = sum_cols_multi(summed_realisations$avoided_degs)/realisation_num
  net_gains = net_degs + rest_gains
  
  if (program_params[[1]]$adjust_cfacs_flag == TRUE){
    net_degs_adjusted = sum_cols_multi(summed_realisations$avoided_degs_adjusted)/realisation_num
    net_gains_adjusted = net_degs_adjusted + rest_gains
    adjusted_contribution = net_degs_adjusted - net_degs
    
  } else{
    net_gains_adjusted = vector()
    adjusted_contribution = vector()
  }
  
  plot_list = list(net_gains, net_gains_adjusted, net_degs, adjusted_contribution, rest_gains)
  
  plot_array = unlist(plot_list)
  mx = max(plot_array)
  mn = min(plot_array)
  
  col_vec = c('red', 'red', 'red', 'blue', 'black', 'darkgreen')
  lty_vec = c(1, 2, 3, 1, 2, 3)
  
  for (plot_ind in 1:length(plot_list)){
    if (plot_ind == 1){
      plot(plot_list[[plot_ind]], type = 'l', lty = lty_vec[plot_ind], lwd = 2, col = col_vec[plot_ind], ylim = c(mn, mx))
    } else {
      lines(plot_list[[plot_ind]], lty = lty_vec[plot_ind], lwd = 2, col = col_vec[plot_ind], ylim = c(mn, mx))
    }
  }
  
  #   legend_vec = c('net outcome', 'net outcome including clearing', 'net outcome including clearing and offsets', 'avoided degredation', 
  #                  'clearing contribution', 'clearing and offsets contribution', 'restoration gains')
  #   legend('topleft', legend_vec, bty="n", lty = lty_vec, col = col_vec, lwd = 2)
  
}




# collated_summed_reals = split_list[[1]]
# cfac_type = program_params[[1]]$cfac_type_in_offset_calc
# plot_type = 'offsets'
# legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains')
# legend_pos = 'topleft'
# plot_title = 'Net Offsets'
# lwd_vec = c(3, 0.15)
# edge_title = plot_characteristics
# col_vec = c('blue', 'mediumorchid4', 'darkgreen')
# ylim = c(net_mn, net_mx)


# plot_split_realisations <- function(collated_summed_reals, cfac_type, plot_type, legend_vec, legend_pos, eco_ind, lwd_vec, plot_title, edge_title, col_vec, realisation_num, ylim){
#   
#   rest_gains = collated_summed_reals$rest_gains
#   
#   if (cfac_type == 'standard'){
#     avoided_degs = collated_summed_reals$avoided_degs
#     nets = collated_summed_reals$net_outcome$standard
#   } else if ((cfac_type == 'include_clearing') || (cfac_type == 'include_clearing_offsets')){
#     avoided_degs = collated_summed_reals$avoided_degs_adjusted
#     nets = collated_summed_reals$net_outcome$adjusted
#   } 
#   
#   plot_summed_realisations(plot_type, rest_gains, avoided_degs, nets, plot_title, eco_ind, lwd_vec, col_vec, legend_vec, legend_pos, realisation_num, ylim)
#   title(edge_title, outer = TRUE)
# }

null_plot <- function(){
  plot(NULL, type= 'n', xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
}
# 
# system_NNL = collated_realisations$system_NNL
# parcel_set_NNL = collated_realisations$parcel_set_NNL 
# parcel_sums_at_offset = collated_realisations$offsets$parcel_sums_at_offset
# parcel_sum_lims = c(0, 20000)
# use_parcel_sets = program_params[[1]]$use_parcel_sets



# plot_realisation_hists(collated_realisations$system_NNL, collated_realisations$parcel_set_NNL, 
#                        collated_realisations$offsets$parcel_sums_at_offset, parcel_sum_lims = c(0, 20000), 
#                        use_parcel_sets = program_params[[1]]$use_parcel_sets, parcel_set_set_NNL_object, eco_ind, edge_title)

plot_parcel_sums_hist <- function(parcel_sums_at_offset, eco_ind, parcel_sum_lims){
  current_parcel_sums_at_offset = unlist(parcel_sums_at_offset, recursive = FALSE)
  parcel_sums_at_offset_array = unlist(lapply(seq_along(current_parcel_sums_at_offset), function(i) current_parcel_sums_at_offset[[i]][[eco_ind]]))
  hist(parcel_sums_at_offset_array, main = 'offset parcel values', xlab = 'selected offset parcel values', xlim = parcel_sum_lims)
}

plot_realisation_hists <- function(system_NNL, parcel_set_NNL, parcel_sums_at_offset, parcel_sum_lims, 
                                   use_parcel_sets, eco_ind, edge_title){
  
  plot_parcel_sums_hist(parcel_sums_at_offset, eco_ind, parcel_sum_lims)
  plot_NNL_hist(system_NNL, plot_tit = 'System NNL Assessment', x_lim = c(0, 100), eco_ind)
  
  if (use_parcel_sets == TRUE){
    plot_NNL_hist(parcel_set_NNL, plot_tit = 'Parcel Set NNL Assessment', x_lim = c(0, 100), eco_ind) 
  } else {
    null_plot()
  }
  title(edge_title, outer = TRUE)
  
}



overlay_realisations <- function(plot_list, plot_title, x_lab, realisation_num, eco_ind, lwd_vec, col_vec, legend_vec, legend_loc, edge_title, plot_lims){
  
  if (length(plot_lims) == 0){
    plot_lims = find_plot_lims(plot_list)
  }
  
  for (plot_ind in seq_along(plot_list)){
    if (plot_ind == 1){
      overlay_plots = FALSE
    } else {
      overlay_plots = TRUE
    }
    plot_collated_realisation_set(plot_list[[plot_ind]], overlay_plots, plot_col = col_vec[plot_ind], realisation_num, eco_ind, lwd_vec, 
                                  x_lab = '', plot_title, plot_lims = plot_lims)
  }

  legend(legend_loc, legend_vec, bty="n", lty = c(2, 2, 2, 2), lwd = array(lwd_vec[1], 4), col = col_vec)
  title(edge_title, outer = TRUE)
}

# plot_realisation_outcomes <- function(offset_gains, dev_losses, net_outcome, plot_title, x_lab, realisation_num, eco_ind, lwd_vec, col_vec, legend_vec, edge_title, plot_lims){
#   
#   #setup_sub_plots(nx = 1, ny = 1, x_tit = TRUE)
#   if (length(plot_lims) == 0){
#     plot_lims = c(min(cbind(net_outcome, dev_losses, offset_gains)), max(cbind(net_outcome, dev_losses, offset_gains)))
#   }
# 
#   plot_collated_realisation_set(offset_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
#                                 x_lab = '', plot_title = '', plot_lims = plot_lims)
#   
#   plot_collated_realisation_set(dev_losses, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
#                                 x_lab = '', plot_title = '', plot_lims = plot_lims)
#   plot_collated_realisation_set(net_outcome, overlay_plots = TRUE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
#                                 x_lab = x_lab, plot_title = plot_title, plot_lims = plot_lims)
#   
#   legend('topleft', legend_vec, bty="n", lty = c(2, 2, 2), lwd = array(lwd_vec[1], 3), col = col_vec)
#   
#   title(edge_title, outer = TRUE)
# }

# plot_landscape_outcomes(collated_realisations$summed_program_realisations, plot_type = 'program', plot_title = 'Program Evaluation', loss_object = collated_realisations$net_program_loss, realisation_num, 
#                         collated_realisations$summed_program_cfacs, eco_ind, lwd_vec, col_vec = c('red', 'blue'), legend_vec = c('Program Value', 'Program Counterfactual'), edge_title, time_steps = global_params$time_steps)
# 
# landscape_realisations = collated_realisations$program_sums$net
# plot_type = 'program'
# plot_title = 'Program Evaluation'
# loss_object = collated_realisations$net_program_loss
# 
# cfacs = collated_realisations$program_cfac_sums$net
# col_vec = c('red', 'blue')
# legend_vec = c('Program Value', 'Program Counterfactual')



# plot_landscape_outcomes(collated_realisations$landscape_vals, plot_type = 'landscape', plot_title = 'Landscape Evaluation', loss_object = collated_realisations$landscape_loss, realisation_num, 
#                         net_cfac_sum, eco_ind, lwd_vec, col_vec = c('red', 'blue'), legend_vec = c('Landscape Value', 'Landscape Counterfactual'), edge_title, time_steps = global_params$time_steps)


# landscape_realisations = collated_realisations$landscape_vals
# plot_type = 'landscape'
# plot_title = 'Landscape Evaluation'
# loss_object = collated_realisations$landscape_loss
# cfacs = net_cfac_sum
# col_vec = c('red', 'blue')
# legend_vec = c('Landscape Value', 'Landscape Counterfactual')
# time_steps = global_params$time_steps
# net_cfac_sum = collated_realisations$net_cfac_sum

plot_landscape_outcomes <- function(landscape_realisations, plot_type, plot_title, loss_object, realisation_num, cfacs, eco_ind, lwd_vec, col_vec, legend_vec, edge_title, time_steps){
  
  current_total_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_object$total_loss[[i]][[eco_ind]]))
  loss_tit = paste('Net Loss at ', time_steps, 'yrs = ', round(mean(unlist(current_total_loss))*100), '%')
  
  current_NNL_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_object$NNL_loss[[i]][[eco_ind]]))
  if (length(current_NNL_loss) > 0){
    NNL_tit = paste('Mean NNL at  ', round(mean(current_NNL_loss*100)), '% landscape loss')
  } else {
    NNL_tit = 'All realisations failed NNL'
  }
  sub_tit = cbind(NNL_tit, loss_tit)
  
  if (plot_type == 'program'){
    current_cfacs <- lapply(seq_along(cfacs), function(i) cfacs[[i]][[eco_ind]])
  } else {
    current_cfacs = cfacs[[eco_ind]]
  }
  mx = max(c(max(unlist(landscape_realisations))), max(unlist(current_cfacs)))
  mn = min(c(min(unlist(landscape_realisations))), min(unlist(current_cfacs)))
  
  plot_collated_realisation_set(landscape_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = sub_tit, plot_title = plot_title, plot_lims = c(mn, mx))
  
  if (plot_type == 'program'){
    plot_collated_realisation_set(cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = c(mn, mx))
  } else {
    lines(cfacs[[eco_ind]], col = col_vec[2], lty = 2, lwd = 2)
  }
  
  #abline(h = mean(landscape_realisations[1, , ]), lty = 2)
  
  legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  title(edge_title, outer = TRUE) 
}




plot_program_evaluation <- function(summed_program_realisations, loss_object, realisation_num, summed_program_cfacs, eco_ind, lwd_vec, col_vec, legend_vec, edge_title){
  
  #setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  
  loss_tit = paste('Net program loss = ', round(mean(loss_object$total_loss)*100), '%')
  
  if (length(loss_object$NNL_loss) > 0){
    #landscape_title = paste('NNL at ', round(mean(loss)), '\U00b1', round(max(range(loss) - (mean(loss))), 1), '% loss of landscape')
    NNL_tit = paste('Mean NNL at  ', round(mean(loss_object$NNL_loss*100)), '% program loss')
  } else NNL_tit = 'All realisations failed NNL'
  
  sub_tit = cbind(NNL_tit, loss_tit)
  
  plot_collated_realisation_set(summed_program_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = sub_tit, plot_title = 'Net Program Value', plot_lims = c(0, max(summed_program_realisations)))
  plot_collated_realisation_set(summed_program_cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = vector())
  
  legend('bottomleft', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  
#   plot_collated_realisation_set( (summed_program_realisations - summed_program_cfacs), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind = 1, lwd_vec, 
#                                  x_lab = '', plot_title = 'Net Value Rel. to Counterfactual', plot_lims = vector() )
#   
  title(edge_title, outer = TRUE)
  
}






plot_parcel_set_parcels <- function(current_set_object){
  
  
  parcel_num = length(current_set_object$parcel_indexes)
  sub_plots = 1:(parcel_num*global_params$eco_dims)
  dim(sub_plots) = c(global_params$eco_dims, parcel_num)
  layout(sub_plots)
  
  rest_gains = current_set_object$rest_gains
  degs = current_set_object$avoided_degs
  lim_vec = c(rest_gains, degs, (rest_gains + degs)) 
  mx = max(lim_vec)
  mn = min(lim_vec)
  
  for (parcel_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(global_params$eco_dims)){
      plot(rest_gains[, eco_ind, parcel_ind], type = 'l', ylim = c(mn, mx), main = paste('parcel index =', current_set_object$parcel_indexes[parcel_ind]), ylab = '', xlab = paste('dim = ', eco_ind))
      lines(degs[, eco_ind, parcel_ind], col = 'blue')
      lines(rest_gains[, eco_ind, parcel_ind] + degs[, eco_ind, parcel_ind], col = 'red')
    }
  }
  
}





plot_sample_parcel_sets <- function(collated_parcel_sets_object, plot_num, global_params, program_params){
  
  parcel_set_indexes = sample(program_params[[1]]$total_dev_num, plot_num)
  setup_sub_plots(nx = 3, ny = 3, x_tit = TRUE)
  for (parcel_set_index in parcel_set_indexes){
    plot_parcel_set_from_collated_object(collated_parcel_sets_object, parcel_set_index, time_horizon = global_params$time_steps, global_params$eco_dims, 
                                         headings = c('Parcel Set Developments', 'Parcel Set Offsets', 'Parcel Set Outcome'))
  }
}




setup_sub_plots <- function(nx, ny, x_tit){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  if (x_tit == TRUE){
    x_space = 5
  } else {x_space = 2}
  
  par(mar = c(x_space, 2, 1, 0), oma = c(2, 4, 2.5, 0.5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
}


# 
# plot_parcel_set(collated_offsets = collated_realisations$current_offsets, collated_devs = collated_realisations$development_parcel_sets, parcel_set_num = collated_realisations$devs$parcel_set_nums[[1]], eco_ind = 1, 
#                 headings = c('Developments', 'Offsets', 'Net Parcel Set Outcome'))


collate_parcel_set_element <- function(rest_gains, avoided_degs){
  collated_outs = list()
  collated_outs$rest_gains = rest_gains
  collated_outs$avoided_degs = avoided_degs
  collated_outs$nets = rest_gains + avoided_degs
  return(collated_outs)
}



# current_offset = list(rest_gains[[realisation_ind]][[eco_ind]][, parcel_set_ind], avoided_degs[[realisation_ind]][[eco_ind]][, parcel_set_ind], 
#                       losses[[realisation_ind]][[eco_ind]][, parcel_set_ind])
# 
# 

plot_parcel_sets <- function(collated_realisations, realisation_ind, eco_ind, col_list, legend_loc){
  
  parcel_set_num = collated_realisations$devs$parcel_set_num[[realisation_ind]]
  parcel_sets_to_plot = sample(parcel_set_num, min(parcel_set_num, 3))
  
  for (parcel_set_ind in parcel_sets_to_plot){
    parcel_set_traj_list = list()
    offset_plot_list = list(collated_realisations$offsets$rest_gains[[realisation_ind]][[eco_ind]][, parcel_set_ind], 
                                    collated_realisations$offsets$avoided_degs[[realisation_ind]][[eco_ind]][, parcel_set_ind],
                                    collated_realisations$offsets$losses[[realisation_ind]][[eco_ind]][, parcel_set_ind],
                                    collated_realisations$offsets$nets[[realisation_ind]][[eco_ind]][, parcel_set_ind])

    dev_plot_list = list(collated_realisations$devs$rest_gains[[realisation_ind]][[eco_ind]][, parcel_set_ind], 
                       collated_realisations$devs$nets[[realisation_ind]][[eco_ind]][, parcel_set_ind])
    
    net_plot_list = list(collated_realisations$offsets$nets[[realisation_ind]][[eco_ind]][, parcel_set_ind],
                       collated_realisations$devs$nets[[realisation_ind]][[eco_ind]][, parcel_set_ind], 
                       collated_realisations$offsets$losses[[realisation_ind]][[eco_ind]][, parcel_set_ind], 
                       collated_realisations$parcel_set_outcomes[[realisation_ind]][[eco_ind]][, parcel_set_ind])
    
    plot_lims <- find_plot_lims(list(offset_plot_list, dev_plot_list, net_plot_list))
     
    overlay_plot_list(plot_list = offset_plot_list, yticks = 'y', axis_lab = TRUE, ylims = plot_lims, heading = 'Offset Evaluation', ylab = '', col_vec = col_list[[1]], lty_vec = c(1, 1, 1, 2), 
                      lwd_vec = c(1, 1, 1, 2), legend_vec = c('Restoration Gains', 'Avoided Degredation', 'losses', 'Relative Gains'), legend_loc)
    overlay_plot_list(plot_list = dev_plot_list, yticks = 'y', axis_lab = TRUE, ylims = plot_lims, heading = 'Development Evaluation', ylab = '', col_vec = col_list[[2]], lty_vec = c(1, 2), 
                      lwd_vec = c(1, 2), legend_vec = c('Absolute Loss', 'Relative Loss'), legend_loc)
    overlay_plot_list(plot_list = net_plot_list, yticks = 'y', axis_lab = TRUE, ylims = plot_lims, heading = 'Net Parcel Evaluation', ylab = '', col_vec = col_list[[3]], lty_vec = c(2, 2, 2, 1), 
                      lwd_vec= c(2, 2, 2, 3), legend_vec = c('Relative Offset Gains', 'Relative Development Losses', 'Illegal Clearing losses', 'Net Parcel Evaluation'), legend_loc)
    
  }
}

find_plot_lims <- function(plot_list){
      mn = min(unlist(plot_list))
      mx = max(unlist(plot_list))
      plot_lims = c(mn, mx)
      return(plot_lims)
}





overlay_plot_list <- function(plot_list, yticks, axis_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (yticks == 'y'){
    plot(plot_list[[1]], axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  } else if ((yticks == 'n')){
    plot(plot_list[[2]], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  }
  for (plot_ind in 2:length(plot_list)){
    lines(plot_list[[plot_ind]],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
  }
  abline(h = 0, lty = 2)
  legend(legend_loc, legend_vec, bty="n", lty = lty_vec, cex = 1,  pt.cex = 1, lwd = lwd_vec, col = col_vec)
}





# 
# 
# overlay_plots <- function(plot_array, yticks, axis_lab, x_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
#   
#   if (yticks == 'y'){
#     plot(plot_array[, 1], axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
#   } else if ((yticks == 'n')){
#     plot(plot_array[, 1], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
#   }
#   
#   for (plot_ind in 2:dim(plot_array)[2]){
#     lines(plot_array[, plot_ind],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
#   }
#   
#   legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
#   abline(h = 0, lty = 2)
#   
# }
# 



# 
# 
# 
# 
# 
# plot_parcel_set_from_collated_object <- function(collated_parcel_sets_object, parcel_set_indexes, time_horizon, eco_dims, headings){
#   
#   collated_offsets = collated_parcel_sets_object$offsets
#   collated_developments = collated_parcel_sets_object$devs
#   collated_NNL = collated_parcel_sets_object$NNL_object
#   
#   NNL_yrs = collated_NNL$NNL_yrs[parcel_set_indexes]
#   success_NNLs = which(NNL_yrs > 0)
#   if (length(success_NNLs) > 0){
#     success_NNL_yrs = NNL_yrs[success_NNLs]
#     x_labs = c('', '', paste('NNL at', round(mean(success_NNL_yrs), 1), 'years'))
#   } else {x_labs = c('', '', 'NNL fail')
#   }
#   
#   plot_list = vector('list', 3)
#   plot_list[[1]] = sum_parcel_sets(collated_developments$rest_gains, collated_developments$avoided_degs, collated_NNL$net_dev_gains, parcel_set_indexes, time_horizon, eco_dims)
#   plot_list[[2]] = sum_parcel_sets(collated_offsets$rest_gains, collated_offsets$avoided_degs, collated_NNL$net_offset_gains, parcel_set_indexes, time_horizon, eco_dims)
#   net_array = array(0, c(time_horizon, 3, eco_dims))
#   net_array[, 1, ] = plot_list[[1]][, 3, ]
#   net_array[, 2, ] = plot_list[[2]][, 3, ]
#   net_array[, 3, ] = plot_list[[1]][, 3, ] + plot_list[[2]][, 3, ]
#   plot_list[[3]] = net_array
#   
#   lim_vec = abind(plot_list[[1]][, , 1], plot_list[[2]][, , 1], plot_list[[3]][, , 1])
#   mx = max(lim_vec)
#   mn = min(lim_vec)
#   overlay_plots_as_vector(plot_list[[1]][, , 1], yticks = 'y', axis_lab = TRUE, x_lab = x_labs[1], ylims = c(mn, mx), (heading = headings[1]), ylab = '', col_vec = c('red', 'red', 'red'), lty_vec = c(1, 2, 1), 
#                           lwd_vec = c(1, 1, 3), legend_vec = c('Parcel Set Restoration Gains', 'Parcel Set Avoided Degredation', 'Net Losses'), legend_loc = 'topleft')
#   overlay_plots_as_vector(plot_list[[2]][, , 1], yticks = 'n', axis_lab = TRUE, x_lab = x_labs[2], ylims = c(mn, mx), (heading = headings[2]), ylab = '', col_vec = c('blue', 'blue', 'blue'), lty_vec = c(1, 2, 1), 
#                           lwd_vec = c(1, 1, 3), legend_vec = c('Parcel Set Restoration Gains', 'Parcel Set Avoided Degredation', 'Parcel Set Gains'), legend_loc = 'topleft')
#   overlay_plots_as_vector(plot_list[[3]][, , 1], yticks = 'n', axis_lab = TRUE, x_lab = x_labs[3], ylims = c(mn, mx), (heading = headings[3]), ylab = '', col_vec = c('red', 'blue', 'black'), lty_vec = c(1, 1, 1), 
#                           lwd_vec = c(1, 1, 3), legend_vec = c('Development Losses', 'Offset Gains', 'Net Outcome'), legend_loc = 'topleft')
#   
# }
# 
# 
# 
# plot_net_parcel_sets <- function(collated_object, eco_dims, parcel_set_list){
#   
#   collated_offsets = collated_object$offsets
#   collated_developments = collated_object$developments
#   
#   eco_ind = 1
#   net_rest_gains = apply(collated_offsets$rest_gains[, , eco_ind], MARGIN = 1, sum)
#   net_avoided_degs = apply(collated_object$avoided_degs[, , parcel_set_list], MARGIN = c(1, 2), sum)
#   net_trajs = apply(collated_object$parcel_set_trajs[, , parcel_set_list], MARGIN = c(1, 2), sum)
#   net_counters = apply(collated_object$parcel_set_counters[, , parcel_set_list], MARGIN = c(1, 2), sum)
#   
#   plot_list = cbind(net_rest_gains, net_avoided_degs, net_trajs, net_counters)
#   mx = max(plot_list)
#   mn = min(plot_list)
#   
#   for (eco_ind in seq_len(eco_dims)){ 
#     plot(net_rest_gains[, eco_ind] , ylim = c(mn, mx), type = 'l', xlab = paste('dim = ', eco_ind), ylab = '', col = 'black')
#     lines(net_avoided_degs[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'blue')
#     lines(net_trajs[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'red')
#     lines(net_counters[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'blue', lty = 2)
#   }
# }
# 
# 
# 
# 
# two_plot <- function(plot_a, plot_b, colours){
#   mx = max(c(plot_a, plot_b))
#   mn = min(c(plot_a, plot_b))
#   plot(plot_a, type = 'l', col = colours[1], ylim = c(mn, mx))
#   lines(plot_b, col = colours[2], ylim = c(mn, mx))
# }
# 
# 
# 
# overlay_plot_list <- function(plot_list, yticks, axis_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
#   
#   if (yticks == 'y'){
#     plot(plot_list[[1]], axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
#   } else if ((yticks == 'n')){
#     plot(plot_list[[2]], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
#   }
#   for (plot_ind in 2:length(plot_list)){
#     lines(plot_list[[plot_ind]],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
#   }
#   abline(h = 0, lty = 2)
#   legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
# }
# 
# overlay_plots_as_vector <- function(plot_array, yticks, axis_lab, x_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
#   
#   if (yticks == 'y'){
#     plot(plot_array[, 1], axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
#   } else if ((yticks == 'n')){
#     plot(plot_array[, 1], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
#   }
#   for (plot_ind in 2:dim(plot_array)[2]){
#     lines(plot_array[, plot_ind],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
#   }
#   legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
#   abline(h = 0, lty = 2)
#   
# }
# 
# 
# 


generate_offset_layer <- function(trajectories, layer_type, program_parcels, land_parcels, time_steps, landscape_dims, eco_dims){
  
  net_traj <- form_net_trajectory(trajectories_list = trajectories[program_parcels], land_parcels = land_parcels[program_parcels], time_steps, landscape_dims, eco_dims)
  
  if (layer_type == 'offset'){
    layer = net_traj[[1]][, , time_steps] > 0
  } else {
    layer = net_traj[[1]][, , 1] > 0
  }
  
  layer_object = list()
  layer_object$trajs <- net_traj
  layer_object$layer <- layer
  return(layer_object)
  
}

make_mov <- function(img_stack, filetype, mov_name, mov_folder){
  # gray.colors(n = 1024, start = 0, end = 1, gamma = 2.2, alpha = NULL)
  graphics.off()
  rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
  
  
  if (!dir.exists(mov_folder)){
    dir.create(mov_folder)
  }
  filename = paste(mov_folder, "tmp%03d.", filetype, sep = '')
  mov_name_to_use = paste(mov_folder, mov_name, '.mpg', sep = '')
  if (filetype == 'png'){
    png(filename, height = dim(img_stack)[1], width = dim(img_stack)[2])
  } else if (filetype == 'jpg'){
    jpeg(filename, height = dim(img_stack)[1], width = dim(img_stack)[2])
  }
  
  im_num = dim(img_stack)[3]
  library(pixmap)
  for (i in seq(im_num)){
    
    image(img_stack[, , i], zlim = c(0, 100), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
    
    print(paste(i, ' of ', im_num))
  }
  dev.off()
  
}
