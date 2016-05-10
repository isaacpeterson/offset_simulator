



plot_collated_realisations <- function(collated_realisations, realisation_num, global_params, parcel_sum_lims, eco_ind, lwd_vec, outer_title){
  
  lwd_vec = c(3, 0.2)
  eco_ind = 1
  outer_title = ''
  offset_cfac_type = 'include_clearing'
  summed_offset_realisations = collated_realisations$summed_offset_realisations
  summed_dev_realisations = collated_realisations$summed_dev_realisations
  col_vec_1 = c('blue', 'mediumorchid4', 'darkgreen')
  col_vec_2 = c('blue', 'mediumorchid4', 'red')
  
  time_horizon = global_params$time_steps
  eco_dims = global_params$eco_dims
  
  setup_sub_plots(nx = 1, ny = 3, x_tit = TRUE)
  
  plot_split_realisations(collated_summed_reals = collated_realisations$summed_offset_realisations, cfac_type = global_params$cfac_type_in_offset_calc, 
                          legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains'), eco_ind, lwd_vec, plot_title = 'Offset Realisations (Program Scale)',
                          outer_title,  col_vec = c('blue', 'mediumorchid4', 'darkgreen'), realisation_num)
  
  plot_split_realisations(collated_summed_reals = collated_realisations$summed_dev_realisations, cfac_type = global_params$cfac_type_in_dev_calc, 
                          legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Losses'), eco_ind, lwd_vec, plot_title = 'Development Realisations (Program Scale)',
                          outer_title,   col_vec = c('blue', 'mediumorchid4', 'red'), realisation_num)
  
  
  system_NNL_yrs = collated_realisations$system_NNLs$NNL_yrs
  system_NNL_plot_object = assess_failed_NNL(system_NNL_yrs)
  
  plot_realisation_outcomes(offset_gains = collated_realisations$summed_offset_realisations$net_outcome$standard, 
                            dev_losses = collated_realisations$summed_dev_realisations$net_outcome$standard, 
                            net_outcome = collated_realisations$net_realisation_gains$standard, 
                            plot_title = 'All Realisation Program Outcomes', x_lab = system_NNL_plot_object$x_lab, eco_ind, lwd_vec, 
                            col_vec = c('darkgreen', 'red', 'black'), legend_vec = c('net offset gains', 'net development losses', 'net program outcomes'), outer_title)
  
  plot_realisation_hists(system_NNL_plot_object, unlist(collated_realisations$parcel_set_NNL_yrs), 
                         unlist(collated_realisations$offsets$parcel_sums_at_offset), parcel_sum_lims = c(0, 20000), match_type = global_params$match_type, outer_title)
  
  summed_program_realisations = simplify_list_to_array(collated_realisations$collated_program_sums$net_trajs)
  summed_program_cfacs = simplify_list_to_array(collated_realisations$collated_program_cfac_sums$net_trajs)
  
  plot_summed_program_realisations(summed_program_realisations, summed_program_cfacs, system_NNL_yrs, eco_dims = global_params$eco_dims, eco_ind, lwd_vec, col_vec = c('red', 'blue', 'black'),
                                   legend_vec = c('Net Value', 'Net Counterfactual Value'), outer_title)
  
  cfac_trajs = simplify2array(unlist(collated_realisations$cfac_trajs, recursive = FALSE))
  net_cfac_sum = apply(cfac_trajs, MARGIN = 1, sum)
  
  plot_landscape_outcomes(collated_realisations$landscape_vals, system_NNL_yrs, realisation_num, system_NNL_plot_object, net_cfac_sum, eco_dims, eco_ind, lwd_vec, col_vec = c('red', 'blue', 'red'), 
                          legend_vec = c('landscape value', 'counterfactual value'), outer_title)
  
}



assess_landscape_loss <- function(landscape_vals, NNL_yrs, realisation_num, eco_dims){
  
  landscape_loss = array(0, c(realisation_num, eco_dims))
  
  for (realisation_ind in seq_len(realisation_num)){
    current_NNL_yr = NNL_yrs[realisation_ind]
    if (current_NNL_yr != 0){
      landscape_loss[realisation_ind, ] = landscape_vals[current_NNL_yr, realisation_ind, ]
    }
  }
  
  success_inds = which(rowSums(landscape_loss) != 0)
  landscape_loss = landscape_loss[success_inds]
  landscape_loss = 100*(1 - landscape_loss/landscape_vals[1, success_inds, ])
  
  if (length(success_inds) > 0){
    landscape_title = paste('NNL at ', round(mean(landscape_loss)), '\U00b1', round(max(range(landscape_loss) - (mean(landscape_loss))), 1), ' % loss of landscape')
  } else landscape_title = 'All realisations failed NNL'
  
  return(landscape_title)
} 



plot_summed_realisations <- function(rest_gains, avoided_degs, nets, plot_title, eco_ind, lwd_vec, col_vec, legend_vec, realisation_num){

  mx = max(cbind(avoided_degs, rest_gains, nets))
  mn = min(cbind(avoided_degs, rest_gains, nets))
  
  plot_collated_realisation_set(rest_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))
  plot_collated_realisation_set(avoided_degs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))
  plot_collated_realisation_set(nets, overlay_plots = TRUE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))  
  legend('topleft', legend_vec, bty="n", lty = c(2, 2, 2), lwd = array(lwd_vec[1], 3), col = col_vec)
  
}






plot_collated_realisation_set <- function(collated_realisations, overlay_plots, plot_col, realisation_num, eco_ind, lwd_vec, x_lab, plot_title, plot_lims){
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
    mn = min(collated_realisations[, , eco_ind])
    mx = max(collated_realisations[, , eco_ind])
  } else {
    mn = plot_lims[1]
    mx = plot_lims[2]
  }
  realisation_mean = sum_cols_multi(collated_realisations)/realisation_num
  if (overlay_plots == FALSE){
    plot(collated_realisations[, 1, eco_ind], type = 'l', main = plot_title, xlab = x_lab, ylim = c(mn, mx), col = back_plot_col, lwd = lwd_vec[2])
  } else { lines(collated_realisations[, 1, eco_ind], lwd = lwd_vec[2], col = back_plot_col)
  }
  
  if (realisation_num > 1){
    for (realisation_ind in 2:realisation_num){
      lines(collated_realisations[, realisation_ind, eco_ind], col = back_plot_col, lwd = lwd_vec[2])
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
    x_lab = paste('System NNL at', round(NNL_object$system_NNL), 'years')
  } else {x_lab = 'NNL fail'}
  
  plot((net_cond), type = 'l', main = 'Landscape Condition Relative to cfac', xlab = x_lab, ylab = '', col = 'red', lwd = 3)
  abline(h = 0, lty = 2)
  
}
plot_NNL_hist <- function(NNL_plot_object, plot_tit, x_lab, x_lim){
  if (length(NNL_plot_object$NNLs) > 0){
    hist(NNL_plot_object$NNLs, main = plot_tit, xlab = NNL_plot_object$x_lab, xlim = x_lim, breaks=seq(min(NNL_plot_object$NNLs),max(NNL_plot_object$NNLs),by=1))
  } else {x_lab = ('All realisations failed NNL')}
  
}




plot_mean_gains_degs <- function(summed_realisations, outer_title, realisation_num){
  
  rest_gains = sum_cols_multi(summed_realisations$rest_gains)/realisation_num
  net_degs = sum_cols_multi(summed_realisations$avoided_degs)/realisation_num
  net_gains = net_degs + rest_gains
  
  if (global_params$adjust_cfacs_flag == TRUE){
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


plot_split_realisations <- function(collated_summed_reals, cfac_type, legend_vec, eco_ind, lwd_vec, plot_title, outer_title, col_vec, realisation_num){
  
  rest_gains = collated_summed_reals$rest_gains
  
  if (cfac_type == 'standard'){
    avoided_degs = collated_summed_reals$avoided_degs
    nets = collated_summed_reals$net_outcome$standard
  } else if ((cfac_type == 'include_clearing') || (cfac_type == 'include_clearing_offsets')){
    avoided_degs = collated_summed_reals$avoided_degs_adjusted
    nets = collated_summed_reals$net_outcome$adjusted
  } 
  
  plot_summed_realisations(rest_gains, avoided_degs, nets, plot_title, eco_ind, lwd_vec, col_vec, legend_vec, realisation_num)
  title(outer_title, outer = TRUE)
}





plot_realisation_hists <- function(system_NNL_plot_object, parcel_set_NNL_yrs, parcel_sums_at_offset_array, parcel_sum_lims, match_type, outer_title){
  
  setup_sub_plots(nx = 1, ny = 3, x_tit = TRUE)
  
  hist(parcel_sums_at_offset_array, main = 'selected offset parcel values', xlab = 'selected offset parcel values', xlim = parcel_sum_lims)
  plot_NNL_hist(system_NNL_plot_object, plot_tit = 'System NNL Assessment', x_lim = c(0, 100))
  if (match_type == 'parcel_set'){
    parcel_set_NNL_plot_object = assess_failed_NNL(parcel_set_NNL_yrs)
    plot_NNL_hist(parcel_set_NNL_plot_object, plot_tit = 'Parcel Set NNL Assessment', x_lim = c(0, 100)) 
  }
  title(outer_title, outer = TRUE)
}

plot_realisation_outcomes <- function(offset_gains, dev_losses, net_outcome, plot_title, x_lab, eco_ind, lwd_vec, col_vec, legend_vec, outer_title){
  
  #setup_sub_plots(nx = 1, ny = 1, x_tit = TRUE)
  
  plot_lims = c(min(cbind(net_outcome, dev_losses, offset_gains)), max(cbind(net_outcome, dev_losses, offset_gains)))
  
  plot_collated_realisation_set(offset_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = plot_lims)
  
  plot_collated_realisation_set(dev_losses, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = plot_lims)
  plot_collated_realisation_set(net_outcome, overlay_plots = TRUE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
                                x_lab = x_lab, plot_title = plot_title, plot_lims = plot_lims)
  
  legend('topleft', legend_vec, bty="n", lty = c(2, 2, 2), lwd = array(lwd_vec[1], 3), col = col_vec)
  
  title(outer_title, outer = TRUE)
}


plot_landscape_outcomes <- function(landscape_realisations, system_NNL_yrs, realisation_num, system_NNL_plot_object, net_cfac_sum, eco_dims, eco_ind, lwd_vec, col_vec,  legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  
  landscape_tit = assess_landscape_loss(landscape_realisations, system_NNL_yrs, realisation_num, eco_dims)
  landscape_tit = paste('Landscape Scale Value (', landscape_tit, ')' )
  
  plot_collated_realisation_set(landscape_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = system_NNL_plot_object$x_lab, plot_title = landscape_tit, plot_lims = c(0, max(landscape_realisations)))
  lines(net_cfac_sum, col = col_vec[2], lwd = 2)
  
  abline(h = mean(landscape_realisations[1, , ]), lty = 2)
  
  legend('topright', legend_vec, bty="n", lty = c(2, 1), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  plot_collated_realisation_set( (landscape_realisations - net_cfac_sum), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
                                 x_lab = system_NNL_plot_object$x_lab, plot_title = 'Landscape Value Relative to Counterfactual', plot_lims = vector())
  
  
  title(outer_title, outer = TRUE) 
}




plot_summed_program_realisations <- function(summed_program_realisations, summed_program_cfacs, system_NNL_yrs, eco_dims, eco_ind, lwd_vec, col_vec, legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  program_scale_tit = assess_landscape_loss(summed_program_cfacs, system_NNL_yrs, realisation_num, eco_dims)
  
  program_scale_tit = paste('Net Program Value (', program_scale_tit, ')')
  plot_collated_realisation_set(summed_program_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = program_scale_tit, plot_lims = c(0, max(summed_program_realisations)))
  plot_collated_realisation_set(summed_program_cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = vector())
  
  legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  
  plot_collated_realisation_set( (summed_program_realisations - summed_program_cfacs), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind = 1, lwd_vec, 
                                 x_lab = '', plot_title = 'Net Value Rel. to Counterfactual', plot_lims = vector() )
  
  title(outer_title, outer = TRUE)
  
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





plot_sample_parcel_sets <- function(collated_parcel_sets_object, plot_num, global_params){
  
  parcel_set_indexes = sample(global_params$total_dev_num, plot_num)
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
  
  par(mar = c(x_space, 1, 1, 0), oma = c(2, 4, 2.5, 0.5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
}





plot_parcel_set_from_collated_object <- function(collated_parcel_sets_object, parcel_set_indexes, time_horizon, eco_dims, headings){
  
  collated_offsets = collated_parcel_sets_object$offsets
  collated_developments = collated_parcel_sets_object$devs
  collated_NNL = collated_parcel_sets_object$NNL_object
  
  NNL_yrs = collated_NNL$NNL_yrs[parcel_set_indexes]
  success_NNLs = which(NNL_yrs > 0)
  if (length(success_NNLs) > 0){
    success_NNL_yrs = NNL_yrs[success_NNLs]
    x_labs = c('', '', paste('NNL at', round(mean(success_NNL_yrs), 1), 'years'))
  } else {x_labs = c('', '', 'NNL fail')
  }
  
  plot_list = vector('list', 3)
  plot_list[[1]] = sum_parcel_sets(collated_developments$rest_gains, collated_developments$avoided_degs, collated_NNL$net_dev_gains, parcel_set_indexes, time_horizon, eco_dims)
  plot_list[[2]] = sum_parcel_sets(collated_offsets$rest_gains, collated_offsets$avoided_degs, collated_NNL$net_offset_gains, parcel_set_indexes, time_horizon, eco_dims)
  net_array = array(0, c(time_horizon, 3, eco_dims))
  net_array[, 1, ] = plot_list[[1]][, 3, ]
  net_array[, 2, ] = plot_list[[2]][, 3, ]
  net_array[, 3, ] = plot_list[[1]][, 3, ] + plot_list[[2]][, 3, ]
  plot_list[[3]] = net_array
  
  lim_vec = abind(plot_list[[1]][, , 1], plot_list[[2]][, , 1], plot_list[[3]][, , 1])
  mx = max(lim_vec)
  mn = min(lim_vec)
  overlay_plots_as_vector(plot_list[[1]][, , 1], yticks = 'y', axis_lab = TRUE, x_lab = x_labs[1], ylims = c(mn, mx), (heading = headings[1]), ylab = '', col_vec = c('red', 'red', 'red'), lty_vec = c(1, 2, 1), 
                          lwd_vec = c(1, 1, 3), legend_vec = c('Parcel Set Restoration Gains', 'Parcel Set Avoided Degredation', 'Net Losses'), legend_loc = 'topleft')
  overlay_plots_as_vector(plot_list[[2]][, , 1], yticks = 'n', axis_lab = TRUE, x_lab = x_labs[2], ylims = c(mn, mx), (heading = headings[2]), ylab = '', col_vec = c('blue', 'blue', 'blue'), lty_vec = c(1, 2, 1), 
                          lwd_vec = c(1, 1, 3), legend_vec = c('Parcel Set Restoration Gains', 'Parcel Set Avoided Degredation', 'Parcel Set Gains'), legend_loc = 'topleft')
  overlay_plots_as_vector(plot_list[[3]][, , 1], yticks = 'n', axis_lab = TRUE, x_lab = x_labs[3], ylims = c(mn, mx), (heading = headings[3]), ylab = '', col_vec = c('red', 'blue', 'black'), lty_vec = c(1, 1, 1), 
                          lwd_vec = c(1, 1, 3), legend_vec = c('Development Losses', 'Offset Gains', 'Net Outcome'), legend_loc = 'topleft')
  
}



plot_net_parcel_sets <- function(collated_object, eco_dims, parcel_set_list){
  
  collated_offsets = collated_object$offsets
  collated_developments = collated_object$developments
  
  eco_ind = 1
  net_rest_gains = apply(collated_offsets$rest_gains[, , eco_ind], MARGIN = 1, sum)
  net_avoided_degs = apply(collated_object$avoided_degs[, , parcel_set_list], MARGIN = c(1, 2), sum)
  net_trajs = apply(collated_object$parcel_set_trajs[, , parcel_set_list], MARGIN = c(1, 2), sum)
  net_counters = apply(collated_object$parcel_set_counters[, , parcel_set_list], MARGIN = c(1, 2), sum)
  
  plot_list = cbind(net_rest_gains, net_avoided_degs, net_trajs, net_counters)
  mx = max(plot_list)
  mn = min(plot_list)
  
  for (eco_ind in seq_len(eco_dims)){ 
    plot(net_rest_gains[, eco_ind] , ylim = c(mn, mx), type = 'l', xlab = paste('dim = ', eco_ind), ylab = '', col = 'black')
    lines(net_avoided_degs[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'blue')
    lines(net_trajs[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'red')
    lines(net_counters[, eco_ind] , ylim = c(mn, mx), xlab = paste('dim = ', eco_ind), ylab = '', col = 'blue', lty = 2)
  }
}




two_plot <- function(plot_a, plot_b, colours){
  mx = max(c(plot_a, plot_b))
  mn = min(c(plot_a, plot_b))
  plot(plot_a, type = 'l', col = colours[1], ylim = c(mn, mx))
  lines(plot_b, col = colours[2], ylim = c(mn, mx))
}



overlay_plots_as_list <- function(plot_list, yticks, axis_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (yticks == 'y'){
    plot(plot_list[[1]], axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  } else if ((yticks == 'n')){
    plot(plot_list[[2]], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  }
  for (plot_ind in 2:length(plot_list)){
    lines(plot_list[[plot_ind]],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
  }
  abline(h = 0, lty = 2)
  legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
}

overlay_plots_as_vector <- function(plot_array, yticks, axis_lab, x_lab, ylims, heading, ylab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (yticks == 'y'){
    plot(plot_array[, 1], axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  } else if ((yticks == 'n')){
    plot(plot_array[, 1], yaxt = 'n', axes = axis_lab, type = 'l', main = heading, xlab = x_lab, ylim = ylims, ylab = ylab, col = col_vec[1], lty = lty_vec[1])
  }
  for (plot_ind in 2:dim(plot_array)[2]){
    lines(plot_array[, plot_ind],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
  }
  legend(legend_loc, legend_vec, bty="n", lty = lty_vec, lwd = lwd_vec, col = col_vec)
  abline(h = 0, lty = 2)
  
}






