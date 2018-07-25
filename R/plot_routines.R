plot_outcome_set <- function(collated_realisations, current_simulation_params, plot_params,
                             realisation_num, site_plot_lims, program_plot_lims, landscape_plot_lims, feature_ind,  set_to_plot){
  
  if (plot_params$plot_site == TRUE){
    plot_site_outcomes(collated_realisations, 
                       plot_params$plot_site_offset, 
                       plot_params$plot_site_dev, 
                       plot_params$output_type, 
                       current_simulation_params, 
                       set_to_plot, 
                       site_plot_lims, 
                       feature_ind,  
                       plot_params$site_outcome_lwd_vec)
  }
  
  if (plot_params$plot_program == TRUE){
    
    plot_outcomes(collated_realisations$program_outcomes$net_outcome, 
                  plot_type = 'program', 
                  enforce_limits = TRUE, 
                  include_legend = FALSE, 
                  y_lims = program_plot_lims ,
                  plot_title = 'Program Outcome', 
                  loss_stats = collated_realisations$net_program_loss, 
                  collated_realisations$realisation_num, 
                  collated_realisations$program_cfacs$program_cfac_sum,
                  plot_params$program_outcome_lwd_vec, 
                  outcome_col = plot_params$landscape_col, 
                  cfac_col = plot_params$cfac_col,
                  legend_vec = c('Outcome', 'Counterfactual'), 
                  current_simulation_params$time_steps)
  }
  
  if (plot_params$plot_landscape == TRUE){ 
    plot_outcomes(collated_realisations$landscape_scale$landscape_outcome, 
                  plot_type = 'landscape', 
                  enforce_limits = TRUE, 
                  include_legend = FALSE, 
                  y_lims =landscape_plot_lims,
                  plot_title = 'Landscape Outcome', 
                  loss_stats = collated_realisations$landscape_loss, 
                  collated_realisations$realisation_num, 
                  collated_realisations$landscape_scale$landscape_cfacs[[1]], 
                  plot_params$landscape_outcome_lwd_vec, 
                  outcome_col = plot_params$landscape_col, 
                  cfac_col = plot_params$cfac_col,
                  legend_vec = c('Outcome', 'Counterfactual'), 
                  time_steps = current_simulation_params$time_steps)
    
  }
}


plot_site_outcomes <- function(collated_realisations, plot_site_offset_outcome, plot_site_dev_outcome, 
                               output_type, current_simulation_params, set_to_plot, site_plot_lims, feature_ind, site_lwd){
  y_lab = get_y_lab(output_type, current_simulation_params, feature_ind)
  
  if (current_simulation_params$use_offset_bank == TRUE){
    offset_site_indexes_to_use = collated_realisations$offset_bank_object$site_indexes
    dev_site_indexes_to_use = collated_realisations$credit_object$site_indexes
  } else{
    offset_site_indexes_to_use = collated_realisations$offsets_object$site_indexes
    dev_site_indexes_to_use = collated_realisations$dev_object$site_indexes
  }
  
  x_lab = ''
  plot_type = 'non-overlay'
  if (plot_site_dev_outcome == TRUE){
    overlay_trajectories(dev_site_indexes_to_use,
                         current_simulation_params$use_offset_bank,
                         trajectories = collated_realisations$site_scale$outcomes, 
                         realisation_ind = 1, 
                         plot_col = 'red', 
                         plot_type, 
                         overlay_type = 'single', 
                         set_to_plot, 
                         y_lab, 
                         site_plot_lims,
                         site_lwd, 
                         x_lab)
    plot_type = 'overlay'
  }
  if (plot_site_offset_outcome == TRUE){
    overlay_trajectories(offset_site_indexes_to_use, 
                         current_simulation_params$use_offset_bank,
                         trajectories = collated_realisations$site_scale$outcomes, 
                         realisation_ind = 1, 
                         plot_col = 'darkgreen', 
                         plot_type, 
                         overlay_type = 'single', 
                         set_to_plot, 
                         y_lab, 
                         site_plot_lims, 
                         site_lwd, 
                         x_lab)
  }
}


plot_impact_set <- function(collated_realisations, current_simulation_params, plot_params, realisation_num, 
                            site_plot_lims, program_plot_lims, landscape_plot_lims, current_feature, sets_to_plot){
  
  # Plot the site scale impacts
  if (plot_params$plot_site == TRUE){
    
    overlay_site_impacts(collated_realisations,
                         plot_params$plot_site_offset, 
                         plot_params$plot_site_dev, 
                         plot_params$plot_site_net, 
                         plot_params$output_type,
                         current_simulation_params,
                         realisation_ind = 1, 
                         current_feature, 
                         plot_from_impact_yr = FALSE, 
                         sets_to_plot,
                         site_plot_lims,
                         current_simulation_params$time_steps, 
                         plot_params$site_impact_col_vec, 
                         plot_params$site_impact_lwd)
  }
  # Plot the program scale impacts
  if (plot_params$plot_program == TRUE){
    
    NNL_object <- find_NNL_characteristics(collated_realisations$program_scale_NNL$NNL_mean, 
                                           collated_realisations$program_scale_impacts$program_total)
    
    overlay_realisations(plot_list = list(unlist(collated_realisations$program_scale_impacts$net_offset_gains, recursive = FALSE), 
                                          unlist(collated_realisations$program_scale_impacts$net_dev_losses, recursive = FALSE),
                                                 unlist(collated_realisations$program_scale_impacts$program_total, recursive = FALSE)),
                         plot_title = 'Program Impact', 
                         x_lab = NNL_object$NNL_label,
                         collated_realisations$realisation_num,
                         plot_params$program_lwd_vec, 
                         col_vec = plot_params$program_col_vec, 
                         legend_loc = 'topleft',
                         legend_vec = 'NA', #c('Net Offset Impact', 'Net Development Impact', 'Net Impact'), 
                         plot_lims = program_plot_lims)
    
    if (length(NNL_object$mean_NNL) >0){
      abline(v = NNL_object$mean_NNL, lty = 2)
    }
    
    last_dev_yr = mean(unlist(lapply(seq_along(collated_realisations$site_scale_impacts$dev_object), 
                     function(i) tail(unlist(collated_realisations$site_scale_impacts$dev_object[[i]]$offset_yrs), 1))))
    dev_end = tail(which(current_simulation_params$intervention_vec > 0), 1)
    if (last_dev_yr < dev_end){
      line_to_use = last_dev_yr
      plot_col = 'red'
    } else {
      line_to_use = dev_end
      plot_col = 'black'
    }
    abline(v = line_to_use, lty = 3, col = plot_col)
    
  }
  
  # Plot the landscape scale impacts
  if (plot_params$plot_landscape == TRUE){
    NNL_object <- find_NNL_characteristics(collated_realisations$landscape_scale_NNL$NNL_mean, 
                                           collated_realisations$landscape_scale$landscape_scale_impact)
    
    overlay_realisations(plot_list = list(unlist(collated_realisations$landscape_scale$landscape_scale_impact, recursive = FALSE)),
                         plot_title = 'Landscape Impact', 
                         x_lab = NNL_object$NNL_label,
                         collated_realisations$realisation_num,
                         plot_params$landscape_lwd_vec, 
                         plot_params$landscape_col,
                         legend_loc = 'topright',
                         legend_vec = 'NA', 
                         landscape_plot_lims) 
  }
}



check_plot_options <- function(plot_params, current_simulation_params, scenario_filenames) {
  
  if(plot_params$plot_type != 'impacts' & plot_params$plot_type != 'outcomes'){
    stop( paste0('\nERROR: Illegal plot option specified. Variable plot_type is set to ', plot_params$plot_type) )
  }
  
  if (sum(current_simulation_params$intervention_vec) < max(plot_params$sets_to_plot)){
    stop (paste('chosen example set to plot needs to be less than ', sum(current_simulation_params$intervention_vec)))
  }
  
  if (plot_params$output_type == 'scenarios'){
    if (length(scenario_filenames) == 0){
      stop( paste('\nERROR: No files that match _simulation_params found in', plot_params$simulation_params_folder) )
    } else if (length(scenario_filenames) < max(plot_params$plot_vec)){
      stop ( paste('\nERROR: only ', length(scenario_filenames), ' scenario params files found, plot_params$plot_vec parameter does not match'))
    }
  } else if (plot_params$output_type == 'features'){
    if (current_simulation_params$feature_num < max(plot_params$plot_vec)){
      stop ( paste('\nERROR: plot_params$plot_vec exceeds number of features (', current_simulation_params$feature_num, ')'))
    }
    
  } else if (plot_params$output_type == 'multiple_sets'){
    if ( max(plot_params$sets_to_plot) > sum(current_simulation_params$intervention_vec)){
      stop ( paste('\nERROR: plot_params$sets_to_plot exceeds number of developments (', sum(current_simulation_params$intervention_vec), ')'))
    }
  }
  
}


NNL_test <- function(NNL_set, collated_impacts){
  
  inds_to_use = which(unlist(lapply(seq_along(NNL_set), function(i) length(NNL_set[[i]]) > 0)))
  
  NNL_to_use = NNL_set[inds_to_use]
  last_vals = lapply(inds_to_use, function(i) collated_impacts[[i]][ length(collated_impacts[[i]]) ])
  inds_to_reject = which(unlist(last_vals) < 0)
  if (length(inds_to_reject) > 0){
    NNL_to_use = NNL_to_use[-inds_to_reject]
  }
  
  return(NNL_to_use)
}



find_NNL_characteristics <- function(NNL_set, collated_impacts){
  
  collated_impacts = unlist(collated_impacts, recursive = FALSE)
  NNL_to_use <- NNL_test(NNL_set, collated_impacts)
  # get the mean values of the list of vecs

  mean_collated_impact <- find_list_mean(collated_impacts)
  
  # get the last element of the mean vec
  final_collated_impact <- tail(mean_collated_impact, n=1)
  
  if (length(NNL_to_use) > 0){
    mean_NNL = round(mean(unlist(NNL_to_use)))
    NNL_label = paste0(length(unlist(NNL_to_use)), ' realisations achieved NNL at ', mean_NNL, ' years')
  } else {
    mean_NNL = vector()
    NNL_label = 'All realisations faileld NNL'
  } 
  
  NNL_object = list()
  # add the last vale of the impact to the plot lable
  NNL_object$NNL_label <- cbind(NNL_label, paste0('mean final impact = ', format(final_collated_impact, scientific=TRUE, digits=3)))
  NNL_object$mean_NNL <- mean_NNL
  return(NNL_object)
}


overlay_trajectories <- function(site_indexes_to_use, offset_bank, trajectories, realisation_ind, plot_col, plot_type, 
                                 overlay_type, sets_to_plot, y_lab, site_plot_lims, lwd, x_lab){
  if (offset_bank == TRUE){
    current_site_indexes_to_use = unlist(site_indexes_to_use[[realisation_ind]])
    plot_list = list(Reduce('+', trajectories[[realisation_ind]][current_site_indexes_to_use]))
  } else {
    if ( sets_to_plot > length(site_indexes_to_use[[realisation_ind]])){
      stop ( paste('\nERROR: plot_params$sets_to_plot exceeds number of devs/offsets'))
    }
    current_site_indexes_to_use = unlist(site_indexes_to_use[[realisation_ind]][sets_to_plot])
    plot_list = trajectories[[realisation_ind]][current_site_indexes_to_use]
    
  }
  
  overlay_plot_list(plot_type, plot_list, yticks = 'y', ylims = site_plot_lims, heading = 'Site Outcomes', ylab = y_lab, x_lab, 
                    col_vec = rep(plot_col, length(plot_list)), lty_vec = rep(1, length(plot_list)), lwd_vec = rep(lwd, length(plot_list)), 
                    legend_vec = 'NA', legend_loc = FALSE)
}


get_y_lab <- function(output_type, current_simulation_params, feature_ind){
  y_lab = paste('Feature', feature_ind)
  if (feature_ind %in% current_simulation_params$features_to_use_in_offset_calc){
    ylab = paste0(y_lab, 'T') 
  } 
  
  if (feature_ind %in% current_simulation_params$features_to_use_in_offset_intervention){
    ylab = paste0(y_lab, 'O') 
  } 
  
  ylab = paste0(y_lab, '\n', current_simulation_params$offset_calc_type, '/', current_simulation_params$dev_calc_type )
  
  #   if (current_simulation_params$use_offset_bank == FALSE){
  #     y_lab = cbind(y_lab, paste0('T.H.', current_simulation_params$offset_time_horizon, ', ill_clear ', current_simulation_params$include_unregulated_loss_in_offset_calc))
  #   } else{
  #     y_lab = cbind(y_lab, paste0(' offset_bank T, Clearing ', current_simulation_params$include_unregulated_loss_in_offset_calc))
  #   }
  #  y_lab = t(y_lab)
  return(y_lab)
}


overlay_site_impacts <- function(collated_realisations, plot_site_offset_impact, plot_site_dev_impact, plot_site_net_impact, output_type, current_simulation_params, realisation_ind, 
                                 feature_ind, plot_from_impact_yr, sets_to_plot, site_plot_lims, time_steps, col_vec, plot_lwd){
  y_lab = get_y_lab(output_type, current_simulation_params, feature_ind)
  plot_lwd = 1
  
  stats_to_use = unlist(collated_realisations$sites_used)
  x_lab = ''
  if (current_simulation_params$use_offset_bank == FALSE){
    offset_set = collated_realisations$site_scale_impacts$offsets_object
    dev_set = collated_realisations$site_scale_impacts$dev_object

#     if (max(sets_to_plot) > length(dev_set[[realisation_ind]]$site_indexes)){
#       stop(cat('\nexample set to plot exceeds total development number of ', length(dev_set$site_indexes[[realisation_ind]]), ' sites'))
#     }
    net_plot_list = collated_realisations$site_scale_net_impacts$net_impacts[[realisation_ind]][sets_to_plot]
    
  } else {
    offset_set = collated_realisations$program_scale_impacts$net_offset_gains
    dev_set = collated_realisations$program_scale_impacts$net_dev_losses
    net_plot_list = collated_realisations$program_scale_impacts$program_total[[realisation_ind]]
  }
  plot_type = 'non-overlay'
  
  for (plot_ind in seq_along(sets_to_plot)){
    current_set_to_plot = sets_to_plot[plot_ind]
    # Plot the impact of the offset site(s) 
    if (plot_site_offset_impact == TRUE){

      overlay_impact(collated_object = offset_set,
                     current_simulation_params$use_offset_bank,
                     visualisation_type = 'stacked', 
                     realisation_ind, 
                     plot_col = col_vec[1], 
                     plot_lwd,
                     plot_type,
                     y_lab,
                     x_lab,
                     plot_from_impact_yr,
                     current_set_to_plot, 
                     site_plot_lims, 
                     time_steps)
      
      plot_type = 'overlay'
    }
    
    
    # Overlay the impact of the development site 
    
    if (plot_site_dev_impact == TRUE){
      
      overlay_impact(dev_set,
                     current_simulation_params$use_offset_bank,
                     visualisation_type = 'non-stacked', 
                     realisation_ind, 
                     plot_col = col_vec[2],
                     plot_lwd,
                     plot_type,
                     y_lab = '',
                     x_lab,
                     plot_from_impact_yr,
                     current_set_to_plot, 
                     site_plot_lims, 
                     time_steps)
    }
    
    # Overlay the net impact of the offset and development impact 

    if (plot_site_net_impact == TRUE){
      overlay_plot_list(plot_type, net_plot_list[plot_ind], yticks = 'y', ylims = site_plot_lims, heading = 'Site Outcomes', ylab = '', x_lab = '', 
                        col_vec = rep(col_vec[3], length(net_plot_list)), lty_vec = rep(1, length(net_plot_list)), lwd_vec = rep(plot_lwd, length(net_plot_list)), 
                        legend_vec = 'NA', legend_loc = FALSE)
    }
    plot_type = 'non-overlay'
  }
}


overlay_impact <- function(collated_object, offset_bank, visualisation_type, realisation_ind, 
                           plot_col, plot_lwd, plot_type, y_lab, x_lab, plot_from_impact_yr, 
                           set_to_plot, plot_lims, time_steps){

  if (offset_bank == FALSE){
    collated_traj_set = collated_object[[realisation_ind]]$nets
    site_indexes = unlist(collated_object[[realisation_ind]]$site_indexes[set_to_plot])
    inds_to_plot = which(unlist(collated_object[[realisation_ind]]$site_indexes) %in% site_indexes)
    if (plot_from_impact_yr){
      offset_yrs = collated_object[[realisation_ind]]$offset_yrs[inds_to_plot]
    } else {
      offset_yrs = rep(list(1), length(inds_to_plot))
    }
    plot_list = lapply(seq_along(inds_to_plot), function(i) collated_traj_set[[inds_to_plot[i]]][offset_yrs[[i]]:time_steps])
    if (visualisation_type == 'stacked'){
      plot_list = lapply(seq_along(plot_list), function(i) Reduce('+', plot_list[1:i]))
    }
  } else {
    plot_list = list(collated_object[[realisation_ind]])
  }
  overlay_plot_list(plot_type, plot_list, yticks = 'y', ylims = plot_lims, heading = 'Site Impact', y_lab, x_lab, 
                    col_vec = rep(plot_col, length(plot_list)), lty_vec = rep(1, length(plot_list)), lwd_vec = rep(plot_lwd, length(plot_list)), 
                    legend_vec = 'NA', legend_loc = FALSE)
}


plot_split_realisations <- function(plot_type, rest_gains, avoided_loss, nets, plot_title, feature_ind, lwd_vec, col_vec, legend_vec, legend_pos, realisation_num, ylim){
  
  plot_num = length(col_vec)
  plot_collated_realisation_set(rest_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)
  if (plot_type == 'offsets'){
    plot_collated_realisation_set(avoided_loss, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)
  }
  plot_collated_realisation_set(nets, overlay_plots = TRUE, plot_col = col_vec[plot_num], realisation_num, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)  
  legend(legend_pos, legend_vec, bty="n", lty = rep(2, plot_num), lwd = array(lwd_vec[1], plot_num), col = col_vec)
}


find_list_mean <- function(list_to_average){
    list_mean = Reduce('+', list_to_average)/length(list_to_average)
    return(list_mean)
}


plot_collated_realisation_set <- function(plot_list, overlay_plots, plot_col, realisation_num, lwd_vec, x_lab, plot_title, plot_lims){
  
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
    mn = min(unlist(plot_list))
    mx = max(unlist(plot_list))
  } else {
    mn = plot_lims[1]
    mx = plot_lims[2]
  }
  
  if (overlay_plots == FALSE){
    graphics::plot(plot_list[[1]], type = 'l', ylab = '', main = plot_title, xlab = x_lab, ylim = c(mn, mx), col = back_plot_col, lwd = lwd_vec[2])
  } else { lines(plot_list[[1]], lwd = lwd_vec[2], col = back_plot_col)
  }
  
  if (realisation_num > 1){
    for (realisation_ind in 2:realisation_num){
      lines(plot_list[[realisation_ind]], col = back_plot_col, lwd = lwd_vec[2])
    }
  }
  
  plot_mean = find_list_mean(plot_list)
  lines(plot_mean, ylim = c(mn, mx), col = plot_col, lwd = lwd_vec[1], lty = 2)
  abline(h = 0, lty = 2)
  
}


plot_NNL_hist <- function(NNL_plot_object, plot_tit, x_lim, feature_ind){
  
  if (NNL_plot_object$NNL_success > 0){
    x_lab = t(cbind( paste0('Mean NNL at  ', round(NNL_plot_object$NNL_mean), 'years'), paste0('NNL success = ', round(NNL_plot_object$NNL_success*100), '%' )))
    NNL_yrs = unlist(NNL_plot_object$NNL_yrs)
    NNL_yrs <- NNL_yrs[which(NNL_yrs > 0)]
    hist(NNL_yrs, main = plot_tit, xlab = x_lab, xlim = x_lim, breaks=seq(min(NNL_yrs),max(NNL_yrs),by=1))
    
  } else {
    null_plot()
  }
  
}



null_plot <- function(){
  graphics::plot(NULL, type= 'n', xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
}

plot_parcel_sums_hist <- function(parcel_sums_at_offset, feature_ind, parcel_sum_lims){
  current_parcel_sums_at_offset = unlist(parcel_sums_at_offset, recursive = FALSE)
  parcel_sums_at_offset_array = unlist(lapply(seq_along(current_parcel_sums_at_offset), function(i) current_parcel_sums_at_offset[[i]]))
  hist(parcel_sums_at_offset_array, main = 'offset parcel values', xlab = 'selected offset parcel values', xlim = parcel_sum_lims)
}

plot_NNL_hists <- function(parcel_set_NNL, program_scale_NNL, system_NNL, use_parcel_sets, feature_ind){
  
  if (use_parcel_sets == TRUE){
    plot_NNL_hist(parcel_set_NNL, plot_tit = 'Site scale NNL Assessment', x_lim = c(0, 100), feature_ind) 
  } else {
    null_plot()
  }
  
  plot_NNL_hist(program_scale_NNL, plot_tit = 'Program scale NNL Assessment', x_lim = c(0, 100), feature_ind)
  plot_NNL_hist(system_NNL, plot_tit = 'Landscape scale NNL Assessment', x_lim = c(0, 100), feature_ind)
  
}


overlay_realisations <- function(plot_list, plot_title, x_lab, realisation_num, lwd_vec, 
                                 col_vec, legend_vec, legend_loc, plot_lims){
  if (length(unlist(plot_list)) == 0){
    null_plot()
    return()
  }
  
  if (length(plot_lims) == 0){
    plot_lims = find_plot_lims(plot_list)
  }
  
  for (plot_ind in seq_along(plot_list)){
    if (plot_ind == 1){
      overlay_plots = FALSE
    } else {
      overlay_plots = TRUE
    }
    plot_collated_realisation_set(plot_list[[plot_ind]], overlay_plots, plot_col = col_vec[plot_ind], 
                                  realisation_num, lwd_vec, 
                                  x_lab, plot_title, plot_lims = plot_lims)
  }
  
  if (legend_vec[1] != 'NA'){
    legend(legend_loc, legend_vec, bty="n", lty = c(2, 2, 2, 2), lwd = array(lwd_vec[1], 4), col = col_vec)
  }
  
}



plot_outcomes <- function(current_outcome_set, plot_type, enforce_limits, include_legend, y_lims, plot_title, 
                          loss_stats, realisation_num,  cfacs, lwd_vec, outcome_col, cfac_col, legend_vec, time_steps){
  
  current_total_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_stats$total_loss[[i]]))
  #loss_tit = paste0('Net Loss at ', time_steps, 'yrs = ', round(mean(unlist(current_total_loss))*100), '%')
  #   current_NNL_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_stats$NNL_loss[[i]]))
  #   if (length(current_NNL_loss) > 0){
  #     NNL_tit = paste0('Mean NNL at  ', round(mean(current_NNL_loss*100)), '% landscape loss')
  #   } else {
  #     NNL_tit = 'All realisations failed NNL'
  #   }
  
  loss_tit = ''
  NNL_tit = ''
  sub_tit = cbind(NNL_tit, loss_tit)
  
  if (enforce_limits == TRUE){
    plot_lims = y_lims
  } else {
    plot_vec = c(unlist(current_outcome_set), unlist(current_cfacs))
    plot_lims = c(min(plot_vec), max(plot_vec))
  }
  plot_collated_realisation_set(current_outcome_set, overlay_plots = FALSE, plot_col = outcome_col, realisation_num, lwd_vec, 
                                x_lab = sub_tit, plot_title = plot_title, plot_lims)
  
  if (plot_type == 'program'){
    plot_collated_realisation_set(cfacs, overlay_plots = TRUE, plot_col = cfac_col, realisation_num, lwd_vec, 
                                  x_lab = '', plot_title = '', plot_lims = y_lims)
  } else {
    lines(cfacs, col = cfac_col, lty = 2, lwd = 2)
  }
  
  #abline(h = mean(current_outcome_set[1, , ]), lty = 2)
  if (include_legend == TRUE){
    legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  }
  
}


setup_sub_plots <- function(nx, ny, x_space, y_space){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(x_space, y_space, 1, 0), oma = c(2, 4, 2.5, 0.5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.3, 0))
  
}




find_plot_lims <- function(plot_list){
  mn = min(unlist(plot_list))
  mx = max(unlist(plot_list))
  plot_lims = c(mn, mx)
  return(plot_lims)
}



overlay_plot_list <- function(plot_type, plot_list, yticks, ylims, heading, ylab, x_lab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (plot_type == 'non-overlay'){
    graphics::plot(plot_list[[1]], type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  } else {
    lines(plot_list[[1]], type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
  }
  
  if (length(plot_list) > 1){
    for (plot_ind in 2:length(plot_list)){
      lines(plot_list[[plot_ind]],  ylim = ylims, col = col_vec[plot_ind], lwd = lwd_vec[plot_ind], lty = lty_vec[plot_ind])
    }
  }
  abline(h = 0, lty = 2)
  if (legend_vec[1] != 'NA'){
    legend(legend_loc, legend_vec, bty="n", lty = lty_vec, cex = 1,  pt.cex = 1, lwd = lwd_vec, col = col_vec)
  }
}



make_mov <- function(img_stack, filetype, mov_name, mov_folder){
  # gray.colors(n = 1024, start = 0, end = 1, gamma = 2.2, alpha = NULL)
  graphics.off()
  rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
  
  
  if (!dir.exists(mov_folder)){
    dir.create(mov_folder)
  }
  filename = paste0(mov_folder, "tmp%03d.", filetype, sep = '')
  mov_name_to_use = paste0(mov_folder, mov_name, '.mpg', sep = '')
  if (filetype == 'png'){
    png(filename, height = dim(img_stack)[1], width = dim(img_stack)[2])
  } else if (filetype == 'jpg'){
    jpeg(filename, height = dim(img_stack)[1], width = dim(img_stack)[2])
  }
  
  im_num = dim(img_stack)[3]
  for (i in seq(im_num)){
    
    image(img_stack[, , i], zlim = c(0, 100), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
    
    print(paste0(i, ' of ', im_num))
  }
  dev.off()
  
}



combine_sites_to_landscape <- function(current_ecology, land_parcels, landscape_dims, feature_num){
  
  landscape = array(0, parcels$landscape_dims)
  
  landscape[unlist(land_parcels)] = unlist(current_ecology[[feature_ind]])
  
  return(landscape)
}



#     if ((realisation_ind == 1) && (feature_ind == 1)){
#       mov_folder = paste0(global_params$collated_folder, '/mov_', 
#                           formatC(scenario_ind, width = 3, format = "d", flag = "0"), '/')
#       if( (global_params$write_movie == TRUE) || (global_params$write_offset_layer == TRUE) ){
#         if(!(file.exists(mov_folder))){
#           dir.create(mov_folder)
#         }
#       }
#       if ( global_params$write_movie == TRUE){
#         write_frames(current_data_stack, filetype = 'png', mov_folder, data_object$site_characteristics_object, global_params, current_simulation_params, 
#                      offset_site_indexes = unlist(output_object$offsets$site_indexes), 
#                      offset_yrs = unlist(output_object$offsets$offset_yrs))
#       }
#     }



write_frames <- function(output_filetype, mov_folder, site_characteristics_object, global_params, current_simulation_params, offset_site_indexes, offset_yrs){
  # gray.colors(n = 1024, start = 0, end = 1, gamma = 2.2, alpha = NULL)
  graphics.off()
  rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
  
  if (!dir.exists(mov_folder)){
    dir.create(mov_folder)
  }
  filename = paste0(mov_folder, "tmp%03d.", output_filetype, sep = '')
  
  if (output_filetype == 'png'){
    png(filename, height = site_characteristics_object$landscape_dims[1], width = site_characteristics_object$landscape_dims[2])
  } else if (output_filetype == 'jpg'){
    jpeg(filename, height = site_characteristics_object$landscape_dims[1], width = site_characteristics_object$landscape_dims[2])
  }
  
  for (yr in seq(current_simulation_params$time_steps)){

    landscape = array(0, site_characteristics_object$landscape_dims)
   
    if (global_params$write_offset_layer == TRUE){
      offset_sites_to_use = which(offset_yrs <= yr)
      landscape[unlist(site_characteristics_object$land_parcels[offset_site_indexes[offset_sites_to_use] ])] = current_simulation_params$max_eco_val
    } 
    image(landscape, zlim = c(0, current_simulation_params$max_eco_val), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
    
    cat('\n image ', yr, ' of ', current_simulation_params$time_steps, 'done\n')
  }
  
  dev.off()
  
}

