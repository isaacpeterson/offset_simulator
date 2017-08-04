

check_plot_options <- function() {

  if(plot_type != 'impacts' & plot_type != 'outcomes')
    stop( paste('\nERROR: Illegal plot option specified. Variable plot_type is set to', plot_type) )
}

plot_outcome_set <- function(collated_realisations, current_policy_params, site_plot_lims, program_plot_lims, 
                             landscape_plot_lims, sets_to_plot, lwd_vec, time_steps, realisation_num){
  
  offset_col_vec = c('blue', 'red', 'darkgreen')
  dev_col_vec = c('blue', 'red')
  net_col_vec = c('darkgreen', 'red', 'black')
  
  plot_site_outcomes(collated_realisations, current_policy_params, sets_to_plot, site_plot_lims)
  
  plot_outcomes(collated_realisations$program_outcomes$net, 
                plot_type = 'program', 
                enforce_limits = TRUE, 
                include_legend = FALSE, 
                y_lims = program_plot_lims,
                plot_title = 'Program Outcome', 
                loss_stats = collated_realisations$net_program_loss, 
                realisation_num, 
                collated_realisations$program_cfacs$program_cfac_sum,
                lwd_vec, 
                col_vec = c('red', 'blue'), 
                legend_vec = c('Outcome', 'Counterfactual'), 
                time_steps)
  
  
  plot_outcomes(collated_realisations$landscape$net_landscape, 
                plot_type = 'landscape', 
                enforce_limits = TRUE, 
                include_legend = FALSE, 
                y_lims = landscape_plot_lims,
                plot_title = 'Landscape Outcome', 
                loss_stats = collated_realisations$landscape_loss, 
                realisation_num, 
                collated_realisations$landscape$landscape_cfacs[[1]], 
                lwd_vec, 
                col_vec = c('red', 'blue'), 
                legend_vec = c('Outcome', 'Counterfactual'), 
                time_steps = time_steps)
}


plot_site_outcomes <- function(collated_realisations, current_policy_params, sets_to_plot, site_plot_lims){
  y_lab = get_y_lab(current_policy_params)

  if (current_policy_params$use_offset_bank == TRUE){
    offset_parcel_indexes_to_use = collated_realisations$collated_offset_bank$parcel_indexes
    dev_parcel_indexes_to_use = collated_realisations$collated_dev_credit$parcel_indexes
  } else{
    offset_parcel_indexes_to_use = collated_realisations$collated_offsets$parcel_indexes
    dev_parcel_indexes_to_use = collated_realisations$collated_devs$parcel_indexes
  }
 overlay_trajectories(dev_parcel_indexes_to_use,
                      current_policy_params$use_offset_bank,
                       trajectories = collated_realisations$landscape$summed_site_trajectories, 
                       realisation_ind = 1, 
                       plot_col = 'red', 
                       plot_type = 'non-overlay', 
                       overlay_type = 'single', 
                       sets_to_plot, 
                       y_lab, 
                       site_plot_lims)
  
  overlay_trajectories(offset_parcel_indexes_to_use, 
                       current_policy_params$use_offset_bank,
                       trajectories = collated_realisations$landscape$summed_site_trajectories, 
                       realisation_ind = 1, 
                       plot_col = 'darkgreen', 
                       plot_type = 'overlay', 
                       overlay_type = 'single', 
                       sets_to_plot, 
                       y_lab, 
                       site_plot_lims)
  
}


# current_policy_params = policy_params
# site_plot_lims = c(-1e4, 1e4)
# program_plot_lims = c(-6e5, 6e5) 
# landscape_plot_lims = c(-6e5, 6e5) 
# 
# lwd_vec = c(3, 0.5) 
# time_steps = run_params$time_steps 
# parcel_num = parcels$land_parcel_num
# realisation_num = run_params$realisation_num
plot_impact_set <- function(collated_realisations, current_policy_params, site_plot_lims, program_plot_lims, landscape_plot_lims, 
                            sets_to_plot, lwd_vec, time_steps, parcel_num, realisation_num){
  
  offset_col_vec = c('blue', 'red', 'darkgreen')
  dev_col_vec = c('blue', 'red')
  net_col_vec = c('darkgreen', 'red', 'black')
  
  overlay_parcel_sets(collated_realisations, 
                      current_policy_params,
                      realisation_ind = 1, 
                      eco_ind = 1, 
                      plot_from_impact_yr = FALSE, 
                      sets_to_plot,
                      site_plot_lims,
                      time_steps)
  
  overlay_realisations(plot_list = list(collated_realisations$program_impacts$net_offset_gains, 
                                        collated_realisations$program_impacts$net_dev_losses,
                                        collated_realisations$program_impacts$net_program),
                       plot_title = 'Program Impact', 
                       x_lab = paste('Program ', write_NNL_label(collated_realisations$program_NNL$NNL_mean)),
                       realisation_num,
                       lwd_vec, 
                       col_vec = net_col_vec, 
                       legend_loc = 'topleft',
                       legend_vec = c('Net Offset Impact', 'Net Development Impact', 'Net Impact'), 
                       plot_lims = program_plot_lims)
  
  plot_list = list(collated_realisations$landscape$landscape_impact)
  
  x_lab = cbind(paste('System ', write_NNL_label(collated_realisations$system_NNL$NNL_mean)), 
                paste('dev sites =', collated_realisations$parcel_nums_used$mean_dev_num, ', offset sites=', collated_realisations$parcel_nums_used$mean_offset_num, 'of ', parcel_num))
  overlay_realisations(plot_list,
                       plot_title = 'Landscape Impact', 
                       x_lab = t(x_lab),
                       realisation_num,
                       lwd_vec, 
                       col_vec = 'black',
                       legend_loc = 'topright',
                       legend_vec = 'NA', 
                       landscape_plot_lims) 
  
}


make_movie <- function(){
  if (show_movie == TRUE){ #combine outputs in list cell format to list of 3D arrays for each eco dimension "net_traj"
    net_traj <- form_net_trajectory(trajectories_list = realisations[[1]]$trajectories, land_parcels= parcels$land_parcels, 
                                    time_steps = run_params$time_steps, landscape_dims = parcels$landscape_dims, eco_dims = ecology_params$eco_dims)
    graphics.off()
    for (yr in seq_len(run_params$time_steps)){
      image(net_traj[[1]][, , yr], zlim = c(ecology_params$min_eco_val, ecology_params$max_eco_val)) #output to series of image slices to build into movie using something like ImageJ
      Sys.sleep(0.1)
      print(paste('year = ', yr))
    }
  }
  
  
  if (write_movie == TRUE){
    net_traj <- form_net_trajectory(trajectories_list = realisations[[1]]$trajectories, land_parcels= parcels$land_parcels, 
                                    time_steps = run_params$time_steps, landscape_dims = parcels$landscape_dims, eco_dims = ecology_params$eco_dims)
    make_mov(img_stack = net_traj[[1]], filetype = 'png', mov_name = 'long_offsets', mov_folder = paste0(output_folder, 'offset_time_slice/'))
  }
  
  if (write_offset_layer == TRUE){ #write all offset parcels to single layer to output as image
    
    rgb.palette <- colorRampPalette(c("black", "green"), space = "rgb")
    offset_layer <- generate_offset_layer(trajectories = realisations[[1]]$trajectories, 
                                          layer_type = 'offset', 
                                          program_parcels = unlist(realisations[[1]]$index_object$offsets),
                                          land_parcels = parcels$land_parcels, 
                                          time_steps = run_params$time_steps, 
                                          landscape_dims = parcels$landscape_dims, 
                                          eco_dims = ecology_params$eco_dims)
    
    png(filename = paste0(output_folder, 'offset_layer.png'), height = dim(offset_layer$layer)[1], width = dim(offset_layer$layer)[2])
    image(offset_layer$layer, zlim = c(0,1), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
    dev.off()
    
    rgb.palette <- colorRampPalette(c("black", "red"), space = "rgb")
    dev_layer <- generate_offset_layer(trajectories = realisations[[1]]$trajectories, 
                                       layer_type = 'development', 
                                       program_parcels = unlist(realisations[[1]]$index_object$developments),
                                       land_parcels = parcels$land_parcels, 
                                       time_steps = run_params$time_steps, 
                                       landscape_dims = parcels$landscape_dims, 
                                       eco_dims = ecology_params$eco_dims) #write all developed parcels to single layer to output as image
    
    png(filename = paste0(output_folder, 'dev_layer.png'), height = dim(dev_layer$layer)[1], width = dim(dev_layer$layer)[2])
    image(dev_layer$layer, zlim = c(0,1), col = rgb.palette(512)) #, col = grey(seq(0, 1, length = 256))
    dev.off()
    
  }
}

plot_single_policy_collated_realisations <- function(collated_realisations, realisation_num, run_params, ecology_params, policy_params, 
                                                     parcel_sum_lims, eco_ind, lwd_vec){
  
  offset_col_vec = c('blue', 'red', 'darkgreen')
  dev_col_vec = c('blue', 'red')
  net_col_vec = c('darkgreen', 'red', 'black')
  
  time_horizon = run_params$time_steps
  eco_dims = ecology_params$eco_dims
  
  if (policy_params$use_parcel_sets == TRUE){
    setup_sub_plots(nx = 3, ny = 3, x_space = 5, y_space = 5)
    plot_parcel_sets(collated_realisations, 
                     realisation_ind = 1, 
                     eco_ind, 
                     col_list = list(offset_col_vec, dev_col_vec, net_col_vec), 
                     legend_loc = 'topleft')
  }
  
  setup_sub_plots(nx = 3, ny = 3, x_space = 5, y_space = 5)
  
  system_NNL = collated_realisations$system_NNL
  
  parcel_set_NNL = collated_realisations$parcel_set_NNL
  
  plot_lims <- find_plot_lims(plot_list = list(collated_realisations$net_offset_gains$net_outcome, 
                                               collated_realisations$net_dev_losses$net_outcome,
                                               collated_realisations$net_program_outcomes,
                                               collated_realisations$net_offset_gains$losses))
  
  overlay_realisations(plot_list = list(collated_realisations$net_offset_gains$rest_gains,
                                        collated_realisations$net_offset_gains$avoided_degs,
                                        #collated_realisations$net_offset_gains$losses,
                                        collated_realisations$net_offset_gains$net_outcome),
                       plot_title = 'Program Scale Offset Impact', 
                       x_lab = '',
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = offset_col_vec, 
                       legend_loc = 'topleft',
                       legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Offset Impact'), 
                       plot_lims = plot_lims) 
  
  overlay_realisations(plot_list = list(collated_realisations$net_dev_losses$rest_gains,
                                        collated_realisations$net_dev_losses$net_outcome),
                       plot_title = 'Program Scale Development Impact', 
                       x_lab = '',
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = dev_col_vec, 
                       legend_loc = 'topleft',
                       legend_vec = c('Development Loss', 'Development Impact'), 
                       plot_lims = plot_lims) 
  
  overlay_realisations(plot_list = list(collated_realisations$net_offset_gains$net_outcome, 
                                        collated_realisations$net_dev_losses$net_outcome,
                                        collated_realisations$net_program_outcomes),
                       plot_title = 'Program Impact', 
                       x_lab = system_NNL$x_lab,
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = net_col_vec, 
                       legend_loc = 'topleft',
                       legend_vec = c('Net Offset Impact', 'Net Development Impact', 'Net Impact'), 
                       plot_lims = plot_lims)
  
  plot_NNL_hists(collated_realisations$parcel_set_NNL, 
                 collated_realisations$program_NNL,
                 collated_realisations$system_NNL,
                 use_parcel_sets = policy_params$use_parcel_sets, 
                 eco_ind)
  
  plot_outcomes(collated_realisations$program_sums$outcome_rel_initial, 
                plot_type = 'program', 
                enforce_limits = TRUE, 
                include_legend = FALSE, 
                y_lims = vector(),
                plot_title = 'Program Outcome', 
                loss_stats = collated_realisations$net_program_loss, 
                realisation_num, 
                collated_realisations$program_cfac_sum_rel_initial,
                eco_ind, 
                lwd_vec, 
                col_vec = c('red', 'blue'), 
                legend_vec = c('Program Outcome', 'Program Counterfactual'), 
                time_steps = run_params$time_steps)
  
  
  #   plot_collated_realisation_set(collated_realisations$landscape_rel_to_cfac_including_clearing, overlay_plots = FALSE, plot_col = 'black', realisation_num, lwd_vec, 
  #                                 x_lab = '', plot_title = 'Program Outcome', plot_lims = vector())
  # 
  plot_outcomes(collated_realisations$net_landscape, 
                plot_type = 'landscape', 
                enforce_limits = TRUE, 
                include_legend = FALSE, 
                y_lims = vector(),
                plot_title = 'Landscape Outcome', 
                loss_stats = collated_realisations$landscape_loss, 
                realisation_num, 
                collated_realisations$net_cfac_sum, 
                eco_ind, lwd_vec, 
                col_vec = c('red', 'blue'), 
                legend_vec = c('Landscape Outcome', 'Landscape Counterfactual'), 
                time_steps = run_params$time_steps)
  
  
  plot_list = list(collated_realisations$landscape_rel_to_cfac_including_clearing)
  overlay_realisations(plot_list,
                       plot_title = 'Lamdscape Impact', 
                       x_lab = '',
                       realisation_num,
                       eco_ind, 
                       lwd_vec, 
                       col_vec = c('black'),
                       legend_loc = 'topright',
                       legend_vec = 'Landscape Impact', 
                       plot_lims = c(min(unlist(plot_list)), (max(unlist(plot_list))) + 0.25*max(abs(unlist(plot_list))))) 
  
}


write_NNL_label <- function(NNL_yrs){
  if (length(unlist(NNL_yrs)) > 0){
    NNL_label = paste('NNL at ', round(mean(unlist(NNL_yrs))), ' years')
  } else {
    NNL_label = 'All realisations faileld NNL'
  } 
  return(NNL_label)
}




# parcel_indexes = collated_realisations$collated_devs$parcel_indexes
# offset_bank = current_policy_params$use_offset_bank
# trajectories = collated_realisations$landscape$summed_site_trajectories
# realisation_ind = 1
# plot_col = 'red' 
# plot_type = 'non-overlay' 
# overlay_type = 'single' 


overlay_trajectories <- function(parcel_indexes_to_use, offset_bank, trajectories, realisation_ind, plot_col, plot_type, 
                                 overlay_type, sets_to_plot, y_lab, site_plot_lims){
  if (offset_bank == FALSE){
      parcel_indexes_to_use = unlist(parcel_indexes_to_use[[realisation_ind]][sets_to_plot])
      plot_list = trajectories[[realisation_ind]][parcel_indexes_to_use]
  } else{
    parcel_indexes_to_use = unlist(parcel_indexes_to_use[[realisation_ind]])
    plot_list = list(Reduce('+', trajectories[[realisation_ind]][parcel_indexes_to_use]))
  }
  overlay_plot_list(plot_type, plot_list, yticks = 'y', ylims = site_plot_lims, heading = 'Site Outcomes', ylab = y_lab, x_lab = '', 
                    col_vec = rep(plot_col, length(plot_list)), lty_vec = rep(1, length(plot_list)), lwd_vec = rep(0.5, length(plot_list)), 
                    legend_vec = 'NA', legend_loc = FALSE)
}



get_y_lab <- function(current_policy_params){
  
  y_lab = current_policy_params$offset_calc_type
  if (current_policy_params$use_offset_bank == FALSE){
    y_lab = paste(y_lab, 'T.H =', current_policy_params$offset_time_horizon, ', Clearing ', current_policy_params$include_illegal_clearing_in_offset_calc)
  } else{
    y_lab = paste(y_lab, ' offset_bank T, Clearing ', current_policy_params$include_illegal_clearing_in_offset_calc)
  }
  
  return(y_lab)
}



overlay_parcel_sets <- function(collated_realisations, current_policy_params, realisation_ind, eco_ind, plot_from_impact_yr, sets_to_plot, site_plot_lims, time_steps){
  y_lab = get_y_lab(current_policy_params)
  plot_lwd = 1
  
  if (current_policy_params$use_offset_bank == FALSE){
    offset_set = collated_realisations$collated_offsets
    dev_set = collated_realisations$collated_devs
    net_plot_list = collated_realisations$program_impacts$net_site_scale[[realisation_ind]][sets_to_plot]
    
  } else {
    offset_set = collated_realisations$program_impacts$net_offset_gains
    dev_set = collated_realisations$program_impacts$net_dev_losses
    net_plot_list = collated_realisations$program_impacts$net_program[[realisation_ind]]
    
  }
  
  overlay_parcel_set_element(collated_object = offset_set,
                             current_policy_params$use_offset_bank,
                             visualisation_type = 'stacked', 
                             realisation_ind, 
                             plot_col = 'darkgreen', 
                             plot_lwd,
                             plot_type = 'non-overlay',
                             y_lab,
                             plot_from_impact_yr,
                             sets_to_plot, 
                             site_plot_lims, 
                             time_steps)
  
  overlay_parcel_set_element(dev_set,
                             current_policy_params$use_offset_bank,
                             visualisation_type = 'non-stacked', 
                             realisation_ind, 
                             plot_col = 'red',
                             plot_lwd,
                             plot_type = 'overlay',
                             y_lab = '',
                             plot_from_impact_yr,
                             sets_to_plot, 
                             site_plot_lims, 
                             time_steps)
  
  overlay_plot_list(plot_type = 'overlay', net_plot_list, yticks = 'y', ylims = site_plot_lims, heading = 'Site Outcomes', ylab = '', x_lab = '', 
                    col_vec = rep('black', length(net_plot_list)), lty_vec = rep(1, length(net_plot_list)), lwd_vec = rep(plot_lwd, length(net_plot_list)), 
                    legend_vec = 'NA', legend_loc = FALSE)
  
}




overlay_parcel_set_element <- function(collated_object, offset_bank, visualisation_type, realisation_ind, 
                                       plot_col, plot_lwd, plot_type, y_lab, plot_from_impact_yr, 
                                       sets_to_plot, plot_lims, time_steps){
  
  if (offset_bank == FALSE){
    collated_traj_set = collated_object$site_nets[[realisation_ind]]
    parcel_indexes = unlist(collated_object$parcel_indexes[[realisation_ind]][sets_to_plot])
    inds_to_plot = which(unlist(collated_object$parcel_indexes[[realisation_ind]]) %in% parcel_indexes)
    if (plot_from_impact_yr){
      offset_yrs = collated_object$offset_yrs[[realisation_ind]][inds_to_plot]
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
  overlay_plot_list(plot_type, plot_list, yticks = 'y', ylims = plot_lims, heading = 'Site Impact', y_lab, x_lab = '', 
                    col_vec = rep(plot_col, length(plot_list)), lty_vec = rep(1, length(plot_list)), lwd_vec = rep(plot_lwd, length(plot_list)), 
                    legend_vec = 'NA', legend_loc = FALSE)
}




plot_split_realisations <- function(plot_type, rest_gains, avoided_degs, nets, plot_title, eco_ind, lwd_vec, col_vec, legend_vec, legend_pos, realisation_num, ylim){
  
  plot_num = length(col_vec)
  plot_collated_realisation_set(rest_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)
  if (plot_type == 'offsets'){
    plot_collated_realisation_set(avoided_degs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)
  }
  plot_collated_realisation_set(nets, overlay_plots = TRUE, plot_col = col_vec[plot_num], realisation_num, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = ylim)  
  legend(legend_pos, legend_vec, bty="n", lty = rep(2, plot_num), lwd = array(lwd_vec[1], plot_num), col = col_vec)
}



plot_collated_realisation_set <- function(current_collated_real, overlay_plots, plot_col, realisation_num, lwd_vec, x_lab, plot_title, plot_lims){
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
  
  realisation_sum = Reduce('+', current_collated_real)
  realisation_mean = realisation_sum/realisation_num
  if (overlay_plots == FALSE){
    plot(current_collated_real[[1]], type = 'l', ylab = '', main = plot_title, xlab = x_lab, ylim = c(mn, mx), col = back_plot_col, lwd = lwd_vec[2])
  } else { lines(current_collated_real[[1]], lwd = lwd_vec[2], col = back_plot_col)
  }
  
  if (realisation_num > 1){
    for (realisation_ind in 2:realisation_num){
      lines(current_collated_real[[realisation_ind]], col = back_plot_col, lwd = lwd_vec[2])
    }
  }
  lines(realisation_mean, ylim = c(mn, mx), col = plot_col, lwd = lwd_vec[1], lty = 2)
  abline(h = 0, lty = 2)
  
}



generate_single_realisation_plots <- function(run_params, ecology_params, realisations, net_cfac_sum, eco_ind){
  time_horizon = run_params$time_steps
  eco_dims = ecology_params$eco_dims
  realisation_ind = sample(length(realisations), 1)
  collated_parcel_sets_object = realisations[[realisation_ind]]$collated_parcel_sets_object
  # plot_sample_parcel_sets(collated_parcel_sets_object, plot_num = 9, global_params)
  
  setup_sub_plots(nx = 1, ny = 3, x_space = 2, y_space = 2)
  
  plot_parcel_set_from_collated_object(collated_parcel_sets_object, parcel_indexes = seq(policy_params$total_dev_num), time_horizon, ecology_params$eco_dims, 
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
  
  hist(collated_parcel_sets_object$collated_offsets$parcel_sums_at_offset, main = 'Single Realisation Selected Parcel Values', xlab = 'Selected offset parcel values')
  setup_sub_plots(nx = 3, ny = 1, x_space = 2, y_space = 2)
  total_parcel_sums = apply(parcel_trajs, MARGIN = 1, sum)
  total_counter_sums = apply(parcel_cfac_trajs, MARGIN = 1, sum)
  plot_array = cbind(t(t(total_parcel_sums)), t(t(total_counter_sums)))
  mx = max(plot_array)
  mn = min(plot_array)
  setup_sub_plots(nx = 1, ny = 2, x_space = 5, y_space = 5)
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
  
  if (NNL_plot_object$NNL_success > 0){
    x_lab = t(cbind( paste('Mean NNL at  ', round(NNL_plot_object$NNL_mean), 'years'), paste('NNL success = ', round(NNL_plot_object$NNL_success*100), '%' )))
    NNL_yrs = unlist(NNL_plot_object$NNL_yrs)
    NNL_yrs <- NNL_yrs[which(NNL_yrs > 0)]
    hist(NNL_yrs, main = plot_tit, xlab = x_lab, xlim = x_lim, breaks=seq(min(NNL_yrs),max(NNL_yrs),by=1))
    
  } else {
    null_plot()
  }
  
}




plot_mean_gains_degs <- function(summed_realisations, policy_params, realisation_num){
  
  rest_gains = sum_cols_multi(summed_realisations$rest_gains)/realisation_num
  net_degs = sum_cols_multi(summed_realisations$avoided_degs)/realisation_num
  net_gains = net_degs + rest_gains
  
  if (policy_params$adjust_cfacs_flag == TRUE){
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
# cfac_type = policy_params$cfac_type_in_offset_calc
# plot_type = 'offsets'
# legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains')
# legend_pos = 'topleft'
# plot_title = 'Net Offsets'
# lwd_vec = c(3, 0.15)
# col_vec = c('blue', 'mediumorchid4', 'darkgreen')
# ylim = c(net_mn, net_mx)


# plot_split_realisations <- function(collated_summed_reals, cfac_type, plot_type, legend_vec, legend_pos, eco_ind, lwd_vec, plot_title, col_vec, realisation_num, ylim){
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

# }

null_plot <- function(){
  plot(NULL, type= 'n', xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, ann = FALSE)
}
# 
# system_NNL = collated_realisations$system_NNL
# parcel_set_NNL = collated_realisations$parcel_set_NNL 
# parcel_sums_at_offset = collated_realisations$collated_offsets$parcel_sums_at_offset
# parcel_sum_lims = c(0, 20000)
# use_parcel_sets = policy_params$use_parcel_sets



# plot_realisation_hists(collated_realisations$system_NNL, collated_realisations$parcel_set_NNL, 
#                        collated_realisations$collated_offsets$parcel_sums_at_offset, parcel_sum_lims = c(0, 20000), 
#                        use_parcel_sets = policy_params$use_parcel_sets, parcel_set_set_NNL_object, eco_ind)

plot_parcel_sums_hist <- function(parcel_sums_at_offset, eco_ind, parcel_sum_lims){
  current_parcel_sums_at_offset = unlist(parcel_sums_at_offset, recursive = FALSE)
  parcel_sums_at_offset_array = unlist(lapply(seq_along(current_parcel_sums_at_offset), function(i) current_parcel_sums_at_offset[[i]]))
  hist(parcel_sums_at_offset_array, main = 'offset parcel values', xlab = 'selected offset parcel values', xlim = parcel_sum_lims)
}

plot_NNL_hists <- function(parcel_set_NNL, program_NNL, system_NNL, use_parcel_sets, eco_ind){
  
  if (use_parcel_sets == TRUE){
    plot_NNL_hist(parcel_set_NNL, plot_tit = 'Site scale NNL Assessment', x_lim = c(0, 100), eco_ind) 
  } else {
    null_plot()
  }
  
  plot_NNL_hist(program_NNL, plot_tit = 'Program scale NNL Assessment', x_lim = c(0, 100), eco_ind)
  plot_NNL_hist(system_NNL, plot_tit = 'Landscape scale NNL Assessment', x_lim = c(0, 100), eco_ind)
  
}


# plot_list = list(collated_realisations$program_impacts$net_offset_gains ,
#                  collated_realisations$program_impacts$net_dev_losses,
#                  collated_realisations$program_impacts$net_program_outcomes)
# plot_title = 'Program Impact' 
# col_vec = net_col_vec 
# legend_loc = 'topleft'
# legend_vec = c('Net Offset Impact', 'Net Development Impact', 'Net Impact') 

# plot_title = 'Landscape Impact' 
# x_lab = '' #t(x_lab)
# col_vec = c('black')
# legend_loc = 'topright'
# legend_vec = 'NA' 
# plot_lims = landscape_plot_lims


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



# landscape_realisations = collated_realisations$landscape$net_landscape 
# plot_type = 'landscape' 
# enforce_limits = TRUE 
# include_legend = FALSE 
# y_lims = landscape_plot_lims
# plot_title = 'Landscape Outcome' 
# loss_stats = collated_realisations$landscape_loss 
# realisation_num 
# cfacs = collated_realisations$landscape$landscape_cfacs 
# lwd_vec 
# col_vec = c('red', 'blue') 
# legend_vec = c('Outcome', 'Counterfactual') 

# time_steps = time_steps

plot_outcomes <- function(landscape_realisations, plot_type, enforce_limits, include_legend, y_lims, plot_title, loss_stats, realisation_num, 
                          cfacs, lwd_vec, col_vec, legend_vec, time_steps){
  
  #current_total_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_stats$total_loss[[i]]))
  #loss_tit = paste('Net Loss at ', time_steps, 'yrs = ', round(mean(unlist(current_total_loss))*100), '%')
  
  #current_NNL_loss = unlist(lapply(seq_len(realisation_num), function(i) loss_stats$NNL_loss[[i]]))
#   if (length(current_NNL_loss) > 0){
#     NNL_tit = paste('Mean NNL at  ', round(mean(current_NNL_loss*100)), '% landscape loss')
#   } else {
#     NNL_tit = 'All realisations failed NNL'
#   }
#   sub_tit = cbind(NNL_tit, loss_tit)
  sub_tit = ''
  
  if (enforce_limits == TRUE){
    plot_lims = y_lims
  } else {
    plot_vec = c(unlist(landscape_realisations), unlist(current_cfacs))
    plot_lims = c(min(plot_vec), max(plot_vec))
  }
  plot_collated_realisation_set(landscape_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, lwd_vec, 
                                x_lab = sub_tit, plot_title = plot_title, plot_lims)
  
  if (plot_type == 'program'){
    plot_collated_realisation_set(cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, lwd_vec, 
                                  x_lab = '', plot_title = '', plot_lims = y_lims)
  } else {
    lines(cfacs, col = col_vec[2], lty = 2, lwd = 2)
  }
  
  #abline(h = mean(landscape_realisations[1, , ]), lty = 2)
  if (include_legend == TRUE){
    legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  }

}




plot_program_evaluation <- function(summed_program_realisations, loss_stats, realisation_num, summed_program_cfacs, eco_ind, lwd_vec, col_vec, legend_vec){
  
  #setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  
  loss_tit = paste('Net program loss = ', round(mean(loss_stats$total_loss)*100), '%')
  
  if (length(loss_stats$NNL_loss) > 0){
    #landscape_title = paste('NNL at ', round(mean(loss)), '\U00b1', round(max(range(loss) - (mean(loss))), 1), '% loss of landscape')
    NNL_tit = paste('Mean NNL at  ', round(mean(loss_stats$NNL_loss*100)), '% program loss')
  } else NNL_tit = 'All realisations failed NNL'
  
  sub_tit = cbind(NNL_tit, loss_tit)
  
  plot_collated_realisation_set(summed_program_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, lwd_vec, 
                                x_lab = sub_tit, plot_title = 'Net Program Value', plot_lims = c(0, max(summed_program_realisations)))
  plot_collated_realisation_set(summed_program_cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, lwd_vec, 
                                x_lab = '', plot_title = '', plot_lims = vector())
  
  legend('bottomleft', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  
  #   plot_collated_realisation_set( (summed_program_realisations - summed_program_cfacs), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind = 1, lwd_vec, 
  #                                  x_lab = '', plot_title = 'Net Value Rel. to Counterfactual', plot_lims = vector() )
  #   

  
}






plot_parcel_set_parcels <- function(current_set_object){
  
  
  parcel_num = length(current_set_object$parcel_indexes)
  sub_plots = 1:(parcel_num*ecology_params$eco_dims)
  dim(sub_plots) = c(ecology_params$eco_dims, parcel_num)
  layout(sub_plots)
  
  rest_gains = current_set_object$rest_gains
  degs = current_set_object$avoided_degs
  lim_vec = c(rest_gains, degs, (rest_gains + degs)) 
  mx = max(lim_vec)
  mn = min(lim_vec)
  
  for (parcel_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(ecology_params$eco_dims)){
      plot(rest_gains[, eco_ind, parcel_ind], type = 'l', ylim = c(mn, mx), main = paste('parcel index =', current_set_object$parcel_indexes[parcel_ind]), ylab = '', xlab = paste('dim = ', eco_ind))
      lines(degs[, eco_ind, parcel_ind], col = 'blue')
      lines(rest_gains[, eco_ind, parcel_ind] + degs[, eco_ind, parcel_ind], col = 'red')
    }
  }
  
}





plot_sample_parcel_sets <- function(collated_parcel_sets_object, plot_num, run_params, ecology_params, policy_params){
  
  parcel_indexes = sample(policy_params$total_dev_num, plot_num)
  setup_sub_plots(nx = 3, ny = 3, x_space = 5, y_space = 5)
  for (parcel_set_index in parcel_indexes){
    plot_parcel_set_from_collated_object(collated_parcel_sets_object, parcel_set_index, time_horizon = run_params$time_steps, ecology_params$eco_dims, 
                                         headings = c('Parcel Set Developments', 'Parcel Set Offsets', 'Parcel Set Outcome'))
  }
}




setup_sub_plots <- function(nx, ny, x_space, y_space){
  par(mfrow = c(ny, nx))
  par(cex = 0.6)
  par(mar = c(x_space, y_space, 1, 0), oma = c(2, 4, 2.5, 0.5))
  
  par(tcl = -0.25)
  par(mgp = c(2, 0.6, 0))
  
}




collate_parcel_set_element <- function(rest_gains, avoided_degs){
  collated_outs = list()
  collated_outs$rest_gains = rest_gains
  collated_outs$avoided_degs = avoided_degs
  collated_outs$nets = rest_gains + avoided_degs
  return(collated_outs)
}




plot_parcel_sets <- function(collated_realisations, realisation_ind, eco_ind, col_list, legend_loc){
  
  parcel_set_num = collated_realisations$collated_devs$parcel_set_num[[realisation_ind]]
  parcel_sets_to_plot = sample(parcel_set_num, min(parcel_set_num, 3))
  
  for (parcel_set_ind in parcel_sets_to_plot){
    parcel_set_traj_list = list()
    offset_plot_list = list(collated_realisations$collated_offsets$rest_gains[[realisation_ind]][, parcel_set_ind], 
                            collated_realisations$collated_offsets$avoided_degs[[realisation_ind]][, parcel_set_ind],
                            collated_realisations$collated_offsets$nets[[realisation_ind]][, parcel_set_ind])
    
    dev_plot_list = list(collated_realisations$collated_devs$rest_gains[[realisation_ind]][, parcel_set_ind], 
                         collated_realisations$collated_devs$nets[[realisation_ind]][, parcel_set_ind])
    
    net_plot_list = list(collated_realisations$collated_offsets$nets[[realisation_ind]][, parcel_set_ind],
                         collated_realisations$collated_devs$nets[[realisation_ind]][, parcel_set_ind], 
                         collated_realisations$parcel_set_outcomes[[realisation_ind]][, parcel_set_ind])
    
    plot_lims <- find_plot_lims(list(offset_plot_list, dev_plot_list, net_plot_list))
    
    overlay_plot_list(plot_type = 'non-overlay', plot_list = offset_plot_list, yticks = 'y', x_lab = '', ylims = plot_lims, heading = 'Offset Impacts', ylab = '', col_vec = col_list[[1]], lty_vec = c(1, 1, 2), 
                      lwd_vec = c(1, 1, 2), legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Relative Gains'), legend_loc)
    overlay_plot_list(plot_type = 'non-overlay', plot_list = dev_plot_list, yticks = 'y', x_lab = '', ylims = plot_lims, heading = 'Development Evaluation', ylab = '', col_vec = col_list[[2]], lty_vec = c(1, 2), 
                      lwd_vec = c(1, 2), legend_vec = c('Development Loss', 'Development Impact'), legend_loc)
    overlay_plot_list(plot_type = 'non-overlay', plot_list = net_plot_list, yticks = 'y', x_lab = '', ylims = plot_lims, heading = 'Site Scale Evaluation', ylab = '', col_vec = col_list[[3]], lty_vec = c(2, 2, 1), 
                      lwd_vec= c(2, 2, 3), legend_vec = c('Offset Impact', 'Development Impact', 'Net Impact'), legend_loc)
    
  }
}

find_plot_lims <- function(plot_list){
  mn = min(unlist(plot_list))
  mx = max(unlist(plot_list))
  plot_lims = c(mn, mx)
  return(plot_lims)
}



overlay_plot_list <- function(plot_type, plot_list, yticks, ylims, heading, ylab, x_lab, col_vec, lty_vec, lwd_vec, legend_vec, legend_loc){
  
  if (plot_type == 'non-overlay'){
    plot(plot_list[[1]], type = 'l', main = heading, ylim = ylims, ylab = ylab, xlab = x_lab, col = col_vec[1], lty = lty_vec[1], lwd = lwd_vec[1])
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
