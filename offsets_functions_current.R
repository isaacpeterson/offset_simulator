# run the model and return outputs
run_offsets_model <- function(global_params, region_params, initial_ecology, cfac_parcel_trajs, decline_rates_initial, parcels){
  
  if (global_params$set_seed == TRUE){
    set.seed(123)
  }
  outs = list()

  outs$model_outputs <- run_system(global_params, region_params, current_ecology = initial_ecology, decline_rates = decline_rates_initial, parcels, index_object)                   # run the model
  parcel_sets_cfacs <- calc_parcel_set_cfacs(outs$model_outputs, global_params, decline_rates_initial)
  outs$collated_parcel_sets_object <- collate_parcel_sets_object(outs$model_outputs, parcel_sets_cfacs, land_parcels, global_params, decline_rates_initial)             
  
  outs$program_cfacs <- write_program_cfacs(cfac_parcel_trajs, offset_parcel_indexes = outs$model_outputs$offset_list, dev_parcel_indexes = outs$model_outputs$development_list)
  
  return(outs)
}  
  
write_program_cfacs <- function(cfac_parcel_trajs, offset_parcel_indexes, dev_parcel_indexes){
  program_cfacs = list()
  program_cfacs$offsets = sum_program_cfacs(cfac_parcel_trajs, offset_parcel_indexes)
  program_cfacs$devs =  sum_program_cfacs(cfac_parcel_trajs, dev_parcel_indexes)
  program_cfacs$net = sum_program_cfacs(cfac_parcel_trajs, c(offset_parcel_indexes, dev_parcel_indexes))
  return(program_cfacs)
}

assess_failed_NNL <- function(NNLs){
  NNL_fails = which(NNLs == 0)
  if (length(NNL_fails) > 0){
    failed_frac = length(NNL_fails)/length(NNLs)
    NNLs = NNLs[-NNL_fails]
  } else {failed_frac = 0}
  
  if (failed_frac < 1){
    x_lab = paste('NNL at', round(mean(NNLs)), '\U00b1', round(max(range(NNLs) - (mean(NNLs))), 1), 'years', '(NNL success = ', round((1 - failed_frac)*100), '%)' )
  } else x_lab = 'All realisations failed NNL'
                
  NNL_plot_object = list()
  NNL_plot_object$failed_frac = failed_frac
  NNL_plot_object$NNLs = NNLs
  NNL_plot_object$x_lab = x_lab
  return(NNL_plot_object)
}

plot_NNL_hist <- function(NNL_plot_object, plot_tit, x_lab, x_lim){
  if (length(NNL_plot_object$NNLs) > 0){
    hist(NNL_plot_object$NNLs, main = plot_tit, xlab = NNL_plot_object$x_lab, xlim = x_lim, breaks=seq(min(NNL_plot_object$NNLs),max(NNL_plot_object$NNLs),by=1))
  } else {x_lab = ('All realisations failed NNL')}
  
}






#plot all realisations in restorations gains/avoided degredation form split into offsets and developments
plot_split_realisations <- function(collated_realisations, eco_ind, lwd_vec, outer_title, col_vec_1, col_vec_2){
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  plot_summed_realisations(collated_realisations$summed_offset_realisations, plot_title = 'Offset Realisations (Program Scale)', eco_ind, 
                         lwd_vec, col_vec = col_vec_1, legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Gains'))
  plot_summed_realisations(collated_realisations$summed_dev_realisations, plot_title = 'Development Realisations (Program Scale)', eco_ind, 
                         lwd_vec, col_vec = col_vec_2, legend_vec = c('Restoration Gains', 'Avoided Degredation', 'Net Losses'))
  title(outer_title, outer = TRUE)
}


plot_realisations_hists <- function(system_NNL_plot_object, collated_realisations, parcel_sum_lims, outer_title){
  
  setup_sub_plots(nx = 1, ny = 3, x_tit = TRUE)
  
  plot_NNL_hist(system_NNL_plot_object, plot_tit = 'System NNL Assessment', x_lim = c(0, 100))
  
  parcel_set_NNL_plot_object = assess_failed_NNL(collated_realisations$ALL_parcel_set_NNLs)
  plot_NNL_hist(parcel_set_NNL_plot_object, plot_tit = 'Parcel Set NNL Assessment', x_lim = c(0, 100)) 
  
  hist(collated_realisations$ALL_offset_initial_parcel_sums, main = 'selected offset parcel values', xlab = 'selected offset parcel values', 
       xlim = parcel_sum_lims)
  title(outer_title, outer = TRUE)
  
}

plot_realisation_outcomes <- function(collated_realisations, plot_title, x_lab, eco_ind, lwd_vec, col_vec, legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 1, x_tit = TRUE)

  dev_losses = collated_realisations$summed_dev_realisations$net_gains
  offset_gains = collated_realisations$summed_offset_realisations$net_gains
  net_outcome = collated_realisations$net_parcel_set_realisations
  plot_lims = c(min(cbind(net_outcome, dev_losses, offset_gains)), max(cbind(net_outcome, dev_losses, offset_gains)))

  plot_collated_realisation_set(net_outcome, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                              x_lab = x_lab, plot_title = plot_title, plot_lims = plot_lims)
  plot_collated_realisation_set(dev_losses, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                              x_lab = '', plot_title = '', plot_lims = plot_lims)
  plot_collated_realisation_set(offset_gains, overlay_plots = TRUE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
                              x_lab = '', plot_title = '', plot_lims = plot_lims)
  
  legend('topleft', legend_vec, bty="n", lty = c(2, 2, 2), lwd = array(lwd_vec[1], 3), col = col_vec)
  
  title(outer_title, outer = TRUE)
}


plot_landscape_outcomes <- function(net_realisations, system_NNL_plot_object, net_cfac_sum, eco_ind, lwd_vec, col_vec,  legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  
  plot_collated_realisation_set(net_realisations, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = system_NNL_plot_object$x_lab, plot_title = 'Landscape Scale Value', plot_lims = c(0, max(net_realisations)))
  lines(net_cfac_sum, col = col_vec[2], lwd = 2)
  
  legend('topright', legend_vec, bty="n", lty = c(2, 1), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  plot_collated_realisation_set( (net_realisations - net_cfac_sum), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind, lwd_vec, 
                                 x_lab = system_NNL_plot_object$x_lab, plot_title = 'Landscape Value Relative to Counterfactual', plot_lims = vector())
 

  title(outer_title, outer = TRUE) 
}




plot_summed_program_realisations <- function(summed_program_realisations, collated_net_cfacs, eco_ind, lwd_vec, col_vec, legend_vec, outer_title){
  
  setup_sub_plots(nx = 1, ny = 2, x_tit = TRUE)
  
  plot_collated_realisation_set(summed_program_realisations$net_program_value, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = 'Net Program Scale Value', plot_lims = c(0, max(summed_program_realisations$net_program_value)))
  plot_collated_realisation_set(collated_net_cfacs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, 
                                x_lab = '', plot_title = 'Net Program Scale Value', plot_lims = vector())
  
  legend('topright', legend_vec, bty="n", lty = c(2, 2), lwd = array(lwd_vec[1], 2), col = col_vec[1:2])
  
  
  plot_collated_realisation_set( (collated_realisations$summed_program_realisations$net_program_value - collated_net_cfacs), overlay_plots = FALSE, plot_col = col_vec[3], realisation_num, eco_ind = 1, lwd_vec, 
                                 x_lab = '', plot_title = 'Net Program Value Relative to Counterfactual', plot_lims = vector() )
  
  
  title(outer_title, outer = TRUE)
  
}

# eco_ind = 1
# lwd_vec = c(2, 0.3)
# outer_title = plot_characteristics
# parcel_sum_lims = c(0, 20000)

plot_collated_realisations <- function(collated_realisations, realisation_num, parcel_sum_lims, eco_ind, lwd_vec, outer_title){
  
  plot_split_realisations(collated_realisations, eco_ind, lwd_vec, outer_title, col_vec_1 = c('blue', 'mediumorchid4', 'darkgreen'), 
                          col_vec_2 = c('blue', 'mediumorchid4', 'red'))  #plot all realisations in restorations gains/avoided degredation form split into offsets and developments
  
  system_NNL_plot_object = assess_failed_NNL(collated_realisations$system_NNLs)
  
  plot_realisation_outcomes(collated_realisations, plot_title = 'All Realisation Program Outcomes', x_lab = system_NNL_plot_object$x_lab, eco_ind, lwd_vec, 
                            col_vec = c('black', 'red', 'darkgreen'), legend_vec = c('net offset gains', 'net development losses', 'net program outcomes'), outer_title)
  
  
  plot_realisations_hists(system_NNL_plot_object, collated_realisations, parcel_sum_lims, outer_title)
  
   
  net_cfac_sum = apply(cfac_parcel_trajs, MARGIN = 1, sum)
  
  plot_landscape_outcomes(collated_realisations$net_realisations, system_NNL_plot_object, net_cfac_sum, eco_ind, lwd_vec, col_vec = c('red', 'blue', 'red'), 
                          legend_vec = c('landscape value', 'counterfactual value'), outer_title)

  plot_summed_program_realisations(collated_realisations$summed_program_realisations, collated_realisations$collated_net_cfacs, eco_ind, lwd_vec, col_vec = c('red', 'blue', 'black'),
                                    legend_vec = c('program scale value', 'program scale counterfactual value'), outer_title)
    
  
  #   if (length(NNL_object$NNL_yrs > 0)){
  #     xl = t(cbind(paste('parcel set mean years to NNL = ', round(mean(NNL_object$NNL_yrs))), paste('NNL success = ', NNL_object$success*100, '%' )))
  #     hist(NNL_object$NNL_yrs, main = '', xlab = xl)
  #   }
  
}


plot_summed_realisations <- function(collated_summed_reals, plot_title, eco_ind, lwd_vec, col_vec, legend_vec){
  
  degs = collated_summed_reals$avoided_degs
  rest_gains = collated_summed_reals$rest_gains
  nets = collated_summed_reals$net_gains
  mx = max(cbind(degs, rest_gains, nets))
  mn = min(cbind(degs, rest_gains, nets))
  
  plot_collated_realisation_set(rest_gains, overlay_plots = FALSE, plot_col = col_vec[1], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))
  plot_collated_realisation_set(degs, overlay_plots = TRUE, plot_col = col_vec[2], realisation_num, eco_ind, lwd_vec, x_lab = '', plot_title = plot_title, plot_lims = c(mn, mx))
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
  
  
  for (realisation_ind in 2:realisation_num){
    lines(collated_realisations[, realisation_ind, eco_ind], col = back_plot_col, lwd = lwd_vec[2])
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
  
  hist(collated_parcel_sets_object$offsets$initial_parcel_sums, main = 'Single Realisation Selected Parcel Values', xlab = 'Selected offset parcel values')
  setup_sub_plots(nx = 3, ny = 1, x_tit = FALSE)
  total_parcel_sums = apply(parcel_trajs, MARGIN = 1, sum)
  total_counter_sums = apply(cfac_parcel_trajs, MARGIN = 1, sum)
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


find_development_vector <- function(time_steps, total_dev_num, sd, min_width, last_dev){
  dev_vec = array(0, time_steps)
  dev_vec[1:(time_steps - last_dev)] = split_vector((time_steps - last_dev), total_dev_num, sd, min_width)
  return(dev_vec)
}
  
split_vector <- function(N, M, sd, min_width) {

  vec <- rnorm(N, M/N, sd)
  if (abs(sum(vec)) < 0.01) vec <- vec + 1
  vec <- round(vec / sum(vec) * M)
  deviation <- M - sum(vec)
  for (. in seq_len(abs(deviation))) {
    vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
  }
  while (any(vec <= min_width)) {
    negs <- vec <= min_width
    pos  <- vec > min_width
    vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
    vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
  }
  vec
}


initialise_shape_parcels <- function(global_params){
  parcels = list()
  parcel_num_x = global_params$parcel_num_x
  parcel_num_y = global_params$parcel_num_y
  parcel_vx = split_vector(parcel_num_x, global_params$ecology_size, sd = 5, min_width = 3)
  parcel_vy = split_vector(parcel_num_y, global_params$ecology_size, sd = 5, min_width = 3)
  
  pixel_indexes = 1:(global_params$ecology_size*global_params$ecology_size)
  dim(pixel_indexes) = c(global_params$ecology_size, global_params$ecology_size)
  land_parcels = mcell(pixel_indexes, parcel_vx, parcel_vy) #lit the ecology array into a series of subarrays with dimensions sz_x by sz_y
  land_parcel_num = length(land_parcels$elements) #total number of parcels
  parcel_indexes = 1:land_parcel_num #index all parcels
  dim(parcel_indexes) = c(parcel_num_y, parcel_num_x) #arrange indicies into array with dimensions of land parcels
  region_vx = split_vector(global_params$region_num_x, parcel_num_x, 1, min_width = 3) 
  region_vy = split_vector(global_params$region_num_y, parcel_num_y, 1, min_width = 3)
  
  regions = mcell(parcel_indexes, region_vx, region_vy)
  
  region_num = length(regions$elements)
  parcels$parcel_indexes = parcel_indexes
  parcels$land_parcel_num = land_parcel_num
  parcels$land_parcels = land_parcels$elements
  parcels$land_parcel_dims = land_parcels$dims
  parcels$regions = regions$elements
  parcels$region_dims = regions$dims
  parcels$region_num = region_num
  parcels$parcel_vx = parcel_vx
  parcels$parcel_vy = parcel_vy
  
  return(parcels)
}


initialise_index_object <- function(parcels, global_params){
  index_object = list()
  index_object$ind_available = parcels$regions
  index_object$developments = vector()
  index_object$offsets = vector()
  index_object$parcel_sets = list()
  index_object$parcel_num_remaining = array(0, c(global_params$total_dev_num, global_params$region_num))
  index_object$dev_count = 0
  index_object$break_flag = FALSE
  return(index_object)
}


initialise_ecology_slice <- function(global_params, land_parcels){
  
  land_parcel_num = length(land_parcels)
  initial_ecology = matrix(1,global_params$ecology_size,global_params$ecology_size)
  
  for (parcel_ind in seq_len(land_parcel_num)){
    initial_parcel_value = global_params$min_initial_eco_val + (global_params$max_initial_eco_val - global_params$min_initial_eco_val - global_params$initial_eco_noise)*runif(1) 
    current_parcel = select_land_parcel(land_parcels, parcel_ind)
    initial_ecology[current_parcel] = initial_ecology[current_parcel]*initial_parcel_value
  }
  initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size),global_params$ecology_size,global_params$ecology_size)
  return(initial_ecology)
}




initialise_ecology <- function(global_params, land_parcels){
  initial_ecology = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$eco_dims))
  for (eco_ind in seq_len(global_params$eco_dims)){
    initial_ecology[, , eco_ind]  = initialise_ecology_slice(global_params, land_parcels)
  }
  return(initial_ecology)
}
  

initialise_offset_object <- function(){
  object = list()
  object$current_parcels = list()
  object$current_parcel_sums = list()
  return(object)
}


mcell <- function(x, vx, vy){
  
  rowsizes = vy;
  colsizes = vx;
  rows = length(rowsizes);
  cols = length(colsizes);
  
  a = 1
  B = vector('list', rows*cols)
  colStart = 0
  for (i in seq_len(cols)){
    rowStart = 0
    for (j in seq_len(rows)){
      B[[a]] = x[rowStart+(1:rowsizes[j]), colStart+(1:colsizes[i])]
      rowStart = rowStart + rowsizes[j]
      a = a + 1
    }
    colStart = colStart + colsizes[i]
  }
  
  parcel = list()
  parcel$dims = c(length(vy), length(vx))
  parcel$elements = B
  return(parcel)
  
}  

ind2sub <- function(rows, ind){
  rw = ((ind-1) %% rows) + 1 
  cl = floor((ind-1) / rows) + 1
  loc = c(rw, cl)
  return(loc)
}


project_ecology <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_horizon, time_fill){
  if (time_fill == 'all'){
    time_vec = seq_len(time_horizon)
  } else {time_vec = time_horizon}
  t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
  eco_projected = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_vec - t_sh)))
  return(eco_projected)
}





build_cfacs_by_year <- function(global_params, decline_rates, land_parcels, initial_ecology){
  current_ecology = initial_ecology
  cfacs = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
  cfacs[, , 1] = initial_ecology
  parcel_num = length(land_parcels)
  
  for (yr in seq(global_params$time_steps)){
    for (parcel_ind in seq_len(parcel_num)){
      current_parcel = select_land_parcel(land_parcels, parcel_ind)
      current_dec_rate = decline_rates[parcel_ind]      
      updated_parcel = sapply(current_ecology[current_parcel], project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, decline_rate = current_dec_rate, time_horizon = 1, time_fill = 'none')
      current_ecology[current_parcel] = updated_parcel 
    }
    if (global_params$blur == TRUE){
      current_ecology = Blur_2D(current_ecology, 0.5, 0.5)
    } 
    cfacs[, , yr] = current_ecology
  }
  
  return(cfacs)
}




predict_parcel_traj <- function(current_ecology, parcel_traj_type, global_params, current_dec_rate, time_horizon){
  
  if (time_horizon == 0){
    ecology_predicted = current_ecology
  } else if (time_horizon > 0){
    ecology_predicted = array(0, c(dim(current_ecology), (time_horizon + 1)))
    ecology_predicted[, , 1] = current_ecology
    
    if (parcel_traj_type == 'protect'){
      decline_rate = current_dec_rate
    } else if (parcel_traj_type == 'maintain'){
      decline_rate = 1e-10
    } else if (parcel_traj_type == 'restore'){
      decline_rate = global_params$restoration_rate
    }
      
    ecology_projected = apply(current_ecology, MARGIN = c(1, 2), FUN = project_ecology, min_eco_val = global_params$min_eco_val, max_eco_val = global_params$max_eco_val, decline_rate, time_horizon, time_fill = 'all')
    
    
    if (length(dim(ecology_projected)) > 2){
      ecology_projected = aperm(ecology_projected, c(2, 3, 1))
    }
    ecology_predicted[, , 2:(time_horizon + 1)] = ecology_projected
  }
  
  return(ecology_predicted)
  
}



# current_ecology = initial_ecology
# decline_rates = decline_rates_initial
# land_parcels = parcels$land_parcels
# parcel_indexes = 1:(parcels$land_parcel_num)
# time_horizons = global_params$time_steps

build_cfacs_list <- function(global_params, decline_rates, parcel_ecologies, parcel_indexes, time_horizon, eco_dims){
  
  parcel_num = length(parcel_indexes)
  cfacs = vector('list', parcel_num)

  for (parcel_count_ind in seq_len(parcel_num)){
    current_cfac = vector('list', eco_dims)
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    for (eco_ind in seq_len(eco_dims)){
      current_dec_rate = find_decline_rate(decline_rates, current_parcel_ind, eco_ind)
      current_parcel_ecology = parcel_ecologies[[parcel_count_ind]][, , eco_ind]
      current_cfac[[eco_ind]] = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = 'protect', global_params, current_dec_rate, time_horizon)
    }
    cfacs[[parcel_count_ind]] = current_cfac
  }
  
  return(cfacs)
  
}


build_cfacs_by_parcel_multi <- function(global_params, decline_rates, parcel_indexes, land_parcels, current_ecology, time_horizon){
  
  eco_dims = dim(current_ecology)[3]
  parcel_num = length(parcel_indexes)
  cfacs = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    current_cfac = vector('list', eco_dims)
    
    for (eco_ind in seq_len(eco_dims)){
      current_ecology_slice = current_ecology[, , eco_ind]
      current_dec_rate = find_decline_rate(decline_rates, current_parcel_ind, eco_ind)
      current_parcel_ecology = current_ecology_slice[current_parcel]
      dim(current_parcel_ecology) = dim(current_parcel)
      current_cfac[[eco_ind]] = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = 'protect', global_params, current_dec_rate, time_horizon)
    }
    
    cfacs[[parcel_count_ind]] = current_cfac
  }
  
  return(cfacs)
  
}





initialise_cfacs_multi <- function(global_params, region_params, land_parcels, initial_ecology, decline_rates){
  cfacs = vector('list', global_params$eco_dims)
  ecology_size = global_params$ecology_size
  for (eco_ind in seq_len(global_params$eco_dims)){
    cfacs[[eco_ind]] = build_cfacs_by_year(global_params, decline_rates[, , eco_ind] , land_parcels, initial_ecology[, , eco_ind] )
  }
  return(cfacs)
}



# initialise trajectories as a list containing N 3 dimensional arrays where N is the number of ecologiecal dimensions
initialise_trajectories <- function(eco_dims, ecology_size, time_steps, initial_ecology){
  trajectories = vector('list', eco_dims)
  for (eco_ind in seq_len(eco_dims)){
    trajectories[[eco_ind]] = array(0, c(ecology_size, ecology_size, time_steps))
  }
  
  return(trajectories)
}







build_decline_rates_multi <- function(parcels, condition_change, mean_decline_rate, decline_rate_std, eco_dims){
  regions = parcels$regions
  region_num = length(regions)
  if (condition_change == 'decline'){
    mean_decline_rate = -mean_decline_rate
  }
#  region_dec_rates = vector('list', region_num)
  decline_rates = array(0, c(dim(parcels$parcel_indexes), eco_dims))
  for (eco_ind in seq_len(eco_dims)){
    current_decline_rates = array(0, dim(parcels$parcel_indexes))
    for (region_ind in seq_len(region_num)){ 
      current_region = regions[[region_ind]]
      current_parcel_num = length(current_region)    
      decline_params = c(length(current_region), mean_decline_rate, decline_rate_std) #params$decline_rate_std[region_ind])
      region_decline_rates = matrix(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), ncol = ncol(current_region))
      #    region_dec_rates[[region_ind]] = current_decline_rates
      current_decline_rates[current_region] = region_decline_rates
    }
    decline_rates[, , eco_ind]  = current_decline_rates
  }
  
  return(decline_rates)
}



select_development_index <- function(ind_available, parcel_num){
  parcel_indexes = ind_available[sample(1:length(ind_available), parcel_num)]
  return(parcel_indexes)
}





update_development_object <- function(global_params, ind_available, current_ecology, decline_rates, land_parcels, offset_dims, yr, time_horizon){
  
  parcel_indexes = select_development_index(ind_available, 1)
  
  dev_calc_type = global_params$dev_calc_type
  restoration_rate = global_params$restoration_rate
  
  parcel_sums_object = find_current_parcel_sums(land_parcels, current_ecology, parcel_indexes)
  
  if (offset_dims == 'singular'){
    eco_dims = 1
  } else if (offset_dims == 'all'){
    eco_dims = global_params$eco_dims
  }
  current_parcel_val = parcel_sums_object$parcel_sums
  
  if (dev_calc_type == 'current_condition'){
    parcel_vals_used = current_parcel_val[1:eco_dims]
  } else if (dev_calc_type == 'future_condition'){
    parcel_vals_used = predict_parcel_vals_multi(predict_type = 'protect', parcel_sums_object$parcel_eco_vals, parcel_indexes, decline_rates, restoration_rate, 
                                                 global_params$min_eco_val, global_params$max_eco_val, eco_dims, time_horizon = time_horizon)
  }
  
  
  development_object = list()
  development_object$yr = yr
  development_object$parcel_ecologies = parcel_sums_object$parcel_eco_vals
  development_object$parcel_sums = parcel_sums_object$parcel_sums
  development_object$parcel_indexes = parcel_indexes
  development_object$parcel_vals_used = parcel_vals_used
  return(development_object)
  
}


# predict_type = 'restore' 
# parcel_eco_vals = parcel_sums_object$parcel_eco_vals 
# parcel_indexes = current_offset_pool 
# decline_rates = decline_rates_initial 
# restoration_rate = global_params$restoration_rate
# min_eco_val = global_params$min_eco_val
# max_eco_val = global_params$max_eco_val

predict_parcel_vals_multi <- function(predict_type, parcel_eco_vals, parcel_indexes, decline_rates, restoration_rate, min_eco_val, max_eco_val, eco_dims, time_horizon){
  
  parcel_num = length(parcel_indexes)
  predicted_parcel_vals = array(0, c(parcel_num, eco_dims))
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_indexes[parcel_count_ind]
    
    for (eco_ind in seq_len(eco_dims)){
      current_decline_rates = decline_rates[, , eco_ind] 
      
      if (predict_type == 'protect'){
        decline_rate = current_decline_rates[current_parcel_ind]      
      } else if (predict_type == 'restore'){
        decline_rate = restoration_rate
      }
      predicted_parcel = sapply(parcel_eco_vals[[parcel_count_ind]][, , eco_ind], project_ecology, min_eco_val, max_eco_val, decline_rate, time_horizon, time_fill = 'none')
      predicted_parcel_vals[parcel_count_ind, eco_ind]  = sum(predicted_parcel)
    }
    
  }
  return(predicted_parcel_vals)
  
}


record_parcel_info_multi <- function(record_type, parcel_sums, parcel_indexes, parcel_eco_vals, parcel_vals_used, yr){
  out_object = list()
  out_object$yr = yr
  out_object$parcel_ecologies = parcel_eco_vals
  out_object$parcel_sums = parcel_sums
  out_object$parcel_indexes = parcel_indexes
  out_object$parcel_vals_used = parcel_vals_used
  if (record_type == 'offset'){
    out_object$adjusted_cfac_trajs = adjusted_counter_trajs
  }
  return(out_object)
  
}



  


find_offset_pool <- function(index_object, region_ind, decline_rates, global_params, region_params, land_parcels, current_ecology, time_horizon, yr){
  
  offset_pool_object = list()
  offset_calc_type = global_params$offset_calc_type
  cfac_type = global_params$cfac_type_in_offset_calc
  adjust_cfacs_flag = global_params$adjust_cfacs_flag
  
  current_offset_pool = index_object$ind_available[[region_ind]]
  parcel_sums_object = find_current_parcel_sums(land_parcels, current_ecology, current_offset_pool)
  
  if (global_params$offset_dims == 'singular'){
    eco_dims = 1
  } else {
    eco_dims = global_params$eco_dims
  }
  
  current_parcel_sums = parcel_sums_object$parcel_sums[, 1:eco_dims]
 
  if ( adjust_cfacs_flag == TRUE){
    
    parcel_indexes = current_offset_pool
    current_cfacs = build_cfacs_by_parcel_multi(global_params, decline_rates_initial, parcel_indexes, land_parcels, current_ecology, time_horizon)
    #adjusted_counters = adjust_counters(cfacs, region_params, global_params, length(parcel_indexes), time_horizon, yr)
    
    adjusted_counters = adjust_cfacs(current_cfacs, (adjusted_cfac_type = cfac_type), region_params, global_params, 
                                                            (current_parcel_num_remaining = length(parcel_indexes)), decline_rates, parcel_indexes, time_horizon, yr)
    #adjusted_counter_trajs = find_parcel_traj_by_list(adjusted_counters, (time_horizon + 1))
    adjusted_counter_trajs = sum_parcel_trajectories(adjusted_counters, eco_dims, parcel_indexes = 1:length(parcel_indexes), time_horizon = (time_horizon + 1))
  }
  
  if ( (offset_calc_type == 'restoration_gains') || (offset_calc_type == 'restoration_from_cfac') || (offset_calc_type == 'restoration_condition_value')){
      restoration_vals = predict_parcel_vals_multi(predict_type = 'restore', parcel_sums_object$parcel_eco_vals, parcel_indexes = current_offset_pool, decline_rates, global_params$restoration_rate, 
                                                          global_params$min_eco_val, global_params$max_eco_val, eco_dims, time_horizon)
  }
  
  if ((offset_calc_type == 'avoided_degredation') || (offset_calc_type == 'restoration_from_cfac') || (offset_calc_type == 'parcel_condition_value') ){

    if ( adjust_cfacs_flag == TRUE){
      cfac_vals = adjusted_counter_trajs[(time_horizon + 1), ]
      cfac_vals = t(t(cfac_vals))
    } else {
      cfac_vals = predict_parcel_vals_multi(predict_type = 'protect', parcel_sums_object$parcel_eco_vals, parcel_indexes = current_offset_pool, decline_rates, global_params$restoration_rate, 
                                                         global_params$min_eco_val, global_params$max_eco_val, eco_dims, time_horizon)
    }
  }
  
  
  
  if (offset_calc_type == 'current_condition'){
    parcel_vals_pool = current_parcel_sums
  } else if (offset_calc_type == 'restoration_gains'){
    parcel_vals_pool = restoration_vals - current_parcel_sums
  } else if (offset_calc_type == 'restoration_from_cfac'){
    parcel_vals_pool = restoration_vals - cfac_vals
  } else if (offset_calc_type == 'restoration_condition_value'){
    parcel_vals_pool = restoration_vals
  } else if (offset_calc_type == 'avoided_degredation'){
    parcel_vals_pool = current_parcel_sums - cfac_vals
  } else if (offset_calc_type == 'parcel_condition_value'){
    parcel_vals_pool = cfac_vals 
  } 
  
  offset_pool_object$current_parcel_pool = current_offset_pool
  offset_pool_object$current_parcel_sums = parcel_sums_object$parcel_sums
  offset_pool_object$parcel_vals_pool = parcel_vals_pool
  offset_pool_object$parcel_eco_vals = parcel_sums_object$parcel_eco_vals
  return(offset_pool_object)
  
}                 



select_offset_index <- function(offset_pool_object, offset_multiplier, development_vals_used){
  outs = list()
  parcel_vals_pool = offset_pool_object$parcel_vals_pool
  current_offset_pool = offset_pool_object$current_parcel_pool
  dev_vals = offset_multiplier*matrix(rep(development_vals_used, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
  err = sqrt(rowSums( (parcel_vals_pool - dev_vals)^2 ) )
  best_ind = which(err == min(err))
  parcel_indexes = current_offset_pool[best_ind]
  parcel_vals_used = parcel_vals_pool[best_ind, ]
  
  
  parcel_num = length(parcel_indexes)
  outs$parcel_eco_vals = vector('list', parcel_num)
  outs$parcel_sums = vector('list', parcel_num)
  outs$parcel_indexes = array(0, parcel_num)
  outs$parcel_vals_used = vector('list', parcel_num)
  
  for (parcel_ind in seq_len(parcel_num)){
    outs$parcel_eco_vals[[parcel_ind]] = offset_pool_object$parcel_eco_vals[[best_ind]]
    outs$parcel_sums[[parcel_ind]] = offset_pool_object$current_parcel_sums[best_ind, ]
    outs$parcel_vals_used[parcel_ind, ] = parcel_vals_used
    outs$parcel_indexes[parcel_ind] = parcel_indexes
  }
 
  return(outs)
}


rowProds <- function(X){ t(t(apply(X,1,FUN="prod"))) }

test_cond <- function(vals_to_match, parcel_vals_pool, development_vals_used, match_array){
  thresh_array = matrix(rep(0.10*vals_to_match, dim(parcel_vals_pool)[1]), ncol = length(development_vals_used), byrow = TRUE)
  cond = (parcel_vals_pool - match_array) < thresh_array
  cond = rowProds(cond)
  return(cond)
}












adjust_cfac <- function(cfacs, region_params, global_params, parcel_indexes, parcel_num_remaining, time_horizon, yr){
  
  parcel_num = length(parcel_indexes)
  eco_dims = global_params$eco_dims
  rec_list = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    rec_list[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  weighted_offset_projections = rec_list
  include_clearing = rec_list
  include_clearing_offsets = rec_list
  
  rm(rec_list)
  
  dev_prob <- find_dev_probability(global_params$dev_vec, yr, time_horizon, parcel_num_remaining)
  
  if (global_params$cfac_type_in_offset_calc == 'include_clearing_offsets'){
    offset_prob = dev_prob
  } else {offset_prob = 0}
  
  counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      current_cfac = cfacs[[parcel_count_ind]][[eco_ind]]
      projected_dims = dim(current_cfac)

      counter_prob_array = rep(counter_probs, projected_dims[1]*projected_dims[2])
      dim(counter_prob_array) = c(length(counter_probs), c(projected_dims[2], projected_dims[1]))
      counter_prob_array = aperm(counter_prob_array, c(3, 2, 1))
      weighted_counters = counter_prob_array*current_cfac
      include_clearing[[parcel_count_ind]][[eco_ind]] = weighted_counters
      
      if (global_params$cfac_type_in_offset_calc == 'include_clearing_offsets'){
        current_weighted_offset_projections = find_summed_offset_projections(projected_dims, current_cfac, offset_prob, global_params, time_horizon)
        weighted_offset_projections[[parcel_count_ind]][[eco_ind]] = current_weighted_offset_projections
        include_clearing_offsets[[parcel_count_ind]][[eco_ind]] = weighted_counters + current_weighted_offset_projections
      } 
    } 
  }
  
  adjusted_counters_object = list()
  adjusted_counters_object$include_clearing = include_clearing
#  adjusted_counters_object$weighted_offset_projections = weighted_offset_projections
  adjusted_counters_object$include_clearing_offsets = include_clearing_offsets
  
  return(adjusted_counters_object)
  
}



find_dev_probability <- function(dev_vec, yr, time_horizon, parcel_num_remaining){
  
  dev_vec = dev_vec[yr:length(dev_vec)]

  if (length(dev_vec) < (time_horizon + 1)){
    dev_vec = c(dev_vec, array(0, ((time_horizon + 1) - length(dev_vec))))
  }

  dev_vec = dev_vec[1:(time_horizon + 1)]
  dev_prob = dev_vec/parcel_num_remaining
}





# development_vals_used = development_object$parcel_vals_used
# offset_multiplier = region_params[[region_ind]]$offset_multiplier
# offset_parcel_for_parcel = region_params[[region_ind]]$offset_parcel_for_parcel
# eco_dims = global_params$eco_dims








update_offset_object <- function(offset_pool_object, offset_multiplier, development_vals_used,  offset_parcel_for_parcel, eco_dims, yr){
  
  offset_object = list()
  match_indexes = vector()
  parcel_vals_pool = offset_pool_object$parcel_vals_pool
  current_offset_pool = offset_pool_object$current_parcel_pool
  vals_to_match = offset_multiplier*development_vals_used
  
  match_ind_available = 1:length(current_offset_pool)
  match_array = matrix(rep(vals_to_match, length(match_ind_available)), ncol = length(development_vals_used), byrow = TRUE)
  #cond = test_cond(vals_to_match, t(t(parcel_vals_pool[match_ind_available, ])), development_vals_used, match_array)
  
  cond = all(vals_to_match > 0)
    
  parcel_vals_pool = t(t(parcel_vals_pool))
  
  while(any(cond > 0)){
    err = sqrt(rowSums( (parcel_vals_pool[match_ind_available, ] - match_array)^2 ) )
    match_ind = which(err == min(err))
    index_used = match_ind_available[match_ind]
    match_indexes = c(match_indexes, index_used)
    parcel_vals_used = parcel_vals_pool[index_used, ]
    
    if ( offset_parcel_for_parcel == TRUE)
      {
      break
    }
    match_ind_available = match_ind_available[-match_ind]
    vals_to_match = vals_to_match - parcel_vals_used
    match_array = matrix(rep(vals_to_match, length(match_ind_available)), ncol = length(development_vals_used), byrow = TRUE)
    
    cond = all(vals_to_match > 0)
    
  }
  
  parcel_num = length(match_indexes)
  parcel_indexes = current_offset_pool[match_indexes]
  parcel_eco_vals = vector('list', parcel_num)
  
  parcel_sums = array(0, c(parcel_num, eco_dims))
  parcel_vals_used = array(0, c(parcel_num, length(vals_to_match)))
  
  for (parcel_count in seq_len(parcel_num)){
    match_index = match_indexes[parcel_count]
    parcel_eco_vals[[parcel_count]] = offset_pool_object$parcel_eco_vals[[match_index]]
    parcel_sums[parcel_count, ] = offset_pool_object$current_parcel_sums[match_index, ]
    parcel_vals_used[parcel_count, ] = parcel_vals_pool[match_index, ]
  }
  
  offset_object$yr = yr
  offset_object$parcel_ecologies = parcel_eco_vals
  offset_object$parcel_sums = parcel_sums
  offset_object$parcel_vals_used = parcel_vals_used
  offset_object$parcel_indexes = parcel_indexes
  
  return(offset_object)
  
}



write_null_offset_object <- function(){
  offset_object = list()
  offset_object$parcel_indexes = list()
  offset_object$current_parcel_sums = list()
  offset_object$parcel_vals_used = list()
  return(offset_object)
}


write_development <- function(development_object, current_ecology){
  
  current_parcel = development_object$current_parcels
  current_ecology[current_parcel] = 0
  
  return(current_ecology)
  
}




find_current_parcel_sums <- function(land_parcels, current_ecology, parcel_indexes){
  
  out_object = list()
  eco_dims = dim(current_ecology)[3]
  parcel_eco_vals = vector('list', length(parcel_indexes))
  parcel_sums = array(0, c(length(parcel_indexes), eco_dims))
  
  for (parcel_count_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_count_ind] 
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    current_parcel_eco_vals = extract_3D_parcel(current_parcel, current_ecology)
    
    if (eco_dims == 1){
      parcel_sums[parcel_count_ind] = sum(current_parcel_eco_vals)
    } else if (eco_dims > 1){
      parcel_sums[parcel_count_ind, ] = apply(current_parcel_eco_vals, 3, sum)
    }
    parcel_eco_vals[[parcel_count_ind]] = current_parcel_eco_vals
  }
  
  out_object$parcel_eco_vals = parcel_eco_vals
  out_object$parcel_sums = parcel_sums
  return(out_object)
  
}





write_parcel_sets <- function(parcel_set_object, yr){
  parcel_set = list()
  parcel_set$yr = yr
  parcel_set$parcel_set_object = parcel_set_object
  return(parcel_set)
}


update_ind_available <- function(update_type, index_object, parcel_indexes, region_ind){
  
  ind_available = index_object$ind_available[[region_ind]]
  ind_available = setdiff(ind_available, parcel_indexes) #remove development parcel from available list   
  
  if ( length(ind_available) < 0 ){   
    break_flag = TRUE
  } else {break_flag = FALSE}
  
  index_object$ind_available[[region_ind]] = ind_available
  
  index_object$break_flag = break_flag
  
  if (update_type == 'development'){
    index_object$developments = c(index_object$developments, parcel_indexes)
  } else if (update_type == 'offset'){
    index_object$offsets = c(index_object$offsets, parcel_indexes)
    
  }
  return(index_object)
  
}

update_parcel_num_remaining <- function(index_object, region_ind){
  
  dev_count = index_object$dev_count
  index_object$parcel_num_remaining[dev_count, region_ind] = length(index_object$ind_available[[region_ind]])
  
  return(index_object)
}


update_decline_rates <- function(decline_rates, restoration_rate, decline_rate_type, offset_action_type, parcel_indexes, offset_dims){

  for (parcel_ind in seq_len(length(parcel_indexes))){
    current_parcel_ind = parcel_indexes[parcel_ind]
    loc = ind2sub(dim(decline_rates)[1], current_parcel_ind)
    if (decline_rate_type == 'development'){
      decline_rates[loc[1], loc[2], ] = 0
    } else if (decline_rate_type == 'offset'){
      
      if (offset_action_type == 'protect'){
        offset_rate = decline_rates[loc[1], loc[2], ]  
      } else if (offset_action_type == 'maintain'){
        offset_rate = 1  
      } else if (offset_action_type == 'restore'){
        offset_rate = restoration_rate
      }
      
      if (offset_dims == 'all'){
        decline_rates[loc[1], loc[2], ] = offset_rate
      } else if (offset_dims == 'singular'){
        decline_rates[loc[1], loc[2], 1] = offset_rate[1]
      }
    }
  }
  
  return(decline_rates)
  
}


find_current_dev_nums <- function(dev_vec, region_num, yr){

  dev_nums = array(0, region_num)
  for (region_ind in seq_len(region_num)){
    current_dev_num = dev_vec[yr]
    dev_nums[region_ind] = current_dev_num
  }
  return(dev_nums)
  
}

                                     
find_parcel_set <- function(region_ind, global_params, region_params, current_ecology, decline_rates, parcels, index_object, yr){
  index_object$dev_count = index_object$dev_count + 1
  index_object <- update_parcel_num_remaining(index_object, region_ind)
  
  if (global_params$use_offset_time_horizon == TRUE){
    time_horizon = global_params$offset_time_horizon
  } else {
    time_horizon = global_params$time_steps - yr
  }
  
  offset_devs_object = list()
  time_remaining = 0:(global_params$time_steps - yr)
  
  development_object <- update_development_object(global_params, index_object$ind_available[[region_ind]], current_ecology, decline_rates, parcels$land_parcels, global_params$offset_dims, yr, time_horizon) 
  index_object <- update_ind_available(update_type = 'development', index_object, development_object$parcel_indexes, region_ind)
  decline_rates <- update_decline_rates(decline_rates, global_params$restoration_rate, decline_rate_type = 'development', offset_action_type = global_params$offset_action_type, development_object$parcel_indexes, global_params$offset_dims)
  
  if (global_params$perform_offsets == TRUE){
                                          
    offset_pool_object <- find_offset_pool(index_object, region_ind, decline_rates, global_params, region_params, parcels$land_parcels, current_ecology, time_horizon, yr)
    offset_object <- update_offset_object(offset_pool_object, region_params[[region_ind]]$offset_multiplier, development_object$parcel_vals_used, region_params[[region_ind]]$offset_parcel_for_parcel, global_params$eco_dims, yr)                  
    index_object <- update_ind_available(update_type = 'offset', index_object, offset_object$parcel_indexes, region_ind)              
    decline_rates <- update_decline_rates(decline_rates, global_params$restoration_rate, decline_rate_type = 'offset', offset_action_type = global_params$offset_action_type, offset_object$parcel_indexes, global_params$offset_dims)
    
  } else {offset_object <- write_null_offset_object() }
  
  
  offset_devs_object$index_object = index_object
  offset_devs_object$decline_rates = decline_rates
  offset_devs_object$offset_object = offset_object
  offset_devs_object$development_object = development_object

  return(offset_devs_object)
} 


# yr = 1
# current_ecology = initial_ecology
# decline_rates = decline_rates_initial
# perform_offsets = TRUE
# record_parcel_sets = TRUE
# region_ind = 1
# current_dev_nums <- find_current_dev_nums(region_params, global_params$region_num, yr)
# current_develop_num = current_dev_nums[region_ind]
# 
# land_parcels = parcels$land_parcels
# time_horizon = 20
# 
# trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)
# offsets_object = list()
# developments_object = list()


write_current_parcel_set <- function(net_parcel_set_object, current_parcel_set_object, dev_count){
  net_parcel_set_object$yr[[dev_count]] = current_parcel_set_object$yr
  net_parcel_set_object$parcel_ecologies[[dev_count]] = current_parcel_set_object$parcel_ecologies
  net_parcel_set_object$parcel_sums[[dev_count]] = current_parcel_set_object$parcel_sums
  net_parcel_set_object$parcel_indexes[[dev_count]] = current_parcel_set_object$parcel_indexes
  net_parcel_set_object$parcel_vals_used[[dev_count]] = current_parcel_set_object$parcel_vals_used
  return(net_parcel_set_object)
}

initialise_parcel_set_object <- function(dev_num){
  
  parcel_set_object = list()
  parcel_set_object$yr = vector('list', dev_num)
  parcel_set_object$parcel_ecologies = vector('list', dev_num)
  parcel_set_object$parcel_sums = vector('list', dev_num)
  parcel_set_object$parcel_indexes = vector('list', dev_num)
  parcel_set_object$parcel_vals_used = vector('list', dev_num)
  return(parcel_set_object)
  
}


# main engine for code - returns all development/offset parcel sets, land parcel trajectories etc.

run_system <- function(global_params, region_params, current_ecology, decline_rates, parcels, index_object){
  
  trajectories <- initialise_trajectories(global_params$eco_dims, global_params$ecology_size, global_params$time_steps, initial_ecologies)    # initialise trajectories as a list of N 3D arrays to fill for each eco dimension
  net_offsets_object <- initialise_parcel_set_object(global_params$total_dev_num)   #initialise offsets object to store all offsets
  net_developments_object <- initialise_parcel_set_object(global_params$total_dev_num) #initialise developments object to store all offsets
  
  #main time loop   
  for (yr in seq_len(global_params$time_steps)){   
   
    current_dev_nums <- find_current_dev_nums(global_params$dev_vec, global_params$region_num, yr) #developments per year
    
    #cycle through regions
    
    for (region_ind in seq_len(parcels$region_num)){ 
      current_develop_num = current_dev_nums[region_ind]

      if (current_develop_num > 0){
        
        
        for (dev_index in seq_len(current_develop_num)){
          
          offset_devs_object <- find_parcel_set(region_ind, global_params, region_params, current_ecology, decline_rates, parcels, index_object, yr) # find the development/offset parcel sets for each proposed development
          decline_rates <- offset_devs_object$decline_rates   # update decline rates
          index_object <- offset_devs_object$index_object     # update index obeject containing all available parcels
          net_offsets_object <- write_current_parcel_set(net_offsets_object, offset_devs_object$offset_object, index_object$dev_count)  #record offset info for current parcel set
          net_developments_object <- write_current_parcel_set(net_developments_object, offset_devs_object$development_object, index_object$dev_count)  #record development info for current parcel set
          
        }
      }
      
    }
    
    for (eco_ind in seq_len(global_params$eco_dims)){
      trajectories[[eco_ind]][, , yr] = current_ecology[, , eco_ind] # record current ecology in trajectories list for each eco dimension
    }
    current_ecology <- project_current_system(current_ecology, parcels$land_parcels, decline_rates, global_params$min_eco_val, 
                                              global_params$max_eco_val, time_horizon = 1, global_params$eco_dims)     # update ecology for subsequent time step using current decline rates
    
    print(yr)
  }

  
  if (global_params$record_parcel_sets == TRUE){
    outputs = list()
    outputs$offset_list = index_object$offsets
    outputs$development_list = index_object$developments
    outputs$offsets = net_offsets_object
    outputs$developments = net_developments_object
    outputs$trajectories = trajectories
    outputs$decline_rates = decline_rates
    outputs$parcel_num_remaining = index_object$parcel_num_remaining
    return(outputs)
  } else{
    return(trajectories)
  }
  
}


select_land_parcel <- function(land_parcels, current_parcel_ind){
  selected_land_parcel = land_parcels[[current_parcel_ind]]
  return(selected_land_parcel)
}


# update land parcels according to development/offset action (maintain/restore) 

project_current_system <- function(current_ecology, land_parcels, decline_rates, min_eco_val, max_eco_val, time_horizon, eco_dims){
  
  parcel_num = length(land_parcels)
  
  for (eco_ind in seq_len(eco_dims)){
    current_ecology_slice = current_ecology[, , eco_ind] #select current ecological dimension to work on
    current_decline_rates = decline_rates[,  , eco_ind] #select current decline rates to work on
    for (parcel_ind in seq_len(parcel_num)){  
      current_parcel = select_land_parcel(land_parcels, parcel_ind)   #select array indexes that correspond to current land parcel
      current_parcel_ecology = current_ecology[current_parcel]
      decline_rate = current_decline_rates[parcel_ind] #select corresponding decline rate
      if (decline_rate == 0){
        updated_parcel_ecology = array(0, length(current_parcel_ecology))   # if the parcel is to be developed (i.e. decline rate = 0), set parcel value to zeros
      } else if (decline_rate == 1){
        updated_parcel_ecology = current_parcel_ecology      # if the parcel is to be maintained (i.e. decline rate = 1), set parcel values to current values
      } else {updated_parcel_ecology = sapply(current_parcel_ecology, project_ecology, min_eco_val = min_eco_val, 
                                              max_eco_val = max_eco_val, decline_rate = decline_rate, time_horizon = time_horizon, time_fill = 'none')}  # update ecology according to ecological curve in project_ecology function (currently logistic) - curve parameters are contained in decline_rates array

      current_ecology_slice[current_parcel] = updated_parcel_ecology 
    }
    current_ecology[, , eco_ind]  = current_ecology_slice
  }
  return(current_ecology) 
}
  

plot_outs <- function(...){
  
  dots = list(...)
  plot_params = dots[[length(dots)]]
  
  plot_num = length(dots) - 1
  sub_plot_num = plot_params[1]*plot_params[2]
  A = 1:sub_plot_num
  dim(A) = c(plot_params[1], plot_params[2])
  layout(A)
  ymaxs = array(0, plot_num)
  ymins = array(0, plot_num)
  for (s in seq_len((length(dots) - 1))){
    ymaxs[s] = max(dots[[s]])
    ymins[s] = min(dots[[s]])
  }
  ymax = max(ymaxs)
  ymin = min(ymins)
  for (s in seq_len(sub_plot_num)){
    plot(dots[[1]][, s], type = 'l', col = 'red', ylim = c(ymin, ymax))
    if (plot_num > 1){
      for (t in 2:plot_num){
        lines(dots[[t]][, s],  ylim = c(ymin, ymax))
      }
    } 
  }
}




extract_parcel_set_trajs <- function(traj_list, parcel_set_indexes){
  
  parcel_set_num = length(parcel_set_indexes)
  
  parcel_set_traj_list = vector('list', parcel_set_num)
  for (parcel_set_ind in seq_len(parcel_set_num)){
    current_parcel_indexes = parcel_set_indexes[[parcel_set_ind]]
    parcel_num = length(current_parcel_indexes)
    current_parcel_set_traj = vector('list', parcel_num)
    for (parcel_count_ind in seq_len(parcel_num)){
      parcel_ind = current_parcel_indexes[parcel_count_ind]
      current_parcel_set_traj[[parcel_count_ind]] = traj_list[[parcel_ind]]
    }
    parcel_set_traj_list[[parcel_set_ind]] = current_parcel_set_traj
  }
  return(parcel_set_traj_list)
  
}



build_traj_list <- function(trajectories, land_parcels, parcel_indexes, eco_dims){
  
  parcel_num = length(parcel_indexes)
  traj_list = vector('list', parcel_num)
  
  for (parcel_count_ind in seq_len(parcel_num)){
    current_parcel_traj = vector('list', eco_dims)
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind = parcel_indexes[parcel_count_ind])
    
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_traj[[eco_ind]] = extract_3D_parcel(current_parcel, trajectories[[eco_ind]][, , ])
    }
    
    traj_list[[parcel_count_ind]] = current_parcel_traj
  }
  
  return(traj_list)
}
  


# outputs = outs$model_outputs
# land_parcels = parcels$land_parcels
# parcel_sets_cfacs = outs$parcel_sets_cfacs
# decline_rates = decline_rates_initial
# trajectories = outputs$trajectories
# time_steps = global_params$time_steps
# parcel_set_ind = 1
# current_sets_object = outputs$offsets
# current_parcel_set = current_sets_object[[parcel_set_ind]]

# parcel_count_ind = 1

# current_cfac_sets_object = parcel_sets_cfacs$offsets


collate_parcel_sets_object <- function(outputs, parcel_sets_cfacs, land_parcels, global_params, decline_rates){
  parcel_set_num = global_params$total_dev_num
  time_horizon = global_params$time_steps
  eco_dims = global_params$eco_dims
  
  traj_list = build_traj_list(outputs$trajectories, land_parcels, parcel_indexes = 1:length(land_parcels), eco_dims)
  offset_traj_list = extract_parcel_set_trajs(traj_list, parcel_set_indexes = outputs$offsets$parcel_indexes)
  dev_traj_list = extract_parcel_set_trajs(traj_list, parcel_set_indexes = outputs$developments$parcel_indexes)
  
  offset_yrs = outputs$offsets$yr
  
  collated_parcel_sets_object = list()
  collated_parcel_sets_object$traj_list = traj_list
  collated_parcel_sets_object$offsets = collate_parcel_sets(outputs$offsets, offset_traj_list, parcel_sets_cfacs$offsets, land_parcels, parcel_set_num, time_horizon, eco_dims, decline_rates)
  collated_parcel_sets_object$devs = collate_parcel_sets(outputs$developments, dev_traj_list, parcel_sets_cfacs$developments, land_parcels, parcel_set_num, time_horizon, eco_dims, decline_rates)
  
  collated_parcel_sets_object$NNL_object = assess_NNL(collated_parcel_sets_object$offsets$rest_gains, collated_parcel_sets_object$offsets$avoided_degs, 
                                                      collated_parcel_sets_object$devs$rest_gains, collated_parcel_sets_object$devs$avoided_degs, eco_dims, parcel_set_num, time_horizon, offset_yrs)
  collated_parcel_sets_object$summed_program_parcels = sum_program_parcels(traj_list, eco_dims, offset_parcel_indexes = (outputs$offset_list), 
                                                                          dev_parcel_indexes = (outputs$development_list), time_horizon)
  
  return(collated_parcel_sets_object)
}


#offset_parcel_indexes = (outs$model_outputs$offset_list)
#dev_parcel_indexes = (outs$model_outputs$development_list)

sum_program_parcels <- function(traj_list, eco_dims, offset_parcel_indexes, dev_parcel_indexes, time_horizon){
  summed_program_parcels = list()
  offset_trajs = sum_parcel_trajectories(traj_list, eco_dims, parcel_indexes = offset_parcel_indexes, time_horizon) 
  dev_trajs = sum_parcel_trajectories(traj_list, eco_dims, parcel_indexes = dev_parcel_indexes, time_horizon)
  net_trajs = offset_trajs + dev_trajs
  
  summed_program_parcels$offset_trajs = sum_cols_multi(offset_trajs)
  summed_program_parcels$dev_trajs = sum_cols_multi(dev_trajs)
  summed_program_parcels$net_trajs = sum_cols_multi(net_trajs)
  return(summed_program_parcels)
}

sum_program_cfacs <- function(cfac_parcel_trajs, parcel_indexes){
  program_cfacs = cfac_parcel_trajs[, parcel_indexes, ]
  program_cfac_sum = sum_cols_multi(program_cfacs)
  return(program_cfac_sum)
}

collate_parcel_sets <- function(current_sets_object, current_parcel_sets_traj_list, current_cfac_sets_object, land_parcels, parcel_set_num, time_horizon, eco_dims, decline_rates){


  rest_gains = find_gains_degs(current_sets_object, current_traj_list = current_parcel_sets_traj_list, trajectory_type = 'traj', parcel_set_num, time_horizon, eco_dims)
  avoided_degs = find_gains_degs(current_sets_object, current_traj_list = current_cfac_sets_object$cfacs, trajectory_type = 'cfac', parcel_set_num, time_horizon, eco_dims)
  avoided_degs_including_clearing = find_gains_degs(current_sets_object, current_traj_list = current_cfac_sets_object$cfacs_include_clearing, trajectory_type = 'cfac', parcel_set_num, time_horizon, eco_dims)
  avoided_degs_including_clearing_offsets = find_gains_degs(current_sets_object, current_traj_list = current_cfac_sets_object$cfacs_include_clearing_offsets, trajectory_type = 'cfac', parcel_set_num, time_horizon, eco_dims)
  initial_parcel_sums = collate_initial_sums(current_sets_object$parcel_sums, parcel_set_num)
  
  parcel_sets_object = list()
  parcel_sets_object$initial_parcel_sums = initial_parcel_sums
  parcel_sets_object$rest_gains = rest_gains
  parcel_sets_object$avoided_degs = avoided_degs
  parcel_sets_object$avoided_degs_including_clearing = avoided_degs_including_clearing
  parcel_sets_object$avoided_degs_including_clearing_offsets = avoided_degs_including_clearing_offsets
  
  return(parcel_sets_object)
    
}  


collate_initial_sums <- function(initial_sums_list, parcel_set_num){
  initial_sums_array = vector()
  
  for (parcel_set_ind in 1:parcel_set_num){
    initial_sums_array = c(initial_sums_array, initial_sums_list[[parcel_set_ind]])
  }
  
  return(initial_sums_array)
  
}

find_gains_degs <- function(current_sets_object, current_traj_list, trajectory_type, parcel_set_num, time_horizon, eco_dims){
  
  gains_degs = array(0, c(time_horizon, parcel_set_num, eco_dims))
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    
    parcel_ecologies = current_sets_object$parcel_ecologies[[parcel_set_ind]]
    parcel_indexes = current_sets_object$parcel_indexes[[parcel_set_ind]]
    parcel_num = length(parcel_indexes)
    yr = current_sets_object$yr[[parcel_set_ind]]
    current_set_element = array(0, c(time_horizon, eco_dims))
    
    for (parcel_count_ind in seq_len(parcel_num)){
      
      for (eco_ind in seq_len(global_params$eco_dims)){
        
        current_parcel_ecology = parcel_ecologies[[parcel_count_ind]][, , eco_ind]
        current_parcel_traj = current_traj_list[[parcel_set_ind]][[parcel_count_ind]][[eco_ind]]
        current_set_element[, eco_ind] = current_set_element[, eco_ind] + sum_rel_intial(current_parcel_traj, current_parcel_ecology, trajectory_type, time_horizon, yr)
        
      }
    }
    
    gains_degs[, parcel_set_ind, eco_ind] = current_set_element
  }  
  
  return(gains_degs)
}


sum_rel_intial <- function(current_parcel_traj, current_parcel_ecology, trajectory_type, time_horizon, yr){
  rel_arr = array(0, c(dim(current_parcel_ecology), time_horizon))
  
  if (trajectory_type == 'cfac'){
    rel_arr[, , yr:time_horizon] = current_parcel_traj - as.vector(current_parcel_ecology)
    rel_arr = -rel_arr
  } else if(trajectory_type == 'traj'){
    rel_arr[, , yr:time_horizon] = current_parcel_traj[, , yr:time_horizon] - as.vector(current_parcel_ecology)
  }
  
  rel_arr = apply(rel_arr, 3, sum)
  return(rel_arr)
}





# collated_offsets = collated_parcel_sets$offsets
# collated_developments = collated_parcel_sets$developments
# parcel_set_num = length(parcel_sets_object$offsets)

assess_NNL <- function(offset_rest_gains, offset_avoided_degs, dev_rest_gains, dev_avoided_degs, eco_dims, parcel_set_num, time_horizon, offset_yrs){
  
  net_offset_gains = offset_rest_gains + offset_avoided_degs
  net_dev_gains = dev_rest_gains + dev_avoided_degs
    
  net_gains = net_offset_gains + net_dev_gains
  system_gains = sum_cols_multi(net_gains)
  if (any(system_gains >0)){
    system_NNL = min(which(system_gains > 0))
  } else {
    system_NNL = 0
  }
  
  rec_array = array(0, c(parcel_set_num, eco_dims))
  NNL_yrs = rec_array
  failed_parcel_sets = rec_array
  thresh = 0
  for (parcel_set_ind in 1:parcel_set_num){
    offset_yr = offset_yrs[[parcel_set_ind]]
    for (eco_ind in 1:eco_dims){
      current_net_gain = net_gains[, parcel_set_ind, eco_ind]
      current_NNL_yr = which(current_net_gain > thresh)
      if (length(current_NNL_yr) > 0){
        NNL_yrs[parcel_set_ind, eco_ind] = min(current_NNL_yr) - offset_yr
      } 
    }
  }
  
  failed_parcel_sets = which(NNL_yrs == 0)
  
  #   if (length(failed_parcel_sets) > 0){
  #     NNL_success_yrs = NNL_yrs[-failed_parcel_sets]
  #   }
  
  NNL_object = list()
  NNL_object$net_offset_gains = net_offset_gains
  NNL_object$net_dev_gains = net_dev_gains
  NNL_object$net_gains = net_gains
  NNL_object$system_gains = system_gains
  NNL_object$system_NNL = system_NNL
  
  NNL_object$NNL_yrs = NNL_yrs
  NNL_object$failed_parcel_sets = failed_parcel_sets
  NNL_object$success = 1 - length(failed_parcel_sets)/parcel_set_num
  return(NNL_object)
  
}



# parcel_sets_object = outs$parcel_sets_object
# time_steps = global_params$time_steps
# eco_dims = global_params$eco_dims
# current_assessed_object = parcel_sets_object$offsets
# parcel_set_ind = 1
# eco_ind = 1

# collate_parcel_sets <- function(parcel_sets_object, global_params, eco_dims){
#   collated_parcel_sets = list()
#   collated_parcel_sets$offsets = collate_parcel_sets_object(parcel_sets_object$offsets, global_params, eco_dims)
#   collated_parcel_sets$developments = collate_parcel_sets_object(parcel_sets_object$developments, global_params, eco_dims)
#   collated_parcel_sets$NNL_object = assess_NNL(collated_parcel_sets$offsets, collated_parcel_sets$developments, eco_dims, parcel_set_num = length(parcel_sets_object$offsets))
#   return(collated_parcel_sets)
# }



assess_degs <- function(parcel_set_to_assess, cfac_type, collate_dims){
  
  if (cfac_type == 'standard'){
    current_avoided_deg = apply(parcel_set_to_assess$avoided_degs, MARGIN = 1, sum)
  } else if (cfac_type == 'include_clearing'){
    current_avoided_deg = apply(parcel_set_to_assess$avoided_degs_include_clearing, MARGIN = 1, sum)
  } else if (cfac_type == 'include_clearing_offsets'){
    current_avoided_deg = apply(parcel_set_to_assess$avoided_degs_include_clearing_offsets, MARGIN = 1, sum)
  }
  current_avoided_deg = t(t(current_avoided_deg))
  return(current_avoided_deg)
}










find_decline_rate <- function(decline_rates, current_parcel_ind, eco_ind){
  loc = ind2sub(dim(decline_rates)[1], current_parcel_ind)
  current_dec_rate = decline_rates[loc[1], loc[2], eco_ind]
  return(current_dec_rate)
}








# outputs = outs$model_outputs
# land_parcels = parcels$land_parcels
# trajectories = outputs$trajectories
# time_steps = global_params$time_steps
# parcel_set_ind = 1
# current_sets_object = outputs$offsets
# parcel_num_remaining = outputs$parcel_num_remaining
# decline_rates = decline_rates_initial
# parcel_count_ind = 1
# cfac_type = global_params$post_facto_cfac_type
# current_parcel_num_remaining = parcel_num_remaining[parcel_set_ind]

calc_parcel_set_cfacs <- function(outputs, global_params, decline_rates){
  net_cfacs_object = list()
  net_cfacs_object$offsets = calc_cfacs_post_facto(outputs$offsets, global_params, decline_rates, parcel_num_remaining = outputs$parcel_num_remaining)
  net_cfacs_object$developments = calc_cfacs_post_facto(outputs$developments, global_params, decline_rates, parcel_num_remaining = outputs$parcel_num_remaining)
  return(net_cfacs_object)
}


calc_cfacs_post_facto <- function(current_sets_object, global_params, decline_rates, parcel_num_remaining){
  
  parcel_set_num = global_params$total_dev_num
  
  cfacs = vector('list', parcel_set_num)
  cfacs_include_clearing = vector('list', parcel_set_num)
  cfacs_include_clearing_offsets = vector('list', parcel_set_num)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    yr = current_sets_object$yr[[parcel_set_ind]]
    time_horizon = global_params$time_steps - yr
    parcel_indexes = current_sets_object$parcel_indexes[[parcel_set_ind]]
    
    current_cfacs = build_cfacs_list(global_params, decline_rates, (parcel_ecologies = current_sets_object$parcel_ecologies[[parcel_set_ind]]), parcel_indexes, time_horizon, (eco_dims = global_params$eco_dims))
    cfacs[[parcel_set_ind]] = current_cfacs
    if (global_params$adjust_counters_post_facto == TRUE){
      cfacs_include_clearing[[parcel_set_ind]] = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing', region_params, global_params, 
                                                            parcel_num_remaining[parcel_set_ind], decline_rates, parcel_indexes, time_horizon, yr)
      cfacs_include_clearing_offsets[[parcel_set_ind]] = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing_offsets', region_params, global_params, 
                                                                    parcel_num_remaining[parcel_set_ind], decline_rates, parcel_indexes, time_horizon, yr)
    }
  }
  
  cfacs_object = list()
  cfacs_object$cfacs = cfacs
  cfacs_object$cfacs_include_clearing = cfacs_include_clearing
  cfacs_object$cfacs_include_clearing_offsets = cfacs_include_clearing_offsets
  return(cfacs_object)
}  




adjust_cfacs <- function(current_cfacs, adjusted_cfac_type, region_params, global_params, current_parcel_num_remaining, decline_rates, parcel_indexes, time_horizon, yr){
  
  dev_prob <- find_dev_probability(global_params$dev_vec, yr, time_horizon, current_parcel_num_remaining)
  
  if (adjusted_cfac_type == 'include_clearing'){
    counter_probs = 1 - cumsum(dev_prob)
    adjusted_cfacs = weight_counters(current_cfacs, global_params, counter_probs, time_horizon)
  } else if (adjusted_cfac_type == 'include_clearing_offsets'){
    offset_prob = dev_prob
    counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
    weighted_counters = weight_counters(current_cfacs, global_params, counter_probs, time_horizon)
    summed_offset_projections = sum_offset_projections(current_cfacs, offset_prob, global_params, decline_rates, parcel_indexes, time_horizon)
    adjusted_cfacs = sum_clearing_offsets(weighted_counters, summed_offset_projections)
  }
  
  return(adjusted_cfacs)
  
}


weight_counters <- function(current_cfacs, global_params, counter_probs, time_horizon){
  
  parcel_num = length(current_cfacs)
  eco_dims = global_params$eco_dims
  weighted_counters = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    weighted_counters[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  for (parcel_count_ind in seq_len(parcel_num)){
    
    for (eco_ind in seq_len(eco_dims)){
      
      current_cfac = current_cfacs[[parcel_count_ind]][[eco_ind]]
      projected_dims = dim(current_cfac)
      
      counter_prob_array = rep(counter_probs, projected_dims[1]*projected_dims[2])
      dim(counter_prob_array) = c(length(counter_probs), c(projected_dims[2], projected_dims[1]))
      counter_prob_array = aperm(counter_prob_array, c(3, 2, 1))
      weighted_counters[[parcel_count_ind]][[eco_ind]] = counter_prob_array*current_cfac
      
    } 
  }
  
  return(weighted_counters)
  
}


sum_offset_projections <- function(current_cfacs, offset_prob, global_params, decline_rates, parcel_indexes, time_horizon){
  
  parcel_num = length(current_cfacs)
  eco_dims = global_params$eco_dims
  summed_offset_projections = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    summed_offset_projections[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  for (parcel_count_ind in seq_len(parcel_num)){

    for (eco_ind in seq_len(eco_dims)){
      current_dec_rate = find_decline_rate(decline_rates, parcel_indexes[parcel_count_ind], eco_ind)
      current_cfac = current_cfacs[[parcel_count_ind]][[eco_ind]]
      projected_dims = dim(current_cfac)
      
      current_summed_offset_projections = array(0, projected_dims)
      
      for (proj_yr in which(offset_prob > 0)){
        
        current_parcel_ecology = current_cfac[, , proj_yr]
        current_offset_prob = offset_prob[proj_yr]
        current_offset_projection = predict_parcel_traj(current_parcel_ecology, parcel_traj_type = global_params$offset_action_type, global_params, current_dec_rate, time_horizon = (time_horizon - proj_yr + 1))
        current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] + current_offset_prob*current_offset_projection
      } 
      summed_offset_projections[[parcel_count_ind]][[eco_ind]] = current_summed_offset_projections
    } 
    
  }
  
  return(summed_offset_projections)
}


sum_clearing_offsets <- function(cfacs_include_clearing, summed_offset_projections, eco_dims){
  parcel_num = length(cfacs_include_clearing)
  eco_dims = global_params$eco_dims
  cfacs_include_clearing_offsets = vector('list', parcel_num)
  
  for (parcel_count_ind in 1:parcel_num){
    cfacs_include_clearing_offsets[[parcel_count_ind]] = vector('list', eco_dims)
  }
  
  for (parcel_count_ind in seq_len(parcel_num)){
    for (eco_ind in seq_len(eco_dims)){
      cfacs_include_clearing_offsets[[parcel_count_ind]][[eco_ind]] = summed_offset_projections[[parcel_count_ind]][[eco_ind]] + cfacs_include_clearing[[parcel_count_ind]][[eco_ind]]
    }
  }
  return(cfacs_include_clearing_offsets)
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


#collated_parcel_sets_object = realisations[[1]]$collated_parcel_sets_object 
# parcel_trajs = outs$parcel_trajs
# assessed_set_index = 5


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




sum_parcel_sets_as_list <- function(rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon){
  summed_parcel_sets = vector('list', 3)
  summed_parcel_sets$rest_gains = sum_cols_multi(rest_gains[, parcel_set_indexes, ])
  summed_parcel_sets$avoided_degs = sum_cols_multi(avoided_degs[, parcel_set_indexes, ])
  summed_parcel_sets$net_gains = sum_cols_multi(net_gains[, parcel_set_indexes, ])
  return(summed_parcel_sets)
}



sum_parcel_sets <- function(rest_gains, avoided_degs, net_gains, parcel_set_indexes, time_horizon, eco_dims){
  summed_parcel_sets = array(0, c(time_horizon, 3, eco_dims))
  for (eco_ind in 1:eco_dims){
    summed_parcel_sets[, 1, ] = sum_cols_multi(rest_gains[, parcel_set_indexes, ])
    summed_parcel_sets[, 2, ] = sum_cols_multi(avoided_degs[, parcel_set_indexes, ])
    summed_parcel_sets[, 3, ] = sum_cols_multi(net_gains[, parcel_set_indexes, ])
  }
  return(summed_parcel_sets)
}

sum_cols_multi <- function(arr_to_use){
  
  if (length(dim(arr_to_use)) <= 1){
    
    arr_out = t(t(arr_to_use))
  } else if (length(dim(arr_to_use)) == 2){
    arr_out = apply(arr_to_use, MARGIN = 1, sum)
    arr_out = t(t(arr_out))
  } else if (length(dim(arr_to_use)) == 3){
    arr_out = apply(arr_to_use, MARGIN = c(1, 3), sum)
    dim(arr_out) = c(dim(arr_out), 1)
    arr_out = aperm(arr_out, c(1, 3, 2))
  }
  return(arr_out)
}



sum_parcel_trajectories <- function(traj_list, eco_dims, parcel_indexes, time_horizon){
  
  parcel_num = length(parcel_indexes)
  parcel_trajs = array(0, c(time_horizon, parcel_num, eco_dims))
  for (parcel_count_ind in seq_len(parcel_num)){
    parcel_ind = parcel_indexes[parcel_count_ind]
    for (eco_ind in seq_len(eco_dims)){
      parcel_trajs[, parcel_count_ind, ] = apply(traj_list[[parcel_ind]][[eco_ind]], MARGIN=3, sum)
    }  
  }
  
  return(parcel_trajs)
  
}


# find_parcel_traj_by_list <- function(trajectories_list, time_horizon, parcel_indexes){
#   
#   parcel_num = length(parcel_indexes)
#   parcel_trajs = array(0, c(time_horizon, parcel_num))
#   eco_ind = 1
#   for (parcel_count_ind in seq_len(parcel_num)){
#     parcel_ind = parcel_indexes[parcel_count_ind]
#     parcel_trajs[, parcel_count_ind] =  apply(trajectories_list[[parcel_ind]][[eco_ind]], MARGIN=3, sum)
#   }
#   
#   return(parcel_trajs)
# }





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






# 
# 
# collate_parcel_sets_object <- function(current_assessed_object, global_params, eco_dims){
#   
#   collated_object = list()
#   
#   parcel_set_num = length(current_assessed_object)
#   rest_gains = sum_current_gains_degs(object_to_collate = current_assessed_object$rest_gains, parcel_set_num, eco_dims)
#   
#   collated_object$yr = parcel_set_yr
#   collated_object$rest_gains = rest_gains
#   collated_object$avoided_degs = avoided_degs
#   collated_object$avoided_degs_include_clearing = avoided_degs_include_clearing
#   collated_object$avoided_degs_include_clearing_offsets = avoided_degs_include_clearing_offsets
#   collated_object$initial_parcel_sums = initial_parcel_sums
#   
#   return(collated_object)
# }




sum_current_gains_degs <- function(object_to_collate, parcel_set_num, eco_dims){
  
  collate_dims = c(dim(object_to_collate)[1], parcel_set_num, eco_dims)
  collate_array = array(0, collate_dims)
  
  for (parcel_set_ind in seq_len(parcel_set_num)){
    parcel_set_to_assess = current_assessed_object[[parcel_set_ind]]
    for (eco_ind in seq_len(eco_dims)){
      collate_array = apply(object_to_collate, MARGIN = 1, sum)
    }
  }
  
}




two_plot <- function(plot_a, plot_b, colours){
  mx = max(c(plot_a, plot_b))
  mn = min(c(plot_a, plot_b))
  plot(plot_a, type = 'l', col = colours[1], ylim = c(mn, mx))
  lines(plot_b, col = colours[2], ylim = c(mn, mx))
}


extract_3D_parcel <- function(current_parcel, trajectories){
  loc_1 = ind2sub(dim(trajectories)[1], current_parcel[1])
  loc_2 = ind2sub(dim(trajectories)[1], current_parcel[length(current_parcel)])
  parcel_sz = c((loc_2[1] - loc_1[1] + 1), (loc_2[2] - loc_1[2] + 1), dim(trajectories)[3])
  parcel_3D = array(0, parcel_sz)
  parcel_3D[, ,] = trajectories[loc_1[1]:loc_2[1], loc_1[2]:loc_2[2], ]
  return(parcel_3D)
}

insert_parcel_trajectory <- function(trajectories, current_parcel, parcel_3D){
  loc_1 = ind2sub(dim(trajectories)[1], current_parcel[1])
  loc_2 = ind2sub(dim(trajectories)[1], current_parcel[length(current_parcel)])
  parcel_sz = c((loc_2[1] - loc_1[1] + 1), (loc_2[2] - loc_1[2] + 1), dim(trajectories)[3])
  trajectories[loc_1[1]:loc_2[1], loc_1[2]:loc_2[2], ] = parcel_3D 
  return(trajectories)
}



write_realisation_sums <- function(current_realisations, realisation_ind, current_summed_realisation){
  current_realisations$net_gains[, realisation_ind, ] = current_summed_realisation$net_gains
  current_realisations$avoided_degs[, realisation_ind, ] = current_summed_realisation$avoided_degs
  current_realisations$rest_gains[, realisation_ind, ] = current_summed_realisation$rest_gains
  return(current_realisations)
}

write_summed_programs <- function(summed_program_realisations, realisation_ind, current_summed_program){
  summed_program_realisations$net_program_value[, realisation_ind, ] = current_summed_program$net_trajs
  summed_program_realisations$net_offset_value[, realisation_ind, ] = current_summed_program$offset_trajs
  summed_program_realisations$net_development_value[, realisation_ind, ] = current_summed_program$dev_trajs
  return(summed_program_realisations)
}


collate_program_cfacs <- function(realisations, time_horizon, eco_dims){
  rec_array = array(0, c(time_horizon, realisation_num, eco_dims))
  realisation_num = length(realisations)
  eco_ind = 1
  for (realisation_ind in 1:realisation_num){
    rec_array[, realisation_ind, eco_ind] = realisations[[realisation_ind]]$program_cfacs$net
  }
  return(rec_array)
}



#wrap realisations up into usable form for plotting
collate_realisations <- function(realisations, parcel_set_indexes, time_horizon, eco_dims){

  collated_net_cfacs <- collate_program_cfacs(realisations, time_horizon, eco_dims)    #group individual net realisation counterfactual values
  
  outs = list()
  summed_dev_realisations = list()
  
  realisation_num = length(realisations)
  rec_array = array(0, c(time_horizon, realisation_num, eco_dims))
  summed_dev_realisations$net_gains = rec_array
  summed_dev_realisations$avoided_degs = rec_array
  summed_dev_realisations$rest_gains = rec_array
  net_realisations = rec_array
  net_parcel_set_realisations = rec_array
  
  summed_offset_realisations = summed_dev_realisations

  summed_program_realisations = list()
  summed_program_realisations$net_program_value = rec_array
  summed_program_realisations$net_offset_value = rec_array
  summed_program_realisations$net_development_value = rec_array
  
  program_cfac_realisations = rec_array
  
  rm(rec_array)
  
  
  
  
  system_NNLs = array(0, c(realisation_num, eco_dims))
  rec_array = array(0, c(length(parcel_set_indexes), realisation_num))
  ALL_parcel_set_NNLs = rec_array
  ALL_offset_initial_parcel_sums = rec_array
  rm(rec_array)
  
  eco_ind = 1
  
  for (realisation_ind in seq_len(realisation_num)){
    current_realisation = realisations[[realisation_ind]]
    collated_object = current_realisation$collated_parcel_sets_object
    
    current_summed_devs = sum_parcel_sets_as_list(collated_object$devs$rest_gains, collated_object$devs$avoided_degs, collated_object$NNL_object$net_dev_gains, parcel_set_indexes, time_horizon)
    current_summed_offsets = sum_parcel_sets_as_list(collated_object$offsets$rest_gains, collated_object$offsets$avoided_degs, collated_object$NNL_object$net_offset_gains, parcel_set_indexes, time_horizon)
    summed_dev_realisations = write_realisation_sums(summed_dev_realisations, realisation_ind, current_summed_devs)
    summed_offset_realisations = write_realisation_sums(summed_offset_realisations, realisation_ind, current_summed_offsets)
    summed_program_realisations = write_summed_programs(summed_program_realisations, realisation_ind, collated_object$summed_program_parcels)

    net_parcel_set_realisations[, realisation_ind, ] = sum_cols_multi(collated_object$NNL_object$net_gains[, parcel_set_indexes, ])
    net_realisations[, realisation_ind, ] = apply(current_realisation$model_outputs$trajectories[[eco_ind]], MARGIN = 3, sum)
    if (length(collated_object$NNL_object$system_NNL) > 0){
      system_NNLs[realisation_ind, ] = collated_object$NNL_object$system_NNL
    }
    ALL_parcel_set_NNLs[, realisation_ind] = collated_object$NNL_object$NNL_yrs
    ALL_offset_initial_parcel_sums[, realisation_ind] = collated_object$offsets$initial_parcel_sums
  }
  
  system_NNL_fails = which(system_NNLs == 0)
  
  outs$collated_net_cfacs = collated_net_cfacs
  outs$system_NNL_fails = system_NNL_fails
  outs$ALL_offset_initial_parcel_sums = ALL_offset_initial_parcel_sums
  outs$ALL_parcel_set_NNLs = ALL_parcel_set_NNLs
  outs$system_NNLs = system_NNLs
  outs$summed_dev_realisations = summed_dev_realisations
  outs$summed_offset_realisations = summed_offset_realisations
  outs$net_parcel_set_realisations = net_parcel_set_realisations
  outs$net_realisations = net_realisations
  outs$summed_program_realisations = summed_program_realisations
  return(outs)
  
}


# collate_parcel_realisations_multi <- function(record_parcel_sets, realisations, parcel_ind, land_parcels, global_params, eco_dims){
#   realisation_num = length(realisations)
#   current_parcel = select_land_parcel(land_parcels, parcel_ind)
#   
#   parcel_realisations = list()
#   parcel_realisations$parcel_3D = vector('list', realisation_num)
#   parcel_realisations$sums = array(0, c(global_params$time_steps, realisation_num, eco_dims))
#   
#   for (realisation_ind in seq_len(realisation_num)){
#     for (eco_ind in seq_len(eco_dims)){
#       if (record_parcel_sets == FALSE){
#         current_realisation = realisations[[realisation_ind]]
#       } else current_realisation = (realisations[[realisation_ind]]$trajectories)
#     
#       current_parcel_3D = extract_3D_parcel(current_parcel, current_realisation[[eco_ind]])
#       parcel_realisations$parcel_3D[[realisation_ind]][[eco_ind]] = current_parcel_3D
#       parcel_realisations$sums[, realisation_ind, eco_ind] = apply(current_parcel_3D, 3, sum)
#     }
#   }
#   return(parcel_realisations)
# }





tally_devs_offsets <- function(realisations, parcels, global_params){
  
  tally_object = list()
  realisation_num = length(realisations)
  parcel_num = length(parcels$land_parcels)
  offset_tally = array(0, c(global_params$time_steps, parcel_num))
  development_tally = array(0, c(global_params$time_steps, parcel_num))
  
  for (realisation_ind in seq_len(realisation_num)){
    current_realisation = realisations[[realisation_ind]]
    current_dev_num = length(current_realisation$development_list)
    for (parcel_set_ind in seq_len(current_dev_num)){
      yr = current_realisation$developments[[parcel_set_ind]]$yr
      current_offset_inds = current_realisation$offsets[[parcel_set_ind]]$parcel_indexes
      current_dev_inds = current_realisation$developments[[parcel_set_ind]]$parcel_indexes
      offset_tally[yr, current_offset_inds] = offset_tally[yr, current_offset_inds] + 1
      development_tally[yr, current_dev_inds] = development_tally[yr, current_dev_inds] + 1
    }
  }
  
  tally_object$offset_tally = offset_tally
  tally_object$development_tally = development_tally
  
  return(tally_object)
  
}
  






sum_parcels_multi <- function(trajectories, parcel_indexes_to_use, land_parcels, global_params, eco_dims){
  
  parcel_num = length(parcel_indexes_to_use)
  parcel_sums = array(0, c(global_params$time_steps, parcel_num, eco_dims))
  
  for (parcel_ind in seq_len(parcel_num)){   
    current_parcel_ind = parcel_indexes_to_use[parcel_ind]
    current_parcel = select_land_parcel(land_parcels, current_parcel_ind)
    for (eco_ind in seq_len(eco_dims)){
      current_parcel_trajectory = extract_3D_parcel(current_parcel, trajectories[[eco_ind]])
      parcel_sums[, parcel_ind, eco_ind] = apply(current_parcel_trajectory, 3, sum)
    }  
  }
  
  return(parcel_sums)  
}

sum_regions <- function(parcel_sums, parcel_index_list, regions, global_params){
  region_num = length(regions)
  region_sums = array(0, c(global_params$time_steps, region_num))
  parcel_num = length(parcel_index_list)
  
  for (parcel_ind in seq_len(parcel_num)){
    current_parcel_ind = parcel_index_list[parcel_ind]
    region_ind = find_region(current_parcel_ind, regions)
    region_sums[, region_ind] = region_sums[, region_ind] + parcel_sums[, parcel_ind]
  }
  
  return(region_sums)
}

find_region <- function(parcel, regions){
  
  for (region_ind in seq_len(length(regions))){
    if (is.element(parcel, regions[[region_ind]])){
      return(region_ind)
    }
  }
}


true_sums <- function(parcel_indexes, object_sums, counter_sums){
  true_sums_object = list()
  true_sums_object$parcel_sums = object_sums$parcel_sums - counter_sums$parcel_sums[, parcel_indexes]
  true_sums_object$net_sums = object_sums$net_sums - counter_sums$net_sums
  return(true_sums_object)
}


find_sums <- function(trajectories, parcels_inds, parcels, global_params){
  object = list()
  object$parcel_sums = sum_parcels(trajectories, parcel_indexes, parcels$land_parcels, time_steps)
  object$region_sums = sum_regions(object$parcel_sums, parcel_indexes, parcels$regions, time_steps)
  object$net_sums = rowSums(object$region_sums)
  return(object)
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



cartesian_mesh <- function(N, M){
  mesh = list()
  xx = seq(-floor(M/2), (ceiling(M/2) - 1))
  yy = seq(-floor(N/2), (ceiling(N/2) - 1))
  x = matrix(rep(xx,each=N),nrow=N);
  y = matrix(rep(yy,M),nrow=N)
  mesh$x = x
  mesh$y = y
  return(mesh)
#  m=length(x); n=length(y);
#  X=matrix(rep(x,each=n),nrow=n);
#  Y=matrix(rep(y,m),nrow=n)
}
  
gauss <- function(x, y, sig_x, sig_y){
  g = exp(-x^2/(sig_x^2)) * exp(-y^2/(sig_y^2))
  return(g)
}


fftshift <- function(fft_array){
  numDims = 2
  idx <- vector('list', numDims)
  for (k in seq_len(numDims)){
    m = dim(fft_array)[k];
    p = ceiling(m/2);
    idx[[k]] = c((p+1):m, (1:p));
  }
  fft_array = fft_array[idx[[1]], ]
  fft_array = fft_array[, idx[[2]]]
  return(fft_array)
}


Blur_2D <- function(A, sig_x, sig_y){
  dims = dim(A)
  M = dims[1]
  N = dims[2]
  mesh = cartesian_mesh(M, N)
  knl = gauss(mesh$x, mesh$y, sig_x, sig_y)
  knl = knl / sum(knl)
  convolve = fftshift( fft( fftshift(knl), inverse = TRUE )) * fftshift( fft( fftshift(A), inverse = TRUE ) )
  A = (fftshift( fft( fftshift( convolve ) ) ))/(M*N)
  A = Re(A)
  return(A)
}






# plot_parcel_set_from_collated_object <- function(collated_object, parcel_set_indexes, global_params, eco_dims){
# 
#   collated_offsets = collated_object$offsets
#   collated_developments = collated_object$developments
#   
#   plot_list = vector('list', 3)
#   plot_list[[1]] = sum_parcel_sets(collated_developments, parcel_set_indexes, global_params, eco_dims)
#   plot_list[[2]] = sum_parcel_sets(collated_offsets, parcel_set_indexes, global_params, eco_dims)
#   net_array = array(0, c(global_params$time_steps, 3, eco_dims))
#   net_array[, 1, ] = plot_list[[1]][, 3, ]
#   net_array[, 2, ] = plot_list[[2]][, 3, ]
#   net_array[, 3, ] = plot_list[[1]][, 3, ] + plot_list[[2]][, 3, ]
#   plot_list[[3]] = net_array
#     
#   lim_vec = abind(plot_list[[1]][, , 1], plot_list[[2]][, , 1], plot_list[[3]][, , 1])
#   mx = max(lim_vec)
#   mn = min(lim_vec)
#   overlay_plots_as_vector(plot_list[[1]][, , 1], yticks = 'y', axis_lab = TRUE, ylims = c(mn, mx), (heading = "developments"), ylab = '', col_vec = c('red', 'red', 'red'), lty_vec = c(1, 2, 1), 
#                 lwd_vec = c(1, 1, 3), legend_vec = c('Net Restoration Gains', 'Net Avoided Degredation', 'Net Losses'), legend_loc = 'topleft')
#   overlay_plots_as_vector(plot_list[[2]][, , 1], yticks = 'n', axis_lab = TRUE, ylims = c(mn, mx), (heading = "offsets"), ylab = '', col_vec = c('blue', 'blue', 'blue'), lty_vec = c(1, 2, 1), 
#                 lwd_vec = c(1, 1, 3), legend_vec = c('Net Restoration Gains', 'Net Avoided Degredation', 'Net Gains'), legend_loc = 'topleft')
#   overlay_plots_as_vector(plot_list[[3]][, , 1], yticks = 'n', axis_lab = TRUE, ylims = c(mn, mx), (heading = "net outcome"), ylab = '', col_vec = c('red', 'blue', 'black'), lty_vec = c(1, 1, 1), 
#                 lwd_vec = c(1, 1, 3), legend_vec = c('Net Losses', 'Net Gains', 'Net Outcome'), legend_loc = 'topleft')
#   
# }





# calc_rel_intial <- function(trajectory_type, global_params, parcel_trajectory, yr, parcel_ecology){
#   rel_arr = array(0, c(dim(parcel_trajectory)[1:2], global_params$time_steps))
#   rel_arr[, , yr:global_params$time_steps] = parcel_trajectory - as.vector(parcel_ecology)
#   if (trajectory_type == 'cfac'){
#     rel_arr = -rel_arr
#   }
#   rel_arr = apply(rel_arr, 3, sum)
# }
# 







# collate_parcel_set_element <- function(global_params, region_params, current_parcel_set, current_cfac_set, adjusted_cfac_set, land_parcels, trajectories, decline_rates){
#   
#   parcel_set_element = list()
#   
#   yr = current_parcel_set$yr
#   parcel_indexes = current_parcel_set$parcel_indexes
#   parcel_ecologies_list = current_parcel_set$parcel_ecologies
#   parcel_num = length(parcel_indexes)
#   
#   rec_array = array(0, c(global_params$time_steps, parcel_num, global_params$eco_dims))
#   avoided_degs = rec_array
#   avoided_degs_include_clearing = rec_array
#   avoided_degs_include_clearing_offsets = rec_array
#   rest_gains = rec_array
#   
#   initial_parcel_sums = array(0, c(parcel_num, global_params$eco_dims))
#   
#   time_horizon = global_params$time_steps - yr 
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     current_parcel = select_land_parcel(land_parcels, current_parcel_ind = parcel_indexes[parcel_count_ind])
#     
#     for (eco_ind in seq_len(global_params$eco_dims)){
#       current_parcel_trajectory = extract_3D_parcel(current_parcel, trajectories[[eco_ind]][, , yr:global_params$time_steps])
#       parcel_ecology = parcel_ecologies_list[[parcel_count_ind]][, , eco_ind] 
#       rest_gains[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'restoration', global_params, current_parcel_trajectory, yr, parcel_ecology)
#       avoided_degs[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'cfac', global_params, current_cfac_set[[parcel_count_ind]][[eco_ind]], yr, parcel_ecology)
#       if (global_params$adjust_counters_post_facto == TRUE){
#         avoided_degs_include_clearing[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'cfac', global_params, adjusted_cfac_set$include_clearing[[parcel_count_ind]][[eco_ind]], yr, parcel_ecology)
#         avoided_degs_include_clearing_offsets[, parcel_count_ind, eco_ind] = calc_rel_intial(trajectory_type = 'cfac', global_params, adjusted_cfac_set$include_clearing_offsets[[parcel_count_ind]][[eco_ind]], yr, parcel_ecology)
#       }
#       initial_parcel_sums[parcel_count_ind, eco_ind] = sum(parcel_ecology)
#     }
#   }
#   
#   parcel_set_element$yr = yr
#   parcel_set_element$rest_gains = rest_gains
#   parcel_set_element$avoided_degs = avoided_degs
#   parcel_set_element$parcel_indexes = parcel_indexes
#   parcel_set_element$parcel_vals_used = current_parcel_set$parcel_vals_used
#   parcel_set_element$initial_parcel_sums = initial_parcel_sums
#   parcel_set_element$avoided_degs_include_clearing = avoided_degs_include_clearing
#   parcel_set_element$avoided_degs_include_clearing_offsets = avoided_degs_include_clearing_offsets
#   return(parcel_set_element)
#   
# }


# 
# 
# outputs = outs$model_outputs
# land_parcels = parcels$land_parcels
# trajectories = outputs$trajectories
# time_steps = global_params$time_steps
# parcel_set_ind = 1
# current_sets_object = outputs$offsets
# current_parcel_set = current_sets_object[[parcel_set_ind]]
# decline_rates = decline_rates_initial
# parcel_count_ind = 1
# parcel_sets_cfacs = outs$parcel_sets_cfacs
# current_cfac_sets_object = parcel_sets_cfacs$offsets

# 
# group_parcel_sets <- function(outputs, parcel_sets_cfacs, land_parcels, global_params, decline_rates){
#   parcel_sets_object = list()
#   
#   parcel_sets_object$offsets = find_parcel_sets(outputs$offsets, parcel_sets_cfacs$offsets, land_parcels, outputs$trajectories, global_params, decline_rates)
#   parcel_sets_object$developments = find_parcel_sets(outputs$developments, parcel_sets_cfacs$developments, land_parcels, outputs$trajectories, global_params, decline_rates)
#   return(parcel_sets_object)
# }
# 
# 
# find_parcel_sets <- function(current_sets_object, current_cfac_sets_object, land_parcels, trajectories, global_params, decline_rates){
#   
#   parcel_set_num = length(current_sets_object)
#   parcel_set_object = vector('list', parcel_set_num)
#   
#   for (parcel_set_ind in seq_len(parcel_set_num)){
#     current_parcel_set = current_sets_object[[parcel_set_ind]]
#     current_cfac_set = current_cfac_sets_object$cfacs[[parcel_set_ind]]
#     if (global_params$adjust_counters_post_facto == TRUE){
#       adjusted_cfac_set = current_cfac_sets_object$adjusted_cfacs[[parcel_set_ind]]
#     }
#     parcel_set_object[[parcel_set_ind]] = collate_parcel_set_element(global_params, region_params, current_parcel_set, current_cfac_set, adjusted_cfac_set, land_parcels, trajectories, decline_rates)
#   }
#   
#   return(parcel_set_object)
#   
# }  






# eco_shift <- function(parcel_vals, min_eco_val, max_eco_val, decline_rate, time_horizon){
#   
#   t_sh = -1/decline_rate * log( ((parcel_vals - min_eco_val)/(max_eco_val - parcel_vals)))
#   eco_shift = min_eco_val + (max_eco_val - min_eco_val)/(1 + exp(-decline_rate*(time_horizon - t_sh)))
#   return(eco_shift)
# }




# 
# initialise_ecology_by_parcel <- function(global_params, parcels){
#   land_parcels = parcels$land_parcels
#   land_parcel_num = parcels$land_parcel_num
#   initial_ecology = vector('list', land_parcel_num)
#   sc = global_params$max_initial_eco_vals - global_params$min_initial_eco_vals - global_params$initial_eco_noise
#   
#   min_parcel_vals = runif(2, min = global_params$min_eco_val, max = (global_params$max_eco_val - global_params$initial_eco_noise))
#   for (parcel_ind in seq_len(land_parcel_num)){
#     current_parcel = land_parcels[[parcel_ind]]
#     parcel_length = length(current_parcel)
#     current_eco_array = global_params$initial_eco_noise*array(runif(parcel_length*eco_dims), c(dim(current_parcel), eco_dims))
#     sc_array = rep(global_params$min_initial_eco_vals, parcel_length)
#     dim(sc_array) = c(eco_dims, parcel_length, 1)
#     sc_array = aperm(sc_array, c(3, 2, 1))
#     dim(sc_array) = dim(current_eco_array)
#     
#     for (eco_ind in 1:eco_dims){
#       current_eco_slice = current_eco_array[, , eco_dim] * rep(global_params$min_initial_eco_vals[eco_ind], length(current_parcel))
#        rep(global_params$min_initial_eco_vals[eco_ind], length(current_parcel))
#   #  min_parcel_vals = global_params$min_initial_eco_val[eco_ind] + runif(land_parcel_num)*(global_params$max_initial_eco_val[eco_ind] - global_params$min_initial_eco_val[eco_ind] - global_params$initial_eco_noise[eco_ind])
#     
#       
#   #    initial_parcel_ecology = min_parcel_vals[parcel_ind] + global_params$initial_eco_spread*runif(length(current_parcel))
#   #    dim(initial_parcel_ecology) = dim(current_parcel)
#   #    initial_ecology[[parcel_ind]][, , eco_ind] = initial_parcel_ecology
#   }
#  # initial_ecology = initial_ecology + global_params$initial_eco_noise*matrix(runif(global_params$ecology_size*global_params$ecology_size),global_params$ecology_size,global_params$ecology_size)
#   return(initial_ecology)
# }




# build_cfacs_by_parcel <- function(global_params, decline_rates, land_parcels, current_ecology){
#   
#   cfacs = array(0, c(global_params$ecology_size, global_params$ecology_size, global_params$time_steps))
#   parcel_num = length(land_parcels)
#   
#   for (parcel_ind in seq_len(parcel_num)){
#     
#     current_parcel = land_parcels[[parcel_ind]]
#     current_dec_rate = decline_rates[parcel_ind]  
#     current_parcel_ecology = initial_ecology[current_parcel]
#     dim(current_parcel_ecology) = dim(current_parcel)
#     projected_ecology = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = current_dec_rate, (time_horizon = global_params$time_steps - 1))
#     cfacs = insert_parcel_trajectory(cfacs, current_parcel, projected_ecology)
#     
#   }
#   
#   return(cfacs)
# }



# build_decline_rates <- function(parcels, region_params){
#   regions = parcels$regions
#   region_num = length(regions)
# #  region_dec_rates = vector('list', region_num)
#   decline_rates = array(0, dim(parcels$parcel_indexes))
#   
#   for (region_ind in seq_len(region_num)){ 
#     current_region = regions[[region_ind]]
#     current_parcel_num = length(current_region)    
#     decline_params = c(length(current_region), region_params[[region_ind]]$mean_decline_rate, region_params[[region_ind]]$decline_rate_std) #params$decline_rate_std[region_ind])
#     current_decline_rates = matrix(rnorm(decline_params[1], mean = decline_params[2], sd = decline_params[3]), ncol = ncol(current_region))
# #    dec_rates[[region_ind]] = current_decline_rates
#     decline_rates[current_region] = current_decline_rates
#   }
#   
#   return(decline_rates)
# }




# adjust_counters <-function(cfacs, region_params, global_params, parcel_num, time_horizon, yr){
#   
#   eco_dims = global_params$eco_dims
#   rec_list = vector('list', parcel_num)
#   
#   for (parcel_count_ind in 1:parcel_num){
#     rec_list[[parcel_count_ind]] = vector('list', eco_dims)
#   }
#   
#   summed_offset_projections = rec_list
#   adjusted_counters = rec_list
#   
#   dev_vec = global_params$dev_vec
#   dev_prob <- find_dev_probability(global_params$dev_vec, yr, time_horizon, parcel_num)
#   
#   if (global_params$cfac_type_in_offset_calc == 'include_clearing_offsets'){
#     offset_prob = dev_prob
#   } else {offset_prob = 0}
#   
#   counter_probs = 1 - (cumsum(dev_prob) + cumsum(offset_prob))
#   eco_ind = 1
#   
#   for (parcel_count_ind in seq_len(parcel_num)){
#     
#     current_cfac = cfacs[[parcel_count_ind]][[eco_ind]]
#     projected_dims = dim(current_cfac)
#     
#     if (cfac_type == 'include_clearing_offsets'){
#       current_summed_offset_projections = array(0, projected_dims)
#       
#       for (proj_yr in which(dev_vec > 0)){
#         
#         current_parcel_ecology = current_cfac[, , proj_yr]
#         current_offset_prob = offset_prob[proj_yr]
#         current_offset_projection = predict_parcel_traj(current_parcel_ecology, global_params, decline_rate = global_params$restoration_rate, time_horizon = (time_horizon - proj_yr))
#         current_summed_offset_projections[, , proj_yr:time_horizon] = current_summed_offset_projections[, , proj_yr:time_horizon] + current_offset_prob*current_offset_projection
#         
#       } 
#       
#     } else {
#       current_summed_offset_projections = 0
#     }
#     
#     summed_offset_projections[[parcel_count_ind]][[eco_ind]] = current_summed_offset_projections
#     
#     c_probs = rep(counter_probs, projected_dims[1]*projected_dims[2])
#     dim(c_probs) = c(length(counter_probs), c(projected_dims[2], projected_dims[1]))
#     c_probs = aperm(c_probs, c(3, 2, 1))
#     adjusted_counters[[parcel_count_ind]][[eco_ind]] = c_probs*current_cfac + current_summed_offset_projections
#     
#   }
#   
#   return(adjusted_counters)
#   
# }


#parcel_num_remaining = (current_parcel_set$parcel_num_remaining)








# find_summed_offset_projections <- function(projected_dims, current_cfac, offset_prob, global_params, time_horizon){
# 
#     current_summed_offset_projections = array(0, projected_dims)
#     
#     for (proj_yr in which(offset_prob > 0)){
#       
#       current_parcel_ecology = current_cfac[, , proj_yr]
#       current_offset_prob = offset_prob[proj_yr]
#       current_offset_projection = predict_parcel_traj(current_parcel_ecology, parcel_traj_type, global_params, current_dec_rate, time_horizon = (time_horizon - proj_yr + 1))
#       current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] = current_summed_offset_projections[, , proj_yr:(time_horizon + 1)] + current_offset_prob*current_offset_projection
#     } 
#     return(current_summed_offset_projections)
#     
# }




# write_offset <- function(offset_object, current_ecology, min_eco_val, max_eco_val, ecology_size, restoration_rate, yr){
#   current_parcels = offset_object$current_parcels
#   parcel_num = length(offset_object$current_parcels)
#   for (parcel_ind in seq_len(parcel_num)){
#     current_parcel = current_parcels[[parcel_ind]]
#     updated_parcel = sapply(current_ecology[current_parcel], project_ecology, min_eco_val = min_eco_val, max_eco_val = max_eco_val, decline_rate = restoration_rate, time_horizon = 1, time_fill = 'none')
#     current_ecology[current_parcel] = updated_parcel 
#   }
#   return(trajectories)
# }



# find_parcel_traj_from_trajectories <- function(land_parcels, parcel_ind, trajectories){
#   time_horizon = dim(trajectories)[3]
#   current_parcel = select_land_parcel(land_parcels, parcel_ind)
#   parcel_traj = array(0, time_horizon)
#   
#   for (yr in seq_len(time_horizon)){ #determine net regional offsets, net regional development_losses
#     current_slice = trajectories[ , , yr]
#     parcel_traj[yr] = sum(current_slice[current_parcel])
#   }
#   return(parcel_traj)
# }


# land_parcels = parcels$land_parcels
# parcel_indexes = 1:parcels$land_parcel_num
# time_steps = global_params$time_steps
# trajectories = outputs$trajectories[[1]]



# find_parcel_trajectories <- function(land_parcels, parcel_indexes, trajectories){
#   time_horizon = dim(trajectories)[3]
#   parcel_trajs = array(0, c(time_horizon, length(parcel_indexes)))
#   
#   for (parcel_ind in seq_len(length(parcel_indexes))){
#     current_ind = parcel_indexes[parcel_ind]
#     parcel_trajs[, parcel_ind] = find_parcel_traj_from_trajectories(land_parcels, current_ind, trajectories)
#   }
#   
#   if (length(parcel_indexes) == 1){
#     dim(parcel_trajs) = c(length(parcel_trajs), 1)
#   }
#   
#   return(parcel_trajs)
# }




# calc_counters_post_facto <- function(current_sets_object, global_params, decline_rates, parcel_num_remaining){
#   
#   parcel_set_num = length(current_sets_object)
#   adjusted_cfacs = vector('list', parcel_set_num)
#   cfacs = vector('list', parcel_set_num)
#   
#   for (parcel_set_ind in seq_len(parcel_set_num)){
#     current_parcel_set = current_sets_object[[parcel_set_ind]]
#     yr = current_parcel_set$yr
#     time_horizon = global_params$time_steps - yr
#     parcel_indexes = current_parcel_set$parcel_indexes
#     current_cfacs = build_cfacs_list(global_params, decline_rates, (parcel_ecologies_list = current_parcel_set$parcel_ecologies), parcel_indexes, time_horizon, (eco_dims = global_params$eco_dims))
#     cfacs[[parcel_set_ind]] = current_cfacs
#     if (global_params$adjust_counters_post_facto == TRUE){
#       current_adjusted_cfac = list()
#       current_adjusted_cfac$include_clearing = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing', region_params, global_params, 
#                                                             parcel_num_remaining[parcel_set_ind], decline_rates, parcel_indexes, time_horizon, yr)
#       current_adjusted_cfac$include_clearing_offsets = adjust_cfacs(current_cfacs, adjusted_cfac_type = 'include_clearing_offsets', region_params, global_params, 
#                                                                     parcel_num_remaining[parcel_set_ind], decline_rates, parcel_indexes, time_horizon, yr)
#       adjusted_cfacs[[parcel_set_ind]] = current_adjusted_cfac
#     }
#   }
#   
#   counters_object = list()
#   counters_object$cfacs = cfacs
#   counters_object$adjusted_cfacs = adjusted_cfacs
#   return(counters_object)
# }  






#   
#   
# 
# plot_parcel_sums <- function(plot_type, assess_type, assess_object, parcel_set_num){
#   
#   if (plot_type == 'offsets'){
#     plot_object = assess_object$offsets
#   } else if(plot_type == 'developments') {
#     plot_object = assess_object$developments
#   }
#   
#   
#   if (assess_type == 'set'){
#     plot_1a = plot_object$net_parcel_set[, parcel_set_num]
#     plot_1b = plot_object$net_counter_set[, parcel_set_num]
#     plot_1c = plot_object$initial_parcel_sums_set[parcel_set_num]*array(1, length(plot_1a))
#     plot_2a = plot_object$rest_gains_set[, parcel_set_num]
#     plot_2b = plot_object$avoided_degs_set[, parcel_set_num]
# 
#   } else if(assess_type == 'individual'){
#     plot_1a = plot_object$net_parcel[, parcel_set_num]
#     plot_1b = plot_object$net_counter[, parcel_set_num]
#     plot_1c = plot_object$initial_parcel_sums[parcel_set_num]*array(1, length(plot_1a))
#     plot_2a = plot_object$rest_gains[, parcel_set_num]
#     plot_2b = plot_object$avoided_degs[, parcel_set_num]
#   }
#  
#   mx = max(cbind(plot_1a, plot_1b))
#   mn = min(cbind(plot_1a, plot_1b))
#   plot(plot_1a, ylim = c(mn, mx), type = 'l', xlab="time", ylab="trajectory", col = 'red')
#   lines(plot_1b, ylim = c(mn, mx), col = 'blue')
#   lines(plot_1c, ylim = c(mn, mx), lty = 2)
#   
#   
#   
#   plot_2c = plot_2a + plot_2b
#   mx = max(cbind(plot_2a, plot_2b, plot_2c))
#   mn = min(cbind(plot_2a, plot_2b, plot_2c))
#   plot(plot_2a, ylim = c(mn, mx), type = 'l', xlab="time", ylab="split")
#   lines(plot_2b, ylim = c(mn, mx), col = 'blue')
#   lines(plot_2c, ylim = c(mn, mx), col = 'red')
#   
# }


# 
# 
# plot_parcel_sets <- function(assess_object, assess_type, parcel_set_num){
#  
#   
#   sub_plots = 1:6
#   dim(sub_plots) = c(2, 3)
#   layout(sub_plots)
#   plot_parcel_sums(plot_type = 'developments', assess_type, assess_object, parcel_set_num)
#   plot_parcel_sums(plot_type = 'offsets', assess_type, assess_object, parcel_set_num)
#   
#   plot_a = assess_object$developments$rest_gains_set[, parcel_set_num] + assess_object$developments$avoided_degs_set[, parcel_set_num]
#   plot_b = assess_object$offsets$rest_gains_set[, parcel_set_num] + assess_object$offsets$avoided_degs_set[, parcel_set_num]
#   two_plot(plot_a, plot_b, cols = c('red', 'black'))
#   lines((plot_a + plot_b), col = 'blue')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
# }
# 
# 
# 
# plot_net_parcel_sets <- function(assess_object){
#   
#   sub_plots = 1:3
#   dim(sub_plots) = c(1, 3)
#   layout(sub_plots)
#   
#   plot_a = rowSums(assess_object$developments$rest_gains_set) 
#   plot_b = rowSums(assess_object$developments$avoided_degs_set)
#   plot_c = rowSums(assess_object$offsets$rest_gains_set) 
#   plot_d = rowSums(assess_object$offsets$avoided_degs_set)
#   
#   two_plot(plot_a, plot_b, cols = c('blue', 'black'))
#   lines((plot_a + plot_b), col = 'red')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
#   two_plot(plot_c, plot_d, cols = c('blue', 'black'))
#   lines((plot_c + plot_d), col = 'red')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
# 
#   two_plot(plot_a + plot_b, plot_c + plot_d, cols = c('blue', 'black'))
#   lines((plot_a + plot_b) + (plot_c + plot_d), col = 'red')
#   grid(nx = NULL, ny = NULL, col = "darkgray", lty = "dotted")
#   
# }

