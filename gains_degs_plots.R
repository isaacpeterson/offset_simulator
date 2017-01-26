setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
rm(list = ls())
WD = getwd()
strt<-Sys.time()

source('initialise_common_params.R')
source('run_system_routines_modularised.R')
source('collate_routines.R')
source('plot_routines.R')



plot_traj <- function(plot_type, time_steps, impact_time, traj_list, ylim_in_use, plot_params){
  plot_num = length(traj_list)
  for (plot_ind in seq(plot_num)){
    current_traj = traj_list[[plot_ind]]
    
    if ((plot_ind == 1) && (plot_type == 'non-overlay')){
      plot(impact_time[[plot_ind]]:impact_time[[plot_ind + 1]], current_traj, type = 'l', ylim = ylim_in_use, yaxt = 'n', xaxt = 'n', xlim = c(0, time_steps), lty = plot_params[[1]], lwd = plot_params[[2]], col = plot_params[[3]], xlab = '', ylab = '')
    } else{
      lines(impact_time[[plot_ind]]:impact_time[[plot_ind + 1]], current_traj, type = 'l', ylim = ylim_in_use, yaxt = 'n', xaxt = 'n', xlim = c(0, time_steps), lty = plot_params[[1]], lwd = plot_params[[2]], col = plot_params[[3]], xlab = '', ylab = '')
    }
    
    if (plot_ind < plot_num){
      segments(x0 = impact_time[[plot_ind + 1]], y0 = current_traj[length(current_traj)], x1 = impact_time[[plot_ind + 1]], y1 = traj_list[[plot_ind+1]][1], lty = plot_params[[1]], lwd = plot_params[[2]], col = plot_params[[3]])
    }
  }
  abline(h = 0)
  
}


plot_outcomes <- function(){
  plot(1:global_params$time_steps, cfac, type = 'l', ylim = ylims, yaxt = 'n', xaxt = 'n', lty = cfac_plot_params[[1]], lwd = cfac_plot_params[[2]], col = cfac_plot_params[[3]], xlab = '', ylab = '')
  abline(h = 0)

  if (plot_net_potential == TRUE){
    lines(net_weighted_potential, type = 'l', ylim = ylims, lty = net_plot_params[[1]], lwd = net_plot_params[[2]], col = net_plot_params[[3]], xlab = '', ylab = '')
  }
  
  if (plot_offset_projections == TRUE){
    for (offset_proj_ind in seq(projected_dev_vec)) {
      current_plot = offset_projections[[offset_proj_ind]]
      if (offset_probs[offset_proj_ind] > 0){
        
        time_line = offset_proj_ind:global_params$time_steps
        lines((time_line + parcel_params$offset_yr - 1), current_plot[time_line], type = 'l', ylim = ylims, lty = offset_proj_plot_params[[1]],
              lwd = offset_proj_plot_params[[2]], col = offset_proj_plot_params[[3]], xlab = '', ylab = '')
        
      }
    }
    
  }
  
  
  if (plot_dev_projections == TRUE){
    for (offset_proj_ind in seq(projected_dev_vec)) {
      if (projected_dev_vec[offset_proj_ind] > 0){
        time_line = offset_proj_ind:global_params$time_steps
        dev_x = time_line[1] + parcel_params$offset_yr - 1
        lines(x = c(dev_x, dev_x), y = c(0, cfac[dev_x]), lty = dev_proj_plot_params[[1]], 
              lwd = dev_proj_plot_params[[2]], col = dev_proj_plot_params[[3]], xlab = '', ylab = '')
      }
    }
    
  }
  
  if ((plot_decline_including_clearing == TRUE)){
    lines(parcel_params$offset_yr:global_params$time_steps, cfacs_include_clearing, lty = weighted_counters_plot_params[[1]],
          lwd = weighted_counters_plot_params[[2]],  col = weighted_counters_plot_params[[3]], xlab = '', ylab = '')
  }
  
  if (plot_current_condition_line == TRUE){
    lines(parcel_params$offset_yr:global_params$time_steps, current_condition_line, lty = current_condition_plot_params[[1]],
          lwd = current_condition_plot_params[[2]],  col = current_condition_plot_params[[3]], xlab = '', ylab = '')
  }
  
  if (plot_outcome == TRUE){
    plot_traj(plot_type = 'overlay', global_params$time_steps, impact_time = list(1, parcel_params$offset_yr, global_params$time_steps), outcome, ylims, restoration_plot_params)
  }
  
  if (plot_summed_offset_projections == TRUE){
    lines(parcel_params$offset_yr:global_params$time_steps, summed_offset_projections[[1]][[1]] + cfac[parcel_params$offset_yr:global_params$time_steps], lty = summed_offset_projections_plot_params[[1]], 
          lwd = summed_offset_projections_plot_params[[2]], col = summed_offset_projections_plot_params[[3]], xlab = '', ylab = '')
  }
}

plot_impact <- function(){
  impact = list(array(0, parcel_params$offset_yr), (outcome[[2]] - (summed_offset_projections[[1]][[1]] + cfac[parcel_params$offset_yr:global_params$time_steps])))
  plot_traj(plot_type = 'non-overlay', global_params$time_steps, impact_time = list(1, parcel_params$offset_yr, global_params$time_steps), impact, ylims, cfac_plot_params)
  lines(parcel_params$offset_yr:global_params$time_steps, outcome[[2]] - (net_weighted_potential[parcel_params$offset_yr:global_params$time_steps]), lty = summed_offset_projections_plot_params[[1]], 
        lwd = summed_offset_projections_plot_params[[2]], col = summed_offset_projections_plot_params[[3]], xlab = '', ylab = '')
  lines(parcel_params$offset_yr:global_params$time_steps, outcome[[2]] - (cfac[parcel_params$offset_yr:global_params$time_steps]), lty = summed_offset_projections_plot_params[[1]], 
        lwd = summed_offset_projections_plot_params[[2]], col = summed_offset_projections_plot_params[[3]], xlab = '', ylab = '')
  lines(parcel_params$offset_yr:global_params$time_steps, outcome[[2]] - (cfacs_include_clearing), lty = summed_offset_projections_plot_params[[1]], 
        lwd = summed_offset_projections_plot_params[[2]], col = summed_offset_projections_plot_params[[3]], xlab = '', ylab = '')
}


build_projections <- function(parcel_params, global_params){
  cfac <- project_ecology(parcel_params$initial_parcel_value, global_params$min_eco_val, global_params$max_eco_val, parcel_params$decline_rate, time_horizon = (global_params$time_steps - 1), time_fill = TRUE)
  
  outcome_1 <- cfac[seq(parcel_params$offset_yr)]
  
  if (impact_type == 'offset'){
    outcome_2 <- project_ecology(cfac[parcel_params$offset_yr], global_params$min_eco_val, global_params$max_eco_val, parcel_params$restoration_rate, 
                                 time_horizon = (global_params$time_steps - parcel_params$offset_yr), time_fill = TRUE)
  } else {
    outcome_2 = array(0, global_params$time_steps - parcel_params$offset_yr + 1)
    dim(outcome_2) = c(length(outcome_2), 1)
    
  }
  
  outcome = list(outcome_1, outcome_2)
  current_condition_line <- rep(cfac[parcel_params$offset_yr], (global_params$time_steps - parcel_params$offset_yr + 1) )
  
  dev_vec[parcel_params$offset_yr] = 1
  projected_dev_vec = dev_vec[min(which(dev_vec > 0)):global_params$time_steps]
  
  current_cfac = cfac[(parcel_params$offset_yr):(global_params$time_steps)]
  dim(current_cfac) = c(length(current_cfac), 1)
  current_cfacs <- rep(list(list(current_cfac)), parcel_params$dev_num)
  
  current_parcel_ecologies <- cfac[[parcel_params$offset_yr]]
  
  current_parcel_ecologies = list(current_parcel_ecologies)
  
  if (adjusted_cfac_type == 'include_clearing'){
    
    adjust_cfacs <- function(current_cfacs, include_potential_developments =TRUE, include_potential_offsets, include_illegal_clearing, 
                             current_program_params, parcel_num_remaining, decline_rates, time_horizons, offset_yrs)
      
      cfacs_include_clearing <- cfacs_include_clearing[[1]][[1]]
      net_weighted_potential = cfac
      net_weighted_potential[parcel_params$offset_yr:global_params$time_steps] = cfacs_include_clearing
      
  } else if (adjusted_cfac_type == 'include_clearing_offsets'){
    
    weighted_counters_object <- find_weighted_counters(current_cfacs, 
                                                       include_illegal_clearing = FALSE, 
                                                       include_potential_developments = FALSE, 
                                                       include_potential_offsets = TRUE,  
                                                       intervention_vec = dev_vec, 
                                                       illegal_clearing_prob = 0,
                                                       offset_intervention_scale = 1,
                                                       eco_dims = global_params$eco_dims, 
                                                       parcel_num_remaining,
                                                       parcel_num = parcel_params$dev_num, 
                                                       time_horizons, 
                                                       offset_yrs,
                                                       time_steps = global_params$time_steps)
    
    offset_projections <- calc_offset_projections(current_cfacs, 
                                                  weighted_counters_object$offset_intervention_probs, 
                                                  parcel_params$restoration_rate, 
                                                  action_type = 'restore', 
                                                  decline_rates, 
                                                  time_horizons, 
                                                  global_params$eco_dims, 
                                                  global_params$min_eco_val, 
                                                  global_params$max_eco_val)
    
    summed_offset_projections <- sum_offset_projs(offset_projections,
                                                  offset_probs = weighted_counters_object$offset_intervention_probs, 
                                                  global_params$eco_dims, 
                                                  time_horizons)
    
    cfacs_include_clearing_offsets = sum_clearing_offsets(weighted_counters_object$weighted_counters, summed_offset_projections, global_params$eco_dims)
    
    offset_projections <- offset_projections[[1]][[1]]
    cfacs_include_clearing <- weighted_counters_object$weighted_counters[[1]][[1]]
    offset_probs = unlist(weighted_counters_object$offset_intervention_probs)
    
    net_weighted_potential = cfac
    net_weighted_potential[parcel_params$offset_yr:global_params$time_steps] = cfacs_include_clearing_offsets[[1]][[1]]
    
  }
  
  projections_object = list()
  projections_object$offset_projections = offset_projections
  projections_obejct$summed_offset_projections = summed_offset_projections
  projections_object$cfacs_include_clearing = cfacs_include_clearing
  projections_object$offset_probs = offset_probs
  projections_object$net_weighted_potential = net_weighted_potential
  return(projections_object)
  
}







filename = '~/Documents/include_clearing_offsets_all_used.pdf'

adjusted_cfac_type = 'include_clearing_offsets'
write_pdf = FALSE

setup_sub_plots(nx = 2, ny = 1, x_tit = '')
impact_type = 'offset'
plot_impact = TRUE
plot_net_potential = TRUE
plot_current_condition_line = FALSE
plot_summed_offset_projections = TRUE
plot_offset_projections = TRUE
plot_outcome = TRUE
plot_dev_projections = TRUE
plot_cfac = TRUE
plot_decline_including_clearing = TRUE

current_condition_plot_params = list(2, 2, 'black', 'current_condition', plot_current_condition_line)     #c(lty, lwd, line_color, legend, plot)
restoration_plot_params = list(1, 3, 'darkgreen', 'restoration curve', plot_outcome)
net_plot_params = list(2, 3, 'black', 'net value', plot_net_potential)
summed_offset_projections_plot_params = list(2, 2, 'darkgreen', 'summed offset projections', plot_offset_projections)
cfac_plot_params = list(2, 3, 'black', 'parcel decline', plot_cfac)
offset_proj_plot_params = list(2, 1, 'darkgreen', 'offset projections', plot_offset_projections)
dev_proj_plot_params = list(2, 1, 'red', 'development projections', plot_dev_projections)
weighted_counters_plot_params = list(2, 2, 'red', 'decline including clearing', plot_decline_including_clearing)

global_params = list()
global_params$time_steps = 50
global_params$eco_dims = 1
global_params$min_eco_val = 0
global_params$max_eco_val = 100


parcel_params = list()
parcel_params$decline_rate = -0.05
parcel_params$restoration_rate = 0.05
parcel_params$offset_yr = 10
parcel_params$initial_parcel_value = 80
parcel_params$adjusted_cfac_type = adjusted_cfac_type
parcel_params$offset_action_type = 'restore'
parcel_params$dev_num = 50
parcel_params$parcel_num_remaining = rep(200, parcel_params$dev_num)
parcel_params$time_horizons = rep( (global_params$time_steps - parcel_params$offset_yr), parcel_params$dev_num)
parcel_params$offset_yrs = rep(list(parcel_params$offset_yr - 1), parcel_params$dev_num)
parcel_params$decline_rates = rep(list(list(parcel_params$decline_rate)), parcel_params$dev_num)

legend_buffer  = 30

dev_vec = generate_intervention_vec(time_steps = global_params$time_steps, prog_start = (parcel_params$offset_yr + 1), 
                                    prog_end = global_params$time_steps, total_prog_num = parcel_params$dev_num, sd = 0)



if (write_pdf == TRUE){
  pdf(filename, width = 3.5, height = 4)
}


ylims = c(-global_params$max_eco_val, global_params$max_eco_val + legend_buffer)



if (write_pdf == TRUE){
  dev.off()
  graphics.off()
}

