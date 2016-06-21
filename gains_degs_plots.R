setwd('~/Documents/R_Codes/Offsets_Working_Feb_3/')
#rm(list = ls())
WD = getwd()
strt<-Sys.time()

source('initialise_common_params.R')

source('run_system_routines.R')
source('collate_routines.R')
source('plot_routines.R')

filename = '~/Documents/include_clearing_offsets_all_used.pdf'

adjusted_cfac_type = 'include_clearing_offsets'
write_pdf = TRUE

plot_net_potential = TRUE
plot_current_condition_line = FALSE
plot_summed_offset_projections = FALSE
plot_offset_projections = FALSE
plot_restoration_curve = FALSE
plot_dev_projections = TRUE
plot_cfac = TRUE
plot_decline_including_clearing = TRUE

current_condition_plot_params = list(2, 2, 'black', 'current_condition', plot_current_condition_line)     #c(lty, lwd, line_color, legend, plot)
restoration_plot_params = list(2, 2, 'darkgreen', 'restoration curve', plot_restoration_curve)
net_plot_params = list(1, 4, 'darkgreen', 'net value', plot_net_potential)
summed_offset_projections_plot_params = list(2, 2, 'darkgreen', 'summed offset projections', plot_offset_projections)
cfac_plot_params = list(2, 3, 'black', 'parcel decline', plot_cfac)
offset_proj_plot_params = list(2, 1, 'darkgreen', 'offset projections', plot_offset_projections)
dev_proj_plot_params = list(2, 1, 'red', 'development projections', plot_dev_projections)
weighted_counters_plot_params = list(2, 2, 'red', 'decline including clearing', plot_decline_including_clearing)

global_params = list()
global_params$time_steps = 50
global_params$offset_yr = 10
global_params$initial_parcel_value = 80
global_params$min_eco_val = 0
global_params$max_eco_val = 100
global_params$decline_rate = -0.05
global_params$restoration_rate = -0.05
global_params$parcel_num_remaining = 200
global_params$dev_num = 100
global_params$eco_dims = 1
global_params$parcel_indexes = 1
global_params$adjusted_cfac_type = adjusted_cfac_type
global_params$offset_action_type = 'restore'
time_horizons = (global_params$time_steps - global_params$offset_yr)

legend_buffer  = 30


cfac <- project_ecology(global_params$initial_parcel_value, global_params$min_eco_val, global_params$max_eco_val, global_params$decline_rate, time_horizon = (global_params$time_steps - 1), time_fill = TRUE)
restoration_curve <- project_ecology(cfac[global_params$offset_yr], global_params$min_eco_val, global_params$max_eco_val, global_params$restoration_rate, 
                                     time_horizon = (global_params$time_steps - global_params$offset_yr), time_fill = TRUE)

current_condition_line <- rep(cfac[global_params$offset_yr], (global_params$time_steps - global_params$offset_yr + 1) )

dev_vec <- find_prog_vector(global_params$time_steps, (prog_start = global_params$offset_yr + 1), prog_end = global_params$time_steps, 
                            total_prog_num = global_params$dev_num, sd = 1)
dev_vec[global_params$offset_yr] = 1
projected_dev_vec = dev_vec[min(which(dev_vec > 0)):global_params$time_steps]

current_cfac = cfac[(global_params$offset_yr):(global_params$time_steps)]
dim(current_cfac) = c(1, 1, length(current_cfac))
current_cfacs <- list(list(current_cfac))

current_parcel_ecologies <- cfac[[global_params$offset_yr]]

current_parcel_ecologies = list(current_parcel_ecologies)

if (adjusted_cfac_type == 'include_clearing'){
  cfacs_include_clearing = adjust_cfacs(current_cfacs, current_parcel_ecologies, adjusted_cfac_type = 'include_clearing', global_params, dev_vec, 
                                        parcel_num_remaining = global_params$parcel_num_remaining, decline_rates = -0.05, global_params$parcel_indexes, 
                                        time_horizons, offset_yrs = global_params$offset_yr)
  cfacs_include_clearing <- cfacs_include_clearing[[1]][[1]]
  net_weighted_potential = cfac
  net_weighted_potential[global_params$offset_yr:global_params$time_steps] = cfacs_include_clearing
  
} else if (adjusted_cfac_type == 'include_clearing_offsets'){
  weighted_counters_object <- find_weighted_counters(current_cfacs, adjusted_cfac_type ='include_clearing_offsets', eco_dims = global_params$eco_dims, dev_vec, 
                                                     parcel_num_remaining = global_params$parcel_num_remaining, parcel_num = length(global_params$parcel_indexes), 
                                                     time_horizons, offset_yrs = global_params$offset_yr)
  
  offset_projections <- calc_offset_projections(current_cfacs, weighted_counters_object$offset_probs, global_params, decline_rates, global_params$parcel_indexes, time_horizons)
  summed_offset_projections <- sum_offset_projs(offset_projections, weighted_counters_object$offset_probs, global_params$eco_dims, parcel_num = length(global_params$parcel_indexes), time_horizons)
  cfacs_include_clearing_offsets = sum_clearing_offsets(weighted_counters_object$weighted_counters, summed_offset_projections, global_params$eco_dims)
  offset_projections <- offset_projections[[1]][[1]]
  cfacs_include_clearing <- weighted_counters_object$weighted_counters[[1]][[1]]
  offset_probs = unlist(weighted_counters_object$offset_probs)
  offset_inds = which(offset_probs > 0)
  net_weighted_potential = cfac
  net_weighted_potential[global_params$offset_yr:global_params$time_steps] = cfacs_include_clearing_offsets[[1]][[1]]
  
}

if (write_pdf == TRUE){
  pdf(filename, width = 3.5, height = 4)
}


ylims = c(0, global_params$max_eco_val + legend_buffer)

plot(1:global_params$time_steps, cfac, type = 'l', ylim = ylims, lty = cfac_plot_params[[1]], lwd = cfac_plot_params[[2]], col = cfac_plot_params[[3]], xlab = '', ylab = '')
abline(h = 0)

if (plot_net_potential == TRUE){
  lines(net_weighted_potential, type = 'l', ylim = ylims, lty = net_plot_params[[1]], lwd = net_plot_params[[2]], col = net_plot_params[[3]], xlab = '', ylab = '')
}

if (plot_offset_projections == TRUE){
  for (offset_proj_ind in seq(projected_dev_vec)) {
    current_plot = offset_projections[[offset_proj_ind]]
    if (offset_probs[offset_proj_ind] > 0){
      
      time_line = offset_proj_ind:global_params$time_steps
      lines((time_line + global_params$offset_yr - 1), current_plot[time_line], type = 'l', ylim = ylims, lty = offset_proj_plot_params[[1]],
            lwd = offset_proj_plot_params[[2]], col = offset_proj_plot_params[[3]], xlab = '', ylab = '')
      
    }
  }
  
}


if (plot_dev_projections == TRUE){
  for (offset_proj_ind in seq(projected_dev_vec)) {
    if (projected_dev_vec[offset_proj_ind] > 0){
      time_line = offset_proj_ind:global_params$time_steps
      dev_x = time_line[1] + global_params$offset_yr - 1
      lines(x = c(dev_x, dev_x), y = c(0, cfac[dev_x]), lty = dev_proj_plot_params[[1]], 
            lwd = dev_proj_plot_params[[2]], col = dev_proj_plot_params[[3]], xlab = '', ylab = '')
    }
  }
  
}

if ((plot_decline_including_clearing == TRUE)){
  lines(global_params$offset_yr:global_params$time_steps, cfacs_include_clearing, lty = weighted_counters_plot_params[[1]],
        lwd = weighted_counters_plot_params[[2]],  col = weighted_counters_plot_params[[3]], xlab = '', ylab = '')
}


if (plot_current_condition_line == TRUE){
  lines(global_params$offset_yr:global_params$time_steps, current_condition_line, lty = current_condition_plot_params[[1]],
        lwd = current_condition_plot_params[[2]],  col = current_condition_plot_params[[3]], xlab = '', ylab = '')
}

if (plot_restoration_curve == TRUE){
  lines(global_params$offset_yr:global_params$time_steps, restoration_curve, lty = restoration_plot_params[[1]],
        lwd = restoration_plot_params[[2]],  col = restoration_plot_params[[3]], xlab = '', ylab = '')
}




if (plot_summed_offset_projections == TRUE){
  lines(global_params$offset_yr:global_params$time_steps, summed_offset_projections[[1]][[1]], lty = summed_offset_projections_plot_params[[1]], 
        lwd = summed_offset_projections_plot_params[[2]], col = summed_offset_projections_plot_params[[3]], xlab = '', ylab = '')
}

lines(net_potential_restoration, type = 'l', ylim = ylims, lty = 2, lwd = net_plot_params[[2]], col = net_plot_params[[3]], xlab = '', ylab = '')

if (write_pdf == TRUE){
  dev.off()
  graphics.off()
}

