initialise_plot_params <- function(){
  plot_params = list()
  plot_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  plot_params$output_type = 'features' # set to plot through 'features', 'scenarios' or 'site_sets'
  plot_params$realisation_num = 'all' # 'all' or number to plot
  plot_params$write_pdf = FALSE
  plot_params$sets_to_plot = 8 # example site to plot
  plot_params$plot_vec = 1 #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  plot_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  plot_params$program_col_vec = c('darkgreen', 'red', 'black') 
  plot_params$cfac_col = 'blue' 
  plot_params$landscape_col = 'black'
  plot_params$lwd_vec = c(3, 0.5)
  #plot_params$plot_subset_type = c('offset_calc_type', 'dev_calc_type', 'offset_time_horizon') # 'offset_calc', 'time_horizon'
  #plot_params$plot_subset_param = c('net_gains', 'future_condition', 15)
    
  plot_params$plot_site_offset = TRUE 
  plot_params$plot_site_dev = TRUE
  plot_params$plot_site_net = TRUE
  plot_params$plot_site = TRUE
  plot_params$plot_program = TRUE
  plot_params$plot_landscape = TRUE
  
  plot_params$site_impact_lwd = 0.5
  plot_params$site_outcome_lwd_vec = c(0.5)
  plot_params$program_lwd_vec = c(3, 0.5)
  plot_params$program_outcome_lwd_vec = c(3, 0.5)
  plot_params$landscape_lwd_vec  = c(3)
  plot_params$landscape_outcome_lwd_vec = c(3)

  plot_params$string_width = 3 # how many digits are used to store scenario index and realisation index
  plot_params$nx = 3 
  plot_params$ny = 4

  plot_params$site_outcome_plot_lims_set = rep(list(c(0, 3e4)), length(plot_params$plot_vec))
  plot_params$program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), length(plot_params$plot_vec))
  plot_params$landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), length(plot_params$plot_vec))
  
  plot_params$site_impact_plot_lims_set = rep(list(c(-1e4, 1e4)), length(plot_params$plot_vec))
  plot_params$program_impact_plot_lims_set = rep(list(c(-1e6, 1e6)), length(plot_params$plot_vec)) 
  plot_params$landscape_impact_plot_lims_set = rep(list(c(-1e6, 0)), length(plot_params$plot_vec))
  

  return(plot_params)
}