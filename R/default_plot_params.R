initialise_plot_params <- function(base_folder){
  plot_params = list()
  plot_params$output_plot_folder = vector()
  plot_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  plot_params$output_type = 'site_sets' # set to plot through 'features', 'scenarios' or 'site_sets'
  plot_params$realisation_num = 'all' # 'all' or number to plot
  plot_params$write_pdf = FALSE
  plot_params$sets_to_plot = 1 # example site to plot
  plot_params$plot_vec = 1:50 #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  plot_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  plot_params$program_col_vec = c('darkgreen', 'red', 'black') 
  plot_params$cfac_col = 'blue' 
  plot_params$landscape_col = 'black'
  plot_params$lwd_vec = c(3, 0.5)
  plot_params$plot_subset_type = 'all'
  plot_params$plot_subset_param = 'all'
    
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
  
  plot_params$site_impact_plot_lims_set = rep(list(c(-1e3, 1e3)), length(plot_params$plot_vec))
  plot_params$program_impact_plot_lims_set = rep(list(c(-1e6, 1e6)), length(plot_params$plot_vec)) 
  plot_params$landscape_impact_plot_lims_set = rep(list(c(-1e6, 0)), length(plot_params$plot_vec))
  


  return(plot_params)
}