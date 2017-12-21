initialise_default_plot_params <- function(base_folder){
  default_plot_params = list()
  default_plot_params$output_plot_folder = vector()
  default_plot_params$plot_type = 'impacts' # can be 'outcomes'  or 'impacts',
  default_plot_params$output_type = 'scenarios' # set to plot through 'features', 'scenarios' or 'site_sets'
  default_plot_params$realisation_num = 'all' # 'all' or number to plot
  default_plot_params$write_pdf = FALSE
  default_plot_params$sets_to_plot = 1 # example site to plot
  default_plot_params$plot_vec = 1 #c(1,4,7,10, 8, 2,3,5,6,9,11,12 ) #1:12
  default_plot_params$site_impact_col_vec = c('darkgreen', 'red', 'black')
  default_plot_params$program_col_vec = c('darkgreen', 'red', 'black') 
  default_plot_params$cfac_col = 'blue' 
  default_plot_params$landscape_col = 'black'
  default_plot_params$lwd_vec = c(3, 0.5)
  default_plot_params$plot_subset_type = 'all'
  default_plot_params$plot_subset_param = 'all'
    
  default_plot_params$plot_site_offset = TRUE 
  default_plot_params$plot_site_dev = TRUE
  default_plot_params$plot_site_net = TRUE
  default_plot_params$plot_site = TRUE
  default_plot_params$plot_program = TRUE
  default_plot_params$plot_landscape = TRUE
  
  default_plot_params$site_impact_lwd = 0.5
  default_plot_params$site_outcome_lwd_vec = c(0.5)
  default_plot_params$program_lwd_vec = c(3, 0.5)
  default_plot_params$program_outcome_lwd_vec = c(3, 0.5)
  default_plot_params$landscape_lwd_vec  = c(3)
  default_plot_params$landscape_outcome_lwd_vec = c(3)

  default_plot_params$string_width = 3 # how many digits are used to store scenario index and realisation index
  default_plot_params$nx = 3 
  default_plot_params$ny = 4

  default_plot_params$site_outcome_plot_lims_set = rep(list(c(0, 3e4)), length(default_plot_params$plot_vec))
  default_plot_params$program_outcome_plot_lims_set = rep(list(c(0e6, 1e7)), length(default_plot_params$plot_vec))
  default_plot_params$landscape_outcome_plot_lims_set = rep(list(c(0, 2e7)), length(default_plot_params$plot_vec))
  
  default_plot_params$site_impact_plot_lims_set = rep(list(c(-1e3, 1e3)), length(default_plot_params$plot_vec))
  default_plot_params$program_impact_plot_lims_set = rep(list(c(-1e6, 1e6)), length(default_plot_params$plot_vec)) 
  default_plot_params$landscape_impact_plot_lims_set = rep(list(c(-1e6, 0)), length(default_plot_params$plot_vec))
  


  return(default_plot_params)
}