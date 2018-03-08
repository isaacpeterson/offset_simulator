context("system tests")

library(tools)
library(futile.logger)

# dsingh, 21 dec 2017: Here we do not compare against an expected RDS output because 
# in the default params the random seed is not set. Instead we do a loose
# check that we get "some" RDS output and "some" plot, which is an indication that 
# at least something ran and generated a plot

test_that("running user-example", {
  
  loglevel <- futile.logger::TRACE
  flog.threshold(loglevel)
  
  flog.info('___________ TEST USER EXAMPLE __________ ')
  
  Sys.setenv("R_TESTS" = "") # needed for R CMD CHECK to run correctly
  
  run_file_dir <- getwd()
  
  # download and user example files to the given directory
  flog.info(paste('creating user example in', run_file_dir))
  offsetsim::osim.create.example(run_file_dir, futile.logger::WARN)
  
  f <- paste0(run_file_dir, '/example_offsetsim_package_usage.R')
  
  outdir = paste0(dirname(run_file_dir), '/output/boundary_test/')
  if (!file.exists(outdir)){
    dir.create(outdir, recursive = TRUE)
  }
  
  #FORCE number_of_cores<-1 as maxCores will fail on Travis (not allowed to spawn that many)
  x <- readLines(f)
  list_element_to_force <- gsub( "osim.run", paste0("user_global_params$number_of_cores = 'all'; \n osim_run"), x )
  cat(list_element_to_force, file=f, sep="\n")
  
  x <- readLines(f)
  list_element_to_force <- gsub( "osim.run", "user_output_params$output_csv_file = TRUE; \n osim.run", x )
  cat(list_element_to_force, file=f, sep="\n")
  
  x <- readLines(f)
  list_element_to_force <- gsub( "osim.run", paste0("user_global_params$simulation_folder = '", outdir, "'; \n osim.run"), x )
  cat(list_element_to_force, file=f, sep="\n")
  
  x <- readLines(f)
  list_element_to_force <- gsub( "osim.run", paste0("user_global_params$realisation_num = 4; \n osim.run"), x )
  cat(list_element_to_force, file=f, sep="\n")
  
  flog.info(paste0('running the user example in ', run_file_dir))
  tryCatch({setwd(run_file_dir); source('example_offsetsim_package_usage.R')}) 
  
  output_folder = paste0(find_current_run_folder(outdir), '/collated_outputs/')
  actual <- list.files(output_folder, pattern = '.rds')
  flog.info(paste('checking if', actual , 'exists'))
  expect_true(length(actual) > 0)
  
  # check that we got plot as PDF
  actual <- list.files(output_folder, pattern = '.pdf')
  flog.info(paste('checking if pdf output ', actual , 'exists'))
  expect_true(length(actual) > 0)
  
  expected_folder = paste0(dirname(run_file_dir), '/expected/test_boundaries/')
  expected_impact_files = list.files(expected_folder, pattern = 'program_impacts.csv')
  
  # check that we got impacts as csv
  actual_impact_files <- list.files(output_folder, pattern = 'program_impacts.csv')
  flog.info(paste('checking if impacts are output as csv files'))
  expect_true(all(actual_impact_files == expected_impact_files))
  
  flog.info(paste('checking if outputs are within boundaries'))
  
  expected_impacts = read.table(file = paste0(expected_folder, expected_impact_files), sep = ',', header = FALSE)
  actual_impacts = read.table(file = paste0(output_folder, actual_impact_files), sep = ',', header = FALSE)
  
  ks_tests = lapply(seq(user_simulation_params$time_steps), 
                    function(x) ks.test(as.numeric(expected_impacts[x, ]), as.numeric(actual_impacts[x, ])))
  p_vals = lapply(seq_along(ks_tests), function(i) ks_tests[[i]]$p.value)
  expect_true(all(unlist(p_vals) > 0.01))
  
  expected_maxs = apply(expected_impacts, 1, max)
  expected_mins = apply(expected_impacts, 1, min)
  expected_mean = apply(expected_impacts, 1, mean)
  
  pdf(paste0(run_file_dir, '/test_impacts.pdf' ))
  plot(expected_maxs, type = 'l', ylim = c(min(expected_mins), max(expected_maxs)))
  lines(expected_mins, lwd = 2, col = 'red')
  lines(expected_maxs, lwd = 2, col = 'red')
  lines(expected_mean, lwd = 2, col = 'red')
  
  for (plot_ind in seq(user_global_params$realisation_num)){
    lines(as.numeric(actual_impacts[, plot_ind ]))
  }
  dev.off()
  unlink(paste0(dirname(run_file_dir), '/output/boundary_test/'), recursive = TRUE)
  #   expect_true(program_impacts > 0)
  #   expected <- md5sum(paste0('../expected/test02out/collated_scenario_001_realisation_001_feature_001.rds'))
  #   actual <- md5sum(paste0('../output/test02out/simulation_runs/00001/collated_outputs/collated_scenario_001_realisation_001_feature_001.rds'))
  #   
})
