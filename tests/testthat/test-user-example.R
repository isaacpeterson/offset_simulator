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
  
  outdir <- '.'
  
  # download and user example files to the given directory
  flog.info(paste('creating user example in', outdir))
  offsetsim::osim.create.example(getwd(), futile.logger::WARN)
  
  f <- paste0(outdir, '/example_offsetsim_package_usage.R')
  x <- readLines(f)
  
  #FORCE number_of_cores<-1 as maxCores will fail on Travis (not allowed to spawn that many)
  y <- gsub( "osim.run", "user_global_params$number_of_cores<-1; osim.run", x )
  
  #FORCE overwrite of test folder
  z <- gsub( "osim.run", "user_global_params$unique_simulation_folder = FALSE; osim.run", x )
  cat(y, file=f, sep="\n")
  cat(z, file=f, sep="\n")
  
  # run it
  testrundir <- getwd()
  flog.info(paste0('running the user example in ',outdir))
  tryCatch({setwd(outdir); source('example_offsetsim_package_usage.R')}, finally = setwd(testrundir)) 
  find_current_run_folder()
  list.files(paste0(find_current_run_folder(), '/'))
  # check that we got some output
  output_folder = paste0(find_current_run_folder(outdir), '/collated_outputs/')
  actual <- list.files(output_folder, pattern = '.rds')
  flog.info(paste('checking if', actual , 'exists'))
  expect_true(length(actual) > 0)
  
  # check that we got some plot as PDF
  actual <- list.files(output_folder, pattern = '.pdf')
  flog.info(paste('checking if', actual , 'exists'))
  expect_true(length(actual) > 0)
  
  flog.info(paste('checking if outputs are within boundaries'))
  program_impacts = read.table(file = paste0(current_simulation_folder, '/expected/test_boundaries/program_impacts.csv'), sep = ',', header = FALSE)
  program_maxs = apply(program_impacts, 1, max)
  program_mins = apply(program_impacts, 1, min)
  program_mean = apply(program_impacts, 1, mean)
  
  expect_true(program_impacts > 0)
#   expected <- md5sum(paste0('../expected/test02out/collated_scenario_001_realisation_001_feature_001.rds'))
#   actual <- md5sum(paste0('../output/test02out/simulation_runs/00001/collated_outputs/collated_scenario_001_realisation_001_feature_001.rds'))
#   
})
