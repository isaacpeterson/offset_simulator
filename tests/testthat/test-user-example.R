context("system tests")

library(tools)
library(futile.logger)

# dsingh, 21 dec 2017: Here we do not compare against an expected RDS output because 
# in the default params the random seed is not set. Instead we do a loose
# check that we get "some" RDS output and "some" plot, which is an indication that 
# at least something ran and generated a plot

test_that("running user-example", {
  
  loglevel <- futile.logger::INFO
  flog.threshold(loglevel)
  
  flog.info('___________ TEST USER EXAMPLE __________ ')
  
  Sys.setenv("R_TESTS" = "") # needed for R CMD CHECK to run correctly
  
  outdir <- '../output/user-example'

  # download and user example files to the given directory
  flog.info(paste('creating user example in', outdir))
  offsetsim::osim.create.example(outdir, futile.logger::WARN)
  
  # FORCE number_of_cores<-2 as maxCores will fail on Travis (not allowed to spawn that many)
  f <- paste0(outdir, '/offsetsim_example.R')
  x <- readLines(f)
  y <- gsub( "osim.run", "user_global_params$number_of_cores<-2; osim.run", x )
  cat(y, file=f, sep="\n")
  
  # run it
  testrundir <- getwd()
  flog.info(paste0('running the user example in ',outdir))
  tryCatch({setwd(outdir); source('offsetsim_example.R')}, finally = setwd(testrundir)) 

  # check that we got some output
  actual <- '../output/user-example/simulated_data/simulation_runs/00001/collated_outputs/collated_scenario_001_realisation_001_feature_001.rds'
  flog.info(paste('checking if', actual , 'exists'))
  expect_true(file.exists(actual))

  # check that we got some plot as PDF
  actual <- '../output/user-example/Rplots.pdf'
  flog.info(paste('checking if', actual , 'exists'))
  expect_true(file.exists(actual))
  
})
