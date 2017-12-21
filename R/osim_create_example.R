#' Creates an offsetsim usage example file 'offsetsim-example.R' in the given directory
#' @param outdir the output directory to create the user example file in (default is '.')
#' @param loglevel logging level to use, for instance futile.logger::INFO
#' @import futile.logger
#' @export
osim.create.example <- function(outdir = '.', loglevel = INFO) {
  
  urlRunParams <- 'https://raw.githubusercontent.com/isaacpeterson/offset_simulator/master/R/default_initialise_params.R'
  urlPlotParams <- 'https://raw.githubusercontent.com/isaacpeterson/offset_simulator/master/R/default_plot_params.R'
  urlRunScript <- 'https://raw.githubusercontent.com/isaacpeterson/offset_simulator/master/user_examples/simulated/run_offset_simulation_source_code.R'

  flog.threshold(loglevel)
  flog.info(paste0("Ensuring output directory '", outdir, "' exists"))
  outfolder <- write_nested_folder(outdir)
  
  runFile <- 'offsetsim_example.R'
  outRunScript <- paste0(outfolder,'/', runFile)
  outRunParams <- paste0(outfolder,'/default_initialise_params.R')
  outPlotParams <- paste0(outfolder,'/default_plot_params.R')
  
  
  flog.info(paste('Writing to ', outfolder)) 
  quiet <- FALSE
  if (loglevel == futile.logger::ERROR || loglevel == futile.logger::WARN || loglevel == futile.logger::INFO) {
    quiet <- TRUE
  }
  msg <- paste0("Could not download user example fiels to ", outfolder, ".\n",
                "Please ensure you have an active internet connection.\n")
  
  flog.info(paste('Writing ', urlRunScript, 'to', outRunScript)) 
  tryCatch(download.file(urlRunScript,outRunScript, quiet = quiet),
           warning = function(err) cat(msg),
           error = function(err) cat(msg)
  )
  
  flog.info(paste('Writing ', urlRunParams, 'to', outRunParams)) 
  tryCatch(download.file(urlRunParams,outRunParams, quiet = quiet),
           warning = function(err) cat(msg),
           error = function(err) cat(msg)
  )
  
  flog.info(paste('Writing ', urlPlotParams, 'to', outPlotParams)) 
  tryCatch(download.file(urlPlotParams,outPlotParams, quiet = quiet),
           warning = function(err) cat(msg),
           error = function(err) cat(msg)
  )
  
  cat('User example downloaded to folder "', outfolder, 
      '";". To run the example do:\nsetwd("',outfolder,'"); source("',runFile,'")\n'
      , sep = "")
}


