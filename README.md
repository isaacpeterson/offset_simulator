Offset simulator
================

Software for simulating the impacts of biodiversity offsetting 


Instructions to run the software
---------------------------------

Download and install the latest version of R. If you are running from a linux-type system this can be done using:

`sudo apt-get update`

`sudo apt-get install r-base`

`sudo apt-get install git`


Install the R packages foreach, doParallel, abind and pixmap. To do this from within R you can run:

`install.packages(c("foreach", "doParallel", "abind", "pixmap"))`


Description on the files
------------------------

### initialise_params.R 

* set policy parameters and ecology parameters e.g. offset gains structure, ecology size and dimensions etc


### run_offsets_simulation.R 

* inputs policy parameters from initialise_params.R
* runs simulations from simulation_routines.R
* collates simulation outputs with collate_routines.R and plot_routines.R


### plot_collated_realisations.R

* Plot the results of the simulation
* run with action_type set to 'plot_outcomes' or 'plot_impacts' to plot either of thiese


Model outputs
--------------

Code should be in a directory called <base dire>/offset_simulator

Outputs will be written into a directory called <base dire>/offset_data

offset_data has the following structure:

* input data name (by default 'simulated', for simulated data). Others will be here if you run with other data types.

Within this directory there are the following directories 

- `simulation_outputs` contains a folders automatically numbered sequentially for each new run. In each of these numbered folders there will be two folders called `collated_realisations` and `simulation_data`.  `simulation_data` is the raw outputs of the simulation. `collated_realisations` are processed outputs which can be used to make plots and there is one file per realization. The collated_realisations files have the impacts calculated with all specified counterfactuals. 

- `simulation_inputs` contains all the processed input data needed to run the simulation (NOTE THIS ONLY APPLIES TO THE LATEST RUN). eg <base dire>/offset_data/simulated/simulation_outputs/00005 where 00005 is the latest run that has been done.





