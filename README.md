# offsetsim

OffsetSim is an iterative model that simulates the landscape under a set of user-defined offset conservation actions and gains structures to yield an assessment of the gains and losses at the site scale, where the loss of biodiversity from a particular development site is mitigated by an associated gain from one more offset sites, the program scale, comprising all sites in the development-offset program, and the landscape scale, comprising all sites in the landscape, i.e. the sites within the development-offset program as well as those not included. The model requires a set of GIS data layers to be defined as feature layers, as well as a set of response functions that describe the effect of each action on each feature layer. The features can represent any user defined spatial layer including species occupation, habitat types, vegetation classes, environmental domains, ecosystems, and ecosystem services. Actions such as development, protection, restoration etc. are defined and linked to these features. The responses to these actions include the characteristics of the offset action (e.g. the characteristics of the offset restoration function) and are provided as user defined functions. 


 

----------

## How to install

Download and install the latest version of R. On Ubuntu or another debian-based linux system, this can be done on the terminal as follows:

```
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install git
```

You will then require the R `devtools` package in order to install offsetsim. To install `devtools`, from inside R do:
```
install.packages('devtools')
```
To now install the `offsetsim` package, from inside R do:
```
devtools::install_github("isaacpeterson/offset_simulator")
```
This may take a while as R downloads, builds, and installs the package and all its dependencies.

----------

## How to run

Once `offsetsim` is installed, the easiest way to get started is to generate an offset simulation example that you can customise. To create the user example from inside R do:
```
library(offsetsim)
osim.create.example('./my-osim-example')
```
This will download the example files (make sure you have an active internet connection) into the directory `./my-osim-example`. To now run the example from inside R do:
```
setwd('./my-osim-example'); source('offsetsim_example.R')
```

----------

## How to get more help

You can get more information on `offsetsim` functionality, from inside R do:
```
??offsetsim
```

----------


## Description on the files


### initialise_params_defaults.R

* The default values for parameters used in the simulation e.g. for the number of developments, the counterfactual used in offsetting etc... There are many parameters in this file and if you are only changing a subset of them, these can be overwritten using another specified file (see for example initialise_params_scale_paper.R). This file can only be used to overwrite parameters in initialise_params_defaults.R and can't be used to define new parameters.


### run_offsets_simulation.R

* inputs policy parameters from initialise_params_defaults.R (and and specified file for overwritting parameter values)
* runs simulations from simulation_routines.R
* collates simulation outputs with collate_routines.R


### plot_collated_realisations.R

* Plot the results of the simulation
* run with action_type set to 'plot_outcomes' or 'plot_impacts' to plot either of these


Model outputs
--------------

Code should be in a directory called <base dire>/offset_simulator

Outputs will be written into a directory called <base dire>/offset_data

offset_data has the following structure:

* input data name (by default 'simulated', for simulated data). Others will be here if you run with other data types.

Within this directory there are the following directories

- `simulation_runs` contains a folders automatically numbered sequentially for each new run. In each of these numbered folders there will be three folders called `collated_outputs`, `simulation_data` and `simulation_params`. `simulation_data` is the raw outputs of the simulation, and will only be retained if the appropriate parameter is set (this can take a lot of space). `collated_realisations` are processed outputs which can be used to make plots and there is one file per realization. The collated_realisations files have the impacts calculated with all specified counterfactuals. `simulation_params` contains the parameter settings and other info used to generated the simulation.

- `simulation_inputs` contains all the processed input data needed to run the simulation (NOTE THIS ONLY APPLIES TO THE LATEST RUN). eg <base dire>/offset_data/simulated/simulation_outputs/00005 where 00005 is the latest run that has been done.


Points to note
--------------
This script can occasionally create runaway processes if the runs are stopped before completion by the user. By default the script will utilize all available processors on your machine. So if you terminate the script while it's running, it may leave some R processes running. This can be terminated by just quitting R, or manually killing those remaining processes.
