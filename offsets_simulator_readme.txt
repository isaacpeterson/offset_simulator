Download and install the latest version of R

if you are running from a unix system to install R:
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install git


from R run
install.packages("foreach")
install.packages("doParallel")
install.packages("abind")
install.packages("pixmap")

initialise_params.R 
	- set policy parameters and ecology parameters e.g. offset gains structure, ecology size and dimensions etc

run_offsets_simulation.R 
	- inputs policy parameters from initialise_params.R, 
	- runs simulations from simulation_routines.R
	- collates simulation outputs with collate_routines.R and plot_routines.R
	- plots outputs with plot_routines.R

plot_collated_realisations.R
	- collate simulation outputs if necessary by setting action_type = 'collate_realisations’, 
	- run again with action_type set to 'plot_outcomes" or 'plot_impacts' to plot either of these 
	- setting the policy_type switch to 'restoration_gains', 'avoided_degs' or 'net_gains' finds all policy variations (time horizon etc) of those policy types and plots the outcomes or impacts for those policy types. 
 
