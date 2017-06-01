Download all files from git repository and run simulations from 
run_offsets_simulation.R - used to input policy parameters, controls system, and talks to run_system_routines.R collate_routines.R and plot_routines.R
initialise_params.R - is used to control policy parameters and ecology parameters e.g. offset gains structure, ecology size and dimensions etc
run_system_routines_modularised.R - functions to run simulation and generate simulation outputs
collate_routines.R - collate system outputs and organise into outputs to plot
plot_routines.R - plot collated outputs


the important switches in run_offsets_simulation are:

save_realisations = TRUE 
collate_realisations = TRUE #TRUE, FALSE to perform collate routines as well as running the simulation or not
save_collated_realisations = TRUE
plot_impacts = TRUE #if collating realisations switch on to see impacts 

once run_offsets_simulation finishes its routines then run plot_collated_realisations.R 

if the realisations have not been collated according to above then set 

action_type = 'collate_realisations'

allow the code to collate, then run plot_collated_realisations.R with the action_type switch set to 'plot_outcomes" or 'plot_impacts' to plot either of these 

setting the policy_type switch to 'restoration_gains', 'avoided_degs' or 'net_gains' finds all policy variations (time horizon etc) of those policy types and plots the outcomes or impacts for those policy types. 
 
