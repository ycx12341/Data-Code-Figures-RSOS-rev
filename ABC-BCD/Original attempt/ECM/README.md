## ECM ##

This folder contains all the essential code and results regarding the evaluations on the ECM density profile. 

Files **paras sampl ecm rx.R** read in the parameters to be evaluated in round x, then resample the parameters to be evaluated in the next round.

File **Automatic.R** contains all the necessary R functions of the ABC scheme.

Files **bcd_ecm_rx.txt** contain the Bhattacharyya distance results of the parameters being evaluated in round x.

Files **Round x parameters 10000 ecm.txt** contain the parameter values being evaluated in round x. 

File **mean_var_obs.txt** contains the reference summary statistics. 

File **BCD_results_ecm.zip** contains all the Bhattacharyya distance results regarding the evaluations of the ECM density profile, written in .rds files.  

Folder **ECM MDE** and **TC ECM MDE** have similar structures to this folder. 
