## Gradient Matching ##

This folder contains all the essential code and results regarding the gradient matching approach presented in Xiao et al. 

File **PDE_GradientMatching_Main** and **PDE_GradientMatching_Functions** are used to produce parameter estimates at different CVs. 

File **PDE_GradientMatching_FixPar** and **PDE_GradientMatching_Functions_FixPar** are used to produce parameter estimates at different CVs with certain parameter values constrained. 

File **PDE_GradientMatching_PostProcess** is used to process all the collected data and obtain the final results presented in the paper. 

File **Convergence check of optimizations** checks the convergence of optimizations performed to obtain the original results, in order to ensure the parameter estimates are obtained after the convergence in **optim** is reached. 

File **Plot_patterns** is used to plot the invasion pattern based on parameter values chosen. 
 
Folder **Gradient plots** contains all the plots of averaged and explicit spatial/temporal gradients involved in the PDE system studied in the manuscript.

Folder **Possible solutions to improve accuracy** contains all the code and results that aim to improve the accuracy of parameter estimates.

Folder **Results without measurement errors** contains the reference gradients predicted by GAM in the gradient matching scheme with no measurement errors added to the data and the true gradients calculated by the finite difference scheme. 
 
Folder **Sensitivity tests results** contains all the results of the three sensitivity tests mentioned in the manuscript. 

Folder **SimRes_ests** and **SimRes_ests_converge_check** contains the original results and the updated results with convergence being checked.

All simulation results were generated using R 3.5.3 “Great Truth”.
