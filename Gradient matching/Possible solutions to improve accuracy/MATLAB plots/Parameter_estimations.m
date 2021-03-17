% Parameter_estimations.m
% Author: Yunchen Xiao

% This MATLAB file generates the parameter estimates returned from the
% gradient matching scheme proposed in Xiao et al. under different
% circumstances which attempt to reduce the bias: truncating the gradient
% matrices, a subset of gradients estimated by GAM being replaced by the
% true values calculated by the finite difference scheme.

%% Environment settings
clc
clear all
close all
 
set(0,'DefaultAxesFontName', 'Times New Roman')
set(0,'DefaultAxesFontSize', 18)
set(0,'defaultaxeslinewidth',1)
set(0,'defaultpatchlinewidth',1)
set(0,'defaultlinelinewidth',4)
set(0,'defaultTextInterpreter','latex')

%% Read in all the data.
paras_all = readtable('Parameters means at all CV levels.txt');
paras_tc_replaced = readtable('Parameters means tc gradients replaced by FDS grads.txt');
paras_tc_replaced_except_gamma = readtable('Parameters means tc gradients except gamma replaced by FDS grads.txt');
paras_cut = readtable('Parameters means truncated.txt');
paras_tc_dm_replaced = readtable('Parameters means tc dm dmdt gradients replaced by FDS grads.txt');

%% Estimates

% dn estimates
dn_est_all = table2array(paras_all(:,2));
dn_est_cut = table2array(paras_cut(:,2));
dn_est_tc_replaced_except_gamma = table2array(paras_tc_replaced_except_gamma(:,2));
dn_est_tc_replaced = table2array(paras_tc_replaced(:,2));
dn_est_tc_dm_replaced = table2array(paras_tc_dm_replaced(:,2));

% gamma estimates
gamma_est_all = table2array(paras_all(:,3));
gamma_est_cut = table2array(paras_cut(:,3));
gamma_est_tc_replaced_except_gamma = table2array(paras_tc_replaced_except_gamma(:,3));
gamma_est_tc_replaced = table2array(paras_tc_replaced(:,3));
gamma_est_tc_dm_replaced = table2array(paras_tc_dm_replaced(:,3));

% rn estimates
rn_est_all = table2array(paras_all(:,4));
rn_est_cut = table2array(paras_cut(:,4));
rn_est_tc_replaced_except_gamma = table2array(paras_tc_replaced_except_gamma(:,4));
rn_est_tc_replaced = table2array(paras_tc_replaced(:,4));
rn_est_tc_dm_replaced = table2array(paras_tc_dm_replaced(:,4));

% eta estimates
eta_est_all = table2array(paras_all(:,5));
eta_est_cut = table2array(paras_cut(:,5));
eta_est_tc_replaced_except_gamma = table2array(paras_tc_replaced_except_gamma(:,5));
eta_est_tc_replaced = table2array(paras_tc_replaced(:,5));
eta_est_tc_dm_replaced = table2array(paras_tc_dm_replaced(:,5));

% dm estimates
dm_est_all = table2array(paras_all(:,6));
dm_est_cut = table2array(paras_cut(:,6));
dm_est_tc_replaced_except_gamma = table2array(paras_tc_replaced_except_gamma(:,6));
dm_est_tc_replaced = table2array(paras_tc_replaced(:,6));
dm_est_tc_dm_replaced = table2array(paras_tc_dm_replaced(:,6));

% alpha estimates
alpha_est_all = table2array(paras_all(:,7));
alpha_est_cut = table2array(paras_cut(:,7));
alpha_est_tc_replaced_except_gamma = table2array(paras_tc_replaced_except_gamma(:,7));
alpha_est_tc_replaced = table2array(paras_tc_replaced(:,7));
alpha_est_tc_dm_replaced = table2array(paras_tc_dm_replaced(:,7));

%% Plots
x = [0.01 0.025 0.05 0.075 0.1];                               

%% Plot of dn estimates under different circumstances:
figure
yyaxis left
plot(x, dn_est_all, 'k-')
title('$\hat{d_n}$')
hold on;
plot(x, dn_est_cut, 'b-')
plot(x, dn_est_tc_replaced_except_gamma, 'r-')
plot(x, dn_est_tc_replaced, 'g-')
plot(x, dn_est_tc_dm_replaced, 'm-')
plot(0.01, dn_est_all(1), 'xk', 'markersize', 20)
plot(0.025, dn_est_all(2), 'xk', 'markersize', 20)
plot(0.05, dn_est_all(3), 'xk', 'markersize', 20)
plot(0.075, dn_est_all(4), 'xk', 'markersize', 20)
plot(0.1, dn_est_all(5), 'xk', 'markersize', 20)
plot(0.01, dn_est_cut(1), 'xb', 'markersize', 20)
plot(0.025, dn_est_cut(2), 'xb', 'markersize', 20)
plot(0.05, dn_est_cut(3), 'xb', 'markersize', 20)
plot(0.075, dn_est_cut(4), 'xb', 'markersize', 20)
plot(0.1, dn_est_cut(5), 'xb', 'markersize', 20)
plot(0.01, dn_est_tc_replaced_except_gamma(1), 'xr', 'markersize', 20)
plot(0.025, dn_est_tc_replaced_except_gamma(2), 'xr', 'markersize', 20)
plot(0.05, dn_est_tc_replaced_except_gamma(3), 'xr', 'markersize', 20)
plot(0.075, dn_est_tc_replaced_except_gamma(4), 'xr', 'markersize', 20)
plot(0.1, dn_est_tc_replaced_except_gamma(5), 'xr', 'markersize', 20)
plot(0.01, dn_est_tc_replaced(1), 'xg', 'markersize', 20)
plot(0.025, dn_est_tc_replaced(2), 'xg', 'markersize', 20)
plot(0.05, dn_est_tc_replaced(3), 'xg', 'markersize', 20)
plot(0.075, dn_est_tc_replaced(4), 'xg', 'markersize', 20)
plot(0.1, dn_est_tc_replaced(5), 'xg', 'markersize', 20)
plot(0.01, dn_est_tc_dm_replaced(1), 'xm', 'markersize', 20)
plot(0.025, dn_est_tc_dm_replaced(2), 'xm', 'markersize', 20)
plot(0.05, dn_est_tc_dm_replaced(3), 'xm', 'markersize', 20)
plot(0.075, dn_est_tc_dm_replaced(4), 'xm', 'markersize', 20)
plot(0.1, dn_est_tc_dm_replaced(5), 'xm', 'markersize', 20)
yline(0.01,'r--','Linewidth',3.5);
xlim([0 0.11])
xlabel('Measurement error CV')
ylim([-0.002 0.012])
ylabel('Mean ($\hat{d_n}$)')
yyaxis right
h = plot(x,dn_est_all,'k-');
delete(h)
ylim([-120 20])
ylabel('Percentage error')

%% Plot of gamma estimations under different circumstances:
figure
yyaxis left
plot(x, gamma_est_all, 'k-')
title('$\hat{\gamma}$')
hold on;
plot(x, gamma_est_cut, 'b-')
plot(x, gamma_est_tc_replaced_except_gamma, 'r-')
plot(x, gamma_est_tc_replaced, 'g-')
plot(x, gamma_est_tc_dm_replaced, 'm-')
plot(0.01, gamma_est_all(1), 'xk', 'markersize', 20)
plot(0.025, gamma_est_all(2), 'xk', 'markersize', 20)
plot(0.05, gamma_est_all(3), 'xk', 'markersize', 20)
plot(0.075, gamma_est_all(4), 'xk', 'markersize', 20)
plot(0.1, gamma_est_all(5), 'xk', 'markersize', 20)
plot(0.01, gamma_est_cut(1), 'xb', 'markersize', 20)
plot(0.025, gamma_est_cut(2), 'xb', 'markersize', 20)
plot(0.05, gamma_est_cut(3), 'xb', 'markersize', 20)
plot(0.075, gamma_est_cut(4), 'xb', 'markersize', 20)
plot(0.1, gamma_est_cut(5), 'xb', 'markersize', 20)
plot(0.01, gamma_est_tc_replaced_except_gamma(1), 'xr', 'markersize', 20)
plot(0.025, gamma_est_tc_replaced_except_gamma(2), 'xr', 'markersize', 20)
plot(0.05, gamma_est_tc_replaced_except_gamma(3), 'xr', 'markersize', 20)
plot(0.075, gamma_est_tc_replaced_except_gamma(4), 'xr', 'markersize', 20)
plot(0.1, gamma_est_tc_replaced_except_gamma(5), 'xr', 'markersize', 20)
plot(0.01, gamma_est_tc_replaced(1), 'xg', 'markersize', 20)
plot(0.025, gamma_est_tc_replaced(2), 'xg', 'markersize', 20)
plot(0.05, gamma_est_tc_replaced(3), 'xg', 'markersize', 20)
plot(0.075, gamma_est_tc_replaced(4), 'xg', 'markersize', 20)
plot(0.1, gamma_est_tc_replaced(5), 'xg', 'markersize', 20)
plot(0.01, gamma_est_tc_dm_replaced(1), 'xm', 'markersize', 20)
plot(0.025, gamma_est_tc_dm_replaced(2), 'xm', 'markersize', 20)
plot(0.05, gamma_est_tc_dm_replaced(3), 'xm', 'markersize', 20)
plot(0.075, gamma_est_tc_dm_replaced(4), 'xm', 'markersize', 20)
plot(0.1, gamma_est_tc_dm_replaced(5), 'xm', 'markersize', 20)
yline(0.05,'r--','Linewidth',3.5);
xlim([0 0.11])
xlabel('Measurement error CV')
ylim([0.025 0.055])
ylabel('Mean ($\hat{\gamma}$)')
yyaxis right
h = plot(x,dn_est_all,'k-');
delete(h)
ylim([-50 10])
ylabel('Percentage error')

%% Plot of rn estimations under different circumstances:
figure
yyaxis left
plot(x, rn_est_all, 'k-')
title('$\hat{r_{n}}$')
hold on;
plot(x, rn_est_cut, 'b-')
plot(x, rn_est_tc_replaced_except_gamma, 'r-')
plot(x, rn_est_tc_replaced, 'g-')
plot(x, rn_est_tc_dm_replaced, 'm-')
plot(0.01, rn_est_all(1), 'xk', 'markersize', 20)
plot(0.025, rn_est_all(2), 'xk', 'markersize', 20)
plot(0.05, rn_est_all(3), 'xk', 'markersize', 20)
plot(0.075, rn_est_all(4), 'xk', 'markersize', 20)
plot(0.1, rn_est_all(5), 'xk', 'markersize', 20)
plot(0.01, rn_est_cut(1), 'xb', 'markersize', 20)
plot(0.025, rn_est_cut(2), 'xb', 'markersize', 20)
plot(0.05, rn_est_cut(3), 'xb', 'markersize', 20)
plot(0.075, rn_est_cut(4), 'xb', 'markersize', 20)
plot(0.1, rn_est_cut(5), 'xb', 'markersize', 20)
plot(0.01, rn_est_tc_replaced_except_gamma(1), 'xr', 'markersize', 20)
plot(0.025, rn_est_tc_replaced_except_gamma(2), 'xr', 'markersize', 20)
plot(0.05, rn_est_tc_replaced_except_gamma(3), 'xr', 'markersize', 20)
plot(0.075, rn_est_tc_replaced_except_gamma(4), 'xr', 'markersize', 20)
plot(0.1, rn_est_tc_replaced_except_gamma(5), 'xr', 'markersize', 20)
plot(0.01, rn_est_tc_replaced(1), 'xg', 'markersize', 20)
plot(0.025, rn_est_tc_replaced(2), 'xg', 'markersize', 20)
plot(0.05, rn_est_tc_replaced(3), 'xg', 'markersize', 20)
plot(0.075, rn_est_tc_replaced(4), 'xg', 'markersize', 20)
plot(0.1, rn_est_tc_replaced(5), 'xg', 'markersize', 20)
plot(0.01, rn_est_tc_dm_replaced(1), 'xm', 'markersize', 20)
plot(0.025, rn_est_tc_dm_replaced(2), 'xm', 'markersize', 20)
plot(0.05, rn_est_tc_dm_replaced(3), 'xm', 'markersize', 20)
plot(0.075, rn_est_tc_dm_replaced(4), 'xm', 'markersize', 20)
plot(0.1, rn_est_tc_dm_replaced(5), 'xm', 'markersize', 20)
yline(5,'r--','Linewidth',3.5);
xlim([0 0.11])
xlabel('Measurement error CV')
ylim([2.8 5.1])
ylabel('Mean ($\hat{r_{n}}$)')
yyaxis right
h = plot(x,dn_est_all,'k-');
delete(h)
ylim([-44 2])
ylabel('Percentage error')

%% Plot of eta estimations under different circumstances:
figure
yyaxis left
plot(x, eta_est_all, 'k-')
title('$\hat{\eta}$')
hold on;
plot(x, eta_est_cut, 'b-')
plot(x, eta_est_tc_replaced_except_gamma, 'r-')
plot(x, eta_est_tc_replaced, 'g-')
plot(x, eta_est_tc_dm_replaced, 'm-')
plot(0.01, eta_est_all(1), 'xk', 'markersize', 20)
plot(0.025, eta_est_all(2), 'xk', 'markersize', 20)
plot(0.05, eta_est_all(3), 'xk', 'markersize', 20)
plot(0.075, eta_est_all(4), 'xk', 'markersize', 20)
plot(0.1, eta_est_all(5), 'xk', 'markersize', 20)
plot(0.01, eta_est_cut(1), 'xb', 'markersize', 20)
plot(0.025, eta_est_cut(2), 'xb', 'markersize', 20)
plot(0.05, eta_est_cut(3), 'xb', 'markersize', 20)
plot(0.075, eta_est_cut(4), 'xb', 'markersize', 20)
plot(0.1, eta_est_cut(5), 'xb', 'markersize', 20)
plot(0.01, eta_est_tc_replaced_except_gamma(1), 'xr', 'markersize', 20)
plot(0.025, eta_est_tc_replaced_except_gamma(2), 'xr', 'markersize', 20)
plot(0.05, eta_est_tc_replaced_except_gamma(3), 'xr', 'markersize', 20)
plot(0.075, eta_est_tc_replaced_except_gamma(4), 'xr', 'markersize', 20)
plot(0.1, eta_est_tc_replaced_except_gamma(5), 'xr', 'markersize', 20)
plot(0.01, eta_est_tc_replaced(1), 'xg', 'markersize', 20)
plot(0.025, eta_est_tc_replaced(2), 'xg', 'markersize', 20)
plot(0.05, eta_est_tc_replaced(3), 'xg', 'markersize', 20)
plot(0.075, eta_est_tc_replaced(4), 'xg', 'markersize', 20)
plot(0.1, eta_est_tc_replaced(5), 'xg', 'markersize', 20)
plot(0.01, eta_est_tc_dm_replaced(1), 'xm', 'markersize', 20)
plot(0.025, eta_est_tc_dm_replaced(2), 'xm', 'markersize', 20)
plot(0.05, eta_est_tc_dm_replaced(3), 'xm', 'markersize', 20)
plot(0.075, eta_est_tc_dm_replaced(4), 'xm', 'markersize', 20)
plot(0.1, eta_est_tc_dm_replaced(5), 'xm', 'markersize', 20)
yline(10,'r--','Linewidth',3.5);
xlim([0 0.11])
xlabel('Measurement error CV')
ylim([9.9 10.5])
ylabel('Mean ($\hat{\eta}$)')
yyaxis right
h = plot(x,dn_est_all,'k-');
delete(h)
ylim([-1 5])
ylabel('Percentage error')

%% Plot of dm estimations under different circumstances:
figure
yyaxis left
plot(x, dm_est_all, 'k-')
title('$\hat{d_{m}}$')
hold on;
plot(x, dm_est_cut, 'b-')
plot(x, dm_est_tc_replaced_except_gamma, 'r-')
plot(x, dm_est_tc_replaced, 'g-')
plot(x, dm_est_tc_dm_replaced, 'm-')
plot(0.01, dm_est_all(1), 'xk', 'markersize', 20)
plot(0.025, dm_est_all(2), 'xk', 'markersize', 20)
plot(0.05, dm_est_all(3), 'xk', 'markersize', 20)
plot(0.075, dm_est_all(4), 'xk', 'markersize', 20)
plot(0.1, dm_est_all(5), 'xk', 'markersize', 20)
plot(0.01, dm_est_cut(1), 'xb', 'markersize', 20)
plot(0.025, dm_est_cut(2), 'xb', 'markersize', 20)
plot(0.05, dm_est_cut(3), 'xb', 'markersize', 20)
plot(0.075, dm_est_cut(4), 'xb', 'markersize', 20)
plot(0.1, dm_est_cut(5), 'xb', 'markersize', 20)
plot(0.01, dm_est_tc_replaced_except_gamma(1), 'xr', 'markersize', 20)
plot(0.025, dm_est_tc_replaced_except_gamma(2), 'xr', 'markersize', 20)
plot(0.05, dm_est_tc_replaced_except_gamma(3), 'xr', 'markersize', 20)
plot(0.075, dm_est_tc_replaced_except_gamma(4), 'xr', 'markersize', 20)
plot(0.1, dm_est_tc_replaced_except_gamma(5), 'xr', 'markersize', 20)
plot(0.01, dm_est_tc_replaced(1), 'xg', 'markersize', 20)
plot(0.025, dm_est_tc_replaced(2), 'xg', 'markersize', 20)
plot(0.05, dm_est_tc_replaced(3), 'xg', 'markersize', 20)
plot(0.075, dm_est_tc_replaced(4), 'xg', 'markersize', 20)
plot(0.1, dm_est_tc_replaced(5), 'xg', 'markersize', 20)
plot(0.01, dm_est_tc_dm_replaced(1), 'xm', 'markersize', 20)
plot(0.025, dm_est_tc_dm_replaced(2), 'xm', 'markersize', 20)
plot(0.05, dm_est_tc_dm_replaced(3), 'xm', 'markersize', 20)
plot(0.075, dm_est_tc_dm_replaced(4), 'xm', 'markersize', 20)
plot(0.1, dm_est_tc_dm_replaced(5), 'xm', 'markersize', 20)
yline(0.01,'r--','Linewidth',3.5);
xlim([0 0.11])
xlabel('Measurement error CV')
ylim([0.0075 0.0101])
ylabel('Mean ($\hat{d_{m}}$)')
yyaxis right
h = plot(x,dn_est_all,'k-');
delete(h)
ylim([-25 1])
ylabel('Percentage error')

%% Plot of alpha estimations under different circumstances:
figure
yyaxis left
plot(x, alpha_est_all, 'k-')
title('$\hat{\alpha}$')
hold on;
plot(x, alpha_est_cut, 'b-')
plot(x, alpha_est_tc_replaced_except_gamma, 'r-')
plot(x, alpha_est_tc_replaced, 'g-')
plot(x, alpha_est_tc_dm_replaced, 'm-')
plot(0.01, alpha_est_all(1), 'xk', 'markersize', 20)
plot(0.025, alpha_est_all(2), 'xk', 'markersize', 20)
plot(0.05, alpha_est_all(3), 'xk', 'markersize', 20)
plot(0.075, alpha_est_all(4), 'xk', 'markersize', 20)
plot(0.1, alpha_est_all(5), 'xk', 'markersize', 20)
plot(0.01, alpha_est_cut(1), 'xb', 'markersize', 20)
plot(0.025, alpha_est_cut(2), 'xb', 'markersize', 20)
plot(0.05, alpha_est_cut(3), 'xb', 'markersize', 20)
plot(0.075, alpha_est_cut(4), 'xb', 'markersize', 20)
plot(0.1, alpha_est_cut(5), 'xb', 'markersize', 20)
plot(0.01, alpha_est_tc_replaced_except_gamma(1), 'xr', 'markersize', 20)
plot(0.025, alpha_est_tc_replaced_except_gamma(2), 'xr', 'markersize', 20)
plot(0.05, alpha_est_tc_replaced_except_gamma(3), 'xr', 'markersize', 20)
plot(0.075, alpha_est_tc_replaced_except_gamma(4), 'xr', 'markersize', 20)
plot(0.1, alpha_est_tc_replaced_except_gamma(5), 'xr', 'markersize', 20)
plot(0.01, alpha_est_tc_replaced(1), 'xg', 'markersize', 20)
plot(0.025, alpha_est_tc_replaced(2), 'xg', 'markersize', 20)
plot(0.05, alpha_est_tc_replaced(3), 'xg', 'markersize', 20)
plot(0.075, alpha_est_tc_replaced(4), 'xg', 'markersize', 20)
plot(0.1, alpha_est_tc_replaced(5), 'xg', 'markersize', 20)
plot(0.01, alpha_est_tc_dm_replaced(1), 'xm', 'markersize', 20)
plot(0.025, alpha_est_tc_dm_replaced(2), 'xm', 'markersize', 20)
plot(0.05, alpha_est_tc_dm_replaced(3), 'xm', 'markersize', 20)
plot(0.075, alpha_est_tc_dm_replaced(4), 'xm', 'markersize', 20)
plot(0.1, alpha_est_tc_dm_replaced(5), 'xm', 'markersize', 20)
yline(0.1,'r--','Linewidth',3.5);
xlim([0 0.11])
xlabel('Measurement error CV')
ylim([0.097 0.102])
ylabel('Mean ($\hat{\alpha}$)')
yyaxis right
h = plot(x,dn_est_all,'k-');
delete(h)
ylim([-3 2])
ylabel('Percentage error')
