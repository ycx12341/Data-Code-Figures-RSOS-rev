% Gradient_plots
% Author: Yunchen Xiao

% This MATLAB file generates the plots of the explicit spatial/temporal 
% gradients, averaged over 200 data sets at different levels of measurement
% errors. 

%% Environment setting
clc
clear all
close all
 
set(0,'DefaultAxesFontName', 'Times New Roman')
set(0,'DefaultAxesFontSize', 18)
set(0,'defaultaxeslinewidth',1)
set(0,'defaultpatchlinewidth',1)
set(0,'defaultlinelinewidth',4)
set(0,'defaultTextInterpreter','latex')

x11 = linspace(0,1,80);
x11_res = x11(2:79);

%% Read in all the data
dndt_grads = readtable('dndt gradients.txt');
dndt_ref_gam = table2array(dndt_grads(1:9, 2:79));
dndt_ref_fds = table2array(dndt_grads(10:18, 2:79));
dndt_cv1 = table2array(dndt_grads(19:27, 2:79));
dndt_cv2 = table2array(dndt_grads(28:36, 2:79));
dndt_cv3 = table2array(dndt_grads(37:45, 2:79));
dndt_cv4 = table2array(dndt_grads(46:54, 2:79));
dndt_cv5 = table2array(dndt_grads(55:63, 2:79));

dn_grads = readtable('dn gradients.txt');
dn_ref_gam = table2array(dn_grads(1:9, 2:79));
dn_ref_fds = table2array(dn_grads(10:18, 2:79));
dn_cv1 = table2array(dn_grads(19:27, 2:79));
dn_cv2 = table2array(dn_grads(28:36, 2:79));
dn_cv3 = table2array(dn_grads(37:45, 2:79));
dn_cv4 = table2array(dn_grads(46:54, 2:79));
dn_cv5 = table2array(dn_grads(55:63, 2:79));

ga_grads = readtable('gamma gradients.txt');
ga_ref_gam = table2array(ga_grads(1:9, 2:79));
ga_ref_fds = table2array(ga_grads(10:18, 2:79));
ga_cv1 = table2array(ga_grads(19:27, 2:79));
ga_cv2 = table2array(ga_grads(28:36, 2:79));
ga_cv3 = table2array(ga_grads(37:45, 2:79));
ga_cv4 = table2array(ga_grads(46:54, 2:79));
ga_cv5 = table2array(ga_grads(55:63, 2:79));

rn_grads = readtable('rn gradients.txt');
rn_ref_gam = table2array(rn_grads(1:9, 2:79));
rn_ref_fds = table2array(rn_grads(10:18, 2:79));
rn_cv1 = table2array(rn_grads(19:27, 2:79));
rn_cv2 = table2array(rn_grads(28:36, 2:79));
rn_cv3 = table2array(rn_grads(37:45, 2:79));
rn_cv4 = table2array(rn_grads(46:54, 2:79));
rn_cv5 = table2array(rn_grads(55:63, 2:79));

dfdt_grads = readtable('dfdt gradients.txt');
dfdt_ref_gam = table2array(dfdt_grads(1:9, 2:79));
dfdt_ref_fds = table2array(dfdt_grads(10:18, 2:79));
dfdt_cv1 = table2array(dfdt_grads(19:27, 2:79));
dfdt_cv2 = table2array(dfdt_grads(28:36, 2:79));
dfdt_cv3 = table2array(dfdt_grads(37:45, 2:79));
dfdt_cv4 = table2array(dfdt_grads(46:54, 2:79));
dfdt_cv5 = table2array(dfdt_grads(55:63, 2:79));

eta_grads = readtable('eta gradients.txt');
eta_ref_gam = table2array(eta_grads(1:9, 2:79));
eta_ref_fds = table2array(eta_grads(10:18, 2:79));
eta_cv1 = table2array(eta_grads(19:27, 2:79));
eta_cv2 = table2array(eta_grads(28:36, 2:79));
eta_cv3 = table2array(eta_grads(37:45, 2:79));
eta_cv4 = table2array(eta_grads(46:54, 2:79));
eta_cv5 = table2array(eta_grads(55:63, 2:79));

dmdt_grads = readtable('dmdt gradients.txt');
dmdt_ref_gam = table2array(dmdt_grads(1:9, 2:79));
dmdt_ref_fds = table2array(dmdt_grads(10:18, 2:79));
dmdt_cv1 = table2array(dmdt_grads(19:27, 2:79));
dmdt_cv2 = table2array(dmdt_grads(28:36, 2:79));
dmdt_cv3 = table2array(dmdt_grads(37:45, 2:79));
dmdt_cv4 = table2array(dmdt_grads(46:54, 2:79));
dmdt_cv5 = table2array(dmdt_grads(55:63, 2:79));

dm_grads = readtable('dm gradients.txt');
dm_ref_gam = table2array(dm_grads(1:9, 2:79));
dm_ref_fds = table2array(dm_grads(10:18, 2:79));
dm_cv1 = table2array(dm_grads(19:27, 2:79));
dm_cv2 = table2array(dm_grads(28:36, 2:79));
dm_cv3 = table2array(dm_grads(37:45, 2:79));
dm_cv4 = table2array(dm_grads(46:54, 2:79));
dm_cv5 = table2array(dm_grads(55:63, 2:79));

al_grads = readtable('alpha gradients.txt');
al_ref_gam = table2array(al_grads(1:9, 2:79));
al_ref_fds = table2array(al_grads(10:18, 2:79));
al_cv1 = table2array(al_grads(19:27, 2:79));
al_cv2 = table2array(al_grads(28:36, 2:79));
al_cv3 = table2array(al_grads(37:45, 2:79));
al_cv4 = table2array(al_grads(46:54, 2:79));
al_cv5 = table2array(al_grads(55:63, 2:79));

%% Temporal gradients of tumour cell density (dn/dt)

for i = 1:9
    figure
    plot(x11_res, dndt_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, dndt_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, dndt_cv1(i,:),'k-')
    plot(x11_res, dndt_cv2(i,:),'g-')
    plot(x11_res, dndt_cv3(i,:),'b-')
    plot(x11_res, dndt_cv4(i,:),'c-')
    plot(x11_res, dndt_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([0 0.6])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{\frac{\partial n}{\partial t}}$ at $t = $ ',num2str(i)])
end 

%% Diffusion term of tumour cells
for i = 1:9
    figure
    plot(x11_res, dn_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, dn_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, dn_cv1(i,:),'k-')
    plot(x11_res, dn_cv2(i,:),'g-')
    plot(x11_res, dn_cv3(i,:),'b-')
    plot(x11_res, dn_cv4(i,:),'c-')
    plot(x11_res, dn_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([-45 25])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{\frac{\partial^{2} n}{\partial x^{2}}}$ at $t = $ ',num2str(i)])
end 

%% Haptotaxis term
for i = 1:9
    figure
    plot(x11_res, ga_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, ga_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, ga_cv1(i,:),'k-')
    plot(x11_res, ga_cv2(i,:),'g-')
    plot(x11_res, ga_cv3(i,:),'b-')
    plot(x11_res, ga_cv4(i,:),'c-')
    plot(x11_res, ga_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([-20 20])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{\frac{\partial}{\partial x}\left(n\frac{\partial f}{\partial x}\right)}$ at $t = $ ', num2str(i)])
end 

%% Logistic growth of tumour cells
for i = 1:9
    figure
    plot(x11_res, rn_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, rn_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, rn_cv1(i,:),'k-')
    plot(x11_res, rn_cv2(i,:),'g-')
    plot(x11_res, rn_cv3(i,:),'b-')
    plot(x11_res, rn_cv4(i,:),'c-')
    plot(x11_res, rn_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([-0.05 0.25])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{n\left(1-n-f\right)}$ at $t = $ ', num2str(i)])
end 

%% Temporal gradients of ECM density (df\dt)
for i = 1:9
    figure
    plot(x11_res, dfdt_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, dfdt_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, dfdt_cv1(i,:),'k-')
    plot(x11_res, dfdt_cv2(i,:),'g-')
    plot(x11_res, dfdt_cv3(i,:),'b-')
    plot(x11_res, dfdt_cv4(i,:),'c-')
    plot(x11_res, dfdt_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([-0.8 0])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{\frac{\partial f}{\partial t}}$ at $t = $ ', num2str(i)])
end 

%% Decay of ECM.
for i = 1:9
    figure
    plot(x11_res, eta_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, eta_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, eta_cv1(i,:),'k-')
    plot(x11_res, eta_cv2(i,:),'g-')
    plot(x11_res, eta_cv3(i,:),'b-')
    plot(x11_res, eta_cv4(i,:),'c-')
    plot(x11_res, eta_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([0 0.06])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{mf}$ at $t = $ ', num2str(i)])
end 

%% Temporal gradients of MDE. (dm/dt)
for i = 1:9
    figure
    plot(x11_res, dmdt_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, dmdt_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, dmdt_cv1(i,:),'k-')
    plot(x11_res, dmdt_cv2(i,:),'g-')
    plot(x11_res, dmdt_cv3(i,:),'b-')
    plot(x11_res, dmdt_cv4(i,:),'c-')
    plot(x11_res, dmdt_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([-0.05 0.13])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{\frac{\partial m}{\partial t}}$ at $t = $ ', num2str(i)])
end 

%% Diffusion term of MDE.
for i = 1:9
    figure
    plot(x11_res, dm_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, dm_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, dm_cv1(i,:),'k-')
    plot(x11_res, dm_cv2(i,:),'g-')
    plot(x11_res, dm_cv3(i,:),'b-')
    plot(x11_res, dm_cv4(i,:),'c-')
    plot(x11_res, dm_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([-10 5])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{\frac{\partial^{2}m}{\partial x^{2}}}$ at $t = $ ', num2str(i)])
end 

%% Growth/proliferation of MDE.
for i = 1:9
    figure
    plot(x11_res, al_ref_gam(i,:), 'r--')
    hold on;
    plot(x11_res, al_ref_fds(i,:),'-','Color',[0.61 0.51 0.74])
    plot(x11_res, al_cv1(i,:),'k-')
    plot(x11_res, al_cv2(i,:),'g-')
    plot(x11_res, al_cv3(i,:),'b-')
    plot(x11_res, al_cv4(i,:),'c-')
    plot(x11_res, al_cv5(i,:),'m-')
    %xlim([0 0.11])
    ylim([0 1.2])
    xlabel('Spatial domain')
    ylabel('Estimated gradients')
    title(['$\hat{n}$ at $t = $ ', num2str(i)])
end 