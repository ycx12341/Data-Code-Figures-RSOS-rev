% Post_process.m
% Author: Yunchen Xiao
% This MATLAB file generates the plots of probability densities of the
% parameter estimations at the end of each round of the first attempt
% applying ABC-BCD scheme on the main reference dataset.

% Environment settings
clc
clear all
close all
 
set(0,'DefaultAxesFontName', 'Times New Roman')
set(0,'DefaultAxesFontSize', 24)
set(0,'defaultaxeslinewidth',1)
set(0,'defaultpatchlinewidth',1)
set(0,'defaultlinelinewidth',4)
set(0,'defaultTextInterpreter','latex')

%% ECM rounds: density plot of eta
ecm_r1 = readtable("Round 1 parameters 10000 ecm.txt");

ecm_r2 = readtable("Round 2 parameters 10000 ecm.txt");

ecm_r3 = readtable("Round 3 parameters 10000 ecm.txt");

ecm_post = readtable("Round 4 parameters 10000 ecm.txt");

eta_r1 = table2array(ecm_r1(:,4));
eta_r2 = table2array(ecm_r2(:,4));
eta_r3 = table2array(ecm_r3(:,4));
eta_post = table2array(ecm_post(:,4));

[g,xii] = ksdensity(eta_r2,'Bandwidth',0.3014);
[h,xiii] = ksdensity(eta_r3,'Bandwidth',0.3014);
[j,xiv] = ksdensity(eta_post,'Bandwidth',0.3014);

figure
yline(1/11,'b','Linewidth',3.5);
xlim([7 18]);
hold on;
plot(xii,g,'g-',xiii,h,'r-',xiv,j,'k-')
plot(mean(table2array(ecm_r1(:,4))),0,'xb','markersize',20)
plot(mean(table2array(ecm_r2(:,4))),0,'xg','markersize',20)
plot(mean(table2array(ecm_r3(:,4))),0,'xr','markersize',20)
plot(mean(table2array(ecm_post(:,4))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density(ECM)','Post-round 1 density(ECM)','Post-round 2 density(ECM)','Post-round 3 density(ECM)'},'Location','northeast','Orientation','vertical','Fontsize',12);
xlabel('$\eta$ values')
ylabel('Probability density')

%% MDE & ECM rounds: density plot of dm and alpha
ecm_mde_r1 = readtable("Round 1 parameters 10000 ecm_mde.txt");

ecm_mde_r2 = readtable("Round 2 parameters 10000 ecm_mde.txt");

ecm_mde_r3 = readtable("Round 3 parameters 10000 ecm_mde.txt");

ecm_mde_post = readtable("Round 4 parameters 10000 ecm_mde.txt");

dm_r1 = table2array(ecm_mde_r1(:,5));
dm_r2 = table2array(ecm_mde_r2(:,5));
dm_r3 = table2array(ecm_mde_r3(:,5));
dm_post = table2array(ecm_mde_post(:,5));

alpha_r1 = table2array(ecm_mde_r1(:,6));
alpha_r2 = table2array(ecm_mde_r2(:,6));
alpha_r3 = table2array(ecm_mde_r3(:,6));
alpha_post = table2array(ecm_mde_post(:,6));

[s,xxii] = ksdensity(table2array(ecm_mde_r2(:,5)),'Bandwidth',0.000902);
[d,xxiii] = ksdensity(table2array(ecm_mde_r3(:,5)),'Bandwidth',0.000902);
[z,xxiv] = ksdensity(table2array(ecm_mde_post(:,5)),'Bandwidth',0.000902);

figure
yline(1/0.0329,'b-','Linewidth',3.5);
xlim([0.0001 0.033])
hold on;
plot(xxii,s,'g-',xxiii,d,'r-',xxiv,z,'k-')
plot(0.01,0,'x','Color',[.69 .56 .74],'markersize',20)
plot(mean(table2array(ecm_mde_r1(:,5))),0,'xb','markersize',20)
plot(mean(table2array(ecm_mde_r2(:,5))),0,'xg','markersize',20)
plot(mean(table2array(ecm_mde_r3(:,5))),0,'xr','markersize',20)
plot(mean(table2array(ecm_mde_post(:,5))),0,'xk','markersize',20)
hold off;
legend({'Initial density(ECM&MDE)','Post-round 1 density(ECM&MDE)','Post-round 2 density(ECM&MDE)','Post-round 3 density(ECM&MDE)'},'Location','northeast','Orientation','vertical','Fontsize',9)
xlabel('$d_m$ values')
ylabel('Probability density')

[s,xxii] = ksdensity(table2array(ecm_mde_r2(:,6)),'Bandwidth',0.003034);
[d,xxiii] = ksdensity(table2array(ecm_mde_r3(:,6)),'Bandwidth',0.003034);
[z,xxiv] = ksdensity(table2array(ecm_mde_post(:,6)),'Bandwidth',0.003034);

figure
yline(1/0.11,'b-','Linewidth',3.5);
xlim([0.07 0.18])
hold on;
plot(xxii,s,'g-',xxiii,d,'r-',xxiv,z,'k-')
plot(mean(table2array(ecm_mde_r1(:,6))),0,'xb','markersize',20)
plot(mean(table2array(ecm_mde_r2(:,6))),0,'xg','markersize',20)
plot(mean(table2array(ecm_mde_r3(:,6))),0,'xr','markersize',20)
plot(mean(table2array(ecm_mde_post(:,6))),0,'xk','markersize',20)
hold off;
legend({'Initial density(ECM&MDE)','Post-round 1 density(ECM&MDE)','Post-round 2 density(ECM&MDE)','Post-round 3 density(ECM&MDE)'},'Location','northeast','Orientation','vertical','Fontsize',10)
xlabel('$\alpha$ values')
ylabel('Probability density')

%% Tumour cells & ECM & MDE rounds: density plot of dn,gamma,rn
all3_r1 = readtable("Round 1 parameters 10000 all 3.txt");

all3_r2 = readtable("Round 2 parameters 10000 all 3.txt");

all3_r3 = readtable("Round 3 parameters 10000 all 3.txt");

all3_r4 = readtable("Round 4 parameters 10000 all 3.txt");

all3_post = readtable("Round 5 parameters 10000 all 3.txt");

[s,xxxii] = ksdensity(table2array(all3_r2(:,2)),'Bandwidth',0.0028);
[d,xxxiii] = ksdensity(table2array(all3_r3(:,2)),'Bandwidth',0.0028);
[z,xxxiv] = ksdensity(table2array(all3_r4(:,2)),'Bandwidth',0.0028);
[q,xxxv] = ksdensity(table2array(all3_post(:,2)),'Bandwidth',0.0028);

figure
yline(1/0.019931,'b-','Linewidth',3.5);
xlim([0.000069 0.02])
hold on;
plot(xxxii,s,'g-',xxxiii,d,'r-',xxxiv,z,'k-',xxxv,q,'y-')
plot(mean(table2array(all3_r1(:,2))),0,'xb','markersize',20)
plot(mean(table2array(all3_r2(:,2))),0,'xg','markersize',20)
plot(mean(table2array(all3_r3(:,2))),0,'xr','markersize',20)
plot(mean(table2array(all3_r4(:,2))),0,'xk','markersize',20)
plot(mean(table2array(all3_post(:,2))),0,'xy','markersize',20)
legend({'Initial density(All 3)','Post-round 1 density(All 3)','Post-round 2 density(All 3)','Post-round 3 density(All 3)','Final density'},'Location','northwest','Orientation','vertical','Fontsize',7)
hold off;
xlabel('$d_n$ values')
ylabel('Probability density')

[s,xxxii] = ksdensity(table2array(all3_r2(:,3)),'Bandwidth',0.00697);
[d,xxxiii] = ksdensity(table2array(all3_r3(:,3)),'Bandwidth',0.00697);
[z,xxxiv] = ksdensity(table2array(all3_r4(:,3)),'Bandwidth',0.00697);
[q,xxxv] = ksdensity(table2array(all3_post(:,3)),'Bandwidth',0.00697);

figure
yline(1/0.255,'b-','Linewidth',3.5);
xlim([0.005 0.26]);
hold on;
plot(xxxii,s,'g-',xxxiii,d,'r-',xxxiv,z,'k-',xxxv,q,'y-')
plot(mean(table2array(all3_r1(:,3))),0,'xb','markersize',20)
plot(mean(table2array(all3_r2(:,3))),0,'xg','markersize',20)
plot(mean(table2array(all3_r3(:,3))),0,'xr','markersize',20)
plot(mean(table2array(all3_r4(:,3))),0,'xk','markersize',20)
plot(mean(table2array(all3_post(:,3))),0,'xy','markersize',20)
legend({'Initial density(All 3)','Post-round 1 density(All 3)','Post-round 2 density(All 3)','Post-round 3 density(All 3)','Final density'},'Location','northeast','Orientation','vertical','Fontsize',10)
hold off;
xlabel('$\gamma$ values')
ylabel('Probability density')

[s,xxxii] = ksdensity(table2array(all3_r2(:,7)),'Bandwidth',0.4);
[d,xxxiii] = ksdensity(table2array(all3_r3(:,7)),'Bandwidth',0.4);
[z,xxxiv] = ksdensity(table2array(all3_r4(:,7)),'Bandwidth',0.4);
[q,xxxv] = ksdensity(table2array(all3_post(:,7)),'Bandwidth',0.4);

figure
yline(1/5.5,'b-','Linewidth',3.5);
xlim([3.5 9]);
hold on;
plot(xxxii,s,'g-',xxxiii,d,'r-',xxxiv,z,'k-',xxxv,q,'y-')
plot(mean(table2array(all3_r1(:,7))),0,'xb','markersize',20)
plot(mean(table2array(all3_r2(:,7))),0,'xg','markersize',20)
plot(mean(table2array(all3_r3(:,7))),0,'xr','markersize',20)
plot(mean(table2array(all3_r4(:,7))),0,'xk','markersize',20)
plot(mean(table2array(all3_post(:,7))),0,'xy','markersize',20)
legend({'Initial density(All 3)','Post-round 1 density(All 3)','Post-round 2 density(All 3)','Post-round 3 density(All 3)','Final density'},'Location','northeast','Orientation','vertical','Fontsize',10)
hold off;
xlabel('$r_n$ values')
ylabel('Probability density')

%% Initial and final densities only of the parameters
[k,l] = ksdensity(table2array(all3_post(:,4)),'Bandwidth',0.3014);

figure
yline(1/11,'b','Linewidth',3.5);
xlim([7 18]);
hold on;
plot(l,k,'k-')
%plot(10,0,'xr','markersize',20)
xline(10,'--r','Linewidth',3.5);
plot(mean(table2array(ecm_r1(:,4))),0,'xb','markersize',20)
plot(mean(table2array(all3_post(:,4))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density','Final density','True value'},'Location','northeast','Orientation','vertical','Fontsize',15);
xlabel('Mean ($\hat{\eta}$)')
ylabel('Probability density')

[k,l] = ksdensity(table2array(all3_post(:,5)),'Bandwidth',0.000902);

figure
yline(1/0.0329,'b','Linewidth',3.5);
xlim([0.0001 0.033]);
hold on;
plot(l,k,'k-')
%plot(0.01,0,'xr','markersize',20)
xline(0.01,'--r','Linewidth',3.5);
plot(mean(table2array(ecm_mde_r1(:,5))),0,'xb','markersize',20)
plot(mean(table2array(all3_post(:,5))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density','Final density','True value'},'Location','northeast','Orientation','vertical','Fontsize',15);
xlabel('Mean ($\hat{d_m}$)')
ylabel('Probability density')

[k,l] = ksdensity(table2array(all3_post(:,6)),'Bandwidth',0.003034);

figure
yline(1/0.11,'b','Linewidth',3.5);
xlim([0.07 0.18]);
hold on;
plot(l,k,'k-')
%plot(0.1,0,'xr','markersize',20)
xline(0.1,'--r','Linewidth',3.5);
plot(mean(table2array(ecm_mde_r1(:,6))),0,'xb','markersize',20)
plot(mean(table2array(all3_post(:,6))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density','Final density','True value'},'Location','northeast','Orientation','vertical','Fontsize',15);
xlabel('Mean ($\hat{\alpha}$)')
ylabel('Probability density')

[k,l] = ksdensity(table2array(all3_post(:,2)),'Bandwidth',0.0028);

figure
yline(1/0.019931,'b','Linewidth',3.5);
xlim([0.000069 0.02]);
hold on;
plot(l,k,'k-')
%plot(0.01,0,'xr','markersize',20)
xline(0.01,'--r','Linewidth',3.5);
plot(mean(table2array(all3_r1(:,2))),0,'xb','markersize',20)
plot(mean(table2array(all3_post(:,2))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density','Final density','True value'},'Location','northeast','Orientation','vertical','Fontsize',15);
xlabel('Mean ($\hat{d_n}$)')
ylabel('Probability density')

[k,l] = ksdensity(table2array(all3_post(:,3)),'Bandwidth',0.00697);

figure
yline(1/0.255,'b','Linewidth',3.5);
xlim([0.005 0.26]);
hold on;
plot(l,k,'k-')
%plot(0.05,0,'xr','markersize',20)
xline(0.05,'--r','Linewidth',3.5);
plot(mean(table2array(all3_r1(:,3))),0,'xb','markersize',20)
plot(mean(table2array(all3_post(:,3))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density','Final density','True value'},'Location','northeast','Orientation','vertical','Fontsize',15);
xlabel('Mean ($\hat{\gamma}$)')
ylabel('Probability density')

[k,l] = ksdensity(table2array(all3_post(:,7)),'Bandwidth',0.4);

figure
yline(1/5.5,'b','Linewidth',3.5);
xlim([3.5 9]);
hold on;
plot(l,k,'k-')
%plot(5,0,'xr','markersize',20)
xline(5,'--r','Linewidth',3.5);
plot(mean(table2array(all3_r1(:,7))),0,'xb','markersize',20)
plot(mean(table2array(all3_post(:,7))),0,'xk','markersize',20)
hold off;
lgd = legend({'Initial density','Final density','True value'},'Location','northeast','Orientation','vertical','Fontsize',15);
xlabel('Mean ($\hat{r_n}$)')
ylabel('Probability density')

%% ECM & MDE rounds: density plot of eta
[k,xxi] = ksdensity(table2array(ecm_mde_r1(:,4)),'Bandwidth',0.3014);
[s,xxii] = ksdensity(table2array(ecm_mde_r2(:,4)),'Bandwidth',0.3014);
[d,xxiii] = ksdensity(table2array(ecm_mde_r3(:,4)),'Bandwidth',0.3014);
[z,xxiv] = ksdensity(table2array(ecm_mde_post(:,4)),'Bandwidth',0.3014);

figure
plot(xxi,k,'b-')
xlim([7 18])
hold on;
plot(xxii,s,'g-',xxiii,d,'r-',xxiv,z,'k-')
plot(mean(table2array(ecm_mde_r1(:,4))),0,'xb','markersize',20)
plot(mean(table2array(ecm_mde_r2(:,4))),0,'xg','markersize',20)
plot(mean(table2array(ecm_mde_r3(:,4))),0,'xr','markersize',20)
plot(mean(table2array(ecm_mde_post(:,4))),0,'xk','markersize',20)
hold off;
legend({'Round 1 density(ECM&MDE)','Post-round 1 density(ECM&MDE)','Post-round 2 density(ECM&MDE)','Post-round 3 density(ECM&MDE)'},'Location','northeast','Orientation','vertical','Fontsize',10)
xlabel('$\eta$ values')
ylabel('Probability density')

%% Tumour cells & ECM & MDE: density plot of eta
[k,xxxi] = ksdensity(table2array(all3_r1(:,4)),'Bandwidth',0.3014);
[s,xxxii] = ksdensity(table2array(all3_r2(:,4)),'Bandwidth',0.3014);
[d,xxxiii] = ksdensity(table2array(all3_r3(:,4)),'Bandwidth',0.3014);
[z,xxxiv] = ksdensity(table2array(all3_r4(:,4)),'Bandwidth',0.3014);
[q,xxxv] = ksdensity(table2array(all3_post(:,4)),'Bandwidth',0.3014);

figure
plot(xxxi,k,'b-')
xlim([7 18]);
hold on;
plot(xxxii,s,'g-',xxxiii,d,'r-',xxxiv,z,'k-',xxxv,q,'y-')
plot(mean(table2array(all3_r1(:,4))),0,'xb','markersize',20)
plot(mean(table2array(all3_r2(:,4))),0,'xg','markersize',20)
plot(mean(table2array(all3_r3(:,4))),0,'xr','markersize',20)
plot(mean(table2array(all3_r4(:,4))),0,'xk','markersize',20)
plot(mean(table2array(all3_post(:,4))),0,'xy','markersize',20)
legend({'Round 1 density(All 3)','Post-round 1 density(All 3)','Post-round 2 density(All 3)','Post-round 3 density(All 3)','Final density'},'Location','northeast','Orientation','vertical','Fontsize',10)
hold off;
xlabel('$\eta$ values')
ylabel('Probability density')

%% Tumour cells & ECM & MDE rounds: density plot of dm 
[k,xxxi] = ksdensity(table2array(all3_r1(:,5)),'Bandwidth',0.000902);
[s,xxxii] = ksdensity(table2array(all3_r2(:,5)),'Bandwidth',0.000902);
[d,xxxiii] = ksdensity(table2array(all3_r3(:,5)),'Bandwidth',0.000902);
[z,xxxiv] = ksdensity(table2array(all3_r4(:,5)),'Bandwidth',0.000902);
[q,xxxv] = ksdensity(table2array(all3_post(:,5)),'Bandwidth',0.000902);

figure
plot(xxxi,k,'b-')
xlim([0.0001 0.033]);
hold on;
plot(xxxii,s,'g-',xxxiii,d,'r-',xxxiv,z,'k-',xxxv,q,'y-')
plot(mean(table2array(all3_r1(:,5))),0,'xb','markersize',20)
plot(mean(table2array(all3_r2(:,5))),0,'xg','markersize',20)
plot(mean(table2array(all3_r3(:,5))),0,'xr','markersize',20)
plot(mean(table2array(all3_r4(:,5))),0,'xk','markersize',20)
plot(mean(table2array(all3_post(:,5))),0,'xy','markersize',20)
legend({'Round 1 density(All 3)','Post-round 1 density(All 3)','Post-round 2 density(All 3)','Post-round 3 density(All 3)','Final density'},'Location','northeast','Orientation','vertical','Fontsize',10)
hold off;
xlabel('$d_m$ values')
ylabel('Probability density')

%% Tumour cells & ECM & MDE rounds: density plot of alpha
[k,xxxi] = ksdensity(table2array(all3_r1(:,6)),'Bandwidth',0.003034);
[s,xxxii] = ksdensity(table2array(all3_r2(:,6)),'Bandwidth',0.003034);
[d,xxxiii] = ksdensity(table2array(all3_r3(:,6)),'Bandwidth',0.003034);
[z,xxxiv] = ksdensity(table2array(all3_r4(:,6)),'Bandwidth',0.003034);
[q,xxxv] = ksdensity(table2array(all3_post(:,6)),'Bandwidth',0.003034);

figure
plot(xxxi,k,'b-')
xlim([0.07 0.18]);
hold on;
plot(xxxii,s,'g-',xxxiii,d,'r-',xxxiv,z,'k-',xxxv,q,'y-')
plot(mean(table2array(all3_r1(:,6))),0,'xb','markersize',20)
plot(mean(table2array(all3_r2(:,6))),0,'xg','markersize',20)
plot(mean(table2array(all3_r3(:,6))),0,'xr','markersize',20)
plot(mean(table2array(all3_r4(:,6))),0,'xk','markersize',20)
plot(mean(table2array(all3_post(:,6))),0,'xy','markersize',20)
legend({'Round 1 density(All 3)','Post-round 1 density(All 3)','Post-round 2 density(All 3)','Post-round 3 density(All 3)','Final density'},'Location','northeast','Orientation','vertical','Fontsize',10)
hold off;
xlabel('$\alpha$ values')
ylabel('Probability density')