# paras sampl ecm mde r1.R
# Author: Yunchen Xiao

# This .R file generates the initial parameters used in the evaluations of 
# ECM+MDE density profiles and returns the parameters to be evaluated in 
# round 2.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the simulation results in .rds 
# files.
save.sims.dir <- "BCD_results_ecm_mde_r1"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameter values at the end of the evaluations of the ECM density 
# profile.
paras.ecm.r4 <- as.matrix(read.table("Round 4 parameters 10000 ecm.txt", 
                           sep = "", header = TRUE))

# Read in the values of $\eta$ from the parameter set above as the initial 
# values of $\eta$ in the current evaluations, sample the values of the other 
# parameters from the initial distributions. 
set.seed(121)
dn<-runif(10000,0.000069,0.02)
gamma<-runif(10000,0.005,0.26)
eta<-paras.ecm.r4[,3]
dm<-runif(10000,0.0001,0.033)
alpha<-runif(10000,0.07,0.18)
rn<-runif(10000,3.5,9)

paras.ecm.mde.r1 <- cbind(dn,gamma,eta,dm,alpha,rn)
write.table(paras.ecm.mde.r1,"Round 1 parameters 10000 ecm_mde.txt")

paras.ecm.mde.r1 <- as.matrix(read.table("Round 1 parameters 10000 ecm_mde.txt", sep = "", 
                                         header = TRUE))

# Set up the parallel running scheme.
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

# Compute the summary statistics (Bhattacharyya distance) for each parameter 
# vector in the parameter set. 
tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.mde.r1[i,], paras.ind = "ecm_mde")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_1_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)
# 1287.22 sec elapsed.

write.table(ests, "bcd_ecm_mde_r1.txt")

# Calculate and record the average summary statistics of the initial parameters,
# when this value is reduced by 90%, we move on to the evaluations of all three
# density profiles.

bcd.ecm.mde.r1 <- unname(ests)
ind.nan.ecm.mde.r1 <- which(is.na(bcd.ecm.mde.r1[,2]))
bcd.ecm.mde.r1.valid <- bcd.ecm.mde.r1[-ind.nan.ecm.mde.r1,]
mean(bcd.ecm.mde.r1.valid[,2]) # 7.092385
min(bcd.ecm.mde.r1.valid[,2]) # 0.02915356

# Resample and record the parameter values to be evaluated in the next round. 
paras.ecm.mde.r2 <- abc.bcd(ss.mat = bcd.ecm.mde.r1, paras = paras.ecm.mde.r1, bw = 0.5)
write.table(paras.ecm.mde.r2, "Round 2 parameters 10000 ecm_mde.txt")
