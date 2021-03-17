# paras sampl ecm r1.R
# Author: Yunchen Xiao
# This .R file generates the initial parameters used in the evaluations of the 
# ECM density profile and returns the parameters to be evaluated in round 2. 

# Clear the current workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# source functions
source("Automatic.R")

# Set the directory to store the simulation results
save.sims.dir <- "BCD_results_ecm_r1"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Generate the initial parameters used in the evaluations of ECM density 
# profile.
set.seed(123)
dn<-runif(10000,0.000069,0.02)
gamma<-runif(10000,0.005,0.26)
eta<-runif(10000,7,18)
dm<-runif(10000,0.0001,0.033)
alpha<-runif(10000,0.07,0.18)
rn<-runif(10000,3.5,9)

paras.ecm.r1 <- cbind(dn,gamma,eta,dm,alpha,rn)
write.table(paras.ecm.r1,"Round 1 parameters 10000 ecm.txt")

paras.ecm.r1 <- as.matrix(read.table("Round 1 parameters 10000 ecm.txt", sep = "", 
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
  bcd.temp <- bcd(paras = paras.ecm.r1[i,], paras.ind = "ecm")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_1_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)
# 1150.01 sec elapsed.

write.table(ests, "bcd_ecm_r1.txt")

# Calculate and record the average summary statistics of the initial parameters,
# when this value is reduced by 75%, we move on to the evaluations of ECM+MDE
# density profiles.

bcd.ecm.r1 <- unname(ests)
ind.nan.ecm.r1 <- which(is.na(bcd.ecm.r1[,2]))
bcd.ecm.r1.valid <- bcd.ecm.r1[-ind.nan.ecm.r1,]
mean(bcd.ecm.r1.valid[,2]) # 3.408093
min(bcd.ecm.r1.valid[,2]) # 0.02653616

# Resample and record the parameters to be evaluated in the next round.
paras.ecm.r2 <- abc.bcd(ss.mat = bcd.ecm.r1, paras = paras.ecm.r1, bw = 0.5)
write.table(paras.ecm.r2, "Round 2 parameters 10000 ecm.txt")
