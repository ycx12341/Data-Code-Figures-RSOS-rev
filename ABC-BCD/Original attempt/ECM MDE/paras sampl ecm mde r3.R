# paras sampl ecm mde r3.R
# Author: Yunchen Xiao

# This .R file reads in the parameters used in round 3 of the evaluations of 
# ECM+MDE density profiles and returns the parameters to be evaluated in round 
# 4.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the results in .rds files. 
save.sims.dir <- "BCD_results_ecm_mde_r3"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameter values to be evaluated in the current round.
paras.ecm.mde.r3 <- as.matrix(read.table("Round 3 parameters 10000 ecm_mde.txt", 
                                         sep = "", header = TRUE))

# Set up the parallel running scheme. 
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

# Compute the summary statistics (Bhattacharyya distance) for each parameter 
# vector in the parameter set. 
tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.mde.r3[i,], paras.ind = "ecm_mde")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_3_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1232.05 sec elapsed. 

write.table(ests, "bcd_ecm_mde_r3.txt")

# Calculate and record the average summary statistics of the parameters 
# evaluated in the current round, the stopping criterion (90% reduction in the 
# mean summary statistics of the initial parameters) has not been met yet!
bcd.ecm.mde.r3 <- unname(ests)
ind.nan.ecm.mde.r3 <- which(is.na(bcd.ecm.mde.r3[,2]))
bcd.ecm.mde.r3.valid <- bcd.ecm.mde.r3[-ind.nan.ecm.mde.r3,]
mean(bcd.ecm.mde.r3.valid[,2]) # 2.240583
min(bcd.ecm.mde.r3.valid[,2]) # 0.02306999

# Resample and record the parameter values to be evaluated in the next round.
paras.ecm.mde.r4 <- abc.bcd(ss.mat = bcd.ecm.mde.r3, paras = paras.ecm.mde.r3, bw = 1.125)
write.table(paras.ecm.mde.r4, "Round 4 parameters 10000 ecm_mde.txt")
