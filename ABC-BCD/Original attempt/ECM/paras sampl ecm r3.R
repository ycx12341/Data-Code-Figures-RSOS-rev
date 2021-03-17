# paras sampl ecm r3.R
# Author: Yunchen Xiao
# This .R file reads in the parameters used in round 3 of the evaluations of the
# ECM density profile and returns the parameters to be evaluated in round 4 at 
# the end. 

# Clear the current workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Set the directory to store the simulation results.
save.sims.dir <- "BCD_results_ecm_r3"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round. 
paras.ecm.r3 <- as.matrix(read.table("Round 3 parameters 10000 ecm.txt", sep = "", 
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
  bcd.temp <- bcd(paras = paras.ecm.r3[i,], paras.ind = "ecm")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_3_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1152.79 sec elapsed.

write.table(ests, "bcd_ecm_r3.txt")

# Calculate and record the average summary statistics of the parameters being 
# investigated in the current round, the stopping criterion (75% reduction in
# the mean summary statistics of the initial parameters) has not been met
# yet! 

bcd.ecm.r3 <- unname(ests)
ind.nan.ecm.r3 <- which(is.na(bcd.ecm.r3[,2]))
bcd.ecm.r3.valid <- bcd.ecm.r3[-ind.nan.ecm.r3,]
mean(bcd.ecm.r3.valid[,2]) # 0.8832961
min(bcd.ecm.r3.valid[,2]) # 0.01971626

# Resample and record the parameters to be evaluated in the next round.
paras.ecm.r4 <- abc.bcd(ss.mat = bcd.ecm.r3, paras = paras.ecm.r3, bw = 1.125)
write.table(paras.ecm.r4, "Round 4 parameters 10000 ecm.txt")
