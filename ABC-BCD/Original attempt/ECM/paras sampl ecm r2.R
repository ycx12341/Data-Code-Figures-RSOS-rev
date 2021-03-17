# paras sampl ecm r2.R
# Author: Yunchen Xiao
# This .R file reads in the parameters used in round 2 of the evaluations of 
# the ECM density profile and returns the parameters to be evaluated in round 3. 

# Clear the current workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the simulation results in .rds
# files. 
save.sims.dir <- "BCD_results_ecm_r2"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round. 
paras.ecm.r2 <- as.matrix(read.table("Round 2 parameters 10000 ecm.txt", sep = "", 
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
  bcd.temp <- bcd(paras = paras.ecm.r2[i,], paras.ind = "ecm")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_2_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1156.42 sec elapsed

write.table(ests, "bcd_ecm_r2.txt")

# Calculate and record the average summary statistics of the parameters being 
# investigated in the current round, the stopping criterion (75% reduction in 
# the mean summary statistics of the initial parameters) has not been met
# yet! 
bcd.ecm.r2 <- unname(ests)
ind.nan.ecm.r2 <- which(is.na(bcd.ecm.r2[,2]))
bcd.ecm.r2.valid <- bcd.ecm.r2[-ind.nan.ecm.r2,]
mean(bcd.ecm.r2.valid[,2]) # 2.318951
min(bcd.ecm.r2.valid[,2]) # 0.02194195

# Resample and record the parameters to be evaluated in the next round.
paras.ecm.r3 <- abc.bcd(ss.mat = bcd.ecm.r2, paras = paras.ecm.r2, bw = 0.75)
write.table(paras.ecm.r3, "Round 3 parameters 10000 ecm.txt")
