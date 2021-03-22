# paras sampl ecm r4.R
# Author: Yunchen Xiao
# This .R file reads in the parameters used in round 4 of the evaluations of the 
# ECM density profile and check if the stopping criterion (75% reduction in the 
# mean summary statistics of the initial parameters) has been met.  

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
save.sims.dir <- "BCD_results_ecm_r4"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round. 
paras.ecm.r4 <- as.matrix(read.table("Round 4 parameters 10000 ecm.txt", sep = "", 
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
  bcd.temp <- bcd(paras = paras.ecm.r4[i,], paras.ind = "ecm")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_4_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)
# 1224.5 sec elapsed

write.table(ests, "bcd_ecm_r4.txt")

# Calculate and record the average summary statistics of the parameters being 
# investigated in the current round, the stopping criterion (80% reduction in 
# the mean summary statistics of the initial parameters) has been met! We 
# record the values of $\eta$ and proceed to the evaluations of the ECM + MDE
# density profiles. 
bcd.ecm.r4 <- unname(ests)
ind.nan.ecm.r4 <- which(is.na(bcd.ecm.r4[,2]))
bcd.ecm.r4.valid <- bcd.ecm.r4[-ind.nan.ecm.r4,]
mean(bcd.ecm.r4.valid[,2]) # 0.1342992
min(bcd.ecm.r4.valid[,2]) # 0.01930463
