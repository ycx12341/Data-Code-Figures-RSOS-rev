# paras sampl ecm mde r4.R
# Author: Yunchen Xiao

# This .R file reads in the parameters used in round 4 of the evaluations of 
# ECM+MDE density profiles and check if the stopping criterion has been met.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the results in .rds files. 
save.sims.dir <- "BCD_results_ecm_mde_r4"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameter values to be evaluated in the current round.
paras.ecm.mde.r4 <- as.matrix(read.table("Round 4 parameters 10000 ecm_mde.txt", 
                                         sep = "", header = TRUE))

# Set up the parallel running scheme. 
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.mde.r4[i,], paras.ind = "ecm_mde")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_4_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1293.11 sec elapsed.

write.table(ests, "bcd_ecm_mde_r4.txt")

# Calculate and record the average summary statistics of the parameters 
# evaluated in the current round, the stopping criterion (90% reduction in the 
# mean summary statistics of the initial parameters) has been met! We record the
# values of $\eta$, $d_m$ and $\alpha$ and proceed to the evaluations of all
# three density profiles!

bcd.ecm.mde.r4 <- unname(ests)
ind.nan.ecm.mde.r4 <- which(is.na(bcd.ecm.mde.r4[,2]))
bcd.ecm.mde.r4.valid <- bcd.ecm.mde.r4[-ind.nan.ecm.mde.r4,]
mean(bcd.ecm.mde.r4.valid[,2]) # 0.2982225
min(bcd.ecm.mde.r4.valid[,2]) # 0.02306999
