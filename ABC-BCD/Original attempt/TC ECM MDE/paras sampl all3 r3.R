# paras sampl all3 r3.R
# Author: Yunchen Xiao

# This .R file reads in the parameters used in round 3 of the evaluations of all 
# three density profiles and returns the parameters to be evaluated in round 4.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the results in .rds files.
save.sims.dir <- "BCD_results_all3_r3"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round.
paras.all3.r3 <- as.matrix(read.table("Round 3 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

# Set up the parallel running scheme. 
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r3[i,], paras.ind = "all_three")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_3_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1139.17 sec elapsed.

write.table(ests, "bcd_all3_r3.txt")

# Calculate and record the average summary statistics of the parameters in the 
# current round, the stopping criterion (98% reduction in the average summary
# statistics of the initial parameters) has not been met yet!
bcd.all3.r3 <- unname(ests)
ind.nan.all3.r3 <- which(is.na(bcd.all3.r3[,2]))
bcd.all3.r3.valid <- bcd.all3.r3[-ind.nan.all3.r3,]
mean(bcd.all3.r3.valid[,2]) # 0.7146851
min(bcd.all3.r3.valid[,2]) # 0.02384367

# Resample and record the parameter values to be evaluated in the next round. 
paras.all3.r4 <- abc.bcd(ss.mat = bcd.all3.r3, paras = paras.all3.r3, bw = 1.125)
write.table(paras.all3.r4, "Round 4 parameters 10000 all 3.txt")
