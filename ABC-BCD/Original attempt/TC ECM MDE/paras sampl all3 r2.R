# paras sampl all3 r2.R
# Author: Yunchen Xiao

# This .R file reads in the parameters of round 2 used in the evaluations of all 
# three density profiles and returns the parameters to be evaluated in round 3.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Set the directory to store the results.
save.sims.dir <- "BCD_results_all3_r2"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round.
paras.all3.r2 <- as.matrix(read.table("Round 2 parameters 10000 all 3.txt", sep = "",
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
  bcd.temp <- bcd(paras = paras.all3.r2[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_2_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1118.98 sec elapsed. 

write.table(ests, "bcd_all3_r2.txt")

# Calculate and record the average summary statistics of the parameters in the 
# current round, the stopping criterion (98% reduction in the average summary
# statistics of the initial parameters) has not been met yet!
bcd.all3.r2 <- unname(ests)
ind.nan.all3.r2 <- which(is.na(bcd.all3.r2[,2]))
bcd.all3.r2.valid <- bcd.all3.r2[-ind.nan.all3.r2,]
mean(bcd.all3.r2.valid[,2]) # 2.184187
min(bcd.all3.r2.valid[,2]) # 0.02530491

# Resample and record the parameter values to be evaluated in the next round. 
paras.all3.r3 <- abc.bcd(ss.mat = bcd.all3.r2, paras = paras.all3.r2, bw = 0.75)
write.table(paras.all3.r3, "Round 3 parameters 10000 all 3.txt")
