# paras sampl all3 r4.R
# Author: Yunchen Xiao

# This .R file reads in the parameters used in round 4 of the evaluations of all 
# three density profiles and returns the parameters to be evaluated in round 5.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the results in .rds files.
save.sims.dir <- "BCD_results_all3_r4"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round.
paras.all3.r4 <- as.matrix(read.table("Round 4 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

# Set up the parallel running scheme. 
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r4[i,], paras.ind = "all_three")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_4_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1048.62 sec elapsed.

write.table(ests, "bcd_all3_r4.txt")

# Calculate and record the average summary statistics of the parameters in the 
# current round, the stopping criterion (98% reduction in the average summary
# statistics of the initial parameters) has not been met yet!
bcd.all3.r4 <- unname(ests)
ind.nan.all3.r4 <- which(is.na(bcd.all3.r4[,2]))
bcd.all3.r4.valid <- bcd.all3.r4[-ind.nan.all3.r4,]
mean(bcd.all3.r4.valid[,2]) # 0.1822057
min(bcd.all3.r4.valid[,2]) # 0.02328316

# Resample and record the parameter values to be evaluated in the next round. 
paras.all3.r5 <- abc.bcd(ss.mat = bcd.all3.r4, paras = paras.all3.r4, bw = 1.6875)
write.table(paras.all3.r5, "Round 5 parameters 10000 all 3.txt")
