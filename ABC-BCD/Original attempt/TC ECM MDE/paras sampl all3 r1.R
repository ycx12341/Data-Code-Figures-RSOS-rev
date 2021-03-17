# paras sampl all3 r1.R
# Author: Yunchen Xiao

# This .R file generates the initial parameters used in the evaluations of all 
# three density profiles and returns the parameters to be evaluated in round 2.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the results in .rds files. 
save.sims.dir <- "BCD_results_all3_r1"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameter values at the end of the evaluations of the ECM+MDE 
# density profile. 
paras.ecm.mde.r4 <- as.matrix(read.table("Round 4 parameters 10000 ecm_mde.txt",
                                         sep = "", header = TRUE))

# Read in the values of $\eta$, $d_m$ and $\alpha$ from the parameter set above 
# as the initial values of $\eta$, $d_m$ and $\alpha$ in the current 
# evaluations, sample the values of the other parameters from the initial 
# distributions. 
set.seed(120)
dn<-runif(10000,0.000069,0.02)
gamma<-runif(10000,0.005,0.26)
eta<-paras.ecm.mde.r4[,3]
dm<-paras.ecm.mde.r4[,4]
alpha<-paras.ecm.mde.r4[,5]
rn<-runif(10000,3.5,9)

paras.all3.r1 <- cbind(dn,gamma,eta,dm,alpha,rn)
write.table(paras.all3.r1,"Round 1 parameters 10000 all 3.txt")

# Set up the parallel running scheme.
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

paras.all3.r1 <- as.matrix(read.table("Round 1 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

# Compute the summary statistics (Bhattacharyya distance) for each parameter 
# vector in the parameter set. 
tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r1[i,], paras.ind = "all_three")
  
  # Optional line: write the results into .rds files. 
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_1_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)
# 1132.58 sec elapsed.

write.table(ests, "bcd_all3_r1.txt")

# Calculate and record the average summary statistics of the initial parameters,
# when this value is reduced by 98%, we terminate the algorithm.
bcd.all3.r1 <- unname(ests)
ind.nan.all3.r1 <- which(is.na(bcd.all3.r1[,2]))
bcd.all3.r1.valid <- bcd.all3.r1[-ind.nan.all3.r1,]
mean(bcd.all3.r1.valid[,2]) # 3.93648
min(bcd.all3.r1.valid[,2]) # 0.02916887

# Resample and record the parameter values to be evaluated in the next round. 
paras.all3.r2 <- abc.bcd(ss.mat = bcd.all3.r1, paras = paras.all3.r1, bw = 0.5)
write.table(paras.all3.r2, "Round 2 parameters 10000 all 3.txt")
