rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_all3_r6"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.all3.r6 <- as.matrix(read.table("Round 6 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r6[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_6_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 935.28 sec elapsed.

write.table(ests, "bcd_all3_r6.txt")

bcd.all3.r6 <- unname(ests)
mean(bcd.all3.r6[,2]) # 0.05533583
min(bcd.all3.r6[,2]) # 0.02974412

(3.868769 - 0.05533583) / 3.868769 * 100
# Reduced by 98.57%, stopping criteria has been met! 

# Post process
paras.final <- apply(paras.all3.r6, 2, mean)
# 0.00865443  0.07777404 12.97844721  0.01883343  0.12992500  6.88988081 
paras.ref <- c(0.008, 0.08, 13, 0.018, 0.13, 7)
(paras.final - paras.ref) / paras.ref * 100
# 8.18037185 -2.78244877 -0.16579068  4.63015001 -0.05769609 -1.57313125 
