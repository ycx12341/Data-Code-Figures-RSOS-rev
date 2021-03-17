rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_all3_r5"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.all3.r5 <- as.matrix(read.table("Round 5 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r5[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_5_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1106 sec elapsed. 

write.table(ests, "bcd_all3_r5.txt")

bcd.all3.r5 <- unname(ests)
ind.nan.all3.r5 <- which(is.na(bcd.all3.r5[,2]))
bcd.all3.r5.valid <- bcd.all3.r5[-ind.nan.all3.r5,]
mean(bcd.all3.r5.valid[,2]) # 0.02597522
min(bcd.all3.r5.valid[,2]) # 0.008256716

(2.942644 - 0.02597522) / 2.942644 * 100
# 99.11728

# Averaged BC distance has been reduced by 99.12%, stopping criteria has 
# been met!

paras.final.est <- apply(paras.all3.r5, 2, mean)
# 0.01262001  0.15875566 11.05031600  0.02521751  0.10953199  6.57956812 
paras.ref <- c(0.013, 0.15, 11, 0.025, 0.11, 6)
(paras.final.est - paras.ref)/paras.ref*100
# -2.9229971  5.8371039  0.4574182  0.8700265 -0.4254607  9.6594687 
