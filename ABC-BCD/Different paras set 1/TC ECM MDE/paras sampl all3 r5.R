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

# 897 sec elapsed.

write.table(ests, "bcd_all3_r5.txt")

bcd.all3.r5 <- unname(ests)
mean(bcd.all3.r5[,2]) # 0.07989762
min(bcd.all3.r5[,2]) # 0.0303578

paras.all3.r6 <- abc.bcd(ss.mat = bcd.all3.r5, paras = paras.all3.r5, bw = 2.53125)
write.table(paras.all3.r6, "Round 6 parameters 10000 all 3.txt")
