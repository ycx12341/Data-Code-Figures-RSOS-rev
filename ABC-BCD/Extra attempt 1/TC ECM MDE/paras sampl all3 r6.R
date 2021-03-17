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

# 1193.29 sec elapsed

write.table(ests, "bcd_all3_r6.txt")

bcd.all3.r6 <- unname(ests)
# ind.nan.all3.r6 <- which(is.na(bcd.all3.r6[,2]))
# bcd.all3.r5.valid <- bcd.all3.r5[-ind.nan.all3.r5,]
mean(bcd.all3.r6[,2]) # 0.05109098
min(bcd.all3.r6[,2]) # 0.02416617

(4.186601 - 0.05109098) / 4.186601 * 100
# 98.77965

# Stopping criteria has been met! 

# Post process
paras.final.est <- apply(paras.all3.r6, 2, mean)
# 0.01005766 0.05578386 9.91084206 0.01003567 0.09962235 5.49422438 
paras.ref <- c(0.01, 0.05, 10, 0.01, 0.1, 5)
(paras.final.est - paras.ref)/paras.ref*100
# 0.5766460 11.5677189 -0.8915794  0.3567452 -0.3776502  9.8844877 
