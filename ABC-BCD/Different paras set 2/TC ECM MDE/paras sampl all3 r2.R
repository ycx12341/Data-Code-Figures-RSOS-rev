rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_all3_r2"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.all3.r2 <- as.matrix(read.table("Round 2 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r2[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_2_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1155.37 sec elapsed. 

write.table(ests, "bcd_all3_r2.txt")

bcd.all3.r2 <- unname(ests)
ind.nan.all3.r2 <- which(is.na(bcd.all3.r2[,2]))
bcd.all3.r2.valid <- bcd.all3.r2[-ind.nan.all3.r2,]
mean(bcd.all3.r2.valid[,2]) # 1.417053
min(bcd.all3.r2.valid[,2]) # 0.01061055

paras.all3.r3 <- abc.bcd(ss.mat = bcd.all3.r2, paras = paras.all3.r2, bw = 0.75)
write.table(paras.all3.r3, "Round 3 parameters 10000 all 3.txt")
