rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_all3_r3"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.all3.r3 <- as.matrix(read.table("Round 3 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r3[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_3_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 986.65 sec elapsed.

write.table(ests, "bcd_all3_r3.txt")

bcd.all3.r3 <- unname(ests)
ind.nan.all3.r3 <- which(is.na(bcd.all3.r3[,2]))
bcd.all3.r3.valid <- bcd.all3.r3[-ind.nan.all3.r3,]
mean(bcd.all3.r3.valid[,2]) # 0.8780164
min(bcd.all3.r3.valid[,2]) # 0.02881669

paras.all3.r4 <- abc.bcd(ss.mat = bcd.all3.r3, paras = paras.all3.r3, bw = 1.125)
write.table(paras.all3.r4, "Round 4 parameters 10000 all 3.txt")
