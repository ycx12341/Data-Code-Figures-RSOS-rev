rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_ecm_mde_r3"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.ecm.mde.r3 <- as.matrix(read.table("Round 3 parameters 10000 ecm_mde.txt", 
                                         sep = "", header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.mde.r3[i,], paras.ind = "ecm_mde")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_3_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 952.27 sec elapsed. 

write.table(ests, "bcd_ecm_mde_r3.txt")

bcd.ecm.mde.r3 <- unname(ests)
ind.nan.ecm.mde.r3 <- which(is.na(bcd.ecm.mde.r3[,2]))
bcd.ecm.mde.r3.valid <- bcd.ecm.mde.r3[-ind.nan.ecm.mde.r3,]
mean(bcd.ecm.mde.r3.valid[,2]) # 1.203434
min(bcd.ecm.mde.r3.valid[,2]) # 0.03292251

paras.ecm.mde.r4 <- abc.bcd(ss.mat = bcd.ecm.mde.r3, paras = paras.ecm.mde.r3, bw = 1.125)
write.table(paras.ecm.mde.r4, "Round 4 parameters 10000 ecm_mde.txt")
