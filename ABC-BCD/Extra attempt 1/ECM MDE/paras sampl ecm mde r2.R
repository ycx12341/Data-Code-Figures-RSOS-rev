rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_ecm_mde_r2"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.ecm.mde.r2 <- as.matrix(read.table("Round 2 parameters 10000 ecm_mde.txt", 
                                         sep = "", header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.mde.r2[i,], paras.ind = "ecm_mde")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_2_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 932.78 sec elapsed

write.table(ests, "bcd_ecm_mde_r2.txt")

bcd.ecm.mde.r2 <- unname(ests)
ind.nan.ecm.mde.r2 <- which(is.na(bcd.ecm.mde.r2[,2]))
bcd.ecm.mde.r2.valid <- bcd.ecm.mde.r2[-ind.nan.ecm.mde.r2,]
mean(bcd.ecm.mde.r2.valid[,2]) # 4.774174
min(bcd.ecm.mde.r2.valid[,2]) # 0.02469644

paras.ecm.mde.r3 <- abc.bcd(ss.mat = bcd.ecm.mde.r2, paras = paras.ecm.mde.r2, bw = 0.75)
write.table(paras.ecm.mde.r3, "Round 3 parameters 10000 ecm_mde.txt")
