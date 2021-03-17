rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_ecm_mde_r4"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.ecm.mde.r4 <- as.matrix(read.table("Round 4 parameters 10000 ecm_mde.txt", 
                                         sep = "", header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.mde.r4[i,], paras.ind = "ecm_mde")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_4_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1081.48 sec elapsed.

write.table(ests, "bcd_ecm_mde_r4.txt")

bcd.ecm.mde.r4 <- unname(ests)
ind.nan.ecm.mde.r4 <- which(is.na(bcd.ecm.mde.r4[,2]))
bcd.ecm.mde.r4.valid <- bcd.ecm.mde.r4[-ind.nan.ecm.mde.r4,]
mean(bcd.ecm.mde.r4.valid[,2]) # 0.1184702
min(bcd.ecm.mde.r4.valid[,2]) # 0.009608336

# Stopping criterion has been met!