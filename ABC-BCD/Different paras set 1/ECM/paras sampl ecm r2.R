rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_ecm_r2"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.ecm.r2 <- as.matrix(read.table("Round 2 parameters 10000 ecm.txt", sep = "", 
                                     header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.r2[i,], paras.ind = "ecm")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_2_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1110.31 sec elapsed 

write.table(ests, "bcd_ecm_r2.txt")

bcd.ecm.r2 <- unname(ests)
ind.nan.ecm.r2 <- which(is.na(bcd.ecm.r2[,2]))
bcd.ecm.r2.valid <- bcd.ecm.r2[-ind.nan.ecm.r2,]
mean(bcd.ecm.r2.valid[,2]) # 1.413815
min(bcd.ecm.r2.valid[,2]) # 0.02746065

paras.ecm.r3 <- abc.bcd(ss.mat = bcd.ecm.r2, paras = paras.ecm.r2, bw = 0.75)
write.table(paras.ecm.r3, "Round 3 parameters 10000 ecm.txt")
