rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_ecm_r3"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.ecm.r3 <- as.matrix(read.table("Round 3 parameters 10000 ecm.txt", sep = "", 
                                     header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.ecm.r3[i,], paras.ind = "ecm")
  
  readr::write_rds(bcd.temp, 
                   path = paste0("./", save.sims.dir, "/Round_3_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1103.25 sec elapsed.

write.table(bcd.ecm.r3, "bcd_ecm_r3.txt")

bcd.ecm.r3 <- unname(ests)
ind.nan.ecm.r3 <- which(is.na(bcd.ecm.r3[,2]))
bcd.ecm.r3.valid <- bcd.ecm.r3[-ind.nan.ecm.r3,]
mean(bcd.ecm.r3.valid[,2]) # 0.3386046
min(bcd.ecm.r3.valid[,2]) # 0.007639883

(4.338274 - 0.3386046)/4.338274*100
# Stopping criteria has been met!

