rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_all3_r1"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.ecm.mde.r4 <- as.matrix(read.table("Round 4 parameters 10000 ecm_mde.txt",
                                         sep = "", header = TRUE))

set.seed(874511)
dn<-runif(10000,0.000069,0.02)
gamma<-runif(10000,0.005,0.26)
eta<-paras.ecm.mde.r4[,3]
dm<-paras.ecm.mde.r4[,4]
alpha<-paras.ecm.mde.r4[,5]
rn<-runif(10000,3.5,9)

paras.all3.r1 <- cbind(dn,gamma,eta,dm,alpha,rn)
write.table(paras.all3.r1,"Round 1 parameters 10000 all 3.txt")

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

paras.all3.r1 <- as.matrix(read.table("Round 1 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r1[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_1_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)
# 1074.72 sec elapsed.

write.table(ests, "bcd_all3_r1.txt")

bcd.all3.r1 <- unname(ests)
ind.nan.all3.r1 <- which(is.na(bcd.all3.r1[,2]))
bcd.all3.r1.valid <- bcd.all3.r1[-ind.nan.all3.r1,]
mean(bcd.all3.r1.valid[,2]) # 2.968427
min(bcd.all3.r1.valid[,2]) # 0.01378707

paras.all3.r2 <- abc.bcd(ss.mat = bcd.all3.r1, paras = paras.all3.r1, bw = 0.5)
write.table(paras.all3.r2, "Round 2 parameters 10000 all 3.txt")
