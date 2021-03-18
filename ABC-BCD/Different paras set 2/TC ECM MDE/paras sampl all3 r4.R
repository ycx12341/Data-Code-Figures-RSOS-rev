rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)

source("Automatic.R")

save.sims.dir <- "BCD_results_all3_r4"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

paras.all3.r4 <- as.matrix(read.table("Round 4 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r4[i,], paras.ind = "all_three")
  
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_4_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1110.36 sec elapsed.

write.table(ests, "bcd_all3_r4.txt")

bcd.all3.r4 <- unname(ests)
ind.nan.all3.r4 <- which(is.na(bcd.all3.r4[,2]))
bcd.all3.r4.valid <- bcd.all3.r4[-ind.nan.all3.r4,]
mean(bcd.all3.r4.valid[,2]) # 0.05367243
min(bcd.all3.r4.valid[,2]) # 0.009342805

(2.968427 - 0.05367243) / 2.968427
# [1] 0.9819189

# Stoppiing criterion has been reached!
paras.ref <- c(0.013, 0.15, 11, 0.025, 0.11, 6)
paras.final <- apply(paras.all3.r4, 2, mean)

(paras.final- paras.ref) / paras.ref * 100
# 0.97802714 4.00049248 0.04309156 2.05991111 0.01234512 6.40896650 
