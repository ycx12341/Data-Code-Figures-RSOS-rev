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

# 944.29 sec elapsed.

write.table(ests, "bcd_all3_r6.txt")

bcd.all3.r6 <- unname(ests)
# ind.nan.all3.r5 <- which(is.na(bcd.all3.r5[,2]))
# bcd.all3.r5.valid <- bcd.all3.r5[-ind.nan.all3.r5,]
mean(bcd.all3.r6[,2]) # 0.04530925
min(bcd.all3.r6[,2]) # 0.02862773

(3.905071 - 0.04530925) / 3.905071 * 100
# Reduced by 98.83%, stopping criteria has been met! 

# Post process
paras.final <- apply(paras.all3.r6, 2, mean)
# 0.008080895  0.079538139 12.769533225  0.018653482  0.130305963  7.075813689 
paras.ref <- c(0.008, 0.08, 13, 0.018, 0.13, 7)
(paras.final - paras.ref) / paras.ref * 100
# 1.0111866 -0.5773268 -1.7728213  3.6304548  0.2353558  1.0830527 
