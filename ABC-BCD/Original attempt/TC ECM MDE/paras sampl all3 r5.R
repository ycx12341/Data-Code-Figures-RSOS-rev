# paras sampl all3 r5.R
# Author: Yunchen Xiao

# This .R file reads in the parameters used in round 5 of the evaluations of all 
# three density profiles and checks if the stopping criterion has been met.

# Clear the workspace and load the necessary packages.
rm(list = ls())
library(doParallel)
library(doRNG)
library(tictoc)
library(readr)
library(reshape2)
library(ggplot2)
library(latex2exp)

# Source functions.
source("Automatic.R")

# Optional line: set the directory to store the results in .rds files. 
save.sims.dir <- "BCD_results_all3_r5"
save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(save.sims.dir)) dir.create(save.sims.dir)
}

# Read in the parameters to be evaluated in the current round.
paras.all3.r5 <- as.matrix(read.table("Round 5 parameters 10000 all 3.txt", sep = "",
                                      header = TRUE))

# Set up the parallel running scheme. 
n.thread <- detectCores() - 1
n.sims <- 10000
cl <- makeCluster(n.thread)
registerDoParallel(cl)

tic()
ests <- foreach (i = 1:n.sims, .combine = rbind) %dopar% {
  bcd.temp <- bcd(paras = paras.all3.r5[i,], paras.ind = "all_three")
  
  # Optional line: store the results into .rds files.
  readr::write_rds(bcd.temp,
                   path = paste0("./", save.sims.dir, "/Round_5_paras", i, "_res.rds"))
  
  c(i, bcd.temp)
}
toc()

stopCluster(cl)

# 1037.9 sec elapsed. 

write.table(ests, "bcd_all3_r5.txt")

# Calculate and record the average summary statistics of the parameters in the 
# current round, the stopping criterion (98% reduction in the average summary
# statistics of the initial parameters) has been met! The algorithm is now 
# terminated.
bcd.all3.r5 <- unname(ests)
mean(bcd.all3.r5[,2]) # 0.074475
min(bcd.all3.r5[,2]) # 0.02210361

(0.074475 - 3.93648)/3.93648*100
# -98.10808

paras.final.est <- apply(paras.all3.r5, 2, mean)
# 0.01023104 0.05184949 9.90794860 0.01037480 0.09974715 5.23125692 
paras.ref <- c(0.01, 0.05, 10, 0.01, 0.1, 5)
(paras.final.est - paras.ref)/paras.ref*100
#  2.3104442  3.6989715 -0.9205140  3.7480423 -0.2528469  4.6251384 

# Mean square errors
dn.mse <- mean(((paras.all3.r5[,1] - 0.01)^2))   # 0.0000137407
ga.mse <- mean(((paras.all3.r5[,2] - 0.05)^2))   # 0.0001427460
eta.mse <- mean(((paras.all3.r5[,3] - 10)^2))    # 0.0460623711
dm.mse <- mean(((paras.all3.r5[,4] - 0.01)^2))   # 0.0000004821
alpha.mse <- mean(((paras.all3.r5[,1] - 0.1)^2)) # 0.0080721527
rn.mse <- mean(((paras.all3.r5[,6] - 5)^2))      # 0.8537346387

# Pairwise heatmap
greek.letters <- c(alpha='\u03b1', gamma='\u03b3', eta = '\u03b7')
colnames(paras.all3.r5) <- paste0(c("dn", greek.letters['gamma'], greek.letters['eta'],
                                    "dm", greek.letters['alpha'], "rn" ))
cormat.paras.final <- cor(paras.all3.r5)
cormat.paras.final[lower.tri(cormat.paras.final)] <- NA
melted.cormat <- melt(cormat.paras.final, na.rm = TRUE)
colnames(melted.cormat) <- c("Parameter", "Parameters", "value")

library(ggplot2)
pw.heatmap <- ggplot(data = melted.cormat, aes(Parameter, Parameters, fill = value)) +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

print(pw.heatmap)

pw.heatmap + 
  geom_text(aes(Parameter, Parameters, label = round(value,2)), color = "black", size = 7)
