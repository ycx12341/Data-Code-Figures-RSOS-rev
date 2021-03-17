# Consistency check.R
# Author: Yunchen Xiao
# This .R file assesses the consistency of the parameter estimates obtained on
# the main reference dataset by calculating the Monte-Carlo errors.  
rm(list = ls())

paras.final.ori <- as.matrix(read.table("Round 5 parameters 10000 all 3 ori.txt", 
                                        sep = "", header = TRUE))

paras.final.ex1 <- as.matrix(read.table("Round 6 parameters 10000 all 3 ex1.txt", 
                                        sep = "", header = TRUE))

paras.final.ex2 <- as.matrix(read.table("Round 5 parameters 10000 all 3 ex2.txt", 
                                        sep = "", header = TRUE))

paras.mean.ori <- apply(paras.final.ori, 2 ,mean)
paras.mean.ex1 <- apply(paras.final.ex1, 2, mean)
paras.mean.ex2 <- apply(paras.final.ex2, 2, mean)

paras.mean.mat <- rbind(paras.mean.ori, paras.mean.ex1, 
                        paras.mean.ex2)

# MC errors:
sd(paras.mean.mat[,1])/sqrt(3) # 7.887399e-05
sd(paras.mean.mat[,2])/sqrt(3) # 0.00129537
sd(paras.mean.mat[,3])/sqrt(3) # 0.04156439
sd(paras.mean.mat[,4])/sqrt(3) # 0.0001512242
sd(paras.mean.mat[,5])/sqrt(3) # 0.0002559215
sd(paras.mean.mat[,6])/sqrt(3) # 0.1093293

# MC error percentage:
sd(paras.mean.mat[,1])/0.01*100 # 1.37%
sd(paras.mean.mat[,2])/0.05*100 # 4.49%
sd(paras.mean.mat[,3])/10*100 # 0.72%
sd(paras.mean.mat[,4])/0.01*100 # 2.62%
sd(paras.mean.mat[,5])/0.1*100 # 0.44%
sd(paras.mean.mat[,6])/5*10 # 0.38%


