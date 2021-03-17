# Replaced TC and dm gradients.R
# Author: Yunchen Xiao

# This file generates the parameter estimates under the circumstances
# of tumour cells related gradients and the second order spatial gradients
# related to dm being replaced by the true gradients calculated by the finite
# difference scheme. 

# Environment settings
library(readr)
in.dir <- "SimRes_ests_converge_check"
new.dir <- "SimRes_replace_tc_and_dm_fds_grads"
n.sim <- 200

save.sims <- TRUE

if(save.sims) {
  if(!dir.exists(new.dir)) dir.create(new.dir)
}

# Read in the simulation results and substitute the tumour cells related gradients 
# with the true gradients calculated by the finite difference scheme, then write them
# into a new directory.
ref.grad <- read.table("True gradients forward difference scheme.txt", sep = "", header = TRUE)

for (i in 1:5) {
  for (j in 1:n.sim) {
    temp.grad.cv <- readr::read_rds(paste0(in.dir, "/cv", i, "_sim", j, "_res.rds"))$grad
    
    temp.grad.cv$grad.lhs_n <- as.matrix(ref.grad[, 1:78])
    temp.grad.cv$grad.rhs_dn <- as.matrix(ref.grad[, 79:156])
    temp.grad.cv$grad.rhs_r <- as.matrix(ref.grad[, 235:312])
    temp.grad.cv$grad.rhs_gamma <- as.matrix(ref.grad[,157:234])
    temp.grad.cv$grad.rhs_dm <- as.matrix(ref.grad[, 547:624])
    
    if(save.sims)
      readr::write_rds(list(grads = temp.grad.cv), 
                       path = paste0("./", new.dir, "/cv", i, "_sim", j, "_res_replace_tc_and_dm.rds"))
  }
}

start.values <- c(0.01, 0.133, 6.25, 12.5, 0.0166, 0.125)

# A function that calculates the sum of squared differences
cal.sse <- function(par, grads, debug = FALSE) {
  sse1 <- sum((grads$grad.lhs_n - (par[1] * grads$grad.rhs_dn - 
                                     par[2] * grads$grad.rhs_gamma + par[3] * grads$grad.rhs_r))^2)
  sse2 <- sum((grads$grad.lhs_f + par[4] * grads$grad.rhs_ita)^2)
  sse3 <- sum((grads$grad.lhs_m - (par[5] * grads$grad.rhs_dm + 
                                     par[6] * grads$grad.rhs_alpha))^2)
  
  # Sum the sse from each equation
  res <- sum(sse1, sse2, sse3)
  if (debug) cat(res, "\n")
  return(res)
}

# Parameter estimates at the first level of cv, 0.01
res.cv1 <- vector()
fail.conv.cv1 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv1 <- readr::read_rds(paste0(new.dir, "/cv", 1, "_sim", i, "_res_replace_tc_and_dm.rds"))$grad
  
  res.cv1.temp <- optim(start.values, cal.sse, grads = temp.grad.cv1, hessian = TRUE,
                        control = list(maxit = 20000))
  
  conv.cv1.temp <- res.cv1.temp$convergence
  if (conv.cv1.temp > 0) {
    fail.conv.cv1 <- cbind(fail.conv.cv1, i)
  }
  
  est.cv1.temp <- res.cv1.temp$par
  res.cv1 <- rbind(res.cv1, est.cv1.temp)
}

cv1.mean <- apply(res.cv1, 2, mean)

# Convergence has been met for all optimizations at the first cv!

# Parameter estimates at the second level of cv, 0.025
res.cv2 <- vector()
fail.conv.cv2 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv2 <- readr::read_rds(paste0(new.dir, "/cv", 2, "_sim", i, "_res_replace_tc_and_dm.rds"))$grad
  
  res.cv2.temp <- optim(start.values, cal.sse, grads = temp.grad.cv2, hessian = TRUE,
                        control = list(maxit = 20000))
  
  conv.cv2.temp <- res.cv2.temp$convergence
  if (conv.cv2.temp > 0) {
    fail.conv.cv2 <- cbind(fail.conv.cv2, i)
  }
  
  est.cv2.temp <- res.cv2.temp$par
  res.cv2 <- rbind(res.cv2, est.cv2.temp)
}

cv2.mean <- apply(res.cv2, 2, mean)

# Convergence has been met for all optimizations at the second cv!

# Parameter estimates at the third level of cv, 0.05
res.cv3 <- vector()
fail.conv.cv3 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv3 <- readr::read_rds(paste0(new.dir, "/cv", 3, "_sim", i, "_res_replace_tc_and_dm.rds"))$grad
  
  res.cv3.temp <- optim(start.values, cal.sse, grads = temp.grad.cv3, hessian = TRUE, 
                        control = list(maxit = 20000))
  
  conv.cv3.temp <- res.cv3.temp$convergence
  if (conv.cv3.temp > 0) {
    fail.conv.cv3 <- cbind(fail.conv.cv3, i)
  }
  
  est.cv3.temp <- res.cv3.temp$par
  res.cv3 <- rbind(res.cv3, est.cv3.temp)
}

cv3.mean <- apply(res.cv3, 2, mean)

# Convergence has been met for all optimizations at the third cv!

# Parameter estimates at the fourth level of cv, 0.075
res.cv4 <- vector()
fail.conv.cv4 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv4 <- readr::read_rds(paste0(new.dir, "/cv", 4, "_sim", i, "_res_replace_tc_and_dm.rds"))$grad
  
  res.cv4.temp <- optim(start.values, cal.sse, grads = temp.grad.cv4, hessian = TRUE,
                        control = list(maxit = 20000))
  
  conv.cv4.temp <- res.cv4.temp$convergence
  if (conv.cv4.temp > 0) {
    fail.conv.cv4 <- cbind(fail.conv.cv4, i)
  }
  
  est.cv4.temp <- res.cv4.temp$par
  res.cv4 <- rbind(res.cv4, est.cv4.temp)
}

cv4.mean <- apply(res.cv4, 2, mean)

# Convergence has been met for all optimization at the fourth cv!

# Parameter estimates at the fourth level of cv, 0.1
res.cv5 <- vector()
fail.conv.cv5 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv5 <- readr::read_rds(paste0(new.dir, "/cv", 5, "_sim", i, "_res_replace_tc_and_dm.rds"))$grad
  
  res.cv5.temp <- optim(start.values, cal.sse, grads = temp.grad.cv5, hessian = TRUE,
                        control = list(maxit = 20000))
  
  est.cv5.temp <- res.cv5.temp$par
  
  conv.cv5.temp <- res.cv5.temp$convergence
  if (conv.cv5.temp > 0) {
    fail.conv.cv5 <- cbind(fail.conv.cv5, i)
  }
  
  res.cv5 <- rbind(res.cv5, est.cv5.temp)
}

cv5.mean <- apply(res.cv5, 2, mean)

cv.all.mean.replace <- rbind(cv1.mean, cv2.mean, cv3.mean, cv4.mean, cv5.mean)
write.table(cv.all.mean.replace, "Parameters means tc dm gradients replaced by FDS grads.txt")

# Comparison plots (Optional)
# cv.all.mean <- read.table("Parameters means at all CV levels.txt", sep = "", header = TRUE)

# true.val <- c(0.01, 0.05, 5, 10, 0.01, 0.1)

# par(mfrow = c(2,3))
# for (i in 1:6) {
#   plot(x = c(0.01, 0.025, 0.05, 0.075, 0.1), cv.all.mean[,i], type = "l",xlab = "Perturbation levels",
#        ylab = "Parameter estimates", ylim = c(min(cv.all.mean[,i])*0.8, max(cv.all.mean[,i])*1.2))
#   lines(x = c(0.01, 0.025, 0.05, 0.075, 0.1), cv.all.mean.replace[,i], col = "blue")
#   abline(h = true.val[i], col = "red")
# }

