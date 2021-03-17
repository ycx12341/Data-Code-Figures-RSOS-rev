# Truncated gradients.R

# Author: Yunchen Xiao

# This .R file generates the parameter estimates under the circumstances
# of deviated gradients being truncated. 

library(readr)
in.dir <- "SimRes_ests_converge_check"
n.sim <- 200

start.values <- c(0.01, 0.133, 6.25, 12.5, 0.0166, 0.125)

# A function minimizes the sum of squared differences of the 
# truncated gradient values. 

trun.sse <- function(par, grads, debug = FALSE) {
    sse1 <- sum((grads$grad.lhs_n[3:7, 21:58] - (par[1] * grads$grad.rhs_dn[3:7, 21:58] - 
                                       par[2] * grads$grad.rhs_gamma[3:7, 21:58] + 
                                         par[3] * grads$grad.rhs_r[3:7, 21:58]))^2)
    sse2 <- sum((grads$grad.lhs_f[3:7, 21:58] + par[4] * grads$grad.rhs_ita[3:7, 21:58])^2)
    sse3 <- sum((grads$grad.lhs_m[3:7, 21:58] - (par[5] * grads$grad.rhs_dm[3:7, 21:58] + 
                                       par[6] * grads$grad.rhs_alpha[3:7, 21:58]))^2)
    
    # Sum the sse from each equation
    res <- sum(sse1, sse2, sse3)
    if (debug) cat(res, "\n")
    return(res)
}

# Parameter estimates at the first level of cv, 0.01
res.cv1 <- vector()
fail.conv.cv1 <- vector()
for (i in 1:n.sim) {
  temp.grad.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", i, "_res.rds"))$grad
  
  res.cv1.temp <- optim(start.values, trun.sse, grads = temp.grad.cv1, hessian = TRUE,
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
  temp.grad.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", i, "_res.rds"))$grad
  
  res.cv2.temp <- optim(start.values, trun.sse, grads = temp.grad.cv2, hessian = TRUE,
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

# Parameter estimates at the third cv level, 0.05
res.cv3 <- vector()
fail.conv.cv3 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", i, "_res.rds"))$grad
  
  res.cv3.temp <- optim(start.values, trun.sse, grads = temp.grad.cv3, hessian = TRUE,
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

# Parameter estimates at the fourth cv level, 0.075
res.cv4 <- vector()
fail.conv.cv4 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", i, "_res.rds"))$grad
  
  res.cv4.temp <- optim(start.values, trun.sse, grads = temp.grad.cv4, hessian = TRUE,
                        control = list(maxit = 20000))
  
  conv.cv4.temp <- res.cv4.temp$convergence
  
  if (conv.cv4.temp > 0) {
    fail.conv.cv4 <- cbind(fail.conv.cv4, i)
  }
  
  est.cv4.temp <- res.cv4.temp$par
  
  res.cv4 <- rbind(res.cv4, est.cv4.temp)
}

cv4.mean <- apply(res.cv4, 2, mean)

# Convergence has been met for all optimizations at the fourth cv!

# Parameter estimates at the fifth cv level, 0.1
res.cv5 <- vector()
fail.conv.cv5 <- vector()

for (i in 1:n.sim) {
  temp.grad.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", i, "_res.rds"))$grad
  
  res.cv5.temp <- optim(start.values, trun.sse, grads = temp.grad.cv5, hessian = TRUE,
                        control = list(maxit = 20000))
  
  conv.cv5.temp <- res.cv5.temp$convergence
  
  if (conv.cv5.temp > 0) {
    fail.conv.cv5 <- cbind(fail.conv.cv5, i)
  }
  
  est.cv5.temp <- res.cv5.temp$par
  
  res.cv5 <- rbind(res.cv5, est.cv5.temp)
}

cv5.mean <- apply(res.cv5, 2, mean)

# Convergence has been met for all optimizations at the fifth cv!

cv.all.mean.trun <- rbind(cv1.mean, cv2.mean, cv3.mean, cv4.mean, cv5.mean)

write.table(cv.all.mean.trun, "Parameters means truncated.txt")

# Comparison plots (Optional)
cv.all.mean <- read.table("Parameters means at all CV levels.txt", sep = "", header = TRUE)

# true.val <- c(0.01, 0.05, 5, 10, 0.01, 0.1)

# par(mfrow = c(2,3))
# for (i in 1:6) {
#   plot(x = c(0.01, 0.025, 0.05, 0.075, 0.1), cv.all.mean[,i], type = "l",xlab = "Perturbation levels",
#        ylab = "Parameter estimates", ylim = c(min(cv.all.mean[,i])*0.8, max(cv.all.mean[,i])*1.1))
#   lines(x = c(0.01, 0.025, 0.05, 0.075, 0.1), cv.all.mean.trun[,i], col = "blue")
#   abline(h = true.val[i], col = "red")
# }
