# Automatic.R
# Author: Yunchen Xiao
# This .R file contains the necessary functions to implement the ABC scheme 
# proposed in Xiao et al. 

bcd <- function(paras, paras.ind) {
  # A function that calculates the Bhattacharrya distance between the 
  # simulations of the parameter vectors in the current parameter set and the
  # simulation of the reference parameter values. 
  
  # Inupt: 
  # para - the parameter set to be evaluated; 
  # paras.ind - indicator of the time series being considered. 
  
  # Output: the Bhattacharyya distances of the parameter vectors in the 
  # parameter set being investigated. 
  
  paras <- unname(paras)
  # Observed summary statistics
  mean.var.obs <- unname(as.matrix(read.table("mean_var_obs.txt",sep="")))
  
  # Space
  l1 <- 0
  l2 <- 1
  x11 <- seq(l1,l2,length = 80)
  n.x11 <- length(x11)
  h <- x11[2] - x11[1]
  
  # Time
  T <- 10
  dt <- 0.001
  t <- seq(0, T, by = 0.001)
  
  # Paras
  dn <- paras[1]
  gamma <- paras[2]
  ita <- paras[3]
  dm <- paras[4]
  alpha <- paras[5]
  r <- paras[6]
  
  beta <- 0
  eps <- 0.01
  
  # Initial condition
  n0 <- rep(0, length(x11))
  
  for (i in 1:length(x11)) {
    if (x11[i]<=0.25) {
      n0[i]<-exp(-x11[i]^2/eps)
    } else {
      n0[i]<-0
    }
  }
  
  # Initial densities for the numerical solver
  
  f0 <- 1-0.5*n0
  
  m0 <- 0.5*n0
  
    n <- n0
    f <- f0
    m <- m0
    
    p <- 1
    
    res <- vector()
    
    while(p * dt <= T) {
      f[2:(length(x11)-1)] <- -ita * dt * m[2:(length(x11)-1)] * f[2:(length(x11)-1)] + 
        f[2:(length(x11)-1)]
      
      m[2:(length(x11)-1)] <- dm * (m[1:(length(x11)-2)] + m[3:length(x11)] - 2 * m[2:(length(x11)-1)]) * dt / (h ^ 2) +
        alpha * n[2:(length(x11)-1)] * dt -  
        beta * m[2:(length(x11)-1)] * dt + m[2:(length(x11)-1)]
      
      n[2:(length(x11)-1)] <- dn * (n[1:(length(x11)-2)] + n[3:length(x11)] - 2 * n[2:(length(x11)-1)]) * dt / (h ^ 2) -  
        gamma * (n[3:length(x11)] - n[2:(length(x11)-1)]) * (f[3:length(x11)]-f[2:(length(x11)-1)]) * dt / (h ^ 2) - 
        gamma * n[2:(length(x11)-1)] * (f[1:(length(x11)-2)] + f[3:length(x11)] - 2 * f[2:(length(x11)-1)]) * dt / (h ^ 2) + 
        r * (1 - f[2:(length(x11)-1)] - n[2:(length(x11)-1)]) * n[2:(length(x11)-1)] * dt + n[2:(length(x11)-1)]
      
      # No flux boundary condition
      n[1] <- n[2]
      n[n.x11] <- n[n.x11 - 1]
      
      f[1] <- f[2]
      f[n.x11] <- f[n.x11 - 1]
      
      m[1] <- m[2]
      m[n.x11] <- m[n.x11 - 1]
      
      # Save the results at each t = 0.1*k (k as positive integers) time steps
      if(p %% 1000 == 0) {
        res <- rbind(res, n, f, m)
      }
      
      p <- p + 1
    }
    
    # Rearrange the means and variances
    res.arr <- matrix(0,nrow = 30, ncol = 80)
    
    tc.ind <- seq(1,28, by = 3)
    ecm.ind <- seq(2,29, by = 3)
    mde.ind <- seq(3,30, by = 3)
    
    res.arr[1:10,] = res[tc.ind,]
    res.arr[11:20,] = res[ecm.ind,]
    res.arr[21:30,] = res[mde.ind,]
    
    mean.var = matrix(0,nrow= 240, ncol = 2); 
    
    for (i in 1:80) {
      mean.var[i,1] <- mean(res.arr[1:10,i])
      mean.var[i,2] <- var(res.arr[1:10,i])
      
      mean.var[i+80,1] <- mean(res.arr[11:20,i])
      mean.var[i+80,2] <- var(res.arr[11:20,i])
      
      mean.var[i+160,1] <- mean(res.arr[21:30,i])
      mean.var[i+160,2] <- var(res.arr[21:30,i])
    }
    
    bcd.vec <- vector();
    
    # Only the density profiles being evaluated currently are taken into 
    # account when calculating the Bhattacharyya distances.
    
    if (paras.ind == "ecm") {
      for (j in 81:160) {
        bcd <- 0.25*log(0.25*((mean.var[j,2]/mean.var.obs[j,2])+(mean.var.obs[j,2]/mean.var[j,2])+2))+0.25*(((mean.var[j,1]-mean.var.obs[j,1])^2)/(mean.var.obs[j,2]+mean.var[j,2]))
        bcd.vec <- c(bcd.vec,bcd)
      } 
    } else if (paras.ind == "ecm_mde") {
      for (j in 81:240) {
        bcd <- 0.25*log(0.25*((mean.var[j,2]/mean.var.obs[j,2])+(mean.var.obs[j,2]/mean.var[j,2])+2))+0.25*(((mean.var[j,1]-mean.var.obs[j,1])^2)/(mean.var.obs[j,2]+mean.var[j,2]))
        bcd.vec <- c(bcd.vec,bcd)
      } 
    } else if (paras.ind == "all_three") {
      for (j in 1:240) {
        bcd <- 0.25*log(0.25*((mean.var[j,2]/mean.var.obs[j,2])+(mean.var.obs[j,2]/mean.var[j,2])+2))+0.25*(((mean.var[j,1]-mean.var.obs[j,1])^2)/(mean.var.obs[j,2]+mean.var[j,2]))
        bcd.vec <- c(bcd.vec,bcd)
      } 
    } else {
      return("Wrong indicator input!")
    }

    
    inv.index <- which(bcd.vec == "Inf")
    
    bcd.vec.2 <- bcd.vec
    bcd.vec.2[inv.index] <- 0
    
    bcd.sum <- sum(bcd.vec.2)
    
  return(bcd.sum)
}

abc.bcd <- function(ss.mat, paras, bw) {
  # A function that reads in the parameters being evaluated in the current round
  # and resamples the parameters to be evaluated in the next round. 
  
  # Input: 
  # ss.mat - matrix of summary statistics; 
  # paras - parameters being evaluated in the current round;
  # bw - bandwidth of weight calculations.
  
  # Set seed, make sure the same inputs produce the same results.
  set.seed(123)
  RNGkind(sample.kind = "Rejection")
  ss.mat <- unname(as.matrix(ss.mat))
  paras <- unname(as.matrix(paras))
  
  # Lower and upper bounds of the parameters.
  paras.lb <- c(0.000069, 0.005, 7, 0.0001, 0.07, 3.5)
  paras.ub <- c(0.02, 0.25, 18, 0.033, 0.18, 9)
  
  # Locate the invalid terms. 
  invalid<-vector()
  
  for (j in 1:length(ss.mat[1,])) {
    invalid.sep <- which(is.na(ss.mat[,j]) == TRUE)
    invalid <- c(invalid,invalid.sep)
  }
  
  # Uniqueness checking, make sure each term only appears once.
  invalid.index <- unique(invalid) 
  
  # Valid Bhattacharyya distance results.
  if(length(invalid.index) == 0) {
    ss.mat.valid <- ss.mat
  } else {
    ss.mat.valid <- ss.mat[-invalid.index,]
  }
  
  # Weights of the valid B-C distance results.
  wt <- (ss.mat.valid[,length(ss.mat.valid[1,])]^(-bw))  
  
  # Valid B-C results + Weight.
  ss.mat.valid.wt <- cbind(ss.mat.valid,wt) 
  
  # Resample probabilities.
  resamp.prob <- wt/sum(wt)
  
  # Valid B-C results + Weights + Resampling probabilities.
  ss.mat.valid.wt.prob <- cbind(ss.mat.valid.wt,resamp.prob)
  
  # Resample the indices.
  resamp.ind <- sample(ss.mat.valid[,1], size = length(paras[,1]), 
                       replace = TRUE, prob = resamp.prob)  
  
  # Resampled parameter vectors, without perturbation.
  paras.nr.unperturbed <- paras[resamp.ind,] 
  
  # An empty matrix used to store the perturbed parameter values.
  paras.nr.perturbed <- matrix(0,nrow = nrow(paras),ncol = ncol(paras)) 
  
  # Perturbed parameter values.
  for (i in 1:length(paras.nr.unperturbed[1,])) {
    for (j in 1:length(paras.nr.unperturbed[,1])){
      h <- sqrt(1-0.05^2)
      paras.nr.perturbed[j,i] <- rnorm(1, h * paras.nr.unperturbed[j,i] + (1-h) * mean(paras.nr.unperturbed[,i]),
                                       0.05*sd(paras.nr.unperturbed[,i]))
      # Make sure the parameter values do not go beyond the boundaries of the 
      # initial distributions.
      while ((paras.nr.perturbed[j,i] > paras.ub[i]) || (paras.lb[i] > paras.nr.perturbed[j,i])) {
        paras.nr.perturbed[j,i] <- rnorm(1, h * paras.nr.unperturbed[j,i] + (1-h) * mean(paras.nr.unperturbed[,i]),
                                         0.05*sd(paras.nr.unperturbed[,i]))
      }
    }
  }
  return(paras.nr.perturbed)
}
