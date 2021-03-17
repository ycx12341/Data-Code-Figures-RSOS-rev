### Gradient matching scheme ########
### Author: Yunchen Xiao & Len Thomas ###########

#Does gradient matching with some parameters fixed
#Relies on the main routine already having been run, so gradients calculated, which
# is the slow part.  The optimization, which is the only part performed here, is
# relatively fast

#Source companion functions
source("PDE_GradientMatching_Functions_FixPar.r")

#Load "readr" package for writing results - just check it's loaded
#(it's actually used first inside the parallel routine)
library(readr)
#Load packages for running the simulation in parallel
library(doParallel) 
library(doRNG)


### Setup ####
save.sims <- TRUE
in.dir <- "SimRes_ests_converge_check"
cons.dir <- "SimRes_constrained"

if(save.sims) {
  if(!dir.exists(cons.dir)) dir.create(cons.dir)
}

cv <- c(0.01, 0.025, 0.05, 0.075, 0.10)
n.cvs <- length(cv)
true.values <-  read_rds(paste0(in.dir[1], "/cv1_sim_res.rds"))$true.values
pars <- dimnames(read_rds(paste0(in.dir[1], "/cv1_sim_res.rds"))$par.ests)[[2]]
n.pars <- length(pars)

n.sims <- 200
#Number of parallel threads to run on
n.threads <- detectCores() - 1

#Set which parameters to fix (those not fixed are estimated)
fixed.par <- rep(NA, 6)
fixed.par[c(1, 2)] <- true.values[c(1, 2)]
#fixed.par[c(1, 5)] <- true.values[c(1, 5)]
#fixed.par[c(1, 2, 3)] <- true.values[c(1, 2, 3)]
is.estimated <- is.na(fixed.par)
n.estimated <- sum(is.estimated)

#For optimization, use start values from manuscript
start.values <- c(0.01, 0.133, 6.25, 12.5, 0.0166, 0.125)
#Trim to only those for which parameters are being estimated
start.values <- start.values[is.estimated]

ref.data.trun <-  read_rds(paste0(in.dir[1], "/cv1_sim1_res.rds"))$ref.data.trun
#Record degrees of freedom for sd calculations
n.data.trun <- sum(apply(sapply(ref.data.trun, dim), 2, prod))
n.params <- length(start.values)
df <- n.data.trun - n.params

### Do estimation ####

cl <- makeCluster(n.threads) 
registerDoParallel(cl)

for(i in 1:length(cv)) {
  #Run the simulation in parallel
  ests <- foreach (sim = 1:n.sims, .combine = rbind) %dopar% {
    
    #Retrieve gradient approximations
    grads <-  readr::read_rds(paste0(in.dir[1], "/cv", i, "_sim", sim, "_res.rds"))$grads
    
    #Estimate parameter values and associated sds
    res <- optim(start.values, calculate.sse, grads = grads, fixed.par = fixed.par, 
                 hessian = TRUE, control = list(maxit = 20000))
    
    par.ests <- res$par
    sd.ests <- estimate.sd(res, df)
    conv <- res$convergence
    
    #Vector to return from the foreach
    c(par.ests, sd.ests, conv)
  }
  par.ests <- ests[, 1:n.estimated]
  sd.ests <- ests[, 1:n.estimated + n.estimated]
  conv.ests <- ests[,length(ests[1, ])]
  colnames(par.ests) <- colnames(sd.ests) <- names(true.values)[is.estimated]
  write_rds(list(true.values = true.values, is.estimated = is.estimated, 
            par.ests = par.ests, sd.ests = sd.ests, conv.ests = conv.ests),
            paste0("./", cons.dir, "/cv", i, "_sim_res_pars_fixed", 
            paste(substr(as.character(!is.estimated), 1, 1), collapse = ""), ".rds"))
}
#Stop the cluster
stopCluster(cl)

### Read in results and plot

mean.est <- perc.err <- mean.est.somefixed <- perc.err.somefixed <- 
  matrix(NA, n.cvs, n.pars, dimnames = list(cv, pars))
for(i in 1:n.cvs) {
  res <- read_rds(paste0(in.dir, "/cv", i, "_sim_res.rds"))
  mean.est[i, ] <- apply(res$par.ests, 2, mean)
  perc.err[i, ] <- abs(mean.est[i, ] - true.values) / true.values * 100
  res.somefixed <- read_rds(paste0("./", cons.dir, "/cv", i, "_sim_res_pars_fixed", 
          paste(substr(as.character(!is.estimated), 1, 1), collapse = ""), ".rds"))
  mean.est.somefixed[i, res.somefixed$is.estimated] <- apply(res.somefixed$par.ests, 2, mean)
  perc.err.somefixed[i, ] <- abs(mean.est.somefixed[i, ] - true.values) / true.values * 100
}

write.table(mean.est.somefixed, "Parameters means at all CV level dn gamma fixed.txt")
#write.table(mean.est.somefixed, "Parameters means at all CV level dn dm fixed.txt")
#write.table(mean.est.somefixed, "Parameters means at all CV level dn gamma rn fixed.txt")

### Check convergence ###

## dn gamma fixed
fail.conv.dngamma <- vector()
for (i in 1:n.cvs) {
  res.temp <- read_rds(paste0(cons.dir, "/cv", i, "_sim_res_pars_fixedTTFFFF.rds"))
  conv.temp <- res.temp$conv.ests
  for (j in 1:n.sims) {
    if (conv.temp[j] > 0) {
      fail.conv.dngamma <- rbind(fail.conv.dngamma, c(i,j))
    }
  }
}

## All optimization converged!

## dn gamma rn fixed ##
fail.conv.dngammarn <- vector()
for (i in 1:n.cvs) {
  res.temp <- read_rds(paste0(cons.dir, "/cv", i, "_sim_res_pars_fixedTTTFFF.rds"))
  conv.temp <- res.temp$conv.ests
  for (j in 1:n.sims) {
    if (conv.temp[j] > 0) {
      fail.conv.dngammarn <- rbind(fail.conv.dngammarn, c(i,j))
    }
  }
}

## All optimization converged!

## dn dm fixed ##
fail.conv.dndm <- vector()
for (i in 1:n.cvs) {
  res.temp <- read_rds(paste0(cons.dir, "/cv", i, "_sim_res_pars_fixedTFFFTF.rds"))
  conv.temp <- res.temp$conv.ests
  for (j in 1:n.sims) {
    if (conv.temp[j] > 0) {
      fail.conv.dndm <- rbind(fail.conv.dndm, c(i,j))
    }
  }
}

## All optimization converged!

### pdf generator ###
pdf(paste0("SimResFixed",  
  paste(substr(as.character(!res.somefixed$is.estimated), 1, 1), collapse = ""), ".pdf"))
old.par <- par(no.readonly= TRUE)
par(mfrow = c(3,2), mar = c(4, 4, 4, 1) + 0.1)
# Plot of estimates vs true values
for(i in 1:n.pars){
  plot(cv, mean.est[, i], ylab = paste0("Est (", pars[i], ")"), 
    ylim = range(c(mean.est[, i]), true.values[i], mean.est.somefixed[, i], na.rm = TRUE), type = "n", 
    main = pars[i])
  abline (h = true.values[i], lty = 2, col = "black", pch = 19)
  lines(cv, mean.est[, i], type = "b", col = "blue", pch = 19, lty = 2)
  lines(cv, mean.est.somefixed[, i], type = "b", col = "blue", pch = 19)
}

# Plot of percentage error of estimates
for(i in 1:n.pars) {
  plot(cv, perc.err[, i], ylab = paste0("abs % err (", pars[i], ")"), 
       ylim = c(0, max(c(perc.err, perc.err.somefixed), na.rm = TRUE)), type = "n", main = pars[i])
  lines(cv, perc.err[, i], type = "b", col = "blue", pch = 19, lty = 2)
  lines(cv, perc.err.somefixed[, i], type = "b", col = "blue", pch = 19)
  
}
dev.off()
