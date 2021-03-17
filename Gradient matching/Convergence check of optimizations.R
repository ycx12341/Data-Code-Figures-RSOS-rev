# Convergence check of optimizations.R
# Author: Yunchen Xiao

# The .R file checks the convergence of optimizations performed in 
# "PDE_GradientMatching_Main.r" to ensure the parameter estimates are
# obtained under the circumstances of successful convergence in the 
# optimizations.


library(readr)

# Source file with all the functions
source("PDE_GradientMatching_Functions.r")

# The directories of the old results and the new results with checked
# convergence
in.dir <- "SimRes_ests"
save.dir <- "SimRes_ests_converge_check"

# If the directory used to store the checked results does not exist, 
# create it
save.sims <- TRUE
if(save.sims) {
  if(!dir.exists(save.dir)) dir.create(save.dir)
}

# Define reference model parameters
dn <- 0.01
gamma <- 0.05
eta <- 10
dm <- 0.01
alpha <- 0.1
rn <- 5

true.values <- c(dn, gamma, rn, eta, dm, alpha)
names(true.values) <- c("dn", "gamma", "rn", "eta", "dm", "alpha")

# Number of CVs used and the simulations made for each CV.
cv.length <- 5
n.sims <- 200

# Start values of the optimization scheme
start.values <- c(0.01, 0.133, 6.25, 12.5, 0.0166, 0.125)

# Degrees of freedom, adopted from the old main file
df <- 2100

# Loop through all the old results, reoptimize the parameters with
# a greater number of maximum iterations in the scheme, and check the
# convergence at the end.

for (i in 1:cv.length) {
  par.ests.vec <- matrix(0, nrow = 200, ncol = 6)
  sd.ests.vec <- matrix(0, nrow = 200, ncol = 6)
  for (j in 1:n.sims) {
    rds.temp <- read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))
    
    # Fixed elements
    ref.data.trun.temp <- rds.temp$ref.data.trun
    pert.data.temp <- rds.temp$pert.data
    grads.temp <- rds.temp$grads
    cv.temp <- rds.temp$cv
    dist.temp <- rds.temp$dist
    
    # Reoptimize the function with a greater max iteration number
    res.temp <- optim(start.values, calculate.sse, grads = grads.temp, hessian = TRUE,
                 control = list(trace = 1, maxit = 20000))
    
    if (res.temp$convergence > 0) warning("Optim did not converge")
    par.ests.temp <- res.temp$par
    sd.ests.temp <- estimate.sd(res.temp, df)
    par.ests.vec[j, ] <- par.ests.temp
    sd.ests.vec[j, ] <- sd.ests.temp
    
    if(save.sims == TRUE) {
      readr::write_rds(list(ref.data.trun = ref.data.trun.temp, pert.data = pert.data.temp, 
                            grads = grads.temp, cv = cv.temp, dist = dist.temp, res = res.temp, 
                            par.ests = par.ests.temp, sd.ests = sd.ests.temp), 
                       path = paste0("./", save.dir, "/cv", i, "_sim", j, "_res.rds"))
    }
  }
  colnames(par.ests.vec) <- colnames(sd.ests.vec) <- names(true.values)
  if(save.sims == TRUE) {
    write_rds(list(true.values = true.values, par.ests = par.ests.vec, sd.ests = sd.ests.vec),
              paste0("./", save.dir, "/cv", i, "_sim_res.rds"))
  }
}

# Compare the convergence of the new results to the old ones
failed.conv.old.res <- vector()

for (i in 1:cv.length) {
  for (j in 1:n.sims) {
    rds.temp <- read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))
    conv.temp <- rds.temp$res$convergence
    
    if (conv.temp > 0) {
      failed.conv.old.res <- rbind(failed.conv.old.res, c(i,j))
    }
  }
}

# 991 out of 1000 failed to meet the convergence criteria!

failed.conv.checked <- vector()


for (i in 1:cv.length) {
  for (j in 1:n.sims) {
    rds.temp <- read_rds(paste0("./", save.dir, "/cv", i, "_sim", j, "_res.rds"))
    conv.temp <- rds.temp$res$convergence
    
    if (conv.temp > 0) {
      failed.conv.checked <- rbind(failed.conv.checked, c(i,j))
    }
  }
}

# None of the 1000 failed to meet the convergence criteria, checked!