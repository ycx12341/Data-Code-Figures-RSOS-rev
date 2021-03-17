# Post process the gradient matching scheme results, potentially from
# multiple runs

library(readr)

#in.dir is a vector with one row for each directory containing results to use
in.dir <- c("SimRes_ests_converge_check3")

#one colour for each directory
cols <- c("red")
n.runs <- length(in.dir)
cv <- c(0.01, 0.025, 0.05, 0.075, 0.10)
n.cvs <- length(cv)
true.values <- read_rds(paste0(in.dir[1], "/cv1_sim_res.rds"))$true.values
pars <- names(true.values)
n.pars <- length(pars)
mean.est <- perc.err <-
  array(NA, dim = c(n.cvs, n.pars, n.runs), dimnames = list(cv, pars, 1:n.runs))
for(i in 1:n.cvs) {
  for(j in 1:n.runs) {
    res <- read_rds(paste0(in.dir[j], "/cv", i, "_sim_res.rds"))
    mean.est[i, , j] <- apply(res$par.ests, 2, mean)
    perc.err[i, , j] <- abs(mean.est[i, , j] - true.values) / true.values * 100
  }
}


# Parameter estimations
write.table(mean.est, "Parameters means at all CV levels.txt")


