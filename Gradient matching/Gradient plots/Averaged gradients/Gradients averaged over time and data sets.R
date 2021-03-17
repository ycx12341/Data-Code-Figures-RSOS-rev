# Gradients averaged over time and data sets.R
# Author: Yunchen Xiao

# This .R file generates the spatial and temporal gradients across the 1D domain 
# in the PDE, averaged over 200 different data sets and different timepoints.

in.dir <- "SimRes_ests_converge_check"
n.sims <- 200

# dndt (Tumour cells temporal gradients)
mean.temp.gradients.n <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.lhs_n
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.n[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.n, "Mean temporal grads tc.txt")

# dn gradients
mean.temp.gradients.dn <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.rhs_dn
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.dn[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.dn, "Mean spatial grads dn.txt")

# gamma gradients
mean.temp.gradients.gamma <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.rhs_gamma
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.gamma[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.gamma, "Mean spatial grads gamma.txt")

# rn gradients
mean.temp.gradients.rn <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.rhs_r
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.rn[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.rn, "Mean spatial grads rn.txt")

# dfdt (ECM temporal gradients)
mean.temp.gradients.ecm <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.lhs_f
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.ecm[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.ecm, "Mean temporal grads ecm.txt")

# eta gradients
mean.temp.gradients.ita <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.rhs_ita
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.ita[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.ita, "Mean spatial grads ita.txt")

# dmdt (Temporal gradients of MDE)
mean.temp.gradients.mde <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.lhs_m
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.mde[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.mde, "Mean temporal grads mde.txt")

# dm gradients
mean.temp.gradients.dm <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.rhs_dm
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.dm[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.dm, "Mean spatial grads dm.txt")

# alpha gradients
mean.temp.gradients.alpha <- matrix(0, nrow = 5, ncol = 78)

for (i in 1:5) {
  temp.grad.cvi <- matrix(0, nrow = 9, ncol = 78)
  for (j in 1:n.sims) {
    temp.res <- readr::read_rds(paste0("./", in.dir, "/cv", i, "_sim", j, "_res.rds"))$grads
    temp.grad <- temp.res$grad.rhs_alpha
    temp.grad.cvi <- temp.grad.cvi + temp.grad
  }
  temp.grad.cvi <- temp.grad.cvi / n.sims
  temp.grad.cvi.mean <- apply(temp.grad.cvi, 2, mean)
  mean.temp.gradients.alpha[i, ] <- temp.grad.cvi.mean
}

write.table(mean.temp.gradients.alpha, "Mean spatial grads alpha.txt")

# Averaged reference gradients predicted by GAM without any measurement 
# errors

ref.grads.gam <- read.table("Reference gradients GAM.txt", sep = "", header = TRUE)

tc.temp.mean <- apply(as.matrix(ref.grads.gam[1:9, 1:78]), 2, mean)
dn.spat.mean <- apply(as.matrix(ref.grads.gam[1:9, 79:156]), 2, mean)
ga.spat.mean <- apply(as.matrix(ref.grads.gam[1:9, 157:234]), 2, mean)
rn.spat.mean <- apply(as.matrix(ref.grads.gam[1:9, 235:312]), 2, mean)
ecm.temp.mean <- apply(as.matrix(ref.grads.gam[1:9, 313:390]), 2, mean)
eta.spat.mean <- apply(as.matrix(ref.grads.gam[1:9, 391:468]), 2, mean)
mde.temp.mean <- apply(as.matrix(ref.grads.gam[1:9, 469:546]), 2, mean)
dm.spat.mean <- apply(as.matrix(ref.grads.gam[1:9, 547:624]), 2, mean)
alpha.spat.mean <- apply(as.matrix(ref.grads.gam[1:9, 625:702]), 2, mean)

ref.grad.gam.mean <- rbind(tc.temp.mean, dn.spat.mean, ga.spat.mean, rn.spat.mean,
                           ecm.temp.mean, eta.spat.mean, mde.temp.mean, dm.spat.mean,
                           alpha.spat.mean)
write.table(ref.grad.gam.mean, "Mean reference data grads gam.txt")

# Averaged true gradients predicted by the finite difference scheme

ref.grads.fds <- read.table("True gradients forward difference scheme.txt", sep = "", header = TRUE)

tc.temp.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 1:78]), 2, mean)
dn.spat.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 79:156]), 2, mean)
ga.spat.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 157:234]), 2, mean)
rn.spat.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 235:312]), 2, mean)
ecm.temp.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 313:390]), 2, mean)
eta.spat.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 391:468]), 2, mean)
mde.temp.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 469:546]), 2, mean)
dm.spat.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 547:624]), 2, mean)
alpha.spat.mean.fds <- apply(as.matrix(ref.grads.fds[1:9, 625:702]), 2, mean)

ref.grad.fds.mean <- rbind(tc.temp.mean.fds, dn.spat.mean.fds, ga.spat.mean.fds, rn.spat.mean.fds,
                           ecm.temp.mean.fds, eta.spat.mean.fds, mde.temp.mean.fds, dm.spat.mean.fds,
                           alpha.spat.mean.fds)

write.table(ref.grad.fds.mean, "Mean reference data grads fds.txt")
