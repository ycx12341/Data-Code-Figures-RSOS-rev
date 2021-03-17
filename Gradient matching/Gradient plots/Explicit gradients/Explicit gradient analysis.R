# Explicit gradient analysis.R

# This .R file evaluates the explicit spatial and temporal gradients 
# in the PDE system discussed in Xiao et al., averaged over 200 data
# sets at different timepoints. 

# Environment settings
library(readr)
in.dir <- "SimRes_ests_converge_check"
n.sim <- 200

# Read in the true gradients calculated by the finite difference scheme
# and reference gradients predicted by GAM.  
ref.grad.gam <- read.table("Reference gradients GAM.txt", sep = "", header = TRUE)
ref.grad.fds <- read.table("True gradients forward difference scheme.txt",
                           sep = "", header = TRUE)

dndt.ref.gam <- as.matrix(ref.grad.gam[1:9, 1:78])
d2ndx2.ref.gam <- as.matrix(ref.grad.gam[1:9, 79:156])
ga.ref.gam <- as.matrix(ref.grad.gam[1:9, 157:234])
rn.ref.gam <- as.matrix(ref.grad.gam[1:9, 235:312])
dfdt.ref.gam <- as.matrix(ref.grad.gam[1:9, 313:390])
ita.ref.gam <- as.matrix(ref.grad.gam[1:9, 391:468])
dmdt.ref.gam <- as.matrix(ref.grad.gam[1:9, 469:546])
dm.ref.gam <- as.matrix(ref.grad.gam[1:9, 547:624])
alpha.ref.gam <- as.matrix(ref.grad.gam[1:9, 625:702])

dndt.ref.fds <- as.matrix(ref.grad.fds[1:9, 1:78])
d2ndx2.ref.fds <- as.matrix(ref.grad.fds[1:9, 79:156])
ga.ref.fds <- as.matrix(ref.grad.fds[1:9, 157:234])
rn.ref.fds <- as.matrix(ref.grad.fds[1:9, 235:312])
dfdt.ref.fds <- as.matrix(ref.grad.fds[1:9, 313:390])
ita.ref.fds <- as.matrix(ref.grad.fds[1:9, 391:468])
dmdt.ref.fds <- as.matrix(ref.grad.fds[1:9, 469:546])
dm.ref.fds <- as.matrix(ref.grad.fds[1:9, 547:624])
alpha.ref.fds <- as.matrix(ref.grad.fds[1:9, 625:702])

# Explicit gradients

# dndt predicted by GAM at different CV
dndt.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.n.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.lhs_n"]]
  dndt.cv1 <- dndt.cv1 + grad.temp.lhs.n.cv1
}

dndt.cv1 <- dndt.cv1/200

dndt.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.n.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.lhs_n"]]
  dndt.cv2 <- dndt.cv2 + grad.temp.lhs.n.cv2
}

dndt.cv2 <- dndt.cv2/200

dndt.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.n.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.lhs_n"]]
  dndt.cv3 <- dndt.cv3 + grad.temp.lhs.n.cv3
}

dndt.cv3 <- dndt.cv3/200

dndt.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.n.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.lhs_n"]]
  dndt.cv4 <- dndt.cv4 + grad.temp.lhs.n.cv4
}

dndt.cv4 <- dndt.cv4/200

dndt.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.n.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.lhs_n"]]
  dndt.cv5 <- dndt.cv5 + grad.temp.lhs.n.cv5
}

dndt.cv5 <- dndt.cv5/200

dndt.grads <- rbind(dndt.ref.gam, dndt.ref.fds, dndt.cv1, dndt.cv2, dndt.cv3, dndt.cv4, dndt.cv5)
write.table(dndt.grads, "dndt gradients.txt")

# d2ndx2, second order derivative associated with dn.

d2ndx2.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dn.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.rhs_dn"]]
  d2ndx2.cv1 <- d2ndx2.cv1 + grad.temp.rhs.dn.cv1
}

d2ndx2.cv1 <- d2ndx2.cv1/200

d2ndx2.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dn.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.rhs_dn"]]
  d2ndx2.cv2 <- d2ndx2.cv2 + grad.temp.rhs.dn.cv2
}

d2ndx2.cv2 <- d2ndx2.cv2/200

d2ndx2.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dn.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.rhs_dn"]]
  d2ndx2.cv3 <- d2ndx2.cv3 + grad.temp.rhs.dn.cv3
}

d2ndx2.cv3 <- d2ndx2.cv3/200

d2ndx2.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dn.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.rhs_dn"]]
  d2ndx2.cv4 <- d2ndx2.cv4 + grad.temp.rhs.dn.cv4
}

d2ndx2.cv4 <- d2ndx2.cv4/200

d2ndx2.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dn.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.rhs_dn"]]
  d2ndx2.cv5 <- d2ndx2.cv5 + grad.temp.rhs.dn.cv5
}

d2ndx2.cv5 <- d2ndx2.cv5/200

d2ndx2.grads <- rbind(d2ndx2.ref.gam, d2ndx2.ref.fds, d2ndx2.cv1, d2ndx2.cv2, d2ndx2.cv3, d2ndx2.cv4, d2ndx2.cv5)
write.table(d2ndx2.grads, "dn gradients.txt")

# gamma gradients, haptotaxis terms

ga.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ga.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.rhs_gamma"]]
  ga.cv1 <- ga.cv1 + grad.temp.rhs.ga.cv1
}

ga.cv1 <- ga.cv1/200

ga.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ga.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.rhs_gamma"]]
  ga.cv2 <- ga.cv2 + grad.temp.rhs.ga.cv2
}

ga.cv2 <- ga.cv2/200

ga.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ga.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.rhs_gamma"]]
  ga.cv3 <- ga.cv3 + grad.temp.rhs.ga.cv3
}

ga.cv3 <- ga.cv3/200

ga.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ga.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.rhs_gamma"]]
  ga.cv4 <- ga.cv4 + grad.temp.rhs.ga.cv4
}

ga.cv4 <- ga.cv4/200

ga.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ga.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.rhs_gamma"]]
  ga.cv5 <- ga.cv5 + grad.temp.rhs.ga.cv5
}

ga.cv5 <- ga.cv5/200

ga.grads <- rbind(ga.ref.gam, ga.ref.fds, ga.cv1, ga.cv2, ga.cv3, ga.cv4, ga.cv5)
write.table(ga.grads, "gamma gradients.txt")

# rn gradients
rn.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.rn.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.rhs_r"]]
  rn.cv1 <- rn.cv1 + grad.temp.rhs.rn.cv1
}

rn.cv1 <- rn.cv1/200

rn.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.rn.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.rhs_r"]]
  rn.cv2 <- rn.cv2 + grad.temp.rhs.rn.cv2
}

rn.cv2 <- rn.cv2/200

rn.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.rn.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.rhs_r"]]
  rn.cv3 <- rn.cv3 + grad.temp.rhs.rn.cv3
}

rn.cv3 <- rn.cv3/200

rn.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.rn.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.rhs_r"]]
  rn.cv4 <- rn.cv4 + grad.temp.rhs.rn.cv4
}

rn.cv4 <- rn.cv4/200

rn.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.rn.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.rhs_r"]]
  rn.cv5 <- rn.cv5 + grad.temp.rhs.rn.cv5
}

rn.cv5 <- rn.cv5/200

rn.grads <- rbind(rn.ref.gam, rn.ref.fds, rn.cv1, rn.cv2, rn.cv3, rn.cv4, rn.cv5)
write.table(rn.grads, "rn gradients.txt")

# dfdt predicted by GAM at different levels of CV
dfdt.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.f.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.lhs_f"]]
  dfdt.cv1 <- dfdt.cv1 + grad.temp.lhs.f.cv1
}

dfdt.cv1 <- dfdt.cv1/200

dfdt.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.f.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.lhs_f"]]
  dfdt.cv2 <- dfdt.cv2 + grad.temp.lhs.f.cv2
}

dfdt.cv2 <- dfdt.cv2/200

dfdt.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.f.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.lhs_f"]]
  dfdt.cv3 <- dfdt.cv3 + grad.temp.lhs.f.cv3
}

dfdt.cv3 <- dfdt.cv3/200

dfdt.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.f.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.lhs_f"]]
  dfdt.cv4 <- dfdt.cv4 + grad.temp.lhs.f.cv4
}

dfdt.cv4 <- dfdt.cv4/200

dfdt.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.f.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.lhs_f"]]
  dfdt.cv5 <- dfdt.cv5 + grad.temp.lhs.f.cv5
}

dfdt.cv5 <- dfdt.cv5/200

dfdt.grads <- rbind(dfdt.ref.gam, dfdt.ref.fds, dfdt.cv1, dfdt.cv2, dfdt.cv3, dfdt.cv4, dfdt.cv5)
write.table(dfdt.grads, "dfdt gradients.txt")

# ita gradients
ita.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ita.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.rhs_ita"]]
  ita.cv1 <- ita.cv1 + grad.temp.rhs.ita.cv1
}

ita.cv1 <- ita.cv1/200

ita.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ita.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.rhs_ita"]]
  ita.cv2 <- ita.cv2 + grad.temp.rhs.ita.cv2
}

ita.cv2 <- ita.cv2/200

ita.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ita.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.rhs_ita"]]
  ita.cv3 <- ita.cv3 + grad.temp.rhs.ita.cv3
}

ita.cv3 <- ita.cv3/200

ita.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ita.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.rhs_ita"]]
  ita.cv4 <- ita.cv4 + grad.temp.rhs.ita.cv4
}

ita.cv4 <- ita.cv4/200

ita.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.ita.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.rhs_ita"]]
  ita.cv5 <- ita.cv5 + grad.temp.rhs.ita.cv5
}

ita.cv5 <- ita.cv5/200

ita.grads <- rbind(ita.ref.gam, ita.ref.fds, ita.cv1, ita.cv2, ita.cv3, ita.cv4, ita.cv5)
write.table(ita.grads, "eta gradients.txt")

# dmdt gradients predicted by GAM at different levels 
dmdt.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.m.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.lhs_m"]]
  dmdt.cv1 <- dmdt.cv1 + grad.temp.lhs.m.cv1
}

dmdt.cv1 <- dmdt.cv1/200

dmdt.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.m.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.lhs_m"]]
  dmdt.cv2 <- dmdt.cv2 + grad.temp.lhs.m.cv2
}

dmdt.cv2 <- dmdt.cv2/200

dmdt.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.m.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.lhs_m"]]
  dmdt.cv3 <- dmdt.cv3 + grad.temp.lhs.m.cv3
}

dmdt.cv3 <- dmdt.cv3/200

dmdt.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.m.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.lhs_m"]]
  dmdt.cv4 <- dmdt.cv4 + grad.temp.lhs.m.cv4
}

dmdt.cv4 <- dmdt.cv4/200

dmdt.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.lhs.m.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.lhs_m"]]
  dmdt.cv5 <- dmdt.cv5 + grad.temp.lhs.m.cv5
}

dmdt.cv5 <- dmdt.cv5/200

dmdt.grads <- rbind(dmdt.ref.gam, dmdt.ref.fds, dmdt.cv1, dmdt.cv2, dmdt.cv3, dmdt.cv4, dmdt.cv5)
write.table(dmdt.grads, "dmdt gradients.txt")

# dm gradients
dm.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dm.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.rhs_dm"]]
  dm.cv1 <- dm.cv1 + grad.temp.rhs.dm.cv1
}

dm.cv1 <- dm.cv1/200

dm.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dm.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.rhs_dm"]]
  dm.cv2 <- dm.cv2 + grad.temp.rhs.dm.cv2
}

dm.cv2 <- dm.cv2/200

dm.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dm.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.rhs_dm"]]
  dm.cv3 <- dm.cv3 + grad.temp.rhs.dm.cv3
}

dm.cv3 <- dm.cv3/200

dm.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dm.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.rhs_dm"]]
  dm.cv4 <- dm.cv4 + grad.temp.rhs.dm.cv4
}

dm.cv4 <- dm.cv4/200

dm.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.dm.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.rhs_dm"]]
  dm.cv5 <- dm.cv5 + grad.temp.rhs.dm.cv5
}

dm.cv5 <- dm.cv5/200

dm.grads <- rbind(dm.ref.gam, dm.ref.fds, dm.cv1, dm.cv2, dm.cv3, dm.cv4, dm.cv5)
write.table(dm.grads, "dm gradients.txt")

# alpha gradients
alpha.cv1 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.alpha.cv1 <- readr::read_rds(paste0(in.dir, "/cv", 1, "_sim", j, "_res.rds"))$grads[["grad.rhs_alpha"]]
  alpha.cv1 <- alpha.cv1 + grad.temp.rhs.alpha.cv1
}

alpha.cv1 <- alpha.cv1/200

alpha.cv2 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.alpha.cv2 <- readr::read_rds(paste0(in.dir, "/cv", 2, "_sim", j, "_res.rds"))$grads[["grad.rhs_alpha"]]
  alpha.cv2 <- alpha.cv2 + grad.temp.rhs.alpha.cv2
}

alpha.cv2 <- alpha.cv2/200

alpha.cv3 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.alpha.cv3 <- readr::read_rds(paste0(in.dir, "/cv", 3, "_sim", j, "_res.rds"))$grads[["grad.rhs_alpha"]]
  alpha.cv3 <- alpha.cv3 + grad.temp.rhs.alpha.cv3
}

alpha.cv3 <- alpha.cv3/200

alpha.cv4 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.alpha.cv4 <- readr::read_rds(paste0(in.dir, "/cv", 4, "_sim", j, "_res.rds"))$grads[["grad.rhs_alpha"]]
  alpha.cv4 <- alpha.cv4 + grad.temp.rhs.alpha.cv4
}

alpha.cv4 <- alpha.cv4/200

alpha.cv5 <- matrix(0, nrow = 9, ncol = 78)

for (j in 1:n.sim) {
  grad.temp.rhs.alpha.cv5 <- readr::read_rds(paste0(in.dir, "/cv", 5, "_sim", j, "_res.rds"))$grads[["grad.rhs_alpha"]]
  alpha.cv5 <- alpha.cv5 + grad.temp.rhs.alpha.cv5
}

alpha.cv5 <- alpha.cv5/200

alpha.grads <- rbind(alpha.ref.gam, alpha.ref.fds, alpha.cv1, alpha.cv2, alpha.cv3, alpha.cv4, alpha.cv5)
write.table(alpha.grads, "alpha gradients.txt")
