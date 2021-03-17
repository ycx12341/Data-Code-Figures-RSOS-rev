### Gradient matching scheme ########
### Author: Yunchen Xiao & Len Thomas ###########

### Single run on dataset with no measurement error ###

#Environment settings
library(readr)

#Source companion functions
source("PDE_GradientMatching_Functions.r")

### Setup ####

#Define simulation parameters

# Define model parameters
dn <- 0.01
gamma <- 0.05
eta <- 10
dm <- 0.01
alpha <- 0.1
rn <- 5
# This parameter not included in the optimization
beta <- 0
# Make a vector to store the true values
true.values <- c(dn, gamma, rn, eta, dm, alpha)
names(true.values) <- c("dn", "gamma", "rn", "eta", "dm", "alpha")

#No parameters fixed here, so set fixed par to 6 NAs
fixed.par <- rep(NA, 6)
is.estimated <- is.na(fixed.par)
n.estimated <- sum(is.estimated)

#For optimization, use start values from manuscript
start.values <- c(0.01, 0.133, 6.25, 12.5, 0.0166, 0.125)
#Trim to only those for which parameters are being estimated
start.values <- start.values[is.estimated]

# Define 1D dimensionless space points
n.x11 <- 80
max.x11 <- 1
x11 <- seq(0, max.x11, length = n.x11)

# Define time discretization and max time
dt <- 0.001
max.t <- 10

# Set initial conditions
eps <- 0.01
n0 <- rep(0, n.x11)
for (i in 1:n.x11) {
  if (x11[i] <= 0.25) {
    n0[i] <- exp(-x11[i] ^ 2 / eps)
  } else {
    n0[i] <- 0
  }
}
f0 <- 1-0.5*n0
m0 <- 0.5*n0

#Generate reference dataset
ref.data.trun <- generate.reference.data(n.x11, max.x11, dt, max.t,
  dn, gamma, eta, dm, alpha, rn, beta, n0, f0, m0, truncate = TRUE)

### Gradient matching estimation ####

#Obtain gradient approximations
dist <- "gamma"
grads <- approximate.gradients(ref.data.trun, x11, max.t, distribution = dist)

#write_rds(grads, "Reference gradients GAM.rds")
grads <- read_rds("Reference gradients GAM.rds")

#Write gradients predicted by GAM into a .txt file
#write.table(grads, "Reference gradients GAM.txt")

#Estimate parameter values
res <- optim(start.values, calculate.sse, grads = grads, fixed.par = fixed.par,
             control = list(trace = 1, maxit = 20000, reltol = 1e-10))
par.ests <- res$par
print(par.ests)

# dn = 0.009957235  gamma = 0.045667239  rn = 4.595227960 
# eta = 10.306459015  dm = 0.009527586  alpha = 0.099251049

#Calculate percent error
perc.err <- (par.ests - true.values) / true.values * 100
print(perc.err)

#        dn      gamma         rn        eta         dm      alpha 
#-0.4276505 -8.6655220 -8.0954408  3.0645902 -4.7241351 -0.7489510 