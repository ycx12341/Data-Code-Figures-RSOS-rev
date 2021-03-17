## True gradients from Numerical scheme.R
## Author: Yunchen Xiao

## This .R file generates the true gradients calculated by the finite 
## difference scheme used to solve the PDE system in Xiao et al. 

# Read in the gradients predicted by GAM which can be used for comparison
# in plots. (Optional)
# ref.grads.gam <- as.matrix(read.table("Reference gradients GAM.txt", sep = "", header = TRUE))

# 1D dimensionless space
n.x11 <- 80
x11 <- seq(0, 1, length = n.x11)
h <- x11[2] - x11[1]

# Time
T <- 10
dt <- 0.001
t <- seq(0, T, by = 0.001)
int.timesteps <- 1 / dt

# Parameters
dn <- 0.01
gamma <- 0.05
ita <- 10
dm <- 0.01
alpha <- 0.1
r <- 5

beta <- 0
eps <- 0.01

# Initial condition
n0 <- rep(0, n.x11)

for (i in 1:n.x11) {
  if (x11[i] <= 0.25) {
    n0[i] <- exp((-x11[i] ^ 2) / eps)
  } else {
    n0[i] <- 0
  }
}

n <- n0

f0 <- 1 - 0.5 * n0
f <- f0

m0 <- 0.5 * n0
m <- m0

# Store the initial values into the result matrix
res <- matrix(0, 3, n.x11)
res [1, ] <- n0
res [2, ] <- f0
res [3, ] <- m0

res.full <- res

# Run the equation
p <- 1

while(p * dt <= T) {
  f[2:(n.x11 - 1)] <- -ita * dt * m[2:(n.x11 - 1)] * f[2:(n.x11 - 1)] + 
    f[2:(n.x11 - 1)]
  
  m[2:(n.x11 - 1)] <- dm * (m[1:(n.x11 - 2)] + m[3:n.x11] - 2 * m[2:(n.x11 - 1)]) * dt / (h ^ 2) +
    alpha * n[2:(n.x11 - 1)] * dt -  
    beta * m[2:(n.x11 - 1)] * dt + m[2:(n.x11 - 1)]
  
  n[2:(n.x11 - 1)] <- dn * (n[1:(n.x11 - 2)] + n[3:n.x11] - 2 * n[2:(n.x11 - 1)]) * dt / (h ^ 2) -  
    gamma * (n[3:n.x11] - n[2:(n.x11 - 1)]) * (f[3:n.x11]-f[2:(n.x11 - 1)]) * dt / (h ^ 2) - 
    gamma * n[2:(n.x11 - 1)] * (f[1:(n.x11 - 2)] + f[3:n.x11] - 2 * f[2:(n.x11 - 1)]) * dt / (h ^ 2) + 
    r * (1 - f[2:(n.x11 - 1)] - n[2:(n.x11 - 1)]) * n[2:(n.x11 - 1)] * dt + n[2:(n.x11 - 1)]
  
  #No flux boundary condition
  n[1] <- n[2]
  n[n.x11] <- n[n.x11 - 1]
  
  f[1] <- f[2]
  f[n.x11] <- f[n.x11 - 1]
  
  m[1] <- m[2]
  m[n.x11] <- m[n.x11 - 1]
  
  res.full <- rbind(res.full,n,f,m)
  
  # Save the results at each integer time steps
  if(p %% int.timesteps == 0) {
    res <- rbind(res, n, f, m)
  }
  
  p <- p + 1
}

# Separate results of n, f and m at the 11 integer timepoints.
n.res <- res[seq(1, (length(res[, 1]) - 2), by = 3), ]
f.res <- res[seq(2, (length(res[, 1]) - 1), by = 3), ]
m.res <- res[seq(3, length(res[, 1]), by = 3), ]

# Separate full results of n, f and m
n.res.full <- res.full[seq(1, (length(res.full[, 1]) - 2), by = 3), ]
f.res.full <- res.full[seq(2, (length(res.full[, 1]) - 1), by = 3), ]
m.res.full <- res.full[seq(3, length(res.full[, 1]), by = 3), ]

ind <- seq(0, 10, by = 0.001)

# dndt
dndt <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(dndt[, 1])) {
  for (j in 1:length(dndt[1,])) {
    dndt[i,j] <- (n.res.full[(i * 1000 + 2), (j + 1)] - n.res.full[(i * 1000), (j + 1)])/(2*dt)
  }
}

# Comparison between true dndt gradients and the ones predicted by GAM
# when no measurement errors were included. (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(dndt[i, ], type = "l", main = paste0("dndt at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 1:78], col = "blue")
# }

# dfdt
dfdt <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(dfdt[, 1])) {
  for (j in 1:length(dfdt[1, ])) {
    dfdt[i,j] <- (f.res.full[(i * 1000 + 2), (j + 1)] - f.res.full[(i * 1000), (j + 1)]) / (2 * dt)
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(dfdt[i, ], type = "l", main = paste0("dfdt at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 313:390], col = "blue")
# }

# dmdt
dmdt <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(dmdt[, 1])) {
  for (j in 1:length(dmdt[1, ])) {
    dmdt[i,j] <- (m.res.full[(i * 1000 + 2), (j + 1)] - m.res.full[(i * 1000), (j + 1)]) / (2 * dt)
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(dmdt[i, ], type = "l", main = paste0("dmdt at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 469:546], col = "blue")
# }

# dn (second order derivative)
d2ndx2 <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(d2ndx2[, 1])) {
  for (j in 1:length(d2ndx2[1, ])) {
    d2ndx2[i,j] <- (n.res.full[(i * 1000 + 1), (j + 2)] + n.res.full[(i * 1000 + 1), j] 
                    - 2 * n.res.full[(i * 1000 + 1), (j + 1)]) / (h ^ 2)
  }
}

# Comparison plots (optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(d2ndx2[i, ], type = "l", main = paste0("d2ndx2 at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 79:156], col = "blue")
# }

# gamma (haptotaxis)
hap <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(hap[, 1])) {
  for (j in 1:length(hap[1, ])) {
    dndx.temp <- (n.res.full[(i * 1000 + 1), (j + 2)] - n.res.full[(i * 1000 + 1), j]) / (2 * h)
    dfdx.temp <- (f.res.full[(i * 1000 + 1), (j + 2)] - f.res.full[(i * 1000 + 1), j]) / (2 * h)
    n.temp <- n.res.full[(i * 1000 + 1), (j + 1)]
    d2fdx2.temp <- (f.res.full[(i * 1000 + 1), (j + 2)] + f.res.full[(i * 1000 + 1), j] 
                    - 2 * f.res.full[(i * 1000 + 1), (j + 1)]) / (h ^ 2)
    
    hap[i, j] <- dndx.temp * dfdx.temp + n.temp * d2fdx2.temp
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(hap[i, ], type = "l", main = paste0("haptotaxis at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 157:234], col = "blue")
# }

# rn (logistic growth)
logis <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(logis[, 1])) {
  for (j in 1:length(logis[1, ])) {
    logis[i, j] <- n.res.full[(i * 1000 + 1), (j + 1)] * (1 - n.res.full[(i * 1000 + 1), (j + 1)] - 
                                                            f.res.full[(i * 1000 + 1), (j + 1)])
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(logis[i, ], type = "l", main = paste0("Logistic growth at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 235:312], col = "blue")
# }

# eta (decay of ecm)
mf <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(logis[, 1])) {
  for (j in 1:length(logis[1, ])) {
    mf[i, j] <- m.res.full[(i * 1000 + 1), (j + 1)] * f.res.full[(i * 1000 + 1), (j + 1)]
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(mf[i, ], type = "l", main = paste0("Decay of ECM at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 391:468], col = "blue")
# }

# dm (second order derivative)
d2mdx2 <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(d2mdx2[, 1])) {
  for (j in 1:length(d2mdx2[1, ])) {
    d2mdx2[i,j] <- (m.res.full[(i * 1000 + 1), (j + 2)] + m.res.full[(i * 1000 + 1), j] 
                    - 2 * m.res.full[(i * 1000 + 1), (j + 1)]) / (h ^ 2)
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#  plot(d2mdx2[i, ], type = "l", main = paste0("d2mdx2 at t = ", i, sep = ""))
#  lines(ref.grads.gam[i, 547:624], col = "blue")
# }

# alpha (growth of mde)
tc.den <- matrix(0, nrow = 9, ncol = (n.x11 - 2))

for (i in 1:length(tc.den[, 1])) {
  for (j in 1:length(tc.den[1, ])) {
    tc.den[i,j] <- n.res.full[(i * 1000 + 1), (j + 1)]
  }
}

# Comparison plots (Optional)
# par(mfrow = c(3,3))
# for (i in 1:9) {
#   plot(tc.den[i, ], type = "l", main = paste0("d2mdx2 at t = ", i, sep = ""))
#   lines(ref.grads.gam[i, 625:702], col = "blue")
# }

# Write the true gradients into a .txt file
ref.grads.fds <- cbind(dndt, d2ndx2, hap, logis, dfdt, mf, dmdt, d2mdx2, tc.den)

write.table(ref.grads.fds, "True gradients forward difference scheme.txt")
