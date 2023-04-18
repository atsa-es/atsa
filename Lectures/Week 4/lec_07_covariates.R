
library(MARSS)
library(ggplot2)

# Load data
data(lakeWAplankton, package = "MARSS")
# lakeWA
fulldat <- lakeWAplanktonTrans
year1 <- 1985; year2 <- 1994
years <- which(fulldat[, "Year"] >= year1 & 
                 fulldat[, "Year"] < year2)
dat <- t(fulldat[years, c("Greens", "Other.algae")])
covariates <- t(fulldat[years, c("Temp", "TP", "pH")])
# Demean and standardize variance to 1
dat <- zscore(dat)
covariates <- zscore(covariates)

# Plot data
LWA <- ts(cbind(t(dat), t(covariates)), 
          start=c(year1,1), freq=12)
plot(LWA, main="", yax.flip=TRUE)

# Fit model with process and observation error
# https://atsa-es.github.io/atsa-labs/sec-msscov-both-error.html
D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "equalvarcov"
C <- matrix(list("t1", "t2", "tp1", "tp2"),2,2)
c <- covariates[1:2,]
R <- "diagonal and unequal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

# Look at the state estimates
plot(kem, plot.type="xtT")

autoplot(kem$states[1,], type="l")

# In this model, the changes in the state (stochastic trend) is 
# being explained (or not) by the covariates.

# Q1. Change the model to one where the effect of temperature
# is the same for both blue green and green algae.
C <- matrix(list("t", "t", "tp1", "tp2"),2,2)

# Q2. Change the model to one where the effect of temperature
# is the same for both blue green and green algae and the effect
# of TP is the same.
C <- matrix(list("t", "t", "tp", "tp"),2,2)

# Q3. Change to a model where TP only affects blue green algae.
C <- matrix(list("t1", "t2", 0, "tp2"),2,2)

