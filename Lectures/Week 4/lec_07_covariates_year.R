
library(MARSS)
library(ggplot2)

# Load data
data(lakeWAplankton, package = "MARSS")
# lakeWA
fulldat <- as.data.frame(lakeWAplanktonTrans)
year1 <- 1963; year2 <- 1994
dat <- subset(fulldat, Month==7 & Year >= year1 & Year <= year2)
dat <- t(dat[, c("Greens", "Other.algae")])
covariates <- subset(fulldat, Month==6 & Year >= year1 & Year <= year2)
covariates <- t(covariates[, c("Temp", "TP")])
# Demean and standardize variance to 1
dat <- zscore(dat)
covariates <- zscore(covariates)

# Plot data
LWA <- ts(cbind(t(dat), t(covariates)), 
          start=c(year1,1), freq=1)
plot(LWA, main="", yax.flip=TRUE)

# Fit model with process and observation error
# https://atsa-es.github.io/atsa-labs/sec-msscov-both-error.html
D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- diag(1,2)
C <- matrix(list("t1", "t2", "tp1", "tp2"),2,2)
c <- covariates
R <- "diagonal and equal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

ggplot(tidy(kem), aes(x=term, y=estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.up)) +
  geom_hline(yintercept = 0) +
  ggtitle(paste("estimated parameters", AIC(kem)))

# Look at the state estimates
plot(kem, plot.type="xtT")

plot(kem$states[1,], type="l")
points(dat[1,])

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

# Fit model with process and observation error
# https://atsa-es.github.io/atsa-labs/sec-msscov-both-error.html
C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- diag(1,2)
D <- matrix(list("t1", "t2", "tp1", "tp2"),2,2)
d <- covariates
R <- "diagonal and equal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

ggplot(tidy(kem), aes(x=term, y=estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.up)) +
  geom_hline(yintercept = 0) +
  ggtitle(paste("estimated parameters", AIC(kem)))


Z <- "identity"
B <- "identity"
Q <- diag(1,2)
C <- matrix(list("t1", "t2", "tp1", "tp2"),2,2)
c <- covariates
D <- matrix(list("t1", "t2", "tp1", "tp2"),2,2)
d <- covariates
R <- "diagonal and equal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

ggplot(tidy(kem), aes(x=term, y=estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.up)) +
  geom_hline(yintercept = 0) +
  ggtitle(paste("estimated parameters", AIC(kem)))
