
library(MARSS)
library(ggplot2)

# Load data
data(lakeWAplankton, package = "MARSS")
# lakeWA
fulldat <- data.frame(lakeWAplanktonTrans)
year1 <- 1975; year2 <- 1994
years <- which(fulldat[, "Year"] >= year1 & 
                 fulldat[, "Year"] < year2)

# Prep the response variables
dat <- t(fulldat[years, c("Greens", "Other.algae")])
dat <- zscore(dat)

# Prep the covariates
covariates <- fulldat[, c("Year", "Month", "Temp", "TP")]
# remove the season
covariates$Month <- as.factor(covariates$Month)
covariates$Temp <- residuals(lm(Temp ~ Month, data=covariates))
covariates$TP <- residuals(lm(TP ~ Month, data=covariates))
covariates$pH <- residuals(lm(TP ~ Month, data=covariates))
# make into a matrix
covariates <- t(covariates[years, c("Temp","TP","pH")])
# Demean and standardize variance to 1
covariates <- zscore(covariates)

# Plot data
LWA <- ts(cbind(t(dat), t(covariates)), 
          start=c(year1,1), freq=12)
plot(LWA, main="", yax.flip=TRUE)

# Is there a seasonal cycle to the algae data?
# A bit
# Add seasonal covariate
TT <- ncol(dat)
covariates <- rbind(covariates, sin(2*pi*(1:TT)/12), cos(2*pi*(1:TT)/12))
rownames(covariates) <- c("Temp6","TP6","Temp","TP", "s","c")

# Fit model with process and observation error
# https://nwfsc-timeseries.github.io/atsa-labs/sec-msscov-both-error.html
D <- d <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "equalvarcov"
C <- matrix(list("t1", "t2", "tp1", "tp2"),2,2)
C <- "unconstrained"
c <- covariates[1:2,]
D <- "unconstrained"
d <- covariates
R <- "diagonal and unequal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "diagonal and unequal"
D <- "unconstrained"
d <- covariates[3:6,]
R <- "diagonal and equal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "diagonal and unequal"
C <- "unconstrained"
c <- covariates[1:2,]
D <- "unconstrained"
d <- covariates[5:6,]
R <- "diagonal and equal"
x0 <- "unequal"
tinitx <- 1
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)

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

