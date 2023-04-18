
library(MARSS)
library(atsalibrary)
library(ggplot2)
library(broom)

# Load data
data(lakeWA, package = "atsalibrary")

# years to use
year1 <- 1975; year2 <- 1994

# Prep the response variables
library(dplyr)
phytoplankton <- c("Diatoms", "Greens")
dat <- lakeWA %>% 
  subset(Year >= year1 & Year <= year2) %>%
  select(all_of(phytoplankton)) %>%
  t %>%
  zscore

# Prep the covariates
library(dplyr)
covariates <- lakeWA %>% 
  subset(Year >= year1 & Year <= year2) %>%
  select(Temp.anom, TP.anom) %>%
  t %>%
  zscore


# Plot data
LWA <- ts(cbind(t(dat), t(covariates)), 
          start=c(year1,1), freq=12)
plot(LWA, main="", yax.flip=TRUE)

# Is there a seasonal cycle to the algae data?
# A bit
par(mfrow=c(1,2))
acf(dat[1,], na.action=na.pass)
acf(dat[2,], na.action=na.pass)

# Add seasonal covariate
TT <- ncol(dat)
covariates <- rbind(covariates, sin(2*pi*(1:TT)/12), cos(2*pi*(1:TT)/12))
rownames(covariates) <- c("Temp","TP", "s","c")

# Fit model with process and observation error
# no covariates except season
# https://atsa-es.github.io/atsa-labs/sec-msscov-both-error.html
D <- d <- C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "unconstrained"
D <- "unconstrained"
d <- covariates[3:4,]
R <- "diagonal and unequal"
x0 <- "unequal"
tinitx <- 0
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)
mods <- data.frame(name="Only season", AICc=kem$AICc)

# Fit model with process and observation error
# https://atsa-es.github.io/atsa-labs/sec-msscov-both-error.html
D <- d <- C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "unconstrained"
C <- "unconstrained"
c <- covariates[1:2,]
D <- "unconstrained"
d <- covariates[3:4,]
R <- "diagonal and unequal"
x0 <- "unequal"
tinitx <- 0
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)
mods <- rbind(mods, data.frame(name="Temp and TP in C", AICc=kem$AICc))

df <- tidy(kem)
df$parameter <- sapply(df$term, function(x){ stringr::str_split(x, "[.]")[[1]][1]})
ggplot(df, aes(x=estimate, y=term)) + geom_point() + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.up), width=0.2) +
  geom_vline(xintercept = 0) +
  ggtitle(paste("estimated parameters; AICc =", round(kem$AICc, digits=2))) +
  facet_wrap(~parameter, scales="free")


D <- d <- C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- "unconstrained"
D <- "unconstrained"
d <- covariates
R <- "diagonal and unequal"
x0 <- "unequal"
tinitx <- 0
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)
mods <- rbind(mods, data.frame(name="Temp and TP in D", AICc=kem$AICc))

df <- tidy(kem)
df$parameter <- sapply(df$term, function(x){ stringr::str_split(x, "[.]")[[1]][1]})
ggplot(df, aes(x=estimate, y=term)) + geom_point() + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.up), width=0.2) +
  geom_vline(xintercept = 0) +
  ggtitle(paste("estimated parameters; AICc =", round(kem$AICc, digits=2))) +
  facet_wrap(~parameter, scales="free")

D <- d <- C <- c <- A <- U <- "zero"
Z <- "identity"
B <- "identity"
Q <- diag(0,2)
D <- "unconstrained"
d <- covariates
R <- "diagonal and unequal"
x0 <- "unequal"
tinitx <- 0
model.list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R, 
                   D = D, d = d, C = C, c = c, x0 = x0, tinitx = tinitx)
kem <- MARSS(dat, model = model.list)
mods <- rbind(mods, data.frame(name="Only Temp and TP", AICc=kem$AICc))

df <- tidy(kem)
df$parameter <- sapply(df$term, function(x){ stringr::str_split(x, "[.]")[[1]][1]})
ggplot(df, aes(x=estimate, y=term)) + geom_point() + 
  geom_errorbar(aes(xmin=conf.low, xmax=conf.up), width=0.2) +
  geom_vline(xintercept = 0) +
  ggtitle(paste("estimated parameters; AICc =", round(kem$AICc, digits=2))) +
  facet_wrap(~parameter, scales="free")

# Look at the estimates
autoplot(kem)

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

