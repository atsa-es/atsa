# https://atsa-es.github.io/atsa-labs/chap-mss.html

# Set up
library(MARSS)
library(colorspace)
hcl_palette_52 <- colorspace::sequential_hcl(52)
hcl_palette_100 <- colorspace::sequential_hcl(100)
library(ggplot2)
        
# Set up the data

data(harborSealWA, package = "MARSS")
dat <- MARSS::harborSealWA
years <- dat[, "Year"]
dat <- dat[, !(colnames(dat) %in% c("Year", "HC"))]
dat <- t(dat)  # transpose to have years across columns
colnames(dat) <- years
n <- nrow(dat) - 1

# Question one
# Plot the data

# Task Group 1
# https://atsa-es.github.io/atsa-labs/sec-mss-a-single-well-mixed-population.html
# Fit a model with one underlying state (population process)
# All sites having different observation variance
mod.list.0 <- list(B = matrix(1), U = matrix("u"), Q = matrix("q"), 
                   Z = matrix(1, 4, 1), A = "scaling", R = "diagonal and unequal", 
                   x0 = matrix("mu"), tinitx = 0)
fit.0 <- MARSS(dat, model = mod.list.0)
# Look at the plots
# Plot 1. Model fitted Y + CI
# Plot 2. States - That's the population estimate
# Last plot. Model innovations residuals ACF
autoplot(fit.0)

# Q1. Change the observation model to all sites
# having the same observation error variance
# Is this more or less supported than all having different variance?

mod.list.1 <- list(B = matrix(1), U = matrix("u"), Q = matrix("q"), 
                   Z = matrix(1, 4, 1), A = "scaling", R = "diagonal and equal", 
                   x0 = matrix("mu"), tinitx = 0)
fit.1 <- MARSS(dat, model = mod.list.1)

# more supported
c(AIC(fit.0), AIC(fit.1))

# Q2. Change to observation errors R="unconstrained"
# What does that mean? Are the errors correlated across sites? Why might that happen?

mod.list.2 <- list(B = matrix(1), U = matrix("u"), Q = matrix("q"), 
                   Z = matrix(1, 4, 1), A = "scaling", R = "unconstrained", 
                   x0 = matrix("mu"), tinitx = 0)
fit.2 <- MARSS(dat, model = mod.list.2)

# less supported
c(AIC(fit.0), AIC(fit.1), AIC(fit.2))

# Means that the observation variances are correlated across sites

# Q3. Look at the observation variance and correlation matrix
M=coef(fit.2, type="matrix")$R # Variance matrix
cov2cor(M) # Correlation matrix

# What does this say about the observation errors?

# Some of the errors seem to be correlated across sites, some positive, some negative



# Task Group 2
# https://atsa-es.github.io/atsa-labs/sec-mss-segind.html
# Fit a model with four underlying state (population process)
mod.list.1 <- list(B = diag(1,4), U = matrix("u",4,1), Q = "diagonal and equal", 
                   Z = diag(1,4), A = "scaling", R = "diagonal and unequal", 
                   x0 = "unequal", tinitx = 0)
fit.1 <- MARSS(dat, model = mod.list.1)
# Q1. Look at the plots
autoplot(fit.1)

# Plot 2. States - That's the population estimate
# Q2. In the model you fit, are the 4 state processes (the lines) correlated with each other? Just based on the model you fit? Think about Q.

# No, not correlated. A good year in subpop 1 does not imply a good year in subpop 2.

# Q3. What does this model say about the observation variances? Same, different?

# Different levels of variance and independent errors.

# Q4. Are we assuming that good years are correlated across sites or uncorrelated?

# uncorrelated.

# Task Group 3
# https://atsa-es.github.io/atsa-labs/sec-mss-segind.html
# Fit a model with four underlying state (population process)
mod.list.2 <- list(B = diag(1,4), U = matrix("u",4,1), Q = "equalvarcov", 
                   Z = diag(1,4), A = "scaling", R = "diagonal and equal", 
                   x0 = "unequal", tinitx = 0)
fit.2 <- MARSS(dat, model = mod.list.2)
# Q1. Look at the plots
autoplot(fit.2)

# Plot 2. States - That's the population estimate
# Q2. In the model you specified, is the variability in the 4 state processes (the lines) correlated with each other? Think about Q.

# Correlated but with the same variance. A good year in subpop 1 does not imply a good year in subpop 2.

Q.diag      0.01218
Q.offdiag   0.00983

So pretty highly correlated.


# Q3. Compare to a model with Q="diagonal and equal" and R="equalvarcov". Which one fits the data better based on AICc?

mod.list.3 <- list(B = diag(1,4), U = matrix("u",4,1), Q = "diagonal and equal", 
                   Z = diag(1,4), A = "scaling", R = "equalvarcov", 
                   x0 = "unequal", tinitx = 0)
fit.3 <- MARSS(dat, model = mod.list.3)

# The model with R="equalvarcov" is less supported
c(AIC(fit.2), AIC(fit.3))

# Q4. How are these 2 models different? How are they different in terms of how they
# model correlation across the sites?

# The first says the subpopulations are correlated (good & bad years correlated across subpopulations). The second one says that subpopulations are uncorrelated but the observation errors are correlated.
