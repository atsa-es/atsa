#################################################################
##  Univariate state-space models
##  Example 6.  Let's fit an AR(1) model with Arima() and MARSS().
#################################################################
library(MARSS)

# Create some AR(1) data
b <- 0.8
q <- 0.1
n <- 1000
x <- arima.sim(model = list(ar = b), n = n, sd = sqrt(q))
y <- as.vector(x)

# Written as a state-space model this is
# x(t) = b x(t-1) + w(t), w(t)~N(0,q)
# x(0) = x0
# y(t) = x(t) + v(t), v(t)~N(0,0)

# Notice that r=0.  No observation error.

fit1 <- forecast::Arima(y, order = c(1, 0, 0), include.mean = FALSE, method = "ML")

# Let's fit with MARSS
mod.list <- list(
  U = matrix(0),
  x0 = matrix("x0"),
  B = matrix("b"),
  Q = matrix("q"),
  Z = matrix(1),
  A = matrix(0),
  R = matrix(0),
  tinitx = 0
)

fit2 <- MARSS(y, model = mod.list, silent = TRUE)

# The parameter estimates are similar but not exactly the same
# because the treatment of the initial conditions (x0) is different.
# If n is small, they will be more different as the effect of the initial
# x0 is stronger.
c(b = coef(fit1), q = fit1$sigma2)
coef(fit2, type = "vector")
