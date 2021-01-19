#################################################################
##  Univariate state-space models
##  Example 5.  The hidden x is a straight line
##              This illustrates that you can reproduce a
##              linear regression fit with a state-space model.
#################################################################
library(MARSS)

# x is the "hidden" trend we want to find
intercept <- 1 # this is x at t=0
slope <- 0.5
r <- 1
n <- 10
t <- 1:n
x <- intercept + slope * t
plot(x, xlim = c(1, n), ylim = c(0, n), type = "l", ylab = "time")

# y is our observation of x with error
y <- x + rnorm(n, 0, sqrt(r))
points(y)

# Let's estimate the x
fit <- lm(y ~ t)
fit
# add fit to our plot
abline(fit, col = "red", lty = 2, lwd = 3)
title("fit is red; true x is black")

## Preliminaries: how to write x=intercept+slope*t as a AR-1
x[1] <- intercept + slope # this is x at t=1
for (i in 2:n) x[i] <- x[i - 1] + slope # n=10 from above

plot(1:n, x, xlim = c(0, n), ylim = c(0, n), type = "l", lwd = 2, col = "blue")
lines(c(4, 5), c(x[4], x[4]))
lines(c(5, 5), c(x[4], x[5]))
text(5, x[4] + slope / 2, "slope", pos = 4)

# Let's write x as a AR-1 model and y as an observation of that
# x(t) = x(t-1) + slope + w(t), w(t)~N(0,0)  so w(t)=0
# x(0) = intercept
# y(t) = x(t) + v(t), v(t)~N(0,r)
mod.list <- list(
  U = matrix("slope"),
  x0 = matrix("intercept"),
  B = matrix(1),
  Q = matrix(0),
  Z = matrix(1),
  A = matrix(0),
  R = matrix("r"),
  tinitx = 0
)

fit2 <- MARSS(y, model = mod.list)
plot(x, xlim = c(1, n), ylim = c(0, n), type = "l", ylab = "time")
points(y)
lines(fit2$states[1, ], col = "blue", lwd = 2)
abline(fit, col = "red", lty = 2, lwd = 3)
title("AR is blue; lm is red; true x is black")

# parameter estimates
est.slope <- coef(fit2)$U[, 1]
est.intercept <- coef(fit2)$x0[, 1]
est.r <- coef(fit2)$R[, 1]

# Let's forecast our OBSERVATIONS forward 10 time steps
# x(t+1)=x(t)+slope
# y(t+1)=x(t+1)+v(t+1), v(t)~N(0,r)
# First let's set up our estimated x
# The last x at t=max(t)
x.est.end <- fit2$states[1, n]
t.forward <- 10
x.forecast <- x.est.end + est.slope * (1:t.forward)

# Let's first add the the real x and observations
ylims <- c(0, x[max(t)] + slope * t.forward + 3 * r)
xlims <- c(n - 9, n + t.forward)
plot((n - 9):n, x[(n - 9):n], xlim = xlims, ylim = ylims, type = "l", ylab = "y", xlab = "t")
points(y)
title(paste("forecast with", n, "data points for estimation\nblue is estimate; red is true"))

# Now let's forecast 1000 times using our estimates
for (i in 1:1000) {
  y.forecast <- x.forecast + rnorm(t.forward, 0, sqrt(est.r))
  jit <- rnorm(1, 0, .1) - .25
  points(n + 1:t.forward + jit, y.forecast, pch = ".", col = "blue")
}

# Now let's forecast 1000 times using truth
x.end <- x[max(t)]
x.true.forecast <- x.end + slope * (1:t.forward)

for (i in 1:1000) {
  y.true.forecast <- x.true.forecast + rnorm(t.forward, 0, sqrt(r))
  jit <- rnorm(1, 0, .1) + .25
  points(n + 1:t.forward + jit, y.true.forecast, pch = ".", col = "red")
}
